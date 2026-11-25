(declaim (optimize (speed 1) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

(defvar *deferred-layout-morphs* nil
  "When non-NIL (a hash-table of morph -> t), defer immediate layout-submorphs calls
during add-morph and flush them at the end of with-deferred-layout.")

(defun note-deferred-layout (morph)
  (when (and *deferred-layout-morphs* (typep morph 'layout-morph))
    (setf (gethash morph *deferred-layout-morphs*) t)))

(defun flush-deferred-layouts (table)
  (when table
    (maphash (lambda (morph presentp)
               (declare (ignore presentp))
               (when (typep morph 'layout-morph)
                 (layout-submorphs morph)
                 (record-morph-damage morph)))
             table)))

(defmacro with-deferred-layout ((&key) &body body)
  "Batch add-morph operations, running layout-submorphs once per touched layout-morph
after BODY completes."
  `(let ((*deferred-layout-morphs* (or *deferred-layout-morphs*
                                       (make-hash-table :test #'eq))))
     (unwind-protect
          (progn ,@body)
       (flush-deferred-layouts *deferred-layout-morphs*))))

(defmethod print-object ((obj location) stream)
  (print-object-inner obj stream))

(defmethod print-object ((obj boxed-morph) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s" (list
                         :id
                         (id obj)
                         :color
                         (color obj)))))

(defmethod initialize-instance :after ((morph morph) &key)
  (unless (typep morph 'world-morph)
    (setf (id morph) (next-morph-id))))

;;; =============== add morph ==================================================
(defmethod add-morph ((morph morph) (submorph morph))
  ;; 1. Cycle: submorph is morph, or an ancestor of morph.
  (when (morph-ancestor-p morph submorph)
    (error "Cannot add ~S to ~S: would create a cycle." submorph morph))

  ;; 2. Duplicate: already a direct child of morph.
  (when (member submorph (submorphs morph) :test #'eq)
    (error "~S is already a submorph of ~S." submorph morph))

  ;; 3. Detach from old owner (remember it for re-layout).
  (let ((old-owner (owner submorph)))
    (when old-owner
      (setf (submorphs old-owner)
            (remove submorph (submorphs old-owner) :test #'eq))
      (setf (owner submorph) nil))

    ;; 4. Attach to new parent without O(n^2) append copies.
    (setf (owner submorph) morph)
    (setf (submorphs morph)
          (if (submorphs morph)
              (nconc (submorphs morph) (list submorph))
              (list submorph)))
    (refresh-location submorph)

    ;; 5. Layout immediately unless inside with-deferred-layout.
    (if *deferred-layout-morphs*
        (progn
          (note-deferred-layout morph)
          (when old-owner
            (note-deferred-layout old-owner)))
        (progn
          (layout-submorphs morph)
          (record-morph-damage morph)
          (when old-owner
            (layout-submorphs old-owner)
            (record-morph-damage old-owner)))))

  submorph)

;;; ================== location ================================================
;;; Ensure every placed-morph starts with a valid (0, 0) location
(defmethod initialize-instance :after ((morph placed-morph) &key)
  (unless (location morph)
    (setf (location morph) (build-location 0 0))))

(defun layout-child-main-size (sub dir)
  "Return SUB's main-axis contribution along DIR: proportional weight or fixed/current size."
  (let ((spec (layout-spec sub))
        (ext  (extent sub)))
    (if (eq dir :horizontal)
        (cond
          ((and spec (proportional-width spec)) (values :proportional (proportional-width spec)))
          ((and spec (fixed-width spec))        (values :fixed (fixed-width spec)))
          (t                                    (values :fixed (if ext (x ext) 0))))
        (cond
          ((and spec (proportional-height spec)) (values :proportional (proportional-height spec)))
          ((and spec (fixed-height spec))        (values :fixed (fixed-height spec)))
          (t                                     (values :fixed (if ext (y ext) 0)))))))

(defun layout-tally-children (subs dir)
  "Return (VALUES fixed-sum prop-sum max-child-w max-child-h) for SUBS along DIR."
  (let ((fixed-sum 0)
        (prop-sum 0.0)
        (max-child-w 0)
        (max-child-h 0))
    (dolist (sub subs)
      (multiple-value-bind (kind val) (layout-child-main-size sub dir)
        (ecase kind
          (:proportional (incf prop-sum val))
          (:fixed        (incf fixed-sum val))))
      (let ((ext (extent sub)))
        (setf max-child-w (max max-child-w (if ext (x ext) 0))
              max-child-h (max max-child-h (if ext (y ext) 0)))))
    (values fixed-sum prop-sum max-child-w max-child-h)))

(defun layout-fit-cross-axis (morph dir max-child-w max-child-h)
  "Grow MORPH's cross-axis extent if unconstrained by a fixed or proportional spec."
  (let ((spec-self (layout-spec morph))
        (pad-x (layout-padding-x morph))
        (pad-y (layout-padding-y morph)))
    (when (and (eq dir :horizontal)
               (not (and spec-self (fixed-height spec-self)))
               (not (and spec-self (proportional-height spec-self))))
      (let ((needed-h (+ max-child-h (* 2 pad-y))))
        (when (or (null (y (extent morph)))
                  (< (y (extent morph)) needed-h))
          (setf (y (extent morph)) needed-h))))
    (when (and (eq dir :vertical)
               (not (and spec-self (fixed-width spec-self)))
               (not (and spec-self (proportional-width spec-self))))
      (let ((needed-w (+ max-child-w (* 2 pad-x))))
        (when (or (null (x (extent morph)))
                  (< (x (extent morph)) needed-w))
          (setf (x (extent morph)) needed-w))))))

(defun layout-child-dimensions (sub dir inner-w inner-h rem prop-sum)
  "Return (VALUES new-w new-h) for SUB inside a container with inner size (INNER-W, INNER-H).
  Falls back to 0 when SUB has no EXTENT, rather than signaling."
  (let ((spec (layout-spec sub))
        (ext  (extent sub)))
    (values
     (cond
       ((and (eq dir :horizontal) spec (proportional-width spec) (> prop-sum 0))
        (round (* rem (/ (proportional-width spec) prop-sum))))
       ((and spec (fixed-width spec))
        (fixed-width spec))
       ((and (eq dir :vertical) spec (proportional-width spec))
        (round (* inner-w (proportional-width spec))))
       (t (if ext (x ext) 0)))
     (cond
       ((and (eq dir :vertical) spec (proportional-height spec) (> prop-sum 0))
        (round (* rem (/ (proportional-height spec) prop-sum))))
       ((and spec (fixed-height spec))
        (fixed-height spec))
       ((and (eq dir :horizontal) spec (proportional-height spec))
        (round (* inner-h (proportional-height spec))))
       (t (if ext (y ext) 0))))))

(defun layout-place-children (morph subs dir fixed-sum prop-sum)
  "Resize and position SUBS inside MORPH along DIR and recursively layout each child."
  (let* ((gap (layout-gap morph))
         (pad-x (layout-padding-x morph))
         (pad-y (layout-padding-y morph))
         (n (length subs))
         (total-gap (* gap (max 0 (1- n))))
         (owner-w (x (extent morph)))
         (owner-h (y (extent morph)))
         (inner-w (max 0 (- owner-w (* 2 pad-x))))
         (inner-h (max 0 (- owner-h (* 2 pad-y))))
         (avail-w (max 0 (- inner-w (if (eq dir :horizontal) total-gap 0))))
         (avail-h (max 0 (- inner-h (if (eq dir :vertical) total-gap 0))))
         (rem (max 0 (- (if (eq dir :horizontal) avail-w avail-h) fixed-sum)))
         (cursor (if (eq dir :horizontal) pad-x pad-y)))
    (dolist (sub subs)
      (multiple-value-bind (new-w new-h)
          (layout-child-dimensions sub dir inner-w inner-h rem prop-sum)
        (let ((centered-y (+ pad-y (max 0 (round (- inner-h new-h) 2))))
              (centered-x (+ pad-x (max 0 (round (- inner-w new-w) 2)))))
          (let ((ext (ensure-extent sub)))
            (setf (x ext) new-w
                  (y ext)  new-h))
          (ecase dir
            (:horizontal
             (set-location-xy sub cursor centered-y)
             (incf cursor (+ new-w gap)))
            (:vertical
             (set-location-xy sub centered-x cursor)
             (incf cursor (+ new-h gap))))
          (layout-submorphs sub))))))

(defmethod layout-submorphs ((morph layout-morph))
  (let* ((subs (submorphs morph))
         (dir (direction morph)))
    (when (> (length subs) 0)
      (multiple-value-bind (fixed-sum prop-sum max-child-w max-child-h)
          (layout-tally-children subs dir)
        (layout-fit-cross-axis morph dir max-child-w max-child-h)
        (layout-place-children morph subs dir fixed-sum prop-sum)))))

;;; ---------------- mouse-over tracking ---------------------------------------
(defun process-mouse-over (handler new-chain)
  (let* ((old (mouse-over-morphs handler))
         (left    (remove-if (lambda (m) (member m new-chain)) old))
         (entered (remove-if (lambda (m) (member m old)) new-chain))
         (changed-p (or left entered (not (equal old new-chain)))))
    (dolist (m (reverse left))
      (when (handles-mouse-over-p m)
        (mouse-leave m)
        (record-morph-damage m)))
    (dolist (m entered)
      (when (handles-mouse-over-p m)
        (mouse-enter m)
        (record-morph-damage m)))
    (setf (mouse-over-morphs handler) new-chain)
    changed-p))

;;; ================= events ===================================================
(defun release-world-pressed-morph (world)
  (when (and world (pressed-morph world))
    (let ((prev (pressed-morph world)))
      (setf (is-pressed prev) nil
            (pressed-morph world) nil)
      (record-morph-damage prev))))

(defun handle-unhandled-mouse-button (morph event x y world)
  (declare (ignore morph x y))
  (when (and world (eq event :released))
    (release-world-pressed-morph world)))

(defmethod handle-event-mouse-button ((morph morph) (event symbol) button x y)
  (declare (ignore button))
  (let ((w (morph-target-world morph)))
    (when (eq event :pressed)
      (when w
        (setf (world-keyboard-focus w) nil)
        (mark-needs-redraw w)))
    (when (and w (eq event :released) (pressed-morph w))
      (mark-needs-redraw w))
    (handle-unhandled-mouse-button morph event x y w)))

(defmethod handle-event-mouse-button ((morph pluggable-button-morph) (event symbol) button x y)
  (declare (ignore button))
  (let ((w (morph-target-world morph)))
    (case event
      (:pressed
       (setf (is-pressed morph) t)
       (when w
         (setf (pressed-morph w) morph
               (world-keyboard-focus w) nil))
       (record-morph-damage morph)
       (when (and (contains-point-p morph x y)
                  (action-selector morph))
         (funcall (action-selector morph))))
      (:released
       (release-world-pressed-morph w)
       (record-morph-damage morph)
       (when (and (contains-point-p morph x y)
                  (secondary-action-selector morph))
         (funcall (secondary-action-selector morph)))))))

(defmethod handle-event-mouse-button ((morph text-model-morph) (event symbol) button x y)
  (declare (ignore button y))
  (let ((w (morph-target-world morph)))
    (case event
      (:pressed
       (when w
         (setf (world-keyboard-focus w) morph))
       (multiple-value-bind (dx dy) (morph-global-offset morph)
         (declare (ignore dy))
         (let ((idx (cursor-index-at-local-x morph (- x dx))))
           (setf (cursor-pos morph) idx
                 (selection-start morph) idx
                 (is-selecting morph) t)))
       (record-morph-damage morph))
      (:released
       (finish-text-selection morph)
       (record-morph-damage morph)))))

(defun current-keyboard-focus ()
  (let ((w (current-world)))
    (and w (world-keyboard-focus w))))

(defun update-text-drag-selection (x)
  (let ((fm (current-keyboard-focus)))
    (when (and (typep fm 'text-model-morph) (is-selecting fm))
      (multiple-value-bind (dx dy) (morph-global-offset fm)
        (declare (ignore dy))
        (let* ((old-pos (cursor-pos fm))
               (new-pos (cursor-index-at-local-x fm (- x dx))))
          (unless (eql old-pos new-pos)
            (setf (cursor-pos fm) new-pos)
            (record-morph-damage fm)
            t))))))
;;; ---------------focus -------------------------------------------------------
(defun world-keyboard-focus (world)
  (when (and world (active-hand world))
    (keyboard-focus (active-hand world))))

(defun (setf world-keyboard-focus) (morph world)
  (when (and world (active-hand world))
    (let ((next-focus (if (and morph (accepts-keyboard-focus-p morph))
                          morph
                          nil)))
      (setf (keyboard-focus (active-hand world)) next-focus))))

(defun morph-has-focus-p (morph)
  (let ((w (morph-target-world morph)))
    (and w (eq (world-keyboard-focus w) morph))))

(defun active-text-morph ()
  (let ((fm (current-keyboard-focus)))
    (when (and fm (typep fm 'text-model-morph))
      fm)))

;;; ============= render =======================================================
(defmethod render ((morph placed-morph))
  (draw-morph-box morph))

(defmethod render-morph ((morph placed-morph) &optional (ox 0) (oy 0))
  (let* ((lx (morph-loc-coord morph #'x))
         (ly (morph-loc-coord morph #'y))
         (nx (+ ox lx))
         (ny (+ oy ly)))
    (when (morph-visible-p morph ox oy)
      (cairo:save)
      (cairo:translate lx ly)
      (unwind-protect
           (progn
             (render morph)
             (dolist (sub (submorphs morph))
               (render-morph sub nx ny)))
        (cairo:restore)))))

(defmethod render ((morph label-morph))
  (let* ((txt (render-text-for-color morph))
         (pad-x 8)
         (ext (extent morph)))
    (when ext
      (setf (x ext) (max (or (x ext) 0)
                         (ceiling (+ (safe-text-advance-width morph txt) (* 2 pad-x))))))
    (call-next-method)
    (draw-morph-label morph txt)))

(defmethod render ((morph pluggable-button-morph))
  (let ((fill (cond
                ((is-pressed morph) "red")
                ((mouse-is-over morph) (mouse-over-color morph))
                (t (color morph)))))
    (draw-morph-box morph fill))
  (draw-morph-label morph (label morph)))

(defmethod widget-text ((morph text-model-morph))
  (cond
    ((text-getter morph)
     (funcall (text-getter morph) (model morph)))
    ((and (model morph) (typep (model morph) 'basic-morphenator-model))
     (or (entered (model morph)) ""))
    (t
     (or (text morph) ""))))

(defmethod (setf widget-text) (value (morph text-model-morph))
  (let ((str (or value "")))
    (cond
      ((text-setter morph)
       (funcall (text-setter morph) str (model morph)))
      ((and (model morph) (typep (model morph) 'basic-morphenator-model))
       (setf (entered (model morph)) str))
      (t
       (setf (text morph) str)))
    str))

(defmethod (setf widget-text) :after (value (morph text-model-morph))
  (declare (ignore value))
  (invalidate-text-metrics morph))

(defmethod render ((morph text-model-morph))
  (call-next-method)
  (let* ((ext (extent morph))
         (w (x ext))
         (h (y ext))
         (pad 4)
         (txt (format nil "~A" (widget-text morph))))
    (clamp-cursor morph)
    (cairo:save)
    (cairo:rectangle pad 0 (max 0 (- w (* 2 pad))) h)
    (cairo:clip)
    (render-prepare-to-show-text morph)
    (let* ((prefix (subseq txt 0 (cursor-pos morph)))
           (cursor-x (safe-text-advance-width morph prefix))
           (inner-w (max 0 (- w (* 2 pad)))))
      (when (> (- cursor-x (scroll-x morph)) inner-w)
        (setf (scroll-x morph) (- cursor-x inner-w)))
      (when (< (- cursor-x (scroll-x morph)) 0)
        (setf (scroll-x morph) cursor-x))
      (let ((draw-x (- pad (scroll-x morph))))
        (multiple-value-bind (sel-s sel-e) (selection-bounds morph)
          (when sel-s
            (let* ((x1 (+ draw-x (safe-text-advance-width morph (subseq txt 0 sel-s))))
                   (x2 (+ draw-x (safe-text-advance-width morph (subseq txt 0 sel-e)))))
              (gui-color:set-rgba "light blue")
              (cairo:rectangle x1 3 (- x2 x1) (- h 6))
              (cairo:fill-path))))
        (gui-color:set-rgba "red")
        (cairo:move-to draw-x 15)
        (cairo:show-text txt)
        (when (morph-has-focus-p morph)
          (let ((caret-x (+ draw-x cursor-x)))
            (gui-color:set-rgba "black")
            (cairo:set-line-width 1.5)
            (cairo:move-to caret-x 3)
            (cairo:line-to caret-x (- h 3))
            (cairo:stroke)))))
    (cairo:restore))
  (when (morph-has-focus-p morph)
    (let ((ext (extent morph)))
      (gui-color:set-rgba "blue")
      (cairo:set-line-width 2.0)
      (cairo:rectangle 1 1 (- (x ext) 2) (- (y ext) 2))
      (cairo:stroke))))

;;; ------------ world ---------------------------------------------------------
(defmethod initialize-instance :after ((morph world-morph) &key)
  (reset-morph-id-counter 1)             ; next non-world morph will be id 2
  (setf (id morph) 1
        (owner morph) nil
        (pressed-morph morph) nil
        (damage-recorder morph) nil
        (location morph) (build-location 0 0)
        (extent morph) (make-instance 'extent
                                      :x (or (car *window-size*) 600)
                                      :y (or (cdr *window-size*) 400)))
  (unless (active-hand morph)
    (setf (active-hand morph)
          (make-instance 'hand-morph
                         :mouse-over-handler (make-instance 'mouse-over-handler)))))

(defmethod render-world ((morph world-morph))
  (let ((ext (extent morph)))
    (when ext
      (draw-box-rect 0 0 (x ext) (y ext) "grey")
      (dolist (m (submorphs morph))
        (render-morph m 0 0))))
  (clear-needs-redraw morph))

(defmethod resize ((morph world-morph) w h)
  "Resize the world on window resize"
  (if (extent morph)
      (setf (x (extent morph)) w
            (y (extent morph)) h)
      (setf (extent morph) (make-instance 'extent :x w :y h)))
  (dolist (sub (submorphs morph))
    (let ((spec (layout-spec sub)))
      (when spec
        (when (proportional-width spec)
          (setf (x (extent sub)) (round (* w (proportional-width spec)))))
        (when (proportional-height spec)
          (setf (y (extent sub)) (round (* h (proportional-height spec)))))))
    (layout-submorphs sub))
  (record-damage-rect morph 0 0 w h))
