(declaim (optimize (speed 2) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

(defmethod print-object ((obj location) stream)
  (print-object-inner obj stream))

(defmethod print-object ((obj boxed-morph) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s" (list
                         :id
                         (id obj)
                         :color
                         (color obj)))))

(defun return-key-p (key-name entered)
  (or (and (stringp key-name)
           (or (string-equal key-name "Return")
               (string-equal key-name "KP_Enter")))
      (and (stringp entered)
           (= (length entered) 1)
           (member (char entered 0) '(#\Return #\Newline #\Linefeed)
                   :test #'char=))))

;;; ================= helpers ==================================================
(defmethod greet-me ((model basic-morphenator-model))
  (setf (greeted model) (entered model)))

(defun draw-box-rect (x y w h color)
  (gui-color:set-rgba (or color "yellow"))
  (cairo:rectangle x y w h)
  (cairo:fill-path))

(defun draw-morph-box (morph &optional color)
  (let ((ext (extent morph)))
    (draw-box-rect 0 0 (x ext) (y ext) (or color (color morph)))))

(defun draw-morph-label (morph text &key (color "white"))
  (render-prepare-to-show-text morph)
  (gui-color:set-rgba color)
  (cairo:show-text (or text "")))

(defun find-text-morph (root)
  (when root
    (if (typep root 'text-model-morph)
        root
        (loop for child in (submorphs root)
              for found = (find-text-morph child)
              when found return found))))

(defun active-text-morph ()
  (let* ((model (the-model))
         (world (and model (world model))))
    (or (and world
             (focused-morph world)
             (typep (focused-morph world) 'text-model-morph)
             (focused-morph world))
        (find-text-morph world))))

(defun delete-last-char (str)
  (let ((s (or str "")))
    (if (> (length s) 0)
        (subseq s 0 (1- (length s)))
        "")))

(defun backspace-key-p (key-name)
  (and (stringp key-name)
       (string-equal key-name "BackSpace")))

(defun make-layout-spec (&key fixed-width fixed-height
                           proportional-width proportional-height
                           (off-axis-edge-width 0))
  (make-instance 'layout-spec
                 :fixed-width fixed-width
                 :fixed-height fixed-height
                 :proportional-width proportional-width
                 :proportional-height proportional-height
                 :off-axis-edge-width off-axis-edge-width))

(defmethod layout-submorphs ((morph layout-morph))
  (let* ((subs (submorphs morph))
         (n (length subs)))
    (when (> n 0)
      (let* ((gap (or (separation morph) 10))
             (total-gap (* gap (max 0 (1- n))))
             (dir (direction morph))
             (avail-w (max 0 (- (x (extent morph))
                                (if (eq dir :horizontal) total-gap 0))))
             (avail-h (max 0 (- (y (extent morph))
                                (if (eq dir :vertical) total-gap 0))))
             (fixed-sum 0)
             (prop-sum 0.0))
        ;; 1. Tally fixed sizes and proportional weights along the main axis
        (dolist (sub subs)
          (let ((spec (layout-spec sub)))
            (if (eq dir :horizontal)
                (cond
                  ((and spec (proportional-width spec))
                   (incf prop-sum (proportional-width spec)))
                  ((and spec (fixed-width spec))
                   (incf fixed-sum (fixed-width spec)))
                  (t
                   (incf fixed-sum (x (extent sub)))))
                (cond
                  ((and spec (proportional-height spec))
                   (incf prop-sum (proportional-height spec)))
                  ((and spec (fixed-height spec))
                   (incf fixed-sum (fixed-height spec)))
                  (t
                   (incf fixed-sum (y (extent sub))))))))
        ;; 2. Resize each child along main and cross axes, then position it
        (let ((rem (max 0 (- (if (eq dir :horizontal) avail-w avail-h) fixed-sum))))
          (dolist (sub subs)
            (let* ((spec (layout-spec sub))
                   (new-w (cond
                            ((and (eq dir :horizontal) spec (proportional-width spec) (> prop-sum 0))
                             (round (* rem (/ (proportional-width spec) prop-sum))))
                            ((and spec (fixed-width spec))
                             (fixed-width spec))
                            ((and (eq dir :vertical) spec (proportional-width spec))
                             (round (* avail-w (proportional-width spec))))
                            (t (x (extent sub)))))
                   (new-h (cond
                            ((and (eq dir :vertical) spec (proportional-height spec) (> prop-sum 0))
                             (round (* rem (/ (proportional-height spec) prop-sum))))
                            ((and spec (fixed-height spec))
                             (fixed-height spec))
                            ((and (eq dir :horizontal) spec (proportional-height spec))
                             (round (* avail-h (proportional-height spec))))
                            (t (y (extent sub))))))
              (setf (x (extent sub)) new-w
                    (y (extent sub)) new-h)
              (refresh-location sub)
              (layout-submorphs sub))))))))

(defun text-advance-width (str)
  "Return the horizontal pixel advance of STR under the current Cairo font."
  (multiple-value-bind (a b c d x-adv)
      (cairo:text-extents str)
    (if (numberp x-adv)
        x-adv
        ;; Fallback if binding returns a text-extents struct/object in A
        (cairo:text-extents-x-advance a))))

(defun clamp-cursor (morph)
  (let ((len (length (widget-text morph))))
    (setf (cursor-pos morph) (max 0 (min (cursor-pos morph) len)))))

(defun insert-at-cursor (morph str)
  (clamp-cursor morph)
  (let* ((txt (widget-text morph))
         (pos (cursor-pos morph)))
    (setf (widget-text morph)
          (concatenate 'string (subseq txt 0 pos) str (subseq txt pos)))
    (incf (cursor-pos morph) (length str))))

(defun delete-before-cursor (morph)
  (clamp-cursor morph)
  (let* ((txt (widget-text morph))
         (pos (cursor-pos morph)))
    (when (> pos 0)
      (setf (widget-text morph)
            (concatenate 'string (subseq txt 0 (1- pos)) (subseq txt pos)))
      (decf (cursor-pos morph)))))

(defun cursor-index-at-local-x (morph local-x)
  "Find the closest character index in MORPH for a click at LOCAL-X."
  (let* ((txt (widget-text morph))
         (len (length txt))
         (pad 4)
         (target-x (+ (- local-x pad) (scroll-x morph))))
    (if (<= target-x 0)
        0
        (loop with prev-w = 0
              for i from 1 to len
              for w = (text-advance-width (subseq txt 0 i))
              when (< target-x (/ (+ prev-w w) 2.0))
                return (1- i)
              do (setf prev-w w)
              finally (return len)))))

;;; ================= init gui =================================================
;;; try to implement: https://drcuis.github.io/DesignGUI/Quick-Start.html
(defmethod init-gui ((model basic-morphenator-model))
  (let
      ((world (make-instance 'world-morph))

       (view (make-instance 'layout-morph
                            :color "pink"
                            :direction :vertical
                            :extent (make-instance 'extent :x 600 :y 400)
                            :layout-spec (make-layout-spec :proportional-width 1.0
                                                           :proportional-height 1.0)))
       (row  (make-instance 'layout-morph
                            :color "orange"
                            :direction :horizontal
                            :extent (make-instance 'extent :x 600 :y 30)
                            :layout-spec (make-layout-spec :proportional-width 1.0
                                                           :fixed-height 30)))
       (label (make-instance 'label-morph
                             :color "brown"
                             :extent (make-instance 'extent :x 60 :y 30)
                             :contents "Name:"))
       (input (make-instance 'text-model-morph
                             :color "white"
                             :extent (make-instance 'extent :x 50 :y 30)
                             :layout-spec (make-layout-spec :proportional-width 1.0)
                             :model model))
       (button (make-instance 'pluggable-button-morph
                              :action-selector (lambda () (greet-me (the-model)))
                              :color "green"
                              :mouse-over-color "lime"
                              :extent (make-instance 'extent :x 60 :y 30)
                              :label "Greet"))

       (greeting-label (make-instance 'label-morph
                                      :color "blue"
                                      :extent (make-instance 'extent :x 50 :y 20)
                                      :contents (lambda () (or (greeted model) "Hmm...")))))

    (progn
      (setf (world model) world)
      (add-morph (world model) view)
      (add-morph view row)
      (add-morph row label)
      (add-morph row input)
      (add-morph row button)
      (add-morph view greeting-label))
    (setf (active-hand world)
          (make-instance 'hand-morph
                         :mouse-over-handler (make-instance 'mouse-over-handler)))))

(defun build-location (x y)
  (make-instance 'location
                 :x x
                 :y y))

(defmethod initialize-instance :after ((morph morph) &key)
  (unless (typep morph 'world-morph)
    (setf (id morph) (1+ (last-morph-id morph)))
    (setf (last-morph-id morph) (id morph))))

;;; =============== add morph ==================================================
(defgeneric layout-submorphs (morph)
  (:method ((morph morph)) nil))

(defmethod add-morph ((morph morph) (submorph morph))
  (setf (owner submorph) morph)
  (setf (submorphs morph) (append (submorphs morph) (list submorph)))
  (refresh-location submorph)
  (layout-submorphs morph))

;;; ================== location ================================================
;;; Ensure every placed-morph starts with a valid (0, 0) location
(defmethod initialize-instance :after ((morph placed-morph) &key)
  (unless (location morph)
    (setf (location morph) (build-location 0 0))))

(defun morph-location-axis (morph axis-fn)
  (if (location morph)
      (or (funcall axis-fn (location morph)) 0)
      0))

(defmethod morph-location-x ((morph morph))
  (morph-location-axis morph #'x))

(defmethod morph-location-y ((morph morph))
  (morph-location-axis morph #'y))

(defmethod prev-sibling-axis-sum ((morph morph) axis-fn)
  (let ((gap 10))
    (loop for m in (submorphs (owner morph))
          until (eq m morph)
          sum (+ (~> m extent (funcall axis-fn _)) gap))))

(defmethod prev-sibling-widths-sum ((morph morph))
  (prev-sibling-axis-sum morph #'x))

(defmethod prev-sibling-heights-sum ((morph morph))
  (prev-sibling-axis-sum morph #'y))

;;; in cuis layout children get their locations overwritten by owner
(defmethod refresh-location ((morph morph))
  (when (owner morph)
    (refresh-location-with-owner morph (owner morph))))

(defmethod refresh-location-with-owner ((morph morph) (owner-morph layout-morph))
  (ecase (direction owner-morph)
    (:vertical
     (setf (location morph)
           (build-location (morph-location-x morph)
                           (prev-sibling-heights-sum morph))))
    (:horizontal
     (setf (location morph)
           (build-location (prev-sibling-widths-sum morph)
                           (morph-location-y morph))))))

(defmethod refresh-location-with-owner ((morph morph) (owner-morph morph))
  ;; plain (non-layout) owner: child keeps its own local offset;
  ;; nesting is handled by the cairo translate, nothing to recompute
  (unless (location morph)
    (setf (location morph) (build-location 0 0))))

(defmethod refresh-location-with-owner :after ((morph morph) (owner-morph layout-morph))
  (let ((oe (extent owner-morph)) (loc (location morph)) (ext (extent morph)))
    (when (and oe loc ext
               (or (< (x loc) 0) (< (y loc) 0)
                   (> (+ (x loc) (x ext)) (x oe))
                   (> (+ (y loc) (y ext)) (y oe))))
      (warn "morph ~s has elements outside its owner" morph))))

;;; ---------------- mouse location --------------------------------------------
(defun morph-global-offset (morph)
  (values
   (loop for m = morph then (owner m) while m
         sum (if (and (location m) (x (location m))) (x (location m)) 0))
   (loop for m = morph then (owner m) while m
         sum (if (and (location m) (y (location m))) (y (location m)) 0))))

(defmethod contains-point-p ((morph morph) x y)
  (declare (ignore x y))
  nil)
(defmethod contains-point-p ((morph boxed-morph) x y)
  (let ((ext (extent morph)))
    (when ext
      (multiple-value-bind (dx dy) (morph-global-offset morph)
        (let ((lx (- x dx)) (ly (- y dy)))
          (and (<= 0 lx) (< lx (x ext))
               (<= 0 ly) (< ly (y ext))))))))

(defgeneric morphs-at (morph x y)
  (:documentation "Morphs containing the point, outermost first.")
  (:method ((morph morph) x y) nil)
  (:method ((morph boxed-morph) x y)
    (when (contains-point-p morph x y)
      (cons morph
            (loop for sub in (reverse (submorphs morph))
                  for chain = (morphs-at sub x y)
                  when chain return chain)))))

(defun process-mouse-over (handler new-chain)
  (let* ((old (mouse-over-morphs handler))
         (left    (remove-if (lambda (m) (member m new-chain)) old))
         (entered (remove-if (lambda (m) (member m old)) new-chain)))
    (dolist (m (reverse left))
      (when (handles-mouse-over-p m) (mouse-leave m)))
    (dolist (m entered)
      (when (handles-mouse-over-p m) (mouse-enter m)))
    (setf (mouse-over-morphs handler) new-chain)))

;;; ================= events ===================================================
(defmethod handle-event-mouse-button ((morph null) (event symbol) button x y)
  (warn "UNHANDLED mouse button event ~s at ~a for ~s" event (format nil "~,2f x ~,2f" x y) morph)
  (setf morph (world (the-model)))
  (warn "pressed is ~S" (pressed-morph morph))
  (when (and (eq event :released)
             (pressed-morph morph))
    (warn "releasing the button")
    (setf (is-pressed (pressed-morph morph)) nil)
    (setf (pressed-morph morph) nil)
    (warn "after releasing ~s" (pressed-morph morph))))
(defmethod handle-event-mouse-button ((morph morph) (event symbol) button x y)
  (warn "UNHANDLED mouse button event ~s at ~a for ~s" event (format nil "~,2f x ~,2f" x y) morph)
  (warn "pressed is ~S" (pressed-morph morph))
  (when (and (eq event :released)
             (pressed-morph morph))
    (warn "releasing the button")
    (setf (is-pressed (pressed-morph morph)) nil)
    (setf (pressed-morph morph) nil)
    (warn "after releasing ~s" (pressed-morph morph))))
(defmethod handle-event-mouse-button ((morph pluggable-button-morph) (event symbol) button x y)
  (case event
    (:pressed
     (setf (is-pressed morph) T)
     (setf (pressed-morph morph) morph)
     (when (and (contains-point-p morph x y)
                (action-selector morph))
       (funcall (action-selector morph))))
    (:released
     (when (pressed-morph morph)
       (setf (is-pressed (pressed-morph morph)) nil)
       (setf (pressed-morph morph) nil))
     (when (and (contains-point-p morph x y)
                (secondary-action-selector morph))
       (funcall (secondary-action-selector morph))))))
(defmethod handle-event-mouse-button ((morph text-model-morph) (event symbol) button x y)
  (declare (ignore button y))
  (when (eq event :pressed)
    (setf (focused-morph morph) morph)
    (multiple-value-bind (dx dy) (morph-global-offset morph)
      (declare (ignore dy))
      (render-prepare-to-show-text morph)
      (setf (cursor-pos morph)
            (cursor-index-at-local-x morph (- x dx))))))

(defgeneric handles-mouse-over-p (morph)
  (:method ((morph morph)) nil))
(defmethod handles-mouse-over-p ((morph pluggable-button-morph)) t)

(defgeneric mouse-enter (morph))
(defgeneric mouse-leave (morph))
(defmethod mouse-enter ((morph pluggable-button-morph))
  (setf (mouse-is-over morph) t))
(defmethod mouse-leave ((morph pluggable-button-morph))
  (setf (mouse-is-over morph) nil))

;;; =============== content ====================================================
;; label-morph contents may be a string or a function
(defmethod label-text ((morph label-morph))
  (let ((c (contents morph)))
    (if (functionp c)
        (funcall c)
        c)))

;;; ============= render =======================================================
(defmethod render-prepare-to-show-text ((morph placed-morph))
  (cairo:select-font-face "Ubuntu Mono" :normal :normal)
  (cairo:set-font-size 15)
  (cairo:move-to 0 15))   ; baseline 15px below local origin

;;; apparently tere is function cairo:traslate tx ty
;;; that may allow to get rid of absolute-x absolute-y
;; https://www.cairographics.org/manual-1.17.2/cairo-Transformations.html#cairo-translate

(defmethod render :around ((morph placed-morph))
  (cairo:save)                                    ; match your binding's style
  (cairo:translate (x (location morph)) (y (location morph)))
  (unwind-protect (call-next-method)
    (cairo:restore)))

(defmethod render ((morph placed-morph))
  (draw-morph-box morph))
(defmethod render :after ((morph morph))
  (loop for m in (submorphs morph)
        do (render m)))

(defmethod render ((morph label-morph))
  (call-next-method)
  (draw-morph-label morph (render-text-for-color morph)))

(defmethod render-text-for-color ((morph label-morph))
  (or (label-text morph) ""))

(defmethod render ((morph pluggable-button-morph))
  (let ((fill (cond
                ((is-pressed morph) "red")
                ((mouse-is-over morph) (mouse-over-color morph))
                (t (color morph)))))
    (draw-morph-box morph fill))
  (draw-morph-label morph (label morph)))

(defgeneric widget-text (morph)
  (:documentation "Return the current text displayed by MORPH."))

(defgeneric (setf widget-text) (value morph)
  (:documentation "Update the text stored or bound by MORPH."))

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

(defmethod render ((morph text-model-morph))
  (draw-morph-box morph)
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
           (cursor-x (text-advance-width prefix))
           (inner-w (max 0 (- w (* 2 pad)))))
      (when (> (- cursor-x (scroll-x morph)) inner-w)
        (setf (scroll-x morph) (- cursor-x inner-w)))
      (when (< (- cursor-x (scroll-x morph)) 0)
        (setf (scroll-x morph) cursor-x))
      (let ((draw-x (- pad (scroll-x morph))))
        (gui-color:set-rgba "red")
        (cairo:move-to draw-x 15)
        (cairo:show-text txt)
        (when (eq (focused-morph morph) morph)
          (let ((caret-x (+ draw-x cursor-x)))
            (gui-color:set-rgba "black")
            (cairo:set-line-width 1.5)
            (cairo:move-to caret-x 3)
            (cairo:line-to caret-x (- h 3))
            (cairo:stroke)))))
    (cairo:restore))
  (when (eq (focused-morph morph) morph)
    (let ((ext (extent morph)))
      (gui-color:set-rgba "blue")
      (cairo:set-line-width 2.0)
      (cairo:rectangle 1 1 (- (x ext) 2) (- (y ext) 2))
      (cairo:stroke))))


;;; ------------ world ---------------------------------------------------------
(defmethod initialize-instance :after ((morph world-morph) &key)
  (setf (last-morph-id morph) 1
        (id morph) 1)
  (setf (owner morph) nil
        (pressed-morph morph) nil
        (location morph) (build-location 0 0)
        (extent morph) (make-instance 'extent
                                      :x (or (car *window-size*) 600)
                                      :y (or (cdr *window-size*) 400))))

(defmethod render-world ((morph world-morph))
  (let ((ext (extent morph)))
    (draw-box-rect 0 0 (x ext) (y ext) "red"))
  (loop for m in (submorphs morph) do (render m)))

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
    (layout-submorphs sub)))
