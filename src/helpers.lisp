(declaim (optimize (speed 2) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

(defvar *text-width-caches* (make-hash-table :test #'eq)
  "Maps a text-model-morph to a cache entry (:key KEY :widths VECTOR).")

(defun text-metrics-cache-key (morph str)
  (declare (ignore morph))
  (let ((cairo-active-p (and (boundp 'cairo:*context*)
                             cairo:*context*
                             t)))
    (cons str cairo-active-p)))

(defun invalidate-text-metrics (morph)
  "Clear cached prefix advance widths for MORPH."
  (when (typep morph 'text-model-morph)
    (remhash morph *text-width-caches*))
  morph)

(defun ensure-prefix-widths (morph str)
  "Return a vector of length (1+ (length STR)) where element I is the advance
width of (subseq STR 0 I), caching results per morph and text state."
  (let* ((key (text-metrics-cache-key morph str))
         (entry (gethash morph *text-width-caches*)))
    (if (and entry (equal (getf entry :key) key))
        (getf entry :widths)
        (let* ((len (length str))
               (widths (make-array (1+ len) :initial-element 0.0d0)))
          (loop for i from 1 to len
                do (setf (aref widths i)
                         (float (safe-text-advance-width morph (subseq str 0 i))
                                0.0d0)))
          (setf (gethash morph *text-width-caches*)
                (list :key key :widths widths))
          widths))))

(defun return-key-p (key-name entered)
  (or (and (stringp key-name)
           (or (string-equal key-name "Return")
               (string-equal key-name "KP_Enter")))
      (and (stringp entered)
           (= (length entered) 1)
           (member (char entered 0) '(#\Return #\Newline #\Linefeed)
                   :test #'char=))))

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

(defun safe-text-advance-width (morph str)
  "Measure STR width via cl-cairo2 when a context is active, otherwise use 15px Ubuntu Mono advance (9.0px/char)."
  (if (and (boundp 'cairo:*context*) cairo:*context*)
      (progn
        (render-prepare-to-show-text morph)
        (nth-value 4 (cairo:text-extents str)))
      (* (length str) 9.0)))

(defun clamp-cursor (morph)
  (let ((len (length (widget-text morph))))
    (setf (cursor-pos morph) (max 0 (min (cursor-pos morph) len)))))

(defun selection-bounds (morph)
  "Return (VALUES start end) of the active selection, or (VALUES nil nil)."
  (clamp-cursor morph)
  (let ((anchor (selection-start morph))
        (cur (cursor-pos morph)))
    (if (and anchor (/= anchor cur))
        (values (min anchor cur) (max anchor cur))
        (values nil nil))))

(defun clear-selection (morph)
  (setf (selection-start morph) nil
        (is-selecting morph) nil))

(defun delete-selection (morph)
  "Delete selected text if any and move cursor to start of deleted range. Returns T if deleted."
  (multiple-value-bind (s e) (selection-bounds morph)
    (when s
      (let ((txt (widget-text morph)))
        (setf (widget-text morph)
              (concatenate 'string (subseq txt 0 s) (subseq txt e)))
        (setf (cursor-pos morph) s)
        (clear-selection morph)
        (invalidate-text-metrics morph)
        t))))

(defun move-text-cursor (morph new-pos shift-p)
  (clamp-cursor morph)
  (if shift-p
      (unless (selection-start morph)
        (setf (selection-start morph) (cursor-pos morph)))
      (clear-selection morph))
  (setf (cursor-pos morph)
        (max 0 (min (length (widget-text morph)) new-pos))))

(defun insert-at-cursor (morph str)
  (delete-selection morph)
  (clamp-cursor morph)
  (let* ((txt (widget-text morph))
         (pos (cursor-pos morph)))
    (setf (widget-text morph)
          (concatenate 'string (subseq txt 0 pos) str (subseq txt pos)))
    (incf (cursor-pos morph) (length str))
    (invalidate-text-metrics morph)))

(defun delete-before-cursor (morph)
  (unless (delete-selection morph)
    (clamp-cursor morph)
    (let* ((txt (widget-text morph))
           (pos (cursor-pos morph)))
      (when (> pos 0)
        (setf (widget-text morph)
              (concatenate 'string (subseq txt 0 (1- pos)) (subseq txt pos)))
        (decf (cursor-pos morph))
        (invalidate-text-metrics morph)))))

(defun cursor-index-at-local-x (morph local-x)
  "Find the closest character index in MORPH for a click at LOCAL-X."
  (let* ((txt (widget-text morph))
         (len (length txt))
         (pad 4)
         (target-x (+ (- local-x pad) (scroll-x morph))))
    (if (or (<= target-x 0) (zerop len))
        0
        (let ((widths (ensure-prefix-widths morph txt)))
          (loop for i from 1 to len
                for prev-w = (aref widths (1- i))
                for w = (aref widths i)
                when (< target-x (/ (+ prev-w w) 2.0))
                  return (1- i)
                finally (return len))))))

(defun finish-text-selection (morph)
  (when (typep morph 'text-model-morph)
    (setf (is-selecting morph) nil)
    (when (eql (selection-start morph) (cursor-pos morph))
      (setf (selection-start morph) nil))))

(defun find-the-morph (x y)
  "Return the innermost morph at global coordinates (X, Y), or NIL."
  (let ((world (current-world)))
    (when world
      (car (last (morphs-at world x y))))))

(defun find-text-morph (morph)
  "Return the first text-model-morph in MORPH's submorph hierarchy, or NIL."
  (when morph
    (if (typep morph 'text-model-morph)
        morph
        (loop for sub in (submorphs morph)
              for found = (find-text-morph sub)
              when found return found))))

(defun clear-text-morph (text-morph)
  (setf (widget-text text-morph) ""
        (cursor-pos text-morph) 0
        (scroll-x text-morph) 0)
  (clear-selection text-morph)
  (invalidate-text-metrics text-morph))

(defun delete-char-at-cursor (text-morph)
  (unless (delete-selection text-morph)
    (clamp-cursor text-morph)
    (let* ((txt (widget-text text-morph))
           (pos (cursor-pos text-morph)))
      (when (< pos (length txt))
        (setf (widget-text text-morph)
              (concatenate 'string (subseq txt 0 pos) (subseq txt (1+ pos))))
        (invalidate-text-metrics text-morph)))))

(defun printable-input-p (entered ctrl-p)
  (and (not ctrl-p)
       entered
       (stringp entered)
       (> (length entered) 0)
       (not (member (char entered 0)
                    '(#\Escape #\Return #\Newline #\Rubout)
                    :test #'char=))))

;;; TODO implement if needed later
(defgeneric new-file (model)
  (:method ((model t))
    (declare (ignore model))
    nil))

(defun morph-ancestor-p (morph ancestor)
  "Return T if ANCESTOR is MORPH itself or appears in MORPH's owner chain."
  (loop for m = morph then (owner m)
        while m
          thereis (eq m ancestor)))

(defun rect-bounds (r)
  "Return (VALUES x1 y1 x2 y2) for plist rect R."
  (let ((x (getf r :x))
        (y (getf r :y))
        (w (getf r :w))
        (h (getf r :h)))
    (values x y (+ x w) (+ y h))))

(defun rects-overlap-or-touch-p (r1 r2)
  "Return T if R1 and R2 overlap or share/touch an edge."
  (multiple-value-bind (ax1 ay1 ax2 ay2) (rect-bounds r1)
    (multiple-value-bind (bx1 by1 bx2 by2) (rect-bounds r2)
      (and (<= (max ax1 bx1) (min ax2 bx2))
           (<= (max ay1 by1) (min ay2 by2))))))

(defun merge-rects (r1 r2)
  "Return the minimal bounding plist rectangle containing both R1 and R2."
  (multiple-value-bind (ax1 ay1 ax2 ay2) (rect-bounds r1)
    (multiple-value-bind (bx1 by1 bx2 by2) (rect-bounds r2)
      (let* ((x (min ax1 bx1))
             (y (min ay1 by1))
             (w (- (max ax2 bx2) x))
             (h (- (max ay2 by2) y)))
        (list :x x :y y :w w :h h)))))

(defun coalesce-damage-rects (rects)
  "Merge all overlapping or touching rectangles in RECTS."
  (let ((acc '()))
    (dolist (r rects (nreverse acc))
      (let ((current r)
            (remaining '()))
        (dolist (existing acc)
          (if (rects-overlap-or-touch-p current existing)
              (setf current (merge-rects current existing))
              (push existing remaining)))
        (setf acc (cons current (nreverse remaining)))))))

(defun record-damage-rect (world x y w h)
  "Record a dirty rectangle (X, Y, W, H) on WORLD and mark WORLD needing redraw."
  (when (and world (typep world 'world-morph))
    (multiple-value-bind (cx cy cw ch) (clamp-rect-to-viewport x y w h world)
      (when cx
        (let ((rect (list :x cx :y cy :w cw :h ch))
              (existing (damage-recorder world)))
          (unless (member rect existing :test #'equal)
            (setf (damage-recorder world)
                  (coalesce-damage-rects (nconc existing (list rect))))))))
    (mark-needs-redraw world)))
