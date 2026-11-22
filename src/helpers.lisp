(declaim (optimize (speed 2) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

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
    (incf (cursor-pos morph) (length str))))

(defun delete-before-cursor (morph)
  (unless (delete-selection morph)
    (clamp-cursor morph)
    (let* ((txt (widget-text morph))
           (pos (cursor-pos morph)))
      (when (> pos 0)
        (setf (widget-text morph)
              (concatenate 'string (subseq txt 0 (1- pos)) (subseq txt pos)))
        (decf (cursor-pos morph))))))

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
              for w = (safe-text-advance-width morph (subseq txt 0 i))
              when (< target-x (/ (+ prev-w w) 2.0))
                return (1- i)
              do (setf prev-w w)
              finally (return len)))))

(defun finish-text-selection (morph)
  (when (typep morph 'text-model-morph)
    (setf (is-selecting morph) nil)
    (when (eql (selection-start morph) (cursor-pos morph))
      (setf (selection-start morph) nil))))

(defun find-the-morph (x y)
  (car (last (morphs-at (current-world) x y))))

(defun clear-text-morph (text-morph)
  (setf (widget-text text-morph) ""
        (cursor-pos text-morph) 0
        (scroll-x text-morph) 0)
  (clear-selection text-morph))


(defun delete-char-at-cursor (text-morph)
  (unless (delete-selection text-morph)
    (clamp-cursor text-morph)
    (let* ((txt (widget-text text-morph))
           (pos (cursor-pos text-morph)))
      (when (< pos (length txt))
        (setf (widget-text text-morph)
              (concatenate 'string (subseq txt 0 pos) (subseq txt (1+ pos))))))))
