(declaim (optimize (speed 2) (safety 3) (debug 3)))
(in-package #:basic-morphenator)

(defvar *text-width-caches* (make-hash-table :test #'eq)
  "Maps a text-model-morph to a cache entry (:key KEY :widths VECTOR).")

(defun text-metrics-cache-key (morph str)
  (declare (ignore morph))
  (let ((cairo-active-p (and (boundp 'cairo:*context*) cairo:*context* t)))
    (cons str cairo-active-p)))

(defun invalidate-text-metrics (morph)
  "Clear cached prefix advance widths for MORPH."
  (when (typep morph 'text-model-morph)
    (remhash morph *text-width-caches*))
  morph)

(defun ensure-prefix-widths (morph str)
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

(defun safe-text-advance-width (morph str)
  "Measure STR width via cl-cairo2 when a context is active, otherwise 9.0px/char."
  (if (and (boundp 'cairo:*context*) cairo:*context*)
      (progn
        (render-prepare-to-show-text morph)
        (nth-value 4 (cairo:text-extents str)))
      (* (length str) 9.0)))

(defun clamp-cursor (morph)
  (let ((len (length (widget-text morph))))
    (setf (cursor-pos morph) (max 0 (min (cursor-pos morph) len)))))

(defun selection-bounds (morph)
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
