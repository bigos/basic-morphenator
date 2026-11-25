(declaim (optimize (speed 1) (safety 3) (debug 3)))
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

(defun move-text-cursor (morph new-pos shift-p)
  (clamp-cursor morph)
  (if shift-p
      (unless (selection-start morph)
        (setf (selection-start morph) (cursor-pos morph)))
      (clear-selection morph))
  (setf (cursor-pos morph)
        (max 0 (min (length (widget-text morph)) new-pos))))

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

(defun sync-label-extent-and-layout (morph &key (pad-x 8) (min-width 50) (height 20))
  "Resize MORPH to fit its current rendered text plus horizontal padding, then
  re-layout its owner if inside a layout-morph."
  (when (and morph (typep morph 'label-morph))
    (let* ((txt (render-text-for-color morph))
           (measured-w (ceiling (+ (safe-text-advance-width morph txt) (* 2 pad-x))))
           (new-w (max min-width measured-w))
           (ext (ensure-extent morph)))
      (setf (x ext) new-w)
      (when height
        (setf (y ext) height))
      (record-morph-damage morph)
      (let ((own (owner morph)))
        (when (and own (typep own 'layout-morph))
          (layout-submorphs own)
          (record-morph-damage own))))))

(defun replace-text-range (morph start end replacement new-cursor-pos &key clear-sel-p)
  "Replace text in MORPH over half-open range [START, END) with REPLACEMENT,
set CURSOR-POS to NEW-CURSOR-POS, optionally clear selection, and invalidate metrics."
  (let* ((txt (widget-text morph))
         (len (length txt))
         (s (max 0 (min start len)))
         (e (max s (min end len))))
    (setf (widget-text morph)
          (concatenate 'string (subseq txt 0 s) replacement (subseq txt e)))
    (when clear-sel-p
      (clear-selection morph))
    (setf (cursor-pos morph)
          (max 0 (min new-cursor-pos (length (widget-text morph)))))
    (invalidate-text-metrics morph)))

(defun delete-selection (morph)
  (multiple-value-bind (s e) (selection-bounds morph)
    (when s
      (replace-text-range morph s e "" s :clear-sel-p t)
      t)))

(defun insert-at-cursor (morph str)
  (if (delete-selection morph)
      (let ((pos (cursor-pos morph)))
        (replace-text-range morph pos pos str (+ pos (length str))))
      (progn
        (clamp-cursor morph)
        (let ((pos (cursor-pos morph)))
          (replace-text-range morph pos pos str (+ pos (length str)))))))

(defun delete-before-cursor (morph)
  (unless (delete-selection morph)
    (clamp-cursor morph)
    (let ((pos (cursor-pos morph)))
      (when (> pos 0)
        (replace-text-range morph (1- pos) pos "" (1- pos))))))

(defun delete-char-at-cursor (text-morph)
  (unless (delete-selection text-morph)
    (clamp-cursor text-morph)
    (let* ((pos (cursor-pos text-morph))
           (len (length (widget-text text-morph))))
      (when (< pos len)
        (replace-text-range text-morph pos (1+ pos) "" pos)))))

(defun clear-text-morph (morph)
  "Replace all text in MORPH with the empty string, reset cursor and selection,
and return MORPH."
  (when (typep morph 'text-model-morph)
    (setf (widget-text morph) "")
    (clear-selection morph)
    (setf (cursor-pos morph) 0))
  morph)
