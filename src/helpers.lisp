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

(defun delete-last-char (str)
  (let ((s (or str "")))
    (if (> (length s) 0)
        (subseq s 0 (1- (length s)))
        "")))

(defun backspace-key-p (key-name)
  (and (stringp key-name)
       (string-equal key-name "BackSpace")))

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
