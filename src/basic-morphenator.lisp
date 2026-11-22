(declaim (optimize (speed 2) (safety 3) (debug 3)))

;;; ============================================================================
;; (ql:quickload :basic-morphenator)
(in-package #:basic-morphenator)
;;; (main)

(defvar *basic-morphenator-model* nil)
(defvar *environment* nil)
(defvar *window-size* nil)

;;; ============================================================================

(defun print-object-inner (obj  stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a"
            (loop for sl in (sb-mop:class-slots (class-of obj))
                  for slot-name = (sb-mop:slot-definition-name sl)
                  collect (cons slot-name
                                (if (slot-boundp obj slot-name)
                                    (prin1-to-string (slot-value obj slot-name))
                                     "---unbound---" ))))))

;;; ============================================================================

(defclass/std basic-morphenator-window (boxes:boxes-window) ())

;;; ============================================================================

(defun the-model () *basic-morphenator-model*)

(defun the-handler ()
  (mouse-over-handler (active-hand (world (the-model)))))

(defun morph-world (morph)
  (loop for m = morph then (owner m)
        while m
        when (typep m 'world-morph) return m))

(defun current-world ()
  (let ((model (the-model)))
    (and model (world model))))

(defun morph-target-world (morph)
  (or (morph-world morph)
      (current-world)))
