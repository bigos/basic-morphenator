(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defpackage #:basic-morphenator
  (:use #:cl)
  (:import-from :serapeum
                ~>
                ->)
  (:import-from :defclass-std
                defclass/std)
  (:local-nicknames (#:sy #:sycamore))
  (:export
   main
   draw-window
   process-event
   menu-bar
   ;; open-file
   ;; cancel-open-file
   ;; save-file
   ;; cancel-save-file
   ))

;;; ============================================================================
;; (ql:quickload :basic-morphenator)
(in-package #:basic-morphenator)
;;; (main)

(defparameter *basic-morphenator-model* nil)
(defparameter *environment* nil)

;;; ============================================================================

(defun print-object-inner (obj  stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a"
            (loop for sl in (sb-mop:class-slots (class-of obj))
                  for slot-name = (sb-mop:slot-definition-name sl)
                  collect (cons slot-name
                                (if (slot-boundp obj slot-name)
                                    (format nil "~S" (slot-value obj slot-name))
                                    (format nil "---unbound---" )))))))

;;; ============================================================================
(defclass/std basic-morphenator-model (boxes:model)
  ((text :std (sy:rope ""))))

(defclass/std basic-morphenator-window (boxes:boxes-window) (()))

;;; ============================================================================

;;; morph classes and methods sit in separate files
