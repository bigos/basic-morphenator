(declaim (optimize (speed 2) (safety 3) (debug 0)))

(defpackage #:gui-color
  (:use #:cl)
  (:export
   set-rgba
   ))

(in-package #:gui-color)

(cffi:defcstruct gdk-rgba
  (red   :float)
  (green :float)
  (blue  :float)
  (alpha :float))

(defun color-to-rgba (color)
  (cffi:with-foreign-object (rgba '(:struct gdk-rgba))
    (let ((pointer (make-instance 'gir::struct-instance
                                  :class (gir:nget gdk::*ns* "RGBA")
                                  :this rgba)))
      (let ((valid-color (gdk:rgba-parse pointer color)))
        (cffi:with-foreign-slots ((red green blue alpha) rgba (:struct gdk-rgba))
          (list valid-color red green blue alpha))))))

(defparameter *rgba-cache* (make-hash-table :test #'equal))

(defun rgba-for-color (color)
  (multiple-value-bind (cached presentp)
      (gethash color *rgba-cache*)
    (if presentp
        cached
        (let ((parsed (color-to-rgba color)))
          (unless (first parsed)
            (error "~S is not a valid color" color))
          (let ((rgba-values (rest parsed)))
            (setf (gethash color *rgba-cache*) rgba-values)
            rgba-values)))))

(defun set-rgba (color)
  (apply #'cairo:set-source-rgba (rgba-for-color color)))
