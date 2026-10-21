(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:basic-morphenator)


(defclass/std morph ()
  ((owner)
   (submorphs)
   (properties)
   (id)
   (private-display-bounds)
   (last-morph-id :allocation :class)))

(defclass/std world (morph)
  ())

(defclass/std colored-box (morph)
  ())
