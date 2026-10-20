(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:basic-morphenator)


(defclass/std morph ()
  ())

(defclass/std world (morph)
  ())
