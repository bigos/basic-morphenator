(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:basic-morphenator)


(defclass/std morph ()
  ((owner)
   (submorphs)
   (properties)
   (id)
   (private-display-bounds)
   (last-morph-id :allocation :class)))

(defclass/std placed-morph (morph)
  ((location)
   (layout-spec)))

(defclass/std box-morph (placed-morph)
  ((extent)))

(defclass/std colored-box-morph (box-morph)
  ((color)))

(defclass/std paste-up-morph (colored-box-morph)
  ((background-image)
   (background-image-data)))

(defclass/std world-morph (paste-up-morph)
  ((active-hand)
   (canvas)
   (damage-recorder)
   (step-list)
   (last-cycle-time)
   (alarms)
   (last-alarm-time)
   (deferred-ui-messages)
   (drawing-failing-morphs)
   (wait-delay)
   (pause)
   (last-cycle-had-any-event)
   (taskbar)
   (scale-factor :allocation :class)))
