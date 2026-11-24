(declaim (optimize (speed 2) (safety 3) (debug 3)))
(in-package #:basic-morphenator)

;;; Morph positioning, global offsets, and layout prerequisites.

(defun build-location (x y)
  (make-instance 'location :x x :y y))

(defun set-location-xy (morph x y)
  "Update MORPH's location coordinates to (X, Y) in place when possible."
  (let ((loc (location morph)))
    (if loc
        (setf (x loc) x (y loc) y)
        (setf (location morph) (build-location x y)))
    loc))

(defun morph-loc-coord (morph axis-fn)
  (let ((loc (and (typep morph 'placed-morph) (location morph))))
    (or (and loc (funcall axis-fn loc)) 0)))

(defmethod morph-location-x ((morph morph)) (morph-loc-coord morph #'x))
(defmethod morph-location-y ((morph morph)) (morph-loc-coord morph #'y))

(defun layout-gap (morph)
  (if (and morph (typep morph 'layout-morph))
      (or (separation morph) 10)
      10))

(defmethod prev-sibling-axis-sum ((morph morph) axis-fn)
  (let* ((own (owner morph))
         (gap (layout-gap own)))
    (loop for m in (submorphs own)
          for idx from 0
          until (eq m morph)
          sum (+ (funcall axis-fn (extent m))
                 (if (> idx 0) gap 0)))))

(defmethod prev-sibling-widths-sum ((morph morph))
  (prev-sibling-axis-sum morph #'x))
(defmethod prev-sibling-heights-sum ((morph morph))
  (prev-sibling-axis-sum morph #'y))

(defmethod refresh-location ((morph morph))
  (when (owner morph)
    (refresh-location-with-owner morph (owner morph))))

(defmethod refresh-location-with-owner ((morph morph) (owner-morph layout-morph))
  (ecase (direction owner-morph)
    (:vertical
     (set-location-xy morph
                      (morph-location-x morph)
                      (prev-sibling-heights-sum morph)))
    (:horizontal
     (set-location-xy morph
                      (prev-sibling-widths-sum morph)
                      (morph-location-y morph)))))

(defmethod refresh-location-with-owner ((morph morph) (owner-morph morph))
  (unless (location morph)
    (setf (location morph) (build-location 0 0))))

(defun morph-global-offset-axis (morph axis-fn)
  (loop for m = morph then (owner m)
        while m
        sum (morph-loc-coord m axis-fn)))

(defun morph-global-offset (morph)
  (values (morph-global-offset-axis morph #'x)
          (morph-global-offset-axis morph #'y)))

(defun morph-ancestor-p (morph ancestor)
  "Return T if ANCESTOR is MORPH itself or appears in MORPH's owner chain."
  (loop for m = morph then (owner m)
        while m
        thereis (eq m ancestor)))
