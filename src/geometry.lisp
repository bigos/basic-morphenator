(declaim (optimize (speed 1) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

;;; ============================================================================
;;; Morph geometry: local coordinates, global offsets, and the location
;;; machinery that layout uses to place children.
;;;
;;; Loaded after classes.lisp (needs morph, placed-morph, layout-morph,
;;; location, extent) and before damage.lisp (record-morph-damage needs
;;; morph-global-offset) and before methods.lisp (add-morph needs
;;; refresh-location; layout-submorphs needs set-location-xy).
;;; ============================================================================

;;; ---------------- location construction -------------------------------------
(defun build-location (x y)
  "Make a fresh location object at (X, Y)."
  (make-instance 'location :x x :y y))

(defun ensure-extent (morph)
  "Return MORPH's extent, creating a zero-sized one if absent."
  (or (extent morph)
      (setf (extent morph) (make-instance 'extent))))

(defun set-location-xy (morph x y)
  "Update MORPH's location coordinates to (X, Y) in place when possible."
  (let ((loc (location morph)))
    (if loc
        (setf (x loc) x
              (y loc) y)
        (setf (location morph) (build-location x y)))
    loc))

;;; ---------------- local coordinates -----------------------------------------
(defun morph-loc-coord (morph axis-fn)
  "Return MORPH's own local coordinate along the axis selected by AXIS-FN,
defaulting to 0 when MORPH has no location."
  (let ((loc (and (typep morph 'placed-morph) (location morph))))
    (or (and loc (funcall axis-fn loc)) 0)))

(defmethod morph-location-x ((morph morph))
  (morph-loc-coord morph #'x))

(defmethod morph-location-y ((morph morph))
  (morph-loc-coord morph #'y))

;;; ---------------- sibling layout helpers ------------------------------------
(defun layout-gap (morph)
  "Separation to use between children of MORPH (default 0 for layout-morph)."
  (if (and morph (typep morph 'layout-morph))
      (or (separation morph) 0)
      0))

(defun layout-padding-x (morph)
  (if (and morph (typep morph 'layout-morph))
      (or (padding-x morph) 0)
      0))

(defun layout-padding-y (morph)
  (if (and morph (typep morph 'layout-morph))
      (or (padding-y morph) 0)
      0))

(defmethod prev-sibling-axis-sum ((morph morph) axis-fn)
  "Sum the sizes of MORPH's preceding siblings along AXIS-FN, including gaps."
  (let* ((own (owner morph))
         (gap (layout-gap own)))
    (loop for m in (submorphs own)
          for idx from 0
          until (eq m morph)
          sum (+ (let ((ext (extent m)))
                   (if ext (funcall axis-fn ext) 0))
                 (if (> idx 0) gap 0)))))

(defmethod prev-sibling-widths-sum ((morph morph))
  (prev-sibling-axis-sum morph #'x))

(defmethod prev-sibling-heights-sum ((morph morph))
  (prev-sibling-axis-sum morph #'y))

;;; ---------------- refresh a child's location from its owner -----------------
;;; In Cuis, layout children get their locations overwritten by the owner.
(defmethod refresh-location ((morph morph))
  (when (owner morph)
    (refresh-location-with-owner morph (owner morph))))

(defmethod refresh-location-with-owner ((morph morph) (owner-morph layout-morph))
  "Recompute MORPH's local position from its siblings under OWNER-MORPH."
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
  "Plain (non-layout) owner: child keeps its own local offset; nesting is
handled by the cairo translate, nothing to recompute."
  (unless (location morph)
    (setf (location morph) (build-location 0 0))))

;;; ---------------- global offsets --------------------------------------------
(defun morph-global-offset-axis (morph axis-fn)
  "Accumulate MORPH's local coordinate along AXIS-FN up its owner chain."
  (loop for m = morph then (owner m)
        while m
        sum (morph-loc-coord m axis-fn)))

(defun morph-global-offset (morph)
  "Return (VALUES global-x global-y) for MORPH."
  (values (morph-global-offset-axis morph #'x)
          (morph-global-offset-axis morph #'y)))

;;; ---------------- tree predicates -------------------------------------------
(defun morph-ancestor-p (morph ancestor)
  "Return T if ANCESTOR is MORPH itself or appears in MORPH's owner chain."
  (loop for m = morph then (owner m)
        while m
        thereis (eq m ancestor)))
