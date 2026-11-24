(declaim (optimize (speed 2) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

;;; ============================================================================
;;; Low-level damage tracking: dirty-rectangle bookkeeping, viewport clamping,
;;; and redraw flag helpers.
;;;
;;; Loaded after classes.lisp (needs world-morph, extent, damage-recorder,
;;; needs-redraw-p) and before both methods.lisp and helpers.lisp so that
;;; record-damage-rect / mark-needs-redraw / viewport-bounds are visible
;;; without forward references.
;;; ============================================================================

;;; ---------------- redraw flags ----------------------------------------------
(defun reset-damage-recorder (&optional (world (current-world)))
  (when (and world (typep world 'world-morph))
    (setf (damage-recorder world) nil)))

(defun mark-needs-redraw (&optional (world (current-world)))
  (when (and world (typep world 'world-morph))
    (setf (needs-redraw-p world) t)))

(defun clear-needs-redraw (&optional (world (current-world)))
  (when (and world (typep world 'world-morph))
    (setf (needs-redraw-p world) nil)
    (reset-damage-recorder world)))

(defun world-needs-redraw-p (&optional (world (current-world)))
  (and world (typep world 'world-morph) (needs-redraw-p world)))

;;; ---------------- viewport --------------------------------------------------
(defun viewport-bounds (&optional (world (current-world)))
  "Return (VALUES width height) for the visible viewport, or (VALUES nil nil) if unknown."
  (let ((ext (and world (typep world 'world-morph) (extent world))))
    (cond
      ((and ext (x ext) (y ext))
       (values (x ext) (y ext)))
      ((and (consp *window-size*) (car *window-size*) (cdr *window-size*))
       (values (car *window-size*) (cdr *window-size*)))
      (t
       (values nil nil)))))

(defun clamp-rect-to-viewport (x y w h world)
  "Normalize and clamp rectangle [X, X+W) x [Y, Y+H) to WORLD's viewport when known.
Returns (VALUES cx cy cw ch) or NIL if empty/outside viewport."
  (let* ((x1 (min x (+ x w)))
         (y1 (min y (+ y h)))
         (x2 (max x (+ x w)))
         (y2 (max y (+ y h))))
    (multiple-value-bind (vw vh) (viewport-bounds world)
      (when vw
        (setf x1 (max 0 x1)
              x2 (min vw x2)))
      (when vh
        (setf y1 (max 0 y1)
              y2 (min vh y2)))
      (let ((cw (- x2 x1))
            (ch (- y2 y1)))
        (when (and (> cw 0) (> ch 0))
          (values x1 y1 cw ch))))))

;;; ---------------- rect plists -----------------------------------------------
(defun rect-bounds (r)
  "Return (VALUES x1 y1 x2 y2) for plist rect R."
  (let ((x (getf r :x))
        (y (getf r :y))
        (w (getf r :w))
        (h (getf r :h)))
    (values x y (+ x w) (+ y h))))

(defun rects-overlap-or-touch-p (r1 r2)
  "Return T if R1 and R2 overlap or share/touch an edge."
  (multiple-value-bind (ax1 ay1 ax2 ay2) (rect-bounds r1)
    (multiple-value-bind (bx1 by1 bx2 by2) (rect-bounds r2)
      (and (<= (max ax1 bx1) (min ax2 bx2))
           (<= (max ay1 by1) (min ay2 by2))))))

(defun merge-rects (r1 r2)
  "Return the minimal bounding plist rectangle containing both R1 and R2."
  (multiple-value-bind (ax1 ay1 ax2 ay2) (rect-bounds r1)
    (multiple-value-bind (bx1 by1 bx2 by2) (rect-bounds r2)
      (let* ((x (min ax1 bx1))
             (y (min ay1 by1))
             (w (- (max ax2 bx2) x))
             (h (- (max ay2 by2) y)))
        (list :x x :y y :w w :h h)))))

(defun coalesce-damage-rects (rects)
  "Merge all overlapping or touching rectangles in RECTS."
  (let ((acc '()))
    (dolist (r rects (nreverse acc))
      (let ((current r)
            (remaining '()))
        (dolist (existing acc)
          (if (rects-overlap-or-touch-p current existing)
              (setf current (merge-rects current existing))
              (push existing remaining)))
        (setf acc (cons current (nreverse remaining)))))))

;;; ---------------- record ----------------------------------------------------
(defun record-damage-rect (world x y w h)
  "Record a dirty rectangle (X, Y, W, H) on WORLD and mark WORLD needing redraw."
  (when (and world (typep world 'world-morph))
    (multiple-value-bind (cx cy cw ch) (clamp-rect-to-viewport x y w h world)
      (when cx
        (let ((rect (list :x cx :y cy :w cw :h ch))
              (existing (damage-recorder world)))
          (unless (member rect existing :test #'equal)
            (setf (damage-recorder world)
                  (coalesce-damage-rects (nconc existing (list rect))))))))
    (mark-needs-redraw world)))

;;; ---------------- record-morph-damage (moved from methods.lisp) -----------
(defun record-morph-damage (morph)
  "Record MORPH's global bounding rectangle on its target world, if any."
  (when morph
    (let ((world (morph-target-world morph)))
      (when (and world (typep world 'world-morph))
        (if (and (typep morph 'boxed-morph) (extent morph)
                 (x (extent morph)) (y (extent morph)))
            (multiple-value-bind (dx dy) (morph-global-offset morph)
              (record-damage-rect world dx dy (x (extent morph)) (y (extent morph))))
            (mark-needs-redraw world))))))
