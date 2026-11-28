(declaim (optimize (speed 1) (safety 3) (debug 3)))

(in-package #:basic-morphenator-test)

;;; =========================== fixtures =======================================
(def-fixture prepare-variables (n)
  (let ((a n)
        (b 2)
        (c 3)
        (expected 6))
    (&body)))

;;; ======================= trivial sanity =====================================
(fiveam:test basic-equalities
  "basic equalities"
  (with-fixture prepare-variables (1)
    (is (eq a 1))
    (is (eq b 2))
    (is (eq c 3))
    (is (eq (+ a b c) expected))))

(def-suite basic-morphenator-suite
  :description "Framework-only tests for basic-morphenator (no GTK window, no scenes).")

(in-suite basic-morphenator-suite)

;;; ======================= input helpers ======================================
(test printable-input-validation
  "Verify printable-input-p filters control keys and non-printable characters."
  (is-true  (bm:printable-input-p "a" nil))
  (is-false (bm:printable-input-p "a" t))
  (is-false (bm:printable-input-p "" nil))
  (is-false (bm:printable-input-p nil nil))
  (is-false (bm:printable-input-p (string #\Escape) nil))
  (is-false (bm:printable-input-p (string #\Return) nil))
  (is-false (bm:printable-input-p (string #\Rubout) nil)))

(test input-and-string-helpers-handle-keys-and-backspace
  "Verify return-key-p, backspace-key-p, printable-input-p, and delete-last-char."
  (is-true  (bm::return-key-p "Return" ""))
  (is-true  (bm::return-key-p "KP_Enter" ""))
  (is-true  (bm::return-key-p "x" (string #\Return)))
  (is-false (bm::return-key-p "a" "a"))
  (is-true  (bm::backspace-key-p "BackSpace"))
  (is-false (bm::backspace-key-p "Delete"))
  (is-true  (bm::printable-input-p "x" nil))
  (is-false (bm::printable-input-p "x" t))
  (is-false (bm::printable-input-p (string #\Rubout) nil))
  (is-false (bm::printable-input-p (string #\Escape) nil))
  (is (string= "ab" (bm::delete-last-char "abc")))
  (is (string= "" (bm::delete-last-char "a")))
  (is (string= "" (bm::delete-last-char ""))))

;;; ======================= text editing (no window) ===========================
(test text-cursor-selection-and-editing
  "Verify cursor movement, shift selection, insertion, and backspace deletion."
  (let ((tm (make-instance 'bm:text-model-morph :text "hello")))
    (setf (bm:cursor-pos tm) 5)
    (bm:move-text-cursor tm 2 t)
    (is (= 2 (bm:cursor-pos tm)))
    (is (= 5 (bm:selection-start tm)))
    (bm:insert-at-cursor tm "XY")
    (is (string= "heXY" (bm:widget-text tm)))
    (is (= 4 (bm:cursor-pos tm)))
    (is (null (bm:selection-start tm)))
    (bm:delete-before-cursor tm)
    (is (string= "heX" (bm:widget-text tm)))
    (is (= 3 (bm:cursor-pos tm)))))

(test invalidate-text-metrics-clears-cached-widths
  "Verify text mutations and explicit invalidation clear cached prefix widths."
  (let ((tm (make-instance 'bm:text-model-morph :text "abcdef")))
    (bm::ensure-prefix-widths tm "abcdef")
    (is-true (gethash tm bm::*text-width-caches*))
    (bm:insert-at-cursor tm "!")
    (is (null (gethash tm bm::*text-width-caches*)))
    (bm::ensure-prefix-widths tm (bm:widget-text tm))
    (is-true (gethash tm bm::*text-width-caches*))
    (bm:invalidate-text-metrics tm)
    (is (null (gethash tm bm::*text-width-caches*)))))

(test invalidate-text-metrics-clears-cached-widths-2
  "Verify invalidating text metrics clears cached prefix widths while keeping text intact."
  (let ((tm (make-instance 'bm:text-model-morph
                           :text "hello world"
                           :extent (make-instance 'bm:extent :x 200 :y 40))))
    (bm::ensure-prefix-widths tm "hello world")
    (is-true (gethash tm bm::*text-width-caches*))
    (bm:invalidate-text-metrics tm)
    (is (null (gethash tm bm::*text-width-caches*)))
    (is (string= "hello world" (bm:widget-text tm)))))

;;; ======================= layout (no window) ================================
(test layout-submorphs-horizontal-and-vertical
  "Verify horizontal and vertical layout sizing and gap placement without overflow."
  (let* ((container (make-instance 'bm:layout-morph
                                   :direction :horizontal
                                   :separation 10
                                   :extent (make-instance 'bm:extent :x 320 :y 40)))
         (left (make-instance 'bm:boxed-morph
                              :color "red"
                              :extent (make-instance 'bm:extent :x 60 :y 40)
                              :layout-spec (make-instance 'bm:layout-spec :fixed-width 60)))
         (right (make-instance 'bm:boxed-morph
                               :color "blue"
                               :extent (make-instance 'bm:extent :x 50 :y 40)
                               :layout-spec (make-instance 'bm:layout-spec :proportional-width 1.0))))
    (bm:add-morph container left)
    (bm:add-morph container right)
    (is (= 0 (bm:x (bm:location left))))
    (is (= 60 (bm:x (bm:extent left))))
    (is (= 70 (bm:x (bm:location right))))
    (is (= 250 (bm:x (bm:extent right))))
    (is (<= (+ (bm:x (bm:location right)) (bm:x (bm:extent right)))
            (bm:x (bm:extent container))))))

(test layout-submorphs-vertical-and-proportional
  "Verify vertical layout stacking, gap placement, and proportional height sizing."
  (let* ((col (make-instance 'bm:layout-morph
                             :direction :vertical
                             :separation 6
                             :extent (make-instance 'bm:extent :x 120 :y 206)))
         (top (make-instance 'bm:boxed-morph
                             :color "red"
                             :extent (make-instance 'bm:extent :x 40 :y 40)
                             :layout-spec (make-instance 'bm:layout-spec :fixed-height 40)))
         (bottom (make-instance 'bm:boxed-morph
                                :color "blue"
                                :extent (make-instance 'bm:extent :x 40 :y 20)
                                :layout-spec (make-instance 'bm:layout-spec :proportional-height 1.0))))
    (bm:add-morph col top)
    (bm:add-morph col bottom)
    (is (= 0 (bm:y (bm:location top))))
    (is (= 40 (bm:y (bm:extent top))))
    (is (= 46 (bm:y (bm:location bottom))))
    (is (= 160 (bm:y (bm:extent bottom))))
    (is (<= (+ (bm:y (bm:location bottom)) (bm:y (bm:extent bottom)))
            (bm:y (bm:extent col))))))

(test layout-submorphs-handles-zero-proportional-sum-safely
  "Verify a proportional child in a non-proportional container does not divide by zero."
  (let* ((row (make-instance 'bm:layout-morph
                             :direction :horizontal
                             :extent (make-instance 'bm:extent :x 200 :y 30)))
         (child (make-instance 'bm:boxed-morph
                               :extent (make-instance 'bm:extent :x 50 :y 20)
                               :layout-spec (make-instance 'bm:layout-spec :proportional-width 1.0))))
    (bm:add-morph row child)
    (is (numberp (bm:x (bm:extent child))))
    (is (<= 0 (bm:x (bm:extent child))))))

(test sync-label-extent-and-layout-updates-width-and-relayouts
  "Verify label resizing respects min-width and updates sibling layout positions."
  (let* ((row (make-instance 'bm:layout-morph
                             :direction :horizontal
                             :separation 10
                             :extent (make-instance 'bm:extent :x 400 :y 30)))
         (lbl (make-instance 'bm:label-morph
                             :contents "Hi"
                             :extent (make-instance 'bm:extent :x 20 :y 20)))
         (box (make-instance 'bm:boxed-morph
                             :extent (make-instance 'bm:extent :x 50 :y 20))))
    (bm:add-morph row lbl)
    (bm:add-morph row box)
    (bm::sync-label-extent-and-layout lbl :pad-x 8 :min-width 50 :height 20)
    (is (= 50 (bm:x (bm:extent lbl))))
    (is (= 60 (bm:x (bm:location box))))

    (setf (bm::contents lbl) "A much longer greeting string")
    (bm::sync-label-extent-and-layout lbl :pad-x 8 :min-width 50 :height 20)
    (is (> (bm:x (bm:extent lbl)) 50))
    (is (= (+ (bm:x (bm:extent lbl)) 10)
           (bm:x (bm:location box))))))

;;; ======================= morph tree invariants =============================
(test add-morph-rejects-self
  (let ((m (make-instance 'bm:boxed-morph)))
    (signals error (bm:add-morph m m))))

(test add-morph-rejects-cycle
  (let ((a (make-instance 'bm:boxed-morph))
        (b (make-instance 'bm:boxed-morph))
        (c (make-instance 'bm:boxed-morph)))
    (bm:add-morph a b)
    (bm:add-morph b c)
    (signals error (bm:add-morph c a))))

(test add-morph-rejects-duplicate-child
  (let ((a (make-instance 'bm:boxed-morph))
        (b (make-instance 'bm:boxed-morph)))
    (bm:add-morph a b)
    (signals error (bm:add-morph a b))))

(test add-morph-reparents-and-relayouts-old-owner
  "Moving a child out of a layout-morph must re-flow the remaining siblings."
  (let ((row  (make-instance 'bm:layout-morph
                             :direction :horizontal
                             :separation 0
                             :extent (make-instance 'bm:extent :x 300 :y 30)))
        (a    (make-instance 'bm:boxed-morph
                             :extent (make-instance 'bm:extent :x 100 :y 30)))
        (b    (make-instance 'bm:boxed-morph
                             :extent (make-instance 'bm:extent :x 100 :y 30)))
        (c    (make-instance 'bm:boxed-morph
                             :extent (make-instance 'bm:extent :x 100 :y 30))))
    (bm:add-morph row a)
    (bm:add-morph row b)
    (bm:add-morph row c)
    (is (= 0   (bm:x (bm:location a))))
    (is (= 100 (bm:x (bm:location b))))
    (is (= 200 (bm:x (bm:location c))))

    ;; Move `a` out — b and c must shift left.
    (bm:add-morph (make-instance 'bm:world-morph) a)
    (is (= 0   (bm:x (bm:location b))))
    (is (= 100 (bm:x (bm:location c))))))

;;; ======================= id assignment =====================================
(test next-morph-id-per-world
  "Pin: worlds get id 1; morphs receive per-world ids on add-morph;
   counters are independent per world; standalone morphs have no id."
  (let* ((w1 (make-instance 'bm:world-morph))
         (a  (make-instance 'bm:boxed-morph))
         (b  (make-instance 'bm:boxed-morph))
         (w2 (make-instance 'bm:world-morph))
         (c  (make-instance 'bm:boxed-morph))
         (d  (make-instance 'bm:boxed-morph)))
    (is (= 1 (bm::id w1)))
    (is (= 1 (bm::id w2)))
    (is (null (bm::id a)))
    (is (null (bm::id b)))
    (is (null (bm::id c)))
    (is (null (bm::id d)))
    ;; w1 issues 2, 3
    (bm:add-morph w1 a)
    (bm:add-morph w1 b)
    (is (= 2 (bm::id a)))
    (is (= 3 (bm::id b)))
    ;; w2 restarts at 2 — independent counter
    (bm:add-morph w2 c)
    (is (= 2 (bm::id c)))
    ;; w1 continues 4
    (bm:add-morph w1 d)
    (is (= 4 (bm::id d)))))

;;; ======================= deferred layout ===================================
(test with-deferred-layout-batches-child-placement
  "Verify with-deferred-layout updates locations/extents once after body completes."
  (let* ((container (make-instance 'bm:layout-morph
                                   :direction :horizontal
                                   :separation 5
                                   :extent (make-instance 'bm:extent :x 210 :y 30)))
         (m1 (make-instance 'bm:boxed-morph
                            :extent (make-instance 'bm:extent :x 100 :y 30)))
         (m2 (make-instance 'bm:boxed-morph
                            :extent (make-instance 'bm:extent :x 100 :y 30))))
    (bm:with-deferred-layout ()
      (bm:add-morph container m1)
      (bm:add-morph container m2))
    (is (= 0 (bm:x (bm:location m1))))
    (is (= 105 (bm:x (bm:location m2))))))

(test with-deferred-layout-batches-and-updates-old-owner
  "Verify deferred layout defers intermediate relayouts and updates previous owners on reparenting."
  (let* ((old-row (make-instance 'bm:layout-morph
                                 :direction :horizontal
                                 :separation 10
                                 :extent (make-instance 'bm:extent :x 300 :y 40)))
         (new-row (make-instance 'bm:layout-morph
                                 :direction :horizontal
                                 :separation 5
                                 :extent (make-instance 'bm:extent :x 300 :y 40)))
         (m1 (make-instance 'bm:boxed-morph
                            :extent (make-instance 'bm:extent :x 40 :y 20)))
         (m2 (make-instance 'bm:boxed-morph
                            :extent (make-instance 'bm:extent :x 50 :y 20))))
    (bm:add-morph old-row m1)
    (bm:add-morph old-row m2)
    (is (= 50 (bm:x (bm:location m2))))
    (bm::with-deferred-layout ()
      (bm:add-morph new-row m1)
      (is (eq new-row (bm::owner m1))))
    (is (= 0 (bm:x (bm:location m2))))))

;;; ======================= viewport / rect helpers (headless) ================
(test clamp-rect-to-viewport-and-overlap-predicates
  "Verify viewport clamping and rectangle overlap/touch checks on edges and outside bounds."
  (let ((world (make-instance 'bm:world-morph)))
    (multiple-value-bind (cx cy cw ch)
        (bm::clamp-rect-to-viewport -10 -5 30 15 world)
      (is (= 0 cx))
      (is (= 0 cy))
      (is (= 20 cw))
      (is (= 10 ch)))
    (is (null (bm::clamp-rect-to-viewport 1200 1200 20 20 world))))
  (is-true  (bm::rects-overlap-or-touch-p '(:x 0 :y 0 :w 10 :h 10)
                                          '(:x 10 :y 0 :w 10 :h 10)))
  (is-false (bm::rects-overlap-or-touch-p '(:x 0 :y 0 :w 10 :h 10)
                                          '(:x 11 :y 0 :w 10 :h 10))))
