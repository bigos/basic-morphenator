(declaim (optimize (speed 1) (safety 3) (debug 3)))

;; (ql:quickload :basic-morphenator/tests)
(in-package #:basic-morphenator-test)
;; (fiveam:run-all-tests)
;; (run! 'basic-equalities)

;; (setf 5am:*on-error* :debug)

;;; =========================== fixtures =======================================
(def-fixture prepare-variables (n)
  (let ((a n)
        (b 2)
        (c 3)
        (expected 6))
    ;; body
    (&body)
    ;; teardown code
    (setf a nil
          b nil
          c nil)))

;;; ======================= tests ==============================================
(fiveam:test basic-equalities
  "basic equalities"
  (with-fixture prepare-variables (1)
    (is (eq a 1))
    (is (eq b 2))
    (is (eq c 3))
    (is (eq (+ a b c) expected))))
;;; ================================================= more tests ===============

(def-suite basic-morphenator-suite
  :description "Tests for basic-morphenator event and key handling.")

(in-suite basic-morphenator-suite)

;;; =========================== fixtures =======================================
(def-fixture with-test-window ()
  (let ((win (bm:main :testing t)))
    (unwind-protect
         (&body)
      (setf bm:*basic-morphenator-model* nil
            bm:*environment* nil))))

;;; ======================= unit tests =========================================
(test printable-input-validation
  "Verify printable-input-p filters control keys and non-printable characters."
  (is-true  (bm:printable-input-p "a" nil))
  (is-false (bm:printable-input-p "a" t))
  (is-false (bm:printable-input-p "" nil))
  (is-false (bm:printable-input-p nil nil))
  (is-false (bm:printable-input-p (string #\Escape) nil))
  (is-false (bm:printable-input-p (string #\Return) nil))
  (is-false (bm:printable-input-p (string #\Rubout) nil)))

(test text-morph-editing-helpers
  "Verify delete-char-at-cursor and clear-text-morph."
  (with-fixture with-test-window ()
    (let* ((world (bm:current-world))
           (tm (bm:find-text-morph world)))
      (is-true tm)
      (setf (bm:widget-text tm) "abcd"
            (bm:cursor-pos tm) 1)
      (bm:delete-char-at-cursor tm)
      (is (string= "acd" (bm:widget-text tm)))
      (bm:clear-text-morph tm)
      (is (string= "" (bm:widget-text tm)))
      (is (= 0 (bm:cursor-pos tm))))))

;;; ==================== integration / event tests =============================
(test process-event-key-and-shortcuts
  "Verify typing requires focused text-model-morph, Return greets, and Alt+c clears."
  (with-fixture with-test-window ()
    (let* ((model (bm:the-model))
           (world (bm:current-world))
           (tm (bm:find-text-morph world)))
      (is-true tm)
      (is (null (bm:active-text-morph)))

      ;; Unfocused typing should not mutate the text morph or model
      (process-event win :key-pressed '("X" "X" 0 nil))
      (is (string= "" (bm:widget-text tm)))
      (is (string= "" (bm:entered model)))

      ;; Focus the text-model-morph and type
      (setf (bm::world-keyboard-focus world) tm)
      (is (eq tm (bm:active-text-morph)))
      (process-event win :key-pressed '("H" "H" 0 nil))
      (process-event win :key-pressed '("i" "i" 0 nil))
      (is (string= "Hi" (bm:widget-text tm)))

      ;; Trigger greeting via Return
      (process-event win :key-pressed '("" "Return" 0 nil))
      (is (string= "Hi" (bm:greeted model)))

      ;; Clear via Alt+c
      (process-event win :key-pressed '("c" "c" 0 (:ALT)))
      (is (string= "" (bm:greeted model)))
      (is (string= "" (bm:widget-text tm))))))

(test clicking-non-focusable-clears-text-focus
  "Verify clicking a button clears keyboard focus from an active text-model-morph."
  (with-fixture with-test-window ()
    (let* ((world (bm:current-world))
           (tm (bm:find-text-morph world))
           (btn (make-instance 'bm:pluggable-button-morph
                               :color "green"
                               :label "Blur"
                               :location (make-instance 'bm:location :x 10 :y 10)
                               :extent (make-instance 'bm:extent :x 80 :y 30))))
      (is-true tm)
      (bm:add-morph world btn)
      (setf (bm::world-keyboard-focus world) tm)
      (is (eq tm (bm:active-text-morph)))

      ;; Clicking the button clears keyboard focus
      (process-event win :pressed '(1 20 20))
      (is (null (bm::world-keyboard-focus world)))
      (is (null (bm:active-text-morph)))

      ;; Typing after losing focus does not edit the text morph
      (process-event win :key-pressed '("Z" "Z" 0 nil))
      (is (string= "" (bm:widget-text tm))))))

(test clicking-empty-world-clears-text-focus
  "Verify clicking empty world space clears keyboard focus from an active text-model-morph."
  (with-fixture with-test-window ()
    (let* ((world (bm:current-world))
           (tm (bm:find-text-morph world))
           (empty-patch (make-instance 'bm:boxed-morph
                                       :color "gray"
                                       :location (make-instance 'bm:location :x 10 :y 250)
                                       :extent (make-instance 'bm:extent :x 100 :y 40))))
      (is-true tm)
      (bm:add-morph world empty-patch)
      (setf (bm::world-keyboard-focus world) tm)
      (is (eq tm (bm:active-text-morph)))

      ;; Clicking a non-focusable morph in world space clears keyboard focus
      (process-event win :pressed '(1 30 260))
      (is (null (bm::world-keyboard-focus world)))
      (is (null (bm:active-text-morph)))

      ;; Subsequent typing leaves text morph untouched
      (process-event win :key-pressed '("Q" "Q" 0 nil))
      (is (string= "" (bm:widget-text tm))))))

(test process-event-resize
  "Verify :resize updates window dimensions and *window-size*."
  (with-fixture with-test-window ()
    (process-event win :resize '(800 600))
    (is (equal '(800 . 600) bm:*window-size*))))

;;; ======================= layout & hit-testing tests =======================
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

(test morphs-at-hit-testing-chain
  "Verify morphs-at returns outermost-to-innermost chain and respects bounds."
  (with-fixture with-test-window ()
    (let* ((world (bm:current-world))
           (chain-inside (bm:morphs-at world 15 15))
           (chain-outside (bm:morphs-at world 9999 9999)))
      (is-true chain-inside)
      (is (eq world (first chain-inside)))
      (is (null chain-outside)))))

(test nested-morph-hit-testing-returns-innermost-child
  "Verify morphs-at chain order and find-the-morph selecting nested child over containers."
  (with-fixture with-test-window ()
    (let* ((world (bm:current-world))
           (container (make-instance 'bm:layout-morph
                                     :direction :horizontal
                                     :location (make-instance 'bm:location :x 20 :y 30)
                                     :extent (make-instance 'bm:extent :x 200 :y 60)))
           (button (make-instance 'bm:pluggable-button-morph
                                  :color "green"
                                  :label "Nested"
                                  :location (make-instance 'bm:location :x 10 :y 10)
                                  :extent (make-instance 'bm:extent :x 80 :y 30))))
      (bm:add-morph world container)
      (bm:add-morph container button)
      ;; Global coordinates of button: x in [30, 110), y in [40, 70)
      (let* ((px 45)
             (py 50)
             (chain (bm:morphs-at world px py))
             (hit (bm:find-the-morph px py)))
        (is (equal chain (list world container button)))
        (is (eq button hit))
        (is (not (eq container hit)))
        (is (not (eq world hit)))))))

;;; ======================= widget & cursor/selection tests ==================
(test button-press-triggers-greet-me
  "Pressing a pluggable-button-morph invokes its action-selector and updates greeted."
  (with-fixture with-test-window ()
    (let* ((model (bm:the-model))
           (clicked nil)
           (btn (make-instance 'bm:pluggable-button-morph
                               :color "green"
                               :extent (make-instance 'bm:extent :x 100 :y 30)
                               :label "Test"
                               :action-selector (lambda ()
                                                  (setf clicked t)
                                                  (bm:greet-me model)))))
      (setf (bm:entered model) "Ada")
      (bm:handle-event-mouse-button btn :pressed 1 10 10)
      (is-true clicked)
      (is (string= "Ada" (bm:greeted model))))))

(test button-press-and-release-outside-clears-pressed-state
  "Verify pressing a button sets is-pressed and releasing outside clears pressed state."
  (with-fixture with-test-window ()
    (let* ((world (bm:current-world))
           (model (bm:the-model))
           (press-count 0)
           (btn (make-instance 'bm:pluggable-button-morph
                               :color "green"
                               :label "Click"
                               :location (make-instance 'bm:location :x 10 :y 10)
                               :extent (make-instance 'bm:extent :x 80 :y 30)
                               :action-selector (lambda ()
                                                  (incf press-count)
                                                  (bm:greet-me model)))))
      (bm:add-morph world btn)
      (setf (bm:entered model) "Grace")

      ;; Press inside the button
      (process-event win :pressed '(1 20 20))
      (is-true (bm::is-pressed btn))
      (is (eq btn (bm::pressed-morph world)))
      (is (= 1 press-count))
      (is (string= "Grace" (bm:greeted model)))

      ;; Release outside the button (e.g., on world background)
      (process-event win :released '(1 350 350))
      (is-false (bm::is-pressed btn))
      (is (null (bm::pressed-morph world))))))

(test text-drag-selection-updates-cursor-and-selection
  "Verify dragging across a focused text-model-morph selects text and clears on release."
  (with-fixture with-test-window ()
    (let* ((world (bm:current-world))
           (tm (bm:find-text-morph world)))
      (is-true tm)
      (setf (bm:widget-text tm) "abcdef"
            (bm:scroll-x tm) 0
            (bm:x (bm:extent tm)) 300
            (bm:y (bm:extent tm)) 30)
      (multiple-value-bind (dx dy) (bm::morph-global-offset tm)
        (let* ((pad 4)
               (mid-y (+ dy 10))
               (w1 (bm::safe-text-advance-width tm (subseq (bm:widget-text tm) 0 1)))
               (w2 (bm::safe-text-advance-width tm (subseq (bm:widget-text tm) 0 2)))
               (w4 (bm::safe-text-advance-width tm (subseq (bm:widget-text tm) 0 4)))
               (w5 (bm::safe-text-advance-width tm (subseq (bm:widget-text tm) 0 5)))
               (press-target (- (/ (+ w1 w2) 2.0) 0.5))
               (drag-target  (- (/ (+ w4 w5) 2.0) 0.5))
               (press-x (+ dx pad (- press-target (bm:scroll-x tm))))
               (drag-x  (+ dx pad (- drag-target (bm:scroll-x tm)))))
          (process-event win :pressed (list 1 press-x mid-y))
          (is (eq tm (bm:world-keyboard-focus world)))
          (is (= 1 (bm:cursor-pos tm)))
          (is (= 1 (bm:selection-start tm)))
          (is-true (bm:is-selecting tm))

          (process-event win :motion (list drag-x mid-y))
          (is (= 4 (bm:cursor-pos tm)))
          (is (= 1 (bm:selection-start tm)))

          (process-event win :released (list 1 drag-x mid-y))
          (is-false (bm:is-selecting tm))
          (is (= 1 (bm:selection-start tm)))
          (is (= 4 (bm:cursor-pos tm))))))))

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
