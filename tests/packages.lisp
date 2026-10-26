(defpackage #:basic-morphenator-test
  (:use #:cl
        #:basic-morphenator
        #:fiveam)
  (:local-nicknames (#:bm #:basic-morphenator))
  (:export #:run!
          #:basic-morphenator-suite)
  (:import-from #:basic-morphenator
                #:process-event))
