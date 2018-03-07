;;;; This file is part of cl-z85
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defsystem "cl-z85"
  :name "cl-z85"
  :description "ZeroMQ base-85 encoding and decoding"
  :version "1.0"
  :license "GPL-3"
  :author "Guillaume LE VAILLANT"
  :in-order-to ((test-op (test-op "cl-z85/tests")))
  :components ((:file "z85")))

(defsystem "cl-z85/tests"
  :name "cl-z85/tests"
  :description "Tests for cl-z85"
  :version "1.0"
  :license "GPL-3"
  :author "Guillaume LE VAILLANT"
  :depends-on ("cl-z85" "fiveam")
  :in-order-to ((test-op (load-op "cl-z85/tests")))
  :perform (test-op (o s)
                    (let ((tests (uiop:find-symbol* 'cl-z85
                                                    :cl-z85/tests)))
                      (uiop:symbol-call :fiveam 'run! tests)))
  :components ((:file "tests")))
