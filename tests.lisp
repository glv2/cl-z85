;;;; This file is part of cl-z85
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :cl-z85/tests
  (:use :cl :z85 :fiveam))

(in-package :cl-z85/tests)


(def-suite cl-z85
  :description "Unit tests for cl-z85")

(in-suite cl-z85)

(test encode
  (is (string= "HelloWorld"
               (z85-encode #(#x86 #x4f #xd2 #x6f #xb5 #x59 #xf7 #x5b))))
  (is (string= "JTKVSB%%)wK0E.X)V>+}o?pNmC{O&4W4b!Ni{Lh6"
               (z85-encode #(#x8E #x0B #xDD #x69 #x76 #x28 #xB9 #x1D
                             #x8F #x24 #x55 #x87 #xEE #x95 #xC5 #xB0
                             #x4D #x48 #x96 #x3F #x79 #x25 #x98 #x77
                             #xB4 #x9C #xD9 #x06 #x3A #xEA #xD3 #xB7)))))

(test decode
  (is (equalp #(#x86 #x4f #xd2 #x6f #xb5 #x59 #xf7 #x5b)
              (z85-decode "HelloWorld")))
  (is (equalp #(#x8E #x0B #xDD #x69 #x76 #x28 #xB9 #x1D
                #x8F #x24 #x55 #x87 #xEE #x95 #xC5 #xB0
                #x4D #x48 #x96 #x3F #x79 #x25 #x98 #x77
                #xB4 #x9C #xD9 #x06 #x3A #xEA #xD3 #xB7)
              (z85-decode "JTKVSB%%)wK0E.X)V>+}o?pNmC{O&4W4b!Ni{Lh6"))))
