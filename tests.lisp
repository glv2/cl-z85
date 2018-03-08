;;;; This file is part of cl-z85
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :cl-z85/tests
  (:use :cl :z85 :fiveam :octet-streams))

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
                             #xB4 #x9C #xD9 #x06 #x3A #xEA #xD3 #xB7))))
  (is (string= ""
               (z85-encode #())))
  (is (string= "06"
               (z85-encode #(6))))
  (is (string= "0ib"
               (z85-encode #(6 5))))
  (is (string= "0SPf"
               (z85-encode #(6 5 4))))
  (is (string= "1{Cbi"
               (z85-encode #(6 5 4 3))))
  (is (string= "1{Cbi02"
               (z85-encode #(6 5 4 3 2))))
  (is (string= "1{Cbi063"
               (z85-encode #(6 5 4 3 2 1))))
  (is (string= "1{Cbi0if3"
               (z85-encode #(6 5 4 3 2 1 0))))
  (is (string= "1{Cbi0S-r3"
               (z85-encode #(6 5 4 3 2 1 0 255))))
  (is (string= "04"
               (z85-encode #(6 5 4 3 2 1 0 255) 2 3))))

(test decode
  (is (equalp #(#x86 #x4f #xd2 #x6f #xb5 #x59 #xf7 #x5b)
              (z85-decode "HelloWorld")))
  (is (equalp #(#x8E #x0B #xDD #x69 #x76 #x28 #xB9 #x1D
                #x8F #x24 #x55 #x87 #xEE #x95 #xC5 #xB0
                #x4D #x48 #x96 #x3F #x79 #x25 #x98 #x77
                #xB4 #x9C #xD9 #x06 #x3A #xEA #xD3 #xB7)
              (z85-decode "JTKVSB%%)wK0E.X)V>+}o?pNmC{O&4W4b!Ni{Lh6")))
  (is (equalp #()
              (z85-decode "")))
  (is (equalp #(6)
              (z85-decode "06")))
  (is (equalp #(6 5)
              (z85-decode "0ib")))
  (is (equalp #(6 5 4)
              (z85-decode "0SPf")))
  (is (equalp #(6 5 4 3)
              (z85-decode "1{Cbi")))
  (is (equalp #(6 5 4 3 2)
              (z85-decode "1{Cbi02")))
  (is (equalp #(6 5 4 3 2 1)
              (z85-decode "1{Cbi063")))
  (is (equalp #(6 5 4 3 2 1 0)
              (z85-decode "1{Cbi0if3")))
  (is (equalp #(6 5 4 3 2 1 0 255)
              (z85-decode "1{Cbi0S-r3")))
  (is (equalp #(4)
              (z85-decode "65404987" 3 5))))

(test encode-stream
  (is (string= "HelloWorld"
               (with-output-to-string (string)
                 (with-octet-input-stream (data #(#x86 #x4f #xd2 #x6f #xb5 #x59 #xf7 #x5b))
                   (z85-encode-stream data string)))))
  (is (string= "JTKVSB%%)wK0E.X)V>+}o?pNmC{O&4W4b!Ni{Lh6"
               (with-output-to-string (string)
                 (with-octet-input-stream (data #(#x8E #x0B #xDD #x69 #x76 #x28 #xB9 #x1D
                                                  #x8F #x24 #x55 #x87 #xEE #x95 #xC5 #xB0
                                                  #x4D #x48 #x96 #x3F #x79 #x25 #x98 #x77
                                                  #xB4 #x9C #xD9 #x06 #x3A #xEA #xD3 #xB7))
                   (z85-encode-stream data string)))))
  (is (string= ""
               (with-output-to-string (string)
                 (with-octet-input-stream (data #())
                   (z85-encode-stream data string)))))
  (is (string= "06"
               (with-output-to-string (string)
                 (with-octet-input-stream (data #(6))
                   (z85-encode-stream data string)))))
  (is (string= "0ib"
               (with-output-to-string (string)
                 (with-octet-input-stream (data #(6 5))
                   (z85-encode-stream data string)))))
  (is (string= "0SPf"
               (with-output-to-string (string)
                 (with-octet-input-stream (data #(6 5 4))
                   (z85-encode-stream data string)))))
  (is (string= "1{Cbi"
               (with-output-to-string (string)
                 (with-octet-input-stream (data #(6 5 4 3))
                   (z85-encode-stream data string)))))
  (is (string= "1{Cbi02"
               (with-output-to-string (string)
                 (with-octet-input-stream (data #(6 5 4 3 2))
                   (z85-encode-stream data string)))))
  (is (string= "1{Cbi063"
               (with-output-to-string (string)
                 (with-octet-input-stream (data #(6 5 4 3 2 1))
                   (z85-encode-stream data string)))))
  (is (string= "1{Cbi0if3"
               (with-output-to-string (string)
                 (with-octet-input-stream (data #(6 5 4 3 2 1 0))
                   (z85-encode-stream data string)))))
  (is (string= "1{Cbi0S-r3"
               (with-output-to-string (string)
                 (with-octet-input-stream (data #(6 5 4 3 2 1 0 255))
                   (z85-encode-stream data string))))))

(test decode-stream
  (is (equalp #(#x86 #x4f #xd2 #x6f #xb5 #x59 #xf7 #x5b)
              (with-octet-output-stream (data)
                (with-input-from-string (string "HelloWorld")
                  (z85-decode-stream string data)))))
  (is (equalp #(#x8E #x0B #xDD #x69 #x76 #x28 #xB9 #x1D
                #x8F #x24 #x55 #x87 #xEE #x95 #xC5 #xB0
                #x4D #x48 #x96 #x3F #x79 #x25 #x98 #x77
                #xB4 #x9C #xD9 #x06 #x3A #xEA #xD3 #xB7)
              (with-octet-output-stream (data)
                (with-input-from-string (string "JTKVSB%%)wK0E.X)V>+}o?pNmC{O&4W4b!Ni{Lh6")
                  (z85-decode-stream string data)))))
  (is (equalp #()
              (with-octet-output-stream (data)
                (with-input-from-string (string "")
                  (z85-decode-stream string data)))))
  (is (equalp #(6)
              (with-octet-output-stream (data)
                (with-input-from-string (string "06")
                  (z85-decode-stream string data)))))
  (is (equalp #(6 5)
              (with-octet-output-stream (data)
                (with-input-from-string (string "0ib")
                  (z85-decode-stream string data)))))
  (is (equalp #(6 5 4)
              (with-octet-output-stream (data)
                (with-input-from-string (string "0SPf")
                  (z85-decode-stream string data)))))
  (is (equalp #(6 5 4 3)
              (with-octet-output-stream (data)
                (with-input-from-string (string "1{Cbi")
                  (z85-decode-stream string data)))))
  (is (equalp #(6 5 4 3 2)
              (with-octet-output-stream (data)
                (with-input-from-string (string "1{Cbi02")
                  (z85-decode-stream string data)))))
  (is (equalp #(6 5 4 3 2 1)
              (with-octet-output-stream (data)
                (with-input-from-string (string "1{Cbi063")
                  (z85-decode-stream string data)))))
  (is (equalp #(6 5 4 3 2 1 0)
              (with-octet-output-stream (data)
                (with-input-from-string (string "1{Cbi0if3")
                  (z85-decode-stream string data)))))
  (is (equalp #(6 5 4 3 2 1 0 255)
              (with-octet-output-stream (data)
                (with-input-from-string (string "1{Cbi0S-r3")
                  (z85-decode-stream string data))))))
