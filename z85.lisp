;;;; This file is part of cl-z85
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :z85
  (:nicknames :cl-z85)
  (:use :cl)
  (:export #:z85-encode
           #:z85-decode))

(in-package :z85)


(defun z85-encode (data)
  "Return the Z85 encoding of the bytes of DATA."
  (let ((encoder (load-time-value "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.-:+=^!/*?&<>()[]{}@%$#" t)))
    (with-output-to-string (encoded)
      (do ((data-length (length data))
           (n 0)
           (tmp (make-string 5))
           (count 0)
           (i 0))
          ((= i data-length))
        (setf n (+ (* n 256) (elt data i)))
        (incf i)
        (incf count)
        (when (or (= count 4) (= i data-length))
          (do ((q n)
               (r 0)
               (j 0 (1+ j)))
              ((= j 5))
            (multiple-value-setq (q r) (floor q 85))
            (setf (char tmp j) (char encoder r)))
          (loop for j from count downto 0 do
            (write-char (char tmp j) encoded))
          (setf count 0)
          (setf n 0))))))

(defun z85-decode (string)
  "Return the bytes encoded in a Z85 STRING."
  (let ((decoder (load-time-value #(#x00 #x44 #x00 #x54 #x53 #x52 #x48 #x00
                                    #x4B #x4C #x46 #x41 #x00 #x3F #x3E #x45
                                    #x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07
                                    #x08 #x09 #x40 #x00 #x49 #x42 #x4A #x47
                                    #x51 #x24 #x25 #x26 #x27 #x28 #x29 #x2A
                                    #x2B #x2C #x2D #x2E #x2F #x30 #x31 #x32
                                    #x33 #x34 #x35 #x36 #x37 #x38 #x39 #x3A
                                    #x3B #x3C #x3D #x4D #x00 #x4E #x43 #x00
                                    #x00 #x0A #x0B #x0C #x0D #x0E #x0F #x10
                                    #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18
                                    #x19 #x1A #x1B #x1C #x1D #x1E #x1F #x20
                                    #x21 #x22 #x23 #x4F #x00 #x50 #x00 #x00)
                                  t)))
    (map '(simple-array (unsigned-byte 8) (*))
         #'char-code
         (with-output-to-string (data)
           (let ((encoded-length (length string)))
             (do ((n 0)
                  (tmp (make-string 4))
                  (count 0)
                  (i 0))
                 ((= i encoded-length))
               (setf n (+ (* n 85) (aref decoder (- (char-code (char string i)) 32))))
               (incf i)
               (incf count)
               (when (or (= count 5) (= i encoded-length))
                 (do ((q n)
                      (r 0)
                      (j 0 (1+ j)))
                     ((= j 4))
                   (multiple-value-setq (q r) (floor q 256))
                   (setf (char tmp j) (code-char r)))
                 (loop for j from (- count 2) downto 0 do
                   (write-char (char tmp j) data))
                 (setf count 0)
                 (setf n 0))))))))
