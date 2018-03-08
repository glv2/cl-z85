;;;; This file is part of cl-z85
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :z85
  (:nicknames :cl-z85)
  (:use :cl)
  (:export #:z85-encode
           #:z85-decode
           #:z85-encode-stream
           #:z85-decode-stream))

(in-package :z85)


(defun z85-encode (data &optional (start 0) end)
  "Return the Z85 encoding of the bytes of DATA between START and END."
  (let* ((encoder (load-time-value "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.-:+=^!/*?&<>()[]{}@%$#" t))
         (data-length (- (or end (length data)) start))
         (string-length (multiple-value-bind (full-blocks remaining-bytes)
                            (floor data-length 4)
                          (+ (* 5 full-blocks)
                             (if (plusp remaining-bytes)
                                 (1+ remaining-bytes)
                                 0))))
         (string (make-string string-length)))
    (do ((tmp (make-string 5))
         (n 0)
         (count 0)
         (i 0)
         (j 0))
        ((= i data-length))
      (setf n (+ (* n 256) (elt data (+ start i))))
      (incf i)
      (incf count)
      (when (or (= count 4) (= i data-length))
        (do ((q n)
             (r 0)
             (k 0 (1+ k)))
            ((= k 5))
          (multiple-value-setq (q r) (floor q 85))
          (setf (char tmp k) (char encoder r)))
        (loop for k from count downto 0 do
          (setf (char string j) (char tmp k))
          (incf j))
        (setf count 0)
        (setf n 0)))
    string))

(defun z85-decode (string &optional (start 0) end)
  "Return the bytes encoded as Z85 characters between START and END in STRING."
  (let* ((decoder (load-time-value #(#x00 #x44 #x00 #x54 #x53 #x52 #x48 #x00
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
                                   t))
         (string-length (- (or end (length string)) start))
         (data-length (multiple-value-bind (full-blocks remaining-characters)
                          (floor string-length 5)
                        (+ (* 4 full-blocks)
                           (if (>= remaining-characters 2)
                               (1- remaining-characters)
                               0))))
         (data (make-array data-length :element-type '(unsigned-byte 8))))
    (do ((tmp (make-array 4 :element-type '(unsigned-byte 8)))
         (n 0)
         (count 0)
         (i 0)
         (j 0))
        ((= i string-length))
      (setf n (+ (* n 85)
                 (aref decoder (- (char-code (char string (+ start i)))
                                  32))))
      (incf i)
      (incf count)
      (when (or (= count 5) (= i string-length))
        (do ((q n)
             (r 0)
             (k 0 (1+ k)))
            ((= k 4))
          (multiple-value-setq (q r) (floor q 256))
          (setf (aref tmp k) r))
        (loop for k from (- count 2) downto 0 do
          (setf (aref data j) (aref tmp k))
          (incf j))
        (setf count 0)
        (setf n 0)))
    data))

(defun z85-encode-stream (data-stream z85-stream)
  "Write the Z85 encoding of DATA-STREAM to Z85-STREAM."
  (let ((buffer (make-array 4 :element-type '(unsigned-byte 8))))
    (loop for n = (read-sequence buffer data-stream)
          do (write-sequence (z85-encode buffer 0 n) z85-stream)
          until (< n 4))
    t))

(defun z85-decode-stream (z85-stream data-stream)
  "Write the bytes encoded in Z85-STREAM to DATA-STREAM."
  (let ((buffer (make-string 5)))
    (loop for n = (read-sequence buffer z85-stream)
          do (write-sequence (z85-decode buffer 0 n) data-stream)
          until (< n 5))
    t))
