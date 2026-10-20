(in-package #:borba-cad/image)

;;;; Reading compressed data: DEFLATE (RFC 1951) inside the zlib wrapper (RFC 1950), which PNG files and PDF streams
;;;; use. The code tables are read through a lookup table of as many bits as the longest code of the block, so that
;;;; a symbol takes one step. What is read comes from a file the program did not write, so every length, distance and
;;;; position is checked, and a damaged or oversized stream is reported, never followed.

(deftype octets ()
  "A plain vector of bytes."
  '(simple-array (unsigned-byte 8) (*)))

(defun make-octets (size)
  "Return a vector of SIZE bytes, all zero.

SIZE: how many bytes."
  (make-array size :element-type '(unsigned-byte 8) :initial-element 0))

(defun image-failure (control
                      &rest arguments)
  "Signal IMAGE-ERROR with a sentence.

CONTROL: an English format control, translated before it is filled in.
ARGUMENTS: its format arguments."
  (error 'base:image-error
         :reason (apply #'i18n:trf control arguments)))

(defconstant +default-inflate-limit+ (* 256 1024 1024)
  "The most bytes a compressed stream may expand to, when the caller sets no other limit.")

(defconstant +maximum-code-bits+ 15
  "Longest Huffman code of DEFLATE, in bits.")

(defconstant +end-of-block-symbol+ 256
  "The literal/length symbol that ends a compressed block.")

(defconstant +length-symbols+ 29
  "How many length symbols follow the end-of-block symbol.")

(defconstant +distance-symbols+ 30
  "How many distance symbols DEFLATE defines.")

(defconstant +maximum-literal-symbols+ 286
  "How many literal/length symbols a dynamic block may declare.")

(defconstant +code-length-symbols+ 19
  "How many symbols the code that codes the code lengths has.")

(defconstant +block-stored+ 0
  "The type of a block that is copied as it is.")

(defconstant +block-fixed+ 1
  "The type of a block that uses the fixed codes of the standard.")

(defconstant +block-dynamic+ 2
  "The type of a block that brings its own codes.")

(defconstant +zlib-deflate-method+ 8
  "The compression method of a zlib header that means DEFLATE.")

(defconstant +zlib-largest-window+ 7
  "The largest window size a zlib header may name, as the exponent above 8 of its CINFO field.")

(defconstant +zlib-header-check+ 31
  "The number the first two bytes of a zlib stream, as a 16-bit number, are a multiple of.")

(defconstant +zlib-dictionary-flag+ #x20
  "The flag of a zlib header that says a preset dictionary is needed.")

(defconstant +adler-size+ 4
  "Bytes of the checksum that ends a zlib stream.")

(defconstant +repeat-previous-symbol+ 16
  "The code-length symbol that repeats the length before it.")

(defconstant +repeat-zero-short-symbol+ 17
  "The code-length symbol that is a short run of zeros.")

(defconstant +repeat-zero-long-symbol+ 18
  "The code-length symbol that is a long run of zeros.")

(declaim (type (simple-array fixnum (*))
               *length-bases* *length-extra-bits* *distance-bases* *distance-extra-bits* *code-length-order*))

(defparameter *length-bases*
  (coerce '(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258)
          '(simple-array fixnum (*)))
  "The shortest match that each length symbol stands for.")

(defparameter *length-extra-bits*
  (coerce '(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0)
          '(simple-array fixnum (*)))
  "How many bits follow each length symbol to say which match it is.")

(defparameter *distance-bases*
  (coerce '(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049 3073 4097 6145 8193 12289
            16385 24577)
          '(simple-array fixnum (*)))
  "The nearest distance that each distance symbol stands for.")

(defparameter *distance-extra-bits*
  (coerce '(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13)
          '(simple-array fixnum (*)))
  "How many bits follow each distance symbol to say which distance it is.")

(defparameter *code-length-order*
  (coerce '(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15)
          '(simple-array fixnum (*)))
  "The order in which a dynamic block lists the lengths of the codes that code its lengths.")

;;; Decoding tables

(defun reverse-bits (code
                     width)
  "Return the WIDTH low bits of CODE in the opposite order, as DEFLATE sends codes from their top bit.

CODE: the number.
WIDTH: how many bits of it count."
  (declare (type fixnum code width))
  (let ((result 0))
    (declare (type fixnum result))
    (dotimes (index width result)
      (setf result (logior (ash result 1) (logand 1 (ash code (- index))))))))

(defun build-decoding-table (lengths
                             start
                             count)
  "Return a lookup table for the Huffman code whose symbol lengths are COUNT numbers from START in LENGTHS, and the
number of bits it is indexed by. An entry holds the symbol times sixteen plus the length of its code, and is zero for
bits that start no code. Signals IMAGE-ERROR for lengths that cannot belong to a prefix code.

LENGTHS: a vector of code lengths, zero for a symbol that does not occur.
START: the position of the first length that counts.
COUNT: how many symbols the code has."
  (declare (type (simple-array fixnum (*)) lengths)
           (type fixnum start count))
  (let ((per-length (make-array (1+ +maximum-code-bits+) :element-type 'fixnum :initial-element 0))
        (next-code (make-array (1+ +maximum-code-bits+) :element-type 'fixnum :initial-element 0))
        (bits 1)
        (left 1))
    (declare (type fixnum bits left))
    (dotimes (symbol count)
      (incf (aref per-length (aref lengths (+ start symbol)))))
    (setf (aref per-length 0) 0)
    (loop for length from 1 to +maximum-code-bits+
          do (setf left (- (* 2 left) (aref per-length length)))
             (when (minusp left)
               (image-failure "The compressed data of this picture is damaged."))
             (when (plusp (aref per-length length))
               (setf bits length)))
    (let ((code 0))
      (declare (type fixnum code))
      (loop for length from 1 to +maximum-code-bits+
            do (setf code (ash (+ code (aref per-length (1- length))) 1))
               (setf (aref next-code length) code)))
    (let ((table (make-array (ash 1 bits) :element-type '(unsigned-byte 16) :initial-element 0)))
      (dotimes (symbol count)
        (let ((length (aref lengths (+ start symbol))))
          (when (plusp length)
            (let ((reversed (reverse-bits (aref next-code length) length)))
              (incf (aref next-code length))
              (loop for index of-type fixnum from reversed below (ash 1 bits) by (ash 1 length)
                    do (setf (aref table index) (logior (ash symbol 4) length)))))))
      (values table bits))))

(defun fixed-lengths ()
  "Return the code lengths of the fixed literal/length code followed by those of the fixed distance code."
  (let ((lengths (make-array (+ 288 +distance-symbols+) :element-type 'fixnum :initial-element 0)))
    (loop for symbol from 0 below 288
          do (setf (aref lengths symbol)
                   (cond ((< symbol 144) 8)
                         ((< symbol 256) 9)
                         ((< symbol 280) 7)
                         (t 8))))
    (loop for symbol from 0 below +distance-symbols+
          do (setf (aref lengths (+ 288 symbol)) 5))
    lengths))

(defvar *fixed-tables* nil
  "The decoding tables of the fixed codes as (literal table, its bits, distance table, its bits), made when first
needed.")

(defun fixed-tables ()
  "Return the decoding tables of the fixed codes as a list: literal table, its bits, distance table, its bits."
  (or *fixed-tables*
      (setf *fixed-tables*
            (let ((lengths (fixed-lengths)))
              (multiple-value-bind (literal-table literal-bits)
                  (build-decoding-table lengths
                                        0
                                        288)
                (multiple-value-bind (distance-table distance-bits)
                    (build-decoding-table lengths
                                          288
                                          +distance-symbols+)
                  (list literal-table literal-bits distance-table distance-bits)))))))

;;; The bits being read

(defstruct (bit-source (:conc-name source-))
  "Where the bits of a compressed stream come from: the bytes, the next one to read, and the bits taken from them
that are not used yet, lowest first. Past the end of the bytes zero bits are supplied, and counted, so that reading
a short stream ends in a report and not in a wild read."
  (input (make-octets 0) :type octets)
  (position 0 :type fixnum)
  (buffer 0 :type (unsigned-byte 62))
  (count 0 :type fixnum)
  (overrun 0 :type fixnum))

(declaim (inline fill-bits peek-bits drop-bits take-bits))

(defun fill-bits (source
                  wanted)
  "Make sure the buffer of a source holds at least WANTED bits.

SOURCE: the bit source.
WANTED: how many bits, at most 32."
  (declare (type bit-source source)
           (type fixnum wanted)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (loop while (< (source-count source) wanted)
        do (let ((input (source-input source))
                 (position (source-position source)))
             (setf (source-buffer source)
                   (logior (source-buffer source)
                           (ash (cond ((< position (length input))
                                       (setf (source-position source) (1+ position))
                                       (aref input position))
                                      (t
                                       (incf (source-overrun source) 8)
                                       0))
                                (source-count source)))
                   (source-count source) (+ 8 (source-count source))))))

(defun peek-bits (source
                  wanted)
  "Return the next WANTED bits as a number, lowest first, without taking them.

SOURCE: the bit source.
WANTED: how many bits, at most 32."
  (declare (type bit-source source)
           (type fixnum wanted)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (fill-bits source wanted)
  (logand (source-buffer source) (1- (ash 1 wanted))))

(defun drop-bits (source
                  dropped)
  "Take DROPPED bits from the buffer of a source, which must hold that many.

SOURCE: the bit source.
DROPPED: how many bits."
  (declare (type bit-source source)
           (type fixnum dropped)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (setf (source-buffer source) (ash (source-buffer source) (- dropped))
        (source-count source) (- (source-count source) dropped)))

(defun take-bits (source
                  wanted)
  "Return the next WANTED bits as a number, lowest first, and take them.

SOURCE: the bit source.
WANTED: how many bits, at most 32."
  (declare (type bit-source source)
           (type fixnum wanted))
  (prog1 (peek-bits source wanted)
    (drop-bits source wanted)))

(defun check-not-truncated (source)
  "Signal IMAGE-ERROR when more bits were taken than the bytes of the source hold.

SOURCE: the bit source."
  (when (> (source-overrun source) (source-count source))
    (image-failure "This picture file ends before the picture does.")))

;;; The bytes being written

(defstruct (sink)
  "Where the bytes of the stream go: the vector, how many are written, and the most that may be."
  (octets (make-octets 0) :type octets)
  (written 0 :type fixnum)
  (limit 0 :type fixnum))

(defun grow-octets (octets
                    needed
                    limit)
  "Return a bigger copy of OCTETS that holds at least NEEDED bytes, or signal IMAGE-ERROR when that is more than LIMIT.

OCTETS: the vector so far.
NEEDED: how many bytes it must hold.
LIMIT: the most bytes the stream may expand to."
  (declare (type octets octets)
           (type fixnum needed limit))
  (when (> needed limit)
    (image-failure "This picture is larger than the program reads."))
  (let ((bigger (make-octets (min limit (max needed (* 2 (length octets)))))))
    (replace bigger octets)
    bigger))

(defun reserve-room (sink
                     wanted)
  "Make the vector of a sink hold WANTED more bytes after those written.

SINK: the sink.
WANTED: how many bytes."
  (declare (type sink sink)
           (type fixnum wanted))
  (when (> (+ (sink-written sink) wanted) (length (sink-octets sink)))
    (setf (sink-octets sink)
          (grow-octets (sink-octets sink)
                       (+ (sink-written sink) wanted)
                       (sink-limit sink)))))

;;; Blocks

(defun read-dynamic-tables (source
                            lengths)
  "Read the header of a dynamic block and return the decoding tables of its codes as a list: literal table, its bits,
distance table, its bits.

SOURCE: the bit source, at the start of the header.
LENGTHS: a vector of at least 320 fixnums to work in."
  (declare (type bit-source source)
           (type (simple-array fixnum (*)) lengths))
  (let* ((literal-count (+ 257 (take-bits source 5)))
         (distance-count (+ 1 (take-bits source 5)))
         (code-length-count (+ 4 (take-bits source 4))))
    (when (or (> literal-count +maximum-literal-symbols+)
              (> distance-count +distance-symbols+))
      (image-failure "The compressed data of this picture is damaged."))
    (fill lengths 0)
    (dotimes (index code-length-count)
      (setf (aref lengths (aref *code-length-order* index)) (take-bits source 3)))
    (multiple-value-bind (code-table code-bits)
        (build-decoding-table lengths
                              0
                              +code-length-symbols+)
      (declare (type (simple-array (unsigned-byte 16) (*)) code-table)
               (type fixnum code-bits))
      (let* ((total (+ literal-count distance-count))
             (read-lengths (make-array total :element-type 'fixnum :initial-element 0))
             (filled 0))
        (declare (type fixnum total filled))
        (loop while (< filled total)
              do (let* ((entry (aref code-table (peek-bits source code-bits)))
                        (code-length (logand entry 15))
                        (symbol (ash entry -4)))
                   (when (zerop code-length)
                     (image-failure "The compressed data of this picture is damaged."))
                   (drop-bits source code-length)
                   (flet ((repeat (value
                                   times)
                            (when (> (+ filled times) total)
                              (image-failure "The compressed data of this picture is damaged."))
                            (dotimes (each times)
                              (setf (aref read-lengths filled) value)
                              (incf filled))))
                     (cond ((< symbol +repeat-previous-symbol+)
                            (repeat symbol 1))
                           ((= symbol +repeat-previous-symbol+)
                            (when (zerop filled)
                              (image-failure "The compressed data of this picture is damaged."))
                            (repeat (aref read-lengths (1- filled)) (+ 3 (take-bits source 2))))
                           ((= symbol +repeat-zero-short-symbol+)
                            (repeat 0 (+ 3 (take-bits source 3))))
                           (t
                            (repeat 0 (+ 11 (take-bits source 7))))))))
        (check-not-truncated source)
        (when (zerop (aref read-lengths +end-of-block-symbol+))
          (image-failure "The compressed data of this picture is damaged."))
        (multiple-value-bind (literal-table literal-bits)
            (build-decoding-table read-lengths
                                  0
                                  literal-count)
          (multiple-value-bind (distance-table distance-bits)
              (build-decoding-table read-lengths
                                    literal-count
                                    distance-count)
            (list literal-table literal-bits distance-table distance-bits)))))))

(defun inflate-stored (source
                       sink)
  "Copy a stored block: after the bits of its header, a length, its complement, and that many bytes as they are.

SOURCE: the bit source, just after the header of the block.
SINK: where the bytes go."
  (declare (type bit-source source)
           (type sink sink))
  (drop-bits source (logand (source-count source) 7))
  (when (plusp (source-overrun source))
    (image-failure "This picture file ends before the picture does."))
  (decf (source-position source) (floor (source-count source) 8))
  (setf (source-buffer source) 0
        (source-count source) 0)
  (let* ((input (source-input source))
         (position (source-position source)))
    (declare (type fixnum position))
    (when (> (+ position 4) (length input))
      (image-failure "This picture file ends before the picture does."))
    (let ((size (+ (aref input position) (ash (aref input (+ position 1)) 8)))
          (complement (+ (aref input (+ position 2)) (ash (aref input (+ position 3)) 8))))
      (unless (= size (logxor complement #xFFFF))
        (image-failure "The compressed data of this picture is damaged."))
      (incf position 4)
      (when (> (+ position size) (length input))
        (image-failure "This picture file ends before the picture does."))
      (reserve-room sink size)
      (replace (sink-octets sink) input
               :start1 (sink-written sink)
               :start2 position
               :end2 (+ position size))
      (incf (sink-written sink) size)
      (setf (source-position source) (+ position size)))))

(defun inflate-codes (source
                      sink
                      literal-table
                      literal-bits
                      distance-table
                      distance-bits)
  "Decode the symbols of a compressed block, up to its end-of-block symbol, into a sink.

SOURCE: the bit source, just after the header of the block.
SINK: where the bytes go.
LITERAL-TABLE: the decoding table of the literal and length symbols.
LITERAL-BITS: the bits it is indexed by.
DISTANCE-TABLE: the decoding table of the distance symbols.
DISTANCE-BITS: the bits it is indexed by."
  (declare (type bit-source source)
           (type sink sink)
           (type (simple-array (unsigned-byte 16) (*)) literal-table distance-table)
           (type (integer 1 15) literal-bits distance-bits)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((input (source-input source))
         (end (length input))
         (position (source-position source))
         (buffer (source-buffer source))
         (count (source-count source))
         (overrun (source-overrun source))
         (output (sink-octets sink))
         (written (sink-written sink))
         (limit (sink-limit sink))
         (literal-mask (1- (ash 1 literal-bits)))
         (distance-mask (1- (ash 1 distance-bits))))
    (declare (type fixnum end position count overrun written limit literal-mask distance-mask)
             (type (unsigned-byte 62) buffer)
             (type octets input output))
    (macrolet ((need (wanted)
                 `(loop while (< count ,wanted)
                        do (setf buffer (logior buffer
                                                (ash (cond ((< position end)
                                                            (prog1 (aref input position)
                                                              (incf position)))
                                                           (t
                                                            (incf overrun 8)
                                                            0))
                                                     count))
                                 count (+ count 8))))
               (drop (dropped)
                 `(setf buffer (ash buffer (- ,dropped))
                        count (- count ,dropped)))
               (reserve (wanted)
                 `(when (> (+ written ,wanted) (length output))
                    (setf output (grow-octets output (+ written ,wanted) limit)))))
      (loop
        (need 15)
        (let* ((entry (aref literal-table (logand buffer literal-mask)))
               (code-length (logand entry 15))
               (symbol (ash entry -4)))
          (when (zerop code-length)
            (image-failure "The compressed data of this picture is damaged."))
          (drop code-length)
          (cond ((< symbol +end-of-block-symbol+)
                 (reserve 1)
                 (setf (aref output written) symbol)
                 (incf written))
                ((= symbol +end-of-block-symbol+)
                 (return))
                (t
                 (let ((index (- symbol 257)))
                   (when (>= index +length-symbols+)
                     (image-failure "The compressed data of this picture is damaged."))
                   (let ((extra (aref *length-extra-bits* index)))
                     (need extra)
                     (let ((length (+ (aref *length-bases* index) (logand buffer (1- (ash 1 extra))))))
                       (declare (type fixnum length))
                       (drop extra)
                       (need 15)
                       (let* ((distance-entry (aref distance-table (logand buffer distance-mask)))
                              (distance-length (logand distance-entry 15))
                              (distance-symbol (ash distance-entry -4)))
                         (when (or (zerop distance-length)
                                   (>= distance-symbol +distance-symbols+))
                           (image-failure "The compressed data of this picture is damaged."))
                         (drop distance-length)
                         (let ((distance-extra (aref *distance-extra-bits* distance-symbol)))
                           (need distance-extra)
                           (let ((distance (+ (aref *distance-bases* distance-symbol)
                                              (logand buffer (1- (ash 1 distance-extra))))))
                             (declare (type fixnum distance))
                             (drop distance-extra)
                             (when (> distance written)
                               (image-failure "The compressed data of this picture is damaged."))
                             (reserve length)
                             (let ((from (- written distance)))
                               (declare (type fixnum from))
                               (dotimes (each length)
                                 (setf (aref output written) (aref output from))
                                 (incf written)
                                 (incf from))))))))))))
        (when (> overrun count)
          (image-failure "This picture file ends before the picture does."))))
    (setf (source-position source) position
          (source-buffer source) buffer
          (source-count source) count
          (source-overrun source) overrun
          (sink-octets sink) output
          (sink-written sink) written)
    nil))

;;; The stream

(defun read-zlib-header (source)
  "Read and check the two bytes that open a zlib stream.

SOURCE: the bit source, at the start of the stream."
  (declare (type bit-source source))
  (let* ((method-and-window (take-bits source 8))
         (flags (take-bits source 8)))
    (unless (and (= +zlib-deflate-method+ (logand method-and-window 15))
                 (<= (ash method-and-window -4) +zlib-largest-window+)
                 (zerop (mod (+ (* 256 method-and-window) flags) +zlib-header-check+))
                 (zerop (logand flags +zlib-dictionary-flag+)))
      (image-failure "The compressed data of this picture is damaged."))
    (check-not-truncated source)))

(defun inflate (input
                &key (start 0)
                     (limit +default-inflate-limit+)
                     (size-hint 0)
                     (zlib t))
  "Return the bytes that a DEFLATE stream holds, and the position after the stream in INPUT. Signals IMAGE-ERROR when
the stream is damaged, ends early, or expands to more than LIMIT bytes.

INPUT: a vector of bytes.
START: where the stream begins.
LIMIT: the most bytes it may expand to.
SIZE-HINT: how many bytes it is expected to expand to, to start with a vector of that size; zero when unknown.
ZLIB: true when the stream has the zlib header and checksum around it, false for the bare DEFLATE data."
  (declare (type octets input)
           (type fixnum start limit size-hint))
  (let* ((source (make-bit-source :input input :position start))
         (sink (make-sink :octets (make-octets (max 16
                                                    (min limit
                                                         (if (plusp size-hint)
                                                             size-hint
                                                             (* 3 (max 0 (- (length input) start)))))))
                          :limit limit))
         (lengths (make-array 320 :element-type 'fixnum :initial-element 0)))
    (when zlib
      (read-zlib-header source))
    (loop
      (let ((final (take-bits source 1))
            (type (take-bits source 2)))
        (cond ((= type +block-stored+)
               (inflate-stored source sink))
              ((= type +block-fixed+)
               (apply #'inflate-codes source sink (fixed-tables)))
              ((= type +block-dynamic+)
               (apply #'inflate-codes source sink (read-dynamic-tables source lengths)))
              (t
               (image-failure "The compressed data of this picture is damaged.")))
        (check-not-truncated source)
        (when (= final 1)
          (return))))
    (drop-bits source (logand (source-count source) 7))
    (let* ((octets (sink-octets sink))
           (written (sink-written sink))
           (next (+ (- (source-position source) (floor (source-count source) 8))
                    (if zlib +adler-size+ 0))))
      (values (if (= written (length octets))
                  octets
                  (subseq octets 0 written))
              (min next (length input))))))
