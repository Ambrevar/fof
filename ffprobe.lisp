(uiop:define-package fof/ffprobe
  (:nicknames #:ffprobe)
  (:documentation "FFprobe abstraction.")
  (:use #:common-lisp)
  (:use #:trivia)
  (:import-from #:hu.dwim.defclass-star #:defclass*)
  (:import-from #:serapeum #:export-always)
  (:import-from #:cl-json))
(in-package fof/ffprobe)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria))

(defvar *ffprobe-command* "ffprobe")

;; TODO: Should leave unspecified fields unbound?

(defclass* disposition ()
    ((default 0)
     (dub 0)
     (original 0)
     (comment 0)
     (lyrics 0)
     (karaoke 0)
     (forced 0)
     (hearing-impaired 0)
     (visual-impaired 0)
     (clean-effects 0)
     (attached-pic 0)
     (timed-thumbnails 0))
    (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
    (:export-accessor-names-p t)
    (:export-class-name-p t))

(defclass* media-stream ()              ; REVIEW: `stream' is reserved by CL.
    ((index 0)
     (codec-name "")
     (codec-long-name "")
     (profile "")
     (codec-type "")
     (width 0)
     (height 0)
     (coded-width 0)
     (coded-height 0)
     (closed-captions 0)
     (has-b-frames 0)
     (pix-fmt "")
     (level 0)
     (color-range "")
     (color-space "")
     (color-transfer "")
     (color-primaries "")
     (chroma-location "")
     (field-order "")
     (refs 0)
     (id "")
     (quarter-sample nil
                     :type boolean)
     (divx-packed nil
                  :type boolean)
     (sample-aspect-ratio "")
     (display-aspect-ratio "")
     (codec-time-base "")               ; TODO: Ratio?
     (codec-tag-string "")
     (codec-tag "")                     ; TODO: Hex?
     (sample-fmt "")
     (sample-rate 0)
     (channels 2)
     (channel-layout "")
     (bits-per-sample 0)
     (dmix-mode 0)
     (ltrt-cmixlev 0.0)
     (ltrt-surmixlev 0.0)
     (loro-cmixlev 0.0)
     (loro-surmixlev 0.0)
     (is-avc nil
             :type boolean)
     (nal-length-size 0)
     (r-frame-rate "")                  ; TODO: Ratio?
     (avg-frame-rate "")                ; TODO: Ratio?
     (time-base "")
     (start-pts 0)
     (start-time 0.0)
     (duration-ts 0.0)
     (duration 0.0)
     (bit-rate 0)
     (bits-per-raw-sample 0)
     (nb-frames 0)
     (max-bit-rate 0)
     (disposition nil
                  :type (or null disposition))
     (side-data-list '())
     (tags '()))
    (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
    (:export-accessor-names-p t)
    (:export-class-name-p t))

(defclass* media-format ()              ; REVIEW: `format' is reserved by CL.
    ((filename "")
     (nb-streams 0)
     (nb-programs 0)
     (format-name "")
     (format-long-name "")
     (start-time 0.0)
     (duration 0.0)
     (size 0)
     (bit-rate 0)
     (probe-score 0)
     (tags '()))
    (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
    (:export-accessor-names-p t)
    (:export-class-name-p t))

(defun normalize-cl-json-keywords (sym)
  "Turn '--' to '-' and remove '+' from keywords."
  (if (keywordp sym)
      (intern (str:replace-all
               "+" ""
               (str:replace-all "--" "-" (symbol-name sym)))
              "KEYWORD")
      sym))

(defun normalize-cl-json-scalar (value)
  "Turn non-ratio number string to numbers."
  (if (stringp value)
      (match value
        ("true" t)
        ("false" nil)
        (_ (let ((result (ignore-errors (parse-number:parse-number value))))
             (if (and result
                      (not (typep result 'ratio)))
                 result
                 value))))
      value))

(defun json->media-args (json)
  (alex:mappend (lambda-match
                  ((cons key value)
                   (list (normalize-cl-json-keywords key)
                         (if (listp value)
                             (json->media-args value)
                             (normalize-cl-json-scalar value)))))
                json))

(export-always 'ffprobe)
(defun ffprobe (path)
  "Return a list of (MEDIA-FORMAT MEDIA-STREAMS...)."
  (let* ((json-string
           (ignore-errors
            (uiop:run-program (list *ffprobe-command*
                                    "-v" "quiet"
                                    "-print_format" "json"
                                    "-show_format"
                                    "-show_streams"
                                    "--"
                                    (write-to-string path))))))
    (when json-string
      (let* ((json (cl-json:decode-json-from-string json-string))
             (format-args (json->media-args (alex:assoc-value json :format)))
             (format (apply #'make-instance 'media-format format-args)))
        (cons format
              (mapcar (lambda (s)
                        (let ((stream-args (json->media-args s)))
                          (apply #'make-instance 'media-stream stream-args)))
                      (alex:assoc-value json :streams)))))))
