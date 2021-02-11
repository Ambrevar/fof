(uiop:define-package fof/mediafile
  (:documentation "Mediafile class (audio, video, pictures).")
  (:use #:common-lisp)
  ;; (:use #:trivia) ; TODO: Unused?
  (:import-from #:hu.dwim.defclass-star #:defclass*)
  (:import-from #:serapeum #:export-always)
  (:import-from #:ambrevar/ffprobe))
(in-package fof/mediafile)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(defclass* mediafile (file+mime)        ; REVIEW: Include MIME?
    ((media-format nil
                   :type (or null ambrevar/ffprobe:media-format))
     (media-streams '()
                    :type (or null
                              (cons ambrevar/ffprobe:media-stream))))
    (:accessor-name-transformer #'name-identity)
    (:export-accessor-names-p t)
    (:export-class-name-p t))

(defmethod initialize-instance :after ((file mediafile) &key)
  (let ((probe (ambrevar/ffprobe:ffprobe (path file))))
    (setf (media-format file) (first probe)
          (media-streams file) (rest probe))))

(export-always 'mediafile)
(defun mediafile (path)
  (make-instance 'mediafile :path path))

(export-always 'mediawalk)
(defun mediawalk (root &rest predicates)
  (let ((*finder-constructor* #'mediafile))
    (apply #'walk root predicates)))

(export-always 'mediafinder)
(defun mediafinder (root &rest predicates)
  (let ((*finder-constructor* #'mediafile))
    (apply #'finder root predicates)))

(export-always 'width)
(defmethod width ((file mediafile))
  (ambrevar/ffprobe:width
   (find-if #'plusp (media-streams file)
            :key #'ambrevar/ffprobe:width)))

(export-always 'height)
(defmethod height ((file mediafile))
  (ambrevar/ffprobe:height
   (find-if #'plusp (media-streams file)
            :key #'ambrevar/ffprobe:height)))

(export-always 'tags)
(defmethod tags ((file mediafile))
  ;; TODO: Get tags from streams too?
  (ambrevar/ffprobe:tags (media-format file)))
