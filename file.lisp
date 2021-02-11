(uiop:define-package fof/file
  (:documentation "File class.")
  (:use #:common-lisp)
  ;; (:use #:trivia) ; TODO: Unused?
  (:import-from #:alexandria)
  (:import-from #:hu.dwim.defclass-star #:defclass*)
  (:import-from #:magicffi)
  (:import-from #:serapeum #:export-always)
  (:import-from #:str))
(in-package fof/file)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

;; TODO: Allow some slots to modify file on disk.  Transaction?
;; Could we edit files virtually nonetheless?  Does that even make sense?

;; TODO: Only expose readers for slots that cannot be modified, such as `path'.

;; TODO: Implement disk-usage for directories.

;; TODO: Replace magicffi with trivial-mime once we can get MIME encoding
;; (https://github.com/Shinmera/trivial-mimes/issues/8), description, and fix
;; the probe-file issue.

(sera:eval-always
  (defun name-identity (name definition)
    (declare (ignore definition))
    name))

(defclass* file ()
    ((path (error "Path required")
           :type string
           ;; :reader path
           )
     (inode 0)
     (link-count 0)
     (kind :regular-file              ; "kind" because `type' is reserved by CL.
           :type (member :directory
                         :character-device
                         :block-device
                         :regular-file
                         :symbolic-link
                         :socket
                         :pipe)
           ;; :reader kind
           )
     (size 0
           ;; :reader size
           )
     (user-id 0)
     (group-id 0)
     ;; TODO: Include blocks?
     (creation-date (local-time:unix-to-timestamp 0))
     (modification-date (local-time:unix-to-timestamp 0))
     (access-date (local-time:unix-to-timestamp 0))
     (permissions '()
                  :type (or null
                            (cons #.(cons 'member (mapcar #'first osicat::+permissions+))))))
    (:accessor-name-transformer #'name-identity)
    (:export-accessor-names-p t)
    (:export-class-name-p t))

(defmethod path ((s string))
  "Useful so that `path' can be called both on a `file' or a `string'."
  s)

(defmethod path ((p pathname))
  "Useful so that `path' can be called both on a `file' or a `pathname'."
  (namestring p))

(export-always 'extension)
(defmethod extension ((file file))
  "Return the file extension.
If none, return the empty string unlike `pathname-type'."
  (or (pathname-type (path file))
      ""))

(export-always 'directory?)
(defmethod directory? ((file file))
  (eq (kind file) :directory))

(export-always 'file?)
(defmethod file? ((file file))
  (eq (kind file) :regular-file))

(export-always 'file=?)
(defun file=? (file1 file2)
  "Return true if FILE1 and FILE2 point to the same file.
They might not be the same objects."
  (and (file? file1) (file? file2)
       (string= (path file1)
                (path file2))))

(defun separator (&optional char?)
  (if char?
      (uiop:directory-separator-for-host)
      (string (uiop:directory-separator-for-host))))

(export-always 'basename)
(defmethod basename ((file file))
  "Return the file basename (including the extension).
This returns the directory name for directories."
  (let* ((path (path file))
         (path  (if (str:ends-with? (separator) path)
                    (subseq path 0 (1- (length path)))
                    path))
         (last-separator (position (separator :char)
                                   path :from-end t)))
    (subseq path
            (1+ last-separator))))

(export-always 'relative-path)
(defmethod relative-path ((file file) &optional (parent-directory *default-pathname-defaults*))
  "Return PATH relative to PARENT-DIRECTORY.
If PARENT-DIRECTORY is not a parent of PATH, return PATH."
  (setf parent-directory (path (uiop:ensure-directory-pathname parent-directory)))
  (if (str:starts-with? parent-directory
                        (path file))
      (subseq (path file) (length parent-directory))
      (path file)))

;; TODO: Support `*print-pretty*'?
;; TODO: `*print-readably*'?
(defmethod print-object ((file file) stream)
  (print-unreadable-object (file stream :type t :identity t)
    (write-string (str:concat (basename file)
                              (when (directory? file) "/"))
                  stream)))

(export-always 'file)
(defmethod initialize-instance :after ((file file) &key)
  (let* ((path (path file))
         (native-path (uiop:truename* (if (pathnamep path)
                                          path
                                          (uiop:parse-native-namestring path)))))
    (unless (or (uiop:file-exists-p native-path)
                (uiop:directory-exists-p native-path))
      (error "~s is not a file path" (or native-path path)))
    ;; TODO: What do we do with non-existent files (e.g. unsaved emacs buffers)?  Just return nil?
    (let ((stat (ignore-errors (osicat-posix:stat native-path))))
      (when stat
        ;; From Osicat's `file-permissions':
        (flet ((stat-permissions (stat)
                 (let ((mode (osicat-posix:stat-mode stat)))
                   (loop for (name . value) in osicat::+permissions+
                         when (plusp (logand mode value))
                           collect name))))
          (setf
           (path file) (uiop:unix-namestring native-path)
           (inode file) (osicat-posix:stat-ino stat)
           (link-count file) (osicat-posix:stat-nlink stat)
           (kind file) (osicat:file-kind native-path) ; TODO: Don't recall `stat'.
           (size file) (osicat-posix:stat-size stat)
           (user-id file) (osicat-posix:stat-uid stat)
           (group-id file) (osicat-posix:stat-gid stat)
           (creation-date file) (local-time:unix-to-timestamp (osicat-posix:stat-ctime stat))
           (modification-date file) (local-time:unix-to-timestamp (osicat-posix:stat-mtime stat))
           (access-date file) (local-time:unix-to-timestamp (osicat-posix:stat-atime stat))
           (permissions file) (stat-permissions stat)))))))

(defun file (path)
  (make-instance 'file :path path))

(export-always 'list-directory)
(defun list-directory (&optional (path *default-pathname-defaults*) sort)
  "Return entries in PATH.
By default, directories come first.
If SORT is non nil, sort them alphabetically.
Second value is the list of directories, third value is the non-directories."
  ;; TODO: Use locale to sort?
  (let* ((subdirs (mapcar #'file (uiop:subdirectories path)))
         (subfiles (mapcar #'file (uiop:directory-files path)))
         (result (append subdirs subfiles)))
    (values
     (if sort
         (sort result #'string< :key #'path)
         result)
     subdirs
     subfiles)))

(export-always '*finder-include-directories*)
(defvar *finder-include-directories* t
  "When non-nil `walk' include directories.")

(export-always '*finder-constructor*)
(defvar *finder-constructor* #'file
  "Function that takes a path and returns a `file'-like object.")

(export-always 'walk)
(defun walk (root &rest predicates)
  "List FILES (including directories) that satisfy all PREDICATES.
Without PREDICATES, list all files."
  (let ((result '()))
    (uiop:collect-sub*directories
     (uiop:ensure-directory-pathname root)
     (constantly t) (constantly t)
     (lambda (subdirectory)
       (setf result (nconc result
                           (let ((subfiles (mapcar *finder-constructor*
                                                   (append (if *finder-include-directories* (list subdirectory) nil)
                                                           (uiop:directory-files subdirectory)))))
                             (if predicates
                                 (delete-if (lambda (file)
                                              (notany (lambda (pred) (funcall pred file))
                                                      predicates))
                                            subfiles)
                                 subfiles))))))
    result))

(export-always 'finder)
(defun finder (root &rest predicates)
  "List files in ROOT that satisfy all PREDICATES.
Without PREDICATES, list all files."
  (let ((*finder-include-directories* nil))
    (apply #'walk root predicates)))

(defun match-date< (timestamp)
  "Return a file predicate that matches on modification time #'< than timestamp."
  (lambda (file)
    (local-time:timestamp< (modification-date file) timestamp)))

(defun match-date> (timestamp)
  "Return a file predicate that matches on modification time #'> than timestamp."
  (lambda (file)
    (local-time:timestamp> (modification-date file) timestamp)))

(export-always 'match-extension)
(defun match-extension (extension &rest more-extensions)
  "Return a predicate for files that match on of the provided extensions.
Useful for `finder'."
  (lambda (file)
    (some (lambda (ext)
            (string= ext (extension file)))
          (cons extension more-extensions))))

(export-always 'match-name)
(defun match-name (name &rest more-names)
  "Return a predicate that matches when one of the names is contained in the
file basename.
Basename includes the extension.  Useful for `finder'."
  (lambda (file)
    (some (lambda (name)
            (str:contains? name (basename file)))
          (cons name more-names))))

;; TODO: Better control to filter in/out directories?
;; (export-always 'match-directory)
;; (defun match-directory (&key (empty? t) (non-empty? t) (files? t))
;;   "Return a predicate that matches on directories.
;; If target is a file, return FILES?.
;; Useful for `walk'."
;;   (lambda (directory)
;;     (if (uiop:directory-exists-p directory)
;;         (let ((files-or-dirs? (or (uiop:directory-files directory)
;;                                   (uiop:subdirectories directory))))
;;           (or (and empty?
;;                    (not files-or-dirs?))
;;               (and non-empty?
;;                    files-or-dirs?)))
;;         files?)))

(export-always 'match-executable)
(defun match-executable ()
  (lambda (file)
    (intersection
     (permissions file)
     '(:user-exec :group-exec :other-exec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter %magic-cookie-mime nil
  "Internal storage for `magic-cookie-mime'.")
(defun magic-cookie-mime ()
  "Return internal, persistent MIME cookie for `magicffi' calls.
Benchmark on thousands of files shows that
keeping the same cookie saves about 15% of time. "
  (when (or (not %magic-cookie-mime)
            (not (magicffi:open-magic-p %magic-cookie-mime)))
    (setf %magic-cookie-mime (magicffi:magic-open '(:symlink :mime)))
    (magicffi:magic-load %magic-cookie-mime))
  %magic-cookie-mime)

(defparameter %magic-cookie-description nil
  "Internal storage for `magic-cookie-description'.")
(defun magic-cookie-description ()
  "Same as `magic-cooke-mime' but for `file' descriptions.
See `%description'."
  (when (or (not %magic-cookie-description)
            (not (magicffi:open-magic-p %magic-cookie-description)))
    (setf %magic-cookie-description (magicffi:magic-open '(:symlink)))
    (magicffi:magic-load %magic-cookie-description))
  %magic-cookie-description)

(defun %mime-type+encoding (path)
  "Return a pair of MIME type and MIME encoding for PATH."
  (str:split "; "
             (magicffi:magic-file (magic-cookie-mime) path)))

(defun %description (path)
  "Return the PATH description as per the `file' UNIX command."
  (magicffi:magic-file (magic-cookie-description) path))

;; TODO: Include the description or do it in another class?  Could be slower.  Benchmark.
(defclass* file+mime (file)
    ((mime-type "")
     (mime-encoding "")
     (description ""))
    (:accessor-name-transformer #'name-identity)
    (:export-accessor-names-p t)
    (:export-class-name-p t))

(defmethod initialize-instance :after ((file file+mime) &key)
  (let ((mime-type+encoding (%mime-type+encoding (path file))))
    (setf (mime-type file) (first mime-type+encoding)
          (mime-encoding file) (second mime-type+encoding)
          (description file) (%description (path file)))))

(defun file+mime (path)
  (make-instance 'file+mime :path path))

(defun walk+mime (root &rest predicates)
  (let ((*finder-constructor* #'file+mime))
    (apply #'walk root predicates)))

(defun finder+mime (root &rest predicates)
  (let ((*finder-constructor* #'file+mime))
    (apply #'finder root predicates)))

(export-always 'match-elf-binary)
(defun match-elf-binary ()
  (lambda (file)
    (and (slot-boundp file 'mime-type)
         (string= "application/x-executable" (mime-type file)))))

(export-always 'match-elf-library)
(defun match-elf-library ()
  (lambda (file)
    (and (slot-boundp file 'mime-type)
         (ppcre:scan "application/x-sharedlib" (first (mime-type file))))))
