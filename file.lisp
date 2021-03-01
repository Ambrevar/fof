(uiop:define-package fof/file
  (:documentation "File class.")
  (:use #:common-lisp)
  (:import-from #:alexandria)
  (:import-from #:hu.dwim.defclass-star #:defclass*)
  (:import-from #:local-time)
  (:import-from #:magicffi)
  (:import-from #:osicat)
  (:import-from #:serapeum #:export-always)
  (:import-from #:str)
  (:import-from #:trivia #:match))
(in-package fof/file)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(defvar *touch-command* "touch") ; TODO: `utime' syscall binding is missing from Osicat.

;; TODO: Run multiple disk writes within a transation?
;; Need proper POSIX bindings.  Can Osicat do all of them?
;; Could we edit files virtually?  Does that even make sense?

;; TODO: Replace magicffi with trivial-mime once we can get MIME encoding
;; (https://github.com/Shinmera/trivial-mimes/issues/8), description, and fix
;; the probe-file issue.

(defclass* file ()
    ((path (error "Path required")
           :type string
           :reader t)
     (inode 0
            :reader t)
     (link-count 0
                 :reader t)
     (kind :regular-file              ; "kind" because `type' is reserved by CL.
           :type (member :directory
                         :character-device
                         :block-device
                         :regular-file
                         :symbolic-link
                         :socket
                         :pipe)
           :reader t)
     (size 0
           :reader t)
     (disk-usage 0
                 :reader t)
     (user-id 0)
     (group-id 0)
     ;; TODO: Include blocks?
     (creation-date (local-time:unix-to-timestamp 0)
                    :reader t)
     (modification-date (local-time:unix-to-timestamp 0))
     (access-date (local-time:unix-to-timestamp 0))
     (permissions '()
                  :type (or null
                            (cons #.(cons 'member (mapcar #'first osicat::+permissions+))))))
    (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
    (:export-slot-names-p t)
    (:export-class-name-p t))

(defmethod (setf user-id) (id (file file))
  (osicat-posix::chown (path file) id (group-id file))
  (setf (slot-value file 'user-id) id))

(defmethod (setf group-id) (id (file file))
  (osicat-posix::chown (path file) (user-id file) id)
  (setf (slot-value file 'group-id) id))

;; TODO: For now, date setters rely on GNU touch.  Find a portable version.
(defmethod (setf modification-date) (timestamp (file file))
  "Set both the `modification-date' and the `access-date' of FILE."
  (uiop:run-program (list *touch-command*
                          (format nil "--date=~a" (local-time:format-rfc3339-timestring nil timestamp))
                          (path file)))
  (setf (slot-value file 'modification-date) timestamp)
  (setf (slot-value file 'access-date) timestamp))

(defmethod (setf access-date) (timestamp (file file))
  (uiop:run-program (list *touch-command*
                          "-a"
                          (format nil "--date=~a" (local-time:format-rfc3339-timestring nil timestamp))
                          (path file)))
  (setf (slot-value file 'modification-date) timestamp))

(defmethod (setf permissions) (permissions (file file))
  (setf (osicat:file-permissions (path file)) permissions)
  (setf (slot-value file 'permissions) permissions))

(defmethod path ((s string))
  "Useful so that `path' can be called both on a `file' or a `string'."
  s)

(defmethod path ((p pathname))
  "Useful so that `path' can be called both on a `file' or a `pathname'."
  (namestring p))

(export-always 'user)
(defmethod user ((file file))
  "Return the name of the user owning the file."
  (nth-value 0 (alex:assoc-value (osicat:user-info (user-id file)) :name)))

(defun read-/etc/group ()
  (let ((content (alex:read-file-into-string "/etc/group")))
    (mapcar (alex:curry #'str:split ":") (str:split (string #\newline) content))))

(defun group-id->name (id)
  (let ((result (find (write-to-string id) (fof/file::read-/etc/group)
                      :key #'caddr :test #'string=)))
    (when result
      (first result))))

(export-always 'group)
(defmethod group ((file file))
  "Return the name of the group owning the file."
  (group-id->name (group-id file)))

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
  (and (typep file1 'file) (typep file2 'file)
       (string= (path file1)
                (path file2))))

(export-always 'separator)
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

(export-always 'exists?)
(defmethod exists? ((file file))
  (and
   (if (directory? file)
       (uiop:directory-exists-p (path file))
       (uiop:file-exists-p (path file)))
   file))

(export-always 'parent)
(defmethod parent ((file file))
  "Return the parent directory of FILE."
  (file
   (if (directory? file)
       (uiop:pathname-parent-directory-pathname (path file))
       (uiop:pathname-directory-pathname (path file)))))

(defmethod disk-usage* ((file file))
  "Compute recursive `disk-usage' of FILE if a directory.
Return the new disk-usage."
  (if (directory? file)
      (if (list-directory file)
          (reduce #'+ (mapcar #'disk-usage* (list-directory file)))
          (size file))
      (slot-value file 'disk-usage)))

(defmethod disk-usage ((file file))
  "Return FILE `disk-usage'.
If FILE is a directory and it's disk-usage is 0 (never computed before), set it
with `disk-usage*' and return the new value."
  (if (or (file? file)
          (/= 0 (slot-value file 'disk-usage)))
      (slot-value file 'disk-usage)
      (disk-usage* file)))

(defun depth (file parent)
  (cond
    ((file=? file parent)
     0)
    ((file=? file (parent file))
     0)
    (t
     (1+ (depth (parent file) parent)))))

(export-always 'relative-path)
(defmethod relative-path ((file file) &optional (parent-directory (file *default-pathname-defaults*)))
  "Return PATH relative to PARENT-DIRECTORY.
If PARENT-DIRECTORY is not a parent of PATH, return PATH."
  (if (str:starts-with? (path parent-directory)
                        (path file))
      (subseq (path file) (length (path parent-directory)))
      (path file)))

(defun shorten-home (path)
  (let ((home (uiop:getenv "HOME")))
    (if (str:starts-with? home path)
        (str:replace-first home "~" path)
        path)))

(defun shorten-path (path &key (abbreviation-length 1) ; TODO: Is there a library for this?
                            (abbreviate-home t)
                            (ellipsis "…"))
  (let* ((path (if abbreviate-home
                   (shorten-home path)
                   path))
         (elements
           (str:split (separator) path
                      :omit-nulls t)))
    (if elements
        (str:concat (when (str:starts-with? (separator) path)
                      (separator))
                    (str:join
                     (separator)
                     (append
                      (mapcar (lambda (dir)
                                (if (<= (length dir) (+ abbreviation-length (length ellipsis)))
                                    dir
                                    (str:concat
                                     (subseq dir 0 abbreviation-length)
                                     ellipsis)))
                              (butlast elements))
                      (list (first (last elements)))))
                    (when (str:ends-with? (separator) path)
                      (separator)))
        path)))


(defparameter +ls-time-format+
  '(:short-month #\space (:day 2 #\ ) #\space  (:hour 2) #\: (:min 2)))


(export-always '*print-object-reader-macro*)
(defvar *print-object-reader-macro* "#F")
(export-always '*print-object-relative-path?*)
(defvar *print-object-relative-path?* nil)
(export-always '*print-object-abbreviation-length*)
(defvar *print-object-abbreviation-length* 2
  "Set to 0 to stop abbreviating.")
(export-always '*print-object-size?*)
(defvar *print-object-size?* nil)
(export-always '*print-object-date?*)
(defvar *print-object-date?* nil)

(defun print-file (file stream
                   &key
                     (reader-macro *print-object-reader-macro*)
                     (relative-path? *print-object-relative-path?*)
                     (abbreviation-length *print-object-abbreviation-length*)
                     (size? *print-object-size?*)
                     (date? *print-object-date?*))
  (let ((path (if relative-path?
                  (relative-path file)
                  (path file))))
    (format stream "~a\"~a~a~a\""
            reader-macro
            (if (= 0 abbreviation-length)
                path
                (shorten-path path :abbreviation-length abbreviation-length))
            (if (and (directory? file)
                     (not (str:ends-with? "/" (path file))))
                "/" "")
            (str:concat
             (when size?
               (str:concat " " (sera:format-human-size nil (size file) :space nil)))
             (when date?
               (str:concat " " (local-time:format-timestring nil (modification-date file)
                                                             :format +ls-time-format+)))))))

;; TODO: Support `*print-pretty*'?
;; TODO: `*print-readably*'?
;; TODO: Auto-update file when mtime changes?  Wouldn't it be too slow?
(defmethod print-object ((file file) stream)
  (print-file file stream))

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
    (setf (slot-value file 'path) (uiop:unix-namestring native-path))
    (let ((stat (ignore-errors (osicat-posix:stat native-path))))
      (if stat
          ;; From Osicat's `file-permissions':
          (flet ((stat-permissions (stat)
                   (let ((mode (osicat-posix:stat-mode stat)))
                     (loop for (name . value) in osicat::+permissions+
                           when (plusp (logand mode value))
                             collect name))))
            (setf
             (slot-value file 'inode) (osicat-posix:stat-ino stat)
             (slot-value file 'link-count) (osicat-posix:stat-nlink stat)
             (slot-value file 'kind) (osicat:file-kind native-path) ; TODO: Don't recall `stat'.
             (slot-value file 'size) (osicat-posix:stat-size stat)
             (slot-value file 'disk-usage) (* 512 (osicat-posix:stat-blocks stat)) ; 512 as per (2)stat.
             (slot-value file 'user-id) (osicat-posix:stat-uid stat)
             (slot-value file 'group-id) (osicat-posix:stat-gid stat)
             (slot-value file 'creation-date) (local-time:unix-to-timestamp (osicat-posix:stat-ctime stat))
             (slot-value file 'modification-date) (local-time:unix-to-timestamp (osicat-posix:stat-mtime stat))
             (slot-value file 'access-date) (local-time:unix-to-timestamp (osicat-posix:stat-atime stat))
             (slot-value file 'permissions) (stat-permissions stat)))
          ;; Errors may happen in particular for broken symlinks, see
          ;; https://github.com/osicat/osicat/issues/40
          (warn "Failed to retrieve ~s metadata" (slot-value file 'path))))))

(defun file (path)
  (make-instance 'file :path path))

(defun read-until (stream delimiter)
  "Return the string read until DELIMITER."
  (concatenate 'string
               (loop :for char = (read-char stream nil :eof)
                     :while (and (not (eq char :eof))
                                 (not (char= char delimiter)))
                     :collect char)))

(defun file-reader (stream char1 char2)
  (declare (ignore char1 char2))
  (read-until stream #\")
  (file (read-until stream #\")))

(export-always 'readtable)
(named-readtables:defreadtable readtable
  (:merge :standard)
  (:dispatch-macro-char #\# #\f #'file-reader))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(export-always 'list-directory)
(defun list-directory (&optional (path (file *default-pathname-defaults*)) sort)
  "Return entries in PATH.
By default, directories come first.
If SORT is non nil, sort them alphabetically.
Second value is the list of directories, third value is the non-directories."
  ;; TODO: Use locale to sort?
  (let* ((subdirs (mapcar #'file (uiop:subdirectories (path path))))
         (subfiles (mapcar #'file (uiop:directory-files (path path))))
         (result (append subdirs subfiles)))
    (values
     (if sort
         (sort result #'string< :key #'path)
         result)
     subdirs
     subfiles)))

(export-always '*finder-include-directories*)
(defvar *finder-include-directories* t  ; TODO: Use?
  "When non-nil `finder' includes directories.")

(export-always '*finder-constructor*)
(defvar *finder-constructor* #'file
  "Function that takes a path and returns a `file'-like object.")

(export-always 'finder*)
(defun finder* (&key
                  (root (file *default-pathname-defaults*))
                  predicates
                  recur-predicates)
  "List FILES (including directories) that satisfy all PREDICATES.
Without PREDICATES, list all files.

Recur in subdirectories when they satisfy all RECUR-PREDICATES.
Without RECUR-PREDICATES, recur in all subdirectories."
  (let ((result '()))
    (uiop:collect-sub*directories
     (uiop:ensure-directory-pathname (path root))
     (constantly t)
     (lambda (dir)
       (every (alex:rcurry #'funcall (file dir)) recur-predicates))
     (lambda (subdirectory)
       (setf result (nconc result
                           (let ((subfiles (mapcar *finder-constructor*
                                                   (append (uiop:subdirectories subdirectory)
                                                           (uiop:directory-files subdirectory)))))
                             (if predicates
                                 (delete-if (lambda (file)
                                              (notevery (alex:rcurry #'funcall file) predicates))
                                            subfiles)
                                 subfiles))))))
    result))

(defun match-path (path-element &rest more-path-elements)
  "Return a predicate that matches when one of the path elements is contained in
the file path.
Useful for `finder'."
  (lambda (file)
    (some (lambda (elem)
            (str:contains? elem (path file)))
          (cons path-element more-path-elements))))

(defun match-path-end (path-suffix &rest more-path-suffixes)
  "Return a predicate that matches when one of the path suffixes matches
the file path.
Useful for `finder'."
  (lambda (file)
    (some (lambda (suffix)
            (str:ends-with? (namestring suffix) (path file)))
          (cons path-suffix more-path-suffixes))))

(defun match-depth< (level &optional (root (file *default-pathname-defaults*)))
  "Return a predicate that matches when the argument file is in a subdirectory
of ROOT less deep than LEVEL."
  (lambda (file)
    (< (depth file root) level)))

(export-always 'finder)
(defun finder (&rest predicate-specifiers) ; TODO: Add convenient regexp support?  Case-folding? Maybe str:*ignore-case* is enough.
  "List files in current directory that satisfy all PREDICATE-SPECIFIERS
Directories are ignored.
Without PREDICATE-SPECIFIERS, list all files.

A predicate specifier can be:

- a string, in which case it is turned into (match-path STRING);
- a pathname, in which case it is turned into (match-path-end PATHNAME);
- a list of predicates, in which case it is turned into (apply #'alexandria:disjoin PREDICATES);
- a function (a predicate).

For a more tunable finder, see `finder*'."
  (labels ((specifier->predicate (specifier)
             (match specifier
               ((and s (type string))
                (match-path s))
               ((and s (type pathname))
                (match-path-end s))
               ((cons pred1 more-preds)
                (apply #'alex:disjoin
                       (mapcar #'specifier->predicate
                               (cons pred1 more-preds))))
               ((and pred (type function))
                pred)
               (other
                (error "Unknown predicate specifier: ~a" other)))))
    (finder* :root (file *default-pathname-defaults*)
             :predicates (cons (complement #'directory?)
                               (mapcar #'specifier->predicate
                                       predicate-specifiers)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `ls -l' proof-of-concept replacement.

(defun permissions->unix (permissions)
  (format nil "~a~a~a~a~a~a~a~a~a"
          (if (find :user-read permissions) "r" "-")
          (if (find :user-write permissions) "w" "-")
          (if (find :user-exec permissions) "x" "-")
          (if (find :group-read permissions) "r" "-")
          (if (find :group-write permissions) "w" "-")
          (if (find :group-exec permissions) "x" "-")
          (if (find :other-read permissions) "r" "-")
          (if (find :other-write permissions) "w" "-")
          (if (find :other-exec permissions) "x" "-")))

(defun max-width (files reader &key (key #'write-to-string))
  (apply #'max (mapcar #'length
                       (mapcar (lambda (file)
                                 (funcall key (funcall reader file)))
                               files))))

(defun ls-l (&key human-readable?)
  "Mimicks Unix' `ls -l'."
  ;; TODO: Add support for file arguments?
  (let* ((current-dir-entries (finder* :recur-predicates (list (match-depth< 2))))
         (size-column-width (max-width current-dir-entries #'size)))
    (dolist (file current-dir-entries)
      (format t (str:concat "~a~a ~a ~a ~a ~" (write-to-string size-column-width) "@a ~a ~a~%")
              (if (directory? file) "d" "-")
              (permissions->unix (permissions file))
              (link-count file)
              (user file)
              (group file)
              (if human-readable?
                  (serapeum:format-file-size-human-readable nil (size file))
                  (size file))
              (local-time:format-timestring nil (modification-date file)
                                            :format +ls-time-format+)
              (relative-path file)))))

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
    ((mime-type ""
                :reader t)
     (mime-encoding ""
                    :reader t)
     (description ""
                  :reader t))
    (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
    (:export-slot-names-p t)
    (:export-class-name-p t))

(defmethod initialize-instance :after ((file file+mime) &key)
  (let ((mime-type+encoding (%mime-type+encoding (path file))))
    (setf (slot-value file 'mime-type) (first mime-type+encoding)
          (slot-value file 'mime-encoding) (second mime-type+encoding)
          (slot-value file 'description) (%description (path file)))))

(defun file+mime (path)
  (make-instance 'file+mime :path path))

(defun finder*+mime (root &rest predicates)
  (let ((*finder-constructor* #'file+mime))
    (finder* :root root
             :predicates predicates)))

(defun finder+mime (&rest predicates)
  (let ((*finder-constructor* #'file+mime))
    (apply #'finder predicates)))
