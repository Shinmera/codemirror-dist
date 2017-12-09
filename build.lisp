#+quicklisp (ql:quickload '(drakma zip legit cl-ppcre alexandria))
#-quicklisp (mapcar #'asdf:load-system '(drakma zip legit cl-ppcre alexandria))

(defparameter *here* #.(uiop:pathname-directory-pathname
                        (or *compile-file-pathname* *load-pathname* (uiop:getcwd))))
(defparameter *archive-url* "https://codemirror.net/codemirror.zip")

(defun status (string &rest args)
  (format T "~& > ~?~%" string args))

(defun directory-name (pathname)
  (let ((dir (pathname-directory pathname)))
    (when (cdr dir)
      (car (last dir)))))

(defun ends-with (end string)
  (and (<= (length end) (length string))
       (string= end string :start2 (- (length string) (length end)))))

(defun rm-r (target)
  (if (uiop:directory-pathname-p target)
      (uiop:delete-directory-tree target :validate (constantly T))
      (delete-file target)))

(defun upwards-pathname (pathname)
  (let ((dir (pathname-directory pathname)))
    (make-pathname :directory (when (cdr dir) (butlast dir)) :defaults pathname)))

(defun pop-directory (subdir)
  (loop for file in (uiop:directory* (uiop:wilden subdir))
        for target = (merge-pathnames (uiop:enough-pathname file subdir)
                                      (upwards-pathname subdir))
        do (ensure-directories-exist target)
           (unless (uiop:directory-pathname-p target)
             (rename-file file target)))
  (rm-r subdir))

(defun download (url target)
  (status "Downloading ~a to ~a" url target)
  (with-open-file (out target :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
    (let ((in (drakma:http-request url :want-stream T)))
      (unwind-protect
           (uiop:copy-stream-to-stream in out :element-type '(unsigned-byte 8))
        (close in)))
    target))

(defun extract (path target &key prefix)
  (status "Extracting ~a to ~a" path target)
  (zip:unzip path target :if-exists :supersede)
  (when prefix
    (let ((subdir (find prefix (uiop:subdirectories target)
                        :test (lambda (a b) (search a (directory-name b))))))
      (when subdir (pop-directory subdir))))
  target)

(defun strip (target)
  (status "Stripping ~a" target)
  (flet ((del (file)
           (rm-r (merge-pathnames file target))))
    (mapcar #'del '("bin/" "demo/" "doc/" "src/" "test/" "mode/meta.js" "mode/index.html"))
    (mapcar #'del (uiop:directory-files target)))
  target)

(defun flatten-modes (target)
  (let ((mode-dir (merge-pathnames "mode/" target)))
    (status "Flattening modes in ~a" mode-dir)
    (dolist (mode (directory (make-pathname :type "js" :defaults (uiop:wilden mode-dir))))
      (unless (ends-with "_test" (pathname-name mode))
        (uiop:copy-file mode (make-pathname :name (pathname-name mode) :type "js"
                                            :defaults mode-dir))))
    (mapcar #'rm-r (uiop:subdirectories mode-dir))))

(defun move-lib (target)
  (let ((lib-dir (merge-pathnames "lib/" target)))
    (status "Moving libs in ~a to root" lib-dir)
    (dolist (file (uiop:directory-files lib-dir))
      (rename-file file (make-pathname :name (pathname-name file)
                                       :type (pathname-type file)
                                       :defaults target)))
    (rm-r lib-dir)))

(defun find-mime-types (target)
  (remove-duplicates
   (list*
    ;; Special handling for extraneous clike modes.
    (list "text/x-c" "mode/clike.js")
    (list "text/x-c++src" "mode/clike.js")
    (list "text/x-c++hdr" "mode/clike.js")
    (list "x-shader/x-vertex" "mode/clike.js")
    (list "x-shader/x-fragment" "mode/clike.js")
    (loop for file in (directory (merge-pathnames (make-pathname :type "js" :name :wild :defaults uiop:*wild-inferiors*) target))
          nconc (let ((list ()))
                  (cl-ppcre:do-register-groups (mime) ("(?:def|defineMIME)\\(\"(\\w+/[\\w-]+)\""
                                                       (alexandria:read-file-into-string file) list)
                    (push (list mime file) list)))))
   :key #'car :test #'string=))

(defun mime-type-to-name (mime)
  (cl-ppcre:regex-replace "^\\w+/(x-)?" mime ""))

(defun mime-type-table (mimes)
  (let ((table (make-hash-table :test 'equal)))
    (loop for mime in mimes
          do (setf (gethash (mime-type-to-name (first mime)) table) mime))
    (sort (alexandria:hash-table-alist table) #'string< :key #'car)))

(defun write-mime-spec (target)
  (with-open-file (out (merge-pathnames "mimes.txt" target)
                       :direction :output :if-exists :supersede)
    (loop for (name mime file) in (mime-type-table (find-mime-types (merge-pathnames "mode/" target)))
          do (format out "~&~a ~a ~a" name mime (enough-namestring file (truename target))))))

(defun clean-all (target)
  (status "Cleaning out all artefacts in ~a"  target)
  (mapcar #'rm-r (remove ".git" (directory (merge-pathnames "*.*" target))
                         :key #'directory-name :test #'string=)))

(defun dist (target)
  (ensure-directories-exist target)
  (clean-all target)
  (extract (download *archive-url* (merge-pathnames "archive.zip" target))
           target :prefix "codemirror-")
  (strip target)
  (flatten-modes target)
  (move-lib target)
  (write-mime-spec target))

(defun main (&optional (target *here*))
  (let ((repo (make-instance 'legit:repository :location target)))
    (status "Checking out master")
    (legit:checkout repo "master")
    (dist target)
    (status "Committing changes")
    (legit:add repo :all)
    (legit:commit repo "Updated.")
    (status "Pushing changes")
    (legit:push repo)))

(main)
