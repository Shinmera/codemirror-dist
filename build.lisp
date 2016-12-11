#+quicklisp (ql:quickload '(drakma zip legit))
#-quicklisp (mapcar #'asdf:load-system '(drakma zip legit))

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
  (move-lib target))

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
