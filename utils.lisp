
(in-package :mgl-pax-ext)

(defparameter *docstrings* (make-hash-table))

(defun get-docparser (sys)
  "Get docparser. Cached"
  (let ((parser (gethash sys *docstrings*)))
    (when (not parser)
      (setf parser (docparser:parse sys)
            (gethash sys *docstrings*) parser))
    parser))

(defun find-node (symbol)
  (let* ((pkg (symbol-package symbol))
         ;; there is an issue here, get-docparser expects a system,
         ;; but we only have a package here, is there a way to get
         ;; system name based on package?
         ;; right now i'm gonna assume that system is the same as
         ;; package
         (pkg-name (intern (package-name pkg) :keyword))
         (sys pkg-name))
    (docparser:query (get-docparser sys)
                     :package-name pkg-name
                     :symbol-name symbol)))

(defun find-docstring (symbol)
  (let ((node (find-node symbol)))
    (when (length node)
      (docparser:node-docstring (aref node 0)))))

(defun find-defun-form (forms name)
  (dolist (form forms)
    (when (eql name (cadr form))
      (return-from find-defun-form form))))

(defun generate-readme (sys manual &key (readme "/README.md"))
  "Generate README.md file, saving it into asdf project root"
  (let* ((sys-dir (asdf:system-source-directory sys))
         (output (merge-pathnames sys-dir readme)))
    (alexandria:with-output-to-file (out output :if-exists :supersede)
      (mgl-pax:document manual :stream out :format :markdown))))
