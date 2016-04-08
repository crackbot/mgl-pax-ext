
(in-package :mgl-pax-ext)

;;;; STATIC-PS-FUNCTION

(define-locative-type static-ps-function (var))

(defmethod locate-object (symbol (locative-type (eql 'static-ps-function)) locative-args)
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'static-ps-function))
                                locative-args stream)
  (let* ((forms (cdr (eval (car locative-args))))
         (defun-form (find-defun-form forms symbol)))
    (destructuring-bind (def name args &rest body)
        defun-form
      (declare (ignore def name))
      (let ((doc-string (if (and (> (length body) 1)
                                 (stringp (car body)))
                            (car body)
                            nil)))
        (format stream "- [contract] ")
        (format stream "~A" symbol)
        (write-char #\Space stream)
        (mgl-pax::print-arglist args stream)
        (write-char #\Space stream)
        (when doc-string
          (terpri stream)
          (format stream "~%~A" (mgl-pax::massage-docstring doc-string)))
        (terpri stream)))))

;; PSMACRO

(docparser:define-parser ps:defpsmacro (name (&rest args) &rest body)
  (let ((docstring (if (stringp (first body))
                       (first body)
                       nil)))
    (make-instance 'docparser::macro-node
                   :name name
                   :docstring docstring
                   :lambda-list args)))

(define-locative-type psmacro ())

(defmethod locate-object (symbol (locative-type (eql 'psmacro)) locative-args)
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'psmacro))
                                locative-args stream)
  (let* ((arglist (gethash symbol ps::*MACRO-TOPLEVEL-LAMBDA-LIST*))
         (doc-string (find-docstring symbol)))
    (mgl-pax::locate-and-print-bullet locative-type locative-args symbol stream)
    ; (format stream "*")
    (write-char #\Space stream)
    (mgl-pax::print-arglist arglist stream)
    (write-char #\Space stream)

    (when doc-string
          (terpri stream)
          (format stream "~%~A" (mgl-pax::massage-docstring doc-string)))
    
    (terpri stream)))

;; CONTRACT COMBINATOR

(define-locative-type combinator ())

(defmethod locate-object (symbol (locative-type (eql 'combinator)) locative-args)
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'combinator))
                                locative-args stream)
  (let* ((arglist (gethash symbol ps::*MACRO-TOPLEVEL-LAMBDA-LIST*))
         (doc-string (find-docstring symbol)))
    (mgl-pax::locate-and-print-bullet locative-type locative-args symbol stream)
    (write-char #\Space stream)
    (mgl-pax::print-arglist arglist stream)
    (write-char #\Space stream)

    (when doc-string
          (terpri stream)
          (format stream "~%~A" (mgl-pax::massage-docstring doc-string)))
    (terpri stream)))
