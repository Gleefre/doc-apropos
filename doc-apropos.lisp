(defpackage #:doc-apropos
  (:use #:cl)
  (:export #:initialize-index
           #:doc-apropos
           #:*doc-types*))

(in-package #:doc-apropos)

(defvar *doc-types* '(VARIABLE FUNCTION STRUCTURE TYPE SETF))

(defvar *index*)

(defun print-symbol (symbol)
  (format nil "~a~:[::~;:~]~a"
          (package-name (symbol-package symbol))
          (eq :external
              (nth-value 1 (find-symbol (symbol-name symbol)
                                        (symbol-package symbol))))
          (symbol-name symbol)))

(defun make-symbol-document (symbol &aux (doc (make-instance 'montezuma:document))
                                         (count 0))
  (montezuma:add-field doc (montezuma:make-field "symbol"
                                                 (print-symbol symbol)
                                                 :index NIL))
  (values
   doc
   (dolist (doc-type *doc-types* count)
     (alexandria:when-let ((documentation (or (documentation symbol doc-type))))
       (montezuma:add-field doc (montezuma:make-field (symbol-name doc-type) documentation))
       (incf count)))))

(defun initialize-index ()
  (setf *index* (make-instance 'montezuma:index))
  (let ((definitions-count 0) (symbols-count 0))
    (do-all-symbols (symbol)
      (montezuma:add-document-to-index *index*
                                       (multiple-value-bind (doc count)
                                           (make-symbol-document symbol)
                                         (incf definitions-count count)
                                         doc))
      (incf symbols-count))
    (values symbols-count definitions-count)))

(initialize-index)

(defvar *doc-type*)

(defun doc-report (%doc score &aux (doc (montezuma:get-document *index* %doc)))
  (declare (ignorable score))
  (format t "~a~%~a~2%"
          (montezuma:document-value doc "symbol")
          (montezuma:document-value doc (symbol-name *doc-type*))))

(defun doc-apropos (string &optional (*doc-type* 'function))
  (montezuma:search-each *index*
                         (format nil "~a:\"~a\"" *doc-type* string)
                         #'doc-report))
