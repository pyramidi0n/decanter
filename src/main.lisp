(in-package :cl-user)
(defpackage :decanter
  (:use
   :cl)
  (:export
   ;; ---

   :url-regex-sugar

   ;; ---

   :defurls

   ;; ---

   :defhandler

   ;; ---

   :application

   :mapping
   :autoreload
   :clack-handler
   :application-debug
   :production
   :static-url-prefix
   :application-directory
   :static-directory

   :add-mapping

   :run
   :stop

   :defapp

   ;; ---

   :request

   :request-method
   :target
   :headers
   :body
   :clack-env

   :get-request
   :head-request
   :post-request
   :put-request
   :delete-request
   :connect-request
   :options-request
   :trace-request
   :patch-request

   :request-from-clack

   :set-content-type
   :body-string

   :file-to-vector
   :file-to-string

   ;; ---

   :response

   :code
   :headers
   :body

   :responsep

   :response-200
   :response-303
   :response-400
   :response-404
   :response-500

   :response-text
   :response-html
   :response-css
   :response-js

   :response-redirect
   :response-bad-request
   :response-error

   :clack-response

   ;; ---

   :with-html-string

   ;;---

   :*middleware-session*))
(in-package :decanter)

;; -----------------------------------------------------------------------------

(defparameter *boolean-true-strs* '("t" "true" "1"))
(defparameter *boolean-false-strs* '("nil" "f" "false" "0"))
(defparameter *boolean-strs* (append *boolean-true-strs* *boolean-false-strs*))

(defparameter *default-static-url-prefix* "/static/")
(defparameter *default-static-url-relative-path* #P"static/")

;; -----------------------------------------------------------------------------

(defparameter *url-regex-sugar* nil)

(defun url-regex-sugar ()
  (labels ((url-regex-sugar-read (stream &rest rest)
             (declare (ignore rest))
             (let* ((url (read stream t nil t))
                    (handler (read stream t nil t)))
               (when (not (and (stringp url) (symbolp handler)))
                 (error 'url-regex-sugar
                        :url url
                        :handler handler))
               (list url :regex handler))))
    (when (not *url-regex-sugar*)
      (set-dispatch-macro-character #\# #\? #'url-regex-sugar-read)
      (setf *url-regex-sugar* t))))

;; -----------------------------------------------------------------------------

(define-condition url-regex-sugar (error)
  ((url :initarg :url :reader e-url)
   (handler :initarg :handler :reader e-handler))
  (:report (lambda (condition stream)
             (format stream "~%Decanter Error~%~%")
             (format stream "  URL regex sugar read macro must be of form: #?\"url\" handler~%")
             (format stream "  where \"url\" is a string literal containing a cl-ppcre~%")
             (format stream "  compatible regular expression and handler is a symbol literal.~%~%")
             (format stream "  * URL: ~s~%" (e-url condition))
             (format stream "  * Handler: ~s~%~%" (e-handler condition)))))

(define-condition request-method-not-implemented (error)
  ((method :initarg :method :reader e-method)
   (target :initarg :target :reader e-target)
   (pattern :initarg :pattern :reader e-pattern))
  (:report (lambda (condition stream)
             (format stream "~%Decanter Error~%~%")
             (format stream "  Request method not implemented for the following:~%~%")
             (format stream "  * Request method: ~a~%" (e-method condition))
             (format stream "  * Target: ~a~%" (e-target condition))
             (format stream "  * Pattern: ~a~%~%" (e-pattern condition)))))

(define-condition request-bad-target (error)
  ((method :initarg :method :reader e-method)
   (target :initarg :target :reader e-target)
   (pattern :initarg :pattern :reader e-pattern))
  (:report (lambda (condition stream)
             (format stream "~%Decanter Error~%~%")
             (format stream "  Request has a bad or malformatted target.~%~%")
             (format stream "  * Request method: ~a~%" (e-method condition))
             (format stream "  * Target: ~a~%" (e-target condition))
             (format stream "  * Pattern: ~a~%~%" (e-pattern condition)))))

(define-condition request-bad-query-parameters (error)
  ((method :initarg :method :reader e-method)
   (target :initarg :target :reader e-target)
   (pattern :initarg :pattern :reader e-pattern))
  (:report (lambda (condition stream)
             (format stream "~%Decanter Error~%~%")
             (format stream "  Request has a bad or malformatted query parameters.~%~%")
             (format stream "  * Request method: ~a~%" (e-method condition))
             (format stream "  * Target: ~a~%" (e-target condition))
             (format stream "  * Pattern: ~a~%~%" (e-pattern condition)))))

(define-condition request-bad-body (error)
  ((method :initarg :method :reader e-method)
   (target :initarg :target :reader e-target)
   (pattern :initarg :pattern :reader e-pattern)
   (body-string :initarg :body-string :reader e-body-string))
  (:report (lambda (condition stream)
             (format stream "~%Decanter Error~%~%")
             (format stream "  Request has a bad or malformatted body.~%~%")
             (format stream "  * Request method: ~a~%" (e-method condition))
             (format stream "  * Target: ~a~%" (e-target condition))
             (format stream "  * Pattern: ~a~%" (e-pattern condition))
             (format stream "  * Body: ~a~%~%" (e-body-string condition)))))

(define-condition handler-function-does-not-exist (error)
  ((method :initarg :method :reader e-method)
   (target :initarg :target :reader e-target))
  (:report (lambda (condition stream)
             (format stream "~%Decanter Error~%~%")
             (format stream "  Handler function does not exist for the following:~%~%")
             (format stream "  * Request method: ~a~%" (e-method condition))
             (format stream "  * Target: ~a~%~%" (e-target condition)))))

(define-condition handler-function-return-value (error)
  ((method :initarg :method :reader e-method)
   (target :initarg :target :reader e-target)
   (pattern :initarg :pattern :reader e-pattern))
  (:report (lambda (condition stream)
             (format stream "~%Decanter Error~%~%")
             (format stream "  Handler function must return either a string or a response object.~%~%")
             (format stream "  * Request method: ~a~%" (e-method condition))
             (format stream "  * Target: ~a~%" (e-target condition))
             (format stream "  * Pattern: ~a~%~%" (e-pattern condition)))))

(define-condition handler-function-request-parameter-type-coercion (error)
  ((method :initarg :method :reader e-method)
   (target :initarg :target :reader e-target)
   (pattern :initarg :pattern :reader e-pattern)
   (param :initarg :param :reader e-param)
   (param-value :initarg :param-value :reader e-param-value)
   (type :initarg :type :reader e-type))
  (:report (lambda (condition stream)
             (format stream "~%Decanter Error~%~%")
             (format stream "  Request parameter type coercion failed.~%~%")
             (format stream "  * Request method: ~a~%" (e-method condition))
             (format stream "  * Target: ~a~%" (e-target condition))
             (format stream "  * Pattern: ~a~%" (e-pattern condition))
             (format stream "  * Request parameter: ~s~%" (e-param condition))
             (format stream "  * Request parameter value: ~s~%" (e-param-value condition))
             (format stream "  * Desired type: ~a~%~%" (e-type condition)))))

(define-condition handler-function-request-parameter-malformatted (error)
  ((method :initarg :method :reader e-method)
   (target :initarg :target :reader e-target)
   (pattern :initarg :pattern :reader e-pattern)
   (param :initarg :param :reader e-param))
  (:report (lambda (condition stream)
             (format stream "~%Decanter Error~%~%")
             (format stream "  Request parameter is malformatted.~%~%")
             (format stream "  * Request method: ~a~%" (e-method condition))
             (format stream "  * Target: ~a~%" (e-target condition))
             (format stream "  * Pattern: ~a~%" (e-pattern condition))
             (format stream "  * Request parameter: ~s~%~%" (e-param condition)))))

(define-condition handler-function-request-parameter-predicate-failed (error)
  ((method :initarg :method :reader e-method)
   (target :initarg :target :reader e-target)
   (pattern :initarg :pattern :reader e-pattern)
   (param :initarg :param :reader e-param)
   (predicate :initarg :predicate :reader e-predicate))
  (:report (lambda (condition stream)
             (format stream "~%Decanter Error~%~%")
             (format stream "  Predicate returned nil on request parameter.~%~%")
             (format stream "  * Request method: ~a~%" (e-method condition))
             (format stream "  * Target: ~a~%" (e-target condition))
             (format stream "  * Pattern: ~a~%" (e-pattern condition))
             (format stream "  * Request parameter: ~s~%" (e-param condition))
             (format stream "  * Predicate: ~s~%~%" (e-predicate condition)))))

(define-condition handler-function-request-parameter-required (error)
  ((method :initarg :method :reader e-method)
   (target :initarg :target :reader e-target)
   (pattern :initarg :pattern :reader e-pattern)
   (param :initarg :param :reader e-param))
  (:report (lambda (condition stream)
             (format stream "~%Decanter Error~%~%")
             (format stream "  Request parameter marked as required, but received a null value.~%~%")
             (format stream "  * Request method: ~a~%" (e-method condition))
             (format stream "  * Target: ~a~%" (e-target condition))
             (format stream "  * Pattern: ~a~%" (e-pattern condition))
             (format stream "  * Request parameter: ~s~%~%" (e-param condition)))))

(define-condition handler-function-params-plist-collision (error)
  ((target :initarg :target :reader e-target)
   (pattern :initarg :pattern :reader e-pattern)
   (key :initarg :key :reader e-key)
   (url-vars :initarg :url-vars :reader e-url-vars)
   (request-vars :initarg :request-vars :reader e-request-vars))
  (:report (lambda (condition stream)
             (format stream "~%Decanter Error~%~%")
             (format stream "  The parameter plist for a handler function has a collision between.~%")
             (format stream "  duplicate keywords. This may be caused by using the same name for url~%")
             (format stream "  rule variables, query string variables, or form variables.~%~%")
             (format stream "  * Target: ~a~%" (e-target condition))
             (format stream "  * Pattern: ~a~%" (e-pattern condition))
             (format stream "  * Duplicated keyword: ~s~%" (e-key condition))
             (format stream "  * URL variables: ~s~%" (e-url-vars condition))
             (format stream "  * Request variables: ~s~%~%" (e-request-vars condition)))))

(define-condition match-var-rules-parse-pattern-malformatted (error)
  ((pattern :initarg :pattern :reader e-pattern))
  (:report (lambda (condition stream)
             (format stream "~%Decanter Error~%~%")
             (format stream "  Malformatted variable rule URL pattern.~%~%")
             (format stream "  * Pattern: ~s~%~%" (e-pattern condition)))))

(define-condition match-var-rules-parse-pattern-chunk-malformatted (error)
  ((chunk :initarg :chunk :reader e-chunk))
  (:report (lambda (condition stream)
             (format stream "~%Decanter Error~%~%")
             (format stream "  Malformatted variable rule URL pattern chunk.~%~%")
             (format stream "  * Chunk: ~s~%~%" (e-chunk condition)))))

(define-condition url-mapping-malformatted (error)
  ((pattern :initarg :pattern :reader e-pattern)
   (mapping :initarg :mapping :reader e-mapping))
  (:report (lambda (condition stream)
             (format stream "~%Decanter Error~%~%")
             (format stream "  Malformatted URL mapping beginning with pattern:~%~%")
             (format stream "  * Pattern: ~s~%" (e-pattern condition))
             (format stream "  * URL Mapping: ~s~%~%" (e-mapping condition)))))

;; -----------------------------------------------------------------------------

(defmacro defurls (sym urls)
  `(defparameter ,sym
     (alexandria:flatten ,urls)))

;; -----------------------------------------------------------------------------

(defmacro subhandler (application request pattern pattern-matches &body body)
  (let* ((method (caar body))
         (request-params (cadar body))
         (forms (cddar body)))
    ;; All request params passed to handler functions are strings or nil. They
    ;; are also untrusted input. Therefore, the type checking we are able to
    ;; perform automatically depends on the ability to either:
    ;;
    ;; 1. Rely on safe parsers for simple types or specific types for which
    ;;    parsers are implemented.
    ;;
    ;; 2. Rely on some variant of the Lisp reader that is secure against
    ;;    against untrusted input.
    ;;
    ;; While it is possible that (2) might exist or could be achieved, a cursory
    ;; search hasn't yielded a good result. Therefore the framework doesn't
    ;; support arbitrary type specifiers for now, nor does it attempt to use
    ;; the Lisp reader to parse request parameters into arbitrary structures.
    ;;
    ;; Instead we support the following types specified as keywords in method
    ;; forms of defhandler:
    ;;
    ;; * :integer - uses parse-number
    ;; * :float   - uses parse-number
    ;; * :number  - uses parse-number
    ;; * :string  - uses identity function
    ;; * :boolean - request parameter may include t/nil, true/false, 1/0. becomes
    ;;              t/nil.
    ;;
    ;; We also support arbitrary predicate functions that accept a string as a
    ;; parameter. These can be authored by the user to check more complex types
    ;; using their own parsing techniques, or otherwise validate request
    ;; parameters.
    ;;
    ;; If both a type is specified as a keyword and a predicate is specified,
    ;; then the predicate operates on the param after it is coerced to the
    ;; specified type.
    ;;
    ;; Note that the specified predicate must be defined in advance of the
    ;; invocation of this macro, which does check to verify it exists and is
    ;; functionp. However, the macro is also structured to search for the
    ;; predicate at runtime, which means that if it is redefined while the
    ;; program is running, any code reliant on this macro will use the new
    ;; predicate.
    ;;
    ;; Specifying the :required keyword after the name of the request
    ;; param (and its type keyword, predicate, or both if present) ensures
    ;; that an error is thrown if the request param is nil. Otherwise,
    ;; it's ignored when nil, its type is not checked, predicates are not
    ;; called on it, and no errors are thrown.
    (labels ((malformatted-param-error (param)
               (error 'handler-function-request-parameter-malformatted
                      :method method
                      :target (target request)
                      :pattern pattern
                      :param param))
             (type-coercion-error (request pattern param-name param-value param-type)
               (error 'handler-function-request-parameter-type-coercion
                      :method method
                      :target (target request)
                      :pattern pattern
                      :param param-name
                      :param-value param-value
                      :type param-type))
             (valid-type-key-p (keyword)
               (and (keywordp keyword)
                    (find keyword
                          '(:integer
                            :float
                            :number
                            :string
                            :boolean))))
             (coerce-param-value (request pattern param-value param-name param-type)
               (labels ((number-or-error (param-value predicate)
                          (let ((n (parse-number:parse-number param-value)))
                            (if (funcall predicate n)
                                n
                                (error ""))))
                        (boolean-or-error (param-value)
                          (cond ((find (string-downcase param-value)
                                       *boolean-true-strs*
                                       :test #'string=)
                                 t)
                                ((find (string-downcase param-value)
                                       *boolean-false-strs*
                                       :test #'string=)
                                 nil)
                                (t (error "")))))
                 (handler-case
                     (case param-type
                       (:integer (number-or-error param-value #'integerp))
                       (:float (number-or-error param-value #'floatp))
                       (:number (number-or-error param-value #'numberp))
                       (:string param-value)
                       (:boolean (boolean-or-error param-value)))
                   (error ()
                     (type-coercion-error request pattern param-name param-value param-type)))))
             (param-unqualified-p (param)
               (and (symbolp param)
                    (not (keywordp param))))
             (param-type-key-p (param)
               (and (listp param)
                    (= (length param) 2)
                    (param-unqualified-p (nth 0 param))
                    (valid-type-key-p (nth 1 param))))
             (param-predicate-p (param)
               (and (listp param)
                    (= (length param) 2)
                    (param-unqualified-p (nth 0 param))
                    (functionp (eval (nth 1 param)))))
             (param-type-key-and-predicate-p (param)
               (and (listp param)
                    (= (length param) 3)
                    (param-unqualified-p (nth 0 param))
                    (valid-type-key-p (nth 1 param))
                    (functionp (eval (nth 2 param)))))
             (param-has-required-key-p (param)
               (and (listp param) (find :required param)))
             (param-required-p (param)
               (eq :required (nth (1- (length param)) param)))
             (param-without-required-key (param)
               (if (= 1 (1- (length param)))
                   (nth 0 param)
                   (subseq param 0 (1- (length param))))))
      (macrolet ((if-param-required (required not-required)
                   `(if (param-has-required-key-p param)
                        (if (param-required-p param)
                            ,required
                            (malformatted-param-error param))
                        ,not-required)))
        (let* ((request-param-names
                 (mapcar (lambda (param)
                           (labels ((param-name (param)
                                      (cond ((param-unqualified-p param) param)
                                            ((param-type-key-p param) (nth 0 param))
                                            ((param-predicate-p param) (nth 0 param))
                                            ((param-type-key-and-predicate-p param) (nth 0 param))
                                            (t (malformatted-param-error param)))))
                             (if-param-required
                              (param-name (param-without-required-key param))
                              (param-name param))))
                         request-params))
               (request-param-functions
                 (mapcar (lambda (param)
                           (labels ((required-param-check (param required)
                                      `(when (and ,required (null param-value))
                                         (error 'handler-function-request-parameter-required
                                                :method ,method
                                                :target (target ,request)
                                                :pattern ,pattern
                                                :param (quote ,param))))
                                    (predicate-failed-error (param predicate)
                                      `(error 'handler-function-request-parameter-predicate-failed
                                              :method ,method
                                              :target (target ,request)
                                              :pattern ,pattern
                                              :param (quote ,param)
                                              :predicate ,predicate))
                                    (param-func-unqualified (param required)
                                      `(lambda (param-value)
                                         ,(required-param-check param required)
                                         param-value))
                                    (param-func-type-key (param required)
                                      `(lambda (param-value)
                                         ,(required-param-check param required)
                                         (when param-value
                                           (funcall ,#'coerce-param-value
                                                    ,request
                                                    ,pattern
                                                    param-value
                                                    (quote ,(nth 0 param))
                                                    ,(nth 1 param)))))
                                    (param-func-predicate (param required)
                                      `(lambda (param-value)
                                         ,(required-param-check param required)
                                         (when param-value
                                           (if (funcall ,(nth 1 param) param-value)
                                               param-value
                                               ,(predicate-failed-error param (nth 1 param))))))
                                    (param-func-type-key-and-predicate (param required)
                                      `(lambda (param-value)
                                         ,(required-param-check param required)
                                         (when param-value
                                           (let* ((coerced-value (funcall ,#'coerce-param-value
                                                                          ,request
                                                                          ,pattern
                                                                          param-value
                                                                          (quote ,(nth 0 param))
                                                                          ,(nth 1 param))))
                                             (if (funcall ,(nth 2 param) coerced-value)
                                                 coerced-value
                                                 ,(predicate-failed-error param (nth 2 param)))))))
                                    (param-func (param &key required)
                                      (cond ((param-unqualified-p param) (param-func-unqualified param required))
                                            ((param-type-key-p param) (param-func-type-key param required))
                                            ((param-predicate-p param) (param-func-predicate param required))
                                            ((param-type-key-and-predicate-p param) (param-func-type-key-and-predicate param required))
                                            (t (malformatted-param-error param)))))
                             (if-param-required
                              (param-func (param-without-required-key param) :required t)
                              (param-func param))))
                         request-params))
               (request-param-setfs
                 (loop for name in request-param-names
                       for func in request-param-functions
                       collect `(setf ,name (funcall (eval ,func) ,name)))))
          `(lambda ,(append
                (list application request pattern pattern-matches)
                `(&key ,@request-param-names &allow-other-keys))
             (declare (ignorable ,application ,request ,pattern ,pattern-matches))
             ,@request-param-setfs
             ,@forms))))))

(defmacro defhandler (handler-name &body body)
  ;; This macro builds handlers.
  ;;
  ;; A handler is a top level function that processes a request and returns
  ;; either a string or a response object. Handlers accept the following
  ;; parameters:
  ;;
  ;; * application              - An application object
  ;; * request                  - A request object
  ;; * pattern                  - A string containing a url pattern (whether a
  ;;                              literal, a regex, or variable rules)
  ;; * pattern-matches          - A list containing the results of matching the
  ;;                              request target against the pattern, which
  ;;                              occurs prior to calling the handler.
  ;; * request-parameters-plist - A plist of parameters from the http request
  ;;                              and pattern variable rules, if applicable.
  ;;                              May come from a query string, or a form, etc.
  ;;
  ;; Using this macro is generally advised. However, it's possible for users
  ;; to defun their own handlers.
  (let* ((application (intern (symbol-name 'application)))
         (request (intern (symbol-name 'request)))
         (pattern (intern (symbol-name 'pattern)))
         (pattern-matches (intern (symbol-name 'pattern-matches)))
         (request-parameters-plist (intern (symbol-name 'request-parameters-plist)))
         (lambda-list (list application request pattern pattern-matches request-parameters-plist)))
    (labels ((form-by-method (method body)
               (loop for form in body
                     when (eq method (first form))
                       return form))
             (wrap-subhandler (form)
               (if form
                   (macroexpand-1 `(subhandler application request pattern pattern-matches ,form))
                   `(lambda (application request pattern pattern-matches &key &allow-other-keys)
                      (declare (ignore application pattern-matches))
                      (error 'request-method-not-implemented
                             :method (request-method request)
                             :target (target request)
                             :pattern pattern)))))
      `(defun ,handler-name ,lambda-list
         (apply
          (funcall
           (lambda ()
             (case (request-method request)
               (:get     ,(wrap-subhandler (form-by-method :get     body)))
               (:head    ,(wrap-subhandler (form-by-method :head    body)))
               (:post    ,(wrap-subhandler (form-by-method :post    body)))
               (:put     ,(wrap-subhandler (form-by-method :put     body)))
               (:delete  ,(wrap-subhandler (form-by-method :delete  body)))
               (:connect ,(wrap-subhandler (form-by-method :connect body)))
               (:options ,(wrap-subhandler (form-by-method :options body)))
               (:trace   ,(wrap-subhandler (form-by-method :trace   body)))
               (:patch   ,(wrap-subhandler (form-by-method :patch   body))))))
          (append (list ,application ,request ,pattern ,pattern-matches) ,request-parameters-plist))))))

;; -----------------------------------------------------------------------------

(defun sym-to-keyword (sym)
  (intern (symbol-name sym) "KEYWORD"))

(defun str-to-keyword (str)
  (intern (string-upcase str) "KEYWORD"))

(defun keyword-keyed-plist (plist)
  (loop for (k v) on plist by #'cddr
        nconcing (list
                  (etypecase k
                    (string (str-to-keyword k))
                    (symbol (sym-to-keyword k)))
                  v)))

(defun plist-keys (plist)
  (mapcar #'car (alexandria:plist-alist plist)))

;; -----------------------------------------------------------------------------

(defclass application ()
  ((mapping
    :initarg :mapping
    :initform '()
    :reader mapping)
   (autoreload
    :initarg :autoreload
    :initform nil
    :accessor autoreload)
   (clack-handler
    :initarg :clack-handler
    :initform nil
    :accessor clack-handler)
   (debug
    :initarg :debug
    :initform t
    :accessor application-debug)
   (production
    :initarg :production
    :initform nil
    :accessor production)
   (static-url-prefix
    :initarg :static-url-prefix
    :initform *default-static-url-prefix*
    :accessor static-url-prefix)
   (application-directory
    :initarg :application-directory
    :initform (uiop/os:getcwd)
    :accessor application-directory)
   (static-directory
    :initarg :static-directory
    :initform (merge-pathnames *default-static-url-relative-path* (uiop/os:getcwd))
    :accessor static-directory)))

(defmethod (setf mapping) (mapping (application application))
  (setf (slot-value application 'mapping) (alexandria:flatten mapping)))

(defun application (mapping
                    &key
                      (autoreload nil)
                      (debug t)
                      (production nil)
                      (application-directory (uiop/os:getcwd))
                      (static-directory (merge-pathnames *default-static-url-relative-path* (uiop/os:getcwd))))
  (make-instance 'application
                 :mapping (alexandria:flatten mapping)
                 :autoreload autoreload
                 :debug debug
                 :production production
                 :application-directory application-directory
                 :static-directory static-directory))

(defmethod add-mapping ((application application) pattern handler &key regex)
  (setf (mapping application)
        (append (mapping application)
                (if regex
                    (list pattern :regex handler)
                    (list pattern handler)))))

(defmethod run ((application application) &key
                                            middleware
                                            standalone
                                            (address "127.0.0.1")
                                            (port 5000))
  ;; middleware is a list of Clack middleware.
  (labels ((find-handler (method target)
             (let ((mapping (mapping application))
                   (request-handler nil)
                   (request-pattern nil)
                   (request-pattern-matches '()))
               ;; We search through the mapping in sequence and set the
               ;; handler and pattern to the last match we locate. This
               ;; means that patterns should be specified from least
               ;; specific to most specific by the user.
               (labels ((match-regex (pattern target)
                          (multiple-value-bind (match substrings)
                              (ppcre:scan-to-strings pattern target)
                            (when match
                              (list :type :regex
                                    :match match
                                    :substrings (map 'list #'identity substrings)))))
                        (match-var-rules (pattern target)
                          (labels ((parse-pattern (pattern)
                                     ;; Abandon all hope, ye who enter here.
                                     (when (and (> (length pattern) 0)
                                                (char= #\/ (elt pattern 0)))
                                       (let ((l '())
                                             (i 0))
                                         (loop while (< i (length pattern))
                                               do (cond ((char= #\/ (elt pattern i))
                                                         (if (and (< (1+ i) (length pattern))
                                                                  (char= #\( (elt pattern (1+ i))))
                                                             (progn
                                                               (push "/" l)
                                                               (incf i))
                                                             (let ((j 1)
                                                                   (s "/"))
                                                               (tagbody
                                                                slurp
                                                                  (loop while (and (< (+ i j) (length pattern))
                                                                                   (not (char= #\/ (elt pattern (+ i j)))))
                                                                        do (progn
                                                                             (setf s (concatenate 'string s (string (elt pattern (+ i j)))))
                                                                             (incf j)))
                                                                  (if (and (< (+ i j 1) (length pattern))
                                                                           (char= #\( (elt pattern (+ i j 1))))
                                                                      (progn
                                                                        (setf s (concatenate 'string s "/"))
                                                                        (incf i (1+ j)))
                                                                      (progn
                                                                        (incf i j)
                                                                        (when (< i (length pattern))
                                                                          (setf s (concatenate 'string s "/"))
                                                                          (setf j 1)
                                                                          (go slurp)))))
                                                               (push s l))))
                                                        ((char= #\( (elt pattern i))
                                                         (let ((j 1)
                                                               (s "("))
                                                           (loop while (and (< (+ i j) (length pattern))
                                                                            (not (char= #\) (elt pattern (+ i j)))))
                                                                 do (progn
                                                                      (setf s (concatenate 'string s (string (elt pattern (+ i j)))))
                                                                      (incf j)))
                                                           (if (and (< (+ i j 1) (length pattern))
                                                                    (char= #\/ (elt pattern (+ i j 1))))
                                                               (progn
                                                                 (setf s (concatenate 'string s ")"))
                                                                 (incf i (1+ j)))
                                                               (incf i j))
                                                           (when (and (= i (1- (length pattern)))
                                                                      (char= #\) (elt pattern i)))
                                                             (setf s (concatenate 'string s ")"))
                                                             (incf i 1))
                                                           (push s l)))
                                                        (t (error 'match-var-rules-parse-pattern-malformatted
                                                                  :pattern pattern))))
                                         (values (nreverse l) i))))
                                   (parse-composite-pattern (pattern)
                                     (mapcar (lambda (chunk)
                                               (labels ((chunk-var-p ()
                                                          (and (> (length chunk) 0)
                                                               (char= #\( (elt chunk 0))
                                                               (char= #\) (elt chunk (1- (length chunk))))))
                                                        (has-type-p (var)
                                                          (> (length var) 1))
                                                        (valid-var-p (var)
                                                          (and (= 1 (length var))
                                                               (symbolp (nth 0 var))
                                                               (not (keywordp (nth 0 var)))))
                                                        (valid-var-typed-p (var)
                                                          (and (= 2 (length var))
                                                               (symbolp (nth 0 var))
                                                               (not (keywordp (nth 0 var)))
                                                               (keywordp (nth 1 var))))
                                                        (valid-p (var)
                                                          (and (> (length var) 0)
                                                               (or (and (has-type-p var) (valid-var-typed-p var))
                                                                   (and (not (has-type-p var)) (valid-var-p var))
                                                                   (error 'match-var-rules-parse-pattern-chunk-malformatted
                                                                          :chunk chunk))))
                                                        (chunk-to-var ()
                                                          (read-from-string chunk)))
                                                 (if (chunk-var-p)
                                                     (let ((var (chunk-to-var)))
                                                       (when (not (valid-p var))
                                                         (error 'match-var-rules-parse-pattern-chunk-malformatted
                                                                :chunk chunk))
                                                       (if (has-type-p var)
                                                           (list :name (nth 0 var)
                                                                 :type (nth 1 var))
                                                           (list :name (nth 0 var))))
                                                     chunk)))
                                             (parse-pattern pattern))))
                            (let ((composite-pattern (parse-composite-pattern pattern))
                                  (i 0)
                                  (total-chars-matched 0)
                                  (matched-variables '()))
                              (labels ((decimal-char-p (c)
                                         (or (digit-char-p c)
                                             (char= #\. c)))
                                       (match-literal (pattern-segment)
                                         (let ((j 0))
                                           (loop for c across pattern-segment
                                                 do (if (and (< (+ i j) (length target))
                                                             (char= c (elt target (+ i j))))
                                                        (incf j)
                                                        (return-from match-literal nil)))
                                           (incf i j)
                                           j))
                                       (match-with-var-char-predicate (predicate)
                                         (let ((j 0)
                                               (target-substr nil))
                                           (loop while (and (< (+ i j) (length target))
                                                            (not (char= #\/ (elt target (+ i j))))
                                                            (funcall predicate (elt target (+ i j))))
                                                 do (incf j))
                                           (when (or (= (+ i j) (length target))
                                                     (char= #\/ (elt target (+ i j))))
                                             (setf target-substr (subseq target i (+ i j)))
                                             (incf i j)
                                             (values j target-substr))))
                                       (match-and-check-numeric-var (predicate)
                                         (multiple-value-bind (n-matched num-str)
                                             (match-with-var-char-predicate predicate)
                                           (when (and n-matched
                                                      (handler-case
                                                          (parse-number:parse-number num-str)
                                                        (error ())))
                                             n-matched)))
                                       (match-var-integer ()
                                         (match-and-check-numeric-var #'digit-char-p))
                                       (match-var-float ()
                                         (match-and-check-numeric-var #'decimal-char-p))
                                       (match-var-number ()
                                         (match-and-check-numeric-var #'decimal-char-p))
                                       (match-var-string ()
                                         (match-with-var-char-predicate #'standard-char-p))
                                       (match-var-boolean ()
                                         (multiple-value-bind (n-matched boolean-str)
                                             (match-with-var-char-predicate #'alphanumericp)
                                           (when (and n-matched
                                                      (find (string-downcase boolean-str)
                                                            *boolean-strs*
                                                            :test #'string=))
                                             n-matched)))
                                       (match-var-typeless ()
                                         (match-var-string))
                                       (match-next-pattern-segment ()
                                         (when (and (< total-chars-matched (length target))
                                                    (> (length composite-pattern) 0))
                                           (let* ((pattern-segment (pop composite-pattern))
                                                  (n-matched (etypecase pattern-segment
                                                               (string (match-literal pattern-segment))
                                                               (list (case (getf pattern-segment :type)
                                                                       (:string (match-var-string))
                                                                       (:boolean (match-var-boolean))
                                                                       (:integer (match-var-integer))
                                                                       (:float (match-var-float))
                                                                       (:number (match-var-number))
                                                                       (otherwise (match-var-typeless)))))))
                                             (when n-matched
                                               (incf total-chars-matched n-matched)
                                               (when (listp pattern-segment)
                                                 (push (append pattern-segment
                                                               (list :value (subseq target (- i n-matched) i)))
                                                       matched-variables))
                                               n-matched)))))
                                (loop while (match-next-pattern-segment))
                                (when (and (= total-chars-matched (length target))
                                           (= 0 (length composite-pattern)))
                                  (list :type :var-rules
                                        :total-chars-matched total-chars-matched
                                        :variables (reverse matched-variables)))))))
                        (match-check (&key pattern handler regex?)
                          (let ((match (if regex?
                                           (match-regex pattern target)
                                           (match-var-rules pattern target))))
                            (when match
                              (setf request-handler handler
                                    request-pattern pattern
                                    request-pattern-matches match)))))
                 (dotimes (i (length mapping))
                   (if (stringp (nth i mapping))
                       (cond ((and (< (+ i 2) (length mapping))
                                   (keywordp (nth (+ i 1) mapping))
                                   (eq :regex (nth (+ i 1) mapping))
                                   (symbolp (nth (+ i 2) mapping))
                                   (not (keywordp (nth (+ i 2) mapping))))
                              (match-check :pattern (nth i mapping)
                                           :regex? (nth (+ i 1) mapping)
                                           :handler (nth (+ i 2) mapping))
                              (incf i 2))
                             ((and (< (+ i 1) (length mapping))
                                   (symbolp (nth (+ i 1) mapping))
                                   (not (keywordp (nth (+ i 1) mapping))))
                              (match-check :pattern (nth i mapping)
                                           :handler (nth (+ i 1) mapping))
                              (incf i 1))
                             (t (error 'url-mapping-malformatted
                                       :pattern (nth i mapping)
                                       :mapping mapping)))
                       (error 'url-mapping-malformatted
                              :pattern (nth i mapping)
                              :mapping mapping))))
               (when (and (null request-handler) (null request-pattern))
                 (error 'handler-function-does-not-exist
                        :method method
                        :target target))
               (values (symbol-function request-handler)
                       request-pattern
                       request-pattern-matches)))
           (url-var-rules-match-p (pattern-matches)
             (eq (getf pattern-matches :type) :var-rules))
           (url-var-rules-plist (pattern-matches)
             (apply #'append
                    (mapcar (lambda (var)
                              (list (sym-to-keyword (getf var :name))
                                    (getf var :value)))
                            (getf pattern-matches :variables))))
           (handler-params-plist (request pattern pattern-matches)
             (let ((url-var-rules-plist
                     (when (url-var-rules-match-p pattern-matches)
                       (url-var-rules-plist pattern-matches)))
                   (request-params-plist
                     (case (request-method request)
                       (:get     (get-params-plist     pattern request))
                       (:head    (head-params-plist    pattern request))
                       (:post    (post-params-plist    pattern request))
                       (:put     (put-params-plist     pattern request))
                       (:delete  (delete-params-plist  pattern request))
                       (:connect (connect-params-plist pattern request))
                       (:options (options-params-plist pattern request))
                       (:trace   (trace-params-plist   pattern request))
                       (:patch   (patch-params-plist   pattern request))))
                   (duplicated-key nil))
               (labels ((key-collision (pl1 pl2)
                          (let ((pl1-keys (plist-keys pl1))
                                (pl2-keys (plist-keys pl2)))
                            (loop for k in pl1-keys
                                  when (find k pl2-keys)
                                    do (progn
                                         (setf duplicated-key k)
                                         (return-from key-collision t))))))
                 (when (key-collision url-var-rules-plist request-params-plist)
                   (error 'handler-function-params-plist-collision
                          :target (target request)
                          :pattern pattern
                          :key duplicated-key
                          :url-vars url-var-rules-plist
                          :request-vars request-params-plist))
                 (append url-var-rules-plist request-params-plist))))
           (wrap-response (method target pattern val)
             (cond ((stringp val) (clack-response (response-html val)))
                   ((responsep val) (clack-response val))
                   (t (error 'handler-function-return-value
                             :method method
                             :target target
                             :pattern pattern))))
           (clack-application (env)
             (labels ((clack-application-handle-request ()
                        (let ((request (request-from-clack env)))
                          (multiple-value-bind (handler pattern pattern-matches)
                              (find-handler (request-method request) (target request))
                            (when (application-debug application)
                              (format t "~%~%~%")
                              (format t "------------~%")
                              (format t "HTTP REQUEST~%")
                              (format t "------------~%")
                              (format t "~%env: ~s~%" env)
                              (format t "~%request: ~s~%" request)
                              (format t "* method: ~s~%" (request-method request))
                              (format t "* target: ~s~%" (target request))
                              (format t "* headers: ~s~%" (headers request))
                              (format t "* body: ~s~%" (body-string request)))
                            (let ((clack-response
                                    (wrap-response
                                     (request-method request) (target request) pattern
                                     (funcall handler
                                              application
                                              request
                                              pattern
                                              pattern-matches
                                              (handler-params-plist request pattern pattern-matches)))))
                              (when (application-debug application)
                                (format t "~%~%~%")
                                (format t "-------------~%")
                                (format t "HTTP RESPONSE~%")
                                (format t "-------------~%")
                                (format t "~%response: ~s~%" clack-response))
                              clack-response)))))
               (if (production application)
                   (handler-case
                       (clack-application-handle-request)
                     (request-method-not-implemented (e)
                       (declare (ignore e))
                       (clack-response (response-404 '(:content-type "text/plain") "Not Found")))
                     (request-bad-target (e)
                       (declare (ignore e))
                       (clack-response (response-400 '(:content-type "text/plain") "Bad Request")))
                     (request-bad-query-parameters (e)
                       (declare (ignore e))
                       (clack-response (response-400 '(:content-type "text/plain") "Bad Request")))
                     (request-bad-body (e)
                       (declare (ignore e))
                       (clack-response (response-400 '(:content-type "text/plain") "Bad Request")))
                     (handler-function-does-not-exist (e)
                       (declare (ignore e))
                       (clack-response (response-404 '(:content-type "text/plain") "Not Found")))
                     (handler-function-return-value (e)
                       (declare (ignore e))
                       (clack-response (response-500 '(:content-type "text/plain") "Internal Server Error")))
                     (handler-function-request-parameter-type-coercion (e)
                       (declare (ignore e))
                       (clack-response (response-400 '(:content-type "text/plain") "Bad Request")))
                     (handler-function-request-parameter-malformatted (e)
                       (declare (ignore e))
                       (clack-response (response-400 '(:content-type "text/plain") "Bad Request")))
                     (handler-function-request-parameter-predicate-failed (e)
                       (declare (ignore e))
                       (clack-response (response-400 '(:content-type "text/plain") "Bad Request")))
                     (handler-function-request-parameter-required (e)
                       (declare (ignore e))
                       (clack-response (response-400 '(:content-type "text/plain") "Bad Request")))
                     (handler-function-params-plist-collision (e)
                       (declare (ignore e))
                       (clack-response (response-400 '(:content-type "text/plain") "Bad Request")))
                     (error (e)
                       (declare (ignore e))
                       (clack-response (response-500 '(:content-type "text/plain") "Internal Server Error"))))
                   (clack-application-handle-request)))))
    (labels ((start-clack ()
               (setf (clack-handler application)
                     (clack:clackup
                      (lambda (env)
                        (labels ((run-clack-app ()
                                   (let ((clack-application-with-middleware
                                           (lack.builder:builder
                                            `(:static :path ,(static-url-prefix application)
                                                      :root ,(static-directory application))
                                            #'clack-application)))
                                     (mapcar (lambda (m)
                                               (setf clack-application-with-middleware
                                                     (funcall m clack-application-with-middleware)))
                                             middleware)
                                     (funcall clack-application-with-middleware env))))
                          (if (production application)
                              (handler-case
                                  (run-clack-app)
                                (error (e)
                                  (declare (ignore e))
                                  ;; Error handler of last resort in case anything breaks in e.g. the Lack
                                  ;; middleware.
                                  (clack-response (response-400 '(:content-type "text/plain") "Bad Request"))))
                              (run-clack-app))))
                      :address address
                      :port port)))
             (run-as-standalone ()
               (unwind-protect
                    (handler-case
                        (progn
                          (start-clack)
                          (sleep most-positive-fixnum))
                      (sb-sys:interactive-interrupt (c)
                        (declare (ignore c))
                        (format t "~%Received SIGINT. Terminating.~%")))
                 (progn
                   (stop application)
                   (format t "Done!~%"))))
             (run-in-repl ()
               (start-clack)))
      (if standalone
          (run-as-standalone)
          (run-in-repl)))))

(defmethod stop ((application application))
  (clack:stop (clack-handler application)))

(defmacro defapp (sym
                  mapping
                  &key
                    (autoreload nil)
                    (debug t)
                    (production nil)
                    (application-directory (uiop/os:getcwd))
                    (static-directory (merge-pathnames *default-static-url-relative-path* (uiop/os:getcwd))))
  `(defparameter ,sym
     (application (alexandria:flatten ,mapping)
                  :autoreload ,autoreload
                  :debug ,debug
                  :production ,production
                  :application-directory ,application-directory
                  :static-directory ,static-directory)))

;; -----------------------------------------------------------------------------

(defclass request ()
  ((method
    :initarg :method
    :initform nil
    :accessor request-method)
   (target
    :initarg :target
    :initform nil
    :accessor target)
   (headers
    :initarg :headers
    :initform '()
    :accessor headers)
   (body
    :initarg :body
    :initform nil
    :accessor body)
   ;; Decanter includes the Clack environment in request objects in the event
   ;; end users have a use for it in their handlers. However, we don't make
   ;; any use of this internally once a request object is constructed.
   ;;
   ;; Note that the process of constructing a request object renders the
   ;; :raw-body stream in clack-env subsequently unusable.
   (clack-env
    :initarg :clack-env
    :initform '()
    :reader clack-env)))

(defun request (&optional
                  (method :get)
                  (target "/")
                  (headers '())
                  (body nil)
                  (clack-env '()))
  (make-instance 'request
                 :method method
                 :target target
                 :headers headers
                 :body body
                 :clack-env clack-env))

(defun get-request (target &optional (headers '()) (clack-env '()))
  (request :get target headers nil clack-env))

(defun head-request (target &optional (headers '()) (clack-env '()))
  (request :head target headers nil clack-env))

(defun post-request (target headers body &optional (clack-env '()))
  (request :post target headers body clack-env))

(defun put-request (target headers body &optional (clack-env '()))
  (request :put target headers body clack-env))

(defun delete-request (target &optional (headers '()) (body nil) (clack-env '()))
  (request :delete target headers body clack-env))

(defun connect-request (target &optional (headers '()) (clack-env '()))
  (request :connect target headers nil clack-env))

(defun options-request (target &optional (headers '()) (clack-env '()))
  (request :options target headers nil clack-env))

(defun trace-request (target &optional (headers '()) (clack-env '()))
  (request :trace target headers nil clack-env))

(defun patch-request (target headers body &optional (clack-env '()))
  (request :patch target headers body clack-env))

(defun request-from-clack (clack-env)
  (labels ((has-content-p ()
             (let* ((content-length (getf clack-env :content-length))
                    (headers (getf clack-env :headers))
                    (transfer-encoding (gethash "transfer-encoding" headers)))
               (or content-length
                   (string= transfer-encoding "chunked"))))
           (empty-vector ()
             (make-array 0 :element-type '(unsigned-byte 8)))
           (body-vector ()
             ;; Slurping the :raw-body flexi-stream means that its position
             ;; cannot be reset.
             ;;
             ;; Consequently any subsequent operations on a body stream, e.g.
             ;; header parsing, must occur on a new stream created from the
             ;; slurped octets.
             (let* ((content-length (getf clack-env :content-length))
                    (body-stream (getf clack-env :raw-body))
                    (body-vector (http-body.util:slurp-stream
                                  body-stream
                                  content-length)))
               body-vector))
           (body-octets ()
             (if (has-content-p) (body-vector) (empty-vector))))
    (let ((target (getf clack-env :request-uri))
          (headers (append
                    (list :content-type (getf clack-env :content-type)
                          :content-length (getf clack-env :content-length))
                    (keyword-keyed-plist
                     (alexandria:hash-table-plist (getf clack-env :headers)))))
          (body (body-octets)))
      (case (getf clack-env :request-method)
        (:get     (get-request     target headers      clack-env))
        (:head    (head-request    target headers      clack-env))
        (:post    (post-request    target headers body clack-env))
        (:put     (put-request     target headers body clack-env))
        (:delete  (delete-request  target headers body clack-env))
        (:connect (connect-request target headers      clack-env))
        (:options (options-request target headers      clack-env))
        (:trace   (trace-request   target headers      clack-env))
        (:patch   (patch-request   target headers body clack-env))))))

(defmethod set-content-type ((request request) content-type)
  (setf (getf (headers request) :content-type) content-type))

(defmethod body-string ((request request))
  (flexi-streams:octets-to-string (body request)))

(defun file-to-vector (file-input-stream)
  (declare (type (or null
                     flexi-streams::vector-input-stream)
                 file-input-stream))
  (alexandria:read-stream-content-into-byte-vector file-input-stream))

(defun file-to-string (file-input-stream)
  (declare (type (or null
                     flexi-streams::vector-input-stream)
                 file-input-stream))
  (flexi-streams:octets-to-string
   (alexandria:read-stream-content-into-byte-vector file-input-stream)))

;; -----------------------------------------------------------------------------

(defun query-string (uri-path)
  (let ((pos (position #\? uri-path)))
    (when pos (subseq uri-path (1+ pos)))))

(defun query-string-plist (query-string)
  (apply #'append
         (mapcar (lambda (query-parameter)
                   (let ((split-pair
                           (uiop:split-string query-parameter :separator "=")))
                     (list (str-to-keyword (car split-pair))
                           (cadr split-pair))))
                 (uiop:split-string query-string :separator "&"))))

(defun query-params-plist (pattern request)
  (handler-case
      (query-string-plist (query-string (target request)))
    (error (e)
      (declare (ignore e))
      (error 'request-bad-query-parameters
             :method (request-method request)
             :target (target request)
             :pattern pattern))))

(defun get-params-plist (pattern request)
  (query-params-plist pattern request))

(defun post-params-plist (pattern request)
  (let ((content-type (getf (headers request) :content-type))
        (content-length (getf (headers request) :content-length))
        (body (body request))
        (body-string (body-string request))
        (query-params (query-params-plist pattern request)))
    ;; Note that it is possible to specify url query parameters with any
    ;; type of http request. When handling a post request, we choose to
    ;; bind a plist of those query parameters, if any, to the
    ;; :url-query-parameters keyword argument for the handler function.
    ;;
    ;; This means that Decanter users shouldn't name any POST parameter
    ;; "url-query-parameters", lest we end up with clashing keyword
    ;; arguments passed to the handler. The likelihood of this is quite low,
    ;; and Decanter documentation cautions against it.
    (macrolet ((request-bad-body-handler (&body body)
                 `(handler-case
                      ,@body
                    (error (e)
                      (declare (ignore e))
                      (error 'request-bad-body
                             :method (request-method request)
                             :target (target request)
                             :pattern pattern
                             :body-string (body-string request))))))
      (labels ((url-encoded-p ()
                 (string= "application/x-www-form-urlencoded" content-type))
               (form-data-p ()
                 (let ((s "multipart/form-data"))
                   (string= s (subseq content-type 0 (length s)))))
               (unspecified-p ()
                 (null content-type)))
        (append (when query-params (list :request-query-parameters query-params))
                (cond ((url-encoded-p) (request-bad-body-handler
                                        (query-string-plist body-string)))
                      ((form-data-p) (keyword-keyed-plist
                                      (alexandria:alist-plist
                                       (flexi-streams:with-input-from-sequence (s body)
                                         (request-bad-body-handler
                                          (http-body:parse
                                           content-type
                                           content-length
                                           (flexi-streams:make-flexi-stream s)))))))
                      ;; In unspecified cases we treat the content-type as if it were
                      ;; "application/octet-stream". Since we've read the whole
                      ;; stream already, we're just dealing with the octets, and we leave
                      ;; those on the request object in the event the handler wants to
                      ;; work with them.
                      ;;
                      ;; See: https://www.rfc-editor.org/rfc/rfc2616#section-7.2.1
                      ((unspecified-p) (progn
                                         (set-content-type request "application/octet-stream")
                                         '()))
                      ;; In all other cases, just leave the body on the request object
                      ;; in the event the handler wants to work with it.
                      (t '())))))))

(defun head-params-plist (pattern request)
  (query-params-plist pattern request))

(defun put-params-plist (pattern request)
  (post-params-plist pattern request))

(defun delete-params-plist (pattern request)
  (query-params-plist pattern request))

(defun connect-params-plist (pattern request)
  ;; CONNECT requests have specific requirements for the request target,
  ;; which should just consist of a host and a port.
  ;;
  ;; This means no query string parameters, and since CONNECT requests
  ;; have no body, no arameters are specified there, either. A CONNECT
  ;; handler should expect no function arguments to be bound at all.
  ;;
  ;; We use a URI parser to validate the target's syntax, specifying a
  ;; throwaway scheme in doing so.
  ;;
  ;; See: https://www.rfc-editor.org/rfc/rfc9110.html#name-connect
  (multiple-value-bind (scheme userinfo host port path query fragment)
      (uri-parse:parse (concatenate 'string "http://" (target request)))
    (if (and (stringp scheme)
             (null userinfo)
             (stringp host)
             (stringp port)
             (null path)
             (null query)
             (null fragment))
        '()
        (error 'request-bad-target
               :method (request-method request)
               :target (target request)
               :pattern pattern))))

(defun options-params-plist (pattern request)
  (query-params-plist pattern request))

(defun trace-params-plist (pattern request)
  (query-params-plist pattern request))

(defun patch-params-plist (pattern request)
  ;; While RFC does not specify any particular syntax for the PATCH request
  ;; body, we choose to examine the headers as if it were a POST request, and
  ;; automatically bind handler parameters when content-type is either
  ;; application/x-www-form-urlencoded or multipart/form-data.
  ;;
  ;; See: https://www.rfc-editor.org/rfc/rfc5789#section-2
  (post-params-plist pattern request))

;; -----------------------------------------------------------------------------

(defclass response ()
  ((code
    :initarg :code
    :accessor code)
   (headers
    :initarg :headers
    :initform '()
    :accessor headers)
   (body
    :initarg :body
    :initform nil
    :accessor body)))

(defun responsep (obj)
  (equalp (type-of obj) 'response))

(defun response (&optional (code 200) (headers '()) (body nil))
  (make-instance 'response
                 :code code
                 :headers headers
                 :body body))

(defun response-200 (&optional (headers '()) (body nil))
  (response 200 headers body))

(defun response-303 (location)
  (response 303 `(:location ,location) nil))

(defun response-400 (&optional (headers '()) (body nil))
  (response 400 headers body))

(defun response-404 (&optional (headers '()) (body nil))
  (response 404 headers body))

(defun response-500 (&optional (headers '()) (body nil))
  (response 500 headers body))

(defun response-text (body)
  (response-200 '(:content-type "text/plain") body))

(defun response-html (body)
  (response-200 '(:content-type "text/html") body))

(defun response-css (body)
  (response-200 '(:content-type "text/css") body))

(defun response-js (body)
  (response-200 '(:content-type "text/javascript") body))

(defun response-redirect (location)
  (response-303 location))

(defun response-bad-request (&optional (headers '()) (body nil))
  (response-400 headers body))

(defun response-error (&optional (headers '()) (body nil))
  (response-500 headers body))

(defmethod clack-response ((response response))
  (list (code response)
        (headers response)
        (list (body response))))

;; -----------------------------------------------------------------------------

(defmacro with-html-string (&body body)
  `(let* ((returned-str nil)
          (written-html
            (with-output-to-string (spinneret:*html*)
              (setf returned-str (spinneret:with-html ,@body)))))
     (if returned-str returned-str written-html)))

;; -----------------------------------------------------------------------------

(defparameter *middleware-session*
  lack/middleware/session:*lack-middleware-session*)
