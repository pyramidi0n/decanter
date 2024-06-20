(in-package :cl-user)
(defpackage :decanter.ext.db
  (:use
   :cl)
  (:import-from :dbi
                #:connect
                #:disconnect

                #:with-transaction

                #:begin-transaction
                #:commit
                #:rollback)
  (:export

   ;; ---

   :connect
   :disconnect

   :with-transaction

   :begin-transaction
   :commit
   :rollback

   ;; ---

   :format-db-keyword
   :format-db-row
   :format-db-rows

   ;; ---

   :sql-statement
   :sql-query

   :sxql-statement
   :sxql-query

   :statement
   :query))
(in-package :decanter.ext.db)

;; -----------------------------------------------------------------------------

(defun format-db-keyword (keyword)
  (let ((keystring (write-to-string keyword)))
    (intern
     (string-upcase
      (substitute #\- #\_ (subseq keystring 2 (1- (length keystring)))))
     "KEYWORD")))

(defun format-db-row (plist)
  (mapcar (lambda (element)
            (if (keywordp element)
                (format-db-keyword element)
                element))
          plist))

(defun format-db-rows (row-list)
  (mapcar #'format-db-row row-list))

;; -----------------------------------------------------------------------------

(defun sql-statement (db-connection sql-string &rest sql-string-params)
  (dbi:execute (dbi:prepare db-connection sql-string) sql-string-params))

(defun sql-query (db-connection sql-string &rest sql-string-params)
  (format-db-rows
   (dbi:fetch-all
    (dbi:execute (dbi:prepare db-connection sql-string) sql-string-params))))

(defun sxql-statement (db-connection sxql)
  (multiple-value-bind (sql-string sql-values)
      (sxql:yield sxql)
    (dbi:execute (dbi:prepare db-connection sql-string) sql-values)))

(defun sxql-query (db-connection sxql)
  (multiple-value-bind (sql-string sql-values)
      (sxql:yield sxql)
    (format-db-rows
     (dbi:fetch-all
      (dbi:execute (dbi:prepare db-connection sql-string) sql-values)))))

(defun statement (db-connection sql-str-or-sxql &rest sql-string-params)
  (if (stringp sql-str-or-sxql)
      (apply #'sql-statement
             (append (list db-connection sql-str-or-sxql) sql-string-params))
      (sxql-statement db-connection sql-str-or-sxql)))

(defun query (db-connection sql-str-or-sxql &rest sql-string-params)
  (if (stringp sql-str-or-sxql)
      (apply #'sql-query
             (append (list db-connection sql-str-or-sxql) sql-string-params))
      (sxql-query db-connection sql-str-or-sxql)))
