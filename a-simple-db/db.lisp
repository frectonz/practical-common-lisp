;; A transaction
;;    amount: integer
;;    currency: string
;;    description: string
;;    personal: boolean

(defvar *db* nil)

(defun make-txn (amount currency description personal)
  (list
    :amount amount
    :currency currency
    :description description
    :personal personal))

(defun add-record (txn) (push txn *db*))

(defun dump-db ()
  (dolist (txn *db*)
    (format t "~{~a:~20t~a~%~}~%" txn)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-txn ()
  (make-txn
    (or (parse-integer (prompt-read "Amount") :junk-allowed t) 0)
    (prompt-read "Currency")
    (prompt-read "Description")
    (y-or-n-p    "Personal")))

(defun add-txns ()
  (loop (add-record (prompt-for-txn))
    (if (not (y-or-n-p "Another?")) (return))))

(defun save-db (filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (with-standard-io-syntax (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select-txns (selector-fn)
  (remove-if-not selector-fn *db*))

; (defun where (&key amount currency description (personal nil personal-p))
;   #'(lambda (txn)
;       (and
;         (if amount      (equal (getf txn :amount)           amount) t)
;         (if currency    (equal (getf txn :currency)       currency) t)
;         (if description (equal (getf txn :description) description) t)
;         (if personal-p  (equal (getf txn :personal)       personal) t))))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

(defun update-txns (selector-fn &key amount currency description (personal nil personal-p))
  (setf *db*
    (mapcar
      #'(lambda (txn)
          (when (funcall selector-fn txn)
            (if amount      (setf (getf txn :amount)           amount))
            (if currency    (setf (getf txn :currency)       currency))
            (if description (setf (getf txn :description) description))
            (if personal-p  (setf (getf txn :personal)       personal)))
        txn) *db*)))

(defun delete-txns (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
