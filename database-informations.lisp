(defparameter *logged* nil)
(defparameter *logged-account* nil)
(defparameter *admin-logged* nil)
(defparameter *admin-list* '())
(defparameter *users* '())
(defparameter *components* '(*RAM* *video-cards* *CPU* *mother-boards* *storage* *PSU*))
(defparameter *video-cards* (make-hash-table))
(defparameter *mother-boards* (make-hash-table))
(defparameter *RAM* (make-hash-table))
(defparameter *CPU* (make-hash-table))
(defparameter *storage* (make-hash-table))
(defparameter *PSU* (make-hash-table)) ;;power-supply-unit
(defparameter *guest-commands* '(login create-account help))
(defparameter *consumer-commands* '(view-history delete-order delete-account log-off build-pc help))
(defparameter *admin-commands* '(a-view-history view-history a-delete-order delete-order a-delete-account delete-account log-off a-make-admin a-delete-admin a-add-component a-delete-component build-pc help))
(defparameter *param-req-com* '(a-view-history a-delete-order a-delete-account a-make-admin a-delete-admin a-add-component a-delete-component))
(defparameter file (open "Proiecte/Database/database-data.txt"))

(setf *admin-list* (string-to-list (read-line file)))
(setf *users* (string-to-list (read-line file)))

(defun initialize (user-list)
    (when user-list
     (progn
      (setf (symbol-value (car user-list)) (make-hash-table))
      (setf (gethash 'password (eval (car user-list))) (read-line file))
      (setf (gethash 'orders (eval (car user-list))) (string-to-list (read-line file)))
      (initialize (cdr user-list)))))
(initialize *users*)

(defun stock-initialize()
 (labels ((read-details(destination a-list)
           (when a-list
             (setf (gethash (car a-list) (eval destination)) (string-to-list (read-line file)))
             (read-details destination (cdr a-list)))))
        (loop for x in *components* do(progn
     (setf (gethash 'the-list (eval x)) (string-to-list (read-line file)))
     (setf (gethash 'chars (eval x)) (string-to-list (read-line file)))
     (read-details x (gethash 'the-list (eval x)))))))
(stock-initialize)
(close file)

(defun update-database()
 (setf file-output (open "Proiecte/Database/database-data.txt" :direction :output :if-exists :supersede))
 (labels ((update-clients (user-list)
           (when user-list
              (format file-output (gethash 'password (eval (car user-list)))) (terpri file-output)
              (format file-output (list-to-string (gethash 'orders (eval (car user-list))))) (terpri file-output)
              (update-clients (cdr user-list))))
              (update-components (component clist)
              (when clist
                (format file-output (list-to-string (gethash (car clist) (eval component)))) (terpri file-output)
                (update-components component (cdr clist)))))
   (format file-output (list-to-string *admin-list*)) (terpri file-output)
   (format file-output (list-to-string *users*)) (terpri file-output)
   (update-clients *users*)
   (loop for x in *components*
     do(progn
       (format file-output (list-to-string (gethash 'the-list (eval x)))) (terpri file-output)
       (format file-output (list-to-string (gethash 'chars (eval x)))) (terpri file-output)
       (update-components x (gethash 'the-list (eval x)))))
    (close file-output)))
