
(defun is-op (str)
    (cond
        ((string= str "+") "OP_PLUS")
        ((string= str "-") "OP_MINUS")
        ((string= str "/") "OP_DIV")
        ((string= str "(") "OP_OP")
        ((string= str ")") "OP_CP")
        ((string= str "\"") "OP_OC")
        ((string= str ",") "OP_COMMA")
        ((string= str "**") "OP_DBMULT")
        (T NIL)
    )
)

(defun is-keyword (str)
    (setf s (string-downcase str))
    (cond
        ((string= s "and") "KW_AND")
        ((string= s "or") "KW_OR")
        ((string= s "not") "KW_NOT")
        ((string= s "equal") "KW_EQUAL")
        ((string= s "less") "KW_LESS")
        ((string= s "nil") "KW_NIL")
        ((string= s "list") "KW_LIST")
        ((string= s "append") "KW_APPEND")
        ((string= s "concat") "KW_CONCAT")
        ((string= s "set") "KW_SET")
        ((string= s "deffun") "KW_DEFFUN")
        ((string= s "for") "KW_FOR")
        ((string= s "if") "KW_IF")
        ((string= s "exit") "KW_EXIT")
        ((string= s "load") "KW_LOAD")
        ((string= s "disp") "KW_DISP")
        ((string= s "true") "KW_TRUE")
        ((string= s "false") "KW_FALSE")
        (T NIL)
    )
)



(defun is-digit (chr)
	(if (and (string>= chr "0") (string<= chr "9"))
        "VALUE"
    ))


(defun is-white (str)
    (setf c (char str 0))
    (or (char= c #\SPACE) (char= c #\RETURN)
        (char= c #\NEWLINE) (char= c #\TAB)
    )
)
(defun is-alnum (chr)
	(or (and (string>= chr "0") (string<= chr "9")) (and (string>= chr "A") (string<= chr "Z")) (and (string>= chr "a") (string<= chr "z")))
    "IDENTIFIER"
    )
(defun is-bracket (chr)
	(or (string= chr "(") (string= chr ")")))

(defun is-quote (chr)
	(string= chr "\""))
(defun split-string-with-delims (str delims)
  (labels ((delim-p (c)
             (position c delims))
           (tokens (stri test)
             (when (> (length stri) 0)
               (let ((p (position-if test stri)))
                 (if p
                     (if (= p 0)
                         (cons (subseq stri 0 (1+ p))
                               (tokens (subseq stri (1+ p) nil) test))
                         (cons (subseq stri 0 p)
                               (tokens (subseq stri p nil) test)))
                     (cons stri nil))))))
    (tokens str #'delim-p)))
    
(defvar line (read-line))

(defparameter my-list (split-string-with-delims line '(#\, #\" #\  #\) #\( )) )
(print my-list)
(defun printList(listP)
    (setq size (length listP))
    (setq index 0)
    (loop
        (setq str (nth index listP))
        (cond
                ((is-keyword str)
                    (print (is-keyword str))
                )
                ((is-digit str)
                    (print (is-digit str))
                )
                ((is-op str)
                    (print (is-op str))
                )
                ((is-alnum str)
                  (print (is-alnum str))  
                )
        )
        (terpri)
        (setq index (+ index 1))
        (if(equal index size) (return))

    )
)

(printList my-list)
