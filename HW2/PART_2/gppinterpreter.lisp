(load "gpplexer.lisp")
(defvar *output-file* "output.txt")
(defvar hastable nil)
(defvar tokens nil)
(defvar line-list nil)

(defun gppinterpreter (&optional filename)
        (if filename 
        (let ((output (open *output-file* :direction :output)))
	        (file-interpreter filename output)  (close output)) (terminal-interpreter)))

(defun terminal-interpreter ()
        (loop for line = (read-line) while (not (string= line "")) do (START (interpreter line) nil)) 
)
(defun file-interpreter (filename output)
        
        (with-open-file (stream filename)
                (loop for line = (read-line stream nil)
                        while line do (START (interpreter line) output)
                )
        )
        (print "output.txt is created")
)


;;--------------------------------------START 
(defun START (pairlist output)
        (setq tokens (cdr pairlist))
        (setq line-list (car pairlist))
        (if (setq result (INPUT))
                (if output      (format output "Syntax OK.~%Result: ~s~%" (car result))   
                                (format t "Syntax OK.~%Result: ~s~%" (car result)))
                (if output      (format output "SYNTAX_ERROR Expression not recognized~%")   
                                (format t "SYNTAX_ERROR Expression not recognized~%"))))


;;--------------------------------------INPUT 
(defun INPUT ()

        (let ((result))
        (cond
                ((setq result (EXPI  0)) result)   ; EXPI-> (+ EXPI EXPI) 
                ((setq result (EXPB  0)) result)  ;EXPI-> (- EXPI EXPI)
                ((setq result (EXPILIST  0)) result)    ;EXPI-> (/ EXPI EXPI)EXIT-rule
                ((setq result (EXIT-rule  0)) result)  
                ))) 


;;--------------------------------------EXPI 
(defun EXPI (index)
        (let ((result))
        (cond
                ((setq result (EXPI-rule-PLUS  index)) result)   ; EXPI-> (+ EXPI EXPI) 
                ((setq result (EXPI-rule-MINUS  index)) result)  ;EXPI-> (- EXPI EXPI)
                ((setq result (EXPI-rule-DIV  index)) result)    ;EXPI-> (/ EXPI EXPI)
                ((setq result (EXPI-rule-MULT  index)) result)   ;EXPI-> (* EXPI EXPI)
                ((setq result (EXPI-rule-DBMULT  index)) result)   ;EXPI-> (** EXPI EXPI)
                ((setq result (EXPI-rule-VALUE  index)) result)  ; EXPI-> VALUE
                ((setq result (EXPI-rule-IDENTIFIER  index)) result)
                ((setq result (EXPI-rule-SET  index)) result)
                ((setq result (EXPI-rule-IF  index)) result)
                ))) 

(defun EXPI-rule-PLUS (index)
        (when   (and    (string= (nth index tokens) "OP_OP") 
                        (string= (nth (+ index 1) tokens) "OP_PLUS") (setq res (EXPI (+ index 2))))
        (when (and (setq res1 (EXPI  (cdr res))) (string= "OP_CP" (nth (cdr res1) tokens)))
                (return-from EXPI-rule-PLUS (cons (+ (car res) (car res1)) (+ 1 (cdr res1)))))))
(defun EXPI-rule-MINUS (index)
        (when   (and    (string= (nth index tokens) "OP_OP") 
                        (string= (nth (+ index 1) tokens) "OP_MINUS") (setq res (EXPI  (+ index 2))))
        (when (and (setq res1 (EXPI  (cdr res))) (string= "OP_CP" (nth (cdr res1) tokens)))
                (return-from EXPI-rule-MINUS (cons (- (car res) (car res1)) (+ 1 (cdr res1)))))))
(defun EXPI-rule-DIV (index)
        (when   (and    (string= (nth index tokens) "OP_OP") 
                        (string= (nth (+ index 1) tokens) "OP_DIV") (setq res (EXPI  (+ index 2))))
        (when (and (setq res1 (EXPI  (cdr res))) (string= "OP_CP" (nth (cdr res1) tokens)))
                (return-from EXPI-rule-DIV (cons (/ (car res) (car res1)) (+ 1 (cdr res1)))))))
(defun EXPI-rule-MULT (index)
        (when   (and    (string= (nth index tokens) "OP_OP") 
                        (string= (nth (+ index 1) tokens) "OP_MULT") (setq res (EXPI  (+ index 2))))
        (when (and (setq res1 (EXPI  (cdr res))) (string= "OP_CP" (nth (cdr res1) tokens)))
                (return-from EXPI-rule-MULT (cons (* (car res) (car res1)) (+ 1 (cdr res1)))))))
(defun EXPI-rule-DBMULT (index)
        (when   (and    (string= (nth index tokens) "OP_OP") 
                        (string= (nth (+ index 1) tokens) "OP_DBMULT") (setq res (EXPI  (+ index 2))))
        (when (and (setq res1 (EXPI  (cdr res))) (string= "OP_CP" (nth (cdr res1) tokens)))
                (return-from EXPI-rule-DBMULT (cons (expt (car res) (car res1)) (+ 1 (cdr res1)))))))
(defun EXPI-rule-VALUE (i)
	(when (string= "VALUE" (nth i tokens)) (return-from EXPI-rule-VALUE (cons (parse-integer (nth i line-list)) (+ i 1)))))

(defun EXPI-rule-IDENTIFIER (i) ; EXPI-> IDENTIFIER
        (when (string= "IDENTIFIER" (nth i tokens)) (cons (listGet (nth i line-list)) (+ i 1))))

(defun EXPI-rule-SET (index)
        (let ((res))
        (when   (and    (string= (nth index tokens) "OP_OP") 
                        (string= (nth (+ index 1) tokens) "KW_SET") (setq res (EXPI (+ index 2))))
                        
        (when (and (setq res1 (EXPI (cdr res))) (string= "OP_CP" (nth (cdr res1) tokens)))
                (return-from EXPI-rule-SET (cons (cdar (listAdd (nth (- (cdr res) 1) line-list) (car res1))) (+ 1 (cdr res1))))))))

(defun EXPI-rule-IF (index)
        (when   (and    (string= (nth index tokens) "OP_OP") 
                        (string= (nth (+ index 1) tokens) "KW_IF") (setq res (EXPB (+ index 2))))
        (if (equal (car res) nil) (return-from EXPI-rule-IF (list "if false"))
        (when (and (setq res1 (EXPI  (cdr res))) (string= "OP_CP" (nth (cdr res1) tokens)) )
                (return-from EXPI-rule-IF (cons (car res1) (+ 1 (cdr res1))))))))


;;--------------------------------------EXPB 
(defun EXPB (index)
        (let ((result))
        (cond
                ((setq result (EXPB-rule-AND  index)) result)   ; EXPI-> (+ EXPI EXPI) 
                ((setq result (EXPB-rule-OR  index)) result)
                ((setq result (EXPB-rule-EQUAL  index)) result)  ;EXPI-> (- EXPI EXPI)
                ((setq result (EXPB-rule-NOT  index)) result) 
                )))
(defun EXPB-rule-AND (index)
        (let ((res))
        (when   (and    (string= (nth index tokens) "OP_OP") 
                        (string= (nth (+ index 1) tokens) "KW_AND") (setq res (EXPB  (+ index 2))))
        (when (and (setq res1 (EXPB  (cdr res))) (string= "OP_CP" (nth (cdr res1) tokens)))
                (return-from EXPB-rule-AND (cons (and (car res) (car res1)) (+ 1 (cdr res1))))))))

(defun EXPB-rule-OR (index)
        (let ((res))
        (when   (and    (string= (nth index tokens) "OP_OP") 
                        (string= (nth (+ index 1) tokens) "KW_OR") (setq res (EXPB  (+ index 2))))
        (when (and (setq res1 (EXPB  (cdr res))) (string= "OP_CP" (nth (cdr res1) tokens)))
                (return-from EXPB-rule-AND (cons (or (car res) (car res1)) (+ 1 (cdr res1))))))))

(defun EXPB-rule-NOT (index)
        (let ((res))
        (when   (and    (string= (nth index tokens) "OP_OP") 
                        (string= (nth (+ index 1) tokens) "KW_NOT") (setq res (EXPB  (+ index 2))))
                        (if (string= (car res) "T") (return-from EXPB-rule-NOT))
                        (return-from EXPB-rule-NOT (cons "T" (cdr res) )  ))))

(defun EXPB-rule-EQUAL (index)
        (when   (and    (string= (nth index tokens) "OP_OP") 
                        (string= (nth (+ index 1) tokens) "KW_EQUAL") (setq res (EXPI  (+ index 2))))
        (when (and (setq res1 (EXPI  (cdr res))) (string= "OP_CP" (nth (cdr res1) tokens)))
                (return-from EXPB-rule-EQUAL (cons (equal (car res) (car res1)) (+ 1 (cdr res1)))))))


;;--------------------------------------EXPILIST
(defun EXPILIST (index)
        (let ((result))
          (cond
                ((setq result (EXPILIST-rule-CONCAT index)) result)   
                ((setq result (EXPILIST-rule-APPEND index)) result)  
                ((setq result (LISTVALUE-rule index)) result)    
                )))

(defun EXPILIST-rule-CONCAT (index)
        (let ((res))
        (when   (and    (string= (nth index tokens) "OP_OP") 
                        (string= (nth (+ index 1) tokens) "KW_CONCAT") (setq res (EXPILIST (+ index 2))))
        
        (when (and (setq res1 (EXPILIST (cdr res))) (string= "OP_CP" (nth (cdr res1) tokens))) 
                (return-from EXPILIST-rule-CONCAT (cons (append (car res) (car res1)) (+ 1 (cdr res1))))))))

(defun EXPILIST-rule-APPEND (index)
        (let ((res))
        (when   (and    (string= (nth index tokens) "OP_OP") 
                        (string= (nth (+ index 1) tokens) "KW_APPEND") (setq res (EXPI (+ index 2))))
        
        (when (and (setq res1 (EXPILIST (cdr res))) (string= "OP_CP" (nth (cdr res1) tokens))) 
                (return-from EXPILIST-rule-APPEND (cons (push (write-to-string (car res)) (car res1)) (+ 1 (cdr res1))))))))

(defun LISTVALUE-rule (index)
        (when   (and    (string= (nth index tokens) "OP_OP") 
                        (string= (nth (+ index 1) tokens) "KW_LIST"))
                        (return-from LISTVALUE-rule (VALUES-rule (+ index 2) ()))))

(defun VALUES-rule (index values)
        (if (string= "OP_CP" (nth index tokens))
                (return-from VALUES-rule (cons (reverse values) (+ index 1)))        
        (push (nth index line-list) values))
        (VALUES-rule (+ index 1) values))

(defun EXIT-rule (index)
        (when   (and    (string= (nth index tokens) "OP_OP") 
                        (string= (nth (+ index 1) tokens) "KW_EXIT") (string= (nth (+ index 2) tokens) "OP_CP"))
                (exit)))
;;--------------------------------------list functions
(defun listGet (id)
        (listGet-recursive hastable id))

(defun listGet-recursive (hastable id)
	(when hastable (if (string= (caar hastable) id) (cdar hastable) (listGet-recursive (cdr hastable) id))))
(defun listAdd (id value)
        (if (listGet id)  (listUpdate hastable id value) (push (cons id value) hastable)))

(defun listUpdate (update-list id value) 
        (when update-list (if (string= (caar update-list) id) (setf (cdar hastable) value) (listUpdate (cdr update-list) id))))


(if *args* (gppinterpreter (car *args*)) (gppinterpreter))
