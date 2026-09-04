;;;; eliza.lisp 

(defun eliza--space-p (c)
  "Devuelve T si c es un separador de espacio/linea/tab."
  (or (char= c #\Space) (char= c #\Tab) (char= c #\Newline) (char= c #\Return)))

(defparameter *eliza-punct-chars* ".,;:()?!\"'")

(defun clean-and-tokenize (line)
  "Convierte LINE a minúsculas, sustituye puntuación por espacios y la divide en tokens."
  (when line
    (let* ((lower (string-downcase line))
           (clean-str
            (with-output-to-string (out)
              (loop for c across lower do
                    (if (find c *eliza-punct-chars* :test #'char-equal)
                        (write-char #\Space out)
                        (write-char c out))))))
      (let ((tokens '())
            (cur ""))
        (loop for c across clean-str do
             (if (eliza--space-p c)
                 (when (> (length cur) 0)
                   (push cur tokens)
                   (setf cur ""))
                 (setf cur (concatenate 'string cur (string c)))))
        (when (> (length cur) 0) (push cur tokens))
        (nreverse tokens)))))

(defparameter *templates*
  (list
   ;; Hola … soy …
   (list (list 'hola (list 's) 'como 'estas 'soy (list 's))
         (list "hola" 1 "como estas tu yo soy" 0 "como esta tu ?")
         (list 1 5))

   ;; Hola … soy …
   (list (list 'hola (list 's) 'soy (list 's))
         (list "hola" 1 "mi nombre es" 0 "en que te puedo ayudar")
         (list 1 3))

   ;; Saludos simples
   (list (list 'hola (list 's))
         (list "Hola" "como" "estas" "tu" "?")
         nil)

   (list (list 'buendia (list 's))
         (list "Buendia" "Como" "estas" "tu" "?")
         nil)

   ;; flags (simulan predicados Prolog like/does/is...)
   (list (list 'te 'gustan 'las (list 's) (list 's))
         (list 'flagLike)
         (list 3))

   (list (list 'tu 'eres (list 's) (list 's))
         (list 'flagDo)
         (list 2))

   (list (list 'que 'eres 'tu (list 's))
         (list 'flagIs)
         (list 2))

   ;; fallback
   (list nil (list "Please" "explain" "a" "little" "more" ".") nil)
   ))


(defun element-match-p (templ-el token)
  "Coincidencia de un elemento con token."
  (cond
    ((null templ-el) t)
    ((and (consp templ-el)
          (symbolp (first templ-el))
          (string= (symbol-name (first templ-el)) "s"))
     (not (null token)))   ;; wildcard s => un token
    ((symbolp templ-el)
     (and token (string= (symbol-name templ-el) token)))
    (t nil)))

(defun match-template (stim input)
  "Devuelve T si stim coincide con input."
  (labels ((rec (slist ilist)
             (cond
               ((null slist) t)
               ((null ilist) nil)
               (t
                (let ((se (first slist))
                      (it (first ilist)))
                  (if (element-match-p se it)
                      (rec (rest slist) (rest ilist))
                      nil))))))
    (rec stim input)))


(defun get-token-at (input pos)
  (if (and input (>= pos 0) (< pos (length input)))
      (nth pos input)
      ""))

(defun build-response-from-resp (resp indices input)
  "Sustituye enteros por tokens según índices."
  (mapcan
   (lambda (e)
     (cond
       ((integerp e)
        (let ((idx (and indices (nth e indices))))
          (list (get-token-at input (or idx -1)))))
       ((symbolp e) (list (symbol-name e)))
       ((stringp e) (list e))
       (t (list (princ-to-string e)))))
   resp))


(defun handle-flag (flag indices input)
  (let ((arg-token (and indices (> (length indices) 0)
                        (get-token-at input (first indices)))))
    (case flag
      (flagLike (if arg-token
                    (list "I" "can't" "tell" "if" "I" "like" arg-token)
                    (list "I" "don't" "have" "preferences.")))
      (flagDo   (if arg-token
                    (list "I" "sometimes" "do" arg-token)
                    (list "I" "do" "various" "things.")))
      (flagIs   (if arg-token
                    (list "I" "am" "a" arg-token)
                    (list "I" "am" "Eliza" ".")))
      (t (list "Please" "explain" "a" "little" "more" ".")))))


(defun find-matching-template (input)
  (find-if (lambda (tpl)
             (let ((stim (first tpl)))
               (if (null stim)
                   (null input) ;; fallback solo si input vacío
                   (match-template stim input))))
           *templates*))


(defun respond-to (input)
  (let ((tpl (find-matching-template input)))
    (when tpl
      (let* ((resp (second tpl))
             (indices (third tpl)))
        (if (and (consp resp) (symbolp (first resp))
                 (find (first resp) '(flagLike flagDo flagIs)))
            (format t "~{~a~^ ~}~%" (handle-flag (first resp) indices input))
            (format t "~{~a~^ ~}~%" (build-response-from-resp resp indices input)))))))


(defun eliza-loop ()
  (format t "Hola, soy Eliza. Ingresa tu consulta.~%")
  (loop
     (format t "~%> ")
     (let ((line (read-line *query-io* nil nil)))
       (when (null line) (return))
       (let ((tokens (clean-and-tokenize line)))
         (cond
           ((or (string= (first tokens) "adios")
                (string= (first tokens) "bye"))
            (format t "Adios.~%")
            (return))
           (t (respond-to tokens)))))))

