;; prepare input: get tokens and mtokens from sentence (specific to
;; cl-conllu)

(ql:quickload :cl-conllu)
(compile-file #p"~/git/ed-2017-2/src/trie.lisp")
(load #p"~/git/ed-2017-2/src/trie.fasl")
(compile-file #p"~/git/ed-2017-2/src/rec-entities.lisp")
(load #p"~/git/ed-2017-2/src/rec-entities.fasl")


;;
;; pre-processing
(defun get-token-form (token)
  (etypecase token
    (cl-conllu:token (cl-conllu:token-form token))
    (cl-conllu:mtoken (cl-conllu:mtoken-form token))))

(defun aux-construct-token-list (tokens start end
				 &key token-list mtoken)
  (when (endp tokens) (return-from aux-construct-token-list
			(values nil token-list)))
;; because an mtoken can be the last one in a sentence.
  (let* ((token (first tokens))
	(token-id (cl-conllu:token-id token))
	(rest-tokens (rest tokens)))
    (cond ((< token-id start)
	   (aux-construct-token-list rest-tokens start end
				     :token-list (cons token
						       token-list)
				     :mtoken mtoken))
	  ((= token-id start)
	   (aux-construct-token-list rest-tokens start end
				     :token-list (cons mtoken
						       token-list)))
	  ((and (> token-id start) (<= token-id end))
	   (aux-construct-token-list rest-tokens start end
				     :token-list token-list))
	  ((> token-id end)
	   (values tokens token-list)))))

(defun construct-token-list (tokens mtokens &optional token-list)
  "construct list of tokens, replacing tokens by their respective
mtokens: (pt em o governo) -> (pt no governo)"
  (if (endp mtokens)
      (append (reverse token-list) tokens)
      (let* ((mtoken (first mtokens))
	     (start (cl-conllu:mtoken-start mtoken))
	     (end (cl-conllu:mtoken-end mtoken)))
	(multiple-value-bind (rest-tokens result-token-list)
	    (aux-construct-token-list tokens start end :mtoken mtoken)
	  (construct-token-list rest-tokens (rest mtokens)
				(append result-token-list
					token-list))))))

(defun process-form (token)
  (process-string (get-token-form token)))

(defun cons-tokens-from-sentence (sentence)
  "this pre-processes cl-conllu classes for input"
  (construct-token-list (cl-conllu:sentence-tokens sentence)
                        (cl-conllu:sentence-mtokens sentence)))

(defun cons-tokens-from-sentences (sentences)
  (mapcar #'cons-tokens-from-sentence sentences))

(defun forms-from-sentence (sentence)
  (mapcar #'get-token-form sentence))

(defun forms-from-sentences (sentences)
  (mapcar #'forms-from-sentence sentences))

(defun chars-from-sentence (sentence)
  (mapcar #'process-form sentence))

(defun chars-from-sentences (sentences)
  (mapcar #'chars-from-sentence sentences))

;;
;; tests
(let* ((raw-sents (cl-conllu:read-file
                   #p"~/git/query-conllu/CF1.conllu"))
       (token-sents (cons-tokens-from-sentences raw-sents))
       (form-sents (forms-from-sentences token-sents))
       (char-sents (chars-from-sentences token-sents))
       (raw-ents (read-entities #p"~/git/ed-2017-2/src/entities.txt"))
       (ents (process-entities raw-ents))
       (trie (start-trie ents))
       (rec-entities (recognize-ents-in-sentences trie char-sents)))
  raw-sents
  token-sents
  form-sents
  char-sents
  raw-ents
  ents
  rec-entities
  (visualize-entities-and-sentences form-sents (reverse rec-entities) raw-ents)
  (viz-count raw-ents (count-and-remove-entities
                       (get-entids-from-entrecs rec-entities))))
