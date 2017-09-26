;; prepare input: get tokens and mtokens from sentence (specific to
;; cl-conllu)

;; this code adapts rec-entities.lisp for use with the :cl-conllu
;; library. it will build the list of sentences getting the forms from
;; their tokens, substituting tokens by mtokens when
;; necessary. sentence lists can be lists of lists of chars for input
;; to rec-entities.lisp, or lists of strings for human consumption.

;; use (dir-recognize-entities dir-path/*.conllu entities-path) to
;; recognize entities in all *.conllu files in a given directory,
;; using the entity list at entities-path. the output will be as

;; ((file-id (sent-id (ent-id index))))

;; (dir-entities-not-found ) and (dir-count-entities ) will return
;; their namesakes. the dir-prefix in these functions indicate that
;; files are being read from a directory one at a time, so that the
;; memory heap does not exhaust. they are also adapted to remove the
;; unnecessary (in their use cases) file-id, which the original
;; functions in rec-entities.lisp don't handle.

;; in the end of the file there are examples of entity recognition and
;; count in conllu files.


(ql:quickload :cl-conllu)
(ql:quickload :split-sequence)
(ql:quickload :alexandria)

(load (compile-file #P"~/git/ed-2017-2/src/projeto/rec-entities.lisp"))

;;
;; pre-process entity list
(defparameter *decontractions* (copy-tree '(("dos" . ("de" "os"))
                                            ("do" . ("de" "o"))
                                            ("das" . ("de" "as"))
                                            ("da" . ("de" "a"))
                                            ("pelas" . ("por" "as"))
                                            ("pelas" . ("por" "a"))
                                            ("pelos" . ("por" "os"))
                                            ("pelo" . ("por" "o"))
                                            ("às" . ("a" "as"))
                                            ("à" . ("a" "a"))
                                            ("na" . ("em" "a"))
                                            ("nas" . ("em" "as"))
                                            ("nos" . ("em" "os"))
                                            ("no" . ("em" "o"))
                                            ("aos" . ("a" "os"))
                                            ("ao" . ("a" "o")))))

(defun decontract-names (name-tokens subs &optional decontracted)
  (if (endp name-tokens)
      decontracted
      (let ((name (first name-tokens)))
        (decontract-names (rest name-tokens) subs
                          (append decontracted
                                  (or (rest (assoc name subs
                                                   :test #'equal))
                                      (list name)))))))

(defun conllu-process-entity (string &optional
                                            (subs *decontractions*)
                                            (separator #\space))
  (alexandria:mappend #'str-to-char
                      (decontract-names
                       (split-sequence:split-sequence separator
                                                      (string-trim
                                                       '(#\space #\tab)
                                                       string))
                       subs)))

(defun conllu-process-entities (entities)
  (process-entities entities :process-fn #'conllu-process-entity))

;;
;; process conllu sentences to list of list of chars
(defun process-form (token)
  (str-to-char (cl-conllu:token-form token)))

(defun cons-tokens-from-sentences (sentences)
  (mapcar #'cl-conllu:sentence-tokens sentences))

(defun chars-from-sentence (sentence)
  (mapcar #'process-form sentence))

(defun chars-from-sentences (sentences)
  (mapcar #'chars-from-sentence sentences))

(defun forms-from-sentence (sentence-tokens)
  (mapcar #'cl-conllu:token-form sentence-tokens))

(defun forms-from-sentences (sentences)
  (mapcar #'forms-from-sentence sentences))


;;
;; reading
(defun chars-in-file (filepath)
  "read conllu file and return lists of chars for each sentence."
  (let* ((raw-sents (cl-conllu:read-file filepath))
        (token-sents (cons-tokens-from-sentences raw-sents))
        (char-sents (chars-from-sentences token-sents)))
    char-sents))

(defun trie-from-entities (path)
  (let* ((raw-ents (read-entities path))
        (ents (conllu-process-entities raw-ents))
        (trie (start-trie ents)))
    (values trie ents)))

(defun aux-dir-recognize-entities (trie file-paths &optional entities)
  (if (endp file-paths)
      entities
      (let* ((file-path (first file-paths))
             (file-id (file-namestring file-path))
             (chars-sents (chars-in-file file-path))
             (sent-entities (recognize-ents-in-sentences trie
                                                         chars-sents)))
        (aux-dir-recognize-entities trie (rest file-paths)
                                    (acons file-id sent-entities
                                           entities)))))

(defun dir-recognize-entities (dir-path entities-path)
  "recognize entities in all .conllu files in a directory. (this reads
one file at time, which prevents stack overflow."
  (multiple-value-bind (trie *) (trie-from-entities entities-path)
    (let ((file-paths (directory dir-path)))
      (aux-dir-recognize-entities trie file-paths))))

;;
;; entity statistics
(defun get-entids-from-entrecs-with-fileid (entrecs)
  (alexandria:mappend (lambda (entrec) (get-entids-from-entrecs
                                        (rest entrec)))
                      entrecs))

(defun dir-entities-not-found (dir-path entities-path)
  (let ((entrecs (dir-recognize-entities dir-path entities-path)))
    (ents-not-found (get-entids-from-entrecs-with-fileid entrecs)
                    (get-number-of-entities entities-path))))
;; (mapcar (lambda (entid) (get-entity raw-ents entid)) *) can make
;; list using entities' names and not id's

(defun dir-count-entities (dir-path entities-path)
  (count-entities
   (get-entids-from-entrecs-with-fileid
    (dir-recognize-entities dir-path entities-path))))
      
;;
;; tests
#|
(let* ((raw-sents (cl-conllu:read-file
                   #p"/home/bruno/docs/dhbb-sample/2.conllu"))
       (token-sents (cons-tokens-from-sentences raw-sents))
       (form-sents (forms-from-sentences token-sents))
       (char-sents (chars-from-sentences token-sents))
       (raw-ents (read-entities #p"entities.txt"))
       (ents (conllu-process-entities raw-ents))
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
  (viz-count raw-ents (count-entities
                       (get-entids-from-entrecs rec-entities))))

;;

(let ((raw-ents (read-entities #p"path to entities list")))
           (with-open-file (stream "~/resultado.txt"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (format stream (write-to-string (viz-count raw-ents (count-entities

                                         (get-entids-from-entrecs-with-fileid (dir-recognize-entities #p"path to conllu files"  #p"path to entities list"))))))))
|#
