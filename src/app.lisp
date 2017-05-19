;;;; app.lisp - core application code

(in-package :my-little-fts)

(declaim (inline doc-path))


;;; Parameters And Variables

(defvar *db* nil
  "Pathname for database.")

(defvar *docdir* #p"./"
  "Pathname for HTML documents.")

(defparameter *compilation-date*
  #.(t1123:as-rfc-1123 (get-universal-time))
  "Datetime of compilation in RFC 1123 format, for caching.")

(defparameter *css* nil
  "Parameter to hold CSS, for server side caching.")


;;; Database

(defun init-db ()
  (with-open-database (db *db*)
    (execute-non-query db "
create virtual table docs
using fts5(path, title, text, tokenize = \"unicode61\")
")))

(defun html->contents (pathname)
  (let ((contents (alexandria:read-file-into-string pathname)))
    (plump:parse contents)))

(defmacro do-html-file ((var rootdir) &body body)
  `(uiop:collect-sub*directories
    ,rootdir
    (constantly t)
    (constantly t)
    (lambda (dir)
      (dolist (,var (uiop:directory-files dir))
        (when (or (search ".html" (namestring ,var))
                  (search ".htm" (namestring ,var))
                  (search ".xhtml" (namestring ,var)))
          ,@body)))))

(defun doc-path (ndrop pathname)
  (let ((directory (cons :relative
                         (nthcdr ndrop (pathname-directory pathname)))))
    (make-pathname :directory directory :defaults pathname)))

(defun insert-document (db path title text)
  (execute-non-query db "
insert into docs (path, title, text) values (?, ?, ?)
" path title text))

(defun insert-html-contents (rootdir)
  (let ((ndirs (length (pathname-directory (uiop:getcwd)))))
    (with-open-database (db *db*)
      (do-html-file (html-pathname rootdir)
        (let* ((contents (html->contents html-pathname))
               (titles (plump:get-elements-by-tag-name contents
                                                       "title")))
          (format t "Indexing: ~a~%" html-pathname)
          (insert-document db
                           (namestring (doc-path ndirs html-pathname))
                           (substitute #\space #\newline
                                       (plump:text (car titles)))
                           (plump:text contents)))))))

(defun print-row (title path snippet)
  (format t "~&TITLE: ~a~%PATH: ~a~%SNIPPET:~a~%~%"
          title path snippet))

(defun query-db (query &optional (fn #'print-row))
  (with-open-database (db *db*)
    (loop :with q
            := (prepare-statement db "
select title, path, snippet(docs, 2, '<b>', '</b>', '', 32) from docs
where docs match ? order by rank
")
              :initially (bind-parameter q 1 query)
          :while (step-statement q)
          :do (funcall fn
                       (statement-column-value q 0)
                       (statement-column-value q 1)
                       (statement-column-value q 2))
          :finally (finalize-statement q))))


;;; Contents Rendering

(defun gen-style ()
  (let ((lass:*pretty* nil))
    (lass:compile-and-write
     '(body
       :padding 0
       :margin 0
       :line-height 1.1em)
     '(header
       :border-bottom solid 4px "#bcb3f5"
       :background-color "#847ee0"
       :padding 3px 0 3px 0
       :display flex
       :flex-direction row)
     '(.logo
       :padding 28px 0 0 0
       :margin 0px 0 0 0
       :order -1
       :width 100px
       (img :margin 0 0 0 25px))
     '(.query
       :padding 26px 0 0 5px
       :order 1)
     '(input[type=text]
       :font-size 1.1em
       :height 1.7em
       :padding-left 8pt
       :transition "box-shadow 0.3s")
     '(.results
       :margin 0 0 0 100px
       (ul
        :padding 0
        :list-style none
        (li
         :padding 5px
         :width 580px
         (.snippet
          :font-size 80%
          :color "#6f726e"
          :margin 0 0 18px 0)
         (.path
          :font-size 80%
          :color "#3e7e42"
          :margin 2px 0 0 0)))
       (a
        :text-decoration none)))))

(defun style ()
  (or *css* (setf *css* (gen-style))))

(defun gen-result (query)
  (let ((*print-pretty* nil))
    (spinneret:with-html
      (:doctype)
      (:html
       (:head
        (:title (format nil "my-little-fts: ~a" query))
        (:link :rel "stylesheet" :type "text/css" :href "/style.css"))
       (:body
        (:header
         (:section
          :class "logo"
          (:img :src "/alien.png"))
         (:section
          :class "query"
          (:form
           :method "get"
           (:input :type "text" :name "q" :size 54 :value query))))
        (handler-case
            (unless (emptyp query)
              (:section
               :class "results"
               (:ul
                (query-db query (lambda (title path snippet)
                                  (:li
                                   (:a :href path title)
                                   (:p :class "path" path)
                                   (:p :class "snippet"
                                       (:raw snippet) "...")))))))
          (t (obj)
            (format *error-output* "~&query-db: ~a~%" obj)
            (values))))))))


;;; Server

(defun parse-query (raw)
  (if (eql 0 (search "q=" raw))
      (let ((trimmed (quri:url-decode raw)))
        (destructuring-bind (q . rest) (cl-ppcre:split "\\=" trimmed)
          (declare (ignore q))
          (or (and rest (substitute #\space #\+
                                    (format nil "~{~a~^ ~}" rest)))
              "")))
      ""))

(defun run-server (&key (port 8080) (server :woo) (use-thread nil))
  (clack:clackup
   (lack:builder
    :accesslog
    (lambda (app)
      (lambda (env)
        (let ((path-info (getf env :path-info)))
          (cond
            ((string= path-info "/")
             (let* ((raw-query (getf env :query-string))
                    (query (parse-query raw-query))
                    (body (with-output-to-string (s)
                            (let ((*standard-output* s))
                              (gen-result query))))
                    (len (length body)))
               `(200 (:content-type
                      "text/html; charset=utf-8"
                      :content-length ,len)
                     (,body))))
            ((string= path-info "/style.css")
             `(200 (:content-type
                    "text/css; charset=utf-8"
                    :cache-control "max-age=604800"
                    :last-modified ,*compilation-date*)
                   (,(style))))
            ((string= path-info "/alien.png")
             `(200 (:content-type
                    "image/png"
                    :cache-control "max-age=604800"
                    :last-modified ,*compilation-date*)
                   ,*alien-png*))
            (t (funcall app env))))))
    (:static :path "/" :root *docdir*)
    (lambda (env)
      (declare (ignore env))
      `(200 (:content-type "text/html; charset=utf-8")
            ("Unhandled request."))))
   :server server
   :port port
   :use-thread use-thread))


;;; Command Line interface

(defun help ()
  (princ "USAGE: my-little-fts COMMAND OPTIONS

COMMANDS:

   index -d DB -f DIR:
      index files under DIR and make DB.

   serve -d DB [-f DIR] [-p PORT]:
      start web interface with files under DIR and DB.
      DIR is optional, default is current directory.
      PORT is optional, default is 8080.

   help:
      show this help and exit

EXAMPLE:

  Build an index DB, then start a web server with the DB:

     $ ls -F
     html/
     $ my-little-fts index -f html -d index.db
     ...
     $ my-little-fts serve -d index.db
     ...

  Then access http://localhost:8080 from web browser.

"))

(defun get-flag (flag lst)
  (let ((xs (member flag lst :test #'equal)))
    (and (not (null xs)) (cadr xs))))

(defmacro with-db-and-dir ((&key args (error-when-db-exist t))
                           &body body)
  (let* ((%dir (gensym "DIR"))
         (%db (gensym "DB")))
    `(let ((,%dir (get-flag "-f" ,args))
           (,%db (get-flag "-d" ,args)))
       (assert (or (null ,%dir)
                   (and (not (null ,%dir))
                        (uiop:directory-exists-p ,%dir)))
               (,%dir)
               "Directory ~s does not exist." ,%dir)
       (assert (and (not (null ,%db))
                    ,(if error-when-db-exist
                         `(not (uiop:file-exists-p ,%db))
                         `(uiop:file-exists-p ,%db)))
               (,%db)
               "Index database ~s already exist." ,%db)
       (let ((*db* ,%db)
             (*docdir* (or ,%dir *docdir*)))
         ,@body))))

(defun index (args)
  (with-db-and-dir (:args args :error-when-db-exist t)
    (init-db)
    (insert-html-contents *docdir*)))

(defun serve (args)
  (with-db-and-dir (:args args :error-when-db-exist nil)
    (let ((port (let ((p (get-flag "-p" args)))
                  (or (and p (parse-integer p :junk-allowed t))
                      8080))))
      (run-server :use-thread nil :port port))))

(defun main (&rest argv)
  (cond
    ((equal "index" (car argv)) (index (cdr argv)))
    ((equal "serve" (car argv)) (serve (cdr argv)))
    (t (help))))
