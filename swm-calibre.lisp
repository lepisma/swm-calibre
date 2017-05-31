;;;; swm-calibre.lisp

(in-package #:swm-calibre)

;;; "swm-calibre" goes here. Hacks and glory await!
(export '(*calibre-root* open-book))

(defvar *calibre-root* nil "Path to calibre root directory")
(defvar *format-preference* '("pdf"
                              "cbr"
                              "cbz"
                              "djvu"
                              "epub"
                              "azw3"
                              "mobi"
                              "zip")
  "Preference order for book formats")

(defmacro concat (&rest rest)
  "Concat string"
  `(concatenate 'string ,@rest))

(defun replace-str (string replacements)
  (if replacements
      (let ((rep (car replacements)))
        (replace-str
         (cl-strings:replace-all string (first rep) (second rep))
         (cdr replacements)))
      string))

(defun get-sql-stmt (query)
  "Return sql statement for the given query"
  (concat
   "SELECT title, author_sort, path FROM books "
   "WHERE lower(title || ' ' || author_sort) like '%' || lower('" query "') || '%'"))

(defun fix-path-string (path-str)
  (replace-str path-str '((" " "\\ ")
                          ("(" "\\(")
                          (")" "\\)")
                          ("_" "\\_")
                          ("'" "\\'"))))

(defun get-calibre-path (item-path)
  (fix-path-string (concat *calibre-root* item-path)))

(defun search-in-calibre (query)
  (let* ((calibre-db (get-calibre-path "metadata.db"))
         (command (concat "sqlite3 " calibre-db " \"" (get-sql-stmt query) "\""))
         (command-output (cl-strings:clean (run-shell-command command t) :char #\Linefeed)))
    (mapcar (lambda (x) (cl-strings:split x "|")) (cl-strings:split command-output #\Linefeed))))

(defun search-menu-table (query)
  (mapcar (lambda (res) (list
                    (concat (first res) " - " (second res))
                    (third res))) (search-in-calibre query)))

(defun format-available (book-path format)
  (let ((ls-out (run-shell-command (concat "ls " book-path "/*." format) t)))
    (if (string-equal "" ls-out) NIL (cl-strings:clean ls-out :char #\Linefeed))))

(defun open-preferred-format (book-path &optional format-list)
  (if format-list
      (let ((availability (format-available book-path (car format-list))))
        (if availability
            (run-shell-command (concat "xdg-open " (fix-path-string availability)))
            (open-preferred-format book-path (cdr format-list))))
      (message "No suitable book format found.")))

(defun xdg-open-book (book-item)
  (let ((book-path (get-calibre-path (second book-item))))
    (open-preferred-format book-path *format-preference*)))

(defcommand open-book (search-term) ((:string "Enter search term: "))
  (let* ((result-table (search-menu-table search-term))
         (hits (length result-table)))
    (cond
      ((null (second (first result-table))) (message "No books found"))
      ((= 1 hits) (xdg-open-book (car result-table)))
      (t (xdg-open-book (select-from-menu (current-screen) result-table "Filter book:"))))))
