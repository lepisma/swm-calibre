;;;; swm-calibre.lisp

(in-package #:swm-calibre)

(export '(*calibre-root* open-book))

(defvar *calibre-root* nil
  "Path to calibre root directory.")

(defvar *format-preference* '("pdf" "cbr" "cbz" "djvu" "epub" "azw3" "mobi")
  "Preference order for book formats.")

(defun get-sql-stmt (query)
  "Return sql statement for the given query."
  (cl-strings:join
   (list "SELECT title, author_sort, path FROM books "
	 "WHERE lower(title || ' ' || author_sort) like '%' || lower('" query "') || '%'")))

(defun get-calibre-path (item-path)
  "Get item path with respect to calibre root directory."
  (merge-pathnames item-path (pathname-as-directory *calibre-root*)))

(defun search-in-calibre (query)
  "Search for items in calibre library."
  (let* ((calibre-db (get-calibre-path "metadata.db"))
         (search-result (inferior-shell:run/ss (list 'sqlite3 calibre-db (get-sql-stmt query)))))
    (mapcar (lambda (x) (cl-strings:split x "|")) (cl-strings:split search-result #\Linefeed))))

(defun get-menu-table (query)
  "Get entries for menu table for stumpwm."
  (mapcar (lambda (res) (list
                    (cl-strings:join (list (first res) " - " (second res)))
                    (third res))) (search-in-calibre query)))

(defun get-book-format (book-path format)
  "Return file path for given format. NIL if not found."
  (let ((files-found (directory (make-pathname :name :wild
                                               :type format
                                               :defaults book-path))))
    (if files-found (first files-found) nil)))

(defun open-preferred-format (book-path &optional format-list)
  "Loop over format-list to open the book."
  (if format-list
      (let ((file-path (get-book-format book-path (car format-list))))
        (if file-path
            (inferior-shell:run/ss (list 'xdg-open file-path))
            (open-preferred-format book-path (cdr format-list))))
      (message "No suitable book format found.")))

(defun xdg-open-book (book-item)
  "Open book represented in book-item using xdg-open."
  (let ((book-path (get-calibre-path (second book-item))))
    (open-preferred-format (pathname-as-directory book-path) *format-preference*)))

(defcommand open-book (search-term) ((:string "Enter search term: "))
  "Main command exposed for opening books."
  (if *calibre-root*
      (progn
	      (let* ((menu-table (get-menu-table search-term))
               (hits (length menu-table)))
	        (cond
	          ((null (second (first menu-table))) (message "No books found"))
	          ((= 1 hits) (xdg-open-book (car menu-table)))
	          (t (xdg-open-book (select-from-menu (current-screen) menu-table "Filter book:"))))))
      (message "*calibre-root* directory not set")))
