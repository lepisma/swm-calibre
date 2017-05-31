;;;; swm-calibre.asd

(asdf:defsystem #:swm-calibre
  :description "Search and open calibre books in StumpWM"
  :author "Abhinav Tushar <abhinav.tushar.vs@gmail.com>"
  :license "GPLv3"
  :depends-on (#:stumpwm :cl-strings)
  :serial t
  :components ((:file "package")
               (:file "swm-calibre")))

