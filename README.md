# swm-calibre

Quickly open calibre books in [StumpWM](https://stumpwm.github.io/).

```lisp
;; Clone the repo to StumpWM's load path (or add-to-load-path)

(load-module "swm-calibre")
(setf swm-calibre:*calibre-root* #p"<path-to-calibre-dir>")

;; Call command
;; open-book "search-term"
```
