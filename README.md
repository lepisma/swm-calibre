# swm-calibre

Quickly open calibre books in [StumpWM](https://stumpwm.github.io/).

```lisp
;; Clone the repo to StumpWM's load path (or add-to-load-path)

(load-module "swm-calibre")
(setf swm-calibre:*calibre-root* "<path-to-calibre-dir-with-trailing-'/'>")

(open-book)
```
