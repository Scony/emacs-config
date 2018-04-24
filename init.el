(cl-assert (>= emacs-major-version 24))

;; package installation from MELPA

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar dynamic-packages
  '(
    cmake-mode
    ))

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      dynamic-packages)


;; hardcoded packages loading

(add-to-list 'load-path (expand-file-name
                         "static/"
                         (file-name-directory load-file-name)))

(defvar static-packages
  '(
    google-c-style
    better-defaults
    loccur
    fill-column-indicator
    ))

(mapc #'(lambda (package)
	  (require package))
      static-packages)

(add-hook 'c-mode-common-hook 'google-set-c-style)
