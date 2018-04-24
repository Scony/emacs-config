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
    elpy
    sr-speedbar
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


;; configuration

(add-hook 'c-mode-common-hook 'google-set-c-style)

(add-hook 'elpy-mode-hook
	  (lambda ()
	    (define-key elpy-mode-map (kbd "<M-left>") nil)
	    (define-key elpy-mode-map (kbd "<M-right>") nil)
	    (define-key elpy-mode-map (kbd "<C-left>") 'elpy-nav-indent-shift-left)
	    (define-key elpy-mode-map (kbd "<C-right>") 'elpy-nav-indent-shift-right)
	    ))
