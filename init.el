(require 'cl-lib)
(cl-assert (>= emacs-major-version 24))

;; package installation from MELPA

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;; (package-refresh-contents)           ; in case of "not found" problems

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar dynamic-packages
  '(
    cmake-mode
    elpy
    sr-speedbar
    textmate
    yaml-mode
    clang-format
    magit
    haskell-mode
    go-mode
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
    ttcn3
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

(textmate-mode)
(global-set-key (kbd "C-f") 'textmate-goto-file)

(let ((my-tags-file (locate-dominating-file default-directory "TAGS")))
  (when my-tags-file
    (message "Loading tags file: %s" my-tags-file)
    (visit-tags-table my-tags-file)))

(global-set-key (kbd "M-f") 'xref-find-apropos)
(global-set-key (kbd "C-c f") 'clang-format-buffer)
(setq clang-format-style-option "file")

(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c b") 'magit-blame)
(global-set-key (kbd "C-c l") 'magit-log-buffer-file)

(setq dired-listing-switches "-alh")
