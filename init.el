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
    markdown-mode
    blacken
    bm
    highlight-symbol
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
    godot-gdscript
    gdformat
    ))

(mapc #'(lambda (package)
	  (require package))
      static-packages)


;; configuration

(add-hook 'c-mode-common-hook
          (lambda ()
            'google-set-c-style
            (define-key c-mode-base-map (kbd "C-c f") 'clang-format-buffer)
            ))

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "C-c f") 'blacken-buffer)
            ))

(add-hook 'elpy-mode-hook
	  (lambda ()
	    (define-key elpy-mode-map (kbd "<M-left>") nil)
	    (define-key elpy-mode-map (kbd "<M-right>") nil)
	    (define-key elpy-mode-map (kbd "<C-left>") 'elpy-nav-indent-shift-left)
	    (define-key elpy-mode-map (kbd "<C-right>") 'elpy-nav-indent-shift-right)
	    ))

(add-hook 'godot-gdscript-mode-hook
          (lambda ()
            (define-key godot-gdscript-mode-map (kbd "C-c f") 'gdformat-buffer)
            ))

(textmate-mode)
(global-set-key (kbd "C-f") 'textmate-goto-file)

(let ((my-tags-file (locate-dominating-file default-directory "TAGS")))
  (when my-tags-file
    (message "Loading tags file: %s" my-tags-file)
    (visit-tags-table my-tags-file)))

(global-set-key (kbd "M-f") 'xref-find-apropos)
;; (global-set-key (kbd "C-c f") 'clang-format-buffer)
(setq clang-format-style-option "file")

(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c b") 'magit-blame-addition)
(global-set-key (kbd "C-c l") 'magit-log-buffer-file)
(global-set-key (kbd "C-c h") 'highlight-symbol)

(setq dired-listing-switches "-alh")

;; disable backup (*~) files
(setq make-backup-files nil)

(add-to-list 'auto-mode-alist '("\\.log\\'" . text-mode))
(add-hook 'text-mode-hook
          (lambda ()
            (define-key text-mode-map (kbd "C-c t") 'bm-toggle)
            (define-key text-mode-map (kbd "C-c n") 'bm-next)
            (define-key text-mode-map (kbd "C-c p") 'bm-previous)
            (define-key text-mode-map (kbd "C-c C-c") 'highlight-symbol-next)
            (set-default 'truncate-lines t)
            (hl-line-mode 1)
            ))

(add-to-list 'auto-mode-alist '("\\.shader\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.gdshader\\'" . c++-mode))
