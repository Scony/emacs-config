(add-to-list 'load-path (expand-file-name
                         "static/"
                         (file-name-directory load-file-name)))

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(setq dummy-string "it works!")
