(add-to-list 'load-path "./static/")

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(setq dummy-string "it works!")
