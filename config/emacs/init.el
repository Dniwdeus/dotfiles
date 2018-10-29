;; setup package manager for emacs
(require 'package)
(setq package-enable-at-startup t)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; bootstrap use package [fn:11]


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; for now keep usepackage verbose.
(setq use-package-verbose t
      use-package-expand-minimally nil
      use-package-compute-statistics t)

;; configure magit

(use-package magit
  :commands (magit-status magit-blame magit-mode)
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c C-g l" . magit-file-log)
         ("C-c C-g c" . magit-commit)
         ("C-c C-g f" . magit-grep))
  :config
  (progn
    ;; Set `magit-status' fullscreen
    (setq magit-post-display-buffer-hook
          #'(lambda ()
              (when (derived-mode-p 'magit-status-mode)
                (delete-other-windows))))

    (setenv "GIT_PAGER" "")
    (add-hook 'magit-log-edit-mode-hook
              '(lambda ()
                 (auto-fill-mode)
                 (flyspell-mode)
                 (set-fill-column 80))))
  (use-package magit-blame
    :bind ("C-c C-g b" . magit-blame-mode))
 )


(setq-default indent-tabs-mode nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (deeper-blue)))
 '(package-selected-packages (quote (magit use-package yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq backup-directory-alist `((".*" . ,temporary-file-directory))) (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(delete-selection-mode 1)
