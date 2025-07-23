;; Load third party modules  -*- lexical-binding: t; -*-
(let ((third-dir (expand-file-name "third" user-emacs-directory)))
  (when (file-directory-p third-dir)
    ;; Add `third/` itself to load-path (for any single-file packages in `third/`)
    (add-to-list 'load-path third-dir)
    ;; Add all subdirectories of `third/` to load-path
    (let ((default-directory third-dir))
      (normal-top-level-add-subdirs-to-load-path))))

(with-eval-after-load 'emacs
  (keymap-global-set "C-x C-m" 'execute-extended-command)
  (setq recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:" "^/var/folders/.*")))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'visual-line-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)
     (js . t)))
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (member lang '("plantuml" "js"))))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate))

(with-eval-after-load 'flymake
  (defun my/restart-flymake ()
    "Restart Flymake mode (turn off and back on)."
    (interactive)
    (flymake-mode -1)
    (flymake-mode 1))

  (defun my/flymake-setup-key ()
    (local-set-key (kbd "C-c t") #'my/restart-flymake))

  (add-hook 'flymake-mode-hook #'my/flymake-setup-key))

(require 'atomic-chrome)
(setq atomic-chrome-debug t)
(atomic-chrome-start-server)

(provide 'private)
