;; Load third party modules  -*- lexical-binding: t; -*-
(let ((third-dir (expand-file-name "third" user-emacs-directory)))
  (when (file-directory-p third-dir)
    ;; Add `third/` itself to load-path (for any single-file packages in `third/`)
    (add-to-list 'load-path third-dir)
    ;; Add all subdirectories of `third/` to load-path
    (let ((default-directory third-dir))
      (normal-top-level-add-subdirs-to-load-path))))

(with-eval-after-load 'emacs
  ;; (pixel-scroll-precision-mode nil)
  ;; (pixel-scroll-precision-use-momentum t)
  (keymap-global-set "C-x C-m" 'execute-extended-command)
  (keymap-global-set "C-w" 'backward-kill-word)
  (setq recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:" "^/var/folders/.*"))
  ;; Assign Apple Color Emoji for the general emoji range
  ;; Covers most pictographs, symbols, flags, etc.
  (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil 'append)
  ;; Rescale emoji font so it matches JetBrainsMono line height
  (add-to-list 'face-font-rescale-alist '("Noto Color Emoji" . 0.8)))

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

(when (eq system-type 'darwin)
    (setq mac-command-modifier 'super))

(setq world-clock-list
      '(("Australia/Canberra" "Canberra")
        ("Asia/Singapore"    "Singapore")
        ("Asia/Kolkata"      "Hyderabad")))

(require 'edit-server)
(edit-server-start)

(provide 'private)
