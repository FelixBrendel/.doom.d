;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name    "Felix Brendel"
      user-mail-address "felixbrendel@airmail.cc")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)
;; (setq doom-theme 'material)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(defun duplicate-line()
  (interactive)
  (let ((inhibit-message 1))
  (save-excursion
    (move-beginning-of-line 1)
    (set-mark (point-marker))
    (move-end-of-line 1)
    (copy-region-as-kill (region-beginning) (region-end))
    (deactivate-mark)
    (insert "\n")
    (yank)
    (pop kill-ring))
  (call-interactively 'next-line)))

(cond ((string= system-type "windows-nt")
       ;; if windows
       (setq build-script-name "build.bat"))
      (t ;; if linux
       (setq build-script-name "build.sh")))


(defun save-and-find-build-script-and-compile ()
  "Walks upward the directory tree until a buildscript is found"
  (interactive)

  (when (and buffer-file-name (buffer-modified-p)) (save-buffer))
  (let ((build-script-path (locate-dominating-file (expand-file-name default-directory) build-script-name)))
    (unless build-script-path
      (error (concat "The default buildscript '" build-script-name "' cannot be found")))
    ;; NOTE(Felix): because we will set compilation to comint mode,
    ;; after the compilation process finishes, we will set it back to
    ;; normal compilation mode, so that we can press g for recompile or
    ;; q for closing the window. As opposoed to typing in these chars as
    ;; we would if we would not switch off the comint mode.
    (setq compilation-finish-functions
          (list (lambda (&rest _) (compilation-minor-mode 1))))
    (compile (concat build-script-path build-script-name) t)))

(defun lookup-docs-for-symbol-at-point ()
  (interactive)
  (+lookup/online (thing-at-point 'symbol) "DevDocs.io"))

(defun mark-word-or-next-word-like-this ()
  "if there is no active region the word under
   the point will be marked, otherwise the next word is selected."
  (interactive)
  (if (region-active-p)
      ;; then
      (progn
        (mc/mark-next-like-this 1)
        (mc/maybe-multiple-cursors-mode))
    ;; else
    (mc--mark-symbol-at-point)))

(map! "C-s"         'swiper
      "C-j"         'join-line
      "C-d"         'mark-word-or-next-word-like-this
      "C-S-d"       'duplicate-line
      "C-S-c C-S-c" 'mc/edit-lines
      "C-c e"       'save-and-find-build-script-and-compile
      "M-d"         'lookup-docs-for-symbol-at-point
      "C-#"         'comment-line)

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(use-package! multiple-cursors)
(use-package! ripgrep)

(use-package! ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist
      '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))

(use-package! company-posframe
  :config
  (company-posframe-mode 1))

(use-package! ox-twbs)

(setq doom-font
      (font-spec :family "Noto Sans Mono" :foundry "outline" :height 113))
(put 'projectile-ripgrep 'disabled nil)

(hl-line-mode -1)
