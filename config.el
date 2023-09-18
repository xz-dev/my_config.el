;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "xz"
      user-mail-address "xiangzhedev@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (font-spec :family "Iosevka Nerd Font Mono"))
(setq doom-unicode-font (font-spec :family "Iosevka Nerd Font Mono"))
;; “中”、“言”字测试
;; 连体字测试
;; "!=" "==" "++" "--" "&&" "||" "/*" "*/" "=>" "<-" "<=" ">=" "<<" ">>" "|||" "::" "===" "=/=" "=!=" "&&&" "+++" "-->" "<<<"
(defun my-cjk-font()
  (dolist (charset '(kana han cjk-misc symbol bopomofo))
    (set-fontset-font t charset (font-spec :family "LXGW WenKai Mono"))))
(add-hook 'after-setting-font-hook #'my-cjk-font)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;;(use-package! telega
;;  :init
;;  (setq telega-use-images t)
;;  :config
;;  (setq telega-use-docker "podman"
;;        telega-docker-run-arguments "--userns=keep-id")
;;  (setq telega-server-libs-prefix "/usr/")
;;  (setq telega-animation-play-inline t)
;;  (setq telega-emoji-use-images t)
;;
;;  (setq telega-notifications-mode 1)
;;  (setq telega-autoplay-mode 1))

(cl-defun pyvenv-autoload ()
  "auto activate venv directory if exists"
  (f-traverse-upwards (lambda (path)
                        (let ((venv-path (f-expand ".venv" path)))
                          (when (f-exists? venv-path)
                            (pyvenv-activate venv-path)
                            (cl-return-from pyvenv-autoload))))))

(add-hook 'python-mode-hook 'pyvenv-autoload)

;; protobuf-mode
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; flutter
;; Assuming usage with dart-mode
(use-package! dart-mode
  ;; Optional
  :hook (dart-mode . flutter-test-mode))

(use-package! flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/apt/flutter/"))

;; python3 debugger
;; TODO: 这里有 bug 要改
(use-package! dap-python)
(after! dap-mode
  (setq dap-python-debugger 'debugpy))

;; GitHub Copilot
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion-by-word)
              ("TAB" . 'copilot-accept-completion-by-word)
              ("C-e" . 'copilot-accept-completion)))
;; prevent overlay conflict (company +childframe) at init.el

;; treemacs
(use-package! treemacs
  :init
  (treemacs-tag-follow-mode)
  :config
  (setq treemacs-read-string-input 'from-minibuffer)
  (setq treemacs-width-is-initially-locked nil))

;; Syntax Highlighting
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; code format
(defun format-buffer-shot ()
  (interactive)
  (format-all-buffer "")
  (when (derived-mode-p 'python-mode)
    (py-isort-buffer)))

(global-set-key (kbd "C-l") 'format-buffer-shot)

;; cucumber
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
(setq feature-step-search-path "features/steps/**")

;; ebuild-mode
;;(autoload 'ebuild-mode "ebuild-mode"
;;	    "Major mode for Portage .ebuild and .eclass files." t)
;;(autoload 'ebuild-repo-mode "ebuild-mode"
;;	    "Minor mode for files in an ebuild repository." t)
;;(autoload 'ebuild-repo-mode-maybe-enable "ebuild-mode")
;;(autoload 'devbook-mode "devbook-mode"
;;	    "Major mode for editing the Gentoo Devmanual." t)
;;(autoload 'gentoo-newsitem-mode "gentoo-newsitem-mode"
;;	    "Major mode for Gentoo GLEP 42 news items." t)
;;(autoload 'glep-mode "glep-mode"
;;	    "Major mode for Gentoo Linux Enhancement Proposals." t)
;;(add-to-list 'auto-mode-alist '("\\.\\(ebuild\\|eclass\\)\\'" . ebuild-mode))
;;(add-to-list 'auto-mode-alist '("*.ebuild" . ebuild-mode))
;;(add-to-list 'auto-mode-alist '("/devmanual.*\\.xml\\'" . devbook-mode))
;;(add-to-list 'auto-mode-alist
;;	            '("/[0-9]\\{4\\}-[01][0-9]-[0-3][0-9]-.+\\.[a-z]\\{2\\}\\.txt\\'"
;;		                     . gentoo-newsitem-mode))
;;(add-to-list 'auto-mode-alist '("/glep.*\\.rst\\'" . glep-mode))
;;(add-to-list 'auto-mode-alist
;;	            '("/\\(package\\.\\(mask\\|unmask\\|use\\|env\
;;					 \\|license\\|properties\\|accept_\\(keywords\\|restrict\\)\\)\
;;		    \\|\\(package\\.\\)?use.\\(stable\\.\\)?\\(force\\|mask\\)\\)\\'"
;;		                     . conf-space-mode))
;;(add-to-list 'auto-mode-alist
;;	            '("/make\\.\\(conf\\|}defaults\\)\\'" . conf-unix-mode))
;;(add-to-list 'interpreter-mode-alist '("openrc-run" . sh-mode))
;;(add-to-list 'interpreter-mode-alist '("runscript" . sh-mode))
;;(add-hook 'find-file-hook #'ebuild-repo-mode-maybe-enable)
;;(modify-coding-system-alist 'file "\\.\\(ebuild\\|eclass\\)\\'" 'utf-8)

;; Dirvish over Dired globally
;;(dirvish-override-dired-mode)

;; https://emacs-china.org/t/emacs-emacs-gc/24757
(require 'emacs-gc-stats)
;; Optionally reset Emacs GC settings to default values (recommended)
(setq emacs-gc-stats-gc-defaults 'emacs-defaults)
;; Optionally set reminder to upload the stats after 3 weeks.
(setq emacs-gc-stats-remind t) ; can also be a number of days
;; Optionally disable logging the command names
;; (setq emacs-gc-stats-inhibit-command-name-logging t)
(emacs-gc-stats-mode +1)
