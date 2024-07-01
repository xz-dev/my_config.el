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
;; - `doom-symbol-font' -- for symbols
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


(use-package! telega
  :init
  (setq telega-use-images t)
  :config
  (setq telega-use-docker "podman"
        telega-docker-run-arguments "--userns=keep-id")
  (setq telega-server-libs-prefix "/usr/")
  (setq telega-animation-play-inline t)
  (setq telega-emoji-use-images t)

  (setq telega-notifications-mode 1)
  (setq telega-autoplay-mode 1)
  (setq telega-accounts (list
                         (list "inkflaw" 'telega-database-dir telega-database-dir)
                         (list "inkflawing" 'telega-database-dir (expand-file-name "inkflawing" telega-database-dir)))))

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
              ("C-e" . 'copilot-accept-completion)
              ("A-." . 'copilot-next-completion)
              ("A-," . 'copilot-previous-completion)))
;; prevent overlay conflict (company +childframe) at init.el

;; treemacs
(use-package! treemacs
  :init
  (treemacs-tag-follow-mode)
  :config
  (setq treemacs-read-string-input 'from-minibuffer
        treemacs-width-is-initially-locked nil))

;; Syntax Highlighting
;;(global-tree-sitter-mode)
;;(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; code format
(defun format-buffer-shot ()
  (interactive)
  (format-all-buffer)
  (when (derived-mode-p 'python-mode)
    (py-isort-buffer)))

(global-set-key (kbd "C-l") 'format-buffer-shot)

;; cucumber
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
(setq feature-step-search-path "features/steps/**")

;; ebuild-run-mode
(eval-after-load 'ebuild-mode `(setq ebuild-log-buffer-mode 'ebuild-run-mode))

;; Dirvish over Dired globally
(use-package! dirvish
  :init
  (dirvish-override-dired-mode))

;; ebuild-mode
(use-package! site-gentoo
  :if (locate-dominating-file user-emacs-directory "site-gentoo.el")
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp")

;; 结巴分词
(use-package! cns
  :commands (cns-mode global-cns-mode cns-auto-enable)
  :config
  (defvar cns-packages-path (expand-file-name "cns" (expand-file-name straight-build-dir (expand-file-name "straight" straight-base-dir))))
  (setq cns-prog (if (file-executable-p (expand-file-name "cnws" cns-packages-path))
                     (expand-file-name "cnws" cns-packages-path)
                   (expand-file-name "cnws.exe" cns-packages-path))
        cns-dict-directory (expand-file-name "dict" cns-packages-path))
  (cond
   ((eq system-type 'windows-nt)
    (use-package! site-gentoo
      :load-path "/usr/share/emacs/site-lisp")
    (setq cns-cmdproxy-shell-path "wsl.exe bash")))
  :hook
  (find-file . cns-auto-enable))

;; high contrast theme
(use-package! haki-theme
  :demand t
  ;;:custom-face
  ;;(haki-region ((t (:background "#2e8b57" :foreground "#ffffff"))))
  ;;(haki-highlight ((t (:background "#fafad2" :foreground "#000000"))))
  :config
  ;;(setq
  ;; ;; If you skip setting this, it will use 'default' font.
  ;; haki-heading-font "Comic Mono"
  ;; haki-sans-font "Iosevka Comfy Motion"
  ;; haki-title-font "Impress BT"
  ;; haki-link-font "VictorMono Nerd Font" ;; or Maple Mono looks good
  ;; haki-code-font "Maple Mono") ;; inline code/verbatim (org,markdown..)

  ;; For meow/evil users (change border of mode-line according to modal states)
  (add-hook 'post-command-hook #'haki-modal-mode-line)

  (load-theme 'haki t))

;; gptel
(use-package! gptel
  :config
  ;; OpenRouter offers an OpenAI compatible API
  (setq!
   gptel-backend
   (gptel-make-openai "OpenRouter"
     :host "openrouter.ai"
     :endpoint "/api/v1/chat/completions"
     :stream t
     :key #'gptel-api-key
     :models '("google/gemma-2-9b-it:free"
               "anthropic/claude-3.5-sonnet"
               "anthropic/claude-3.5-sonnet:beta"
               "anthropic/claude-3-haiku"
               "meta-llama/llama-3-70b-instruct"
               "gryphe/mythomax-l2-13b"
               "openai/gpt-4o"
               "google/gemini-pro-1.5"
               "microsoft/wizardlm-2-8x22b"))))
