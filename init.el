(require 'package)

(setq package-list '(
					 yasnippet
					 yaml-mode
					 websocket
					 swiper
					 sphinx-doc
					 smex
					 seq
					 s
					 rust-mode
					 restclient
					 request
					 racer
					 pyvenv
					 python-environment
					 py-autopep8
					 popup
					 pkg-info
					 pandoc-mode
					 multiple-cursors
					 markdown-mode
					 magit
					 jedi
					 idle-highlight-mode
					 highlight-indentation
					 google-translate
					 go-mode
					 ggtags
					 graphviz-dot-mode
					 flymake-go
					 flymake-rust
					 flycheck
					 flycheck-rust
					 find-file-in-project
					 exec-path-from-shell
					 epl
					 epc
					 elpy
					 ein
					 deferred
					 dash
					 ctable
					 cl-generic
					 concurrent
					 company
					 cargo
					 auto-complete
					 async
					 ahg
					 lua-mode
					 dockerfile-mode
					 json-reformat
					 yafolding
					 idea-darkula-theme
					 ))

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(setenv "LANG" "ru_RU.UTF-8")
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq make-backup-files nil)
(delete-selection-mode 1)

(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(setq inhibit-startup-message t)

(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)

;; Docker
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Copy lime or region
(defun duplicate-line-or-region (&optional n)
      "Duplicate current line, or region if active.
    With argument N, make N copies.
    With negative N, comment out original line and use the absolute value."
      (interactive "*p")
      (let ((use-region (use-region-p)))
        (save-excursion
          (let ((text (if use-region        ;Get region if active, otherwise line
                          (buffer-substring (region-beginning) (region-end))
                        (prog1 (thing-at-point 'line)
                          (end-of-line)
                          (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                              (newline))))))
            (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
              (insert text))))
        (if use-region nil                  ;Only if we're working with a line (not a region)
          (let ((pos (- (point) (line-beginning-position)))) ;Save column
            (if (> 0 n)                             ;Comment out original with negative arg
                (comment-region (line-beginning-position) (line-end-position)))
            (forward-line 1)
            (forward-char pos)))))
(global-set-key "\C-c\C-k" 'duplicate-line-or-region)

;;Translate
(require 'google-translate)
(require 'google-translate-default-ui)
(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)
(setq google-translate-default-target-language "ru")
(setq google-translate-default-source-language "en")

;;resizing window
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
    (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
    (global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; SQL settings
(setq sql-postgres-program "/Applications/Postgres.app/Contents/Versions/9.6/bin/psql")

;; smex (ido в M-x)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;;Multiple-cursors
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; js
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; c
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(defun my-flycheck-c-setup ()
  (setq flycheck-clang-language-standard "gnu99"))

(add-hook 'c-mode-hook #'my-flycheck-c-setup)

;; go
(require 'go-mode)
(eval-after-load "go-mode"
'(require 'flymake-go))
(load-file "~/.emacs.d/go-autocomplete.el")
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)
(setq exec-path (cons "/usr/local/go/bin" exec-path))
(add-to-list 'exec-path "~/.go/bin")
(defun my-go-mode-hook ()
  ; Call Gofmt before saving
 (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
 (if (not (string-match "go" compile-command))
     (set (make-local-variable 'compile-command) 
          "go build -gcflags \"-N -l\""))
  ; Godef jump key binding
 (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

;;rust
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path "/usr/local/src/rust/src")
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common) ;
(setq company-tooltip-align-annotations t)
(setq company-idle-delay 1)
(setq company-minimum-prefix-length 1)
(eval-after-load "rust-mode" '(require 'racer))
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(require 'flymake-rust)
(add-hook 'rust-mode-hook 'flymake-rust-load)

;;DCVS
(require 'ahg)
(require 'magit)

;; XML
(load-file "~/.emacs.d/xmllint.el")
(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))



(add-hook 'nxml-mode-hook 'hs-minor-mode)
;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)


(defun comment-dwim-line (&optional arg)
    "Replacement for the comment-dwim command.
     If no region is selected and current line is not blank and we are not at the end of the line,
     then comment current line.
     Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
     (interactive "*P")
     (comment-normalize-vars)
     (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
         (comment-or-uncomment-region (line-beginning-position) (line-end-position))
	 (comment-dwim arg)
     )
)
(global-set-key (kbd "s-/") 'comment-dwim-line)

(defun move-text-internal (arg)
  (cond (
	 (and mark-active transient-mark-mode)
         (if (> (point) (mark))
	     (exchange-point-and-mark))
         (let (
	       (column (current-column))
	       (text (delete-and-extract-region (point) (mark)))
	      )
	      (forward-line arg)
	      (move-to-column column t)
	      (set-mark (point))
	      (insert text)
	      (exchange-point-and-mark)
	      (setq deactivate-mark nil))
	 )
	(t
	 (beginning-of-line)
         (when (or (> arg 0) (not (bobp)))
	   (forward-line)
	   (when (or (< arg 0) (not (eobp)))
	     (transpose-lines arg))
	   (forward-line -1))
	 )
  )
  )
(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)

;; Navigation
(require 'ido)
(ido-mode t)

(require 'yasnippet)
(setq yas-prompt-functions '(yas-x-prompt yas-dropdown-prompt))
(yas-global-mode 1)

;;Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(gdb-many-windows t)
 '(markdown-command "/usr/local/bin/pandoc")
 '(org-export-backends (quote (ascii html icalendar latex md)))
 '(package-selected-packages
   (quote
	(yaml-mode yafolding swiper sphinx-doc smex restclient py-autopep8 pandoc-mode multiple-cursors markdown-mode magit lua-mode json-reformat jedi idle-highlight-mode graphviz-dot-mode google-translate go-mode ggtags flymake-go exec-path-from-shell elpy ein dockerfile-mode cyberpunk-theme ahg)))
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(setq tramp-default-method t)
 '(show-paren-mode t)
 '(tab-width 4))

;; Python
(require 'ein)
(add-hook 'python-mode-hook (lambda ()
                                  (require 'sphinx-doc)
                                  (sphinx-doc-mode t)
								  (idle-highlight-mode t)
								  ))
(elpy-enable)
(elpy-use-ipython)

(setq elpy-rpc-backend "rope")

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Line number
(global-linum-mode t)
(electric-pair-mode 1)
(column-number-mode t)

;; Themes
(global-hl-line-mode t)
(set-default-font "Hack-12")
(load-theme 'idea-darkula t)

;;spell checker
(setq ispell-program-name "aspell")
;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=ru"))
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
