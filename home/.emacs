(server-start)
(require 'cl)

;; basics
(setq inhibit-splash-screen t)
(set-face-font 'default "DejaVu Sans Mono-10")
(global-hl-line-mode)
(global-linum-mode 1)
(setq delete-by-moving-to-trash t) ; use the recycle bin

;; put backup files in a temp folder
(setq backup-directory-alist
`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
`((".*" ,temporary-file-directory t)))

;; use bash as the default shell
(setq explicit-shell-file-name "d:/opt/msys64/usr/bin/bash.exe")
(setq shell-file-name "bash")
(setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
(setenv "SHELL" shell-file-name)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;; load path
(add-to-list 'load-path "~/.emacs.d/settings")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d/el-get")
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/elpa")

;; packages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize))

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/") 
                         ("org" . "http://orgmode.org/elpa/")))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(require 'el-get)
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq el-get-github-default-url-type "https")
(require 'el-get-elpa)
;; Build the El-Get copy of the package.el packages if we have not
;; built it before. Will have to look in to updating later ...
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))
(el-get 'sync)

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

; set local recipes, el-get-sources should only accept PLIST element
(setq
 el-get-sources
 '((:name buffer-move			; have to add your own keys
      :after (progn
           (global-set-key (kbd "<C-S-up>")     'buf-move-up)
           (global-set-key (kbd "<C-S-down>")   'buf-move-down)
           (global-set-key (kbd "<C-S-left>")   'buf-move-left)
           (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

   (:name smex				; a better (ido like) M-x
      :after (progn
           (setq smex-save-file "~/.emacs.d/.smex-items")
           (global-set-key (kbd "M-x") 'smex)
           (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

   (:name magit				; git meet emacs, and a binding
      :after (progn
           (global-set-key (kbd "C-x C-z") 'magit-status)))

   (:name goto-last-change		; move pointer back to last change
      :after (progn
           ;; when using AZERTY keyboard, consider C-x C-_
           (global-set-key (kbd "C-x C-/") 'goto-last-change)))))

;;; now set our own packages
(setq my:el-get-packages
      '(el-get			; el-get is self-hosting
        pydoc-info      ;
	ein             ; ipython support
        popup           ;
        magit           ; git for emacs
        escreen        	; screen for emacs, C-\ C-
        switch-window	; takes over C-x o   
        jedi            ; python completion
        yasnippet		; powerful snippet mode
        websocket       ; emacs sockets
        request         ;
        auto-complete))		; complete as you type with overlays

;;; Some recipes require extra tools to be installed
;;; Note: el-get-install requires git, so we know we have at least that.
(when (el-get-executable-find "svn")
  (loop for p in '(psvn    	; M-x svn-status
           )
    do (add-to-list 'my:el-get-packages p)))

(setq my:el-get-packages
      (append my:el-get-packages
              (mapcar #'el-get-source-name el-get-sources)))

;;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)

;; avoid compiz manager rendering bugs
(add-to-list 'default-frame-alist '(alpha . 100))

;; cut/copy/paste with C-x/C-c/C-v, check out C-RET too
;(cua-mode)

;; Use the clipboard so that copy/paste works
;(setq x-select-enable-clipboard t)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

; winner-mode provides C-<left> to get back to previous window layout
(winner-mode 1)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; M-x shell is a nice shell interface to use, let's make it colorful.  If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; use ido for minibuffer completion
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)
(setq ido-default-buffer-method 'selected-window)

;; default key to switch buffer is C-x b, but that's not easy enough
;;
;; when you do that, to kill emacs either close its frame from the window
;; manager or do M-x kill-emacs.  Don't need a nice shortcut for a once a
;; week (or day) action.
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-c") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)

;; have vertical ido completion lists
(setq ido-decorations
      '("\n-> " "" "\n   " "\n   ..." "[" "]"
	" [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-set-key [f11] 'fullscreen)
(fullscreen) ; switch to full-screen mode during startup
    
(progn
  (require 'elisp-slime-nav)
  (defun my-lisp-hook ()
    (elisp-slime-nav-mode)
    (turn-on-eldoc-mode)
    )
  (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
)
(setq inferior-lisp-program "D:/usr/local/sbcl/1.2.11/sbcl")
(setq slime-contribs '(slime-fancy))

(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")

(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

(require 'goto-chg)

(require 'winring)
(winring-initialize)

(require 'evil)
(evilnc-default-hotkeys)
(evil-mode 1)
(require 'powerline)
(powerline-evil-center-color-theme)

(global-evil-leader-mode)
(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer)

(load "~/.emacs.d/repos/nxhtml/autostart.el")

(setq py-install-directory "~/.emacs.d/repos/python-mode/")
(add-to-list 'load-path py-install-directory)
(require 'python-mode)
(require 'ipython)
(require 'ein)

(when (executable-find "ipython")
 (setq python-shell-interpreter "ipython"))

;; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
;; use the wx backend, for both mayavi and matplotlib
;(setq py-python-command-args
  ;'("--gui=tcl" "--pylab=tcl" "-colors"))
;(setq py-force-py-shell-name-p t)

;; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
;; don't split windows
;(setq py-split-windows-on-execute-p nil)
;; try to automagically figure out indentation
(setq py-smart-indentation t)
