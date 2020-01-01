;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;  YTZE EMACS CONFIGURATION  ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;  DECEMBER 2019  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                     ;;
;;                               !         !                           ;;
;;                              ! !       ! !                          ;;
;;                             ! . !     ! . !                         ;;
;;                               ^^^^^^^^^ ^                           ;;
;;                             ^             ^                         ;;
;;                           ^  (0)       (0)  ^                       ;;
;;                          ^        ""         ^                      ;;
;;                         ^   ***************    ^                    ;;
;;                       ^   *                 *   ^                   ;;
;;                      ^   *   /\   /\   /\    *    ^                 ;;
;;                     ^   *                     *    ^                ;;
;;                    ^   *   /\   /\   /\   /\   *    ^               ;; 
;;                   ^   *                         *    ^              ;;
;;                   ^  *                           *   ^              ;; 
;;                   ^  *                           *   ^              ;;
;;                    ^ *                           *  ^               ;;
;;                     ^*                           * ^                ;;
;;                      ^ *                        * ^                 ;; 
;;                      ^  *                      *  ^                 ;;
;;                        ^  *       ) (         * ^                   ;;
;;                            ^^^^^^^^ ^^^^^^^^^                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; DEFAULT SCRIPT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (deeper-blue)))
 '(font-lock-mode -1 t)
 '(font-use-system-font t)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(indicate-buffer-boundaries (quote left))
 '(package-selected-packages
   (quote
    (vscode-icon all-the-icons use-package rainbow-delimiters js2-highlight-vars
		 emmet-mode dired-sidebar project-explorer projectile move-text company
		 tern-auto-complete seq indent-guide autopair rjsx-mode jsx-mode react-snippets
		 js-auto-beautify exec-path-from-shell json-mode web-mode flycheck powerline delight
		 company-quickhelp xref-js2 js2-refactor dummyparens company-tern ac-js2)))
 '(show-paren-mode t)
 '(visible-bell 1)) ;;disables end of buffer sounds and replace it with top screen flash
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-jsdoc-tag ((t (:foreground "yellow")))))

;; ;;(setq visible-bell 1)
;; (setq visible-bell 1)
;; (setq ring-bell-function 'ignore)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;         customized paths        ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/setup-files/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; MELPA CONFIGURATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; MELPA MANAGES EMACS ADD ONS (PACKAGES) ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; ENABLE INDENTATION GUIDES ;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://github.com/DarthFennec/highlight-indent-guides
;;
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                     ;;
;;                          F L Y C H E C K                            ;; 
;;                                                                     ;;
;; http://www.flycheck.org/manual/latest/index.html                    ;;
;; flycheck performs on the fly check for possible errors while typing ;;
;;                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable js2mode
;;(require 'js2mode)

(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customizing flycheck to use local node_modules eslint if exists ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                       RJSX MODE AUTO ENABLE                          ;;;
;;;                                                                      ;;;
;;;enable rjsx mode                                                      ;;:

(require 'rjsx-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
;;;
;;;(add-to-list 'auto-mode-alist '("components\/.*\.js\'" . rjsx-mode))
;;;(add-to-list 'auto-mode-alist '("containers\/.*\.js\'" . rjsx-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;     BACKGROUND SETTINGS    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (display-graphic-p)
    (setq initial-frame-alist
          '(
            (tool-bar-lines . 0)
            (width . 106)
            (height . 60)
            (background-color . "#050a20")
            (left . 50)
            (top . 50)))
  (setq initial-frame-alist '( (tool-bar-lines . 0))))

(setq default-frame-alist initial-frame-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                     
;; CUSTOMIZE THE CURSOR                      
;; available cursor values "box" "hollow" "bar" "hbar"
;;
;; (setq-default cursor-type 'bar)                    
;; (set-cursor-color "#ffffff")
;; 
;;

(setq-default cursor-type 'box)                    
(set-cursor-color "#49ff33") 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable column number mode (visualize column number)
(column-number-mode)

;; enable parents pair
(show-paren-mode)

;; enable autopair for parens
(electric-pair-mode)

;;highlight current line 
(global-hl-line-mode t)
;;highlight current line
(set-face-background 'hl-line "#000000")

;;autocompletion in emacs command line (for example when searching files to open)
(ido-mode t)

;;eventually undo change layouts but dunno how it works
(winner-mode t)

;;jump between buffers (standard editor tab style)
;;default keybindings are:[alt]+[up/down/right/left]  
;;https://www.emacswiki.org/emacs/WindMove
(windmove-default-keybindings 'meta)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;                 T E R N                  ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'company)
(require 'company-tern)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))


;; Enable highlight var on hoover minor mode 
(eval-after-load "js2-highlight-vars-autoloads"
  '(add-hook 'js2-mode-hook (lambda () (js2-highlight-vars-mode))))

;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;                P R O J E C T I L E                ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;     PROJECT  TOOL    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;         D I R E D - S I D E B A R                 ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;     SIDEBAR TOOL    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This package provides a tree browser similar to `neotree' or `treemacs'
;; but leverages `dired' to do the job of display.
;; to toggle sidebar C-x C-n

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))


;; omits hidden files from the list
;; toggle with C-x M-o
;; https://www.emacswiki.org/emacs/DiredOmitMode

;; Load Dired X when Dired is loaded.
  (add-hook 'dired-load-hook '(lambda () (require 'dired-x)))
;; Turn on Omit mode.
  (setq dired-omit-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;       TRANSPOSE LINES CUSTOM COMMAND              ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(meta shift up)]  'move-line-up)
(global-set-key [(meta shift down)]  'move-line-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;       TRUNC LONG LINES VISUALIZATION              ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-default 'truncate-lines t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;            RAINBOW BRACKETS MODE                  ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;To start the mode automatically in `foo-mode', add the following to your init file:
  (add-hook 'foo-mode-hook #'rainbow-delimiters-mode)

;;To start the mode automatically in most programming modes (Emacs 24 and above):
  (add-hook 'Prog-mode-hook #'rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;            Multiple Cursors Mode                  ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(Global-Set-Key (Kbd "C-C M C") 'Mc/Edit-Lines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;            web-mode on .html docs                 ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;  E N D  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


