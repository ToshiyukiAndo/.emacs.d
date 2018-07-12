

;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ---------------------必須設定-----------------------------------
;; @ load-path


;; load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))


;; load-pathに追加するフォルダ
;; 2つ以上フォルダを指定する場合の引数 => (add-to-load-path "elisp" "xxx" "xxx")
(add-to-load-path "elisp")

;;カスタムロードパスの設定フォルダ（現状はテーマが入ってるだけ）
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/elisp/themes/"))

;;melpaの設定
(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; elpaの設定
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;;-----------------------好みの設定---------------------------------

;;対応する括弧の強調表示
(show-paren-mode t)
;;時間表時
(display-time)
(put 'upcase-region 'disabled nil)


;;スクロール一行ずつ
(setq scroll-step 1)

;; 文字コード
(prefer-coding-system 'utf-8-unix)

;;最後の行に開業を追加する
(setq require-final-newline t)

;; common lisp
(require 'cl)


;; Windowsで英数と日本語にMeiryoを指定
;; Macで英数と日本語にRictyを指定
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-face-attribute 'default nil
                             :family "Meiryo"  ;; 英数
                             :height 100)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Meiryo")))  ;; 日本語
        ((eq ws 'ns)
         (set-face-attribute 'default nil
                             :family "Ricty"  ;; 英数
                             :height 140)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty")))))  ;; 日本語

;; スタートアップ非表示
(setq inhibit-startup-screen t)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; ツールバー非表示
;;(tool-bar-mode -1)

;; メニューバーを非表示
(menu-bar-mode -1)

;; スクロールバー非表示
;;(set-scroll-bar-mode nil)

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format
            (format "%%f - Emacs@%s" (system-name)))

;; 行番号表示
(global-linum-mode t)
(set-face-attribute 'linum nil
		    :foreground "#808080"
		    :height 0.9)


;;emacsの警告音などを消す
(setq ring-bell-function 'ignore)

;;helmの設定
(require 'helm-config)
(helm-mode 1)
;;helmを使ってるときのキーバインドの設定
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-map "\C-l" (kbd "C-u 5 C-n"))
(define-key helm-map "\C-o" (kbd "C-u 5 C-p"))
(define-key helm-find-files-map "\C-l" (kbd "C-u 5 C-n"))
(define-key helm-find-files-map "\C-o" (kbd "C-u 5 C-p"))


;;helmを使うときのキーバインドの設定
(define-key global-map (kbd "C-x b")   'helm-buffers-list)
(define-key global-map (kbd "C-x f") 'helm-for-files)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)

;;カーソル位置の記憶
(save-place-mode 1)

;;キー移動を5行づつするやつ
(define-key global-map "\C-l" (kbd "C-u 5 C-n"))
(define-key global-map "\C-o" (kbd "C-u 5 C-p"))

;;ページ送り上を改善
(define-key global-map "\C-t" (kbd "M-v"))

;;backspace
(define-key global-map "\C-h" 'delete-backward-char)

;;ctrl+;でコメントアウト
;;(define-key global-map "\C-;" 'comment-or-uncomment-region)

;;time-zoneの設定
;;(setq zone-timer (run-with-idle-timer 120 t 'zone))

;;emacs, twitter
;;(require 'twittering-mode)

;; 括弧の範囲内を強調表示
(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;; 括弧の範囲色
(set-face-background 'show-paren-match "#505050")


;; 選択領域の色
(set-face-background 'region "#306969")

;; タブをスペースで扱う，スペース4
(setq-default tab-width 4 indent-tabs-mode nil)
(setq-default c-basic-offset 4     ;;基本インデント量4
              tab-width 4)          ;;タブ幅4

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;;バックアップファイルを作らない
(setq backup-inhibited t)

;;カラーテーマ

;;(color-theme-initialize)
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;(setq custom-theme-directory "~/.emacs.d/themes")
(load-theme 'manoj-dark t)



;;モードラインの色を黒にした
(set-face-background 'mode-line "Black")

;; Auto Complete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'fundamental-mode)  ;; fundamental-mode
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ;; 曖昧マッチ

;;括弧を表示する
(require 'smartparens-config)
(smartparens-global-mode t)

;;smart-newline
(require 'smart-newline)
(define-key global-map (kbd "\C-m") 'smart-newline)


;;クリップボードの共有
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)


;; indent-guide
;; インデントに色つける奴
;;(require 'indent-guide)
;;(indent-guide-global-mode)
;;(set-face-foreground 'indent-guide-face "white")

;;(setq indent-guide-recursive t)

;;マークダウン記述するときにつかうもの
;;(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;;jedi
(jedi:setup)
  (define-key jedi-mode-map (kbd "<C-tab>") nil) ;;C-tabはウィンドウの移動に用いる
  (setq jedi:complete-on-dot t)
;;  (setq ac-sources
;;    (delete 'ac-source-words-in-same-mode-buffers ac-sources)) ;;jediの補完候補だけでいい
(add-to-list 'ac-sources 'ac-source-filename)
(add-to-list 'ac-sources 'ac-source-jedi-direct)


;;undo-tree
;;\C-zに設定しました
(require 'undo-tree)
(global-undo-tree-mode t)

;;undotreeとundoの設定（M-zでundotreeが起動，C-zで普通のundo）
(define-key global-map "\M-z" (kbd "C-x u"))
(define-key global-map "\C-z" 'undo)

;;_________________________________________________________________
;; Mewの設定（imapでSSLを使ってGmailを送受信する設定）
;;_________________________________________________________________
;;
;;mew
;;(require 'mew)
;;mewの設定は.mew.elに移行しました．

;;マークダウンの設定
(setq markdown-command "multimarkdown")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (mew goto-chg yasnippet undo-tree smartparens smart-newline py-autopep8 pos-tip php-mode markdown-mode madhat2r-theme jedi indent-guide helm gnuplot-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
