

;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

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

;; メニューバーを非表示
(menu-bar-mode -1)

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format
            (format "%%f - Emacs@%s" (system-name)))

;; 行番号表示
(global-display-line-numbers-mode)

;;emacsの警告音などを消す
(setq ring-bell-function 'ignore)

;;カーソル位置の記憶
(save-place-mode 1)

;;キー移動を5行づつするやつ
(define-key global-map "\C-l" (kbd "C-u 5 C-n"))
(define-key global-map "\C-o" (kbd "C-u 5 C-p"))

;;ページ送り上を改善
(define-key global-map "\C-t" (kbd "M-v"))

;;backspace
(define-key global-map "\C-h" 'delete-backward-char)

;;time-zoneの設定
(setq zone-timer (run-with-idle-timer 120 t 'zone))

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
(load-theme 'misterioso t)

;;モードラインの色を黒にした
(set-face-background 'mode-line "Black")

;; indent-guide
;; インデントに色つける奴        
;;(require 'indent-guide)
;;(indent-guide-global-mode)
;;(set-face-foreground 'indent-guide-face "white")

(setq indent-guide-recursive t)

;;マークダウン記述するときにつかうもの
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(define-key global-map "\C-z" 'undo)

