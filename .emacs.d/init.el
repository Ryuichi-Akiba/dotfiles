;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")
;; パスの設定
(add-to-list 'exec-path "/usr/local/bin")

;; 文字コードの設定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
;; Mac OS Xの場合のファイル名の設定
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
;; Windowsの場合のファイル名の設定
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;; asciiフォント
(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 120)
;; 日本語フォント
(set-fontset-font
 nil 'japanese-jisx0208
 ;; 英語名の場合
 ;; (font-spec :family "Hiragino Mincho Pro"))
 (font-spec :family "ヒラギノ丸ゴ Pro"))
;; フォントの横幅を調整
(setq face-font-rescale-alist
      '((".*Menlo.*" . 1.0)
        (".*Hiragino_Mincho_Pro.*" . 1.2)
        (".*Hiragino_Maru_Gothic_Pro.*" . 1.2)
        (".*Hiragino_Kaku_Gothic_Pro.*" . 1.2)
        ("-cdac$" . 1.3)))

;; 現在行のハイライト
(defface my-hl-line-face
  ;; 背景がdarkならば背景色を紺に
  '((((class color) (background dark))
     (:background "NavyBlue" t))
    ;; 背景がlightならば背景色を緑に
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; TABの表示幅。初期値は8
(setq-default tab-width 4)
;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)

(when (require 'color-theme nil t)
  ;; テーマを読み込むための設定
  (color-theme-initialize)
  ;; テーマを変更する
  (color-theme-deep-blue))

;; リージョンの背景色を変更
(set-face-background 'region "darkgreen")

;; paren-mode:対応する括弧を強調して表示する
(setq show-paren-delay 0)
(show-paren-mode t)
;; parenのスタイル: expressionは括弧内も強調表示
(setq show-paren-style 'expression)
;; フェイスを変更する
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")
;; 行番号を常に表示
(global-linum-mode t)
;; カラム番号を表示
(column-number-mode t)
;; ファイルサイズを表示
(size-indication-mode t)
;; 時計を表示
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time-mode t)
;; バッテリー残量を表示
;;(display-battery-mode t)

;; バックアップファイルを作成しない
;; (setq make-backup-files nil)
;; オートセーブファイルを作らない
;; (setq auto-save-default nil)
;; バックアップファイルの作成場所をシステムのTempディレクトリに変更する
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
;; オートセーブファイルの作成場所をシステムのTempディレクトリに変更する
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; 入力されるシーケンスを置き換える
;; ?\C-? はDELのシーケンス
(keyboard-translate ?\C-h ?\C-?)
;; C-mにnewline-and-indentを割り当てる。初期値はnewline
(define-key global-map (kbd "C-m") 'newline-and-indent)
;; C-tでウィンドウを切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)
;; 折り返しトグルコマンド
;; (define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
