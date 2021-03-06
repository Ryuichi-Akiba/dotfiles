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

;; package.elの設定
(when (require 'package nil t)
  ;; MELPAを追加
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  ;; ELPAを追加
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa"))
  ;; Marmaladeを追加
  (add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/"))
  ;; インストールしたパッケージにロードパスを通して読み込む
  (package-initialize))

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
;; バックアップとオートセーブファイルを~/.emacs.d/backups/へ集める
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))
;; オートセーブファイル作成までの秒間隔
;; (setq auto-save-timeout 15)
;; オートセーブファイル作成までのタイプ間隔
;; (setq auto-save-interval 60)

;; multi-termの設定
(when (require 'multi-term nil t)
  ;; 利用するシェルを指定
  (setq multi-term-program "/usr/local/bin/zsh"))

;; ファイルが #! から始まる場合、+xを付けて保存する
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; emacs-lisp-modeのフックをセット
;;(add-hook 'emacs-lisp-mode-hook
;;          '(lambda ()
;;             (when (require 'eldoc nil t)
;;               (setq eldoc-idle-delay 0.2)
;;               (setq eldoc-echo-area-use-multiline-p t)
;;               (turn-on-eldoc-mode))))

;; emacs-lisp-mode-hook用の関数を定義
(defun elisp-mode-hooks ()
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))
;; emacs-lisp-modeのフックをセット
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

;; auto-installの設定
(when (require 'auto-install nil t)
  ;; インストールディレクトリを設定する 初期値は ~/.emacs.d/auto-install/
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; EmacsWikiに登録されているelispの名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;; 必要であればプロキシの設定を行う
  ;; (setq url-proxy-services '(("http" . "localhost:8339")))
  ;; install-elispの関数を利用可能にする
  (auto-install-compatibility-setup))

;; redo+ の設定
;; (install-elisp "http://www.emacswiki.org/emacs/download/redo+.el")
(when (require 'redo+ nil t)
  ;; C-' にリドゥを割り当てる
  (global-set-key (kbd "C-'") 'redo)
  ;; 日本語キーボードの場合C-. などがよいかも
  ;; (global-set-key (kbd "C-.") 'redo)
  )

;; 入力されるシーケンスを置き換える
;; ?\C-? はDELのシーケンス
(keyboard-translate ?\C-h ?\C-?)
;; C-mにnewline-and-indentを割り当てる。初期値はnewline
(define-key global-map (kbd "C-m") 'newline-and-indent)
;; C-tでウィンドウを切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)
;; 折り返しトグルコマンド
;; (define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; anything
;; (auto-install-batch "anything")
(when (require 'anything nil t)
  (setq
   ;; 候補を表示するまでの時間 デフォルトは0.5
   anything-idle-delay 0.3
   ;; タイプして再描画するまでの時間 デフォルトは0.1
   anything-input-idle-delay 0.2
   ;; 候補の最大表示数 デフォルトは50
   anything-candidate-number-limit 100
   ;; 候補が多いときに体感速度を速くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)

   (when (require 'anything-config nil t)
     ;; root権限でアクションを実行するときのコマンド
     ;; デフォルトはsu
     (setq anything-su-or-sudo "sudo"))

   (require 'anything-match-plugin nil t)

   (when (and (executable-find "cmigemo")
              (require 'migemo nil t))
     (require 'anything-migemoo nil t))

   (when (require 'anything-complete nil t)
     ;; lispシンボルの補完候補の再検索時間
     (anything-lisp-complete-symbol-set-timer 150))

   (require 'anything-show-completion nil t)

   (when (require 'auto-install nil t)
     (require 'anything-auto-install nil t))

   (when (require 'descbinds-anything nil t)
     ;; describe-bindingsをAnythingに置き換える
     (descbinds-anything-install)))
