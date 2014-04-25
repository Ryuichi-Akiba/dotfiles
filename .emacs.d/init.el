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

;; 入力されるシーケンスを置き換える
;; ?\C-? はDELのシーケンス
(keyboard-translate ?\C-h ?\C-?)
;; C-mにnewline-and-indentを割り当てる。初期値はnewline
(define-key global-map (kbd "C-m") 'newline-and-indent)
;; C-tでウィンドウを切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)
;; 折り返しトグルコマンド
;; (define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
