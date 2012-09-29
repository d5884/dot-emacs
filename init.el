;;; init.el --- User initialize file. -*- coding: utf-8 -*-

;;; Commentary:
;;
;; ユーザ設定初期化ファイル.
;; Emacs 22 以前は未検証
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; メールアドレス等
(setq user-full-name "Daisuke Kobayashi")
(setq user-mail-address "d5884jp@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cl
(eval-when-compile
  (setq byte-compile-warnings '(not cl-functions free-vars)))
(require 'cl nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 設定ファイル向け関数/マクロ等

(defmacro ini:aif (pred then &rest else)
  "PRED を評価し、結果が non-nil ならば THEN、nil ならば ELSE の評価結果を返す.
THEN、ELSE 内では PRED の評価結果を `it' で参照出来る."
  `(let ((it ,pred))
     (if it ,then ,@else)))

(defmacro ini:awhen (pred &rest body)
  "PRED を評価し、結果が non-nil ならば BODY を評価し、最後の式の結果を返す.
BODY 内では PRED の評価結果を `it' で参照出来る."
  `(ini:aif ,pred (progn ,@body)))

(defmacro ini:aand (pred &rest conds)
  "PRED および CONDS を評価し、結果が全て non-nil ならば最後の式の結果を返す.
CONDS 内では PRED の評価結果を `it' で参照出来る."
  `(let ((it ,pred))
     (and it ,@conds)))

(defmacro ini:system-path (path &optional base)
  "PATH をシステムで認識可能なパスに変換する.
PATH が相対パスの場合 `expand-file-name' で絶対パスに変換される.
BASE が non-nil の場合は絶対パス変換時の基準ディレクトリと見なす.
環境変数などの Emacs 外のプログラムに参照される場合に用いる."
  (if (eq system-type 'windows-nt)
      `(subst-char-in-string ?/ ?\\ (expand-file-name ,path ,base))
    `(expand-file-name ,path ,base)))

(defmacro ini:concat-system-path (paths &optional base original)
  "PATHS をシステムで認識可能なパスの連結に変換する.
PATHS の各要素は自身と BASE を引数に `ini:system-path' で処理される.
セパレータには `path-separator' が用いられる.
ORIGINAL が non-nil であれば最後に連結される."
  `(apply 'concat
	  (mapconcat (lambda (p) (ini:system-path p ,base))
		     ,paths path-separator)
	  (if ,original
	      (list path-separator ,original))))

(defmacro ini:find-exists-path (&rest paths)
  "PATHS のうち存在する最初のパスを返す."
  `(find-if 'file-exists-p (list ,@paths)))

(defmacro ini:command (&rest body)
  "BODY に `interactive' を付与してコマンド化した関数を返す."
  `(lambda () (interactive) ,@body))

(defmacro ini:library-within (lib file &optional exists)
  "ライブラリ LIB と同じディレクトリに配置されている FILE へのパスを返す.
EXISTS が t の場合かつ FILE が存在しない場合は nil を返す.
LIB が存在しない場合は nil を返す."
  `(ini:awhen (locate-library ,lib)
     (let ((name (expand-file-name ,file (file-name-directory it))))
       (if (or (not ,exists)
	       (file-exists-p name))
	   name))))

(defmacro ini:define-quit-key (trigger-function buffer &optional key)
  "TRIGGER-FUNCTION で作成される BUFFER に `bury-buffer' コマンドの呼び出しを定義する.
KEY が non-nil の場合は KEY に、nil の場合は q に `burry-buffer' がバインドされる."
  `(defadvice ,trigger-function (after ,(intern (format "ini:ad-%s-define-quit-key"
							trigger-function)) activate)
     "実行時に `bury-buffer' 呼び出し用のキー設定を行なう."
     (ignore-errors
       (with-current-buffer ,buffer
	 (local-set-key ,(or key "q") 'bury-buffer)))))

;; マクロのインデント設定
(put 'ini:aif 'lisp-indent-function 2)
(put 'ini:awhen 'lisp-indent-function 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ロードパス追加

;; .emacs.d を init.el が置いてある場所にする
;; (setq user-emacs-directory (file-name-directory
;; 			    (or (buffer-file-name) load-file-name)))

;; (存在するなら) ~/.emacs.d/lisp および直下のディレクトリを load-path へ追加
;; 再帰的には追加しない
(ini:awhen (ini:find-exists-path (locate-user-emacs-file "lisp"))
  (setq load-path
	(cons it (append
		  (remove-if-not 'file-directory-p (directory-files it t "^[^.]"))
		  load-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 言語設定/UTF-8化
(set-language-environment 'Japanese)
(cond
 ((eq system-type 'windows-nt)
  (prefer-coding-system 'utf-8-dos)
  (set-file-name-coding-system 'cp932)
  (modify-coding-system-alist 'process "[cC][mM][dD]" '(undecided . cp932)))
 (t
  (prefer-coding-system 'utf-8-unix)))

;; charset と coding-system の優先度設定
(set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
		      'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)

(set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932) ; cp932 を追加

;; 全角チルダ/波ダッシュの解釈をWindowsスタイルに (U+301C > U+FF5E)
(let ((table (make-translation-table-from-alist '((#x301c . #xff5e)))))
  (dolist (coding-system '(utf-8 cp932 utf-16le))
    (coding-system-put coding-system :decode-translation-table table)
    (coding-system-put coding-system :encode-translation-table table)))

;; 改行コードの表示を変更
(setq eol-mnemonic-dos "d")
(setq eol-mnemonic-unix "u")
(setq eol-mnemonic-mac "m")
(setq eol-mnemonic-undecided "?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; フォント/ウィンドウサイズ設定
(when window-system
  (setq vertical-centering-font-regexp ".*")

  (flet ((font-candidate (&rest font-list)
			 (find-if (lambda (f) (find-font (font-spec :name f)))
				  font-list)))
    (let ((fontset "fontset-standard"))
      ;; ベースとなる ASCII フォント
      (ini:awhen (font-candidate "Consolas-11:weight=normal:slant=normal"
				 "DejaVu Sans Mono-10:weight=normal:slant=normal")
	
	;; ASCII
	(set-fontset-font fontset 'ascii it)
	
	;; アクセント付きアルファベット類/ロシア語/ギリシャ語
	(ini:awhen (font-candidate "Consolas"
				   "DefaVu Sans Mono")
	  (dolist (charset '(latin-iso8859-1
			     latin-iso8859-2 
			     latin-iso8859-3
			     latin-iso8859-4
			     cyrillic-iso8859-5
			     greek-iso8859-7))
	    (set-fontset-font fontset charset it)))
	
	;; 日本語 / meiryoKe_602r1.ttc
	;; http://okrchicagob.blog4.fc2.com/blog-entry-121.html
	(ini:awhen (font-candidate "MeiryoKe_Console"
				   "VL ゴシック"
				   "ＭＳ ゴシック")
	  (set-fontset-font fontset 'unicode
			    `(,it . "iso10646-1") nil 'append))
	
	;; ;; Burmese ビルマ語 / Padauk.ttf
	;; ;; http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=padauk
	;; (ini:awhen (font-candidate "Padauk")
	;;   (set-fontset-font fontset 'unicode
	;; 		    `(,it . "iso10646-1") nil 'append))
	
	;; ;; Oriya オリヤー語 / ori1UniMed.ttf
	;; ;; http://www.exnet.btinternet.co.uk/
	;; (ini:awhen (font-candidate "ori1Uni")
	;;   (set-fontset-font fontset 'unicode
	;; 		    `(,it . "iso10646-1") nil 'append))

	;; ;; サイズ調整
	;; (setq face-font-rescale-alist (append '(("MeiryoKe_Console" . 1.2)
	;; 					("Consolas" . 1.0))
	;; 				      face-font-rescale-alist))
	
	;; フレームに設定
	(add-to-list 'default-frame-alist (cons 'font fontset))
	)))

  ;; ウィンドウサイズ
  (setq initial-frame-alist `((top . 60) (left . 120) ,@initial-frame-alist))
  (setq default-frame-alist `((width . 100) (height . 40) ,@default-frame-alist))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cygwin 連携
(when (eq system-type 'windows-nt)
  ;; インストールルート検索
  (ini:awhen (or (ini:aand (getenv "CYGWIN_DIR")
			   (file-exists-p it))
		 (find-if #'file-exists-p
			  (mapcar (lambda (p) (expand-file-name "cygwin" p))
				  (list (getenv "LOCALAPPDATA")
					(getenv "APPDATA")
					(getenv "USERPROFILE")
					(getenv "HOME")
					(getenv "ProgramW6432")
					(getenv "ProgramFiles")
					"c:/"
					"c:/gnupack/app/cygwin"))))

    (let ((cygwin-exec-path		; cygwin のルートパスからの相対パスとして追加
	   (mapcar (lambda (path)
		     (expand-file-name (if (and (> (length path) 0) (eq (aref path 0) ?/))
					   (substring path 1)
					 path) it))
		   `(,(locate-user-emacs-file "bin") "~/bin" "/usr/local/bin" "/usr/bin" "/bin"))))
      (setenv "PATH" (ini:concat-system-path cygwin-exec-path nil (getenv "PATH")))
      (setq exec-path (append cygwin-exec-path exec-path))

      (setenv "LANG" "ja_JP.UTF-8")
      (setenv "CYGWIN" "nodosfilewarning winsymlinks")

      (setq null-device "/dev/null")

      ;; shell
      (when (executable-find "bash")
	(setcar default-process-coding-system ; DOSコマンド混在のため出力を未定に
		(coding-system-change-text-conversion (car default-process-coding-system)
							   'undecided))
	;; (setq locale-coding-system 'utf-8) ; ansi-term, term (conflict current-time-zone)
	
	(setq shell-file-name "bash")
	(setq shell-command-switch "-c")
	(setq system-uses-terminfo nil)
	
	(setenv "SHELL" shell-file-name)

	(defadvice comint-send-input (before ini:ad-comint-send-input activate)
	     "出力時の文字コードを自動判断させる."
	     (ini:awhen (get-process "shell")
	       (set-process-coding-system it
					  (coding-system-change-text-conversion
					   (car default-process-coding-system) 'undecided)
					  (coding-system-change-text-conversion
					   (cdr default-process-coding-system) 'utf-8))))
	(eval-after-load "term"
	  '(require 'shell))
	
	(eval-after-load "shell"
	  ;; gcc -o f_bash.exe fakecygpty.c
	  ;; http://www.meadowy.org/meadow/browser/trunk/nt/fakecygpty.c
	  '(progn
	     (setq explicit-shell-file-name (if (executable-find "f_bash") "f_bash"
					      shell-file-name))
	     
	     (eval-after-load "tramp"
	       '(ini:awhen (executable-find "f_bash")
		  (setq tramp-encoding-shell it)
		  
		  (defadvice shell (around ini:ad-shell-tramp-setup activate)
		    "リモート接続時のシェル名を通常のシェル名に戻す."
		    (if (file-remote-p default-directory)
			(let ((explicit-shell-file-name shell-file-name))
			  ad-do-it)
		      ad-do-it)))
	       )))
	)
      
      ;; システムの Info 追加
      (eval-after-load "info"
	'(ini:awhen (ini:find-exists-path "/usr/share/info")
	   (add-to-list 'Info-additional-directory-list it)))
      
      ;; cygwin-mount / http://home.avvanta.com/~offby1/cygwin-mount/cygwin-mount.el
      ;; ... or git clone https://github.com/emacsmirror/cygwin-mount
      (when (require 'cygwin-mount nil t)
	(cygwin-mount-activate))

      ;; プロセス呼び出し時に引数のみ cp932 へ
      (dolist (pair '((call-process-region . 6)
      		      (call-process . 4)
      		      (start-process . 3)))
      	(let ((f (car pair))
      	      (p (cdr pair)))
      	  (eval `(defadvice ,f (before ,(intern (format "ini:ad-%s-encode-setup" f))
      				       activate)
      		   ,(format "実行時に%d番目以降の引数を cp932 でエンコードする." p)
      		   (ad-set-args ,p
      				(mapcar (lambda (arg)
      					  (if (multibyte-string-p arg)
      					      (encode-coding-string arg 'cp932)
      					    arg))
      					(ad-get-args ,p)))))))

      ;; Cygwin 環境だと C-c/C-z/C-d がまともに動かないので直接送信に置き換え
      (defadvice comint-interrupt-subjob (around ini:ad-comint-send-intr activate)
      	"直接 C-c を送信."
      	(process-send-string nil "\C-c"))
      (defadvice comint-stop-subjob (around ini:ad-comint-send-stop activate)
      	"直接 C-z を送信."
      	(process-send-string nil "\C-z"))
      (defadvice comint-send-eof (around ini:ad-comint-send-eof activate)
      	"直接 C-d を送信."
      	(process-send-string nil "\C-d"))

      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; キーバインド変更

;; Ctrl-h を削除に
(when (load "term/bobcat" nil t)
  (terminal-init-bobcat))

(global-set-key (kbd "C-x ?") 'help-command)
(global-set-key (kbd "C-x 7") 'toggle-truncate-lines)
(global-set-key (kbd "C-x C-z") 'compile)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z z") 'shell)
(global-set-key [remap list-buffers] 'bs-show)

(when window-system
  ;; フレーム外/モードラインでのホイール回しでエラーを出さない
  (global-set-key (kbd "<nil> <wheel-up>") 'ignore)
  (global-set-key (kbd "<nil> <wheel-down>") 'ignore)
  (global-set-key (kbd "<mode-line> <wheel-up>") 'ignore)
  (global-set-key (kbd "<mode-line> <wheel-down>") 'ignore)
  
  ;; ホイールクリックで貼り付けは使わない
  (global-unset-key (kbd "<down-mouse-2>"))
  (global-unset-key (kbd "<mouse-2>"))
  (global-unset-key (kbd "<left-fringe> <mouse-2>"))
  (global-unset-key (kbd "<right-fringe> <mouse-2>"))

  ;; 文字サイズ変更
  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
  (global-unset-key (kbd "<C-down-mouse-2>"))
  (global-set-key (kbd "<C-mouse-2>") (ini:command (text-scale-set 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 標準の変数設定

;; 色々
(setq frame-title-format "%b")
(setq kill-do-not-save-duplicates t)
(setq require-final-newline t)
(setq search-invisible t)
(setq truncate-partial-width-windows nil)
(setq view-read-only t)
(setq visible-bell t)
(setq comment-style 'multi-line)
;; (setq-default cursor-in-non-selected-windows nil)

;; 起動画面
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; スクロール関係
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 35)
(setq scroll-margin 0)
(setq scroll-step 1)
(setq next-screen-context-lines 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))

;; バックアップ関係
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 起動時のエコーエリアメッセージを黙らせる
;; ファイル中の inhibit-startup-echo-area-message の記述を直接見にくるので
;; Daemon 起動時以外は表示関数を潰す
(defadvice display-startup-echo-area-message (around ini:ad-shut-up-echo-message activate)
  "起動時のエコーエリアのメッセージを表示しない."
  (if (daemonp) ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; パッケージ別機能設定

(tool-bar-mode -1)
(menu-bar-mode -1)
(set-scroll-bar-mode 'right)
(blink-cursor-mode -1)
(column-number-mode t)
(show-paren-mode t)
(delete-selection-mode t)
(temp-buffer-resize-mode t)
(mouse-avoidance-mode 'exile)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; package
(eval-after-load "package"
  '(progn
     (setq package-archives (append
			     '(("marmalade" . "http://marmalade-repo.org/packages/")
			       ("melpa" . "http://melpa.milkbox.net/packages/"))
			     package-archives))
     (package-initialize)
     ))

;; compile
(eval-after-load "compile"
  '(setq compilation-scroll-output t))

;; recentf
(eval-after-load "recentf" ;; 基本的に使わないがファイルをホームに作らないよう設定
  '(setq recentf-save-file (locate-user-emacs-file "recentf")))

;; info
(eval-after-load "info"
  '(ini:awhen (ini:find-exists-path (locate-user-emacs-file "info"))
       (add-to-list 'Info-additional-directory-list it)))

;; ispell
(eval-after-load "ispell"
  ;; from http://www.an.econ.kobe-u.ac.jp/~namba/meadow/words.lzh
  '(ini:awhen (ini:find-exists-path "/usr/dict/words"
				    "/usr/share/dict/words"
				    (locate-user-emacs-file "words"))
     (setq ispell-alternate-dictionary it)))

;; man & woman
(eval-after-load "woman"
  '(setq woman-fill-frame t
	 woman-cache-filename (locate-user-emacs-file "woman_cache")))

;; bookmark
(eval-after-load "bookmark"
  '(setq bookmark-default-file (locate-user-emacs-file "bookmark")))

;; imenu
(when (fboundp 'imenu)
  (global-set-key (kbd "C-z C-j") 'imenu)
  (eval-after-load "imenu"
    '(setq imenu-auto-rescan t)))

;; shell
(eval-after-load "shell"
  '(progn
     (setq comint-prompt-read-only t)

     (add-hook 'shell-mode-hook
	       '(lambda ()
		  (ini:awhen (get-process "shell")
		    (set-process-query-on-exit-flag it nil))))

     (define-key shell-mode-map (kbd "M-p") 'comint-previous-matching-input-from-input)
     (define-key shell-mode-map (kbd "M-n") 'comint-next-matching-input-from-input)))

;; ansi-color
(when (require 'ansi-color nil t)
  (when (eq 'light (frame-parameter nil 'background-mode))
    (setq ansi-color-names-vector
	  ["black"		; black
	   "dark red"		; red
	   "dark gree"		; green
	   "dark goldenrod"     ; yellow
	   "dark blue"		; blue
	   "dark magenta"       ; magenta
	   "dark cyan"		; cyan
	   "white"])		; white
    (setq ansi-color-map (ansi-color-make-color-map)))
  (ansi-color-for-comint-mode-on))

;; apropos
(eval-after-load "apropos"
  '(progn
     (setq apropos-do-all t)
     (define-key apropos-mode-map (kbd "n") 'forward-button)
     (define-key apropos-mode-map (kbd "p") 'backward-button)))

;; cua-mode
(when (require 'cua-base nil t)
  (cua-selection-mode t))

;; iswitchb
(when (require 'iswitchb nil t)
  (iswitchb-mode t)
  (setq iswitchb-default-method 'samewindow)
  (add-hook 'iswitchb-define-mode-map-hook
	    (lambda ()
	      (define-key iswitchb-mode-map (kbd "C-f") 'iswitchb-next-match)
	      (define-key iswitchb-mode-map (kbd "C-b") 'iswitchb-prev-match)))

  (defadvice iswitchb-exhibit
    (after ini:ad-iswitchb-exhibit-with-display-buffer activate)
    "選択しているバッファをウィンドウに表示する."
    (when (and (eq iswitchb-method iswitchb-default-method)
	       iswitchb-matches)
      (let ((iswitchb-method 'samewindow)
	    (selected (get-buffer-window
		       (find-if-not 'minibufferp (buffer-list)))))
	(when selected
	  (select-window selected)
	  (iswitchb-visit-buffer
	   (get-buffer (car iswitchb-matches)))
	  (select-window (minibuffer-window))))
      ))
  )

;; grep
(eval-after-load "grep"
  '(progn
     (when (executable-find "xargs")
       (setq grep-find-use-xargs 'gnu))

     (when (executable-find "lgrep")
       ;; lv 付属の多国語化 grep
       (setq grep-program "lgrep")
       (setq grep-command "lgrep -n -Au8 -Ia ")
       ;; (defadvice lgrep )
       (setq grep-find-ignored-files nil))
     ))

;; バッファ名のユニーク化
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

;; サーバ機能
(when (and (require 'server nil t)
	   (not (server-running-p)))
  (server-start))

;; スクリプトファイルの自動 +x
(when (fboundp 'executable-make-buffer-file-executable-if-script-p)
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

;; Tramp
(eval-after-load "tramp"
  '(setq tramp-default-method "ssh"))

(eval-after-load "tramp-sh"
  '(let ((process-environment tramp-remote-process-environment))
     (setenv "LC_ALL" nil)		; リモートのロケールは接続先に準じる
     (setq tramp-remote-process-environment process-environment)))

;; ffap
(when (require 'ffap nil t)
  (ffap-bindings)
  (when window-system
    (global-set-key (kbd "<mouse-2>") 'ffap-at-mouse))
  )

;; hideshow
(eval-after-load "hideshow"
  '(progn
     (defvar ini:hs-fringe-mark 'right-arrow
       "*隠れた行の fringe に表示する bitmap 名.
`fringe-bitmaps' 内に設定されているシンボル名から選ぶ.")

     (defun ini:hs-mark-fringe (ovr)
       "`hs-toggle-hiding'で隠された行の OVR を編集して fringe にマークを付ける."
       (when (eq 'code (overlay-get ovr 'hs))
	 (let ((hiding-text "...")
	       (fringe-anchor (make-string 1 ?x)))
	   (put-text-property 0 1 'display (list 'left-fringe ini:hs-fringe-mark) fringe-anchor)
	   (overlay-put ovr 'before-string fringe-anchor)
	   (overlay-put ovr 'display hiding-text))))

     (setq hs-set-up-overlay 'ini:hs-mark-fringe)
     (define-key hs-minor-mode-map (kbd "C-c <C-SPC>") 'hs-toggle-hiding)))

;; elisps
(defun ini:byte-compile-current-file ()
  "開いているファイルをバイトコンパイルする."
  (interactive)
  (byte-compile-file buffer-file-name))

(defun ini:byte-compile-current-file-as-necessary ()
  "開いているファイルをバイトコンパイルする.
既にコンパイル済みのファイルがあり、ソースファイルの方が新しい場合のみコンパイルする."
  (interactive)
  (require 'bytecomp nil t)
  (let* ((file (buffer-file-name))
	 (dest (byte-compile-dest-file file)))
    (when (and (file-exists-p dest)
	       (file-writable-p dest)
	       (file-newer-than-file-p file dest))
      (byte-compile-file file))))

(eval-after-load "lisp-mode"
  '(progn
     (define-key lisp-interaction-mode-map (kbd "C-m") 'newline-and-indent)
     (define-key emacs-lisp-mode-map (kbd "C-m") 'newline-and-indent)
     (define-key emacs-lisp-mode-map [remap compile] 'ini:byte-compile-current-file)

     (add-hook 'emacs-lisp-mode-hook
     	       (lambda ()
     		 (hs-minor-mode t)
     		 (eldoc-mode t)
		 (add-hook 'after-save-hook
			   'ini:byte-compile-current-file-as-necessary
			   nil t)
     		 ))
     ))

;; occur
(define-key occur-mode-map (kbd "n") 'occur-next)
(define-key occur-mode-map (kbd "p") 'occur-prev)
(add-hook 'occur-mode-hook 'next-error-follow-minor-mode)

;; view-mode
(eval-after-load "view"
  '(progn
     (define-key view-mode-map "j" 'next-line)
     (define-key view-mode-map "k" 'previous-line)
     (defadvice view-mode-exit (before ini:ad-view-mode-exit activate)
       (when view-mode
	 (hl-line-mode -1)))
     (add-hook 'view-mode-hook (lambda () (hl-line-mode t)))
     ))

;; dired
(global-set-key (kbd "C-x C-d") 'dired-other-window)
(eval-after-load "dired"
  '(progn
     (setq dired-listing-switches "-lGAF")
     ;; (setq dired-listing-switches "-lAF --time-style=long-iso")
     (setq ls-lisp-dirs-first t)
     (setq ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M"))
     (setq ls-lisp-use-localized-time-format t)

     (add-hook 'dired-mode-hook
	       (lambda ()
		 ;; dired 上でのみゴミ箱使用
		 (toggle-truncate-lines t)
		 (set (make-local-variable 'delete-by-moving-to-trash) t)
		 (hl-line-mode t)))

     (if (executable-find "cygstart")
	 (define-key dired-mode-map (kbd "E")
	   (ini:command (start-process "cygstart" nil "cygstart"
				       (dired-get-file-for-visit)))))

     (define-key dired-mode-map (kbd "v")
       (ini:command (view-file-other-window (dired-get-file-for-visit))))

     (define-key dired-mode-map (kbd "q")
       (ini:command (kill-buffer (current-buffer))))

     (dolist (f '(dired-find-file dired-up-directory))
       (eval `(defadvice ,f (around ,(intern
				      (format "ini:ad-%s-and-kill" f))
				    activate)
		"移動前のディレクトリバッファ削除およびソート順序保持."
		(let ((prev-buffer (current-buffer))
		      (switch dired-actual-switches))
		  ad-do-it
		  (if (and (not (eq prev-buffer (current-buffer)))
			   (eq major-mode 'dired-mode))
		      (progn
			;; popwin 管理下の場合はバッファの差し替えを通知する
			(if (and (boundp 'popwin:popup-buffer)
				 (eq popwin:popup-buffer prev-buffer))
			    (setq popwin:popup-buffer (current-buffer)))
			(kill-buffer prev-buffer)
			(dired-sort-other (setq dired-actual-switches switch))))))
	     ))

     ;; find-dired
     (eval-after-load "find-dired"
       '(progn
	  (setq find-ls-option (cons (format "-print0 | xargs -0 ls %sd --time-style=long-iso"
					     dired-listing-switches)
				     (format "%sd" dired-listing-switches)))
	  
	  (when (require 'grep nil t)
	    (defadvice find-grep-dired (around ini:ad-find-grep-replace activate)
	      "lgrep がちゃんと動かないので普通の grep に置き換え."
	      (let ((grep-program "grep"))
		ad-do-it)))
	  ))

     ;; dired-x は導入するが C-x C-j は skk 等で使用
     (let ((cxcj (key-binding (kbd "C-x C-j"))))
       (when (require 'dired-x nil t)
	 (global-set-key (kbd "C-x C-j") cxcj)))
     ))

;; cc-mode
(eval-after-load "cc-mode"
  '(progn
     (add-hook 'c-mode-hook
	       (lambda ()
		 (hs-minor-mode t)
		 (hide-ifdef-mode t)))
     ))

;; ruby-mode / http://svn.ruby-lang.org/cgi-bin/viewvc.cgi/trunk/misc/
;; ... or svn co http://svn.ruby-lang.org/repos/ruby/trunk/misc ruby
(eval-after-load "ruby-mode"
  '(progn
     (when (require 'ruby-electric nil t)
       (add-hook 'ruby-mode-hook 'ruby-electric-mode))

     (eval-after-load "yasnippet"
       '(eval-after-load "auto-complete"
	  '(defun ac-ruby-mode-setup ()
	     (setq ac-sources (cons 'ac-source-yasnippet ac-sources)))))

     (eval-after-load "hideshow"
       '(add-to-list 'hs-special-modes-alist
     		     `(ruby-mode
     		       ,(concat "\\<"
     				(regexp-opt '("class" "module" "def" "if" "unless" "loop"
     					      "case" "while" "until" "for" "begin" "do"))
     				"\\>\\|{")
     		       "\\<end\\>\\|}"
     		       "#"
     		       ruby-move-to-block
     		       nil))
       )

     (add-hook 'ruby-mode-hook 'hs-minor-mode)
     ))

(when (executable-find "ruby")
  ;; rubydb
  (when (locate-library "rubydb3x")
    (autoload 'rubydb "rubydb3x" nil t)
    (eval-after-load "rubydb3x"
      '(setq rubydb-command-name
	     (if (executable-find "f_ruby") "f_ruby" "ruby"))
      ))

  ;; inf-ruby
  (when (locate-library "inf-ruby")
    (autoload 'run-ruby "inf-ruby" nil t)
    (autoload 'inf-ruby-keys "inf-ruby" nil t)
    (eval-after-load "inf-ruby"
      '(progn
	 (setq ruby-program-name
	       (format "%s %s --inf-ruby-mode"
		       (or (executable-find "f_ruby") "ruby")
		       (or (executable-find "irb")
			   (ini:find-exists-path "/usr/local/bin/irb"
						 "/usr/bin/irb")
			   "irb")))
	 
	 ;; inf-ruby-completion
	 ;;     from http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-core/20892
	 ;; ... or git clone git://gist.github.com/1543904.git inf-ruby-completion
	 (require 'inf-ruby-completion nil t)))
    (eval-after-load "ruby-mode"
      '(inf-ruby-keys))
    ))

;; zencoding / git clone https://github.com/rooney/zencoding
(autoload 'zencoding-mode "zencoding-mode" nil t)
(eval-after-load "zencoding-mode"
  '(progn
     (define-key zencoding-mode-keymap (kbd "<C-return>") nil)
     (define-key zencoding-mode-keymap (kbd "C-j") nil)
     (define-key zencoding-mode-keymap (kbd "C-M-/") 'zencoding-expand-line)
     (eval-after-load "yasnippet"
       '(define-key zencoding-mode-keymap [remap zencoding-expand-line] 'zencoding-expand-yas))
     (setq zencoding-indentation 2)
     ))

;; html-mode
(eval-after-load "sgml-mode"
  '(add-hook 'html-mode-hook 'zencoding-mode))

;; nxml-mode
(when (locate-library "nxml-mode")
  (fset 'html-mode 'nxml-mode)
  (fset 'xml-mode 'nxml-mode)

  (eval-after-load "nxml-mode"
    '(add-hook 'nxml-mode-hook 'zencoding-mode)))

;; skeleton
(when (and (not (locate-library "flex-autopair"))
	   (require 'skeleton nil t))
  (setq skeleton-pair t)
  (global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  ;; (global-set-key (kbd "`") 'skeleton-pair-insert-maybe)
  (global-set-key (kbd "\"") 'skeleton-pair-insert-maybe))

;; flex-autopair
(when (require 'flex-autopair nil t)
  (setq flex-autopair-echo-actionp nil)
  (flex-autopair-mode))

;; gswin32
;;  http://www.khotta.org/ghost/index.html
;;  http://w32tex.org/index-ja.html
(when (eq system-type 'windows-nt)
  (let* ((gs-root (ini:aif (executable-find "gswin32c")
		      (expand-file-name ".." (file-name-directory it))
		    (ini:find-exists-path "c:/gs" "c:/gnupack/app/gs")))
	 (gswin-command (expand-file-name "bin/gswin32c" gs-root)))
    (when gs-root
      (unless (getenv "GS_LIB")
	(setenv "GS_LIB" (ini:concat-system-path '("lib" "kanji" "Resource/Init") gs-root)))
      (unless (getenv "GS_DLL")
	(setenv "GS_DLL" (ini:system-path "bin/gsdll32.dll" gs-root)))
      (setenv "PATH" (ini:concat-system-path '("bin" "lib") gs-root (getenv "PATH")))

      ;; gs
      (eval-after-load "gs"
	`(setq gs-command ,gswin-command
	       gs-device "display"))

      ;; lpr
      (eval-after-load "lpr"
	'(setq printer-name nil))

      ;; ps-print-buffer
      (eval-after-load "ps-print"
	`(setq ps-print-color-p t
	       ps-lpr-command ,gswin-command
	       ps-lpr-switches '("-sDEVICE=mswinpr2" "-dNOPAUSE" "-dBATCH" "-dWINKANJI")
	       ps-multibyte-buffer 'non-latin-printer
	       ps-printer-name nil
	       ps-printer-name-option nil))

      ;; doc-view
      (eval-after-load "doc-view"
	`(progn
	   (setq doc-view-ghostscript-program ,gswin-command)
	   (push "-dWINKANJI" doc-view-ghostscript-options)
	   ))
      )))

;; calendar
(eval-after-load "calendar"
  '(progn
     ;; solor / geocode from http://api.knecht.jp/geocoding
     (setq calendar-latitude 35.6894875)
     (setq calendar-longitude 139.6917064)
     (setq calendar-location-name "Tokyo, JP")
     (setq calendar-time-display-form '((format "%2s:%2s%s" 12-hours minutes am-pm)))
     (setq calendar-date-display-form '((format "%2s/%2s/%2s" year month day)))
     (setq calendar-mark-holidays-flag t)
     (add-hook 'today-visible-calendar-hook 'calendar-mark-today)

     (setq diary-file (locate-user-emacs-file "diary"))
     (setq diary-entry-marker 'link)
     (setq diary-list-include-blanks t)
     (setq calendar-mark-diary-entries-flag t)

     (define-key calendar-mode-map (kbd "n") 'calendar-forward-week)
     (define-key calendar-mode-map (kbd "p") 'calendar-backward-week)
     (define-key calendar-mode-map (kbd "f") 'calendar-forward-day)
     (define-key calendar-mode-map (kbd "b") 'calendar-backward-day)
     (define-key calendar-mode-map (kbd "C-j") 'diary-view-entries)
     (define-key calendar-mode-map (kbd "<RET>") 'diary-view-entries)
     
     (ini:define-quit-key calendar-list-holidays "*Holidays*")
     (ini:define-quit-key calendar-sunrise-sunset-month "*Sunrise/Sunset Times*")
     (ini:define-quit-key calendar-lunar-phases "*Phases of Moon*")

     ;; 月の満ち欠けの日本語化
     (eval-after-load "lunar"
       '(setq lunar-phase-names '("新月" "上弦" "満月""下弦")))

     ;; 日本の祝日表示
     ;; http://www.meadowy.org/meadow/netinstall/browser/branches/3.00/pkginfo
     (when (require 'japanese-holidays nil t)
       (setq calendar-holidays
	     (append japanese-holidays holiday-local-holidays holiday-other-holidays))
       
       (setq calendar-weekend-marker (defface calendar-weekend nil
				       "Face for indicating week end in the calendar."))
       
       (add-hook 'today-visible-calendar-hook 'calendar-mark-weekend)
       (add-hook 'today-invisible-calendar-hook 'calendar-mark-weekend))
     ))

;; emacs-w3m / cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot co emacs-w3m
(when (and (executable-find "w3m")
	   (require 'w3m-load nil t))
  (eval-after-load "w3m"
    '(progn
       (setq w3m-home-page "http://www.google.com")
       (setq w3m-default-display-inline-images t)
       (setq w3m-use-title-buffer-name t)
       (setq w3m-use-cookies t)

       (ini:awhen (ini:library-within "w3m" "icons" t)
	 (setq w3m-icon-directory it))
       ))
  )

;; markdown-mode / git clone git://jblevins.org/git/markdown-mode.git
(when (locate-library "markdown-mode")
  (autoload 'markdown-mode "markdown-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.\\(md\\(wn\\|t\\)?\\|markdown\\|text\\)\\'" .
				  markdown-mode)))

;; sdic / http://www.namazu.org/~tsuchiya/sdic/
(when (locate-library "sdic")
  (dolist (function '(sdic-describe-word sdic-describe-word-at-point))
    (autoload function "sdic" nil t))
  (global-set-key (kbd "C-c w") 'sdic-describe-word)
  (global-set-key (kbd "C-c C-w") 'sdic-describe-word-at-point)

  (flet ((dict (file) (expand-file-name file (locate-user-emacs-file "etc/sdic"))))
    (let ((waei-dict (dict "jedict-u.sdic"))
	  (eiwa-dict (dict "gene-u.sdic")))
      (eval-after-load "sdic"
	`(progn
	   (setq sdic-default-coding-system 'utf-8-unix)
	   (setq sdic-eiwa-dictionary-list '((sdicf-client ,eiwa-dict
							   (strategy direct))))
	   (setq sdic-waei-dictionary-list '((sdicf-client ,waei-dict
							   (add-keys-to-headword t)
							   (strategy direct))))

	   ;; popwin 対応
	   (when (require 'popwin nil t)
	     (add-to-list 'popwin:special-display-config
			  `(,sdic-buffer-name :height ,sdic-window-height))

	     ;; バッファ表示系統を popwin が認識可能なシンプルな操作に置き換える
	     (defadvice sdic-other-window (around ini:ad-sdic-other-normalize
						  activate)
	       "`sdic' のバッファ移動を普通の操作にする."
	       (other-window 1))

	     (defadvice sdic-close-window (around ini:ad-sdic-close-normalize
						  activate)
	       "`sdic' のバッファクローズを普通の操作にする."
	       (bury-buffer sdic-buffer-name))
	     
	     (defadvice sdic-display-buffer (around ini:ad-sdic-display-normalize
						    activate)
	       "`sdic' のバッファ表示を普通の操作にする."
	       (setq ad-return-value (buffer-size))
	       (let ((p (or (ad-get-arg 0)
			    (point))))
		 (and sdic-warning-hidden-entry
		      (> p (point-min))
		      (message "この前にもエントリがあります。"))
		 (display-buffer (get-buffer sdic-buffer-name))
		 (set-window-start (get-buffer-window sdic-buffer-name) p)
		 (goto-char p)
		 ))
	     )
	   )))))

;; shell-pop / http://www.emacswiki.org/emacs/shell-pop.el
;; ... or git clone https://github.com/emacsmirror/shell-pop
(when (locate-library "shell-pop")
  (autoload 'shell-pop "shell-pop" nil t)
  (global-set-key (kbd "C-z C-z") 'shell-pop))

;; popwin / git clone https://github.com/m2ym/popwin-el
(when (require 'popwin nil t)
  (global-set-key (kbd "C-z C-s") 'popwin:stick-popup-window)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:special-display-config
  	'("*Process List*"
  	  "*Proced*"
	  ;; vc
	  "*vc-diff*"
  	  "*vc-change-log*"
	  ;; calendar and diary
	  (calendar-mode :stick t)
	  "*Sunrise/Sunset Times*"
	  "*Phases of Moon*"
  	  "*Holidays*"
	  (diary-fancy-display-mode :noselect t)
  	  ("*Shell Command Output*" :stick t :noselect t)
	  ("*Occur*" :stick t)		; not mode because occur-edit
	  (apropos-mode :stick t)
	  (grep-mode :stick t)
	  (completion-list-mode :noselect t)
  	  (compilation-mode :stick t :noselect t)
	  (help-mode :stick t)
  	  (dired-mode :stick t)))
  )

;; session / http://emacs-session.sourceforge.net/
(when (require 'session nil t)
  (add-hook 'after-init-hook 'session-initialize)
  (setq session-initialize '(de-saveplace session places))
  (setq session-save-file (locate-user-emacs-file "session"))
  (setq session-globals-include '((kill-ring 50)
				  (session-file-alist 500 t)
				  (file-name-history 10000)))
  (setq session-globals-max-string 100000000)
  (setq session-undo-check -1)
  (setq history-length t)
  (defadvice session-initialize-do (around ini:session-load-silently activate)
    "セッションロード時のメッセージを抑制する."
    (let ((org-load (symbol-function 'load)))
      (flet ((load (file &optional noerror nomessage nosuffix must-suffix)
		   (funcall org-load file noerror t nosuffix must-suffix)))
	ad-do-it))))

;; Daredevil SKK / http://openlab.ring.gr.jp/skk/
;; ... or cvs -d:pserver:guest@openlab.jp:/circus/cvsroot login [guest]
;;        cvs -d:pserver:guest@openlab.jp:/circus/cvsroot co -d skk skk/main
(when (require 'skk-setup nil t)
  (setq skk-user-directory user-emacs-directory)
  (setq skk-init-file (expand-file-name "skk-init.el" skk-user-directory))
  (global-set-key [remap toggle-input-method] 'skk-mode)
  (global-set-key [remap skk-auto-fill-mode] 'ignore)
  
  ;; 実験的拡張へのロードパス追加(あれば)
  (ini:awhen (ini:library-within "skk" "experimental" t)
    (add-to-list 'load-path it))

  (add-hook 'skk-load-hook
	    (lambda ()
	      ;; ローカルの辞書設定
	      (let ((dict-dir (locate-user-emacs-file "etc/skk")))
		(ini:awhen (ini:find-exists-path (expand-file-name "SKK-JISYO.L" dict-dir))
		  (setq skk-large-jisyo it
			skk-aux-large-jisyo it))

		;; 追加の通常辞書 (あれば)
		(setq skk-extra-jisyo-file-list
		      (remove-if-not 'file-exists-p
				     (mapcar
				      (lambda (f)
					(expand-file-name f dict-dir))
				      '("SKK-JISYO.JIS2"
					"SKK-JISYO.JIS2004"
					"SKK-JISYO.JIS3_4"
					"SKK-JISYO.assoc"
					"SKK-JISYO.geo"
					"SKK-JISYO.jinmei"
					"SKK-JISYO.station"
					"SKK-JISYO.law"
					"SKK-JISYO.fullname"
					"SKK-JISYO.propernoun"
					"SKK-JISYO.okinawa"
					"SKK-JISYO.pubdic+"
					"SKK-JISYO.edict"
					"zipcode/SKK-JISYO.zipcode"
					"zipcode/SKK-JISYO.office.zipcode")
				      )))
		
		;; チュートリアルファイル
		(ini:awhen (locate-file "SKK.tut" (list dict-dir
							(ini:library-within "skk" "etc")))
		  (setq skk-tut-file it)))
	      
	      (when (require 'skk-study nil t)
		(setq skk-study-backup-file nil))
	      (when (require 'skk-tankan nil t) ; Tan@ or /10@ or /@@
		(add-to-list 'skk-search-prog-list
			     '(skk-tankan-search 'skk-search-jisyo-file
						 skk-large-jisyo 10000)))
	      (require 'skk-hint nil t)	; ▽はやま<SPC>;はし<SPC>
	      ;; (require 'context-skk)
	      
	      (setq skk-latin-mode-string "A")
	      (setq skk-abbrev-mode-string "@")
	      (setq skk-hiragana-mode-string "あ")
	      (setq skk-katakana-mode-string "ア")
	      (setq skk-jisx0208-latin-mode-string "Ａ")
	      (setq skk-jisx0201-mode-string "ｱｧ")
	      (setq skk-indicator-use-cursor-color nil)

	      (defadvice skk-mode-string-to-indicator (before ini:ad-skk-mode-elimit-hyphen
							      activate)
		"SKKインジケータの先頭のハイフン削除."
		(ad-set-arg 1 (replace-regexp-in-string "^-+" ":" (ad-get-arg 1))))

	      (setq skk-use-jisx0201-input-method t)
	      (setq skk-share-private-jisyo t)
	      (setq skk-search-sagyo-henkaku 'anything)
	      (setq skk-egg-like-newline t)
	      (setq skk-delete-implies-kakutei nil)
	      (setq skk-isearch-mode-enable nil)
	      ;; (setq skk-isearch-start-mode 'latin)
	      (setq skk-check-okurigana-on-touroku 'ask)
	      ;; (setq skk-henkan-okuri-strictly t)
	      (setq skk-henkan-strict-okuri-precedence t)
	      (setq skk-backup-jisyo nil)
	      (setq skk-keep-record nil)
	      (setq skk-dcomp-activate t)
	      (setq skk-show-annotation t)
	      )))

;; migemo / http://0xcc.net/migemo/
;; ... or cvs -d:pserver:anonymous@migemo.cvs.sourceforge.net:/cvsroot/migemo login
;;        cvs -d:pserver:anonymous@migemo.cvs.sourceforge.net:/cvsroot/migemo co migemo
;; cmigemo / http://www.kaoriya.net/software/cmigemo
;; ... or    http://code.google.com/p/cmigemo/
(ini:awhen (or (executable-find "cmigemo")
	       (executable-find "migemo"))
  (when (require 'migemo nil t)
    (setq migemo-command it)

    (when (string-match "^cmigemo" (file-name-nondirectory migemo-command))
      (setq migemo-options '("-q" "--emacs"))
      (setq migemo-user-dictionary nil)
      (setq migemo-regex-dictionary nil)
      (setq migemo-coding-system 'utf-8-unix)
      (setq migemo-dictionary (locate-file "utf-8/migemo-dict"
					   `(,(locate-user-emacs-file "etc/migemo")
					     "/usr/local/share/migemo"
					     "/usr/share/migemo")
					   ))
      )
    (setq migemo-use-pattern-alist t)
    (setq migemo-use-frequent-pattern-alist t)
    (setq migemo-pattern-alist-length 1024)
    (define-key isearch-mode-map (kbd "M-k") 'migemo-isearch-toggle-migemo) ; compatible with kogiku

    (migemo-init)

    ;; 小菊 / http://sourceforge.jp/projects/kogiku/
    ;; ... or cvs -d:pserver:anonymous@cvs.sourceforge.jp:/cvsroot/kogiku login
    ;;        cvs -d:pserver:anonymous@cvs.sourceforge.jp:/cvsroot/kogiku co kogiku
    (when (require 'kogiku nil t)
      (setq kogiku-minibuffer-prompt-string "")
      (setq kogiku-minibuffer-indicator-strings '("J" "E"))
      (setq kogiku-enable-once nil)
      (setq kogiku-use-advocate nil)
      ;; emacs-23 以降の partial-completion に非対応
      ;; initial-completionもダメっぽい
      (setq completion-styles (delete 'partial-completion completion-styles)))
    ))

;; yasnippet / git clone https://github.com/capitaomorte/yasnippet
(when (require 'yasnippet nil t)
  (setq yas-verbosity 1)
  (setq yas-prompt-functions (delete 'yas-x-prompt yas-prompt-functions))
  (setq yas-trigger-key "<f12>")	; 基本的に auto-complete からしか使わない
  (yas-global-mode t)

  ;; auto insert
  (add-hook 'find-file-hook 'auto-insert)
  (eval-after-load "autoinsert"
    '(progn
       (setq auto-insert-directory (locate-user-emacs-file "template"))

       (defvar ini:auto-insert-template-modtime nil
	 "テンプレートディレクトリの更新時間.")

       (defun ini:auto-insert-yas-expand ()
	 "`auto-insert' するテンプレートを `yasnippet' のスニペットと見做して展開する."
	 (yas-expand-snippet (buffer-string) (point-min) (point-max)))

       (defadvice auto-insert (before ini:auto-insert-update-template activate)
	 "`auto-insert' 前にテンプレート一覧を更新する."
	 (let ((modtime (file-attributes auto-insert-directory)))
	   (unless (equal modtime ini:auto-insert-template-modtime)
	     (setq ini:auto-insert-template-modtime modtime)
	     (setq auto-insert-alist nil)
	     (dolist (template (directory-files auto-insert-directory nil "^[^.]"))
	       (add-to-list 'auto-insert-alist
			    (cons (intern (file-name-sans-extension template))
				  (vector template 'ini:auto-insert-yas-expand))))
	     )))

       ))
  )

;; auto-complete-mode / https://github.com/auto-complete/auto-complete
;; https://github.com/auto-complete/popup-el
;; https://github.com/auto-complete/fuzzy-el
(when (require 'auto-complete-config nil t)
  (ini:awhen (ini:library-within "auto-complete-config" "dict" t)
    (add-to-list 'ac-dictionary-directories it))

  (ac-config-default)

  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (define-key ac-completing-map (kbd "C-g") 'ac-stop)

  (setq ac-use-menu-map t)
  
  (when (require 'pos-tip nil t)
    (setq ac-quick-help-prefer-pos-tip t))

  (eval-after-load "yasnippet"		; for >0.8.0(beta)
    '(if (and (fboundp 'yas--get-snippet-tables)
  	      (not (fboundp 'yas/get-snippet-tables)))
	 (progn
	   (defalias 'yas/table-hash 'yas--table-hash)
	   (defalias 'yas/get-snippet-tables 'yas--get-snippet-tables)
	   (defalias 'yas/table-parent 'yas--table-parents))))
  )

;; popup-kill-ring / http://www.emacswiki.org/emacs/popup-kill-ring.el
;; ... or bzr branch https://code.launchpad.net/~khiker/+junk/popup-kill-ring
;; ... or git clone https://github.com/emacsmirror/popup-kill-ring
(when (ignore-errors (require 'popup-kill-ring)) ;require popup and pos-tip
  (setq popup-kill-ring-interactive-insert t)
  (global-set-key (kbd "M-y") 'popup-kill-ring)
  (define-key popup-kill-ring-keymap (kbd "TAB") 'popup-kill-ring-next)
  (define-key popup-kill-ring-keymap (kbd "M-y") 'popup-kill-ring-next))

;; image+ / git clone https://github.com/mhayashi1120/Emacs-imagex
(when (and (executable-find "convert")
	   (require 'image+ nil t))
  (imagex-auto-adjust-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 追加関数定義

(when window-system
  (global-unset-key (kbd "<mouse-3>"))
  (global-set-key (kbd "<down-mouse-3>")
		  (lambda (event &optional prefix)
		    "編集メニューを表示する."
		    (interactive "@e")
		    (popup-menu menu-bar-edit-menu event prefix)))

  (defun ini:close-or-exit-emacs (&optional arg)
    "フレームが一つなら emacs を終了、それ以外はフレームを閉じる.
ARG が non-nil の場合はフレームの数に関係なく emasc を終了する."
    (interactive "P")
    (if (or arg (eq 1 (length (frame-list))))
	(save-buffers-kill-terminal)
      (delete-frame)))

  (global-set-key [remap save-buffers-kill-terminal] 'ini:close-or-exit-emacs)

  (defun ini:frame-size-maximize (&optional fullboth)
    "フレームサイズを最大化.
FULLBOTH が non-nil ならフルスクリーン表示にする."
    (interactive "P")
    (set-frame-parameter nil 'fullscreen
			 (if fullboth 'fullboth 'maximized))
    (when (fboundp 'w32-send-sys-command)
      (w32-send-sys-command #xf030)))

  (defun ini:frame-size-restore ()
    "フレームサイズを通常に戻す."
    (interactive)
    (set-frame-parameter nil 'fullscreen nil)
    (when (fboundp 'w32-send-sys-command)
      (w32-send-sys-command #xf120)))

  (defun ini:toggle-frame-size-maxmized (&optional fullboth)
    "フレームの最大化状態を切り替える.
FULLBOTH が non-nil なら最大化時にフルスクリーン表示にする."
    (interactive "P")
    (if (frame-parameter nil 'fullscreen)
	(ini:frame-size-restore)
      (ini:frame-size-maximize fullboth)))

  (global-set-key (kbd "C-z C-:") 'ini:toggle-frame-size-maxmized)
  (global-set-key (kbd "C-z C-;") 'iconify-or-deiconify-frame))

(defvar ini:scratch-save-file (locate-user-emacs-file "scratch")
  "*`*scratch' バッファの保存先ファイル名.")

(defvar ini:scratch-buffer-save-interval 1
  "*`*scratch*' バッファの自動保存間隔.")

(defvar ini:prev-scratch-modified-tick 0
  "`*scratch*' バッファの前回保存時の更新状態.")

(defun ini:refresh-scratch-buffer ()
  "`*scratch*' バッファを新規作成する.
既に存在している場合は内容をクリアする."
  (interactive)
  (let ((exists-p (get-buffer "*scratch*")))
    (with-current-buffer (get-buffer-create "*scratch*")
      (unless (eq major-mode initial-major-mode)
	(funcall initial-major-mode))
      (erase-buffer)
      (when (and initial-scratch-message
		 (not inhibit-startup-message))
	(insert initial-scratch-message))
      (set-buffer-modified-p nil))
    
    (if exists-p
	(message "*scratch* is cleaned up.")
      (message "another *scratch* is created."))
    )
  nil) ; for kill-buffer-query-functions

(defun ini:resume-scratch-buffer ()
  "`*scratch*' バッファの内容を復帰する."
  (interactive)
  (let ((scratch (get-buffer-create "*scratch*"))
	(file (expand-file-name ini:scratch-save-file))
	(buffer-undo-list t))
    (with-current-buffer scratch
      (when (file-exists-p file)
	(erase-buffer)
	(insert-file-contents file)
	(set-buffer-modified-p nil)

	(setq ini:scratch-modified-tick (buffer-chars-modified-tick))
	))))

(defun ini:save-scratch-buffer ()
  "`*scratch*' バッファの内容を保存する."
  (interactive)
  (ini:awhen (get-buffer "*scratch*")
    (with-current-buffer it
      (let ((modified-tick (buffer-chars-modified-tick)))
	(unless (eq modified-tick ini:prev-scratch-modified-tick)
	  (setq ini:prev-scratch-modified-tick modified-tick)
	  (save-restriction
	    (widen)
	    (write-region (point-min) (point-max)
			  (expand-file-name ini:scratch-save-file)
			  nil 'slient)
	    ))))))

(add-hook 'after-init-hook
	  (lambda ()
	    (ini:resume-scratch-buffer)
	    
	    ;; 読み込みに成功したら自動保存を有効化
	    (run-with-idle-timer ini:scratch-buffer-save-interval t 'ini:save-scratch-buffer)
	    (add-hook 'kill-emacs-hook 'ini:save-scratch-buffer)
	    ))

;; from http://www.fan.gr.jp/~ring/Meadow/meadow.html#ys:highlight-string
(defadvice insert-for-yank (after ini:ad-yank-highlight-string activate)
  "文字列ヤンク時にハイライト表示する."
  (let ((ol (make-overlay (mark t) (point))))
    (unwind-protect
	(progn (overlay-put ol 'face 'highlight)
	       (sit-for 0.5))
      (delete-overlay ol))))

(defadvice undo (after ini:ad-undo-highlight-string activate)
  "アンドゥで再挿入された文字列をハイライト表示する."
  (dolist (c buffer-undo-list)
    (let ((beg (car c))
	  (end (cdr c)))
      (cond ((and (integerp beg)
		  (integerp end))
	     (let ((ol (make-overlay beg end)))
	       (unwind-protect
		   (progn (overlay-put ol 'face 'highlight)
			  (sit-for 0.5))
		 (delete-overlay ol)
		 (return))))
	    ((stringp beg)
	     (return))))))

(defun ini:flip-window-state (&optional silent)
  "ウィンドウ分割状態を切り替える.
SILENT が non-nil の場合は切り替えメッセージを表示しない.
2状態固定."
  (interactive)
  (let* ((cur (current-window-configuration))
	 (state (frame-parameter nil 'ini:last-window-state))
	 (conf (car state))
	 (side (case (cdr state) (?A ?B) (?B ?A) (t ?B))))
    (if conf
	(set-window-configuration conf)
      (delete-other-windows))
    (unless silent
      (message "Flip to side \"%c\"." side))
    (set-frame-parameter nil 'ini:last-window-state (cons cur side))
    (force-mode-line-update)))

(global-set-key (kbd "C-z C-l") 'ini:flip-window-state)

;; フレームタイトルに状態を表示する
(setq frame-title-format
      (append (if (atom frame-title-format)
		  (list frame-title-format)
		frame-title-format)
	      '((:eval (ini:awhen (frame-parameter nil 'ini:last-window-state)
			 (format " [%c]" (cdr it)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 色設定
(flet ((color-candidate (&rest colors)
			(find-if #'color-defined-p colors)))
  (when (and (eq (frame-parameter nil 'background-mode) 'dark)
	     (string= (frame-parameter nil 'cursor-color) "black")
	     (set-cursor-color "white")))

  (set-face-attribute 'region nil
		      :foreground (color-candidate "SystemHilightText" "White")
		      :background (color-candidate "SystemHilight" "Royal Blue"))

  (set-face-attribute 'isearch nil
		      :foreground "#fff"
		      :background "#27c")
  
  (set-face-attribute 'lazy-highlight nil
		      :foreground "#fff"
		      :background "#aaa")
  
  (face-spec-reset-face 'match)
  (set-face-attribute 'match nil :inherit 'lazy-highlight)
  
  (eval-after-load "calendar"
    '(progn
       (when (facep 'calendar-weekend)
	 (set-face-attribute 'calendar-weekend nil
			     :foreground "red1"))
       ))

  (eval-after-load "cua-base"
    '(progn
       (face-spec-reset-face 'cua-rectangle)
       (set-face-attribute 'cua-rectangle nil :inherit 'region)
       (face-spec-reset-face 'cua-global-mark)
       (set-face-attribute 'cua-global-mark nil :inherit 'region
			   :weight 'bold)
       ))

  (eval-after-load "paren"
    '(progn
       (set-face-attribute 'show-paren-match nil
			   :weight 'bold
			   :foreground "#005"
			   :background "#ccf")
       (set-face-attribute 'show-paren-mismatch nil
			   :weight 'bold
			   :foreground "#700"
			   :background "#fcc")
       ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; カスタマイズファイル読み込み
(setq custom-file (locate-user-emacs-file "custom.el"))
(load (file-name-sans-extension custom-file) t t)

;; 作業ディレクトリをホームディレクトリに
(ini:awhen (getenv "HOME") (cd it))

;; (provide 'init)

;;; init.el ends here
