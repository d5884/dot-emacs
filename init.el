;;; init.el --- User initialize file. -*- coding: utf-8 -*-

;;; Commentary:
;;
;; ユーザ設定初期化ファイル.
;; Emacs 24.3 以降用
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; メールアドレス等
(setq user-full-name "Daisuke Kobayashi")
(setq user-mail-address "d5884jp@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GC 設定
(setq gc-cons-threshold (* 40 1024 1024))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; コンパイル時のワーニング抑制
(eval-when-compile
  (setq byte-compile-warnings '(not free-vars)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cl
(require 'cl-lib nil t)

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

(defmacro ini:system-file-name (name &optional directory)
  "NAME をシステムで認識可能なファイルパスに変換する.
`expand-file-name' により DIRECTORY を基準にして絶対パスに変換される.
環境変数などの Emacs 外のプログラムに参照される場合に用いる."
  (if (eq system-type 'windows-nt)
      `(subst-char-in-string ?/ ?\\ (expand-file-name ,name ,directory))
    `(expand-file-name ,name ,directory)))

(defmacro ini:concat-system-file-names (names &optional directory original)
  "NAMES をシステムで認識可能なファイルパスの連結に変換する.
NAMES の各要素は自身と DIRECTORY を引数に `ini:system-file-name' で処理される.
セパレータには `path-separator' が用いられる.
ORIGINAL が non-nil であれば最後に連結される."
  `(apply 'concat
	  (mapconcat (lambda (name) (ini:system-file-name name ,directory))
		     ,names path-separator)
	  (if ,original
	      (list path-separator ,original))))

(defmacro ini:emacs-d (file)
  "~/.emacs.d 以下の FILE を返す."
  `(eval-when-compile (locate-user-emacs-file ,file)))

(defmacro ini:locate-directory (directory)
  "DIRECTORY が存在するなら返す."
  `(locate-file "." (delq nil (list ,directory)) nil (lambda (p) (when (file-exists-p p) 'dir-ok))))

(defmacro ini:find-directory (directories)
  "DIRECTORIES のうち最初に見付かったディレクトリを返す."
  `(locate-file "." (delq nil (copy-sequence ,directories)) nil (lambda (p) (when (file-exists-p p) 'dir-ok))))

(defmacro ini:library-within (lib file &optional exists)
  "ライブラリ LIB と同じディレクトリに配置されている FILE 名を返す.
EXISTS が t の場合かつ FILE が存在しない場合は nil を返す.
LIB が存在しない場合は nil を返す."
  `(ini:awhen (locate-library ,lib)
     (let ((name (expand-file-name ,file (file-name-directory it))))
       (if (or (not ,exists)
	       (file-exists-p name))
	   name))))

(defmacro ini:make-silently-loading (func)
  "FUNC 内の `load' のメッセージ出力を強制的に抑制する."
  `(defadvice ,func (around
		     ,(intern (format "ini:make-silently-loading-in-%s" (quote func)))
		     activate)
     "`load' 時のメッセージを抑制する."
     (let ((org-load (symbol-function 'load)))
       (cl-letf (((symbol-function 'load)
		  (lambda (file &optional noerror nomessage nosuffix must-suffix)
		    (funcall org-load file noerror t nosuffix must-suffix))))
	 ad-do-it)
       )))

;; before emacs-24.4
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "FILE をロード後に BODY を評価する."
    `(eval-after-load ,file
       `(funcall ,(lambda () ,@body))))
  
  (put 'with-eval-after-load 'lisp-indent-function 1)
  (font-lock-add-keywords 'emacs-lisp-mode
			  '(("with-eval-after-load" . 'font-lock-keyword-face)))
  )

;; マクロのインデント設定
(put 'ini:aif 'lisp-indent-function 2)
(put 'ini:awhen 'lisp-indent-function 1)

(font-lock-add-keywords 'emacs-lisp-mode
			`((,(regexp-opt '("ini:aif" "ini:awhen"))
			   . 'font-lock-keyword-face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ロードパス追加

;; .emacs.d を init.el が置いてある場所にする
;; (setq user-emacs-directory (file-name-directory
;; 			    (or (buffer-file-name) load-file-name)))

;; (存在するなら) ~/.emacs.d/lisp および直下のディレクトリを load-path へ追加
;; データフォルダ等もあるので再帰的には追加しない
(setq load-path
      (append
       (ini:awhen (ini:locate-directory (ini:emacs-d "lisp"))
	 (cons it (cl-remove-if-not #'file-directory-p (directory-files it t "^[^.]"))))
       load-path))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package 初期化
(package-initialize)
(setq package-enable-at-startup nil)
(setq package-archives (append
			'(("melpa" . "http://melpa.milkbox.net/packages/"))
			package-archives))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 言語設定
(when (or (equal (getenv "LANG") "JPN") ;; windows default
	  (and (null (getenv "LANG")) (null (getenv "LC_ALL"))))
  (set-locale-environment (setenv "LANG" "ja_JP.UTF-8")))

;; coding-system の優先度設定
(set-coding-system-priority 'utf-8 'cp932) ; shift_jis より cp932 優先

;; 波ダッシュを全角チルダへ(Windowsスタイル) (U+301C > U+FF5E)
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
(when (display-graphic-p)
  (setq vertical-centering-font-regexp ".*")
  
  (cl-macrolet ((font-candidate (&rest font-list)
				(cl-find-if (lambda (f) (find-font (font-spec :name f)))
					    font-list)))
    (let ((fontset "fontset-standard")
	  (inhibit-redisplay t)
	  width1-charset-list)
      ;; ベースとなる ASCII フォント
      (ini:awhen (font-candidate "Consolas:pixelsize=15:weight=normal:slant=normal"
				 "DejaVu Sans Mono-11:weight=normal:slant=normal")
	
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
	    (set-fontset-font fontset charset it)
	    (push charset width1-charset-list)))
	
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

	;; 文字幅調整
	(when width1-charset-list
	  (let ((table (make-char-table nil)))
	    (dolist (charset width1-charset-list)
	      (map-charset-chars
	       (lambda (range _a)
		 (set-char-table-range table range 1))
	       charset))
	    (optimize-char-table table)
	    (set-char-table-parent table char-width-table)
	    (setq char-width-table table)))
	
	;; フレームに設定
	(add-to-list 'default-frame-alist (cons 'font fontset))
	)))

  ;; ウィンドウサイズ
  ;; w32-resume-frame / git clone https://github.com/d5884/w32-resume-frame
  (if (require 'w32-resume-frame nil t)
      (w32-resume-activate)
    (setq initial-frame-alist `((top . 60) (left . 120) ,@initial-frame-alist)))
  (setq default-frame-alist `((width . 100) (height . 40) ,@default-frame-alist))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cygwin 連携
(when (eq system-type 'windows-nt)
  ;; cygwin へのパス等が通されていない前提で emacs 内で諸々を設定する

  ;; パイプ間の待ち時間を減らす
  (setq w32-pipe-read-delay 5)

  ;; インストールルート検索
  (ini:awhen (ini:find-directory
	      (cons (getenv "CYGWIN_DIR")
		    (mapcar (lambda (p) (expand-file-name "cygwin" p))
			    (list (getenv "LOCALAPPDATA")
				  (getenv "APPDATA")
				  (getenv "USERPROFILE")
				  (getenv "HOME")
				  (getenv "ProgramW6432")
				  (getenv "ProgramFiles")
				  "c:/"
				  "c:/gnupack/app/cygwin"))))
    (let ((cygwin-exec-path ; cygwin のルートパスからの相対パスとして追加
	   (mapcar (lambda (path)
		     (expand-file-name (if (eq (aref path 0) ?/)
					   (substring path 1) path) it))
		   `(,(ini:emacs-d "bin") "~/bin" "/usr/local/bin" "/usr/bin" "/bin"))))
      (setenv "PATH" (ini:concat-system-file-names cygwin-exec-path nil (getenv "PATH")))
      (setq exec-path (append cygwin-exec-path exec-path))

      (setenv "CYGWIN" "nodosfilewarning winsymlinks")

      (setq null-device "/dev/null")

      ;; DOSコマンド混在のためプロセスでの出力のコードを未定に
      (setq default-process-coding-system
	    (cons (coding-system-change-text-conversion
		   (car default-process-coding-system) 'undecided)
		  (cdr default-process-coding-system)))

      ;; comint での出力コード自動判別設定 (undefined なだけだと判定後変更されてしまう)
      (defadvice comint-send-input (before ini:comint-send-detect-coding activate)
	"出力時の文字コードを自動判断に毎回戻す."
	(ini:awhen (get-buffer-process (current-buffer))
	  (set-process-coding-system it
				     (coding-system-change-text-conversion
				      (car default-process-coding-system) 'undecided)
				     (cdr (process-coding-system it)))))

      ;; ファイル名のエンコーディングを実態とあわせる
      (when (boundp 'w32-unicode-filenames)
	(set-file-name-coding-system 'utf-8))

      ;; shell
      (setq shell-file-name "bash")
      (setq shell-command-switch "-c")
      (setq system-uses-terminfo nil)

      (setenv "SHELL" shell-file-name)

      (with-eval-after-load "term"
	(require 'shell)
	(defadvice cd (around ini:cd-accept-multibyte activate)
	  "`term' で/proc等に移動時の強制終了を防ぐ."
	  (unless (ignore-errors ad-do-it)
	    (ad-set-arg 0 "~/")
	    ad-do-it))

	(defadvice term-emulate-terminal (around ini:terminal-detect-coding activate)
	  "`term' で複数のコーディング出力を受け付ける."
	  (let ((locale-coding-system 'undecided))
	    ad-do-it)))

      (with-eval-after-load "tramp"
	(setq tramp-encoding-shell "bash"))
      
      ;; gdb 使用時のエラー回避
      (with-eval-after-load "gdb-mi"
	(eval-when-compile
	  (declare-function gdb-input "gdb-mi"))
	(add-hook 'gdb-mode-hook
		  (lambda ()
		    (gdb-input "-gdb-set interactive-mode auto" 'ignore))))

      ;; cygwin で追加される Info
      (with-eval-after-load "info"
	(ini:awhen (ini:locate-directory "/usr/share/info")
	  (add-to-list 'Info-additional-directory-list it)))
      
      ;; cygwin-mount / (package-install 'cygwin-mount)
      (when (require 'cygwin-mount nil t)
      	(cygwin-mount-activate))

      ;; NTEmacs の場合、プロセスの引数は起動した環境のコードページに依存するため
      ;; プロセス呼び出し時に引数のみ cp932 へ強制変換する
      (dolist (pair '((call-process-region . 6)
      		      (call-process . 4)
      		      (start-process . 3)))
      	(let ((f (car pair))
      	      (p (cdr pair)))
      	  (eval `(defadvice ,f (before ,(intern (format "ini:%s-encode-setup" f))
      				       activate)
      		   ,(format "実行時に%d番目以降の引数を cp932 でエンコードする." p)
      		   (ad-set-args ,p
      				(mapcar (lambda (arg)
      					  (if (multibyte-string-p arg)
      					      (encode-coding-string arg 'cp932)
      					    arg))
      					(ad-get-args ,p)))))))

      (when (and (= emacs-major-version 24)
		 (= emacs-minor-version 4)) ;; 24.5 以降はどうなるかわからんので
	(defconst w32-pipe-limit 4096
	  "Windows でのパイプバッファサイズ.")

	(defadvice process-send-string (around ini:workaround-for-process-send-string activate)
	  "4096 バイト超を一度に送信すると cygwin の select が停止する問題への対処."
	  (if (not (eq (process-type (ad-get-arg 0)) 'real))
	      ad-do-it
	    (let* ((proc (get-process (or (ad-get-arg 0)
					  (get-buffer-process (current-buffer)))))
		   (rest (encode-coding-string (ad-get-arg 1)
					       (cdr (process-coding-system proc))))
		   (inhibit-eol-conversion t))
	      (while (> (length rest) w32-pipe-limit)
		(ad-set-arg 1 (substring rest 0 w32-pipe-limit))
		ad-do-it
		(setq rest (substring rest w32-pipe-limit)))
	      (ad-set-arg 1 rest)
	      ad-do-it
	      )))
	)
      ;; fakecygpty
      ;; gcc -o fakecygpty.exe fakecygpty.c
      ;; gcc -o qkill.exe qkill.c
      ;; git clone https://github.com/d5884/fakecygpty
      (when (require 'fakecygpty nil t)
	(fakecygpty-activate))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; パス追加
(add-to-list 'exec-path (expand-file-name (ini:emacs-d "bin")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; キーバインド変更

;; Ctrl-h を DEL に
(when (load "term/bobcat" nil t)
  (terminal-init-bobcat)
  ;; (setq help-char ?\C-?)
  )		; => C-h

;; ignore はヘルプ等に表示しない
(put 'ignore 'suppress-keymap t)

(global-set-key (kbd "C-x ?") 'help-command)
(global-set-key (kbd "C-x 7") 'toggle-truncate-lines)
(global-set-key (kbd "C-x C-z") 'compile)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "<pause>") 'toggle-debug-on-error)
(global-set-key [remap list-buffers] 'bs-show)
(global-set-key (kbd "C-z z") (defun ini:ansi-term ()
				(interactive)
				"`ansi-term' を実行する."
				(ansi-term shell-file-name)))
(global-set-key (kbd "<apps>") (defun ini:show-apps-menu ()
				 "編集メニューを表示する."
				 (interactive)
				 (popup-menu menu-bar-edit-menu)))

(when (display-mouse-p)
  ;; フレーム外/モードラインでのホイール回しでエラーを出さない
  (dolist (pos '("nil" "mode-line" "right-fringe" "left-fringe"))
    (global-set-key (kbd (format "<%s> <wheel-up>" pos)) 'ignore)
    (global-set-key (kbd (format "<%s> <wheel-down>" pos)) 'ignore))

  ;; ミニバッファでもホイール回しでエラーを出さない
  (add-hook 'minibuffer-setup-hook
	    (lambda ()
	      (local-set-key (kbd "<wheel-up>") 'ignore)
	      (local-set-key (kbd "<wheel-down>") 'ignore)))
  (define-key minibuffer-inactive-mode-map (kbd "<wheel-up>") 'ignore)
  (define-key minibuffer-inactive-mode-map (kbd "<wheel-down>") 'ignore)
  
  ;; ホイールクリックで貼り付けは使わない
  (dolist (key '("<mouse-2>" "<down-mouse-2>"
		 "<left-fringe> <mouse-2>" "<right-fringe> <mouse-2>"))
    (global-unset-key (kbd key)))
  (with-eval-after-load "ffap"
    (global-set-key (kbd "<mouse-2>") 'ffap-at-mouse))
  
  ;; 右クリックは編集メニュー表示
  (dolist (key '("<mouse-3>" "<down-mouse-3>" "<drag-mouse-3>"))
    (global-unset-key (kbd key)))
  (when (display-popup-menus-p)
    (global-set-key (kbd "<mouse-3>")
		    (defun ini:show-edit-menu (event &optional prefix)
		      "編集メニューを表示する."
		      (interactive "@e")
		      (popup-menu menu-bar-edit-menu event prefix))))

  ;; Ctrl+Wheel で文字サイズ変更
  (when (display-multi-font-p)
    (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
    (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
    (global-unset-key (kbd "<C-down-mouse-2>"))
    (global-set-key (kbd "<C-mouse-2>") (defun ini:text-scale-reset ()
					  "テキストのスケーリングをリセットする."
					  (interactive)
					  (text-scale-set 0)))
    ))

(when (display-multi-frame-p)
  (defun ini:close-or-exit-emacs (&optional arg)
    "フレームが一つなら emacs を終了、それ以外はフレームを閉じる.
ARG が non-nil の場合はフレームの数に関係なく emacs を終了する."
    (interactive "P")
    (if (or arg (eq 1 (length (frame-list))))
	(save-buffers-kill-terminal)
      (delete-frame)))

  (global-set-key [remap save-buffers-kill-terminal] 'ini:close-or-exit-emacs)

  (unless (fboundp 'toggle-frame-maximized)
    (defun toggle-frame-maximized ()
      "フレームサイズの最大化状態を切り替える."
      (if (frame-parameter nil 'fullscreen)
	  (progn
	    (and (fboundp 'w32-send-sys-command)
		 (w32-send-sys-command #xf030))
	    (set-frame-parameter nil 'fullscreen 'maximized))
	(and (fboundp 'w32-send-sys-command)
	     (w32-send-sys-command #xf120))
	(set-frame-parameter nil 'fullscreen nil))))

  (global-set-key (kbd "C-z C-:") 'toggle-frame-maximized)
  (global-set-key (kbd "C-z C-;") 'iconify-or-deiconify-frame))

;; IME関連キーの無効化
(global-set-key (kbd "<enlw>") 'toggle-input-method) ; 半角/全角
(global-set-key (kbd "<auto>") 'toggle-input-method) ; 半角/全角
(global-set-key (kbd "<M-kanji>") 'ignore)           ; Alt+半角/全角
(global-set-key (kbd "<convert>") 'ignore)           ; 無変換
(global-set-key (kbd "<non-convert>") 'ignore)       ; 変換
(global-set-key (kbd "<copy>") 'ignore)	             ; カタカナ/ひらがな/ローマ字

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
(setq recenter-positions '(middle top))
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
(setq auto-save-list-file-prefix nil)

;; 起動時のエコーエリアメッセージを黙らせる
;; inhibit-startup-echo-area-message はユーザ名をリテラルで書く必要があるので
;; Daemon 起動時以外は表示関数を直接潰す
(defadvice display-startup-echo-area-message (around ini:shut-up-echo-message activate)
  "起動時のエコーエリアのメッセージを表示しない."
  (if (daemonp) ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; パッケージ別機能設定

;; diminish / (package-install 'diminish)
(unless (package-installed-p 'diminish)
  (defalias 'diminish 'ignore))

(tool-bar-mode -1)
(menu-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
;; (set-scroll-bar-mode 'right)
(blink-cursor-mode -1)
(column-number-mode t)
(show-paren-mode t)
(delete-selection-mode t)
(temp-buffer-resize-mode t)
(mouse-avoidance-mode 'exile)

;; (put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; yank/undo highlighting
;;   from http://www.fan.gr.jp/~ring/Meadow/meadow.html#ys:highlight-string
(defadvice insert-for-yank (after ini:yank-highlight-string activate)
  "文字列ヤンク時にハイライト表示する."
  (let ((ol (make-overlay (mark t) (point))))
    (unwind-protect
	(progn (overlay-put ol 'face 'highlight)
	       (sit-for 0.5))
      (delete-overlay ol))))

(defadvice undo (after ini:undo-highlight-string activate)
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

;; outline-minor-mode
(add-hook 'outline-minor-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-o") outline-mode-prefix-map)))

;; transient-mark-mode
(defadvice exchange-point-and-mark (after ini:exchange-point-and-mark-deactivate activate)
  "Function `transient-mark-mode' が有効な時にリージョンに色が付かないようにする."
  (if (and (transient-mark-mode mark-active))
      (deactivate-mark)))

;; completion
(setq completion-show-help nil)
(define-key completion-list-mode-map (kbd "<tab>") 'next-completion)
(define-key completion-list-mode-map (kbd "S-<tab>") 'previous-completion)
(define-key completion-list-mode-map (kbd "n") 'next-completion)
(define-key completion-list-mode-map (kbd "p") 'previous-completion)

;; compile
(with-eval-after-load "compile"
  (setq compilation-scroll-output t)
  (define-key compilation-mode-map (kbd "n") 'next-error)
  (define-key compilation-mode-map (kbd "p") 'previous-error)

  ;; (add-hook 'compilation-mode-hook 'next-error-follow-minor-mode)
  
  (dolist (func '(compile recompile))
    (eval `(defadvice ,func (around ,(intern (format "ini:%s-silently" func)) activate)
	     "エラー発生時のみ *compilation* バッファ表示."
	     (cl-letf (((symbol-function 'display-buffer) 'ignore)
		       ((symbol-function 'set-window-start) 'ignore)
		       ((symbol-function 'set-window-point) 'ignore))
	       (let (compilation-start-hook) 	; local hook ではなく compile のみの hook
		 (add-hook 'compilation-start-hook
			   (lambda (proc)
			     (add-hook 'compilation-finish-functions
				       (lambda (buffer msg)
					 (with-current-buffer buffer
					   (font-lock-mode -1)
					   (font-lock-fontify-buffer)
					   (font-lock-mode 1)
					   (if (or (not (string-match "finished" msg))
						   (text-property-not-all
						    (point-min) (point-max)
						    'compilation-message nil))
					       (display-buffer buffer)
					     (when (get-buffer-window buffer)
					       (delete-window (get-buffer-window buffer))))))
				       nil t)))
		 ad-do-it))))
    )
  )


;; gnutls
(with-eval-after-load "gnutls"
  (setq gnutls-trustfiles (mapcar 'expand-file-name gnutls-trustfiles)))

;; recentf
(with-eval-after-load "recentf" ;; 基本的に使わないがファイルをホームに作らないよう設定
  (setq recentf-save-file (ini:emacs-d "recentf")))

;; info
(with-eval-after-load "info"
  (ini:awhen (ini:locate-directory (ini:emacs-d "info"))
    (add-to-list 'Info-additional-directory-list it)))

;; ispell
(with-eval-after-load "ispell"
  ;; from http://www.an.econ.kobe-u.ac.jp/~namba/meadow/words.lzh
  (ini:awhen (locate-file "words"
			  `(,(ini:emacs-d "share")
			    ,user-emacs-directory
			    "/usr/dict"
			    "/usr/share/dict"))
    (setq ispell-alternate-dictionary it)))

;; windmove
(global-set-key (kbd "C-z C-n") 'windmove-down)
(global-set-key (kbd "C-z C-p") 'windmove-up)
(global-set-key (kbd "C-z C-f") 'windmove-right)
(global-set-key (kbd "C-z C-b") 'windmove-left)

;; man & woman
(with-eval-after-load "woman"
  (setq woman-fill-frame t
	woman-cache-filename (ini:emacs-d "woman-cache")))

;; bookmark
(with-eval-after-load "bookmark"
  (setq bookmark-default-file (ini:emacs-d "bookmark")))

;; imenu
(global-set-key (kbd "C-z C-j") 'imenu)
(with-eval-after-load "imenu"
  (setq imenu-auto-rescan t))

;; shell and term utility
(defun ini:add-process-sentinel (process sentinel)
  "PROCESS に センチネル SENTINEL を追加する.
SENTINEL は元々設定されていたセンチネルが実行されてから呼び出される."
  (let ((org-sentinel (and (processp process)
			   (process-sentinel process))))
    (set-process-sentinel process
			  (if org-sentinel
			      `(lambda (proc msg)
				 (funcall (function ,org-sentinel) proc msg)
				 (funcall (function ,sentinel) proc msg))
			    sentinel))
    ))

(defun ini:set-process-cleaner (&optional process)
  "PROCESS 終了時にバッファとウィンドウを削除する.
また Emacs 終了時にプロセスも終了させる.
PROCESS が nil の場合はカレントバッファのプロセスに設定する."
  (ini:awhen (or (and (processp process)
		      process)
		 (get-buffer-process (current-buffer)))
    (set-process-query-on-exit-flag it nil)
    ;; PROCESS のバッファを削除し、ウィンドウが開いていたら閉じる
    (ini:add-process-sentinel it
			      (lambda (process event)
				(let ((buf (process-buffer process)))
				  (dolist (win (get-buffer-window-list buf))
				    (unless (one-window-p)
				      (delete-window win)))
				  (kill-buffer buf))))
    ))

;; shell
(with-eval-after-load "shell"
  (setq comint-prompt-read-only t)

  (add-hook 'shell-mode-hook 'ini:set-process-cleaner)
  (define-key shell-mode-map (kbd "M-p") 'comint-previous-matching-input-from-input)
  (define-key shell-mode-map (kbd "M-n") 'comint-next-matching-input-from-input))

;; term
(with-eval-after-load "term"
  (add-hook 'term-exec-hook 'ini:set-process-cleaner)

  ;; C-c に C-x を取り込まない
  (set-keymap-parent term-raw-escape-map nil)
  ;; char-mode で使いたいキーを開放して C-c に移動
  (dolist (key '("M-x" "M-:" "C-z" "C-u"))
    (define-key term-raw-map (kbd key) nil)
    (define-key term-raw-map (kbd (concat "C-c " key))
      (if (string-match-p "^M-" key)
	  'term-send-raw-meta
	'term-send-raw)))
  
  (define-key term-mode-map (kbd "C-c C-w") nil)
  )

;; ansi-color
(with-eval-after-load "comint"
  (require 'ansi-color nil t)
  (when (eq 'light (frame-parameter nil 'background-mode))
    (setq ansi-color-names-vector
	  ["black"		; black
	   "dark red"		; red
	   "dark gree"       	; green
	   "dark goldenrod"	; yellow
	   "dark blue"	; blue
	   "dark magenta"     ; magenta
	   "dark cyan"	; cyan
	   "white"])		; white
    (setq ansi-color-map (ansi-color-make-color-map)))
  (ansi-color-for-comint-mode-on))

;; apropos
(with-eval-after-load "apropos"
  (setq apropos-do-all t)
  (define-key apropos-mode-map (kbd "n") 'forward-button)
  (define-key apropos-mode-map (kbd "p") 'backward-button))

;; cua-mode
(when (require 'cua-base nil t)
  (cua-selection-mode t))

;; ido
(when (require 'ido nil t)
  (setq ido-default-buffer-method 'selected-window)
  (setq ido-save-directory-list-file nil)
  (setcar (nthcdr 2 ido-decorations) ",")
  (setcar (nthcdr 3 ido-decorations) ", ...")
  (ido-mode 'buffer)

  (add-hook 'ido-minibuffer-setup-hook
	    (lambda ()
	      ;; disable some keys
	      (dolist (key '("C-f" "C-b" "C-d" "C-x C-f" "C-x C-d"))
		(define-key ido-buffer-completion-map (kbd key) nil))))

  (defadvice ido-exhibit (after ini:ido-exhibit-display-buffer activate)
    "選択しているバッファをウィンドウに表示する."
    (when ido-matches
      (let ((selected (get-buffer-window
		       (cl-find-if-not #'minibufferp (buffer-list)))))
	(when selected
	  (select-window selected)
	  (ido-visit-buffer
	   (get-buffer (car ido-matches)) t)
	  (select-window (minibuffer-window))))
      ))
  )

;; grep
(with-eval-after-load "grep"
  (eval-when-compile
    (declare-function grep-apply-setting "grep"))

  (when (executable-find "lgrep")
    ;; lv 付属の多国語化 grep
    (setq grep-program "lgrep")
    (grep-apply-setting 'grep-command "lgrep -n -Au8 -Ia ")
    (grep-apply-setting 'grep-template
			(format "%s <C> <R> - <F>" grep-command))
    (grep-apply-setting 'grep-find-template 
			(format "find . <X> -type f <F> -exec %s <C> <R> - <N> {} +"
				grep-command))
    )
  )

;; バッファ名のユニーク化
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

;; サーバ機能
(when (and (require 'server nil t)
	   (memq (server-running-p) '(nil :other)))
  (server-start))

;; スクリプトファイルの自動 +x
(when (fboundp 'executable-make-buffer-file-executable-if-script-p)
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

;; ssh-agent / git clone https://github.com/d5884/ssh-agent
(when (locate-library "ssh-agent")
  (autoload 'ssh-agent-add-key "ssh-agent" nil t)

  (with-eval-after-load "magit"
    (defadvice magit-push-dwim (before ini:ssh-agent-with-magit-push activate)
      (ssh-agent-add-key))

    (defadvice magit-fetch (before ini:ssh-agent-with-magit-fetch activate)
      (ssh-agent-add-key)))

  (with-eval-after-load "tramp-sh"
    (defadvice tramp-send-command (before ini:ssh-agent-with-tramp activate)
      (ssh-agent-add-key)))
  )

;; Tramp
(with-eval-after-load "tramp"
  (setq tramp-default-method "ssh")
  (setq tramp-verbose 2))

(with-eval-after-load "tramp-sh"
  (let ((process-environment tramp-remote-process-environment))
    (setenv "LC_ALL" nil)		; リモートのロケールは接続先に準じる
    (setq tramp-remote-process-environment process-environment)))

;; ffap
(when (require 'ffap nil t)
  (ffap-bindings))

;; hideshow
(with-eval-after-load "hideshow"
  (diminish 'hs-minor-mode)

  (defvar ini:hs-fringe-mark 'right-arrow
    "隠れた行の fringe に表示する bitmap 名.
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
  (define-key hs-minor-mode-map (kbd "C-c <C-SPC>") 'hs-toggle-hiding))

;; gdb
(with-eval-after-load "gdb-mi"
  (eval-when-compile
    (declare-function gdb-many-windows "gdb-mi"))
  (add-hook 'gdb-mode-hook (lambda ()
			     (gdb-many-windows)
			     (gud-tooltip-mode))))

;; ediff
(with-eval-after-load "ediff"
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)

  (defadvice ediff-find-file (around ini:ediff-mark-newly-opened activate)
    "ediff が開いたファイルを quit 時に削除できるようフラグを付ける."
    (let ((existing-p (and find-file-existing-other-name
			   (find-buffer-visiting (symbol-value (ad-get-arg 0))))))
      ad-do-it
      (unless existing-p
	(ediff-with-current-buffer (symbol-value (ad-get-arg 1))
	  (setq-local ini:ediff-kill-on-quit t)))))

  (eval-when-compile (require 'ediff nil t))
  (defvar ini:ediff-window-configuration-stash nil
    "`ediff' 実行前のウィンドウ状態の一時保存先.")

  (add-hook 'ediff-before-setup-hook
	    (lambda ()
	      (setq ini:ediff-window-configuration-stash
		    (current-window-configuration))))
  (add-hook 'ediff-quit-hook
	    (lambda ()
	      (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-ancestor-buffer))
	      	(ediff-with-current-buffer buf
	      	  (when (and (boundp 'ini:ediff-kill-on-quit)
			     ini:ediff-kill-on-quit)
		    (kill-buffer))))
	      (set-window-configuration ini:ediff-window-configuration-stash)))
  )

;; magit / (package-install 'magit)
(when (package-installed-p 'magit)
  (global-set-key (kbd "C-z C-m") 'magit-status)

  (with-eval-after-load "session"
    (setq session-set-file-name-exclude-regexp
  	  (concat session-set-file-name-exclude-regexp
  		  "\\|" (regexp-opt '("COMMIT_EDITMSG"))))
    ))

;; flymake
(autoload 'flymake-find-file-hook "flymake" nil t)
(add-hook 'find-file-hook 'flymake-find-file-hook)

(defmacro ini:flymake-gen-simple-init (type fmask command &rest options)
  "`flymake-mode' で使う、TYPE 用の文法チェック関数を定義する.
チェック関数は `flymake-TYPE-init' の形で定義される.
`fmask' にマッチするファイルに対して適用される.
チェック用のコマンドを COMMAND で、引数を OPTIONS で指定する.
OPTIONS ではチェック用の一時ファイル名を `local-file' で参照できる.
COMMAND が存在しない場合は定義を行なわない."
  (when (executable-find command)
    `(progn
       (defun ,(intern (format "flymake-%s-init" type)) ()
	 (let* ((temp-file (flymake-init-create-temp-buffer-copy
			    'flymake-create-temp-inplace))
		(local-dir (file-name-directory buffer-file-name))
		(local-file (file-relative-name temp-file local-dir)))
	   (list ,command (list ,@options))))
       (push (list ,fmask ',(intern (format "flymake-%s-init" type)))
	     flymake-allowed-file-name-masks)))
  )

(with-eval-after-load "flymake"
  (setq flymake-start-syntax-check-on-newline nil)
  (setq flymake-gui-warnings-enabled nil)
  (unless (boundp 'flymake-warning-predicate)
    (defvaralias 'flymake-warning-predicate 'flymake-warning-re))

  ;; (defadvice flymake-post-syntax-check (before ini:flymake-force-interrupted-flag activate)
  ;; 	 "`flymake-mode' でチェックが異常終了時に固まるのを防ぐ."
  ;; 	 (setq flymake-check-was-interrupted t))
  )

;; elisps
(defun ini:byte-compile-current-file-if-necessary ()
  "開いているファイルをバイトコンパイルする.
既にコンパイル済みのファイルがあり、ソースファイルの方が新しい場合のみコンパイルする."
  (interactive)
  (require 'bytecomp)
  (let* ((file (buffer-file-name))
	 (dest (byte-compile-dest-file file)))
    (when (and (file-exists-p dest)
	       (file-writable-p dest)
	       (file-newer-than-file-p file dest))
      (byte-compile-file file))))

(with-eval-after-load "lisp-mode"
  (define-key lisp-interaction-mode-map (kbd "C-m") 'newline-and-indent)
  (define-key emacs-lisp-mode-map (kbd "C-m") 'newline-and-indent)
  (define-key emacs-lisp-mode-map [remap compile] 'emacs-lisp-byte-compile)
  
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (hs-minor-mode t)
	      (eldoc-mode t)
	      (add-hook 'after-save-hook
			'ini:byte-compile-current-file-if-necessary
			nil t)
	      ))
  )

;; eldoc
(with-eval-after-load "eldoc"
  (diminish 'eldoc-mode))

;; occur
(define-key occur-mode-map (kbd "n") 'occur-next)
(define-key occur-mode-map (kbd "p") 'occur-prev)
(add-hook 'occur-mode-hook 'next-error-follow-minor-mode)

;; view-mode
(with-eval-after-load "view"
  (define-key view-mode-map "j" 'next-line)
  (define-key view-mode-map "k" 'previous-line)
  ;; (defadvice view-mode-exit (before ini:view-mode-exit activate)
  ;;   "`hl-line-mode' を無効にする."
  ;;   (when view-mode
  ;;     (hl-line-mode -1)))
  ;; (add-hook 'view-mode-hook (lambda () (hl-line-mode t)))
  )

;; dired
(global-set-key (kbd "C-x C-d") 'dired-other-window)


(with-eval-after-load "dired"
  (eval-when-compile
    (declare-function dired-get-file-for-visit "dired"))
  
  (setq dired-dwim-target t)
  (setq dired-isearch-filenames t)
  (setq dired-listing-switches "-lAF")
  (if (eq system-type 'windows-nt)
      (with-eval-after-load "ls-lisp"
	(setq ls-lisp-dirs-first t)
	(setq ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M"))
	(setq ls-lisp-use-localized-time-format t))
    (setq dired-listing-switches (concat "--time-style=long-iso "
					 "--group-directories-first "
					 dired-listing-switches)))

  (add-hook 'dired-mode-hook
	    (lambda ()
	      ;; dired 上でのみゴミ箱使用
	      (toggle-truncate-lines t)
	      (setq-local delete-by-moving-to-trash t)
	      (hl-line-mode t)))

  (when (eq system-type 'windows-nt)
    (define-key dired-mode-map (kbd "E")
      (defun ini:dired-execute ()
	"ファイルを関連付けされたプログラムで開く."
	(interactive)
	(w32-shell-execute nil (dired-get-file-for-visit)))))

  (define-key dired-mode-map (kbd "v")
    (defun ini:dired-view-other-window ()
      "別ウィンドウでファイルを閲覧する."
      (interactive)
      (view-file-other-window (dired-get-file-for-visit))))

  (define-key dired-mode-map (kbd "q") 'kill-this-buffer)
  
  ;; dired のバッファを無駄に増やさないため、移動時に移動前のバッファを消す
  (dolist (f '(dired-find-file dired-up-directory))
    (eval `(defadvice ,f (around ,(intern
				   (format "ini:%s-and-kill" f))
				 activate)
	     "移動前のディレクトリバッファ削除およびソート順序保持."
	     (let ((prev-buffer (current-buffer))
		   (switch dired-actual-switches))
	       ad-do-it
	       (if (and (not (eq prev-buffer (current-buffer)))
			(not (string= (buffer-name prev-buffer) "*Find*"))
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
  (with-eval-after-load "find-dired"
    (setq find-ls-option (cons (format "-exec ls %sd --time-style=long-iso {} +"
				       dired-listing-switches)
			       (format "%sd" dired-listing-switches)))

    (when (require 'grep nil t)
      (defadvice find-grep-dired (around ini:find-grep-replace activate)
	"lgrep がちゃんと動かないので普通の grep に置き換え."
	(let ((grep-program "grep"))
	  ad-do-it)))
    )

  ;; dired-x は導入するが C-x C-j は skk 等で使用
  (let ((cxcj (key-binding (kbd "C-x C-j"))))
    (when (require 'dired-x nil t)
      (global-set-key (kbd "C-x C-j") cxcj)))
  )


;; ;; flyspell
;; (when (and (or (executable-find "ispell")
;; 	       (executable-find "aspell"))
;; 	   (fboundp 'flyspell-prog-mode))
;;   (with-eval-after-load "flyspell"
;;        (custom-set-variables '(flyspell-use-meta-tab nil)))
;;   (add-hook 'prog-mode-hook
;; 	    'flyspell-prog-mode))

;; cc-mode
(with-eval-after-load "cc-mode"
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (hs-minor-mode t)
	      (hide-ifdef-mode t)
	      (setq-local flymake-warning-predicate "^[Ww]arning\\|警告")))
  
  (when (require 'flymake nil t)
    ;; c
    (ini:flymake-gen-simple-init cc "\\.c$"
				 "gcc" "-Wall" "-fsyntax-only" local-file)
    ;; c++
    (ini:flymake-gen-simple-init c++ "\\.cpp\\|\\.CC"
				 "g++" "-Wall" "-fsyntax-only" local-file)
    )
  )

;; ruby-mode
;; rubydb, etc... / (package-install 'ruby-additional)
;; inf-ruby / (package-install 'inf-ruby)
(with-eval-after-load "ruby-mode"
  (when (require 'ruby-electric nil t)
    (add-hook 'ruby-mode-hook 'ruby-electric-mode))
  
  (when (and (require 'yasnippet nil t)
	     (require 'auto-complete nil t))
    (defun ac-ruby-mode-setup ()
      (setq ac-sources (cons 'ac-source-yasnippet ac-sources))))
  
  (when (require 'hideshow nil t)
    (add-to-list 'hs-special-modes-alist
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
  
  (when (require 'flymake nil t)
    (ini:flymake-gen-simple-init ruby "\\.rb$" "ruby" "-c" local-file))
  )

(when (executable-find "ruby")
  ;; rubydb
  (when (locate-library "rubydb3x")
    (autoload 'rubydb "rubydb3x" nil t))
  
  ;; inf-ruby
  (when (locate-library "inf-ruby")
    (autoload 'run-ruby "inf-ruby" nil t)
    (autoload 'inf-ruby-keys "inf-ruby" nil t)
    (with-eval-after-load "inf-ruby"
      ;; inf-ruby-completion
      ;;   from http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-core/20892
      ;; ... or git clone git://gist.github.com/1543904.git inf-ruby-completion
      (require 'inf-ruby-completion nil t))
    (with-eval-after-load "ruby-mode"
      (inf-ruby-keys))
    ))
;; zencoding / (package-install 'zencoding-mode)

;; ;; w32-symlinks
;; (when (and (eq system-type 'windows-nt)
;; 	   (require 'w32-symlinks nil t))
;;   (custom-set-variables '(w32-symlinks-handle-shortcuts t))
;;   (defadvice w32-symlinks-parse-shortcut (around emacs23-fix activate)
;;     (let ((default-enable-multibyte-characters nil))
;;       ad-do-it))
;;   )

(when (locate-library "zencoding-mode")
  (autoload 'zencoding-mode "zencoding-mode" nil t)
  
  (with-eval-after-load "zencoding-mode"
    (define-key zencoding-mode-keymap (kbd "<C-return>") nil)
    (define-key zencoding-mode-keymap (kbd "C-j") nil)
    (define-key zencoding-mode-keymap (kbd "C-M-/") 'zencoding-expand-line)
    (when (require 'yasnippet nil t)
      (define-key zencoding-mode-keymap [remap zencoding-expand-line] 'zencoding-expand-yas))
    (setq zencoding-indentation 2)
    )
  
  (with-eval-after-load "nxml-mode"
    (add-hook 'nxml-mode-hook 'zencoding-mode))

  (with-eval-after-load "css-mode"
    (add-hook 'css-mode-hook 'zencoding-mode))
  )

(when (locate-library "smart-compile")
  (autoload 'smart-compile "smart-compile" nil t)
;; smart-compile / (package-install 'smart-compile)
  (autoload 'recompile "compile" nil t)

  (defun ini:smart-recompile (arg)
    "一度目は `smart-compile'、二度目は `recompile' を呼び出す.
ARG が non-nil の場合は `smart-compile' を呼び出す."
    (interactive "P")
    (if (or (not (local-variable-p 'compile-command))
	    arg)
	(smart-compile 4)
      (recompile)))
  
  (global-set-key [remap compile] 'ini:smart-recompile)
  )

;; nxml-mode
(fset 'html-mode 'nxml-mode)
(fset 'xml-mode 'nxml-mode)

(setq magic-mode-alist
      (cons '("<\\?xml" . nxml-mode) magic-mode-alist))
(with-eval-after-load "smart-compile"
  (setq smart-compile-alist (cons
			     '(nxml-mode browse-url-of-buffer)
			     smart-compile-alist)))


;; flex-autopair / (package-install 'flex-autopair)
(if (require 'flex-autopair nil t)
    (progn
      (diminish 'flex-autopair-mode)
      (setq flex-autopair-echo-actionp nil)
      (flex-autopair-mode))
  ;; or skeleton
  (when (require 'skeleton nil t)
    (setq skeleton-pair t)
    (global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
    (global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
    (global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
    ;; (global-set-key (kbd "`") 'skeleton-pair-insert-maybe)
    (global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
    ))

;; gswin32
;;  http://www.khotta.org/ghost/index.html
;;  http://w32tex.org/index-ja.html
(when (eq system-type 'windows-nt)
  (ini:awhen (ini:aif (executable-find "gswin32c")
		 (expand-file-name ".." (file-name-directory it))
	       (ini:find-directory '("c:/gs" "c:/gnupack/app/gs")))
      (defvar gswin-command (expand-file-name "bin/gswin32c" it)
	"ghostscript の実行プログラム.")

      (unless (getenv "GS_LIB")
	(setenv "GS_LIB" (ini:concat-system-file-names '("lib" "kanji" "Resource/Init") it)))
      (unless (getenv "GS_DLL")
	(setenv "GS_DLL" (ini:system-file-name "bin/gsdll32.dll" it)))
      (setenv "PATH" (ini:concat-system-file-names '("bin" "lib") it (getenv "PATH")))

      ;; lpr
      (with-eval-after-load "lpr"
	(setq printer-name nil))
      
      ;; gs
      (with-eval-after-load "gs"
	(setq gs-command gswin-command
	      gs-device "display"))
      
      ;; ps-print-buffer
      (with-eval-after-load "ps-print"
	(setq ps-print-color-p t
	      ps-lpr-command gswin-command
	      ps-lpr-switches '("-sDEVICE=mswinpr2" "-dNOPAUSE" "-dBATCH" "-dWINKANJI")
	      ps-multibyte-buffer 'non-latin-printer
	      ps-printer-name nil
	      ps-printer-name-option nil))

      ;; doc-view
      (with-eval-after-load "doc-view"
	(setq doc-view-ghostscript-program gswin-command)
	(push "-dWINKANJI" doc-view-ghostscript-options)
	)
      ))

;; calendar
(with-eval-after-load "calendar"
  ;; solor / geocode from http://api.knecht.jp/geocoding
  (setq calendar-latitude 35.6894875)
  (setq calendar-longitude 139.6917064)
  (setq calendar-location-name "Tokyo, JP")
  (setq calendar-time-display-form '((format "%2s:%2s%s" 12-hours minutes am-pm)))
  (setq calendar-date-display-form '((format "%2s/%2s/%2s" year month day)))
  (setq calendar-mark-holidays-flag t)
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)

  (setq diary-file (ini:emacs-d "diary"))
  (setq diary-entry-marker 'link)
  (setq diary-list-include-blanks t)
  (setq calendar-mark-diary-entries-flag t)

  (define-key calendar-mode-map (kbd "n") 'calendar-forward-week)
  (define-key calendar-mode-map (kbd "p") 'calendar-backward-week)
  (define-key calendar-mode-map (kbd "f") 'calendar-forward-day)
  (define-key calendar-mode-map (kbd "b") 'calendar-backward-day)
  (define-key calendar-mode-map (kbd "C-j") 'diary-view-entries)
  (define-key calendar-mode-map (kbd "<RET>") 'diary-view-entries)
  
  ;; 月の満ち欠けの日本語化
  (with-eval-after-load "lunar"
    (setq lunar-phase-names '("新月" "上弦" "満月""下弦")))

  ;; 日本の祝日表示 / (package-install 'japanese-holidays)
  (when (require 'japanese-holidays nil t)
    (setq calendar-holidays
	  (append japanese-holidays holiday-local-holidays holiday-other-holidays))
    
    (add-hook 'today-visible-calendar-hook 'japanese-holiday-mark-weekend)
    (add-hook 'today-invisible-calendar-hook 'japanese-holiday-mark-weekend)
    )
  )

;; auth-source
(with-eval-after-load "auth-source"
  (setq auth-sources (cons (ini:emacs-d "authinfo.gpg") auth-sources))
  )

;; gnus and mail (for gmail)
(setq mail-user-agent 'gnus-user-agent)
(setq read-mail-command 'gnus)

(with-eval-after-load "gnus"
  (setq gnus-startup-file (ini:emacs-d "gnus/newsrc"))
  (setq gnus-directory (ini:emacs-d "gnus/news"))
  (setq gnus-save-newsrc-file nil)


  (setq gnus-select-method '(nnimap "gmail"
				    (nnimap-address "imap.gmail.com")
				    (nnimap-server-port 993)
				    (nnimap-stream ssl)))
  (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  )

(with-eval-after-load "message"
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq message-auto-save-directory nil)
  )

(with-eval-after-load "sendmail"
  (setq send-mail-function 'smtpmail-send-it)
  )

(with-eval-after-load "smtpmail"
  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-stream-type 'tls)
  (setq smtpmail-local-domain "gmail.com")
  )

;; eww
(with-eval-after-load "eww"
  (setq eww-search-prefix "http://www.google.co.jp/search?q=")

  (defadvice eww-display-html (after ini:eww-change-buffer-coding-system activate)
    (set-buffer-file-coding-system (ad-get-arg 0)))

  (defadvice eww-submit (around ini:eww-override-find-coding-systems-string activate)
    (cl-letf (((symbol-function 'find-coding-systems-string)
	       (lambda (string)
		 (list buffer-file-coding-system))))
      ad-do-it))
  )

(when (locate-library "markdown-mode")
  (autoload 'markdown-mode "markdown-mode" nil t)
;; markdown-mode / (package-install 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.\\(md\\(wn\\|t\\)?\\|markdown\\|text\\)\\'" .
				  markdown-mode)))

;; sdic / http://www.namazu.org/~tsuchiya/sdic/
(when (locate-library "sdic")
  (autoload 'sdic-describe-word "sdic" nil t)
  (autoload 'sdic-describe-word-at-point "sdic" nil t)
  (global-set-key (kbd "C-c w") 'sdic-describe-word)
  (global-set-key (kbd "C-c C-w") 'sdic-describe-word-at-point)

  (with-eval-after-load "sdic"
    (setq sdic-default-coding-system 'utf-8-unix)
    (setq sdic-eiwa-dictionary-list `((sdicf-client ,(ini:emacs-d "share/sdic/gene-u.sdic")
						    (strategy direct))))
    (setq sdic-waei-dictionary-list `((sdicf-client ,(ini:emacs-d "share/sdic/jedict-u.sdic")
						    (add-keys-to-headword t)
						    (strategy direct))))

    ;; speak / git clone https://github.com/d5884/speak
    (when (require 'speak nil t)
      ;; sdic-mode-map is defined in sdic-mode, not on loading.
      (add-hook 'sdic-mode-hook
		(lambda ()
		  (define-key sdic-mode-map (kbd "s") 'speak-line)
		  (define-key sdic-mode-map (kbd "S") 'speak))))

    ;; popwin 対応
    (when (require 'popwin nil t)
      (add-to-list 'popwin:special-display-config
		   `(,sdic-buffer-name :height ,sdic-window-height))

      ;; バッファ表示系統を popwin が認識可能なシンプルな操作に置き換える
      (defadvice sdic-other-window (around ini:sdic-other-normalize
					   activate)
	"`sdic' のバッファ移動を普通の操作にする."
	(other-window 1))

      (defadvice sdic-close-window (around ini:sdic-close-normalize
					   activate)
	"`sdic' のバッファクローズを普通の操作にする."
	(bury-buffer sdic-buffer-name))
      
      (defadvice sdic-display-buffer (around ini:sdic-display-normalize
					     activate)
	"`sdic' のバッファ表示を普通の操作にする."
	(setq ad-return-value (buffer-size))
	(let ((p (or (ad-get-arg 0)
		     (point))))
	  (and sdic-warning-hidden-entry
	       (> p (point-min))
	       (message "この前にもエントリがあります。"))
	  (unless (eq (window-buffer) (get-buffer sdic-buffer-name))
	    (display-buffer (get-buffer sdic-buffer-name)))
	  (set-window-start (get-buffer-window sdic-buffer-name) p)
	  (goto-char p)
	  ))
      )
    ))

(when (locate-library "shell-pop")
  (autoload 'shell-pop "shell-pop" nil t)
;; shell-pop / (package-install 'shell-pop)
  (global-set-key (kbd "C-z C-z") 'shell-pop)
  (with-eval-after-load "shell-pop"
    (setq shell-pop-internal-mode "ansi-term")
    (setq shell-pop-internal-mode-buffer "*ansi-term*")
    (setq shell-pop-internal-mode-func (lambda () (ansi-term shell-file-name)))
    (setq shell-pop-autocd-to-working-dir nil)

    (defadvice shell-pop-out (around ini:safe-pop-out activate)
      "戻り先の window が死んでいたら window を消すだけにする."
      (if (one-window-p)
	  (switch-to-buffer shell-pop-last-buffer)
	(if (window-live-p shell-pop-last-window)
	    ad-do-it
	  (delete-window))))
    ))

;; popwin / (package-install 'popwin)
(when (require 'popwin nil t)
  (global-set-key (kbd "C-z C-s") 'popwin:stick-popup-window)
  (setq popwin:special-display-config
	(append
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
	   (dired-mode :stick t))
	 popwin:special-display-config))
  (popwin-mode 1)
  )

(when (locate-library "stripe-buffer")
  (autoload 'turn-on-stripe-buffer-mode "stripe-buffer")
;; stripe-buffer / (package-install 'stripe-buffer)
  (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
  (add-hook 'tabulated-list-mode-hook 'turn-on-stripe-buffer-mode))

(when (locate-library "yascroll")
  (autoload 'yascroll:show-scroll-bar "yascroll" nil t)
;; yascroll / (package-install 'yascroll)
  ;; 1行単位のスクロールにしているとちらつくので必要な時だけ表示にする
  (dolist (fn '(set-mark exchange-point-and-mark scroll-up scroll-down recenter))
    (eval `(defadvice ,fn (after ,(intern (format "ini:show-yascroll-on-%s" fn)) activate)
	     "スクロールバーを表示する."
	     (when (not (memq major-mode '(term-mode shell-mode)))
	       (yascroll:show-scroll-bar)))))
  (with-eval-after-load "isearch"
    (add-hook 'isearch-update-post-hook 'yascroll:show-scroll-bar))
  )

;; session / (package-install 'session)
(when (require 'session nil t)
  (add-hook 'after-init-hook 'session-initialize)
  (setq session-initialize '(de-saveplace session places menus))
  (setq session-save-file (ini:emacs-d "session-data"))
  (setq session-globals-include '((kill-ring 50)
				  (read-expression-history 100)
				  (session-file-alist 500 t)
				  (file-name-history 10000)))
  (setq session-globals-max-string 100000000)
  (setq session-undo-check -1)
  (setq history-length t)

  (ini:make-silently-loading session-initialize-do))

;; quail-japanese
(with-eval-after-load "japanese"
  (setq quail-japanese-use-double-n t))

;; Mozc / https://code.google.com/p/mozc/source/browse/trunk/src/unix/emacs/mozc.el
;;    and http://www49.atwiki.jp/ntemacs?cmd=upload&act=open&pageid=50&file=mozc_emacs_helper.zip
(when (and (executable-find "mozc_emacs_helper")
	   (require 'mozc nil t))
  (setq default-input-method "japanese-mozc")

  (when (memq system-type '(windows-nt cygwin))
    (defadvice mozc-session-execute-command (after ini:mozc-session-execute-command activate)
      "`mozc' を有効化した際に自動的にひらがな入力モードに変更する."
      (if (eq (ad-get-arg 0) 'CreateSession)
	  (mozc-session-sendkey '(hiragana)))))

  (defadvice mozc-mode (after ini:mozc-minibuffer-workaround activate)
    "ミニバッファから抜ける際に正しく input-method を無効化する."
    (when (and mozc-mode
  	       (eq (selected-window) (minibuffer-window)))
      (add-hook 'minibuffer-exit-hook 'mozc-exit-from-minibuffer)))

  (defun mozc-exit-from-minibuffer ()
    "`minibuffer' から抜ける際に `deactivate-input-method' を呼び出す."
    (when (equal current-input-method "japanese-mozc")
      (deactivate-input-method)
      (if (<= (minibuffer-depth) 1)
  	  (remove-hook 'minibuffer-exit-hook 'mozc-exit-from-minibuffer))))

  (defadvice mozc-leim-deactivate (around ini:mozc-deactive-workaround activate)
    "正しく `mozc-mode' を終了させる."
    (mozc-mode -1))

  (defadvice mozc-helper-process-recv-response (after ini:mozc-accept-output-workaround activate)
    "他プロセスが終了した際に accept-process-output がタイムアウトする問題対策."
    (unless ad-return-value
      (setq ad-return-value (mozc-helper-process-recv-response))))

  ;; mozc のステータス取得系
  (defvar ini:mozc-status-alist nil
    "`mozc-mode' の状態を表す alist.")

  (defconst ini:mozc-status-default-mode
    (if (memq system-type '(windows-nt cygwin)) 'direct 'hiragana)
    "`mozc-mode' の初期変換モード.")

  (defadvice mozc-mode (after ini:mozc-status-init activate)
    "`mozc-status-alist' を初期化する."
    (unless mozc-mode
      (setq ini:mozc-status-alist nil)))

  (defadvice mozc-session-recv-corresponding-response (after ini:mozc-status-update activate)
    "`mozc-status-alist' を更新する."
    (when ad-return-value
      (setq ini:mozc-status-alist
	    `((session-id . ,(mozc-protobuf-get ad-return-value 'emacs-session-id))
	      (mode       . ,(or (mozc-protobuf-get ad-return-value 'output 'mode)
				 ini:mozc-status-default-mode
				 'direct))
	      (state      . ,(cond
			      ((mozc-protobuf-get ad-return-value 'output 'preedit)
			       'preedit)
			      ((mozc-protobuf-get ad-return-value 'output 'result)
			       'result)
			      (t
			       'empty)))))))

  ;; ;; mozc-el-extensions / git clone https://github.com/iRi-E/mozc-el-extensions
  ;; (setq mozc-isearch-use-workaround nil)
  ;; (require 'mozc-isearch nil t)

  ;; ccc がある場合(= skk と共存する場合)は ccc でカーソルカラー変更
  (if (require 'ccc nil t)
      (progn
	(add-hook 'input-method-activate-hook
		  (lambda () (set-buffer-local-cursor-color "dark red")))
	(add-hook 'input-method-deactivate-hook
		  (lambda () (set-cursor-color-buffer-local nil))))
    (when (require 'mozc-cursor-color nil t)
      (let ((normal (if (and (eq (frame-parameter nil 'background-mode) 'dark)
			     (string= (frame-parameter nil 'cursor-color) "black"))
			"white" "black"))
	    (ime "dark red"))
	(setq mozc-cursor-color-alist
	      `((direct . ,normal)
		(read-only . ,normal)
		(hiragana . ,ime)
		(full-katakana . ,ime)
		(half-ascii . ,ime)
		(full-ascii . ,ime)
		(half-katakana . ,ime)))))
    )

  (setq mozc-leim-title "[あ]")

  (when (require 'mozc-mode-line-indicator nil t)
    (setq mozc-mode-line-indicator-title-format "[%s]"))

  ;; mozc-popup / git clone https://github.com/d5884/mozc-popup
  (when (and (require 'popup nil t)
	     (require 'mozc-popup nil t))
    (setq mozc-candidate-style 'popup)))

;; Daredevil SKK / (package-install 'ddskk)
;; 辞書 / cvs -d:pserver:guest@openlab.jp:/circus/cvsroot login [guest]
;;        cvs -d:pserver:guest@openlab.jp:/circus/cvsroot co -d ~/.emacs.d/share/skk skk/dic
(when (and (load "ddskk-autoloads" t t)
	   (require 'skk-leim nil t))
  (setq skk-user-directory user-emacs-directory)
  (setq skk-init-file (expand-file-name "skk-init.el" skk-user-directory))

  (global-set-key (kbd "C-x C-j") (defun ini:force-skk-activate ()
				    "強制的に `current-input-method' を `skk-mode' にする."
				    (interactive)
				    (if (equal current-input-method "japanese-skk")
					(deactivate-input-method)
				      (when current-input-method
					(deactivate-input-method))
				      (set-input-method "japanese-skk"))))
  
  ;; 実験的拡張へのロードパス追加(あれば)
  (ini:awhen (ini:library-within "skk" "experimental" t)
    (add-to-list 'load-path it))

  (add-hook 'skk-load-hook
	    (lambda ()
	      ;; ローカルの辞書設定
	      (let ((dict-dir (ini:emacs-d "share/skk")))
		(ini:awhen (locate-file "SKK-JISYO.L" (list dict-dir))
		  (setq skk-large-jisyo it
			skk-aux-large-jisyo it))

		;; 追加の通常辞書 (あれば)
		(setq skk-extra-jisyo-file-list
		      (cl-remove-if-not #'file-exists-p
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
		
		;; cdb は使わない
		(setq skk-search-prog-list
		      (cl-remove-if (lambda (prog)
				      (eq (car prog) 'skk-search-cdb-jisyo))
				    skk-search-prog-list))
		)

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

	      (defadvice skk-mode-string-to-indicator (before ini:skk-mode-elimit-hyphen
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
	      (setq skk-show-tooltip t)
	      (setq skk-use-jisx0201-input-method t)

	      (when (require 'pos-tip nil t)
		(setq skk-tooltip-function 'pos-tip-show))
	      )))

;; migemo / (package-install 'migemo)
;; cmigemo / http://www.kaoriya.net/software/cmigemo
(when (and (or (executable-find "cmigemo")
	       (executable-find "migemo"))
	   (locate-library "migemo"))
  (defvar ini:org-isearch-lazy-highlight-search
    (symbol-function 'isearch-lazy-highlight-search)
    "migemo に置き換えられる前の `isearch-lazy-highlight-search'.")
  
  (when (require 'migemo nil t)
    (setq migemo-command (or (executable-find "cmigemo")
			     (executable-find "migemo")))

    (when (executable-find "cmigemo")
      (setq migemo-options '("-q" "--emacs"))
      (setq migemo-user-dictionary nil)
      (setq migemo-regex-dictionary nil)
      (setq migemo-coding-system 'utf-8)
      (setq migemo-dictionary (locate-file "utf-8/migemo-dict"
					   `(,(ini:emacs-d "share/migemo")
					     "/usr/local/share/migemo"
					     "/usr/share/migemo")
					   ))
      (when (eq system-type 'cygwin)
	(setq migemo-dictionary
	      (replace-regexp-in-string
	       "\r?\n" "" (shell-command-to-string
			   (format "cygpath -w \"%s\""
				   (shell-quote-argument migemo-dictionary))))))
      )

    (setq migemo-use-pattern-alist t)
    (setq migemo-use-frequent-pattern-alist t)
    (setq migemo-pattern-alist-length 1024)
    (setq migemo-pattern-alist-file (ini:emacs-d "migemo-pattern"))
    (define-key isearch-mode-map (kbd "M-k") 'migemo-isearch-toggle-migemo) ; compatible with kogiku

    ;; pty を消費しない
    (let ((process-connection-type nil))
      (migemo-init))

    ;; query-replace 系での lazy-highlight 対応
    (dolist (fn '(query-replace query-replace-regexp))
      (eval `(defadvice ,fn (around ,(intern (format "ini:%s-with-migemo"
						     fn)) activate)
	       "migemo 導入時でもハイライトを有効にする."
	       (cl-letf (((symbol-function 'isearch-lazy-highlight-search)
			  ini:org-isearch-lazy-highlight-search))
		 ad-do-it
		 ))))

    (with-eval-after-load "isearch"
      ;; isearch 中に leim を使用しない
      (define-key isearch-mode-map [remap toggle-input-method] 'undefined)
      (define-key isearch-mode-map [remap isearch-toggle-input-method] 'undefined)
      (define-key isearch-mode-map [remap isearch-toggle-specified-input-method]
    	'undefined))

    (defadvice isearch-lazy-highlight-update (around ini:suppress-error-isearch-regexp activate)
      "正規表現検索時のエラー回避."
      (ignore-errors
	ad-do-it))

    ;; isearch 前後での LEIM 切り替えバグパッチ
    (defadvice isearch-mode (before migemo-search-ad activate)
      "adviced by migemo."
      (setq migemo-search-pattern nil)
      (setq migemo-search-pattern-alist nil)
      (setq migemo-current-input-method nil)
      (when migemo-isearch-enable-p
    	;; when migemo is enabled, disable input method.
    	(setq migemo-current-input-method current-input-method)
    	(deactivate-input-method)))

    (defadvice isearch-done (after migemo-search-ad activate)
      "adviced by migemo."
      (setq migemo-search-pattern nil)
      (setq migemo-search-pattern-alist nil)
      (when (and migemo-current-input-method
    		 (not current-input-method))
    	;; when current input method is saved, and
    	;; was not changed during isearch-mode,
    	;; restore it.
    	(activate-input-method migemo-current-input-method)))

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
      (setq completion-styles (delq 'partial-completion completion-styles)))
    ))

(when (locate-library "direx")
  (autoload 'direx:jump-to-directory-other-window "direx" nil t)
;; direx / (package-install 'direx)
  (global-set-key (kbd "C-z C-d") 'direx:jump-to-directory-other-window)

  (with-eval-after-load "direx"
    (define-key direx:direx-mode-map (kbd "[") 'direx:expand-item)
    (define-key direx:direx-mode-map (kbd "]") 'direx:collapse-item)
    
    (defvar direx:mask-dot t
      "ドットファイルをマスクする.")
    
    (define-key direx:direx-mode-map (kbd "a")
      (defun direx:toggle-mask ()
	"ドットファイルのマスクを切り替える."
	(interactive)
	(setq direx:mask-dot (not direx:mask-dot))
	(call-interactively 'direx:refresh-whole-tree)))

    (defadvice direx:node-children (around ini:sort-by-directory activate)
      "direx でディレクトリを最初に表示する."
      (let ((org-directory-files (symbol-function 'directory-files)))
	(cl-letf (((symbol-function 'directory-files)
		   (lambda (directory &optional full match nosort)
		     (if nosort
			 (funcall org-directory-files directory full match nosort)
		       (sort (funcall org-directory-files directory full
				      (when direx:mask-dot "^[^.]"))
			     (lambda (a b)
			       (let ((dir-a (file-directory-p a))
				     (dir-b (file-directory-p b)))
				 (if (eq dir-a dir-b)
				     (string-lessp a b)
				   (if dir-a t)))))))))
	  ad-do-it)))
    )

  (with-eval-after-load "popwin"
    (add-to-list 'popwin:special-display-config
		 '(direx:direx-mode :position left :width 25 :dedicated t)))
  )

;; yasnippet / (package-install 'yasnippet)
(when (require 'yasnippet nil t)
  (diminish 'yas-minor-mode)

  (setq yas-verbosity 1)
  (setq yas-prompt-functions (delq 'yas-x-prompt yas-prompt-functions))
  (setq yas-expand-only-for-last-commands '(self-insert-command ac-expand))

 (when (fboundp 'yas--load-yas-setup-file)
   (ini:make-silently-loading yas--load-yas-setup-file))

  (yas-global-mode t)

  (with-eval-after-load "term"
    (add-hook 'term-mode-hook
	      (lambda () (yas-minor-mode -1))))
  
  ;; auto insert
  (add-hook 'find-file-hook 'auto-insert)
  (with-eval-after-load "autoinsert"
    (setq auto-insert-directory (ini:emacs-d "template"))

    (defvar ini:auto-insert-template-modtime nil
      "テンプレートディレクトリの更新時間.")

    (defun ini:auto-insert-yas-expand ()
      "`auto-insert' するテンプレートを `yasnippet' のスニペットと見做して展開する."
      (yas-expand-snippet (buffer-string) (point-min) (point-max)))

    (defadvice auto-insert (before ini:auto-insert-update-template activate)
      "`auto-insert' 前にテンプレート一覧を更新する.
モード名と拡張子を除いたファイル名が一致する場合テンプレートと見做す."
      (let ((modtime (file-attributes auto-insert-directory)))
	(unless (equal modtime ini:auto-insert-template-modtime)
	  (setq ini:auto-insert-template-modtime modtime)
	  (setq auto-insert-alist
		(mapcar (lambda (f)
			  (cons (intern (file-name-sans-extension f))
				(vector f 'ini:auto-insert-yas-expand)))
			(directory-files auto-insert-directory nil "^[^.]")))
	  )))
    )
  )

;; auto-complete-mode / (package-install 'auto-complete)
(when (require 'auto-complete-config nil t)
  (diminish 'auto-complete-mode)

  (ini:awhen (ini:library-within "auto-complete-config" "dict" t)
    (add-to-list 'ac-dictionary-directories it))

  (ac-config-default)

  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (define-key ac-completing-map (kbd "C-g") 'ac-stop)

  (setq ac-use-menu-map t)

  (when (require 'pos-tip nil t)
    (setq ac-quick-help-prefer-pos-tip t))
  )

(when (ignore-errors (require 'popup-kill-ring)) ;require popup and pos-tip
;; popup-kill-ring / (package-install 'popup-kill-ring)
  (setq popup-kill-ring-interactive-insert t)
  (global-set-key (kbd "M-y") 'popup-kill-ring)
  (define-key popup-kill-ring-keymap (kbd "TAB") 'popup-kill-ring-next)
  (define-key popup-kill-ring-keymap (kbd "M-y") 'popup-kill-ring-next))

(when (and (executable-find "convert")
	   (locate-library "image+"))
  (with-eval-after-load "image"
    (require 'image+ nil t)
;; image+ / (package-install 'image+)
    (imagex-auto-adjust-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 追加関数定義

;;;;;;;;;;;;;;;;;;;
;; バッファの永続化
(defvar-local ini:permanent-buffer nil
  "non-nil の場合、バッファが kill 不可になる.")
(put 'ini:permanent-buffer 'permanent-local t)

(defun ini:make-buffer-permanently (buffer &optional flag)
  "バッファを kill 出来なくする.
flag が clear の場合は kill 時にバッファの中身を空にする.
flag が関数の場合は対象関数を呼び出す.
その際、関数の戻り値が non-nil の場合バッファは kill される.
flag が -1 の場合は kill 可能に戻す.
flag が上記以外の non-nil の場合は削除不可にする."
  (with-current-buffer buffer
    (setq ini:permanent-buffer (if (eq flag -1)
				   nil
				 (or flag t)))))

(add-hook 'kill-buffer-query-functions
	  (lambda ()
	    (cond
	     ((null ini:permanent-buffer)
	      t)
	     ((eq ini:permanent-buffer 'clear)
	      (widen)
	      (erase-buffer)
	      (message "Buffer was cleared.")
	      nil)
	     ((functionp ini:permanent-buffer)
	      (funcall ini:permanent-buffer))
	     (t
	      (message "This buffer is flagged to permanent buffer.")
	      nil))))

(add-hook 'after-init-hook
	  (lambda ()
	    (ini:make-buffer-permanently "*scratch*"
					 (lambda ()
					   (ini:refresh-scratch-buffer t)))
	    (ini:make-buffer-permanently "*Messages*" 'clear)
	    ))


;;;;;;;;;;;;;;;;;;;
;; scratch 自動保存
(defvar ini:scratch-save-file (ini:emacs-d "scratch")
  "`*scratch*' バッファの自動保存先ファイル名.")

(defvar ini:scratch-scrap-directory (ini:emacs-d "snap")
  "`*scratch*' バッファのスクラップ先ディレクトリ名")

(defvar ini:scratch-buffer-save-interval 1
  "`*scratch*' バッファの自動保存間隔.")

(defvar ini:prev-scratch-modified-tick 0
  "`*scratch*' バッファの前回保存時の更新状態.")

(defun ini:refresh-scratch-buffer (&optional scrap)
  "`*scratch*' バッファを初期状態に戻す.
削除されていた場合はバッファを新規作成し、存在している場合は内容をクリアする.
SCRAP が non-nil の場合、`ini:scratch-scrap-directory' 内に
現在の `*scratch*' バッファの内容を保存する."
  (interactive)
  (let ((exists-p (get-buffer "*scratch*")))
    (when (and exists-p scrap)
      (ini:scratch-buffer-scrap))
    (with-current-buffer (or exists-p (get-buffer-create "*scratch*"))
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

(defun ini:scratch-buffer-scrap ()
  "`*scratch*' バッファの内容を `ini:scratch-scrap-directory' 内に保存する."
  (interactive)
  (ini:awhen (get-buffer "*scratch*")
    (make-directory ini:scratch-scrap-directory t)
    (let ((name-base (format "scratch-%s%%02d.el" (format-time-string "%Y%m%d-%H%M%S")))
	  (serial 0)
	  scrap-name)
      (while (file-exists-p
	      (setq scrap-name (expand-file-name
				(format name-base serial) ini:scratch-scrap-directory)))
	(setq serial (1+ serial)))
      (with-current-buffer it
	(save-match-data
	  (save-restriction
	    (widen)
	    (goto-char (point-min))
	    (unless (re-search-forward "\\`[ \r\n\t]*\\'" nil t)
	      (write-region (point-min) (point-max) scrap-name nil 'silent))))))
    ))

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

(defun ini:flip-window-state (&optional renew)
  "ウィンドウ分割状態を切り替える.
RENEW が non-nil の場合は新しい状態を作る.
2状態固定."
  (interactive "P")
  (let* ((cur (current-window-configuration))
	 (state (frame-parameter nil 'ini:last-window-state))
	 (conf (unless renew (car state)))
	 (side (cl-case (cdr state) (?A ?B) (?B ?A) (t ?B))))
    (if conf
	(set-window-configuration conf)
      (delete-other-windows)
      (switch-to-buffer "*scratch*"))
    (message "Flip to side \"%c\"." side)
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
(cl-flet ((color-candidate (&rest colors)
			   (cl-find-if #'color-defined-p colors)))
  (require 'color)
  (when (and (eq (frame-parameter nil 'background-mode) 'dark)
	     (string= (frame-parameter nil 'cursor-color) "black"))
    (set-cursor-color "white"))

  (set-face-attribute 'region nil
		      :foreground (color-candidate "SystemHilightText" "White")
		      :background (color-candidate "SystemHilight" "Royal Blue"))

  (set-face-attribute 'isearch nil
		      :background (color-darken-name (color-candidate "SystemHilight"
								      "Royal Blue") 10)
		      :inherit 'region)
  
  (set-face-attribute 'lazy-highlight nil
		      :background (color-darken-name (color-candidate "SystemWindow"
								      "White") 40)
		      :inherit 'isearch)
  
  (face-spec-reset-face 'match)
  (set-face-attribute 'match nil :inherit 'lazy-highlight)
  
  (with-eval-after-load "cua-base"
    (face-spec-reset-face 'cua-rectangle)
    (set-face-attribute 'cua-rectangle nil :inherit 'region)
    (face-spec-reset-face 'cua-global-mark)
    (set-face-attribute 'cua-global-mark nil :inherit 'region
			:weight 'bold)
    )

  (with-eval-after-load "paren"
    (set-face-attribute 'show-paren-match nil
			:weight 'bold
			:foreground "#005"
			:background "#ccf")
    (set-face-attribute 'show-paren-mismatch nil
			:weight 'bold
			:foreground "#700"
			:background "#fcc")
    )

  (when (> (display-color-cells nil) 256)
    (with-eval-after-load "stripe-buffer"
      (set-face-attribute 'stripe-highlight nil
			  :background "gray97")))

  (with-eval-after-load "mozc"
    (set-face-attribute 'mozc-cand-overlay-even-face nil
			:background "gray80")
    (set-face-attribute 'mozc-cand-overlay-odd-face nil
			:background "gray80")
    (set-face-attribute 'mozc-cand-overlay-footer-face nil
			:foreground "white"
			:background "gray50")
    (set-face-attribute 'mozc-cand-overlay-focused-face nil
			:foreground "white"
			:background "gray30"))
  (with-eval-after-load "mozc-popup"
    (set-face-attribute 'mozc-cand-overlay-description-face nil
			:foreground "gray46"))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; カスタマイズファイル読み込み
(setq custom-file (ini:emacs-d "custom.el"))
(load (file-name-sans-extension custom-file) t t)

;; 作業ディレクトリをホームディレクトリに
(ini:awhen (getenv "HOME") (cd it))

;;; init.el ends here
