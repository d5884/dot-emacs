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

(defmacro ini:aand1 (pred &rest conds)
  "PRED および CONDS を評価し、結果が全て non-nil ならば PRED の結果を返す.
CONDS 内では PRED の評価結果を `it' で参照出来る."
  `(let ((it ,pred))
     (if (and it ,@conds)
	 it)))

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

(defmacro ini:emacs-d (path)
  "~/.emacs.d 以下の PATH を返す."
  `(eval-when-compile (locate-user-emacs-file ,path)))

(defmacro ini:locate-path (path)
  "PATH が存在するなら返す."
  `(locate-file "." (list ,path) nil (lambda (p) (when (file-exists-p p) 'dir-ok))))

(defmacro ini:find-path (paths)
  "PATHS に見付かったパスを返す."
  `(locate-file "." ,paths nil (lambda (p) (when (file-exists-p p) 'dir-ok))))

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
  "TRIGGER-FUNCTION で作成される BUFFER に `bury-buffer' の呼び出しを定義する.
KEY が non-nil の場合は KEY に、nil の場合は q にバインドされる.
特定のモードが存在しないバッファに対して用いる."
  `(defadvice ,trigger-function (after ,(intern (format "ini:%s-define-quit-key"
							trigger-function)) activate)
     "実行時に `bury-buffer' 呼び出し用のキー設定を行なう."
     (ignore-errors
       (with-current-buffer ,buffer
	 (local-set-key ,(or key "q") 'bury-buffer)))))

(defmacro ini:when-when-compile (pred &rest form)
  "展開時に PRED を評価し、 non-nil の場合に FORM を展開する.
`system-type' や実行ファイルの有無などの変化があまりない箇所での最適化に用いる."
  (when (eval pred)
    `(progn ,@form)))

(defmacro ini:cond-when-compile (&rest clauses)
  "展開時に CLAUSES の評価部を評価し、実行部を展開する. `cond' を参照.
`system-type' や実行ファイルの有無などの変化があまりない箇所での最適化に用いる."
  (if (null clauses)
      nil
    (if (eval (caar clauses))
	(cons 'progn (cdar clauses))
      `(ini:cond-when-compile ,@(cdr clauses)))))

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
(put 'ini:when-when-compile 'lisp-indent-function 1)

(font-lock-add-keywords 'emacs-lisp-mode
			`((,(regexp-opt '("ini:aif" "ini:awhen"
					  "ini:when-when-compile"
					  "ini:cond-when-compile"))
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
       (ini:awhen (ini:locate-path (ini:emacs-d "lisp"))
	 (cons it (cl-remove-if-not #'file-directory-p (directory-files it t "^[^.]"))))
       load-path))

;; locate-file のキャッシュ導入
;; (require 'locate-file-cache nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 言語設定
(set-language-environment 'Japanese)
(ini:cond-when-compile
 ((eq system-type 'windows-nt)
  (prefer-coding-system 'utf-8-dos)
  (set-file-name-coding-system 'cp932)
  (modify-coding-system-alist 'process "[cC][mM][dD]" '(undecided . cp932)))
 (t
  (prefer-coding-system 'utf-8-unix)))

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
	  (inhibit-redisplay t))
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
(ini:when-when-compile (eq system-type 'windows-nt)
  ;; cygwin へのパス等が通されていない前提で emacs 内で諸々を設定する

  ;; パイプ間の待ち時間を減らす
  (setq w32-pipe-read-delay 5)

  ;; インストールルート検索
  (ini:awhen (eval-when-compile
	       (or (ini:aand1 (getenv "CYGWIN_DIR")
			      (file-exists-p it))
		   (ini:find-path
		    (mapcar (lambda (p) (expand-file-name "cygwin" p))
			    (list (getenv "LOCALAPPDATA")
				  (getenv "APPDATA")
				  (getenv "USERPROFILE")
				  (getenv "HOME")
				  (getenv "ProgramW6432")
				  (getenv "ProgramFiles")
				  "c:/"
				  "c:/gnupack/app/cygwin")))))
    (let ((cygwin-exec-path		; cygwin のルートパスからの相対パスとして追加
	   (mapcar (lambda (path)
		     (expand-file-name (if (and (> (length path) 0) (eq (aref path 0) ?/))
					   (substring path 1)
					 path) it))
		   `(,(ini:emacs-d "bin") "~/bin" "/usr/local/bin" "/usr/bin" "/bin"))))
      (setenv "PATH" (ini:concat-system-path cygwin-exec-path nil (getenv "PATH")))
      (setq exec-path (append cygwin-exec-path exec-path))

      (setenv "LANG" "ja_JP.UTF-8")
      (setenv "CYGWIN" "nodosfilewarning winsymlinks")

      (setq null-device "/dev/null")

      ;; shell
      (ini:when-when-compile (executable-find "bash")
	(setq shell-file-name "bash")
	(setq shell-command-switch "-c")
	(setq system-uses-terminfo nil)

	(setenv "SHELL" shell-file-name)

	;; DOSコマンド混在のためプロセスでの出力のコードを未定に
	(setcar default-process-coding-system
		(coding-system-change-text-conversion (car default-process-coding-system)
						      'undecided))
	;; shell-mode での出力コード自動判別設定 (undefined なだけだと判定後変更されてしまう)
	(defadvice comint-send-input (before ini:comint-send-detect-coding activate)
	  "出力時の文字コードを自動判断に毎回戻す."
	  (ini:awhen (get-buffer-process (current-buffer))
	    (set-process-coding-system it
				       (coding-system-change-text-conversion
					(car default-process-coding-system) 'undecided)
				       (coding-system-change-text-conversion
					(cdr default-process-coding-system) 'utf-8))))

	(with-eval-after-load "term"
	  (require 'shell)
	  (defadvice cd (around ini:cd-accept-multibyte activate)
	    "`term' でマルチバイトのディレクトリに移動時の強制終了を防ぐ."
	    (let ((dir (decode-coding-string dir 'undecided)))
	      ad-do-it))

	  (defadvice term-emulate-terminal (around ini:terminal-detect-coding activate)
	    "`term' で複数のコーディング出力を受け付ける."
	    (let ((locale-coding-system 'undecided))
	      ad-do-it)))	
	
	(with-eval-after-load "tramp"
	  (setq tramp-encoding-shell "bash"))
	)
      
      ;; cygwin で追加される Info
      (with-eval-after-load "info"
	(ini:awhen (ini:locate-path "/usr/share/info")
	  (add-to-list 'Info-additional-directory-list it)))
      
      ;; cygwin-mount / http://home.avvanta.com/~offby1/cygwin-mount/cygwin-mount.el
      ;; ... or git clone https://github.com/emacsmirror/cygwin-mount
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
      
      ;; fakecygpty
      ;; gcc -o fakecygpty.exe fakecygpty.c
      ;; https://github.com/d5884/fakecygpty
      (ini:when-when-compile (executable-find "fakecygpty")
	(defvar fakery-command (executable-find "fakecygpty")
	  "fakecygpty の実行ファイル.")
	(defvar fakery-sigqueue-command (executable-find "sigqueue")
	  "sigqueue の実行ファイル.")
	
	(defvar fakery-enabled t
	  "non-nil の場合、fakecygpty によるラップを有効にする.")
	
      	(defadvice start-process (around ini:start-process-to-fake last activate)
      	  "`process-connection-type' が non-nil の場合、fakecygpty を経由させる."
	  (if (and
	       fakery-enabled
	       process-connection-type	; if non-nil, required pty.
	       (ad-get-arg 2))
	      (progn
		(ad-set-args 3 (cons (ad-get-arg 2) (ad-get-args 3)))
		(ad-set-arg 2 "fakecygpty")
		ad-do-it
		(when ad-return-value
		  (process-put ad-return-value :fakery-pty-p t)))
	    ad-do-it))

	(defun fakery-process-p (process)
	  "PROCESS が fakecygpty 経由で起動している場合は non-nil を返す."
	  (and (processp process)
	       (process-get process :fakery-pty-p)))

	(defadvice process-command (after ini:process-command-to-fake activate)
	  "fakecygpty 経由の場合は実際に実行しているコマンド名を返す."
	  (when (fakery-process-p (ad-get-arg 0))
	    (setq ad-return-value (cdr ad-return-value))))

	(defadvice process-tty-name (after ini:process-tty-name-to-fake activate)
	  "fakecygpty 経由の場合は cygwin 上での tty 名を返す."
	  (when (fakery-process-p (ad-get-arg 0))
	    (setq ad-return-value
		    (with-temp-buffer
		      (if (\= 0 (call-process
				 "sh" nil (current-buffer) nil
				 "-c"
				 (format 
				  (concat
				   "X=`ls /proc/*/ppid | xargs grep -l \"^%s$\" 2>/dev/null` ; "
				   "X=`dirname $X 2>/dev/null` && cat `echo -n \"$X/ctty\"`")
				  (process-id (ad-get-arg 0)))))
			  (replace-regexp-in-string "\r?\n" "" (buffer-string))
			"?")))))
	
	(defadvice process-send-eof (around ini:send-process-eof-to-fake activate)
	  "fakecygpty 経由の場合は ^D を送信する."
	  (let ((target (if (ad-get-arg 0)
			    (get-process (ad-get-arg 0))
			  (get-buffer-process (current-buffer)))))
	    (if (fakery-process-p target)
		(send-string target "\C-d")
	      ad-do-it)))
	
	(defadvice interrupt-process (around ini:interrupt-process-to-fake activate)
	  "fakecygpty 経由の場合は ^C を送信する."
	  (let ((target (if (ad-get-arg 0)
			    (get-process (ad-get-arg 0))
			  (get-buffer-process (current-buffer)))))
	    (if (fakery-process-p target)
		(send-string target "\C-c")
	      ad-do-it)))
	
	(defadvice quit-process (around ini:quit-process-to-fake activate)
	  "fakecygpty 経由の場合は ^\\ を送信する."
	  (let ((target (if (ad-get-arg 0)
			    (get-process (ad-get-arg 0))
			  (get-buffer-process (current-buffer)))))
	    (if (fakery-process-p target)
		(send-string target "\C-\\")
	      ad-do-it)))
	
	(defadvice stop-process (around ini:stop-process-to-fake activate)
	  "fakecygpty 経由の場合は ^Z を送信する."
	  (let ((target (if (ad-get-arg 0)
			    (get-process (ad-get-arg 0))
			  (get-buffer-process (current-buffer)))))
	    (if (fakery-process-p target)
		(send-string target "\C-z")
	      ad-do-it)))

	(defadvice signal-process (around ini:signal-process-to-fake activate)
	  "cygwin のプロセスに対して任意のシグナルを送れるようにする.
Windows のプロセスに対してはオリジナルの `signal-process' を呼び出す.
対話的に呼び出された場合は無効."
	  (let* ((proc (ad-get-arg 0))
		 (pid (cond
		       ((integerp proc)
			proc)
		       ((stringp proc)
			(ignore-errors (process-id (get-process proc))))
		       ((processp proc)
			(process-id proc))
		       ((null proc)
			(ignore-errors (process-id (get-buffer-process
						    (current-buffer)))))
		       (t nil))))
	    (if (or (null pid)
		    (/= 0 (call-process fakery-sigqueue-command nil nil nil
					(prin1-to-string pid t)
					(prin1-to-string (ad-get-arg 1) t)
					"0")))
		ad-do-it
	      (setq ad-return-value 0))))

	(defadvice set-process-window-size (around set-process-window-size-to-fake activate)
	  "fakecygpty 経由の場合は siqgueue にて WINCH シグナルを送信する."
	  (if (and (fakery-process-p (ad-get-arg 0))
		   (= 0 (call-process fakery-sigqueue-command nil nil nil
				      (prin1-to-string (process-id (ad-get-arg 0)) t)
				      "SIGWINCH"
				      (prin1-to-string (+ (* 65536 (ad-get-arg 1))
							  (ad-get-arg 2))))))
	      (setq ad-return-value t)
	    ad-do-it)

	  )

	))))

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
(setq recenter-positions '(middle))
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
;; inhibit-startup-echo-area-message はユーザ名をリテラルで書く必要があるので
;; Daemon 起動時以外は表示関数を直接潰す
(defadvice display-startup-echo-area-message (around ini:shut-up-echo-message activate)
  "起動時のエコーエリアのメッセージを表示しない."
  (if (daemonp) ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; パッケージ別機能設定

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
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

;; package
(ini:when-when-compile (locate-library "package")
  (package-initialize)
  (setq package-enable-at-startup nil)
  (setq package-archives (append
			  '(("marmalade" . "http://marmalade-repo.org/packages/")
			    ("melpa" . "http://melpa.milkbox.net/packages/"))
			  package-archives)))

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

  (add-hook 'compilation-mode-hook 'next-error-follow-minor-mode)
  
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


;; recentf
(with-eval-after-load "recentf" ;; 基本的に使わないがファイルをホームに作らないよう設定
  (setq recentf-save-file (ini:emacs-d "recentf")))

;; info
(with-eval-after-load "info"
  (ini:awhen (eval-when-compile (ini:locate-path (ini:emacs-d "info")))
    (add-to-list 'Info-additional-directory-list it)))

;; ispell
(with-eval-after-load "ispell"
  ;; from http://www.an.econ.kobe-u.ac.jp/~namba/meadow/words.lzh
  (ini:awhen (eval-when-compile (locate-file "words"
					     `("/usr/dict"
					       "/usr/share/dict"
					       ,(ini:emacs-d "etc")
					       ,user-emacs-directory)))
    (setq ispell-alternate-dictionary it)))

;; man & woman
(with-eval-after-load "woman"
  (setq woman-fill-frame t
	woman-cache-filename (ini:emacs-d "woman_cache")))

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
  ;; C-z は自分で使いたいので C-c C-z に移動
  (define-key term-raw-map (kbd "C-c C-z") 'term-send-raw)
  ;; char-mode で使いたいキーを開放
  (dolist (key '("M-x" "M-:" "C-z" "C-u"))
    (define-key term-raw-map (kbd key) nil))
  
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

;; iswitchb
(when (require 'iswitchb nil t)
  (iswitchb-mode t)
  (setq iswitchb-default-method 'samewindow)
  (add-hook 'iswitchb-define-mode-map-hook
	    (lambda ()
	      (define-key iswitchb-mode-map (kbd "C-f") 'iswitchb-next-match)
	      (define-key iswitchb-mode-map (kbd "C-b") 'iswitchb-prev-match)))

  (defadvice iswitchb-exhibit
    (after ini:iswitchb-exhibit-with-display-buffer activate)
    "選択しているバッファをウィンドウに表示する."
    (when (and (eq iswitchb-method iswitchb-default-method)
	       iswitchb-matches)
      (let ((iswitchb-method 'samewindow)
	    (selected (get-buffer-window
		       (cl-find-if-not #'minibufferp (buffer-list)))))
	(when selected
	  (select-window selected)
	  (iswitchb-visit-buffer
	   (get-buffer (car iswitchb-matches)))
	  (select-window (minibuffer-window))))
      ))
  )

;; grep
(with-eval-after-load "grep"
  (eval-when-compile
    (declare-function grep-apply-setting "grep"))

  (grep-apply-setting 'grep-find-use-xargs 'exec-plus)

  (ini:when-when-compile (executable-find "lgrep")
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
	   (not (server-running-p)))
  (server-start))

;; スクリプトファイルの自動 +x
(ini:when-when-compile (fboundp 'executable-make-buffer-file-executable-if-script-p)
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

;; Tramp
(with-eval-after-load "tramp"
  (setq tramp-default-method "ssh"))

(with-eval-after-load "tramp-sh"
  (let ((process-environment tramp-remote-process-environment))
    (setenv "LC_ALL" nil)		; リモートのロケールは接続先に準じる
    (setq tramp-remote-process-environment process-environment)))

;; ffap
(when (require 'ffap nil t)
  (ffap-bindings))

;; hideshow
(with-eval-after-load "hideshow"
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
(with-eval-after-load "gdb"
  (setq gdb-many-windows t)
  (setq gdb-use-separate-io-buffer t)
  (add-hook 'gdb-mode-hook 'gud-tooltip-mode))

;; ediff
(with-eval-after-load "ediff"
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (defvar ini:ediff-window-configuration-stash nil
    "`ediff' 実行前のウィンドウ状態の一時保存先.")
  (add-hook 'ediff-before-setup-hook
	    (lambda ()
	      (setq ini:ediff-window-configuration-stash
		    (current-window-configuration))))
  (add-hook 'ediff-quit-hook
	    (lambda ()
	      (set-window-configuration ini:ediff-window-configuration-stash)))
  )

;; magit
(ini:when-when-compile (locate-library "magit")
  (autoload 'magit-status "magit" nil t)
  (global-set-key (kbd "C-z C-m") 'magit-status))

;; flymake
(ini:when-when-compile (locate-library "flymake")
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

    ;; (defadvice flymake-post-syntax-check (before ini:flymake-force-interrupted-flag activate)
    ;; 	 "`flymake-mode' でチェックが異常終了時に固まるのを防ぐ."
    ;; 	 (setq flymake-check-was-interrupted t))
    )
  )

;; elisps
(autoload 'byte-compile-dest-file "bytecomp")
(defun ini:byte-compile-current-file-if-necessary ()
  "開いているファイルをバイトコンパイルする.
既にコンパイル済みのファイルがあり、ソースファイルの方が新しい場合のみコンパイルする."
  (interactive)
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

;; occur
(define-key occur-mode-map (kbd "n") 'occur-next)
(define-key occur-mode-map (kbd "p") 'occur-prev)
(add-hook 'occur-mode-hook 'next-error-follow-minor-mode)

;; view-mode
(with-eval-after-load "view"
  (define-key view-mode-map "j" 'next-line)
  (define-key view-mode-map "k" 'previous-line)
  (defadvice view-mode-exit (before ini:view-mode-exit activate)
    "`hl-line-mode' を無効にする."
    (when view-mode
      (hl-line-mode -1)))
  (add-hook 'view-mode-hook (lambda () (hl-line-mode t)))
  )

;; dired
(global-set-key (kbd "C-x C-d") 'dired-other-window)


(with-eval-after-load "dired"
  (eval-when-compile
    (declare-function dired-get-file-for-visit "dired"))
  
  (setq dired-dwim-target t)
  (setq dired-isearch-filenames t)
  (setq dired-listing-switches "-lAF")
  ;; (setq dired-listing-switches "-lAF --time-style=long-iso")
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M"))
  (setq ls-lisp-use-localized-time-format t)

  (add-hook 'dired-mode-hook
	    (lambda ()
	      ;; dired 上でのみゴミ箱使用
	      (toggle-truncate-lines t)
	      (setq-local delete-by-moving-to-trash t)
	      (hl-line-mode t)))

  (ini:when-when-compile (executable-find "cygstart")
    (define-key dired-mode-map (kbd "E")
      (defun ini:dired-execute-by-cygstart ()
	"cygstart を使ってファイルに関連付けられたプログラムを実行する."
	(interactive)
	(start-process "cygstart" nil "cygstart"
		       (dired-get-file-for-visit)))))
  
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
	      (setq-local flymake-warning-re "^[Ww]arning\\|警告")))
  
  (when (require 'flymake nil t)
    ;; c
    (ini:flymake-gen-simple-init cc "\\.c$"
				 "gcc" "-Wall" "-fsyntax-only" local-file)
    ;; c++
    (ini:flymake-gen-simple-init c++ "\\.cpp\\|\\.CC"
				 "g++" "-Wall" "-fsyntax-only" local-file)
    )
  )

;; ruby-mode / http://svn.ruby-lang.org/cgi-bin/viewvc.cgi/trunk/misc/
;; ... or svn co http://svn.ruby-lang.org/repos/ruby/trunk/misc ruby
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

(ini:when-when-compile (executable-find "ruby")
  ;; rubydb
  (ini:when-when-compile (locate-library "rubydb3x")
    (autoload 'rubydb "rubydb3x" nil t))
  
  ;; inf-ruby
  (ini:when-when-compile (locate-library "inf-ruby")
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

;; ;; w32-symlinks
;; (when (and (eq system-type 'windows-nt)
;; 	   (require 'w32-symlinks nil t))
;;   (custom-set-variables '(w32-symlinks-handle-shortcuts t))
;;   (defadvice w32-symlinks-parse-shortcut (around emacs23-fix activate)
;;     (let ((default-enable-multibyte-characters nil))
;;       ad-do-it))
;;   )

;; zencoding / git clone https://github.com/smihica/zencoding
(ini:when-when-compile (locate-library "zencoding-mode")
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

;; smart-compile
;;   from http://emacswiki.org/emacs/SmartCompile
;; ... or git clone https://github.com/emacsmirror/smart-compile.git
(ini:when-when-compile (locate-library "smart-compile")
  (autoload 'smart-compile "smart-compile" nil t)
  (autoload 'recompile "compile" nil t)

  (defun ini:smart-recompile (arg)
    "一度目は `smart-compile'、二度目は `recompile' を呼び出す.
ARG が non-nil の場合は `smart-compile' を呼び出す."
    (interactive "P")
    (if (or (not (local-variable-p 'compile-command))
	    arg)
	(smart-compile)
      (recompile)))
  
  (global-set-key [remap compile] 'ini:smart-recompile)
  )

;; nxml-mode
(ini:when-when-compile (fboundp 'nxml-mode)
  (fset 'html-mode 'nxml-mode)
  (fset 'xml-mode 'nxml-mode)

  (setq magic-mode-alist
	(cons '("<\\?xml" . nxml-mode) magic-mode-alist))
  (with-eval-after-load "smart-compile"
    (setq smart-compile-alist (cons
			       '(nxml-mode browse-url-of-buffer)
			       smart-compile-alist)))
  )

;; flex-autopair / git clone https://github.com/uk-ar/flex-autopair.git
(if (require 'flex-autopair nil t)
    (progn
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
(ini:when-when-compile (eq system-type 'windows-nt)
  (let ((gs-root (eval-when-compile (ini:aif (executable-find "gswin32c")
					(expand-file-name ".." (file-name-directory it))
				      (ini:find-path '("c:/gs" "c:/gnupack/app/gs"))))))
    (when gs-root
      (defvar gswin-command (expand-file-name "bin/gswin32c" gs-root)
	"ghostscript の実行プログラム.")

      (unless (getenv "GS_LIB")
	(setenv "GS_LIB" (ini:concat-system-path '("lib" "kanji" "Resource/Init") gs-root)))
      (unless (getenv "GS_DLL")
	(setenv "GS_DLL" (ini:system-path "bin/gsdll32.dll" gs-root)))
      (setenv "PATH" (ini:concat-system-path '("bin" "lib") gs-root (getenv "PATH")))

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
      )))


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
  
  (ini:define-quit-key calendar-list-holidays "*Holidays*")
  (ini:define-quit-key calendar-sunrise-sunset-month "*Sunrise/Sunset Times*")
  (ini:define-quit-key calendar-lunar-phases "*Phases of Moon*")

  ;; 月の満ち欠けの日本語化
  (with-eval-after-load "lunar"
    (setq lunar-phase-names '("新月" "上弦" "満月""下弦")))

  ;; 日本の祝日表示
  ;;   from http://www.meadowy.org/meadow/netinstall/browser/branches/3.00/pkginfo
  ;; ... or git clone https://github.com/emacs-jp/japanese-holidays.git
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
(ini:when-when-compile (locate-library "gnus")
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
  )

;; markdown-mode / git clone git://jblevins.org/git/markdown-mode.git
(ini:when-when-compile (locate-library "markdown-mode")
  (autoload 'markdown-mode "markdown-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.\\(md\\(wn\\|t\\)?\\|markdown\\|text\\)\\'" .
				  markdown-mode)))

;; sdic / http://www.namazu.org/~tsuchiya/sdic/
(ini:when-when-compile (locate-library "sdic")
  (autoload 'sdic-describe-word "sdic" nil t)
  (autoload 'sdic-describe-word-at-point "sdic" nil t)
  (global-set-key (kbd "C-c w") 'sdic-describe-word)
  (global-set-key (kbd "C-c C-w") 'sdic-describe-word-at-point)

  (with-eval-after-load "sdic"
    (setq sdic-default-coding-system 'utf-8-unix)
    (setq sdic-eiwa-dictionary-list `((sdicf-client ,(ini:emacs-d "etc/sdic/gene-u.sdic")
						    (strategy direct))))
    (setq sdic-waei-dictionary-list `((sdicf-client ,(ini:emacs-d "etc/sdic/jedict-u.sdic")
						    (add-keys-to-headword t)
						    (strategy direct))))

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
	  (display-buffer (get-buffer sdic-buffer-name))
	  (set-window-start (get-buffer-window sdic-buffer-name) p)
	  (goto-char p)
	  ))
      )
    ))

;; shell-pop / http://www.emacswiki.org/emacs/shell-pop.el
;; ... or git clone https://github.com/emacsmirror/shell-pop
(ini:when-when-compile (locate-library "shell-pop")
  (autoload 'shell-pop "shell-pop" nil t)
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

;; popwin / git clone https://github.com/m2ym/popwin-el
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

;; yascroll / git clone https://github.com/m2ym/yascroll-el yascroll
(ini:when-when-compile (locate-library "yascroll")
  (autoload 'yascroll:show-scroll-bar "yascroll" nil t)
  ;; 1行単位のスクロールにしているとちらつくので必要な時だけ表示にする
  (dolist (fn '(set-mark exchange-point-and-mark scroll-up scroll-down recenter))
    (eval `(defadvice ,fn (after ,(intern (format "ini:show-yascroll-on-%s" fn)) activate)
	     "スクロールバーを表示する."
	     (when (not (memq major-mode '(term-mode shell-mode)))
	       (yascroll:show-scroll-bar)))))
  (with-eval-after-load "isearch"
    (add-hook 'isearch-update-post-hook 'yascroll:show-scroll-bar))
  )

;; session
;;   from http://emacs-session.sourceforge.net/
;; ... or git clone  https://github.com/emacsmirror/session.git
(when (require 'session nil t)
  (add-hook 'after-init-hook 'session-initialize)
  (setq session-initialize '(de-saveplace session places))
  (setq session-save-file (ini:emacs-d "session"))
  (setq session-globals-include '((kill-ring 50)
				  (read-expression-history 100)
				  (session-file-alist 500 t)
				  (file-name-history 10000)))
  (setq session-globals-max-string 100000000)
  (setq session-undo-check -1)
  (setq history-length t)
  (defadvice session-initialize-do (around ini:session-load-silently activate)
    "セッションロード時のメッセージを抑制する."
    (let ((org-load (symbol-function 'load)))
      (cl-letf (((symbol-function 'load)
		 (lambda (file &optional noerror nomessage nosuffix must-suffix)
		   (funcall org-load file noerror t nosuffix must-suffix))))
	ad-do-it)
      ))
  )

;; Daredevil SKK / http://openlab.ring.gr.jp/skk/
;; ... or cvs -d:pserver:guest@openlab.jp:/circus/cvsroot login [guest]
;;        cvs -d:pserver:guest@openlab.jp:/circus/cvsroot co -d skk skk/main
(when (require 'skk-setup nil t)
  (setq skk-user-directory user-emacs-directory)
  (setq skk-init-file (expand-file-name "skk-init.el" skk-user-directory))
  (global-set-key [remap toggle-input-method] 'skk-mode)
  (global-set-key [remap skk-auto-fill-mode] 'ignore)
  
  ;; 実験的拡張へのロードパス追加(あれば)
  (ini:awhen (eval-when-compile (ini:library-within "skk" "experimental" t))
    (add-to-list 'load-path it))

  (add-hook 'skk-load-hook
	    (lambda ()
	      ;; ローカルの辞書設定
	      (let ((dict-dir (ini:emacs-d "etc/skk")))
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
		(setq skk-tooltip-function
		      (lambda (tooltip-str)
			(pos-tip-show tooltip-str))))
	      )))

;; migemo / http://0xcc.net/migemo/
;; ... or cvs -d:pserver:anonymous@migemo.cvs.sourceforge.net:/cvsroot/migemo login
;;        cvs -d:pserver:anonymous@migemo.cvs.sourceforge.net:/cvsroot/migemo co migemo
;;        git clone https://github.com/emacs-jp/migemo
;; cmigemo / http://www.kaoriya.net/software/cmigemo
;;           https://github.com/koron/cmigemo
(ini:when-when-compile (or (executable-find "cmigemo")
			   (executable-find "migemo")
			   (locate-library "migemo"))
  (defvar ini:org-isearch-lazy-highlight-search
    (symbol-function 'isearch-lazy-highlight-search)
    "migemo に置き換えられる前の `isearch-lazy-highlight-search'.")
  
  (when (require 'migemo nil t)
    (setq migemo-command (or (executable-find "cmigemo")
			     (executable-find "migemo")))

    (ini:when-when-compile (executable-find "cmigemo")
      (setq migemo-options '("-q" "--emacs"))
      (setq migemo-user-dictionary nil)
      (setq migemo-regex-dictionary nil)
      (setq migemo-coding-system 'utf-8-dos)
      (setq migemo-dictionary (eval-when-compile
				(locate-file "utf-8/migemo-dict"
					     `(,(ini:emacs-d "etc/migemo")
					       "/usr/local/share/migemo"
					       "/usr/share/migemo")
					     )))
      )
    (setq migemo-use-pattern-alist t)
    (setq migemo-use-frequent-pattern-alist t)
    (setq migemo-pattern-alist-length 1024)
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

;; direx / git clone https://github.com/m2ym/direx-el
(ini:when-when-compile (locate-library "direx")
  (autoload 'direx:jump-to-directory-other-window "direx" nil t)
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

;; yasnippet / git clone --recursive https://github.com/capitaomorte/yasnippet
(when (require 'yasnippet nil t)
  (setq yas-verbosity 1)
  (setq yas-prompt-functions (delq 'yas-x-prompt yas-prompt-functions))
  (setq yas-expand-only-for-last-commands '(self-insert-command ac-expand))
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
      "`auto-insert' 前にテンプレート一覧を更新する."
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

  (with-eval-after-load "yasnippet"		; for >0.8.0(beta)
    (if (and (fboundp 'yas--get-snippet-tables)
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
(ini:when-when-compile (and (executable-find "convert")
			    (locate-library "image+"))
  (with-eval-after-load "image"
    (require 'image+ nil t)
    (imagex-auto-adjust-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 追加関数定義

(when (display-multi-frame-p)
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
    (ini:when-when-compile (fboundp 'w32-send-sys-command)
      (w32-send-sys-command #xf030)))

  (defun ini:frame-size-restore ()
    "フレームサイズを通常に戻す."
    (interactive)
    (set-frame-parameter nil 'fullscreen nil)
    (ini:when-when-compile (fboundp 'w32-send-sys-command)
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

;; from http://www.fan.gr.jp/~ring/Meadow/meadow.html#ys:highlight-string
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
(cl-macrolet ((color-candidate (&rest colors)
			       (cl-find-if #'color-defined-p colors)))
  (require 'color)
  (when (and (eq (frame-parameter nil 'background-mode) 'dark)
	     (string= (frame-parameter nil 'cursor-color) "black")
	     (set-cursor-color "white")))

  (set-face-attribute 'region nil
		      :foreground (color-candidate "SystemHilightText" "White")
		      :background (color-candidate "SystemHilight" "Royal Blue"))

  (set-face-attribute 'isearch nil
		      :background (color-darken-name (color-candidate "SystemHilight"
								      "Royal Blue") 10)
		      :inherit 'region)
  
  (set-face-attribute 'lazy-highlight nil
		      :background (color-darken-name (color-candidate "SystemWindow"
								      "While") 40)
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
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; カスタマイズファイル読み込み
(setq custom-file (ini:emacs-d "custom.el"))
(load (file-name-sans-extension custom-file) t t)

;; 作業ディレクトリをホームディレクトリに
(ini:awhen (getenv "HOME") (cd it))

;;; init.el ends here
