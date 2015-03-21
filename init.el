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

(defmacro init:awhen (pred &rest body)
  "PRED を評価し、結果が non-nil ならば BODY を評価し、最後の式の結果を返す.
BODY 内では PRED の評価結果を `it' で参照出来る."
  (declare (indent 1))
  `(let ((it ,pred))
     (when it ,@body)))

(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\\<init:awhen\\>" . font-lock-keyword-face)))

(declare-function cygwin-convert-file-name-to-windows "cygw32.c")
(defmacro init:system-file-name (name &optional directory)
  "NAME をシステムで認識可能なファイルパスに変換する.
`expand-file-name' により DIRECTORY を基準にして絶対パスに変換される.
環境変数などの Emacs 外のプログラムに参照される場合に用いる."
  `(cond
    ((eq system-type 'windows-nt)
     (subst-char-in-string ?/ ?\\ (expand-file-name ,name ,directory)))
    ((eq system-type 'cygwin)
     (cygwin-convert-file-name-to-windows (expand-file-name ,name ,directory)))
    (t
     (expand-file-name ,name ,directory))))

(defmacro init:concat-system-file-names (names &optional directory original)
  "NAMES をシステムで認識可能なファイルパスの連結に変換する.
NAMES の各要素は自身と DIRECTORY を引数に `init:system-file-name' で処理される.
セパレータには `path-separator' が用いられる.
ORIGINAL が non-nil であれば最後に連結される."
  `(apply 'concat
          (mapconcat (lambda (name) (init:system-file-name name ,directory))
                     ,names path-separator)
          (if ,original
              (list path-separator ,original))))

(defmacro init:emacs-d (file)
  "~/.emacs.d 以下の FILE を返す."
  `(locate-user-emacs-file ,file))

(defmacro init:locate-directory (directory)
  "DIRECTORY が存在するなら返す."
  `(locate-file "." (delq nil (list ,directory)) nil
                (lambda (p) (when (file-exists-p p) 'dir-ok))))

(defmacro init:find-directory (directories)
  "DIRECTORIES のうち最初に見付かったディレクトリを返す."
  `(locate-file "." (delq nil (copy-sequence ,directories)) nil
                (lambda (p) (when (file-exists-p p) 'dir-ok))))

(defmacro init:make-silently-loading (func)
  "FUNC 内の `load' のメッセージ出力を強制的に抑制する."
  `(defadvice ,func (around
                     ,(intern (format "init:make-silently-loading-in-%s" (quote func)))
                     activate)
     "`load' 時のメッセージを抑制する."
     (let ((org-load (symbol-function 'load)))
       (cl-letf (((symbol-function 'load)
                  (lambda (file &optional noerror _nomessage nosuffix must-suffix)
                    (funcall org-load file noerror t nosuffix must-suffix))))
         ad-do-it))))

;; before emacs-24.4
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "FILE をロード後に BODY を評価する."
    (declare (indent 1))
    `(eval-after-load ,file
       `(funcall ,(lambda () ,@body))))

  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("with-eval-after-load" . 'font-lock-keyword-face))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ロードパス追加

;; ポータブル化
(when after-init-time
  (let ((this-file (or (buffer-file-name) load-file-name)))
    ;; .emacs.d を init.el が置いてある場所にする
    (setq user-emacs-directory (file-name-directory this-file))

    ;; -l init.el で起動したときも after-init-hook を実行する
    (setq after-init-hook nil)
    (with-eval-after-load this-file (run-hooks 'after-init-hook))

    ;; HOME 設定
    (unless (getenv "HOME")
      (setenv "HOME" (expand-file-name ".." user-emacs-directory)))))

;; (存在するなら) ~/.emacs.d/lisp および直下のディレクトリを load-path へ追加
;; データフォルダ等もあるので再帰的には追加しない
;; normal-top-level-add-subdirs-to-load-path は地味に遅いから使わない
(setq load-path
      (append
       (init:awhen (init:locate-directory (init:emacs-d "lisp"))
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
          width-adjustment-alist)
      ;; ベースとなる ASCII フォント
      (init:awhen (font-candidate "Consolas:pixelsize=15:weight=normal:slant=normal"
                                  "DejaVu Sans Mono-11:weight=normal:slant=normal")

        ;; ASCII
        (set-fontset-font fontset 'ascii it)

        ;; アクセント付きアルファベット類/ロシア語/ギリシャ語
        (init:awhen (font-candidate "Consolas"
                                    "DefaVu Sans Mono")
          (dolist (charset '(latin-iso8859-1
                             latin-iso8859-2
                             latin-iso8859-3
                             latin-iso8859-4
                             cyrillic-iso8859-5
                             greek-iso8859-7))
            (set-fontset-font fontset charset it)
            ;; 幅1に矯正
            (push (cons charset 1) width-adjustment-alist)))

        ;; 日本語その他 / meiryoKe_602r1.ttc
        ;; http://okrchicagob.blog4.fc2.com/blog-entry-121.html
        (init:awhen (font-candidate "MeiryoKe_Console"
                                    "VL ゴシック"
                                    "ＭＳ ゴシック")
          (set-fontset-font fontset 'unicode
                            `(,it . "iso10646-1") nil 'append))

        ;; fallback font
        (init:awhen (font-candidate "Arial Unicode MS")
          (set-fontset-font fontset 'unicode
                            `(,it . "iso10646-1") nil 'append))

        ;; ローマ数字は幅2
        (push '((#x2160 . #x216f) . 2) width-adjustment-alist) ; Ⅰ .. Ⅿ
        (push '((#x2170 . #x217f) . 2) width-adjustment-alist) ; ⅰ .. ⅿ

        ;; PinYin 発音記号
        (push '((#x01cd . #x01dc) . 2) width-adjustment-alist) ; Ǎ .. ǜ

        ;; 文字幅調整
        (when width-adjustment-alist
          (let ((table (make-char-table nil)))
            (dolist (pair width-adjustment-alist)
              (let ((target (car pair))
                    (width (cdr pair)))
                (cond
                 ((symbolp target)
                  (map-charset-chars (lambda (range _arg)
                                       (set-char-table-range table range width))
                                     target))
                 (t
                  (set-char-table-range table target width)))))
            (optimize-char-table table)
            (set-char-table-parent table char-width-table)
            (setq char-width-table table)))

        ;; フレームに設定
        (add-to-list 'default-frame-alist (cons 'font fontset)))))

  ;; ウィンドウサイズ
  ;; w32-resume-frame / git clone https://github.com/d5884/w32-resume-frame
  (if (require 'w32-resume-frame nil t)
      (w32-resume-frame-activate)
    (setq initial-frame-alist `((top . 60) (left . 120) ,@initial-frame-alist)))
  (setq default-frame-alist `((width . 100) (height . 40) ,@default-frame-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cygwin 連携
(when (eq system-type 'windows-nt)
  ;; cygwin へのパス等が通されていない前提で emacs 内で諸々を設定する

  ;; パイプ間の待ち時間を減らす
  (setq w32-pipe-read-delay 5)

  ;; インストールルート検索
  (let* ((cygdll (locate-file "cygwin1.dll" exec-path))
         (root-dir (or (and cygdll
                            (expand-file-name ".." (file-name-directory cygdll)))
                       (init:find-directory
                        (apply 'append
                               (mapcar (lambda (p)
                                         (list (expand-file-name "cygwin64" p)
                                               (expand-file-name "cygwin" p)))
                                       (list "c:/"
                                             "c:/gnupack/app/cygwin"
                                             (getenv "HOME")
                                             (getenv "USERPROFILE")
                                             (getenv "LOCALAPPDATA")
                                             (getenv "APPDATA")
                                             (getenv "ProgramFiles"))))))))
    (when root-dir
      ;; パスが通ってなければ通す
      (unless cygdll
        (let ((cygwin-exec-path
               (mapcar (lambda (path) (expand-file-name path root-dir))
                       '("~/bin" "usr/local/bin" "usr/bin" "bin"))))
          (setenv "PATH" (init:concat-system-file-names cygwin-exec-path nil (getenv "PATH")))
          (setq exec-path (append cygwin-exec-path exec-path))))

      ;; emacs のみで使用
      (add-to-list 'exec-path (expand-file-name (init:emacs-d "bin")))

      (unless (getenv "CYGWIN")
        (setenv "CYGWIN" "nodosfilewarning winsymlinks"))

      (setq null-device "/dev/null")

      ;; DOSコマンド混在のためプロセスでの出力のコードを未定に
      (setq default-process-coding-system
            (cons (coding-system-change-text-conversion
                   (car default-process-coding-system) 'undecided)
                  (cdr default-process-coding-system)))

      ;; comint での出力コード自動判別設定 (undecided なだけだと判定後変更されてしまう)
      (defadvice comint-send-input (before init:comint-send-detect-coding activate)
        "出力時の文字コードを自動判断に毎回戻す."
        (init:awhen (get-buffer-process (current-buffer))
          (set-process-coding-system it
                                     (coding-system-change-text-conversion
                                      (car default-process-coding-system) 'undecided)
                                     (cdr (process-coding-system it)))))

      ;; ファイル名のエンコーディングを実態とあわせる
      ;; ls-lisp-use-insert-directory-program が non-nil な場合向け
      (when (boundp 'w32-unicode-filenames)
        (set-file-name-coding-system 'utf-8))

      ;; shell
      (setq shell-file-name "bash")
      (setq shell-command-switch "-c")
      (setq system-uses-terminfo nil)

      (setenv "SHELL" shell-file-name)

      (with-eval-after-load "term"
        (defadvice cd (around init:cd-accept-multibyte activate)
          "`term' で/proc等に移動時の強制終了を防ぐ."
          (unless (ignore-errors ad-do-it)
            (ad-set-arg 0 "~/")
            ad-do-it))

        (defadvice term-emulate-terminal (around init:terminal-detect-coding activate)
          "`term' で複数のコーディング出力を受け付ける."
          (let ((locale-coding-system 'undecided))
            ad-do-it)))

      (with-eval-after-load "tramp"
        (setq tramp-encoding-shell "bash"))

      ;; gdb 使用時のエラー回避
      (with-eval-after-load "gdb-mi"
        (declare-function gdb-input "gdb-mi")
        (add-hook 'gdb-mode-hook
                  (lambda ()
                    (gdb-input "-gdb-set interactive-mode auto" 'ignore))))

      ;; cygwin で追加される Info
      (with-eval-after-load "info"
        (init:awhen (init:locate-directory "/usr/share/info")
          (add-to-list 'Info-additional-directory-list it)))

      ;; NTEmacs の場合、プロセスの引数は起動した環境のコードページに依存するため
      ;; プロセス呼び出し時に引数のみ locale-coding-system へ強制変換する
      ;; クォート処理は elisp 側で行う (ダメ文字対策)
      (defvar init:cygcheck-cache nil
        "cygcheck の結果のキャッシュ.")

      (defun init:cygwin-program-p (filename)
        "FILENAME が cygwin のプログラムかどうか判定する."
        (let* ((target (executable-find filename))
               (cache (assoc target init:cygcheck-cache))
               (w32-quote-process-args nil)) ; advice 中で再帰しないよう nil
          (when target
            (unless cache
              (setq cache (cons target
                                (with-temp-buffer ; cygwin のライブラリをロードしているか判定
                                  (when (eq 0 (call-process "ldd" nil t nil
                                                            (concat "\"" target "\"")))
                                    (goto-char (point-min))
                                    (number-or-marker-p
                                     (re-search-forward "cygwin[0-9]+\.dll" nil t))))))
              (push cache init:cygcheck-cache))
            (cdr cache))))

      ;; func / prog / arg
      (dolist (desc '((call-process-region 2 6)
                      (call-process 0 4)
                      (start-process 2 3)))
        (let ((f (car desc))
              (p (nth 1 desc))
              (a (nth 2 desc)))
          (eval `(defadvice ,f (around ,(intern (format "init:%s-arguments-setup" f))
                                       activate)
                   ,(format "実行時に%d番目以降の引数を `locale-coding-system' でエンコードする."
                            (1+ a))
                   (let ((cygwin-quote (and w32-quote-process-args ; cygwin-program-p の再帰防止
                                            (init:cygwin-program-p (ad-get-arg ,p)))))
                     (ad-set-args ,a
                                  (mapcar
                                   (lambda (arg)
                                     (when w32-quote-process-args
                                       (setq arg
                                             (concat "\""
                                                     (if cygwin-quote
                                                         (replace-regexp-in-string
                                                          "[\"\\\\]" "\\\\\\&" arg)
                                                       (replace-regexp-in-string
                                                        "\\(\\(\\\\\\)*\\)\"" "\\1\\\\\\&" arg))
                                                     "\"")))
                                     (if (multibyte-string-p arg)
                                         (encode-coding-string arg locale-coding-system)
                                       arg))
                                   (ad-get-args ,a)))
                     (let ((w32-quote-process-args nil))
                       ad-do-it))))))

      (when (version<= "24.4" emacs-version) ; 24.4 「から」発生
        (defconst w32-pipe-limit 4096
          "Windows でのパイプバッファサイズ.")

        (defadvice process-send-string (around init:workaround-for-process-send-string activate)
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
              ad-do-it))))

      ;; cygwin-mount / (package-install 'cygwin-mount)
      (when (require 'cygwin-mount nil t)
        (cygwin-mount-activate))

      ;; fakecygpty
      ;; gcc -o fakecygpty.exe fakecygpty.c
      ;; gcc -o qkill.exe qkill.c
      ;; git clone https://github.com/d5884/fakecygpty
      (when (require 'fakecygpty nil t)
        (fakecygpty-activate)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; パス追加
(add-to-list 'exec-path (expand-file-name (init:emacs-d "bin")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; キーバインド変更

;; Ctrl-h を DEL に
(when (load "term/bobcat" nil t)
  (terminal-init-bobcat))

;; ignore はヘルプ等に表示しない
(put 'ignore 'suppress-keymap t)

(global-set-key (kbd "C-x ?") 'help-command)
(global-set-key (kbd "C-x 7") 'toggle-truncate-lines)
(global-set-key (kbd "C-x C-z") 'compile)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "<pause>") 'toggle-debug-on-error)
(global-set-key [remap list-buffers] 'bs-show)
(global-set-key (kbd "C-z z") (defun init:ansi-term ()
                                "`ansi-term' を実行する."
                                (interactive)
                                (ansi-term shell-file-name)))
(global-set-key (kbd "<apps>") (defun init:show-apps-menu ()
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
                    (defun init:show-edit-menu (event &optional prefix)
                      "編集メニューを表示する."
                      (interactive "@e")
                      (popup-menu menu-bar-edit-menu event prefix))))

  ;; Ctrl+Wheel で文字サイズ変更
  (when (display-multi-font-p)
    (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
    (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
    (global-unset-key (kbd "<C-down-mouse-2>"))
    (global-set-key (kbd "<C-mouse-2>") (defun init:text-scale-reset ()
                                          "テキストのスケーリングをリセットする."
                                          (interactive)
                                          (text-scale-set 0)))))

(when (display-multi-frame-p)
  (global-set-key [remap save-buffers-kill-emacs]
                  (defun init:close-or-exit-emacs (&optional arg)
                    "フレームが一つなら emacs を終了、それ以外はフレームを閉じる.
ARG が non-nil の場合はフレームの数に関係なく emacs を終了する."
                    (interactive "P")
                    (if (or arg (eq 1 (length (frame-list))))
                        (save-buffers-kill-terminal)
                      (delete-frame))))

  (global-set-key (kbd "C-z C-:")
                  (if (fboundp 'toggle-frame-maximized)
                      'toggle-frame-maximized
                    (defun toggle-frame-maximized ()
                      "フレームサイズの最大化状態を切り替える."
                      (interactive)
                      (if (not (frame-parameter nil 'fullscreen))
                          (progn
                            (and (fboundp 'w32-send-sys-command)
                                 (w32-send-sys-command #xf030))
                            (set-frame-parameter nil 'fullscreen 'maximized))
                        (and (fboundp 'w32-send-sys-command)
                             (w32-send-sys-command #xf120))
                        (set-frame-parameter nil 'fullscreen nil))))))

(global-set-key (kbd "C-z C-;") 'suspend-frame)

;; IME関連キーの整理
(global-set-key (kbd "<enlw>") 'toggle-input-method)  ; 半角/全角
(global-set-key (kbd "<auto>") 'toggle-input-method)  ; 半角/全角
(global-set-key (kbd "<kanji>") 'toggle-input-method) ; 半角/全角
(global-set-key (kbd "<M-kanji>") 'ignore)            ; Alt+半角/全角
(global-set-key (kbd "<convert>") 'ignore)            ; 無変換
(global-set-key (kbd "<muhenkan>") 'ignore)           ; 無変換
(global-set-key (kbd "<non-convert>") 'ignore)        ; 変換
(global-set-key (kbd "<henkan>") 'ignore)             ; 変換
(global-set-key (kbd "<copy>") 'ignore)               ; カタカナ/ひらがな/ローマ字

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 各種機能/パッケージ別

;; diminish / (package-install 'diminish)
(unless (package-installed-p 'diminish)
  (defalias 'diminish 'ignore))

;; 編集系色々
(setq comment-style 'multi-line)
(setq kill-do-not-save-duplicates t)
(setq require-final-newline t)
(setq search-invisible t)
(setq truncate-partial-width-windows nil)
(setq visible-bell t)
(setq disabled-command-function nil)
(setq history-length 1000)

;; バックアップ関係
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; 起動画面関係
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

(defadvice display-startup-echo-area-message (around init:shut-up-echo-message activate)
  "起動時のエコーエリアのメッセージを表示しない.
`inhibit-startup-echo-area-message' はユーザ名をリテラルで書く必要があるので
Daemon 起動時以外は表示関数を直接潰す"
  (when (daemonp) ad-do-it))

;; スクロール関係
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 35)
(setq scroll-margin 0)
(setq scroll-step 1)
(setq next-screen-context-lines 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))
(setq recenter-positions '(middle top))

;; フレーム/カーソル関連
(setq frame-title-format "%b")
(blink-cursor-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(when (featurep 'scroll-bar)
  (scroll-bar-mode -1))
(mouse-avoidance-mode 'exile)

;; モードライン関係
(column-number-mode t)

;; ansi-color
(with-eval-after-load "comint"
  (require 'ansi-color nil t)
  (when (eq 'light (frame-parameter nil 'background-mode))
    (setq ansi-color-names-vector
          ["black"          ; black
           "dark red"       ; red
           "dark green"     ; green
           "dark goldenrod" ; yellow
           "dark blue"      ; blue
           "dark magenta"   ; magenta
           "dark cyan"      ; cyan
           "white"])        ; white
    (setq ansi-color-map (ansi-color-make-color-map)))
  (ansi-color-for-comint-mode-on))

;; apropos
(with-eval-after-load "apropos"
  (setq apropos-do-all t)
  (define-key apropos-mode-map (kbd "n") 'forward-button)
  (define-key apropos-mode-map (kbd "p") 'backward-button))

;; auth-source
(with-eval-after-load "auth-source"
  (setq auth-sources (cons (init:emacs-d "authinfo.gpg") auth-sources)))

;; auto-complete-mode / (package-install 'auto-complete)
(when (require 'auto-complete-config nil t)
  (diminish 'auto-complete-mode)

  (ac-config-default)

  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (define-key ac-completing-map (kbd "C-g") 'ac-stop)

  (setq ac-use-menu-map t)

  (when (require 'pos-tip nil t)
    (setq ac-quick-help-prefer-pos-tip t)))

;; calculator
(global-set-key (kbd "C-z C-c") 'calculator)

(with-eval-after-load "calculator"
  (declare-function calculator-save-and-quit "calculator")
  ;; C-ret は cua-mode に上書きされてるので
  (define-key calculator-mode-map [remap calculator]
    (defun init:calculator-quit-and-yank ()
      "`calculator' を終了して結果を `yank' する."
      (interactive)
      (calculator-save-and-quit)
      (yank))))

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

  (setq diary-file (init:emacs-d "diary"))
  (setq diary-entry-marker 'link)
  (setq diary-list-include-blanks t)
  (setq calendar-mark-diary-entries-flag t)

  ;; 日本の祝日表示 / (package-install 'japanese-holidays)
  (when (require 'japanese-holidays nil t)
    (setq calendar-holidays
          (append japanese-holidays holiday-local-holidays holiday-other-holidays))

    (add-hook 'today-visible-calendar-hook 'japanese-holiday-mark-weekend)
    (add-hook 'today-invisible-calendar-hook 'japanese-holiday-mark-weekend)))

;; cc-mode
(with-eval-after-load "cc-mode"
  (add-hook 'c-mode-common-hook
            (lambda ()
              (hs-minor-mode t)
              (hide-ifdef-mode t)))
  ;; flycheck
  (when (package-installed-p 'flycheck)
    (add-hook 'c-mode-common-hook 'flycheck-mode)))

;; completion
(setq completion-show-help nil)

;; compile
(with-eval-after-load "compile"
  (setq compilation-scroll-output t)
  (define-key compilation-mode-map (kbd "n") 'next-error)
  (define-key compilation-mode-map (kbd "p") 'previous-error)

  (dolist (func '(compile recompile))
    (eval `(defadvice ,func (around ,(intern (format "init:%s-silently" func)) activate)
             "エラー発生時のみ *compilation* バッファ表示."
             (let ((compilation-start-hook
                    compilation-start-hook)) ; compile/recompile から呼ばれたとき専用
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
               (cl-letf (((symbol-function 'display-buffer) 'ignore)
                         ((symbol-function 'set-window-start) 'ignore)
                         ((symbol-function 'set-window-point) 'ignore))
                 ad-do-it))))))

;; cua-mode
(when (require 'cua-base nil t)
  (cua-selection-mode t))

;; dired
(global-set-key (kbd "C-x C-d") 'dired-other-window)

(with-eval-after-load "dired"
  (declare-function browse-url-default-browser "browse-url")
  (declare-function dired-get-file-for-visit "dired")

  (setq dired-dwim-target t)
  (setq dired-isearch-filenames t)
  (setq dired-listing-switches "-lAF")
  (if (and (featurep 'ls-lisp)
           (not ls-lisp-use-insert-directory-program))
      (progn
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

  (autoload 'browse-url-default-browser "browse-url")
  (define-key dired-mode-map (kbd "E")
    (defun init:dired-execute (arg)
      "ファイルを関連付けされたプログラムで開く.
プリフィクスキーが入力されている場合はカレントディレクトリを開く."
      (interactive "P")
      (browse-url-default-browser (if arg "." (dired-get-file-for-visit)))))

  (define-key dired-mode-map (kbd "v")
    (defun init:dired-view-other-window ()
      "別ウィンドウでファイルを閲覧する."
      (interactive)
      (view-file-other-window (dired-get-file-for-visit))))

  (define-key dired-mode-map (kbd "q") 'kill-this-buffer)

  ;; dired のバッファを無駄に増やさないため、移動時に移動前のバッファを消す
  (dolist (f '(dired-find-file dired-up-directory))
    (eval `(defadvice ,f (around ,(intern
                                   (format "init:%s-and-kill" f))
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
                     (dired-sort-other (setq dired-actual-switches switch))))))))

  ;; find-dired
  (with-eval-after-load "find-dired"
    (setq find-ls-option (cons (format "-exec ls %sd --time-style=long-iso {} +"
                                       dired-listing-switches)
                               (format "%sd" dired-listing-switches)))

    (when (require 'grep nil t)
      (defadvice find-grep-dired (around init:find-grep-replace activate)
        "lgrep がちゃんと動かないので普通の grep に置き換え."
        (let ((grep-program "grep"))
          ad-do-it))))

  ;; dired-x は導入するが C-x C-j は skk 等で使用
  (let ((cxcj (key-binding (kbd "C-x C-j"))))
    (when (require 'dired-x nil t)
      (global-set-key (kbd "C-x C-j") cxcj))))

;; ediff
(with-eval-after-load "ediff"
  (declare-function ediff-with-current-buffer "ediff")

  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)

  (defadvice ediff-find-file (around init:ediff-mark-newly-opened activate)
    "ediff が開いたファイルを quit 時に削除できるようフラグを付ける."
    (let ((existing-p (and find-file-existing-other-name
                           (find-buffer-visiting (symbol-value (ad-get-arg 0))))))
      ad-do-it
      (unless existing-p
        (ediff-with-current-buffer (symbol-value (ad-get-arg 1))
          (setq-local init:ediff-kill-on-quit t)))))

  (defvar init:ediff-window-configuration-stash nil
    "`ediff' 実行前のウィンドウ状態の一時保存先.")

  (add-hook 'ediff-before-setup-hook
            (lambda ()
              (setq init:ediff-window-configuration-stash
                    (current-window-configuration))))
  (add-hook 'ediff-quit-hook
            (lambda ()
              (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-ancestor-buffer))
                (ediff-with-current-buffer buf
                  (when (and (boundp 'init:ediff-kill-on-quit)
                             init:ediff-kill-on-quit)
                    (kill-buffer))))
              (set-window-configuration init:ediff-window-configuration-stash))))

;; eldoc
(with-eval-after-load "eldoc"
  (diminish 'eldoc-mode))

;; elisps
(defun init:byte-compile-current-file-if-necessary ()
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
                        'init:byte-compile-current-file-if-necessary
                        nil t))))

;; eval-expression
(add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

;; eww
(with-eval-after-load "eww"
  (setq eww-search-prefix "http://www.google.co.jp/search?q=")

  ;; x-euc-jp は ddskk-ml の過去ログ等で使われているので…
  (define-coding-system-alias 'x-euc-jp 'euc-jp)

  (defadvice eww-display-html (after init:eww-change-buffer-coding-system activate)
    "エンコーディングをバッファに記録する."
    (set-buffer-file-coding-system (ad-get-arg 0)))

  (defadvice eww-submit (around init:eww-override-find-coding-systems-string activate)
    "バッファと同じエンコーディングで submit する."
    (cl-letf (((symbol-function 'find-coding-systems-string)
               (lambda (_string)
                 (list buffer-file-coding-system))))
      ad-do-it)))

;; executable-make-buffer-executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; ffap
(when (require 'ffap nil t)
  (ffap-bindings))

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
    (global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)))

;; flycheck / (package-install 'flycheck)
(when (package-installed-p 'flycheck)
  (with-eval-after-load "flycheck"
    (defadvice flycheck-start-command-checker (around init:flycheck-de-localize activate)
      "認識されないのでローカライズを解除する."
      (let ((process-environment process-environment))
        (setenv "LC_ALL" "C")
        ad-do-it))

    ;; flycheck-pos-tip / (package-install 'flycheck-pos-tip)
    (when (package-installed-p 'flycheck-pos-tip)
      (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages))))

;; gdb
(with-eval-after-load "gdb-mi"
  (declare-function gdb-many-windows "gdb-mi")
  (add-hook 'gdb-mode-hook (lambda ()
                             (gdb-many-windows)
                             (gud-tooltip-mode))))

;; gnus and mail (for gmail)
(setq mail-user-agent 'gnus-user-agent)
(setq read-mail-command 'gnus)

(with-eval-after-load "gnus"
  (setq gnus-startup-file (init:emacs-d "gnus/newsrc"))
  (setq gnus-directory (init:emacs-d "gnus/news"))
  (setq gnus-save-newsrc-file nil)


  (setq gnus-select-method '(nnimap "gmail"
                                    (nnimap-address "imap.gmail.com")
                                    (nnimap-server-port 993)
                                    (nnimap-stream ssl)))
  (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"))

(with-eval-after-load "message"
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq message-auto-save-directory nil))

(with-eval-after-load "sendmail"
  (setq send-mail-function 'smtpmail-send-it))

(with-eval-after-load "smtpmail"
  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-stream-type 'tls)
  (setq smtpmail-local-domain "gmail.com"))

;; gnutls
(when (eq system-type 'windows-nt)
  (with-eval-after-load "gnutls"
    ;; gnutls の dll が dos 形式のパスを要求するため.
    (setq gnutls-trustfiles (mapcar 'expand-file-name gnutls-trustfiles))))

;; grep
(with-eval-after-load "grep"
  (declare-function grep-apply-setting "grep")

  (when (executable-find "lgrep")
    ;; lv 付属の多国語化 grep
    (setq grep-program "lgrep")
    (grep-apply-setting 'grep-command "lgrep -n -Au8 -Ia ")
    (grep-apply-setting 'grep-template
                        (format "%s <C> <R> - <F>" grep-command))
    (grep-apply-setting 'grep-find-template
                        (format "find . <X> -type f <F> -exec %s <C> <R> - <N> {} +"
                                grep-command))))

;; hideshow
(with-eval-after-load "hideshow"
  (diminish 'hs-minor-mode)

  (defvar init:hs-fringe-mark 'right-arrow
    "隠れた行の fringe に表示する bitmap 名.
`fringe-bitmaps' 内に設定されているシンボル名から選ぶ.")

  (defun init:hs-mark-fringe (ovr)
    "`hs-toggle-hiding'で隠された行の OVR を編集して fringe にマークを付ける."
    (when (eq 'code (overlay-get ovr 'hs))
      (let ((hiding-text "...")
            (fringe-anchor (make-string 1 ?x)))
        (put-text-property 0 1 'display (list 'left-fringe init:hs-fringe-mark) fringe-anchor)
        (overlay-put ovr 'before-string fringe-anchor)
        (overlay-put ovr 'display hiding-text))))

  (setq hs-set-up-overlay 'init:hs-mark-fringe)
  (define-key hs-minor-mode-map (kbd "C-z <C-SPC>") 'hs-toggle-hiding))

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

  (defadvice ido-exhibit (after init:ido-exhibit-display-buffer activate)
    "選択しているバッファをウィンドウに表示する."
    (when ido-matches
      (let ((selected (get-buffer-window
                       (cl-find-if-not #'minibufferp (buffer-list)))))
        (when selected
          (select-window selected)
          (ido-visit-buffer
           (get-buffer (car ido-matches)) t)
          (select-window (minibuffer-window)))))))

;; image+ / (package-install 'image+)
(with-eval-after-load "image"
  (when (and (executable-find "convert")
             (require 'image+ nil t))
    (imagex-auto-adjust-mode t)))

;; imenu
(global-set-key (kbd "C-z C-j") 'imenu)
(with-eval-after-load "imenu"
  (setq imenu-auto-rescan t))

;; info
(with-eval-after-load "info"
  (init:awhen (init:locate-directory (init:emacs-d "info"))
    (add-to-list 'Info-additional-directory-list it)))

;; isearch
(with-eval-after-load "isearch"
  (setq isearch-allow-scroll t)
  (setq lazy-highlight-initial-delay 0))

;; ispell
(with-eval-after-load "ispell"
  ;; from http://www.an.econ.kobe-u.ac.jp/~namba/meadow/words.lzh
  (init:awhen (locate-file "words"
                           `(,(init:emacs-d "share")
                             ,user-emacs-directory
                             "/usr/dict"
                             "/usr/share/dict"))
    (setq ispell-alternate-dictionary it)))

;; magit / (package-install 'magit)
(when (package-installed-p 'magit)
  (global-set-key (kbd "C-z C-m") 'magit-status)

  (with-eval-after-load "magit"
    (setq magit-auto-revert-mode-lighter "")))

;; man & woman
(with-eval-after-load "woman"
  (setq woman-fill-frame t))

;; markdown-mode / (package-install 'markdown-mode)
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.\\(md\\(wn\\|t\\)?\\|markdown\\|text\\)\\'" .
                                  markdown-mode)))

;; migemo / (package-install 'migemo)
;; cmigemo / http://www.kaoriya.net/software/cmigemo
(init:awhen (or (executable-find "cmigemo")
                (executable-find "migemo"))
  (defvar init:org-isearch-lazy-highlight-search
    (symbol-function 'isearch-lazy-highlight-search)
    "migemo に置き換えられる前の `isearch-lazy-highlight-search'.")

  (when (require 'migemo nil t)
    (setq migemo-command it)

    (when (string-match-p "cmigemo" it)
      (setq migemo-options '("-q" "--emacs"))
      (setq migemo-user-dictionary nil)
      (setq migemo-regex-dictionary nil)
      (setq migemo-coding-system 'utf-8)
      (setq migemo-dictionary
            (init:system-file-name (locate-file "utf-8/migemo-dict"
                                                `(,(init:emacs-d "share/migemo")
                                                  "/usr/local/share/migemo"
                                                  "/usr/share/migemo")))))

    (setq migemo-use-pattern-alist t)
    (setq migemo-use-frequent-pattern-alist t)
    (setq migemo-pattern-alist-length 1024)
    (setq migemo-pattern-alist-file (init:emacs-d "migemo-pattern"))
    ;; compatible key with kogiku
    (define-key isearch-mode-map (kbd "M-k") 'migemo-isearch-toggle-migemo)

    ;; pty を消費しない
    (let ((process-connection-type nil))
      (migemo-init))

    ;; query-replace 系での lazy-highlight 対応
    (dolist (fn '(query-replace query-replace-regexp))
      (eval `(defadvice ,fn (around ,(intern (format "init:%s-with-migemo"
                                                     fn)) activate)
               "migemo 導入時でもハイライトを有効にする."
               (cl-letf (((symbol-function 'isearch-lazy-highlight-search)
                          init:org-isearch-lazy-highlight-search))
                 ad-do-it))))

    (with-eval-after-load "isearch"
      ;; isearch 中に leim を使用しない
      (define-key isearch-mode-map [remap toggle-input-method] 'undefined)
      (define-key isearch-mode-map [remap isearch-toggle-input-method] 'undefined)
      (define-key isearch-mode-map [remap isearch-toggle-specified-input-method]
        'undefined))

    (defadvice isearch-lazy-highlight-update (around init:suppress-error-isearch-regexp activate)
      "正規表現検索時のエラー回避."
      (ignore-errors
        ad-do-it))))

;; mozc / (package-install 'mozc)
;;    and http://www49.atwiki.jp/ntemacs?cmd=upload&act=open&pageid=50&file=mozc_emacs_helper.zip
(when (and (executable-find "mozc_emacs_helper")
           (require 'mozc nil t))
  (setq default-input-method "japanese-mozc")
  (setq mozc-leim-title "[あ]")

  (when (memq system-type '(windows-nt cygwin))
    (defadvice mozc-session-execute-command (after init:mozc-session-execute-command activate)
      "`mozc' を有効化した際に自動的にひらがな入力モードに変更する."
      (if (eq (ad-get-arg 0) 'CreateSession)
          (mozc-session-sendkey '(hiragana)))))

  (defadvice mozc-mode (after init:mozc-minibuffer-workaround activate)
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

  (defadvice mozc-leim-deactivate (around init:mozc-deactive-workaround activate)
    "正しく `mozc-mode' を終了させる."
    (mozc-mode -1))

  (defadvice mozc-helper-process-recv-response (after init:mozc-accept-output-workaround activate)
    "他プロセスが終了した際に accept-process-output がタイムアウトする問題対策."
    (unless ad-return-value
      (setq ad-return-value (mozc-helper-process-recv-response))))

  ;; mozc-popup / (package-install 'mozc-popup)
  (when (and (require 'popup nil t)
             (require 'mozc-popup nil t))
    (setq mozc-candidate-style 'popup)))

;; nxml-mode
(fset 'html-mode 'nxml-mode)
(fset 'xml-mode 'nxml-mode)

(setq magic-mode-alist
      (cons '("<\\?xml" . nxml-mode) magic-mode-alist))

(with-eval-after-load "nxml-mode"
  (with-eval-after-load "smart-compile"
    (setq smart-compile-alist (cons
                               '(nxml-mode browse-url-of-buffer)
                               smart-compile-alist))))

;; occur
(define-key occur-mode-map (kbd "n") 'occur-next)
(define-key occur-mode-map (kbd "p") 'occur-prev)
(add-hook 'occur-mode-hook 'next-error-follow-minor-mode)

;; popup-kill-ring / (package-install 'popup-kill-ring)
(when (require 'popup-kill-ring nil t)
  (setq popup-kill-ring-interactive-insert t)
  (global-set-key (kbd "M-y") 'popup-kill-ring)
  (define-key popup-kill-ring-keymap (kbd "TAB") 'popup-kill-ring-next)
  (define-key popup-kill-ring-keymap (kbd "M-y") 'popup-kill-ring-next))

;; popwin / (package-install 'popwin)
(when (require 'popwin nil t)
  (global-set-key (kbd "C-z C-s") 'popwin:stick-popup-window)
  (global-set-key [remap view-echo-area-messages] 'popwin:messages)
  (setq popwin:reuse-window nil)
  (setq popwin:special-display-config
        (append
         '((calendar-mode :stick t)
           ("*Shell Command Output*" :stick t :noselect t)
           ("*Occur*" :stick t)         ; not mode because occur-edit
           (apropos-mode :stick t)
           (grep-mode :stick t)
           (compilation-mode :stick t :noselect t)
           (help-mode :stick t)
           (dired-mode :stick t))
         popwin:special-display-config))
  ;; special-mode 派生はとりえあえず全部 popwin 管轄下へ
  (add-to-list 'popwin:special-display-config
               (list (lambda (b)
                       (with-current-buffer b
                         (derived-mode-p 'special-mode)))) t)
  (popwin-mode 1))

;; quail-japanese
(with-eval-after-load "japanese"
  (setq quail-japanese-use-double-n t))

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
                   nil)))

  (add-hook 'ruby-mode-hook 'hs-minor-mode)

  ;; flycheck
  (when (package-installed-p 'flycheck)
    (add-hook 'ruby-mode-hook 'flycheck-mode)))

;; savehist
(savehist-mode 1)
(add-hook 'savehist-save-hook
          (lambda ()
            ;; 保存したくないファイル名パターン
            (setq file-name-history
                  (cl-delete-if (apply-partially 'string-match-p
                                                 (concat "\\<"
                                                         (regexp-opt '("COMMIT_EDITMSG"))
                                                         "\\'"))
                                file-name-history))))

;; saveplace
(when (require 'saveplace nil t)
  (setq save-place-file (init:emacs-d "places"))
  (setq-default save-place t))

;; sdic / http://www.namazu.org/~tsuchiya/sdic/
(when (locate-library "sdic")
  (autoload 'sdic-describe-word "sdic" nil t)
  (autoload 'sdic-describe-word-at-point "sdic" nil t)
  (global-set-key (kbd "C-z w") 'sdic-describe-word)
  (global-set-key (kbd "C-z C-w") 'sdic-describe-word-at-point)

  (with-eval-after-load "sdic"
    (setq sdic-default-coding-system 'utf-8-unix)
    (setq sdic-eiwa-dictionary-list `((sdicf-client ,(init:emacs-d "share/sdic/gene-u.sdic")
                                                    (strategy direct))))
    (setq sdic-waei-dictionary-list `((sdicf-client ,(init:emacs-d "share/sdic/jedict-u.sdic")
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
      (defadvice sdic-other-window (around init:sdic-other-normalize
                                           activate)
        "`sdic' のバッファ移動を普通の操作にする."
        (other-window 1))

      (defadvice sdic-close-window (around init:sdic-close-normalize
                                           activate)
        "`sdic' のバッファクローズを普通の操作にする."
        (bury-buffer sdic-buffer-name))

      (defadvice sdic-display-buffer (around init:sdic-display-normalize
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
          (goto-char p))))))

;; server
(when (and (require 'server nil t)
           (memq (server-running-p) '(nil :other)))
  (server-start))

;; shell-pop / (package-install 'shell-pop)
(when (package-installed-p 'shell-pop)
  (global-set-key (kbd "C-z C-z") 'shell-pop)
  (with-eval-after-load "shell-pop"
    (setq shell-pop-internal-mode "ansi-term")
    (setq shell-pop-internal-mode-buffer "*ansi-term*")
    (setq shell-pop-internal-mode-func (lambda () (ansi-term shell-file-name)))
    (setq shell-pop-autocd-to-working-dir nil)

    (defadvice shell-pop-out (around init:safe-pop-out activate)
      "戻り先の window が死んでいたら window を消すだけにする."
      (if (one-window-p)
          (switch-to-buffer shell-pop-last-buffer)
        (if (window-live-p shell-pop-last-window)
            ad-do-it
          (delete-window))))))

;; shell/term
(defun init:add-process-sentinel (process sentinel)
  "PROCESS に センチネル SENTINEL を追加する.
SENTINEL は元々設定されていたセンチネルが実行されてから呼び出される."
  (let ((org-sentinel (and (processp process)
                           (process-sentinel process))))
    (set-process-sentinel process
                          (if org-sentinel
                              `(lambda (proc msg)
                                 (funcall (function ,org-sentinel) proc msg)
                                 (funcall (function ,sentinel) proc msg))
                            sentinel))))

(defun init:set-process-cleaner (&optional process)
  "PROCESS 終了時にバッファとウィンドウを削除する.
また Emacs 終了時にプロセスも終了させる.
PROCESS が nil の場合はカレントバッファのプロセスに設定する."
  (init:awhen (or (and (processp process)
                       process)
                  (get-buffer-process (current-buffer)))
    (set-process-query-on-exit-flag it nil)
    ;; PROCESS のバッファを削除し、ウィンドウが開いていたら閉じる
    (init:add-process-sentinel it
                               (lambda (process _msg)
                                 (let ((buf (process-buffer process)))
                                   (when (and buf (buffer-live-p buf))
                                     (dolist (win (get-buffer-window-list buf))
                                       (unless (one-window-p)
                                         (delete-window win))
                                       (kill-buffer buf))))))))

;; shell
(with-eval-after-load "shell"
  (setq comint-prompt-read-only t)

  (add-hook 'shell-mode-hook 'init:set-process-cleaner)
  (define-key shell-mode-map (kbd "M-p") 'comint-previous-matching-input-from-input)
  (define-key shell-mode-map (kbd "M-n") 'comint-next-matching-input-from-input))

;; term
(with-eval-after-load "term"
  (add-hook 'term-exec-hook 'init:set-process-cleaner)

  ;; C-c に C-x を取り込まない
  (set-keymap-parent term-raw-escape-map nil)
  ;; char-mode で使いたいキーを開放して C-c に移動
  (dolist (key '("M-x" "M-:" "C-z" "C-u" "C-\\"))
    (define-key term-raw-map (kbd key) nil)
    (define-key term-raw-map (kbd (concat "C-c " key))
      (if (string-match-p "^M-" key)
          'term-send-raw-meta
        'term-send-raw))))

;; show-paren
(setq show-paren-delay 0)
(show-paren-mode t)

;; skk / (package-install 'ddskk)
;; 辞書 / cvs -d:pserver:guest@openlab.jp:/circus/cvsroot login [guest]
;;        cvs -d:pserver:guest@openlab.jp:/circus/cvsroot co -d ~/.emacs.d/share/skk skk/dic
(when (require 'skk-leim nil t)
  (setq skk-user-directory user-emacs-directory)
  (setq skk-init-file (expand-file-name "skk-init.el" skk-user-directory))

  (global-set-key (kbd "C-x C-j") (defun init:force-skk-activate ()
                                    "強制的に `current-input-method' を `skk-mode' にする."
                                    (interactive)
                                    (if (equal current-input-method "japanese-skk")
                                        (deactivate-input-method)
                                      (when current-input-method
                                        (deactivate-input-method))
                                      (set-input-method "japanese-skk"))))

  (add-hook 'skk-load-hook
            (lambda ()
              ;; ローカルの辞書設定
              (let ((dict-dir (init:emacs-d "share/skk")))
                (init:awhen (locate-file "SKK-JISYO.L" (list dict-dir))
                  (setq skk-large-jisyo it
                        skk-aux-large-jisyo it))

                ;; 追加の通常辞書 (あれば)
                (setq skk-extra-jisyo-file-list
                      (cl-remove-if-not #'file-exists-p
                                        (mapcar
                                         (lambda (f)
                                           (expand-file-name
                                            (concat "SKK-JISYO." f) dict-dir))
                                         '("JIS2" "JIS2004" "JIS3_4"
                                           "assoc" "geo" "jinmei" "station"
                                           "law" "fullname" "propernoun"
                                           "okinawa" "edict")))))

              (when (require 'skk-study nil t)
                (setq skk-study-backup-file nil))
              (when (require 'skk-tankan nil t) ; Tan@ or /10@ or /@@
                (add-to-list 'skk-search-prog-list
                             '(skk-tankan-search 'skk-search-jisyo-file
                                                 skk-large-jisyo 10000)))
              (require 'skk-hint nil t) ; ▽はやま<SPC>;はし<SPC>
              ;; (require 'context-skk)

              (setq skk-latin-mode-string "A")
              (setq skk-abbrev-mode-string "@")
              (setq skk-hiragana-mode-string "あ")
              (setq skk-katakana-mode-string "ア")
              (setq skk-jisx0208-latin-mode-string "Ａ")
              (setq skk-jisx0201-mode-string "ｱｧ")
              (setq skk-indicator-use-cursor-color nil)

              (defadvice skk-mode-string-to-indicator (before init:skk-mode-elimit-hyphen
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

              (when (require 'pos-tip nil t)
                (setq skk-tooltip-function 'pos-tip-show)))))

;; smart-compile / (package-install 'smart-compile)
(when (package-installed-p 'smart-compile)
  (defun init:smart-recompile (arg)
    "初回のみ `smart-compile' を呼び出す. 二度目以降は問合せなしに `compile' する.
ARG が non-nil の場合は再度 `smart-compile' を呼び出す."
    (interactive "P")
    (if (or (not (local-variable-p 'compile-command))
            arg)
        (smart-compile 4)
      (compile compile-command)))

  (global-set-key [remap compile] 'init:smart-recompile))

;; ssh-agent / git clone https://github.com/d5884/ssh-agent
(when (locate-library "ssh-agent")
  (autoload 'ssh-agent-add-key "ssh-agent" nil t)

  (with-eval-after-load "magit"
    (defadvice magit-push-dwim (before init:ssh-agent-with-magit-push activate)
      "git push 前に ssh-add 実行."
      (ssh-agent-add-key))

    (defadvice magit-fetch (before init:ssh-agent-with-magit-fetch activate)
      "git fetch 前に ssh-add 実行."
      (ssh-agent-add-key)))

  (with-eval-after-load "tramp-sh"
    (defadvice tramp-send-command (before init:ssh-agent-with-tramp activate)
      "リモートコマンド実行前に ssh-add 実行."
      (ssh-agent-add-key))))

;; stripe-buffer / (package-install 'stripe-buffer)
(when (package-installed-p 'stripe-buffer)
  (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
  (add-hook 'tabulated-list-mode-hook 'turn-on-stripe-buffer-mode))

;; temp-buffer-resize
(if (not (package-installed-p 'popwin))
    (temp-buffer-resize-mode t)
  ;; popwin でも temp-buffer-resize
  (with-eval-after-load "popwin"
    (add-hook 'popwin:after-popup-hook
              (lambda ()
                (when (and (popwin:popup-window-live-p)
                           (with-current-buffer popwin:popup-buffer
                             (not (derived-mode-p 'compilation-mode))))
                  (let ((max-height (plist-get (cdr popwin:popup-last-config)
                                               :height)))
                    (fit-window-to-buffer popwin:popup-window max-height)))))))

;; tramp
(with-eval-after-load "tramp"
  (setq tramp-default-method "ssh")
  (setq tramp-verbose 2))

(with-eval-after-load "tramp-sh"
  (let ((process-environment tramp-remote-process-environment))
    (setenv "LC_ALL" nil)               ; リモートのロケールは接続先に準じる
    (setq tramp-remote-process-environment process-environment)))

;; transient-mark-mode
(defadvice exchange-point-and-mark (after init:exchange-point-and-mark-deactivate activate)
  "Function `transient-mark-mode' が有効な時にリージョンに色が付かないようにする."
  (if (and (transient-mark-mode mark-active))
      (deactivate-mark)))

;; uniquify
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

;; view-mode
(setq view-read-only t)
(with-eval-after-load "view"
  (define-key view-mode-map "j" 'next-line)
  (define-key view-mode-map "k" 'previous-line))

;; windmove
(global-set-key (kbd "C-z C-n") 'windmove-down)
(global-set-key (kbd "C-z C-p") 'windmove-up)
(global-set-key (kbd "C-z C-f") 'windmove-right)
(global-set-key (kbd "C-z C-b") 'windmove-left)

;; yank/undo highlighting
;;   from http://www.fan.gr.jp/~ring/Meadow/meadow.html#ys:highlight-string
(defadvice insert-for-yank (after init:yank-highlight-string activate)
  "文字列ヤンク時にハイライト表示する."
  (let ((ol (make-overlay (mark t) (point))))
    (unwind-protect
        (progn (overlay-put ol 'face 'highlight)
               (sit-for 0.5))
      (delete-overlay ol))))

(defadvice undo (after init:undo-highlight-string activate)
  "アンドゥで再挿入された文字列をハイライト表示する."
  (catch 'return
    (dolist (entry buffer-undo-list)
      (let ((beg (car entry))
            (end (cdr entry)))
        (cond
         ((null entry)) ;; boundary. skip it.
         ((and (integerp beg)
               (integerp end))
          (let ((ol (make-overlay beg end)))
            (unwind-protect
                (progn (overlay-put ol 'face 'highlight)
                       (sit-for 0.5))
              (delete-overlay ol)
              (throw 'return nil))))
         (t (throw 'return nil)))))))

;; yascroll / (package-install 'yascroll)
(when (package-installed-p 'yascroll)
  ;; 1行単位のスクロールにしているとちらつくので必要な時だけ表示にする
  (dolist (fn '(set-mark exchange-point-and-mark scroll-up scroll-down recenter))
    (eval `(defadvice ,fn (after ,(intern (format "init:show-yascroll-on-%s" fn)) activate)
             "スクロールバーを表示する."
             (unless (memq major-mode '(term-mode shell-mode))
               (yascroll:show-scroll-bar)))))
  (with-eval-after-load "isearch"
    (add-hook 'isearch-update-post-hook 'yascroll:show-scroll-bar)))

;; yasnippet / (package-install 'yasnippet)
(when (require 'yasnippet nil t)
  (diminish 'yas-minor-mode)

  (setq yas-verbosity 1)
  (setq yas-prompt-functions (delq 'yas-x-prompt yas-prompt-functions))
  (setq yas-expand-only-for-last-commands '(self-insert-command ac-expand))

  (when (fboundp 'yas--load-yas-setup-file)
    (init:make-silently-loading yas--load-yas-setup-file))

  (yas-global-mode t)

  (with-eval-after-load "term"
    (add-hook 'term-mode-hook
              (lambda () (yas-minor-mode -1))))

  ;; auto insert
  (add-hook 'find-file-hook 'auto-insert)
  (with-eval-after-load "autoinsert"
    (setq auto-insert-directory (init:emacs-d "template"))

    (defvar init:auto-insert-template-modtime nil
      "テンプレートディレクトリの更新時間.")

    (defun init:auto-insert-yas-expand ()
      "`auto-insert' するテンプレートを `yasnippet' のスニペットと見做して展開する."
      (yas-expand-snippet (buffer-string) (point-min) (point-max)))

    (defadvice auto-insert (before init:auto-insert-update-template activate)
      "`auto-insert' 前にテンプレート一覧を更新する.
モード名と拡張子を除いたファイル名が一致する場合テンプレートと見做す."
      (let ((modtime (file-attributes auto-insert-directory)))
        (unless (equal modtime init:auto-insert-template-modtime)
          (setq init:auto-insert-template-modtime modtime)
          (setq auto-insert-alist
                (mapcar (lambda (f)
                          (cons (intern (file-name-sans-extension f))
                                (vector f 'init:auto-insert-yas-expand)))
                        (directory-files auto-insert-directory nil "^[^.]"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 持ち歩きたい関数定義

;;;;;;;;;;;;;;;;;;;
;; scratch 自動保存/永続化
(defvar init:scratch-save-file (init:emacs-d "scratch")
  "`*scratch*' バッファの自動保存先ファイル名.")

(defvar init:scratch-snapshot-directory (init:emacs-d "snap")
  "`*scratch*' バッファのスナップショット先ディレクトリ名.")

(defvar init:scratch-buffer-save-interval 1
  "`*scratch*' バッファの自動保存間隔.")

(defvar init:prev-scratch-modified-tick 0
  "`*scratch*' バッファの前回保存時の更新状態.")

(defun init:refresh-scratch-buffer ()
  "`*scratch*' バッファを初期状態に戻す.
削除されていた場合はバッファを新規作成し、存在している場合は内容をクリアする.
また、`init:scratch-snapshot-directory' 内に現在の `*scratch*' バッファの内容を保存する."
  (interactive)
  (let ((exists-p (get-buffer "*scratch*")))
    (when exists-p
      (init:scratch-buffer-snapshot))
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
      (message "another *scratch* is created."))))

(defun init:scratch-buffer-snapshot ()
  "`*scratch*' バッファの内容を `init:scratch-snapshot-directory' 内に保存する."
  (interactive)
  (init:awhen (get-buffer "*scratch*")
    (make-directory init:scratch-snapshot-directory t)
    (let ((name-base (format "scratch-%s%%02d.el" (format-time-string "%Y%m%d-%H%M%S")))
          (serial 0)
          snapshot-name)
      (while (file-exists-p
              (setq snapshot-name (expand-file-name
                                   (format name-base serial) init:scratch-snapshot-directory)))
        (setq serial (1+ serial)))
      (with-current-buffer it
        (save-match-data
          (save-restriction
            (widen)
            (goto-char (point-min))
            (unless (re-search-forward "\\`[ \r\n\t]*\\'" nil t)
              (write-region (point-min) (point-max) snapshot-name nil 'silent))))))))

(defun init:resume-scratch-buffer ()
  "`*scratch*' バッファの内容を復帰する."
  (interactive)
  (let ((scratch (get-buffer-create "*scratch*"))
        (file (expand-file-name init:scratch-save-file))
        (buffer-undo-list t))
    (with-current-buffer scratch
      (when (file-exists-p file)
        (erase-buffer)
        (insert-file-contents file)
        (set-buffer-modified-p nil)

        (setq init:scratch-modified-tick (buffer-chars-modified-tick))))))

(defun init:save-scratch-buffer ()
  "`*scratch*' バッファの内容を保存する."
  (interactive)
  (init:awhen (get-buffer "*scratch*")
    (with-current-buffer it
      (let ((modified-tick (buffer-chars-modified-tick)))
        (unless (eq modified-tick init:prev-scratch-modified-tick)
          (setq init:prev-scratch-modified-tick modified-tick)
          (save-restriction
            (widen)
            (write-region (point-min) (point-max)
                          (expand-file-name init:scratch-save-file)
                          nil 'slient)))))))

(add-hook 'after-init-hook
          (lambda ()
            (init:resume-scratch-buffer)

            ;; 読み込みに成功したら自動保存を有効化
            (run-with-idle-timer init:scratch-buffer-save-interval t 'init:save-scratch-buffer)
            (add-hook 'kill-emacs-hook 'init:save-scratch-buffer)

            ;; 永続化
            (add-hook 'kill-buffer-query-functions
                      (lambda ()
                        (if (equal (buffer-name) "*scratch*")
                            (progn (init:refresh-scratch-buffer) nil) t)))
            (add-hook 'after-save-hook
                      (lambda ()
                        (unless (member "*scratch*" (mapcar 'buffer-name (buffer-list)))
                          (init:refresh-scratch-buffer))))))

(defun init:flip-window-state (&optional renew)
  "ウィンドウ分割状態を切り替える.
RENEW が non-nil の場合は新しい状態を作る.
2状態固定."
  (interactive "P")
  (let* ((cur (current-window-configuration))
         (state (frame-parameter nil 'init:last-window-state))
         (conf (unless renew (car state)))
         (side (cl-case (cdr state) (?A ?B) (?B ?A) (t ?B))))
    (if conf
        (set-window-configuration conf)
      (delete-other-windows)
      (switch-to-buffer "*scratch*"))
    (message "Flip to side \"%c\"." side)
    (set-frame-parameter nil 'init:last-window-state (cons cur side))
    (force-mode-line-update)))

(global-set-key (kbd "C-z C-l") 'init:flip-window-state)

;; フレームタイトルに状態を表示する
(setq frame-title-format
      (append (if (atom frame-title-format)
                  (list frame-title-format)
                frame-title-format)
              '((:eval (init:awhen (frame-parameter nil 'init:last-window-state)
                         (format " [%c]" (cdr it)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 色設定
(cl-flet ((color-candidate (&rest colors)
                           (cl-find-if #'color-defined-p colors)))
  (require 'color)

  (load-theme 'wombat t)

  (with-eval-after-load "cua-base"
    (face-spec-reset-face 'cua-rectangle)
    (set-face-attribute 'cua-rectangle nil :inherit 'region)
    (face-spec-reset-face 'cua-global-mark)
    (set-face-attribute 'cua-global-mark nil :inherit 'region
                        :weight 'bold))

  (with-eval-after-load "paren"
    (face-spec-reset-face 'show-paren-match)
    (set-face-attribute 'show-paren-match nil :inherit 'highlight))

  (when (> (display-color-cells nil) 256)
    (with-eval-after-load "stripe-buffer"
      (set-face-attribute 'stripe-highlight nil
                          :background
                          (if (eq 'light (frame-parameter nil 'background-mode))
                              (color-darken-name (face-background 'default) 3)
                            (color-lighten-name (face-background 'default) 3)))))

  (with-eval-after-load "mozc"
    (set-face-attribute 'mozc-cand-overlay-even-face nil
                        :foreground "black"
                        :background "gray80")
    (face-spec-reset-face 'mozc-cand-overlay-odd-face)
    (set-face-attribute 'mozc-cand-overlay-odd-face nil
                        :inherit 'mozc-cand-overlay-even-face)
    (set-face-attribute 'mozc-cand-overlay-footer-face nil
                        :foreground "white"
                        :background "gray50")
    (set-face-attribute 'mozc-cand-overlay-focused-face nil
                        :foreground "white"
                        :background "gray30"))
  (with-eval-after-load "mozc-popup"
    (set-face-attribute 'mozc-cand-overlay-description-face nil
                        :foreground "gray46"))

  ;; term 用 face に ansi-color の設定をコピー
  (with-eval-after-load "term"
    (require 'ansi-color nil t)
    (dotimes (i (length ansi-color-names-vector))
      (let ((color (aref ansi-color-names-vector i)))
        (set-face-attribute (aref ansi-term-color-vector (1+ i)) nil
                            :foreground color :background color))))

  ;; カーソルカラー
  (let ((ime-color "dark red"))
    ;; input method 全般
    ;; ccc / (package-install 'ccc)
    (when (require 'ccc nil t)
      (ccc-setup)
      (add-hook 'input-method-activate-hook
                (eval `(lambda () (ccc-set-buffer-local-cursor-color ,ime-color))))
      (add-hook 'input-method-deactivate-hook
                (lambda () (ccc-set-cursor-color-buffer-local nil))))

    ;; skk
    (when (package-installed-p 'ddskk)
      (eval-after-load "skk"
        `(setq skk-cursor-hiragana-color ,ime-color)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; カスタマイズファイル読み込み
(setq custom-file (init:emacs-d "custom.el"))
(load (file-name-sans-extension custom-file) t t)

;; 作業ディレクトリをホームディレクトリに
(init:awhen (getenv "HOME") (cd it))

;;; init.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
;; End:
