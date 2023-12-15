;;; init.el --- User initialize file. -*- coding: utf-8 -*-

;;; Commentary:
;;
;; ユーザ設定初期化ファイル.
;; Emacs 29.1 以降用
;;

;;; Code:

;; 一時的なGC
(setq gc-cons-threshold most-positive-fixnum)
(setq read-process-output-max (* 1024 1024))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; メールアドレス等
(setq user-full-name "Daisuke Kobayashi")
(setq user-mail-address "d5884jp@gmail.com")

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

(defmacro init:emacs-d (file)
  "~/.emacs.d 以下の FILE を返す."
  `(locate-user-emacs-file ,file))

(defmacro init:locate-directory (directory)
  "DIRECTORY が存在するなら返す."
  `(locate-file "." (delq nil (list ,directory)) nil
                (lambda (p) (when (file-exists-p p) 'dir-ok))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package 初期化/ロードパス追加

;; Package 初期化
(package-initialize)
(setq package-enable-at-startup nil)
(setq package-archives (append
                        '(("melpa" . "http://melpa.org/packages/")
                          ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/"))
                        package-archives))

;; ~/.emacs.d/lisp および直下のディレクトリを load-path へ追加
(setq load-path
      (append
       (init:awhen (init:locate-directory (init:emacs-d "lisp"))
         (cons it (cl-remove-if-not #'file-directory-p (directory-files it t "^[^.]"))))
       load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 言語設定

;; coding-system の優先度設定
(set-coding-system-priority 'utf-8 'cp932) ; shift_jis より cp932 優先

;; 波ダッシュを全角チルダへ(Windowsスタイル) (U+301C > U+FF5E)
(let ((table (make-translation-table-from-alist '((#x301c . #xff5e)))))
  (dolist (coding-system '(utf-8 cp932 utf-16le))
    (coding-system-put coding-system :decode-translation-table table)
    (coding-system-put coding-system :encode-translation-table table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; フォント/ウィンドウサイズ設定
(when (display-graphic-p)
  (setq vertical-centering-font-regexp ".*")
  (init:awhen (cl-find-if (lambda (f)
                            (find-font (font-spec :family f)))
                          '("HackGen Console NF""Ricty" "Ricty Diminished"))
    (set-fontset-font "fontset-standard" 'ascii (font-spec :family "HackGen Console NF"
                                                           :size 12.0
                                                           :weight 'normal :slant 'normal))
    (setq-default line-spacing 0.2)
    )

  ;; フレームに設定
  (add-to-list 'default-frame-alist (cons 'font "fontset-standard")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; パス追加
(add-to-list 'exec-path (expand-file-name (init:emacs-d "bin")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; キーバインド変更

;; Ctrl-h を DEL に
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

(autoload 'ibuffer-bs-show "ibuffer" nil t)

(global-set-key (kbd "C-x ?") 'help-command)
(global-set-key (kbd "C-x 7") 'toggle-truncate-lines)
(global-set-key (kbd "C-x C-z") 'compile)
(global-unset-key (kbd "C-z"))
(global-set-key [remap list-buffers] 'ibuffer-bs-show)
(global-set-key (kbd "C-z z") (defun init:ansi-term ()
                                "`ansi-term' を実行する."
                                (interactive)
                                (ansi-term shell-file-name)))

(when (display-mouse-p)
  ;; ホイールクリックで貼り付けは使わない
  (dolist (key '("<mouse-2>" "<down-mouse-2>"
                 "<left-fringe> <mouse-2>" "<right-fringe> <mouse-2>"))
    (global-unset-key (kbd key)))
  (global-set-key (kbd "<mouse-2>") 'ffap-at-mouse)

  ;; 右クリックは編集メニュー表示
  (dolist (key '("<mouse-3>" "<down-mouse-3>" "<drag-mouse-3>"))
    (global-unset-key (kbd key)))
  (when (display-popup-menus-p)
    (global-set-key (kbd "<mouse-3>") menu-bar-edit-menu)))

;; IME関連キーの整理
; 全角/半角は IME トグルに
(global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)

; 無変換/変換/カタカナ/ひらがな/ローマ字は無効化
(dolist (key '("<muhenkan>" "<henkan>" "<hiragana-katakana>"))
  (global-set-key (kbd key) 'ignore))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 各種機能/パッケージ別

;; 編集系色々
(setq comment-style 'multi-line)
(setq kill-do-not-save-duplicates t)
(setq require-final-newline t)
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

(advice-add 'display-startup-echo-area-message :override 'ignore)

;; スクロール関係
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 35)
(setq scroll-margin 0)
(setq scroll-step 1)
(setq next-screen-context-lines 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))
(setq recenter-positions '(middle))

;; フレーム/カーソル関連
(setq frame-title-format "%b")
(blink-cursor-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; モードライン関係
(column-number-mode t)

;; ffap
(ffap-bindings)

;; ansi-color
(with-eval-after-load "comint"
  (ansi-color-for-comint-mode-on))

;; corfu
(when (require 'corfu nil t)
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.1)
  (setq corfu-auto-prefix 1)
  (setq corfu-preselect 'prompt)
  (global-corfu-mode)
  (when (require 'kind-icon nil t)
    (setq corfu-margin-formatters '(kind-icon-margin-formatter))
    (setq kind-icon-default-face 'corfu-default))

  (when (require 'corfu-popupinfo nil t)
    (setq corfu-popupinfo-delay 0)
    (corfu-popupinfo-mode)))


;; js-mode
(setq auto-mode-alist (append '(("\\.js\\'" . js-ts-mode)
                                ("\\.ts\\'"  . typescript-ts-mode)
                                ("\\.jsx\\'" . js-jsx-mode)
                                ("\\.tsx\\'" . tsx-ts-mode))
                              auto-mode-alist))
(with-eval-after-load "js"
  (define-key js-mode-map (kbd "M-.") 'xref-find-definitions)

  (dolist (mode '(js-ts-mode-hook js-jsx-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook))
    ;; eglot
    (when (package-installed-p 'eglot)
      (add-hook mode 'eglot-ensure)
      ;; flymake-eslint
      (when (package-installed-p 'flymake-eslint)
        (add-hook mode (lambda ()
                         (add-hook 'eglot-managed-mode-hook 'flymake-eslint-enable)))))
    ;; prettier
    (when (package-installed-p 'prettier)
      (add-hook mode 'prettier-mode))))

;; python-mode
(setq auto-mode-alist (append '(("\\.py\\'" . python-ts-mode))
                              auto-mode-alist))

(with-eval-after-load "python"
  (add-hook 'python-ts-mode-hook 'eglot-ensure))

;; editor-config
(when (require 'editorconfig nil t)
  (editorconfig-mode 1))

;; flymake-diagnostic-at-point
(when (package-installed-p 'flymake-diagnostic-at-point)
  (with-eval-after-load "flymake"
    (require 'flymake-diagnostic-at-point nil t)
    (add-hook 'flymake-mode-hook 'flymake-diagnostic-at-point-mode)))


;; calculator
(global-set-key (kbd "C-z C-c") 'calculator)

;; completion
(setq completion-show-help nil)

;; delete-selection-mode
(delete-selection-mode 1)

;; dired
(global-set-key (kbd "C-x C-d") 'dired-other-window)
(with-eval-after-load "dired"
  (setq dired-dwim-target t)
  (setq dired-isearch-filenames t)
  (setq dired-listing-switches
        (concat "-oglAF "
                "--time-style=long-iso "
                "--group-directories-first "))

  (add-hook 'dired-mode-hook
            (lambda ()
              (toggle-truncate-lines t)
              (hl-line-mode t)
              ;; dired 上でのみゴミ箱使用
              (setq-local delete-by-moving-to-trash t)))

  (define-key dired-mode-map (kbd "q") 'kill-this-buffer)

  ;; find-dired
  (with-eval-after-load "find-dired"
    (setq find-ls-option (cons (format "-exec ls -d %s {} +"
                                       dired-listing-switches)
                               (format "-d %s" dired-listing-switches)))))

;; nerd-icons-dired
(when (package-installed-p 'nerd-icons-dired)
  (when (package-installed-p 'dired-sidebar)
    (defalias 'all-the-icons-dired--refresh 'nerd-icons-dired--refresh))
  (add-hook 'dired-mode-hook 'nerd-icons-dired-mode))

;; nerd-icons-dired
(when (package-installed-p 'nerd-icons-completion)
  (add-hook 'after-init-hook 'nerd-icons-completion-mode)
  (when (package-installed-p 'marginalia)
    (add-hook 'marginalia-mode-hook 'nerd-icons-completion-marginalia-setup)))

;; nerd-icons-ibuffer
(when (package-installed-p 'nerd-icons-ibuffer)
  (add-hook 'ibuffer-mode-hook 'nerd-icons-ibuffer-mode))

;; doom-modeline
(when (require 'doom-modeline nil t)
  (doom-modeline-mode 1))

;; electric-pair
(electric-pair-mode 1)

;; show-paren
(setq show-paren-delay 0)
(show-paren-mode t)

;; indent-guid
(when (package-installed-p 'indent-guide)
  (indent-guide-global-mode))

;; magit / (package-install 'magit)
(when (package-installed-p 'magit)
  (global-set-key (kbd "C-z C-m") 'magit-status)

  (with-eval-after-load "magit"
    (setq magit-auto-revert-mode-lighter "")))

;; diff-hl
(when (require 'diff-hl nil t)
  (global-diff-hl-mode 1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (global-set-key (kbd "C-z C-d") 'diff-hl-show-hunk)
  (with-eval-after-load "magit"
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

;; blamer
(when (require 'blamer nil t)
  (global-blamer-mode t))

;; grep
(with-eval-after-load "grep"
  (declare-function grep-apply-setting "grep")

  (when (executable-find "rg")
    ;; ripgrep
    (setq grep-program "rg")
    ; grep
    (grep-apply-setting 'grep-command "rg -Hnuu --no-heading ")
    ; lgrep
    (grep-apply-setting 'grep-template
                        (format "%s<C> -g '<F>' <R>" grep-command))
    ; rgrep XXX broken...
    (grep-apply-setting 'grep-find-template
                        (format "%s<C> -g '<F>' <R> <D>" grep-command))))

;; vertico and more
(when (require 'vertico nil t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)

  (vertico-mode)

  (when (require 'vertico-posframe nil t)
    (vertico-posframe-mode))

  (when (require 'vertico-directory nil t)
    (define-key vertico-map (kbd "RET") 'vertico-directory-enter)
    (define-key vertico-map (kbd "DEL") 'vertico-directory-delete-char)
    (define-key vertico-map (kbd "M-DEL") 'vertico-directory-delete-word)
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

  (when (require 'marginalia nil t)
    (marginalia-mode)))

;; imenu
(global-set-key (kbd "C-z C-j") 'imenu)
(with-eval-after-load "imenu"
  (setq imenu-auto-rescan t))

;; imenu-list
(when (package-installed-p 'imenu-list)
  (global-set-key (kbd "C-z C-l") 'imenu-list))

;; isearch
(with-eval-after-load "isearch"
  (setq isearch-allow-scroll t)
  (setq lazy-highlight-initial-delay 0))

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
              (eldoc-mode t)
              (add-hook 'after-save-hook
                        'init:byte-compile-current-file-if-necessary
                        nil t))))

;; eval-expression
(add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

;; executable-make-buffer-executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; migemo / (package-install 'migemo)
;; cmigemo / http://www.kaoriya.net/software/cmigemo
(init:awhen (or (executable-find "cmigemo")
                (executable-find "migemo"))
  (when (require 'migemo nil t)
    (setq migemo-command it)

    (when (string-match-p "cmigemo" it)
      (setq migemo-options '("-q" "--emacs"))
      (setq migemo-user-dictionary nil)
      (setq migemo-regex-dictionary nil)
      (setq migemo-coding-system 'utf-8)
      (setq migemo-dictionary
            (expand-file-name (locate-file (format "%S/migemo-dict" migemo-coding-system)
                                           `(,(init:emacs-d "share/migemo")
                                             "/usr/local/share/migemo"
                                             "/usr/share/cmigemo")))))

    (setq migemo-use-pattern-alist t)
    (setq migemo-use-frequent-pattern-alist t)
    (setq migemo-pattern-alist-length 1024)
    (setq migemo-pattern-alist-file (init:emacs-d "migemo-pattern"))

    (migemo-init)

    ;; migemo-completion
    ;; git clone https://github.com/d5884/migemo-completion
    ;; (when (require 'migemo-completion nil t)
    ;;   (setq completion-category-overrides
    ;;         (append '((file (styles migemo))
    ;;                   (buffer (styles migemo)))
    ;;                 completion-category-overrides)))
    ))

;; mozc-im / (package-install 'mozc-im)
;;    and http://www49.atwiki.jp/ntemacs?cmd=upload&act=open&pageid=50&file=mozc_emacs_helper.zip
(init:awhen (or (executable-find "mozc_emacs_helper")
                (executable-find "mozc-helper.sh"))
  (when (require 'mozc-im nil t)
    (setq mozc-helper-program-name it)
    (setq default-input-method "japanese-mozc-im")
    (setq mozc-leim-title "[あ]")

    (advice-add 'mozc-session-execute-command :after
                (lambda (arg1 &rest args)
                  (when (eq arg1 'CreateSession)
                    (mozc-session-sendkey '(hiragana)))))

    (advice-add 'mozc-helper-process-recv-response :filter-return
                (lambda (ret)
                  (if ret
                      ret
                    (mozc-helper-process-recv-response))))

    ;; mozc-popup / (package-install 'mozc-popup)
    (when (require 'mozc-popup nil t)
      (setq mozc-candidate-style 'popup))))

;; occur
(define-key occur-mode-map (kbd "n") 'occur-next)
(define-key occur-mode-map (kbd "p") 'occur-prev)
(add-hook 'occur-mode-hook 'next-error-follow-minor-mode)

(when (require 'shackle nil t)
  (setq shackle-default-rule '(:same t))
  ;; XXX
  )


;; savehist
(savehist-mode 1)
(add-hook 'savehist-save-hook
          (lambda ()
            ;; 保存したくないファイル名パターン
            (setq file-name-history
                  (cl-delete-if
                   (apply-partially 'string-match-p
                                    (concat "\\<"
                                            (regexp-opt '("COMMIT_EDITMSG"))
                                            "\\'"))
                   file-name-history))))

;; saveplace
(save-place-mode 1)

;; google translate / (package-install 'google-translate)
(when (package-installed-p 'google-translate)
  (autoload 'google-translate-translate "google-translate")
  (defun google-translate-auto-at-point ()
    "カーソル位置の単語をGoogleで翻訳する."
    (interactive)
    (google-translate-auto (if (use-region-p)
                               (buffer-substring-no-properties
	                        (region-beginning) (region-end))
                             (or (thing-at-point 'word) ""))))

  (defun google-translate-auto (&optional word)
    "`word'をGoogleで翻訳する.
リージョンが選択されている場合はその内容を翻訳する."
    (interactive (list (read-string "Word: "
                                    (if (use-region-p)
                                        (buffer-substring-no-properties
			                 (region-beginning) (region-end))
                                      (thing-at-point 'word)))))
    (if (string-match (format "\\`[%s]+\\'" "[:ascii:]") word)
        (google-translate-translate "en" "ja" word)
      (google-translate-translate "ja" "en" word)))

  (advice-add 'google-translate-buffer-insert-translation :after
              (lambda (_)
                (save-excursion
                  (goto-char (point-min))
                  (forward-line 2)
                  (delete-region (point-min) (point)))))

  ;; (setq display-buffer-alist '(("\\*Google Translate\\*"
  ;;                               display-buffer-in-side-window
  ;;                               (side . top)
  ;;                               (slot . 0)
  ;;                               (window-width . 85)
  ;;                               (window-parameters
  ;;                                (no-other-window . t)))))

  (global-set-key (kbd "C-z C-w") 'google-translate-auto-at-point)
  (global-set-key (kbd "C-z w") 'google-translate-auto))

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
    (setq shell-pop-autocd-to-working-dir nil)))

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
  (define-key shell-mode-map (kbd "M-p")
              'comint-previous-matching-input-from-input)
  (define-key shell-mode-map (kbd "M-n")
              'comint-next-matching-input-from-input))

;; term
(with-eval-after-load "term"
  (add-hook 'term-exec-hook 'init:set-process-cleaner)

  ;; C-c に C-x を取り込まない
  (set-keymap-parent term-raw-escape-map nil)
  ;; char-mode で使いたいキーを開放して raw 入力は C-c に移動
  (dolist (key '("M-x" "M-:" "C-z" "C-u" "C-\\"))
    (define-key term-raw-map (kbd key) nil)
    (define-key term-raw-map (kbd (concat "C-c " key))
      (if (string-match-p "^M-" key)
          'term-send-raw-meta
        'term-send-raw))))

;; stripe-buffer / (package-install 'stripe-buffer)
(when (package-installed-p 'stripe-buffer)
  (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
  (add-hook 'tabulated-list-mode-hook 'turn-on-stripe-buffer-mode))

;; temp-buffer-resize
(temp-buffer-resize-mode t)

;; tramp
(with-eval-after-load "tramp"
  (setq tramp-default-method "ssh")
  (setq tramp-verbose 2))

(with-eval-after-load "tramp-sh"
  (let ((process-environment tramp-remote-process-environment))
    ; リモートのロケールは接続先に準じる
    (setenv "LC_ALL" nil)
    (setq tramp-remote-process-environment process-environment)))

;; uniquify
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

(when (require 'volatile-highlights nil t)
  (volatile-highlights-mode 1))

;; yascroll / (package-install 'yascroll)
(when (package-installed-p 'yascroll)
  ;; 1行単位のスクロールにしているとちらつくので必要な時だけ表示にする
  (dolist (fn '(set-mark
                exchange-point-and-mark
                scroll-up
                scroll-down
                recenter))
    (advice-add fn :after
                (lambda (&rest args)
                  (unless (memq major-mode '(term-mode shell-mode))
                    (yascroll:show-scroll-bar)))))
  (with-eval-after-load "isearch"
    (add-hook 'isearch-update-post-hook 'yascroll:show-scroll-bar)))

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
    (let ((name-base (format "scratch-%s%%02d.el"
                             (format-time-string "%Y%m%d-%H%M%S")))
          (serial 0)
          snapshot-name)
      (while (file-exists-p
              (setq snapshot-name (expand-file-name
                                   (format name-base serial)
                                   init:scratch-snapshot-directory)))
        (setq serial (1+ serial)))
      (with-current-buffer it
        (save-match-data
          (save-restriction
            (widen)
            (goto-char (point-min))
            (unless (re-search-forward "\\`[ \r\n\t]*\\'" nil t)
              (write-region (point-min) (point-max)
                            snapshot-name nil 'silent))))))))

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
                          nil 'silent)))))))

(add-hook 'after-init-hook
          (lambda ()
            (init:resume-scratch-buffer)

            ;; 読み込みに成功したら自動保存を有効化
            (run-with-idle-timer init:scratch-buffer-save-interval t
                                 'init:save-scratch-buffer)
            (add-hook 'kill-emacs-hook 'init:save-scratch-buffer)

            ;; 永続化
            (add-hook 'kill-buffer-query-functions
                      (lambda ()
                        (if (equal (buffer-name) "*scratch*")
                            (progn (init:refresh-scratch-buffer) nil) t)))
            (add-hook 'after-save-hook
                      (lambda ()
                        (unless (member "*scratch*"
                                        (mapcar 'buffer-name (buffer-list)))
                          (init:refresh-scratch-buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 色設定
(require 'color)

(require 'modus-themes nil t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-bold-constructs t)
(setq modus-themes-region '(bg-only no-extend))
(setq modus-themes-paren-match '(bold intense))
(setq modus-themes-mode-line '(borderless accented 2))
(load-theme 'modus-vivendi t)

(set-cursor-color "gray66")

;; (set-face-attribute 'isearch nil
;;                     :background "gray77"
;;                     :foreground "black")


(with-eval-after-load "diff-hl"
  (set-face-attribute 'diff-hl-change nil
                      :foreground "slate blue"
                      :background "slate blue")
  (set-face-attribute 'diff-hl-insert nil
                      :foreground "cadet blue"
                      :background "cadet blue")
  (set-face-attribute 'diff-hl-delete nil
                      :foreground "dark salmon"
                      :background "dark salmon"))

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

;; input method カーソルカラー
;; ccc / (package-install 'ccc)
(when (require 'ccc nil t)
  (ccc-setup)
  (add-hook 'input-method-activate-hook
            (lambda () (ccc-set-buffer-local-cursor-color "slate blue")))
  (add-hook 'input-method-deactivate-hook
            (lambda () (ccc-set-cursor-color-buffer-local nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; カスタマイズファイル読み込み
(setq custom-file (init:emacs-d "custom.el"))
(load (file-name-sans-extension custom-file) t t)

;; 作業ディレクトリをホームディレクトリに
(init:awhen (getenv "HOME") (cd it))

;; GC を戻す
(setq gc-cons-threshold (* 1024 1024 128))

;;; init.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
;; End:
