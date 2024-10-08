;;; init.el --- my Emacs settings
;;; Commentary:
;;;  My Emacs settings.
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 追加の設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initファイルロード関数の定義
(defun load-env-if-exists (env-path)
  "与えられたENV-PATHが存在する場合、initファイルとみなしてロードします。"
  (let ((init-env-el (expand-file-name env-path)))
    (when (file-exists-p init-env-el)
      (load-file init-env-el))))

;;;
;; 環境ごとの設定を~/.emacs.d/init_env_pre.elに書く
;;;
(load-env-if-exists (expand-file-name "init_env_pre.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; グローバルな設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;; ファイルの扱い
;;;;;;;;
;; load-path
(setq load-path (append (list (expand-file-name "site-lisp" user-emacs-directory)) load-path))

;; exec-path
(defun add-executable-path (path)
  "exec-path変数とPATH環境変数の両方に引数で与えられたpathを追加します。"
  (when (file-exists-p path)
    (let* ((path-in-env (replace-regexp-in-string "/" "\\\\" path))
          (path-in-env-semicolon (concat path-in-env ";")))
      (add-to-list 'exec-path path)
      (setenv "PATH" (concat path-in-env-semicolon (getenv "PATH"))))))

;; GnuWin32のinstall.batですべてのパッケージをインストールした場合
(add-executable-path "C:/gnu/gnuwin32/bin")
;; GnuWin32の実行ファイルを個別にダウンロードした場合
(add-executable-path "C:/coreutils/bin")
(add-executable-path "C:/diffutils/bin")
(add-executable-path "C:/findutils/bin")
(add-executable-path "C:/grep/bin")
(add-executable-path "C:/sed/bin")
;; Gitのdiff等を用いる場合
(add-executable-path "c:/Program Files/Git/usr/bin")

;; バックアップファイル(foo.txt~)
;; バックアップファイルを作らない
(setq make-backup-files nil)
;; 自動保存ファイル(#foo.txt#)
;; 自動保存ファイルの保存場所を"~/.emacs.d/tmp"に変更する
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/" user-emacs-directory) t)))

;; 自動保存リストファイル(~/.emacs.d/auto-save-list/.saves-xxxx)
;; 自動保存リストファイルはデフォルトの作るので設定を変更しない

;; ロックファイル(.#foo.txt)
;; ロックファイルを作らない
(setq create-lockfiles nil)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
;; カスタマイズをinit.elではなく~/.emacs.d/emacs-custom.elに保存する
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))

;;;;;;;;
;; 初期化
;;;;;;;;
;; メニューバーを表示する場合はnon-nil
(menu-bar-mode -1)
;; emacsclient
(server-start)
;; 対となる括弧を強調表示する場合はnon-nil
(show-paren-mode 1)
;; ツールバーを表示する場合はnon-nil
(tool-bar-mode -1)
;; カーソルのブリンクを有効にする場合はnon-nil
(setq blink-cursor-mode nil)
;; カーソル位置の桁を表示する場合はnon-nil
(setq column-number-mode t)
;; デバッグをする場合はnon-nil
(setq debug-on-error nil)
;; 起動画面がいつまでも消えてくれないのではなから使わない
(setq inhibit-startup-message t)
;; 行数を表示する場合はnon-nil
(setq line-number-mode t)
;; ベル音が不要な場合はnon-nilな関数かシンボル
(setq ring-bell-function 'ignore)
;; ベル音を画面のフラッシュに変更する場合はnon-nil
(setq visible-bell nil)
;; カーソルを点灯したままにする
(setq visible-cursor nil)

;;;;;;;;
;; 特定のモードに関連付けられないキーの設定
;;;;;;;;
;; C-zを無効にする
(global-set-key "\C-z" nil)
;; C-h キーでカーソルの左の文字が消えるようにする。
(global-set-key "\C-h" 'backward-delete-char)
;; Ref: http://q.hatena.ne.jp/1137478760 の回答20
;; ミニバッファ内でC-wで単語削除です。上位パスのファイルを選択する際に便利です。
(define-key minibuffer-local-completion-map "\C-w" 'backward-kill-word)

;;;;;;;;
;; タブと空白の設定
;;;;;;;;
;; 8-character tab length is too long. set 4 character.
;; タブキーを押したときのインデント幅は
;; M-x edit-tab-stopsで設定
;; tab-stop-listを直接編集してもOK
(setq-default tab-width 4)
(setq tab-stop-list
      '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76))
;; 字下げをタブではなく空白にする
(setq-default indent-tabs-mode nil)

;;;;;;;;
;; 日本語表示の設定
;;;;;;;;
;; 日本語 info が文字化けしないように
(auto-compression-mode t)
;; 日本語 grep
(if (file-exists-p "/usr/bin/lgrep")
    (setq grep-command "lgrep -n "))

;;;;;;;;
;; settings for utf-8
;;;;;;;;
;; Ref: http://forum.ubuntulinux.jp/viewtopic.php?pid=909#p909
(set-language-environment "English")
(prefer-coding-system 'utf-8-dos)
(set-default-coding-systems 'utf-8-dos)
(set-keyboard-coding-system 'utf-8-dos)
;; Ref: http://yanok.net/2015/09/windows-emacs.html
;; Windows 上の Emacs のクリップボードの文字コード設定
(set-clipboard-coding-system 'utf-16le)
(set-terminal-coding-system 'utf-8-dos)
(setq file-name-coding-system 'utf-8-dos)

;;;;;;;;
;; Native Compile
;;;;;;;;
(when (and (functionp 'native-comp-available-p) (native-comp-available-p))
  ;; コンパイル時の警告やエラーを出力するとき*Warning*バッファのウィンドウをポップアップさせない
  (setq native-comp-async-report-warnings-errors 'silent))

;;;;;;;;
;; Windows用の設定
;;;;;;;;
(when (eq window-system 'w32)
  ;; Emacsのスタートアップ時のディレクトリを~/に設定する
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq default-directory "~/"))))

;;;;;;;;;;;;;; Emacs 標準Lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;; all CC Mode modes
;;;;;;;;
(add-hook 'c-mode-common-hook
          (lambda ()
            (put 'c-file-offsets 'safe-local-variable 'listp)
            (c-set-style "bsd")
            (c-set-offset 'arglist-close 0)
            (c-set-offset 'case-label 2)
            (setq c-basic-offset 2)
            (setq indent-tabs-mode nil)))

;;;;;;;;
;; C++ mode (c++-mode)
;;;;;;;;
;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             (c-set-style "bsd")
;;             (setq indent-tabs-mode nil)
;;             (setq c-basic-offset 2)))

;;;;;;;;
;; cedet, ede, semantic, etc.
;; Ref: Emacs Part 31
;; URL: http://pc12.2ch.net/test/read.cgi/unix/1251665639/312
;;;;;;;;
;; DBファイルを一ヶ所に集約
(setq semanticdb-default-save-directory (expand-file-name "semantic" user-emacs-directory))
;; disable semantic-mode and global-*-mode in CEDET
;; CEDET conflicts js2-mode, python-mode
(semantic-mode -1)

;;;;;;;;
;; comint mode
;;;;;;;;
;; Try without shell prompt change
;; Compatible with Windows and at least some Linux shells
;; change the method used to determine the current directory in a shell
;; ref: https://www.emacswiki.org/emacs/ShellDirtrackByPrompt
;; ref: https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Tracking.html#Directory-Tracking
(require 'dirtrack)
(require 'shell)
(add-hook 'comint-mode-hook
          (lambda ()
            ;; stop the usual shell-dirtrack mode
            (shell-dirtrack-mode 0)
            ;; (setq ssh-directory-tracking-mode 'ftp)
            ;; for help making this regular expression you may want to use "M-x re-builder", where M is usually alt
            (setq dirtrack-list '(":*\\(PS \\)?\\([A-Za-z]*:*~*[\/\\].*?\\)[^-+A-Za-z0-9_.()//\\ ]" 2))
            ;; this shows any change in directory that dirtrack mode sees
            ;; (dirtrack-debug-mode)
            ;; enable the more powerful dirtrack mode
            (dirtrack-mode)))

;;;;;;;;
;; ediff
;;;;;;;;
(require 'ediff)
;; This is what you probably want if you are using a tiling window
;; manager under X, such as ratpoison.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; This will split the window horizontally if the frame is wider than 150 chars and vertically otherwise.
(setq ediff-split-window-function (if (> (frame-width) 150)
                                      'split-window-horizontally
                                    'split-window-vertically))

;;;;;;;;
;; eglot
;;;;;;;;
(require 'eglot)
(with-eval-after-load "eglot"
  (setq eglot-autoshutdown t))

;;;;;;;;
;; ido-mode
;;;;;;;;
(require 'ido)
(ido-mode 1)
(setq ido-auto-merge-work-directories-length -1)
;; See: http://www.gnu.org/software/emacs/manual/html_mono/ido.html#Ignoring
;; foo.gitという名前でリポジトリをcloneするポリシーのため、hidden extensionsから削除する。
;; .gitという隠しディレクトリも見えるようになるが、挙動として問題ない。
;; ちなみに、ido-find-fileに隠されたアイテムを表示するにはC-a (ido-toggle-ignore) を使う。
(setq completion-ignored-extensions (delete ".git/" completion-ignored-extensions))

;;;;;;;;
;; java-mode
;;;;;;;;
(add-hook 'java-mode-hook (lambda ()
                            (setq indent-tabs-mode nil)
                            (setq c-basic-offset 4)))

;;;;;;;;
;; mouse, mwheel
;;;;;;;;
(require 'mouse)
(require 'mwheel)
(if (eq system-type 'gnu/linux)
    (xterm-mouse-mode 1))
(mouse-wheel-mode t)

;;;;;;;;
;; nxml-mode
;;;;;;;;
(require 'xml)
(setq nxml-slash-auto-complete-flag t)

;;;;;;;;
;; org-mode
;;;;;;;;
(require 'org)
(setq org-startup-folded nil)
(setq org-startup-truncated nil)
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-lisp.html
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)))

;;;;;;;;
;; package.el (MELPA)
;;;;;;;;
;; (See also: https://github.com/milkypostman/melpa#usage)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu-devel" . "https://elpa.gnu.org/devel/") t)
;; Emacs 27から、パッケージはinit.elのロードよりも前にロードされるようになり、
;; package-initializeを呼ぶ必要はなくなりました。
(when (< emacs-major-version 27)
  (package-initialize))
;; package-selected-packagesに存在するパッケージでインストールしていないパッケージがあればインストールする関数です。
(defun my-install-packages-if-not-installed ()
  "install packages listed in package-selected-packages if they have not been installed yet."
  (interactive)
    (when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages)))

;;;;;;;;
;; shell-mode
;;;;;;;;
(require 'shell)
;; *shell*バッファを現在のウィンドウで開く
(add-to-list 'display-buffer-alist
             '("^\\*shell\\*\\(<[0-9]+>\\)?$" . (display-buffer-same-window)))

;; solarized-darkと組み合わせた時のプロンプトの色
(when (eq window-system 'x)
  (set-face-foreground 'comint-highlight-prompt "#268bd2"))

;; shell-modeの拡張
;; lsなどの色の設定
;; (autoload 'ansi-color-for-comint-mode-on "ansi-color"
;;  "Set `ansi-color-for-comint-mode' to t." t)
;;(setq ansi-color-names-vector ["black" "red3" "green3" "yellow3" "navy" "magenta3" "cyan3" "white"])

;; company-modeのパス補完動作のカスタマイズ
(defun first-to-last (suffix list)
  "先頭の要素がSUFFIXを含む場合、LISTの先頭要素を末尾に移動した新しいリストを返します。"
  (if (and list (string-suffix-p suffix (nth 0 list)))
      (let* ((first (car list))
            (deleted (remove first list)))
        (add-to-list 'deleted first t))
    list))

(defun company--sort-with-making-special-name-at-the-end (candidates)
  "`../`があればリストの後ろに、`./`があればリストの後ろに置きます。"
  (first-to-last "./" (first-to-last "../" candidates)))

(add-hook 'shell-mode-hook
          (lambda ()
            ;; use CP932
            (set-buffer-process-coding-system 'cp932-dos 'cp932-dos)
            ;; TAB幅は8
            (setq tab-width 8)
            ;; 存在しないファイル名の入力をスムーズにするため
            (setq-local company-require-match nil)
            ;; 色付け
            (ansi-color-for-comint-mode-on)
            ;; "../" "./"を後ろに回す
            (setq-local company-transformers '(company--sort-with-making-special-name-at-the-end))))

;;;
;; shell-modeにおいて、company-modeのcomapny-capfが実行ファイル名の解決を行うとき
;; 探索するディレクトリを制限します。
;;;
(defcustom shell-command-ignored-exec-path-regexp "/mnt/.*"
  "a REGEXP to be ignored when searching executables from `exec-path' directories."
  :type 'regexp)

(defvar shell-command-original-exec-path nil
  "the original exec-path value before running shell-command-completion.")

(defun deep-copy-sequence (x)
  "Make a deep copy of the given sequence X."
  (mapcar #'copy-sequence x))

(defun shell-command-backup-exec-path ()
  "backup `exec-path'"
  (setq shell-command-original-exec-path (deep-copy-sequence exec-path)))

(defun shell-command-remove-from-exec-path ()
  "remove elements matching `shell-command-ignored-exec-path-regexp' from `exec-path'"
  (cl-delete-if
   (lambda (path)
     (string-match-p shell-command-ignored-exec-path-regexp path)) exec-path))

(defun shell-command-restore-exec-path (&rest args)
  "restore `exec-path' from `shell-command-ignored-exec-path-regexp'"
  (when shell-command-original-exec-path
    (setq exec-path shell-command-original-exec-path)
    (setq shell-command-original-exec-path nil)))

(defun shell-command-comint-completion-at-point-around (orig-func &rest args)
  "An advice function which change `exec-path' during calling ORIG-FUNC. Restores `exec-path' at the end. It works only in shell-mode."
  (when (derived-mode-p 'shell-mode)
    (shell-command-backup-exec-path)
    (shell-command-remove-from-exec-path)
    (let ((res (ignore-errors (apply orig-func args))))
      (shell-command-restore-exec-path)
      res)))

;; To enable this, uncomment the following line.
;; (advice-add 'comint-completion-at-point :around #'shell-command-comint-completion-at-point-around)

;;;;;;;;
;; uniquify
;; Ref: http://q.hatena.ne.jp/1137478760 の回答24
;; a/index.html と b/index.html をひらいたときに、
;; バッファ名を index.html<a>, index.html<b> としてくれます。
;; デフォルトの連番だとどれがどれだかわからなくなるので。
;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;;;;;;;
;; url-http.el
;;;;;;;;
;; Emacs 28.1未満ではProxy-Authorizationヘッダーが送信されない
;; url-https-proxy-connect関数をアドホックで修正する
;; https://github.com/syl20bnr/spacemacs/issues/4807#issuecomment-723332754
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=42422
(when (or (< emacs-major-version 28) (and (= emacs-major-version 28) (< emacs-minor-version 1)))
  (with-eval-after-load 'url-http
    (defun url-https-proxy-connect (connection)
      (setq url-http-after-change-function 'url-https-proxy-after-change-function)
      (process-send-string connection
                           (format
                            (concat "CONNECT %s:%d HTTP/1.1\r\n"
                                    "Host: %s\r\n"
                                    (let ((proxy-auth (let ((url-basic-auth-storage
                                                             'url-http-proxy-basic-auth-storage))
                                                        (url-get-authentication url-http-proxy nil 'any nil))))
                                      (if proxy-auth (concat "Proxy-Authorization: " proxy-auth "\r\n")))
                                    "\r\n")
                            (url-host url-current-object)
                            (or (url-port url-current-object)
                                url-https-default-port)
                            (url-host url-current-object))))))

;;;;;;;;;;;;;; 関数宣言 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;
;; my-insert-file-name
;; Emacs Wiki: Insert File Name
;; http://www.emacswiki.org/emacs-ja/InsertFileName
;; Running
;;   M-x my-insert-file-name ~/.emacs RET
;; will insert the file name as it appears in the MiniBuffer.
;; If you want the full path to the file, you can have it "expanded"
;; with a PrefixArgument.
;; Running
;;   C-u M-x my-insert-file-name ~/.emacs RET
;; will insert
;;   /home/username/.emacs
;; into the buffer.
;;;;;;;;;;;;
(defun my-insert-file-name (arg filename)
  "If ARG is non nil, insert name of file FILENAME into buffer after point.
Set mark after the inserted text.

Prefixed with \\[universal-argument], expand the file name to
its fully canocalized path.

See `expand-file-name'."
  ;; Based on insert-file in Emacs -- ashawley 2008-09-26
  (interactive "*P\nfInsert file name: ")
  (if arg
      (insert (expand-file-name filename))
    (insert filename)))

;; C-c C-c 現バッファの内容を保存してバッファを消す
;; Ref: http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SaveAndKillBuffer
(defun my-save-and-kill-buffer ()
  (interactive)
  (save-buffer)
  (kill-buffer nil))

;; リージョンの単語をソートする
(defvar separators-per-mode
  '((emacs-lisp-mode " +" " ")
    (haskell-mode ", *" ", ")
    (otherwise ", *" ", ")))

(defun my-sort-words-in-region (start end)
  "sort words separated white spaces in the current region."
  (interactive "r")
  (let* ((separators (if (assoc major-mode separators-per-mode)
                        (assoc major-mode separators-per-mode)
                       (assoc 'otherwise separators-per-mode)))
         (sep-regexp (cadr separators))
         (sep-fixed (caddr separators)))
    (replace-string
     (buffer-substring start end)
     (my-sort-words-in-line (buffer-substring start end) sep-regexp sep-fixed)
     nil start end)))

;; 文字列内の単語をソートする
(defun my-sort-words-in-line (text sep-regexp sep-fixed)
  "sort words separated white spaces in a line."
  (mapconcat 'identity (sort
   (split-string text sep-regexp) 'string<) sep-fixed))

;;;;
;; M-x powershell
;; https://stackoverflow.com/questions/872510/can-i-use-powershell-in-shell-mode-for-emacs
;;;;
(defun powershell (&optional buffer)
  "Launches a powershell in buffer *powershell* and switches to it."
  (interactive)
  (let ((buffer (or buffer "*powershell*"))
    (powershell-prog "c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe"))
    (make-comint-in-buffer "shell" "*powershell*" powershell-prog)
    (switch-to-buffer buffer)
    (set-buffer-process-coding-system 'cp932-dos 'cp932-dos)))

;;;
;; previous-lineのオーバーライド
;;;
;; バッファの最初の行で previous-line しても、
;; "beginning-of-buffer" と注意されないようにする。
;; http://www.bookshelf.jp/2ch/unix/1001393679.html のNo. 8
(defun previous-line (arg)
  (interactive "p")
  (if (interactive-p)
      (condition-case nil
          (line-move (- arg))
        ((beginning-of-buffer end-of-buffer)))
    (line-move (- arg)))
  nil)

;;;;;;;;
;; reopen-file
;;;;;;;;
;; http://namazu.org/~satoru/diary/?200203c&to=200203272#200203272
;; 編集中のファイルを開き直す
;; - yes/no の確認が不要;;   - revert-buffer は yes/no の確認がうるさい
;; - 「しまった! 」というときにアンドゥで元のバッファの状態に戻れる
;;   - find-alternate-file は開き直したら元のバッファの状態に戻れない
;;
(defun reopen-file ()
  "Reopen file without confirm yes/no."
  (interactive)
  (let ((file-name (buffer-file-name))
        (old-supersession-threat
         (symbol-function 'ask-user-about-supersession-threat))
        (point (point)))
    (when file-name
      (fset 'ask-user-about-supersession-threat (lambda (fn)))
      (unwind-protect
          (progn
            (erase-buffer)
            (insert-file file-name)
            (set-visited-file-modtime)
            (goto-char point))
        (fset 'ask-user-about-supersession-threat
              old-supersession-threat)))))
;; reopen-fileをC-x C-rにバインド
(define-key ctl-x-map "\C-r"  'reopen-file)

;;;
;; requireの代わりに使います。
;;;
(setq require-if-not-loaded-packages '())
(defun require-if-not (feature &optional else-body)
  "パッケージをロードします。パッケージのロードに失敗した場合はELSE-BODYを実行します。ロードに失敗した場合FEATUREがrequire-if-not-loaded-packages変数に追加されます。パッケージをロードした場合はtを、ロードに失敗した場合にはnilを返します。"
  (if (require feature nil t)
      (progn
        (message "require-if-not: [%s] is loaded." feature)
        t)
    (progn
      (add-to-list 'require-if-not-loaded-packages feature)
      (if (and (boundp 'else-body) (functionp 'else-body))
          (funcall else-body)
        (message "require-if-not: [%s] is not loaded." feature))
      nil)))

;;;;;;;;;;;;; 以下、ELispファイルを追加する必要があるものを設定 ;;;;;;
;;;;;;;;;;;;; アルファベット順になるよう努力 ;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;; customize font
;;;
;; Ref: https://www.shimmy1996.com/en/posts/2018-06-24-fun-with-fonts-in-emacs/
;; Ref: https://qiita.com/melito/items/238bdf72237290bc6e42
;; Ref: http://misohena.jp/blog/2017-09-26-symbol-font-settings-for-emacs25.html
;; Ref: https://www.reddit.com/r/emacs/comments/ggd90c/color_emoji_in_emacs_27/
(defvar user--cjk-font "VL Gothic"
  "Default font for CJK characters")

(defvar user--latin-font "VL Gothic"
  "Default font for Latin characters")

(defvar user--cjk-proportional-font "VL PGothic"
  "Default font for Latin characters")

(defvar user--unicode-font "Noto Sans Mono CJK JP"
  "Default font for Unicode characters. including emojis")

(defvar user--unicode-emoji-font "Noto Color Emoji"
  "Default font for Unicode emoji characters.")

;; Notoフォントでベンガル語(charset名はbengali)を表示するとクラッシュする。
;; バックトレースを見るとlibm17n/libotf0でクラッシュしているようだ。
;; $ fc-list :lang=bn
;; を実行してベンガル語をサポートするフォント一覧を出力すると、
;; Notoフォント以外にFreeSansがある。
;; ので、FreeSansをフォールバックフォントとして用いる。
(defvar user--unicode-font-fallback "FreeSans"
  "Fallback font for Unicode characters.")

(defvar user--standard-fontset "fontset-user"
  "Standard fontset for user.")

(defun user--set-font ()
  "Set Unicode, Latin and CJK font for user--standard-fontset."
  ;; 記号にはデフォルトのフォントではなく指定のフォントを使いたい
  (setq use-default-font-for-symbols nil)
  (create-fontset-from-ascii-font user--cjk-font nil (replace-regexp-in-string "fontset-" "" user--standard-fontset))
  ;; unicodeに対してuser--cjk-fontがグリフを持っていればそれを使い、
  ;; 持っていない場合にはuser--unicode-fontで補完する
  (set-fontset-font user--standard-fontset 'unicode
                    (font-spec :family user--cjk-font)
                    nil)
  (set-fontset-font user--standard-fontset 'unicode
                    (font-spec :family user--unicode-font)
                    nil 'append)
  ;; latinに対してuser--latin-fontを使う
  (set-fontset-font user--standard-fontset 'latin
                    (font-spec :family user--latin-font)
                    nil 'prepend)
  ;; CJKに対してuser--cjk-fontを使う
  (dolist (charset '(kana han cjk-misc hangul kanbun bopomofo))
    (set-fontset-font user--standard-fontset charset
                  (font-spec :family user--cjk-font)
                  nil 'prepend))
  ;; symbolに対してuser--unicode-emoji-fontを使う
  (set-fontset-font t 'symbol user--unicode-emoji-font nil 'append)
  ;; TODO 日本語フォントではU+2018とU+2019は全角幅だがWeb上の英文ではアポストロフィに使われていて
  ;; 見栄えが悪い。現状は全角で表示し必要に応じてU+0027に置換する。よい方法はないものか。
  (dolist (charset '((#x2018 . #x2019)    ;; Curly single quotes "‘’"
                     (#x201c . #x201d)))  ;; Curly double quotes "“”"
    (set-fontset-font user--standard-fontset charset
                      (font-spec :family user--cjk-font)
                      nil)) ; 上書きするために第5引数ADDは省略する
  ;; フォールバックフォントを用いる言語(charsetは C-u C-x = のscriptセクションの名前を用いる)
  (dolist (charset '(bengali bengali-akruti bengali-cdac))
    (set-fontset-font user--standard-fontset charset
                      (font-spec :family user--unicode-font-fallback)
                      nil 'prepend)))

(when window-system
  ;; create fontset-user
  (user--set-font)
  ;; Ensure user--standard-fontset gets used for new frames.
  (add-to-list 'default-frame-alist `(font . ,user--standard-fontset))
  (add-to-list 'initial-frame-alist `(font . ,user--standard-fontset)))

;;;
;; customize theme, color
;;;
(if (or (eq window-system 'x) (eq window-system 'w32))
    (if (package-installed-p 'solarized-theme)
        (load-theme 'solarized-dark t)
      (load-theme 'tango-dark t)))

;;;
;; cygwin
;;;
(if (eq system-type 'cygwin)
    (progn (load "init-cygwin")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 追加の設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; custom-fileをロードする
;;;
(load-env-if-exists custom-file)

;;;
;; 環境ごとの設定を~/.emacs.d/init_env_post.elに書く
;;;
(load-env-if-exists (expand-file-name "init_env_post.el" user-emacs-directory))

(provide 'init)
;;; init.el ends here
;; Local Variables:
;; coding: utf-8-dos
;; End:
