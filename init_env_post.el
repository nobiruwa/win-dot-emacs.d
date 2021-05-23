;;;;;;;;;;;;; 以下、ELispファイルを追加する必要があるものを設定 ;;;;;;
;;;;;;;;;;;;; アルファベット順になるよう努力 ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; company-mode
;; company-*
;;;
(require 'company)

;; company-backends
(require 'company-dict)

(setq company-dict-dir "~/repo/nobiruwa.github/dot-emacs.d.git/company-dict")

(with-eval-after-load "company"
  (global-company-mode +1)
  ;; C-[ C-i
  (global-set-key (kbd "C-M-i") 'company-complete)
  (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)
  (define-key lisp-interaction-mode-map (kbd "C-M-i") 'company-complete)
  ;; デフォルト値を保存copy-treeが再帰的にリストをコピーできる
  (setq company-backends-default (copy-tree company-backends))
  (setq company-backends
        '(company-bbdb
          company-nxml
          company-css
          company-semantic
          ;; lsp-modeが以下の警告を表示するので、とりあえずコメントアウト
          ;; `company-lsp` is not supported anymore. Using `company-capf` as the `lsp-completion-provider`.
          ;; company-lsp
          company-xcode
          company-cmake
          company-capf
          company-files
          (company-dabbrev-code company-gtags company-etags company-keywords company-dict)
          company-oddmuse
          company-dabbrev)))

;;;;;;;;
;; GCL GNU Common Lisp
;;;;;;;;
(setq inferior-lisp-program "sbcl")

;;;;;;;;
;; counsel
;;;;;;;;
(require 'counsel)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;;;;;;;;
;;  emmet-mode
;;;;;;;;
(eval-after-load "emmet-mode"
  '(progn
     (message "[emmet] redefine emmet-preview-accpet")
     (defun emmet-preview-accept
       ()
       "Original emmet-preview-accept does not work.
Temporarily, bind expr to the return value of emmet-expr-on-line."
       (interactive)
       (let ((ovli emmet-preview-input))
         (if (not (and (overlayp ovli)
                       (bufferp (overlay-buffer ovli))))
             (message "Preview is not active")
           (let* ((indent (current-indentation))
                  (markup (emmet-preview-transformed indent))
                  (expr (emmet-expr-on-line)))
             (when markup
               (delete-region (overlay-start ovli) (overlay-end ovli))
               (emmet-insert-and-flash markup)
               (emmet-reposition-cursor expr)))))
       (emmet-preview-abort))
     ))

(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'emmet-mode-hook
          (lambda ()
            (setq emmet-insert-flash-time 0.001)
            (define-key emmet-mode-keymap (kbd "C-j") nil)
            (define-key emmet-mode-keymap (kbd "<C-return>") nil)
            (define-key emmet-mode-keymap (kbd "C-c C-v") 'emmet-expand-line)))

;;;;;;;;
;; flycheck-mode
;;;;;;;;
(setq flycheck-flake8-maximum-complexity 10)
(global-set-key (kbd "<f8>") 'flycheck-mode)
;;(add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;
;; god-mode
;;;;;;;;
(global-set-key (kbd "\C-\\") 'god-local-mode)

;;;;;;;;
;; graphviz-dot-mode
;;;;;;;;
;; cogre-dot-modeがgraphviz-dot-modeを発見できるようrequire
(require 'graphviz-dot-mode)
(setq graphviz-dot-auto-indent-on-semi nil)
(add-hook 'graphviz-dot-mode-hook (lambda () (auto-complete-mode)))

;;;;;;;;
;; haskell-mode
;;;;;;;;
;; See https://github.com/syl20bnr/spacemacs/issues/706
;; and https://github.com/haskell/haskell-mode/wiki/Indentation
;; haskell-indentation-mode is the current implementataion,
;; but it's too buggy.
(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-haskell-indentation)
            (setq haskell-indent-offset 2)))

;;;;;;;;
;; highlight-indentation
;;;;;;;;
(require 'highlight-indentation)
(add-hook 'python-mode-hook
          (lambda ()
            "turn on highlight-indentation-mode"
            (highlight-indentation-mode 1)))

;;;;;;;;
;; ivy-mode
;;;;;;;;
(require 'ivy)
;; M-x lsp-java-generate-overrides や M-x lsp-java-spring-initializr など、複数の選択肢から選択する際に使う
;; ivyのキーマップには登録されていないが必要不可欠な関数なので、ここで登録する
(define-key ivy-minibuffer-map (kbd "M-RET") 'ivy-mark)
;; `./`と`../`を先頭に表示する必要はない
;; リストの末尾に置けるならばよかったのだが
(setq ivy-extra-directories nil)

;;;;;;;;
;; js-mode
;;;;;;;;
(add-hook 'js-mode-hook
     (lambda ()
       (setq js-indent-level 2)))

;;;;;;;;
;; js2-mode
;; It will refuse to run unless you have byte-compiled it.
;; You must byte-compile it with your version of Emacs because
;; different versions of Emacs have different byte-compiled formats.
;;;;;;;;
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
     (lambda ()
       (setq js2-basic-offset 2)))

;;;;;;;;
;; org-mode
;;;;;;;;
(add-hook 'org-mode-hook
          (lambda ()
            ;; Disable enlarged org-mode header appearance
            ;; https://emacs.stackexchange.com/questions/22584/disable-enlarged-org-mode-header-appearance/22589#22589
            "Stop the org-level headers from increasing in height relative to the other text."
            (dolist (face '(org-level-1
                            org-level-2
                            org-level-3
                            org-level-4
                            org-level-5))
              (set-face-attribute face nil :family user--cjk-font :weight 'semi-bold :height 1.0))))

;;;;;;;;
;; purescript-mode
;;;;;;;;
(add-hook 'purescript-mode-hook
          (lambda ()
            (setq haskell-literate nil)
            (haskell-indentation-mode)))

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

;;;;;;;;
;; plantuml-mode
;;;;;;;;
(require 'plantuml-mode)
(add-hook 'plantuml-mode-hook
          (lambda ()
            (setq plantuml-jar-path (expand-file-name "~/opt/plantuml/plantuml.jar"))))

;;;;;;;;
;; skk
;;;;;;;;
;; skk-modeが有効になると、C-jがskk-kakutei-keyにバインドされる
;; 使用頻度の殆どないC-oにnewlineをバインドする
(add-hook 'skk-load-hook
          '(lambda ()
             (progn
               (if (functionp 'electric-newline-and-maybe-indent)
                   (progn
                     (define-key skk-abbrev-mode-map "\C-o" 'electric-newline-and-maybe-indent)
                     (define-key skk-latin-mode-map "\C-o" 'electric-newline-and-maybe-indent)
                     (define-key skk-jisx0208-latin-mode-map "\C-o" 'electric-newline-and-maybe-indent)
                     (define-key skk-j-mode-map "\C-o" 'electric-newline-and-maybe-indent))))))

(setq skk-aux-large-jisyo nil)
;; ▽モードと▼モード時のアンドゥ情報を記録しない
(setq skk-undo-kakutei-word-only t)
;; C-x j のskk-auto-fill-modeは使わない
(global-set-key "\C-xj" 'skk-mode)
(global-set-key "\C-x\C-j" 'skk-mode)

;; SKK L辞書の場所
(setq skk-large-jisyo (expand-file-name "~/.emacs.d/dict/SKK-JISYO.L"))

;;;;;;;;
;; slime
;;;;;;;;
(require 'slime)

;;;;;;;;
;; solarized-theme
;;;;;;;;
;; make the fringe stand out from the background
;(setq solarized-distinct-fringe-background t)

;; Don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)

;; make the modeline high contrast
;(setq solarized-high-contrast-mode-line t)

;; Use less bolding
;(setq solarized-use-less-bold t)

;; Use more italics
;(setq solarized-use-more-italic t)

;; Use less colors for indicators such as git:gutter, flycheck and similar
;(setq solarized-emphasize-indicators nil)

;; Don't change size of org-mode headlines (but keep other size-changes)
(setq solarized-scale-org-headlines nil)

;; Avoid all font-size changes
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)

;;;;;;;;
;; swiper
;;;;;;;;
(require 'swiper)
;; swiper use M-s as the prefix.
(global-set-key (kbd "M-s M-s") 'swiper)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)
(global-set-key (kbd "M-s s") 'swiper-thing-at-point)

;;;;;;;;
;; undo-tree
;;;;;;;;
(require 'undo-tree)
(global-undo-tree-mode)
;; rxvt-unicode detects C-c C-/ as C-c C-_
(define-key undo-tree-map (kbd "C-c C-/") 'undo-tree-redo)
(define-key undo-tree-map (kbd "C-c C-_") 'undo-tree-redo)

;;;;;;;;
;; vue-mode
;;;;;;;;
(require 'vue-mode)
(add-hook 'vue-mode-hook
          (lambda ()
            (setq vue-html-extra-indent 2)
            (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
            (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))))

;;;;;;;;
;; wdired
;;;;;;;;;
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;;;;;;;;
;; web-mode
;;;;;;;;
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[s]?css\\'" . web-mode))
(add-hook 'web-mode-hook (lambda ()
                           (progn
                             (auto-fill-mode -1)
                             (setq web-mode-markup-indent-offset 2)
                             (setq web-mode-css-indent-offset 2)
                             (setq web-mode-code-indent-offset 2)
                             (setq web-mode-auto-close-style 1))))

;;;;;;;;
;; wgrep
;;;;;;;;
(require 'wgrep nil t)

;;;;;;;;
;; yasnippet
;;;;;;;;
(require 'yasnippet)
(setq yas-prompt-functions '(yas/ido-prompt))
(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs
             (expand-file-name "~/repo/nobiruwa.github/yasnippet-snippets.git"))
(yas-load-directory (expand-file-name "~/repo/nobiruwa.github/yasnippet-snippets.git"))

;; あるバッファで YASnippet マイナーモードを OFF にしたい
;;(set-default 'yas/dont-activate
;;             #'(lambda ()
;;                 (and yas/root-directory
;;                      (null (yas--get-snippet-tables)))))

;; ~/.emacs.d/snippets/java-mode/getAset のための関数
(defun downcase-initial (obj)
  "It downcases the first letter of OBJ."
  (if (and
       (boundp 'obj)
       (stringp obj)
       (< 0 (length obj)))
      (concat (downcase (substring obj 0 1)) (substring obj 1))
    ""))

;; Local Variables:
;; coding: utf-8-dos
;; End:
