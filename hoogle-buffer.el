;;; hoogle-buffer.el --- Hoogle in a buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Created: February 06, 2024
;; License: GPL-3.0-or-later
;; Version: 0.1.0
;; Keywords: docs languages
;; Homepage: https://codeberg.org/rahguzar/consult-hoogle
;; Package-Requires: ((emacs "27.1") (haskell-mode "16.1"))

;; This file is part of GNU Emacs.

;;; Commentary:

;; Display results of a hoogle search in a buffer.

;;; Code:
(require 'consult-hoogle)
(require 'url-handlers)

;;; Customization Options
(defgroup hoogle-buffer nil
  "Hoogle in an Emacs buffer."
  :group 'haskell)

(defcustom hoogle-buffer-args
  (when (executable-find "hoogle")
    '("hoogle" . ("search" "--jsonl" "-q" "--count=250")))
  "The hoogle invocation used to get results.
It is should be a cons (COMMAND . ARGS).  COMMAND should be valid executable.
It is called arguments ARGS with the search query appended.  It should produce
search results in JSON lines format."
  :type '(choice (nil :tag "Use hoogle at hoogle.haskell.org")
                 (cons (string :tag "Hoogle command")
                       (repeat :tag "Args for hoogle" string)))
  :group 'hoogle-buffer)

(defcustom hoogle-buffer-project-args
  '("cabal-hoogle" . ("run" "--" "search" "--jsonl" "-q" "--count=250"))
  "The hoogle invocation used to get results for current project.
It should be cons (COMMAND . ARGS). See `consult-hoogle-args' for details.  By
default it uses `cabal-hoogle' https://github.com/kokobd/cabal-hoogle ."
  :type '(cons (string :tag "Project specific hoogle command")
          (repeat :tag "Args for hoogle" string))
  :group 'hoogle-buffer)

;;; Internal variables
(defconst hoogle-buffer--url-format
  "https://hoogle.haskell.org/?mode=json&hoogle=%s&start=1&count=50"
  "Url format for using hoogle at hoogle.haskell.org .")

(defvar hoogle-buffer--fontification-buffer nil
  "Indirect buffer used for fontification in a hoogle buffer.")
(put 'hoogle-buffer--fontification-buffer 'permanent-local t)

;;; Major mode
(define-derived-mode hoogle-buffer-mode special-mode "Hoogle"
  "Major mode for displaying Hoogle search results."
  :syntax-table nil :abbrev-table nil :interactive nil
  (setq line-prefix "  ")
  (setq wrap-prefix "  ")
  (setq-local jit-lock-functions '(hoogle-buffer--fontify)
              outline-regexp (rx "§" (* whitespace))
              outline-minor-mode-cycle t
              outline-minor-mode-highlight nil)
  (hoogle-buffer--setup-fontification-buffer)
  (jit-lock-mode t)
  (outline-minor-mode)
  (visual-line-mode))

;;;; Fontification
(defun hoogle-buffer--setup-fontification-buffer ()
  "Setup the indirect buffer used for fontification."
  (when (buffer-live-p hoogle-buffer--fontification-buffer)
    (with-current-buffer hoogle-buffer--fontification-buffer
      (setq-local delay-mode-hooks t)
      (haskell-mode)
      (font-lock-mode -1)
      (setq-local font-lock-dont-widen t
                  font-lock-support-mode nil)
      (current-buffer))))

(defun hoogle-buffer--get-fontification-buffer ()
  "Get the indirect buffer used for fontification."
  (if (buffer-live-p hoogle-buffer--fontification-buffer)
      hoogle-buffer--fontification-buffer
    (setq-local hoogle-buffer--fontification-buffer
                (make-indirect-buffer (current-buffer)
                                      (generate-new-buffer-name
                                       (concat (buffer-name) "--fontification"))
                                      nil t))
    (hoogle-buffer--setup-fontification-buffer)))

(defun hoogle-buffer--fontify (start end)
  "Fontify current hoogle buffer from START to END."
  (goto-char start)
  (beginning-of-line)
  (while (re-search-forward
          (rx bol (? "§ ") (group (or "Package" "Module")) ": " (group (* nonl)))
          end t)
    (put-text-property (match-beginning 2) (match-end 2) 'face
                       (if (equal (match-string 1) "Package")
                           'haskell-quasi-quote-face
                         'haskell-keyword-face)))
  (if (memq (get-text-property start 'face) '(nil 'shr-code))
      (setq start (previous-single-property-change start 'face nil (point-min))))
  (when (memq (get-text-property end 'face) '(nil 'shr-code))
    (setq end (next-single-property-change end 'face nil (point-max))))
  (with-current-buffer (hoogle-buffer--get-fontification-buffer)
    (goto-char start)
    (let ((start start))
      (while (< (point) end)
        (while
            (and (not (memq (get-text-property (point) 'face) '(nil shr-code)))
                 (goto-char (next-single-char-property-change (point) 'face))))
        (setq start (point))
        (goto-char (next-single-property-change start 'face nil (point-max)))
        (narrow-to-region start (point))
        (font-lock-fontify-region start (point))
        (widen)
        (unless (eobp) (forward-char)))))
  `(jit-lock-bounds ,start . ,end))

;;; Process handling
(defun hoogle-buffer--insert-results (proc)
  "Insert available results from PROC into results buffer."
  (goto-char (point-min))
  (while (not (eobp))
    (set-marker (process-mark proc) (min (+ 2 (line-end-position)) (point-max)))
    (when-let ((result (ignore-errors
                         (json-parse-string
                          (delete-and-extract-region
                           (line-beginning-position) (line-end-position))
                          :object-type 'alist)))
               (start (point)))
      (insert (propertize "§ " 'face 'bold 'line-prefix ""))
      (consult-hoogle--details result)
      (goto-char (- (process-mark proc) 2))
      (end-of-line)
      (put-text-property start (point) 'hoogle-result result)
      (insert "\n"))
    (goto-char (process-mark proc)))
  (goto-char (point-min))
  (while (re-search-forward (rx "\n" (>= 2 (: (* whitespace) "\n"))) nil t)
    (replace-match "\n\n"))
  (let ((inhibit-read-only t))
   (with-current-buffer (process-get proc 'results-buffer)
    (save-excursion
      (goto-char (point-max))
      (insert-buffer-substring (process-buffer proc)))))
  (delete-region (point-min) (point-max)))

(defun hoogle-buffer--filter (proc string)
  "Handle output STRING from hoogle process PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (widen)
      (goto-char (point-max))
      (insert string)
      (search-backward "\n" nil t)
      (narrow-to-region (point-min) (1+ (point)))
      (hoogle-buffer--insert-results proc))))

(defun hoogle-buffer--sentinel (proc status)
  "Sentinel for hoogle process PROC, handling STATUS."
  (pcase status
    ("finished\n"
     (when (buffer-live-p (process-buffer proc))
       (with-current-buffer (process-buffer proc)
         (hoogle-buffer--insert-results proc)
         (kill-buffer))))
    ((rx (or "failed" "connection broken" "(core dumped)" "exited abnormally"))
     (message "hoogle process error :%s" status))))

;;; Url Callback
(defun hoogle-buffer--url-results (buffer status)
  "Return results by parsing a hoogle url from BUFFER response with STATUS."
  (unwind-protect
      (if-let ((err (plist-get :error status)))
          (error "Error %s in retrieving hoogle results: %S" (car err) (cdr err))
        (with-temp-buffer
          (erase-buffer)
          (set-buffer-multibyte t)
          (url-insert buffer)
          (decode-coding-region (point-min) (point-max) 'utf-8)
          (goto-char (point-min))
          (pop-to-buffer (current-buffer))
          (json-parse-buffer :object-type 'alist :array-type 'list)))
    (kill-buffer buffer)))

(defun hoogle-buffer--render-html (str)
  "Render the html string STR."
  (with-temp-buffer
    (insert str)
    (shr-render-region (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun hoogle-buffer--url-callback (status results-buffer time-stamp)
  "Callback for `url-queue-retrieve' to insert results into RESULTS-BUFFER.
See `url-retrieve' for STATUS."
  (let ((url-buffer (current-buffer))
        (inhibit-read-only t))
    (with-current-buffer results-buffer
      (save-excursion
        (goto-char (point-max))
        (dolist (result (hoogle-buffer--url-results url-buffer status))
          (cl-callf hoogle-buffer--render-html (alist-get 'item result))
          (insert (propertize "§ " 'face 'bold 'line-prefix ""))
          (let ((start (point)))
            (consult-hoogle--details result)
            (goto-char (point-max))
            (put-text-property (- start 2) (point) 'hoogle-result result))
          (insert "\n\n"))
        (goto-char (point-min))
        (while (re-search-forward
                (rx "\n" (>= 2 (: (* whitespace) "\n"))) nil t)
          (replace-match "\n\n")))
      (pop-to-buffer results-buffer)
      (message "hoogle took %s milliseconds"
               (time-convert (time-since time-stamp) 1000)))))

;;; Interactive Commands
;;;###autoload
(defun hoogle-buffer (query results-buffer)
  "Display hoogle search results for QUERY in RESULTS-BUFFER.
By default uses local hoogle executable if it is found. Otherwise hoogle at
hoogle.haskell.org is used. See `hoogle-buffer-args' for customization."
  (interactive (list (read-string "Hoogle: ")
                     (get-buffer-create "*hoogle-search*")))
  (let ((inhibit-read-only t))
    (with-current-buffer results-buffer
      (erase-buffer)
      (hoogle-buffer-mode)))
  (if hoogle-buffer-args
      (let* ((proc-buffer (get-buffer-create "*hoogle-process*"))
             (proc (make-process :name "*hoogle-process*" :buffer proc-buffer
                                 :noquery t :connection-type 'pipe
                                 :command `(,@hoogle-buffer-args ,query)
                                 :filter #'hoogle-buffer--filter
                                 :sentinel #'hoogle-buffer--sentinel)))
        (process-put proc 'results-buffer results-buffer)
        (pop-to-buffer results-buffer))
    (url-queue-retrieve (format hoogle-buffer--url-format (url-hexify-string query))
                        #'hoogle-buffer--url-callback
                        `(,results-buffer ,(current-time)) t)))

(defun hoogle-buffer-project (query results-buffer)
  "Display search results for QUERY in RESULTS-BUFFER for current project.
By default uses cabal-hoogle and the database should have been generated
by running `cabal-hoogle generate'.  `hoogle-buffer-project-args' can be
customized to configure an alternate command."
  (interactive (list (read-string "Hoogle: ")
                     (get-buffer-create "*hoogle-search*")))
  (let ((hoogle-buffer-args hoogle-buffer-project-args))
    (hoogle-buffer query results-buffer)))

(defun hoogle-buffer-web (query results-buffer)
  "Display hoogle search results for QUERY in RESULTS-BUFFER.
Results are obtained by querying hoogle.haskell.org ."
  (interactive (list (read-string "Hoogle: ")
                     (get-buffer-create "*hoogle-search*")))
  (let (hoogle-buffer-args)
    (hoogle-buffer query results-buffer)))

(provide 'hoogle-buffer)
;;; hoogle-buffer.el ends here
