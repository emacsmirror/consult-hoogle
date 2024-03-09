;;; consult-hoogle.el --- Hoogle frontend using consult -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: rahguzar <rahguzar@zohomail.eu>
;; Maintainer: rahguzar <rahguzar@zohomail.eu>
;; Created: April 10, 2022
;; License: GPL-3.0-or-later
;; Version: 0.2.0
;; Keywords: docs languages
;; Homepage: https://codeberg.org/rahguzar/consult-hoogle
;; Package-Requires: ((emacs "27.1") (haskell-mode "16.1"))

;; This file is part of GNU Emacs.

;;; Commentary:

;; Search the local hoogle database from Emacs using the nicities provided by
;; consult.

;;; Code:

;;;; Packages
(require 'consult)
(require 'hoogle-base)
(require 'haskell-mode)

(declare-function hoogle-buffer "hoogle-buffer")

;;;; Variables
(defgroup consult-hoogle nil
  "A frontend for hoogle."
  :group 'consult)

(define-obsolete-variable-alias 'consult-hoogle-args 'hoogle-base-args "0.2.0")
(define-obsolete-variable-alias 'consult-hoogle-project-args 'hoogle-base-project-args "0.2.0")

(defcustom consult-hoogle-show-module-and-package t
  "Whether to show the package and module in the candidate line."
  :type 'boolean
  :group 'consult-hoogle)

(defvar consult-hoogle--history nil
  "Variable to store history for hoogle searches.")

(defvar consult-hoogle-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map hoogle-base-map)
    (define-key map (kbd "M-<up>") #'consult-hoogle-scroll-docs-down)
    (define-key map (kbd "M-<down>") #'consult-hoogle-scroll-docs-up)
    (define-key map (kbd "C-e") #'consult-hoogle-export-to-buffer)
    map))

;;;; Constructing the string to display
(defun consult-hoogle--builder (input)
  "Build command line given INPUT."
  (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
    (unless (string-blank-p arg)
      (cons (append hoogle-base-args (list arg) opts)
            (cdr (consult--default-regexp-compiler input 'basic t))))))

(defun consult-hoogle--fontify (text)
  "Fontify TEXT, returning the fontified text.
This is adapted from `haskell-fontify-as-mode' but for better performance
we use the same buffer throughout."
  (with-current-buffer " *Hoogle Fontification*"
    (erase-buffer)
    (insert text)
    (font-lock-ensure)
    (buffer-substring (point-min) (point-max))))

(defun consult-hoogle--format-value (item in module from package)
  "Construct the disaply string from ITEM IN MODULE FROM and PACKAGE."
  (if (not consult-hoogle-show-module-and-package)
      item
    (concat item from module in package)))

(defun consult-hoogle--format-result (json)
  "Parse the JSON resturned by hoogle to construct a result."
  (when-let ((parsed (ignore-errors (json-parse-string json :object-type 'alist))))
    (let* ((in (propertize " in " 'face 'font-lock-comment-face))
           (from (propertize " from " 'face 'font-lock-comment-face))
           (module (cl-callf propertize
                       (alist-get 'name (alist-get 'module parsed) "")
                     'face 'haskell-keyword-face))
           (package (cl-callf propertize
                        (alist-get 'name (alist-get 'package parsed) "")
                      'face 'haskell-quasi-quote-face)))
      (propertize
       (pcase (alist-get 'type parsed)
         (""  (consult-hoogle--format-value
               (cl-callf consult-hoogle--fontify (alist-get 'item parsed))
               from module in package))
         ("module" (concat "Module "
                           (cl-callf hoogle-base--name (alist-get 'item parsed)
                             'haskell-keyword-face)
                           in package))
         ("package" (concat "Package "
                            (cl-callf hoogle-base--name (alist-get 'item parsed)
                              'haskell-quasi-quote-face))))
       'consult--candidate parsed))))

;;;; Constructing the details buffer for the selected result
(defun consult-hoogle--show-details (action cand)
  "Show the details for the current CAND and handle ACTION."
  (when-let (((equal (buffer-name) " *Hoogle Documentation*"))
             (inhibit-read-only t))
    (erase-buffer)
    (pcase action
      ('preview (when cand
                  (hoogle-base--details cand)
                  (goto-char (point-min))))
      ('return (kill-buffer-and-window)))))

;;;; Refining searches
(defun consult-hoogle--async-input ()
  "Return the async part of the input."
  (let* ((style (alist-get consult-async-split-style consult-async-split-styles-alist))
         (input (substring-no-properties (minibuffer-contents)))
         (initial (when (plist-get style :initial)
                    (string-match (rx bos (group (opt punct))) input)
                    (match-string 1 input)))
         (separator (or (plist-get style :separator) initial))
         (async-rx (rx-to-string `(: ,(or initial "") (group (0+ (not ,separator)))))))
    (goto-char (minibuffer-prompt-end))
    (when (looking-at async-rx)
      (substring-no-properties (match-string 1)))))

(defun consult-hoogle--modify-async-input (fun)
  "Change async part of input to (funcall FUN async-input)."
  (save-excursion
    (when-let ((input (consult-hoogle--async-input)))
      (replace-match (string-trim (funcall fun input)) nil t nil 1))))

;;;; Consult integration
(defun consult-hoogle--candidate ()
  "Get the current candidate."
  (when-let ((candidate (run-hook-with-args-until-success
                         'consult--completion-candidate-hook)))
    (get-text-property 0 'consult--candidate candidate)))

(defun consult-hoogle--search (&optional state action)
  "Search the local hoogle database and take ACTION with the selection.
STATE is the optional state function passed to the `consult--read'."
  (let ((consult-async-min-input 0)
        (hoogle-base-find-candidate #'consult-hoogle--candidate)
        (hoogle-base-modify-query-function #'consult-hoogle--modify-async-input)
        (fun (or action (lambda (alist) (hoogle-base--browse-url 'item alist)))))
    (with-current-buffer (get-buffer-create " *Hoogle Fontification*" t)
      (setq-local delay-mode-hooks t)
      (haskell-mode))
    (unwind-protect
        (funcall fun (consult--read
                      (consult--async-command #'consult-hoogle--builder
                        (consult--async-map #'consult-hoogle--format-result)
                        (consult--async-highlight #'consult-hoogle--builder))
                      :prompt "Hoogle: "
                      :require-match t
                      :initial (consult--async-split-initial "")
                      :lookup #'consult--lookup-candidate
                      :state state
                      :sort nil
                      :keymap consult-hoogle-map
                      :add-history (consult--async-split-thingatpt 'symbol)
                      :category 'consult-hoogle
                      :history '(:input consult-hoogle--history)))
      (when-let ((buf (get-buffer " *Hoogle Fontification*")))
        (kill-buffer buf)))))

;;;; Interactive Commands
;;;###autoload
(defun consult-hoogle (arg)
  "Search the local hoogle database.
By default this shows the documentation for the current candidate in a side
window.  This can be disabled by a prefix ARG."
  (interactive (list current-prefix-arg))
  (if arg (consult-hoogle--search)
    (let* ((buf (get-buffer-create " *Hoogle Documentation*" t))
           (window (display-buffer buf
                                   `(display-buffer-in-side-window
                                     (window-height . 16)
                                     (side . bottom)
                                     (slot . -1)))))
      (with-current-buffer buf
        (visual-line-mode)
        (read-only-mode))
      (with-selected-window window
        (consult-hoogle--search #'consult-hoogle--show-details)))))

;;;###autoload
(defun consult-hoogle-project (arg)
  "Search the local hoogle database for current project.
By default uses cabal-hoogle and the database should have been generated
by running `cabal-hoogle generate'.  `consult-hoogle-project-args' can be
customized to configure an alternate command.
By default this shows the documentation for the current candidate in a side
window.  This can be disabled by a prefix ARG."
  (interactive (list current-prefix-arg))
  (let ((hoogle-base-args hoogle-base-project-args)
        (default-directory (haskell-cabal-find-dir)))
    (consult-hoogle arg)))

(defun consult-hoogle-scroll-docs-down (&optional arg)
  "Scroll the window with documentation ARG lines down."
  (interactive)
  (with-selected-window (get-buffer-window " *Hoogle Documentation*")
    (scroll-down arg)))

(defun consult-hoogle-scroll-docs-up (&optional arg)
  "Scroll the window with documentation ARG lines down."
  (interactive)
  (with-selected-window (get-buffer-window " *Hoogle Documentation*")
    (scroll-up arg)))

(defun consult-hoogle-export-to-buffer ()
  "Open a buffer containing results for the async part of current search."
  (interactive)
  (let ((input (consult-hoogle--async-input)))
    (hoogle-buffer input (get-buffer-create "*hoogle-search*"))
    (abort-recursive-edit)))

(provide 'consult-hoogle)
;;; consult-hoogle.el ends here
