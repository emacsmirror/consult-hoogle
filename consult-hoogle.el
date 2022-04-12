;;; consult-hoogle.el --- Hoogle frontend using consult -*- lexical-binding: t; -*-
;;
;; Created: April 10, 2022
;; Modified: April 10, 2022
;; License: GPL-3.0-or-later
;; Version: 0.0.1
;; Keywords: docs languages
;; Homepage: https://github.com/aikrahguzar/consult-hoogle
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Search the local hoogle database from Emacs using the nicities provided by
;; consult.
;;
;;; Code:

;;;; Packages
(require 'consult)
(require 'map)
(require 'subr-x)
(require 'haskell-mode)
(require 'shr)

;;;; Variables
(defcustom consult-hoogle-args "hoogle search --jsonl -q --count=100" "The hoogle invocation used to get results." :type 'string :group 'consult)

(defcustom consult-hoogle-show-module-and-package t "Whether to show the package and module in the candidate line." :type 'boolean :group 'consult)

(defvar consult-hoogle--history nil "Variable to store history for hoogle searches.")

(defvar consult-hoogle-map (let ((map (make-sparse-keymap)))
                             (define-key map (kbd "M-i") #'consult-hoogle-browse-item)
                             (define-key map (kbd "M-j") #'consult-hoogle-browse-package)
                             (define-key map (kbd "M-m") #'consult-hoogle-browse-module)
                             (define-key map (kbd "M-<up>") #'consult-hoogle-scroll-docs-down)
                             (define-key map (kbd "M-<down>") #'consult-hoogle-scroll-docs-up)
                             map))

;;;; Constructing the string to display
(defun consult-hoogle--builder (input)
  "Build command line given CONFIG and INPUT."
  (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
    (unless (string-blank-p arg)
      (list :command (append (split-string-and-unquote consult-hoogle-args) (list arg) opts)
            :highlight (cdr (consult--default-regexp-compiler input 'basic t))))))

(defun consult-hoogle--format (lines) "Format the LINES from hoogle result."
       (seq-map #'consult-hoogle--format-result lines))

(defun consult-hoogle--format-result (json) "Parse the JSON resturned by hoogle to construct a result."
       (when-let ((parsed (ignore-errors (json-parse-string json :object-type 'alist))))
         (propertize (pcase (map-elt parsed 'type)
                       ("" (consult-hoogle--format-value parsed))
                       ("module" (consult-hoogle--format-module parsed))
                       ("package" (consult-hoogle--format-package parsed)))
                     'consult--candidate parsed)))

(defun consult-hoogle--fontify (text)
  "Fontify TEXT, returning the fontified text.
This is adapted from `haskell-fontify-as-mode' but for better performance
instead of running `haskell-mode' we just extract the font-lock parts from
it we need."
  (with-current-buffer " *Hoogle Fontification*"
    (erase-buffer) (insert text)
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings (font-lock-fontify-buffer)))
    (buffer-substring (point-min) (point-max))))

(defun consult-hoogle--format-value (alist) "Construct the disaply string from ALIST for a value."
       (let* ((item (consult-hoogle--fontify (map-elt alist 'item)))
              (module (map-nested-elt alist '(module name) ""))
              (package (map-nested-elt alist '(package name) "")))
                 (if (not consult-hoogle-show-module-and-package) item
                   (concat item (propertize " from " 'face 'font-lock-comment-face) (propertize module 'face 'haskell-keyword-face)
                   (propertize " in " 'face 'font-lock-comment-face) (propertize package 'face 'haskell-quasi-quote-face)))))

(defun consult-hoogle--format-module (alist) "Construct the disaply string from ALIST for a module."
       (let ((name (cadr (split-string (map-elt alist 'item) nil t " +")))
             (package (map-nested-elt alist '(package name) "")))
         (concat "Module " (propertize name 'face 'haskell-keyword-face) " in " (propertize package 'face 'haskell-quasi-quote-face))))

(defun consult-hoogle--format-package (alist) "Construct the disaply string from ALIST for a package."
       (let ((name (cadr (split-string (map-elt alist 'item) nil t " +"))))
         (concat "Package " (propertize name 'face 'haskell-quasi-quote-face))))

;;;; Following the urls from hoogle results.
(defun consult-hoogle--browse-url (type &optional alist) "Open the url of TYPE from ALIST."
       (if-let ((type-alist (map-elt alist 'type))
                (type-url (pcase type-alist
                            ("" type)
                            ("module" (if (eq type 'module) 'item type))
                            ("package" (if (eq type 'package) 'item type))))
                (url (pcase type-url
                       ('item (map-elt alist 'url))
                       ('module (map-nested-elt alist '(module url)))
                       ('package (map-nested-elt alist '(package url))))))
           (if (or (eq type 'package) (equal type-alist "package"))
                (browse-url (concat url "index.html"))
             (browse-url url))
         (message "No suitable url for current alist.")))

;;;; Constructing the details buffer for the selected result
(defun consult-hoogle--details (alist) "Construct the details from ALIST."
       (pcase (map-elt alist 'type)
         ("" (consult-hoogle--details-value alist))
         ("module" (consult-hoogle--details-module alist))
         ("package" (consult-hoogle--details-package alist)))
       (let ((beg (point)))
         (insert (map-elt alist 'docs)) (shr-render-region beg (point-max)) (goto-char (point-min))))

(defun consult-hoogle--details-value (alist) "Construct the disaply string from ALIST for a value."
       (let* ((item (map-elt alist 'item))
              (module (map-nested-elt alist '(module name) ""))
              (package (map-nested-elt alist '(package name) "")))
         (insert (consult-hoogle--fontify item) "\n"
                 (propertize "Module: " 'face 'bold) (propertize module 'face 'haskell-keyword-face) "\n"
                 (propertize "Package: " 'face 'bold) (propertize package 'face 'haskell-quasi-quote-face) "\n")))

(defun consult-hoogle--details-module (alist) "Construct the disaply string from ALIST for a module."
       (let ((name (cadr (split-string (map-elt alist 'item) nil t " +")))
             (package (map-nested-elt alist '(package name) "")))
         (insert (propertize "Module: " 'face 'bold) (propertize name 'face 'haskell-keyword-face) "\n"
                 (propertize "Package: " 'face 'bold) (propertize package 'face 'haskell-quasi-quote-face) "\n")))

(defun consult-hoogle--details-package (alist) "Construct the disaply string from ALIST for a package."
       (let ((name (cadr (split-string (map-elt alist 'item) nil t " +"))))
         (insert (propertize "Package: " 'face 'bold) (propertize name 'face 'haskell-quasi-quote-face) "\n")))

(defun consult-hoogle--show-details (action cand) "Show the details for the current CAND and handle ACTION."
       (when (equal (buffer-name) " *Hoogle Documentation*")
         (erase-buffer)
         (pcase action
           ('preview (when cand (consult-hoogle--details cand)))
           ('return (kill-buffer-and-window) (kill-buffer " *Hoogle Fontification*")))))

;;;; Consult integration
(defun consult-hoogle--search (&optional state action)
  "Search the local hoogle database and take ACTION with the selection.
STATE is the optional state function passed to the consult--read."
  (let ((consult-async-min-input 0)
        (fun (or action (lambda (alist) (consult-hoogle--browse-url 'item alist)))))
    (with-current-buffer (get-buffer-create " *Hoogle Fontification*" t) (let (haskell-mode-hook) (haskell-mode)))
    (funcall fun (consult--read (consult--async-command #'consult-hoogle--builder
                                  (consult--async-map #'consult-hoogle--format-result)
                                  (consult--async-highlight #'consult-hoogle--builder))
                                :prompt "Hoogle: " :require-match t :initial (consult--async-split-initial "")
                                :lookup #'consult--lookup-candidate :state state :sort nil
                                :keymap consult-hoogle-map
                                :add-history (consult--async-split-thingatpt 'symbol)
                                :history '(:input consult-hoogle--history)))
    (when-let ((buf (get-buffer " *Hoogle Fontification*"))) (kill-buffer buf))))

;;;; Interactive Commands
;;;###autoload
(defun consult-hoogle (arg)
  "Search the local hoogle database.
By default this shows the documentation for the current candidate in a side
window. This can be disabled by a prefix ARG."
  (interactive (list current-prefix-arg))
  (if arg (consult-hoogle--search)
    (let* ((buf (get-buffer-create " *Hoogle Documentation*" t))
           (window (display-buffer buf '(display-buffer-in-side-window (window-height . ,(+ 3 vertico-count)) (side . bottom) (slot . -1)))))
      (with-selected-window window (consult-hoogle--search #'consult-hoogle--show-details)))))

(defun consult-hoogle-browse-item () "Browse the url for current item." (interactive)
       (consult-hoogle--browse-url 'item (get-text-property 0 'consult--candidate (run-hook-with-args-until-success 'consult--completion-candidate-hook))))

(defun consult-hoogle-browse-module () "Browse the url for the module the current item belongs to." (interactive)
       (consult-hoogle--browse-url 'module (get-text-property 0 'consult--candidate (run-hook-with-args-until-success 'consult--completion-candidate-hook))))

(defun consult-hoogle-browse-package () "Browse the url for the package the current item belongs to." (interactive)
       (consult-hoogle--browse-url 'package (get-text-property 0 'consult--candidate (run-hook-with-args-until-success 'consult--completion-candidate-hook))))

(defun consult-hoogle-scroll-docs-down (&optional arg) "Scroll the window with documentation ARG lines down." (interactive)
       (with-selected-window (get-buffer-window " *Hoogle Documentation*") (scroll-down arg)))

(defun consult-hoogle-scroll-docs-up (&optional arg) "Scroll the window with documentation ARG lines down." (interactive)
       (with-selected-window (get-buffer-window " *Hoogle Documentation*") (scroll-up arg)))

(provide 'consult-hoogle)

;;; consult-hoogle.el ends here
