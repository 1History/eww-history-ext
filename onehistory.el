;;; onehistory.el --- History solution for emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jiacai Liu

;; Author: Jiacai Liu <jiacai2050@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: eww, elfeed, history
;; URL: https://github.com/1History/emacs-onehistory

;;; Code:

(require 'onehistory-dyn)
(require 'tabulated-list)
(require 'seq)

(defcustom onehistory-db-file (expand-file-name "onehistory.db" user-emacs-directory)
  "File where onehistory will store its database."
  :group 'onehistory
  :type 'file)

(defcustom onehistory-eww-integration t
  "Whether save eww history to onehistory"
  :group 'onehistory
  :type 'boolean)

(defcustom onehistory-elfeed-integration nil
  "Whether save elfeed history to onehistory"
  :group 'onehistory
  :type 'boolean)

(defcustom onehistory-default-query-keyword nil
  "Default keyword for query history"
  :group 'onehistory
  :type '(choice (const :tag "Don't set" nil)
                 (string :tag "Keyword"))
  :set (lambda (name value)
         (custom-set-default name value)
         (setq onehistory-query-keyword value)))

(defcustom onehistory-default-query-limit 1000
  "Default limit for query history"
  :group 'onehistory
  :type 'integer
  :set (lambda (name value)
         (custom-set-default name value)
         (setq onehistory-query-limit value)))

(defvar onehistory-db nil
  "The core database for elfeed.")

(defvar onehistory-query-keyword onehistory-default-query-keyword
  "The case-insensitive keyword used when query history.")

(defvar onehistory-query-limit onehistory-default-query-limit
  "Limit how many histories return when call query-latest")

(defvar onehistory-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'onehistory-browse-history)
    (define-key map (kbd "w") 'onehistory-copy-history-url)
    (define-key map (kbd "t") 'onehistory-copy-history-title)
    (define-key map (kbd "s") 'onehistory-search-by-keyword)
    (define-key map (kbd "s-u") 'tabulated-list-revert)
    map)
  "Local keymap for onehistory mode buffers.")

(defun onehistory-db-ensure ()
  (when (null onehistory-db)
    (setf onehistory-db (onehistory-dyn--open-db onehistory-db-file))))

(defun onehistory-save-history (url title)
  (onehistory-db-ensure)
  (onehistory-dyn--save-history onehistory-db
                                url
                                title))

(defun onehistory-query-by-range (start-time end-time)
  (onehistory-db-ensure)
  (onehistory-dyn--query-histories-by-range onehistory-db
                                            (string-to-number (format-time-string "%s" start-time))
                                            (string-to-number (format-time-string "%s" end-time))
                                            onehistory-query-keyword))

(defun onehistory-query-latest ()
  (onehistory-db-ensure)
  (onehistory-dyn--query-latest-histories onehistory-db
                                          onehistory-query-limit
                                          onehistory-query-keyword))

(defun onehistory-eww-hook ()
  (let ((title (plist-get eww-data :title))
        (url (plist-get eww-data :url)))
    (if (null url)
        (message "Can't find url in %s" major-mode)
      (onehistory-save-history url title))))

(defun onehistory-save-elfeed-entry (entry)
  (let ((title (elfeed-entry-title entry))
        (url (elfeed-entry-link entry)))
    (if (null url)
        (message "Can't find url in %s" major-mode)
      (onehistory-save-history url title))))

(defun onehistory-elfeed-search-show-entry-around (orign-func entry)
  (onehistory-save-elfeed-entry entry)
  (funcall orign-func entry))

;;;###autoload
(defun onehistory-enable ()
  "Enable onehistory to save history"
  (interactive)
  (when onehistory-eww-integration
    (advice-add 'elfeed-search-show-entry :around
                'onehistory-elfeed-search-show-entry-around))

  (when onehistory-elfeed-integration
    (add-hook 'eww-after-render-hook 'onehistory-eww-hook)))

;;;###autoload
(defun onehistory-disable ()
  "Disable onehistory to save history"
  (interactive)
  (when onehistory-eww-integration
    (remove-hook 'eww-after-render-hook 'onehistory-eww-hook))

  (when onehistory-elfeed-integration
    (advice-remove 'elfeed-search-show-entry
                   'onehistory-elfeed-search-show-entry-around)))

(defun onehistory--get-url ()
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 2)))

(defun onehistory--get-title ()
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 1)))

(defun onehistory-browse-history ()
  "Browse history at point."
  (interactive)
  (if-let ((url (onehistory--get-url)))
      (browse-url url)
    (user-error "There is no history at point")))

(defun onehistory-copy-history-url ()
  "Copy history URL at point."
  (interactive)
  (if-let ((url (onehistory--get-url)))
      (progn
        (message url)
        (kill-new url))
    (user-error "There is no history at point")))

(defun onehistory-copy-history-title ()
  "Copy history title at point."
  (interactive)
  (if-let ((title (onehistory--get-title)))
      (progn
        (message title)
        (kill-new title))
    (user-error "There is no history at point")))

(defun onehistory-tabulated-list-revert (&optional revert)
  (setq-local onehistory-query-keyword onehistory-default-query-keyword)
  (setq-local onehistory-query-limit onehistory-default-query-limit))

(defun onehistory-search-by-keyword (keyword)
  (interactive "sKeyword: ")
  (when (eq major-mode 'onehistory-mode)
    (setq-local onehistory-query-keyword keyword)
    (tabulated-list-print t)))

(define-derived-mode onehistory-mode tabulated-list-mode "onehistory" "History solution for Emacs"
  (setq tabulated-list-format [("Time" 20 t)
                               ("Title" 50 t)
                               ("Location" 70 t)
                               ("Visit Count" 10 (lambda (x y)
                                                   (>
                                                    (string-to-number (aref (cadr x) 3))
                                                    (string-to-number (aref (cadr y) 3)))))]

        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Time" t)
        tabulated-list-entries (lambda ()
                                 (seq-into (onehistory-query-latest) 'list)))

  (add-hook 'tabulated-list-revert-hook 'onehistory-tabulated-list-revert nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun onehistory-list ()
  "Display histories as table list"
  (interactive)
  (with-current-buffer (get-buffer-create "*onehistory*")
    (onehistory-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(provide 'onehistory)

;; Local Variables:
;; coding: utf-8
;; End:

;;; onehistory.el ends here
