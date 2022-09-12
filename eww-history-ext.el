;;; eww-history-ext.el --- Persist eww histories into SQLite  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jiacai Liu

;; Author: Jiacai Liu <jiacai2050@gmail.com>
;; Version: 0.2.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: eww, elfeed, history
;; URL: https://github.com/1History/eww-history-ext

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; See documentation in README.org or visit homepage

;;; Code:

(require 'eww-history-ext-dyn)
(require 'tabulated-list)
(require 'seq)

(defcustom eww-history-ext-db-file (expand-file-name "eww-history.db" user-emacs-directory)
  "File where eww-history-ext will store its database."
  :group 'eww-history-ext
  :type 'file)

(defcustom eww-history-ext-eww-integration t
  "Whether save eww history to eww-history-ext"
  :group 'eww-history-ext
  :type 'boolean)

(defcustom eww-history-ext-elfeed-integration nil
  "Whether save elfeed history to eww-history-ext"
  :group 'eww-history-ext
  :type 'boolean)

(defcustom eww-history-ext-default-query-keyword nil
  "Default keyword for query history"
  :group 'eww-history-ext
  :type '(choice (const :tag "Don't set" nil)
                 (string :tag "Keyword"))
  :set (lambda (name value)
         (custom-set-default name value)
         (setq eww-history-ext-query-keyword value)))

(defcustom eww-history-ext-default-query-limit 1000
  "Default limit for query history"
  :group 'eww-history-ext
  :type 'integer
  :set (lambda (name value)
         (custom-set-default name value)
         (setq eww-history-ext-query-limit value)))

(defvar eww-history-ext-db nil
  "The core database for elfeed.")

(defvar eww-history-ext-query-keyword eww-history-ext-default-query-keyword
  "The case-insensitive keyword used when query history.")

(defvar eww-history-ext-query-limit eww-history-ext-default-query-limit
  "Limit how many histories return when call query-latest")

(defvar eww-history-ext-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'eww-history-ext-browse-history)
    (define-key map (kbd "w") 'eww-history-ext-copy-history-url)
    (define-key map (kbd "t") 'eww-history-ext-copy-history-title)
    (define-key map (kbd "s") 'eww-history-ext-search-by-keyword)
    (define-key map (kbd "d") 'eww-history-ext-delete-history)
    (define-key map (kbd "b") 'eww-history-ext-add-bookmark)
    (define-key map (kbd "s-u") 'tabulated-list-revert)
    map)
  "Local keymap for eww-history-ext mode buffers.")

(defun eww-history-ext-db-ensure ()
  (when (null eww-history-ext-db)
    (setf eww-history-ext-db (eww-history-ext-dyn--open-db eww-history-ext-db-file))))

(defun eww-history-ext-save-history (url title)
  (eww-history-ext-db-ensure)
  (eww-history-ext-dyn--save-history eww-history-ext-db
                                 url
                                 title))

(defun eww-history-ext-query-by-range (start-time end-time)
  (eww-history-ext-db-ensure)
  (eww-history-ext-dyn--query-histories-by-range eww-history-ext-db
                                             (string-to-number (format-time-string "%s" start-time))
                                             (string-to-number (format-time-string "%s" end-time))
                                             eww-history-ext-query-keyword))

(defun eww-history-ext-query-latest ()
  (eww-history-ext-db-ensure)
  (eww-history-ext-dyn--query-latest-histories eww-history-ext-db
                                           eww-history-ext-query-limit
                                           eww-history-ext-query-keyword))

(defun eww-history-ext-eww-hook ()
  (let ((title (plist-get eww-data :title))
        (url (plist-get eww-data :url)))
    (if (null url)
        (message "Can't find url in %s" major-mode)
      (eww-history-ext-save-history url title))))

(defun eww-history-ext-save-elfeed-entry (entry)
  (let ((title (elfeed-entry-title entry))
        (url (elfeed-entry-link entry)))
    (if (null url)
        (message "Can't find url in %s" major-mode)
      (eww-history-ext-save-history url title))))

(defun eww-history-ext-elfeed-search-show-entry-around (orign-func entry)
  (eww-history-ext-save-elfeed-entry entry)
  (funcall orign-func entry))

;;;###autoload
(defun eww-history-ext-enable ()
  "Enable eww-history-ext to save history"
  (interactive)
  (when eww-history-ext-eww-integration
    (add-hook 'eww-after-render-hook 'eww-history-ext-eww-hook))

  (when eww-history-ext-elfeed-integration
    (advice-add 'elfeed-search-show-entry :around
                'eww-history-ext-elfeed-search-show-entry-around)))

;;;###autoload
(defun eww-history-ext-disable ()
  "Disable eww-history-ext to save history"
  (interactive)
  (when eww-history-ext-eww-integration
    (remove-hook 'eww-after-render-hook 'eww-history-ext-eww-hook))

  (when eww-history-ext-elfeed-integration
    (advice-remove 'elfeed-search-show-entry
                   'eww-history-ext-elfeed-search-show-entry-around)))

(defun eww-history-ext--get-url ()
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 2)))

(defun eww-history-ext--get-title ()
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 1)))

(defun eww-history-ext-browse-history ()
  "Browse history at point."
  (interactive)
  (if-let ((url (eww-history-ext--get-url)))
      (browse-url url)
    (user-error "There is no history at point")))

(defun eww-history-ext-copy-history-url ()
  "Copy history URL at point."
  (interactive)
  (if-let ((url (eww-history-ext--get-url)))
      (progn
        (message url)
        (kill-new url))
    (user-error "There is no history at point")))

(defun eww-history-ext-copy-history-title ()
  "Copy history title at point."
  (interactive)
  (if-let ((title (eww-history-ext--get-title)))
      (progn
        (message title)
        (kill-new title))
    (user-error "There is no history at point")))

(defun eww-history-ext-delete-history ()
  "Delete history at point."
  (interactive)
  (if-let ((entry (tabulated-list-delete-entry)))
      (eww-history-ext-dyn--delete-history eww-history-ext-db
                                           (string-to-number (car entry)))
    (user-error "There is no history at point")))

(defun eww-history-ext-add-bookmark ()
  "Bookmark history at point."
  (interactive)
  (when-let* ((title (eww-history-ext--get-title))
              (url (eww-history-ext--get-url))
              (eww-data `(:url ,url :title ,title)))
    (eww-add-bookmark)))

(defun eww-history-ext-tabulated-list-revert (&optional revert)
  (setq-local eww-history-ext-query-keyword eww-history-ext-default-query-keyword)
  (setq-local eww-history-ext-query-limit eww-history-ext-default-query-limit))

(defun eww-history-ext-search-by-keyword (keyword)
  (interactive "sKeyword: ")
  (when (eq major-mode 'eww-history-ext-mode)
    (setq-local eww-history-ext-query-keyword keyword)
    (tabulated-list-print t)))

(define-derived-mode eww-history-ext-mode tabulated-list-mode "eww-history-ext" "Persist eww histories into SQLite"
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
                                 (seq-into (eww-history-ext-query-latest) 'list)))

  (add-hook 'tabulated-list-revert-hook 'eww-history-ext-tabulated-list-revert nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun eww-history-ext-list ()
  "Display histories as table list"
  (interactive)
  (with-current-buffer (get-buffer-create "*eww-history-ext*")
    (eww-history-ext-mode)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(provide 'eww-history-ext)

;; Local Variables:
;; coding: utf-8
;; End:

;;; eww-history-ext.el ends here
