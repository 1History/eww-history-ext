;;; onehistory.el --- History solution for emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jiacai Liu

;; Author: Jiacai Liu <jiacai2050@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: eww, elfeed, history
;; URL: https://github.com/1History/emacs-onehistory

;;; Code:

(require 'onehistory-dyn)

(defcustom onehistory-db-file (expand-file-name "onehistory.db" user-emacs-directory)
  "File where onehistory will store its database."
  :group 'onehistory
  :type 'file)

(defcustom onehistory-latest-history-limit 1000
  "Limit how many histories return when call query-latest"
  :group 'onehistory
  :type 'integer)

(defcustom onehistory-eww-integration t
  "Whether save eww history to onehistory"
  :group 'onehistory
  :type 'boolean)

(defcustom onehistory-elfeed-integration nil
  "Whether save elfeed history to onehistory"
  :group 'onehistory
  :type 'boolean)

(defvar onehistory-db nil
  "The core database for elfeed.")

(defun onehistory-db-ensure ()
  (when (null onehistory-db)
    (setf onehistory-db (onehistory-dyn--open-db onehistory-db-file))))

(defun onehistory-save-history (url title)
  (onehistory-db-ensure)
  (onehistory-dyn--save-history onehistory-db
                                url
                                title))

(defun onehistory-query-by-range (start-time end-time &optional keyword)
  (onehistory-db-ensure)
  (onehistory-dyn--query-histories-by-range onehistory-db
                                            (string-to-number (format-time-string "%s" start-time))
                                            (string-to-number (format-time-string "%s" end-time))
                                            keyword))

(defun onehistory-query-latest (&optional limit keyword)
  (onehistory-db-ensure)
  (onehistory-dyn--query-latest-histories onehistory-db
                                          (or limit onehistory-latest-history-limit)
                                          keyword))

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

(provide 'onehistory)

;; Local Variables:
;; coding: utf-8
;; End:

;;; onehistory.el ends here
