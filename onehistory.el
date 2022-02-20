;;; onehistory.el --- Save eww history to sqlite  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jiacai Liu

;; Author: Jiacai Liu <jiacai2050@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: eww, elfeed, history
;; URL: https://example.com/jrhacker/superfrobnicate

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
  (onehistory-dyn--query-histories onehistory-db
                                   (* 1000 (string-to-number (format-time-string "%s" start-time)))
                                   (* 1000 (string-to-number (format-time-string "%s" end-time)))
                                   keyword))

(defun onehistory-query-latest (&optional keyword)
  (onehistory-db-ensure)
  (onehistory-dyn--query-latest-histories onehistory-db
                                          onehistory-latest-history-limit
                                          keyword))

(provide 'onehistory)

;; Local Variables:
;; coding: utf-8
;; End:

;;; onehistory.el ends here
