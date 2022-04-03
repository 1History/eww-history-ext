
(add-to-list 'load-path default-directory)

(require 'ert)
(require 'eww-history-ext)

(setq eww-history-ext-db-file (make-temp-file "eww-history-ext"))
(eww-history-ext-enable)

(defun eww-history-ext-test-visit-url (url)
  (let ((buf (get-buffer-create "eww-history-test")))
    (if (version<= "28" emacs-version)
        (eww url nil buf)
      ;; https://github.com/emacs-mirror/emacs/blob/emacs-27/lisp/net/eww.el#L266
      ;; EWW don't have buffer arguments in Emacs 27.
      (eww-history-ext-save-history url ""))
    (kill-buffer buf)))

(ert-deftest test-eww-history ()
  (let ((urls '("https://www.gnu.org/"
                "https://www.baidu.com/"
                "https://github.com/"
                "https://www.gnu.org/")))
    (dolist (url urls)
      (eww-history-ext-test-visit-url url))

    (let* ((results (eww-history-ext-query-latest))
           (ids (seq-map 'car results))
           (rows (seq-map 'cadr results))
           (out-urls (seq-map (lambda (r)
                                (seq-elt r 2))
                              rows))
           (out-counts (seq-map (lambda (r)
                                  (seq-elt r 3))
                                rows)))
      ;; Results
      ;;   [("4"
      ;;     ["2022-04-03 09:22:29" "" "https://www.gnu.org/" "2"])
      ;;    ("3"
      ;;     ["2022-04-03 09:22:29" "" "https://github.com/" "1"])
      ;;    ("2"
      ;;     ["2022-04-03 09:22:29" "" "https://www.baidu.com/" "1"])
      ;;    ("1"
      ;;     ["2022-04-03 09:22:29" "" "https://www.gnu.org/" "2"])]

      (should (eql (length results) 4))
      (should (equal out-urls (seq-reverse urls)))
      (should (equal out-counts '("2" "1" "1" "2")))
      )))
