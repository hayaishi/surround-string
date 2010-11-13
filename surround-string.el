;-*- Emacs-Lisp -*-
(require 'cl)
(defun surround-string (surround-string)
  (interactive)
  (lexical-let* ((surround-list (split-string surround-string "|"))
                 (begin-string (car surround-list))
                 (end-string (cadr surround-list)))
    (lambda ()
      (interactive)
      (let* ((befor-point-max (point-max))
             (begin (if mark-active
                        (region-beginning)
                      (progn
                        (mark-text-object)
                        (region-beginning))))
             (end (point))
             (match-rgx (mapconcat 'identity (list "^" begin-string ".*?" end-string "$") ""))
             (replace-rgx (mapconcat
                           'identity
                           (list "^\\(" begin-string "\\)\\(.*?\\)\\(" end-string "\\)$") ""))
             (select-string (buffer-substring-no-properties begin end)))
        (if (null (string-match match-rgx select-string))
            (progn
              (goto-char end)
              (insert end-string)
              (goto-char begin)
              (insert begin-string))
          (progn
            (setq select-string (replace-regexp-in-string replace-rgx "\\2" select-string))
            (delete-region begin end)
            (goto-char begin)
            (insert select-string)))
        (goto-char (+ end (- (point-max) befor-point-max (length end-string))))))))

(defun mark-text-object ()
  (interactive)
  (let* ((current-point (point))
         (begin (if (re-search-backward " \\|\n" nil 1)
                    (goto-char (+ (point) 1))
                  (point-min)))
         (end (if (re-search-forward " \\|\n" nil 1)
                    (goto-char (- (point) 1))
                (point-max))))
    (progn
      (push-mark begin t transient-mark-mode)
      (goto-char end))))

(provide 'surround-string)
