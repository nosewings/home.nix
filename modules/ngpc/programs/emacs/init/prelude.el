(cl-float-limits)

(setq make-backup-files nil
      create-lockfiles nil)

(setq frame-resize-pixelwise t
      show-paren-delay 0)
(column-number-mode)
(global-display-line-numbers-mode)
(global-hl-line-mode)
(show-paren-mode)

(setq-default indent-tabs-mode nil)

(setq-default show-trailing-whitespace t)
(defun ngpc/dont-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace nil))

(xterm-mouse-mode)

(setq echo-keystrokes cl-least-positive-float)

(defun ngpc/rename-current-buffer-file (new-file)
  (interactive (list (read-file-name "Rename current buffer file: " nil nil nil (file-name-nondirectory (buffer-file-name)))))
  (rename-file (buffer-file-name new-file)
               (rename-buffer (file-name-nondirectory new-file))))

(defun ngpc/insert-fake-sha256 ()
  (interactive)
  (insert "0000000000000000000000000000000000000000000000000000"))

(defmacro ngpc/with-output-eater (var &rest body)
  (declare (indent 1))
  (let ((tmpvar (make-symbol "tmp")))
    `(let ((,tmpvar nil)
           (,var (lambda (c) (setq ,tmpvar (cons c ,tmpvar)))))
       ,@body
       (concat (nreverse ,tmpvar)))))

(defun ngpc/eval-last-sexp (arg)
  (interactive "P")
  (if (not (eq arg 1))
      (call-interactively #'eval-last-sexp)
    (save-mark-and-excursion
      (mark-sexp -1)
      (kill-new (ngpc/with-output-eater out
                  (prin1 (eval (read (buffer-substring (region-beginning) (region-end)))) out))))))
(global-set-key (kbd "C-x C-e") #'ngpc/eval-last-sexp)
