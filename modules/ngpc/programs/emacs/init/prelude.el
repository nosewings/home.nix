(cl-float-limits)

(setq make-backup-files nil
      create-lockfiles nil)

(setq custom-file null-device)

(setq frame-resize-pixelwise t
      show-paren-delay 0)
(column-number-mode)
(show-paren-mode)

(setq-default indent-tabs-mode nil)

(global-hl-line-mode)
(defun ngpc/disable-hl-line ()
  (setq-local global-hl-line-mode nil))

(defun ngpc/enable-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace t))

(xterm-mouse-mode)

(setq echo-keystrokes cl-least-positive-float)

(defun find-file/make-directory (filename &optional wildcards)
  (let ((dir (file-name-directory filename)))
    (unless (file-exists-p dir)
      (make-directory dir t))))
(advice-add #'find-file :before #'find-file/make-directory)

;; Adapted from a similar Spacemacs function.
(defun ngpc/rename-current-buffer-file ()
  "Interactively rename the current buffer and its associated file."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (when (not (and current-file (file-exists-p current-file)))
      (error "Buffer is not visiting a file"))
    (let* ((current-buffer-name (buffer-name))
           (new-file (read-file-name "Rename file and buffer: " (file-name-directory current-file)))
           (new-dir (file-name-directory new-file)))
      (when (not (file-exists-p new-dir))
        (make-directory new-dir t))
      (rename-file current-file new-file)
      (set-visited-file-name new-file)
      (set-buffer-modified-p nil)
      (when (fboundp 'recentf-add-file)
        (recentf-remove-if-non-kept current-file)
        (recentf-add-file new-file)))))

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
      (kill-new (prin1-to-string (eval (read (buffer-substring (region-beginning) (region-end)))))))))
(global-set-key (kbd "C-x C-e") #'ngpc/eval-last-sexp)

(defun ngpc/switch-theme (theme)
  (interactive "SSwitch to theme: ")
  ;; Try to enable the new theme first: if it doesn't exist, we don't
  ;; want to disable the current themes.
  (load-theme theme t)
  (dolist (atheme custom-enabled-themes)
    (when (not (eq atheme theme))
      (disable-theme atheme))))

(defconst ngpc/quit-prompts
  '("L'important n'est pas la chute: c'est l'atterrissage! "))
(setq confirm-kill-emacs
      (defun ngpc/confirm-kill-emacs (_)
        (yes-or-no-p (seq-random-elt ngpc/quit-prompts))))

(defalias 'rm #'delete-file)
(defalias 'rmdir #'delete-directory)
