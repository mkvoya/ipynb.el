;;; ipynb-preview.el --- Inline and separate Markdown previews -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Convert CommonMark to HTML with markdown-it-py and render locally with SHR.
;; No browser, JavaScript, automatic network requests, or local image reads.
;;; Code:
(require 'ipynb)
(require 'shr)
(require 'dom)

(defvar-local ipynb-preview--cache nil)
(defvar-local ipynb-preview--error nil)
(defvar-local ipynb-preview--parent nil)
(defvar-local ipynb-preview--cell nil)
(defvar ipynb-preview--attachments nil)

(defun ipynb-preview--image (dom)
  "Render embedded images in DOM; leave other images as alt text."
  (let* ((src (or (dom-attr dom 'src) "")) (alt (or (dom-attr dom 'alt) "image"))
         (bundle (and (string-prefix-p "attachment:" src) ipynb-preview--attachments
                      (gethash (url-unhex-string (substring src 11)) ipynb-preview--attachments)))
         image)
    (when (and ipynb-display-images (display-images-p))
      (condition-case nil
          (dolist (entry '(("image/png" . png) ("image/jpeg" . jpeg) ("image/svg+xml" . svg)))
            (let* ((mime (car entry)) (kind (cdr entry))
                   (value (or (and bundle (gethash mime bundle))
                              (and (string-prefix-p (concat "data:" mime ";base64,") src)
                                   (substring src (length (concat "data:" mime ";base64,")))))))
              (when (and (not image) value (image-type-available-p kind))
                (setq image (create-image (if (and bundle (eq kind 'svg)) (ipynb--text value)
                                           (base64-decode-string (ipynb--text value)))
                                         kind t :max-width 800 :max-height 500)))))
        (error nil)))
    (if image (insert-image image alt) (insert (format "[Image: %s]" alt)))))
(defun ipynb-preview--html (source)
  (let ((python (ipynb--python)) (script (expand-file-name "python/ipynb_markdown.py" ipynb--directory))
        (stderr (make-temp-file "ipynb-markdown-"))
        (coding-system-for-read 'utf-8-unix) (coding-system-for-write 'utf-8-unix))
    (unwind-protect
        (with-temp-buffer
          (insert source)
          (let ((status (call-process-region (point-min) (point-max) python t (list t stderr) nil script)))
            (unless (equal status 0)
              (error "%s" (with-temp-buffer (insert-file-contents stderr) (string-trim (buffer-string))))))
          (buffer-string))
      (delete-file stderr))))
(defun ipynb-preview--render (source attachments width)
  "Return SOURCE rendered with ATTACHMENTS, wrapped to WIDTH."
  (unless (fboundp 'libxml-parse-html-region) (error "This Emacs needs libxml2 for Markdown previews"))
  (let ((html (ipynb-preview--html source)) (ipynb-preview--attachments attachments)
        (shr-width (max 20 width)) (shr-use-fonts nil) (shr-use-colors nil)
        (shr-inhibit-images t) (shr-external-rendering-functions '((img . ipynb-preview--image))))
    (with-temp-buffer
      (insert html)
      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
        (erase-buffer) (shr-insert-document dom))
      (buffer-string))))
(defun ipynb-preview--string (cell &optional source width)
  "Return cached rendered CELL, or nil on a renderer error."
  (unless ipynb-preview--cache (setq ipynb-preview--cache (make-hash-table :test #'equal)))
  (let* ((source (or source (ipynb--text (gethash "source" cell))))
         (width (or width (window-body-width (or (get-buffer-window (current-buffer)) (selected-window)))))
         (key (list source (ipynb--json (or (gethash "attachments" cell) (ipynb--object))) width ipynb-display-images ipynb-python-command))
         (cached (gethash key ipynb-preview--cache 'missing)))
    (if (not (eq cached 'missing)) (unless (eq cached 'failed) cached)
      (condition-case err
          (let ((rendered (ipynb-preview--render source (gethash "attachments" cell) width)))
            (setq ipynb-preview--error nil)
            (puthash key rendered ipynb-preview--cache) rendered)
        (error (setq ipynb-preview--error (error-message-string err))
               (puthash key 'failed ipynb-preview--cache) nil)))))
;;;###autoload
(defun ipynb-toggle-markdown-preview ()
  "Toggle persistent Markdown rendering in the notebook." (interactive)
  (with-current-buffer (if ipynb-cell-edit-mode ipynb--parent (current-buffer))
    (setq ipynb-markdown-preview (not ipynb-markdown-preview) ipynb-preview--cache nil)
    (ipynb--render)
    (if (and ipynb-markdown-preview ipynb-preview--error)
        (message "Markdown preview: %s" ipynb-preview--error)
      (message "Markdown preview %s" (if ipynb-markdown-preview "enabled" "disabled")))))
(defun ipynb-preview-refresh ()
  "Refresh a standalone preview, including uncommitted cell text." (interactive)
  (unless (buffer-live-p ipynb-preview--parent) (user-error "Notebook closed"))
  (let ((buffer (current-buffer)) (cell ipynb-preview--cell)
        (width (window-body-width (or (get-buffer-window (current-buffer)) (selected-window)))) rendered)
    (with-current-buffer ipynb-preview--parent
      (unless (memq cell ipynb--cells) (user-error "Cell was deleted"))
      (let* ((editor (cl-find-if (lambda (b) (and (buffer-live-p b) (eq (buffer-local-value 'ipynb--cell b) cell))) ipynb--editors))
             (source (and editor (with-current-buffer editor (buffer-substring-no-properties (point-min) (point-max))))))
        (setq ipynb-preview--cache nil rendered (ipynb-preview--string cell source width))
        (unless rendered (user-error "%s" ipynb-preview--error))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer) (insert rendered) (goto-char (point-min))))))
(defvar ipynb-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'ipynb-preview-refresh) map))
(define-derived-mode ipynb-preview-mode special-mode "Markdown preview"
  "Read a notebook Markdown cell.  Press g to refresh, q to close."
  (add-hook 'window-size-change-functions
            (lambda (_window) (when (buffer-live-p ipynb-preview--parent) (ipynb-preview-refresh))) nil t))
;;;###autoload
(defun ipynb-preview-markdown ()
  "Preview the current Markdown cell in a separate Emacs buffer." (interactive)
  (let* ((parent (if ipynb-cell-edit-mode ipynb--parent (current-buffer)))
         (cell (if ipynb-cell-edit-mode ipynb--cell (ipynb--current-cell))))
    (unless (equal (gethash "cell_type" cell) "markdown") (user-error "Select a Markdown cell"))
    (let ((buffer (get-buffer-create (format "*%s Markdown %s*" (buffer-name parent) (gethash "id" cell)))))
      (with-current-buffer buffer
        (ipynb-preview-mode)
        (setq ipynb-preview--parent parent ipynb-preview--cell cell)
        (setq default-directory (buffer-local-value 'default-directory parent))
        (ipynb-preview-refresh))
      (display-buffer buffer))))

(provide 'ipynb-preview)
;;; ipynb-preview.el ends here
