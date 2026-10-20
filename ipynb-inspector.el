;;; ipynb-inspector.el --- Jupyter variable inspector -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Inspect symbols used by a cell without executing that cell.  Uses the
;; language-neutral Jupyter inspect protocol, so it works beyond Python.
;;; Code:
(require 'ipynb)
(require 'tabulated-list)

(defcustom ipynb-inspector-max-symbols 200
  "Maximum symbols inspected per refresh." :type 'integer :group 'ipynb)
(defvar-local ipynb-inspector--parent nil)
(defvar-local ipynb-inspector--cells nil)
(defvar-local ipynb-inspector--all nil)
(defvar-local ipynb-inspector--generation 0)
(defvar-local ipynb-inspector--details nil)
(defvar-local ipynb-inspector--pending nil)
(defvar-local ipynb-inspector--process nil)

(defun ipynb-inspector--symbols (source mode)
  "Find unique symbol expressions in SOURCE using MODE's syntax table."
  (with-temp-buffer
    (insert source)
    (when (fboundp mode) (let ((inhibit-message t)) (delay-mode-hooks (funcall mode))))
    (syntax-propertize (point-max))
    (goto-char (point-min))
    (let (result)
      (while (re-search-forward "[[:alpha:]_][[:alnum:]_]*\\(?:\\.[[:alpha:]_][[:alnum:]_]*\\)*" nil t)
        (let* ((name (match-string-no-properties 0))
               (state (save-excursion (syntax-ppss (match-beginning 0)))))
          (unless (or (nth 3 state) (nth 4 state) (member name result)) (push name result))))
      (nreverse result))))
(defun ipynb-inspector--summary (text)
  "Extract a concise type and value summary from kernel inspection TEXT."
  (let ((case-fold-search t) type value)
    (when (string-match "^Type:[ \t]*\\(.+\\)" text) (setq type (match-string 1 text)))
    (when (string-match "^\\(?:Value\\|String form\\|Summary\\):[ \t]*\\(.+\\)" text)
      (setq value (match-string 1 text)))
    (vector (or type "object")
            (truncate-string-to-width (replace-regexp-in-string "[\n\r\t]+" " " (or value (car (split-string text "\n" t)) "")) 100 nil nil "…"))))
(defun ipynb-inspector--next (buffer generation)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (= generation ipynb-inspector--generation)
        (if (or (not (buffer-live-p ipynb-inspector--parent))
                (not (process-live-p ipynb-inspector--process)))
            (setq mode-line-process "Kernel disconnected — start it, then press g")
          (let ((symbol (pop ipynb-inspector--pending)) (parent ipynb-inspector--parent)
                (process ipynb-inspector--process))
            (if (null symbol)
                (setq mode-line-process (format "%d objects · g refresh · RET inspect · q close" (length tabulated-list-entries)))
              (setq mode-line-process (format "Inspecting %s (%d remaining)…" symbol (length ipynb-inspector--pending)))
              (with-current-buffer parent
                (when (eq ipynb--process process)
                  (let ((id (ipynb--id)) timer)
                    (setq timer
                          (run-at-time 15 nil
                                       (lambda ()
                                         (when (buffer-live-p parent)
                                           (with-current-buffer parent (when ipynb--requests (remhash id ipynb--requests))))
                                         (when (buffer-live-p buffer)
                                           (with-current-buffer buffer
                                             (when (= generation ipynb-inspector--generation)
                                               (setq mode-line-process "Inspection timed out (kernel busy?) — press g to retry")))))))
                    (puthash id
                             (lambda (content)
                               (cancel-timer timer)
                               (when (buffer-live-p buffer)
                                 (with-current-buffer buffer
                                   (when (= generation ipynb-inspector--generation)
                                     (when (eq (gethash "found" content) t)
                                       (let* ((text (ansi-color-apply (ipynb--text (gethash "text/plain" (gethash "data" content (ipynb--object))))))
                                              (summary (ipynb-inspector--summary text)))
                                         (puthash symbol text ipynb-inspector--details)
                                         (push (list symbol (vector symbol (aref summary 0) (aref summary 1))) tabulated-list-entries)
                                         (tabulated-list-print t)))
                                     (run-at-time 0 nil #'ipynb-inspector--next buffer generation))))) ipynb--requests)
                    (ipynb--send "inspect" "id" id "code" symbol "cursor" (length symbol))))))))))))

(defun ipynb-inspector-refresh ()
  "Refresh the current inspector from its cells and live kernel." (interactive)
  (unless (buffer-live-p ipynb-inspector--parent) (user-error "Notebook closed"))
  (let ((buffer (current-buffer)) (all ipynb-inspector--all) (cells ipynb-inspector--cells) names)
    (with-current-buffer ipynb-inspector--parent
      (unless (and (process-live-p ipynb--process) (member ipynb--status '("idle" "busy")))
        (user-error "Start the notebook kernel first"))
      (let ((mode (cdr (assoc (ipynb--language) ipynb-language-modes))))
        (dolist (cell (if all ipynb--cells cells))
          (when (and (memq cell ipynb--cells) (equal (gethash "cell_type" cell) "code"))
            (let* ((editor (cl-find-if (lambda (b) (and (buffer-live-p b) (eq (buffer-local-value 'ipynb--cell b) cell))) ipynb--editors))
                   (source (if editor (with-current-buffer editor (buffer-substring-no-properties (point-min) (point-max)))
                             (ipynb--text (gethash "source" cell)))))
              (setq names (append names (ipynb-inspector--symbols source mode)))))))
      (with-current-buffer buffer (setq ipynb-inspector--process (buffer-local-value 'ipynb--process ipynb-inspector--parent))))
    (setq names (delete-dups names))
    (when (> (length names) ipynb-inspector-max-symbols)
      (message "Inspector limited to %d symbols" ipynb-inspector-max-symbols)
      (setq names (cl-subseq names 0 ipynb-inspector-max-symbols)))
    (setq ipynb-inspector--pending names ipynb-inspector--details (make-hash-table :test #'equal)
          tabulated-list-entries nil ipynb-inspector--generation (1+ ipynb-inspector--generation))
    (tabulated-list-print)
    (ipynb-inspector--next buffer ipynb-inspector--generation)))
(defun ipynb-inspector-show () "Show full kernel documentation for the selected object." (interactive)
  (let* ((name (tabulated-list-get-id)) (text (gethash name ipynb-inspector--details)))
    (unless text (user-error "Choose an object row"))
    (with-help-window "*ipynb variable*" (princ (concat name "\n\n" text)))))
(defvar ipynb-inspector-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'ipynb-inspector-refresh)
    (define-key map (kbd "RET") #'ipynb-inspector-show) map))
(define-derived-mode ipynb-inspector-mode tabulated-list-mode "Notebook variables"
  "Inspect live objects used in notebook code cells."
  (setq tabulated-list-format [("Name" 28 t) ("Type" 18 t) ("Value / documentation" 0 t)]
        tabulated-list-padding 2 tabulated-list-sort-key '("Name" . nil))
  (tabulated-list-init-header))
;;;###autoload
(defun ipynb-inspect-cell (&optional all)
  "List live objects referenced by the current cell, or ALL cells with prefix."
  (interactive "P")
  (let* ((parent (if ipynb-cell-edit-mode ipynb--parent (current-buffer)))
         (cell (if ipynb-cell-edit-mode ipynb--cell (ipynb--current-cell)))
         (buffer (get-buffer-create (format "*%s variables*" (buffer-name parent)))))
    (unless (or all (equal (gethash "cell_type" cell) "code")) (user-error "Select a code cell"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ipynb-inspector-mode) (ipynb-inspector-mode))
      (setq ipynb-inspector--parent parent ipynb-inspector--cells (list cell) ipynb-inspector--all all)
      (ipynb-inspector-refresh))
    (pop-to-buffer buffer)))

(provide 'ipynb-inspector)
;;; ipynb-inspector.el ends here
