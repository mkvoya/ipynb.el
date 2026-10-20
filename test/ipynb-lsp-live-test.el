;;; ipynb-lsp-live-test.el --- Real server integration -*- lexical-binding: t; -*-
(require 'ert)
(require 'ipynb-lsp)
(defvar ipynb-test-lsp-command
  (or (getenv "IPYNB_TEST_LSP") (executable-find "basedpyright-langserver") (executable-find "pyright-langserver")))
(ert-deftest ipynb-lsp-live-cross-cell ()
  (skip-unless ipynb-test-lsp-command)
  (let ((parent (generate-new-buffer " *ipynb-lsp-test*")) editor)
    (unwind-protect
        (with-current-buffer parent
          (ipynb-mode)
          (puthash "source" "def double(number: int) -> int:\n    return number * 2" (car ipynb--cells))
          (ipynb-insert-cell)
          (puthash "source" "result = double(21)\nresult" (cadr ipynb--cells))
          (ipynb-lsp-start (list ipynb-test-lsp-command "--stdio"))
          (ipynb-edit-cell) (setq editor (current-buffer))
          (goto-char (point-min)) (search-forward "double")
          (let ((hover (ipynb-lsp--request :textDocument/hover)))
            (should (string-match-p "double" (ipynb-lsp--markup (plist-get hover :contents)))))
          (let* ((locations (ipynb-lsp--request :textDocument/definition))
                 (xrefs (ipynb-lsp--xrefs locations)))
            (should xrefs)
            (should (string-match-p "Cell 1" (xref-item-summary (car xrefs)))))
          (let ((references (ipynb-lsp--request :textDocument/references '(:context (:includeDeclaration t)))))
            (should (= 2 (length references))))
          (ipynb-lsp-rename "twice")
          (with-current-buffer parent
            (should (string-prefix-p "def twice" (gethash "source" (car ipynb--cells))))
            (should (string-match-p "twice(21)" (gethash "source" (cadr ipynb--cells)))))
          ;; Unsaved code in the first cell participates in second-cell completion.
          (let ((first (with-current-buffer parent (ipynb-lsp--ensure-editor (car ipynb--cells)))))
            (with-current-buffer first (goto-char (point-max)) (insert "\nunsaved_variable = 123")))
          (goto-char (point-max)) (insert "\nunsaved_")
          (let ((capf (ipynb-lsp-completion-at-point)))
            (should (member "unsaved_variable" (all-completions "unsaved_" (nth 2 capf)))))
          (with-current-buffer parent
            (let ((deadline (+ (float-time) 15)))
              (while (and (not ipynb-lsp--diagnostics) (< (float-time) deadline)) (accept-process-output nil 0.1)))
            (should ipynb-lsp--diagnostics)))
      (when (buffer-live-p parent)
        (with-current-buffer parent
          (ipynb-lsp-stop)
          (dolist (buffer ipynb--editors) (when (buffer-live-p buffer) (with-current-buffer buffer (set-buffer-modified-p nil)) (kill-buffer buffer)))
          (set-buffer-modified-p nil))
        (kill-buffer parent)))))

(ert-deftest ipynb-lsp-live-formatting ()
  (skip-unless (getenv "IPYNB_TEST_PYLSP"))
  (with-temp-buffer
    (ipynb-mode)
    (puthash "source" "x=  1\n" (car ipynb--cells))
    (ipynb-insert-cell)
    (puthash "source" "y=  2\n" (cadr ipynb--cells))
    (unwind-protect
        (progn
          (ipynb-lsp-start (list (getenv "IPYNB_TEST_PYLSP")))
          (ipynb-format-cell t)
          (should (equal "x = 1\n" (gethash "source" (car ipynb--cells))))
          (should (equal "y = 2\n" (gethash "source" (cadr ipynb--cells)))))
      (ipynb-lsp-stop))))
