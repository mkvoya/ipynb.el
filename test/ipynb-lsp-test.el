;;; ipynb-lsp-test.el --- LSP regression tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'ipynb-lsp)

(ert-deftest ipynb-lsp-utf16-and-boundaries ()
  (with-temp-buffer
    (ipynb-mode)
    (puthash "source" "x = '😀'\n世界 = x" (car ipynb--cells))
    (setq ipynb--cells (append ipynb--cells (list (ipynb--new-cell "markdown") (ipynb--new-cell))))
    (puthash "source" "print(世界)" (nth 2 ipynb--cells))
    (let* ((built (ipynb-lsp--build)) (text (car built)) (segs (cdr built)))
      (should (= 2 (length segs)))
      (dotimes (i (1+ (length text)))
        (should (= i (ipynb-lsp--offset text (ipynb-lsp--position text i)))))
      (should-error (ipynb-lsp--offset text '(:line 0 :character 6)))
      (should-error (ipynb-lsp--plan-edits
                     (list (list :range (list :start (ipynb-lsp--position text 0)
                                              :end (ipynb-lsp--position text (length text))) :newText "oops")) text segs)))))

(ert-deftest ipynb-lsp-atomic-edit-plan ()
  (with-temp-buffer
    (ipynb-mode)
    (puthash "source" "foo = 1" (car ipynb--cells))
    (ipynb-insert-cell) (puthash "source" "print(foo)" (cadr ipynb--cells))
    (let* ((built (ipynb-lsp--build)) (text (car built)) (segments (cdr built))
           (edits (list (list :range '(:start (:line 0 :character 0) :end (:line 0 :character 3)) :newText "answer")
                        (list :range '(:start (:line 2 :character 6) :end (:line 2 :character 9)) :newText "answer"))))
      (ipynb-lsp--commit-plan (ipynb-lsp--plan-edits edits text segments))
      (should (equal "answer = 1" (gethash "source" (car ipynb--cells))))
      (should (equal "print(answer)" (gethash "source" (cadr ipynb--cells))))
      (ipynb-undo)
      (should (equal "foo = 1" (gethash "source" (car ipynb--cells))))
      (should (equal "print(foo)" (gethash "source" (cadr ipynb--cells)))))))

(ert-deftest ipynb-wide-separator ()
  (let ((ipynb-separator-width 100))
    (should (= 100 (string-width (string-trim-right (ipynb--separator "── 1 · code [ ]")))))))

(ert-deftest ipynb-lsp-stale-diagnostics-and-cell-mapping ()
  (let ((editor (generate-new-buffer " *diagnostic-cell*")))
    (unwind-protect
        (with-temp-buffer
          (ipynb-mode)
          (puthash "source" "emoji = '😀'" (car ipynb--cells))
          (ipynb-insert-cell) (puthash "source" "unknown" (cadr ipynb--cells))
          (let ((built (ipynb-lsp--build)))
            (setq ipynb-lsp--text (car built) ipynb-lsp--segments (cdr built)
                  ipynb-lsp--uri "file:///test.py" ipynb-lsp--version 3))
          (let ((diagnostics [(:range (:start (:line 2 :character 0) :end (:line 2 :character 7)) :severity 1 :message "Unknown name")]))
            (ipynb-lsp--notification nil 'textDocument/publishDiagnostics
                                    (list :uri ipynb-lsp--uri :version 2 :diagnostics diagnostics))
            (should-not ipynb-lsp--diagnostics)
            (ipynb-lsp--notification nil 'textDocument/publishDiagnostics
                                    (list :uri ipynb-lsp--uri :version 3 :diagnostics diagnostics))
            (with-current-buffer editor (insert "unknown"))
            (let ((mapped (ipynb-lsp--cell-diagnostics (cadr ipynb--cells) editor)))
              (should (= 1 (length mapped)))
              (should (= 1 (flymake-diagnostic-beg (car mapped))))
              (should (= 8 (flymake-diagnostic-end (car mapped)))))))
      (when (buffer-live-p editor) (kill-buffer editor)))))

(ert-deftest ipynb-lsp-completion-adds-import-to-other-cell ()
  (let ((parent (generate-new-buffer " *completion-notebook*")) editor)
    (unwind-protect
        (with-current-buffer parent
          (ipynb-mode)
          (puthash "source" "x = 1" (car ipynb--cells))
          (ipynb-insert-cell) (puthash "source" "Pa" (cadr ipynb--cells))
          (let ((built (ipynb-lsp--build)))
            (setq ipynb-lsp--text (car built) ipynb-lsp--segments (cdr built)))
          (ipynb-edit-cell) (setq editor (current-buffer)) (goto-char (point-max))
          (cl-letf (((symbol-function 'ipynb-lsp--live-p) (lambda () t))
                    ((symbol-function 'jsonrpc-notify) #'ignore)
                    ((symbol-function 'ipynb-lsp--request)
                     (lambda (&rest _) [(:label "Path" :additionalTextEdits
                                              [(:range (:start (:line 0 :character 0) :end (:line 0 :character 0))
                                                :newText "from pathlib import Path\n")]) (:label "PathLike")])))
            (let ((completion (ipynb-lsp-completion-at-point)))
              (delete-region (nth 0 completion) (nth 1 completion)) (insert "Path")
              (funcall (plist-get (nthcdr 3 completion) :exit-function) "Path" 'finished)))
          (with-current-buffer parent
            (should (equal "from pathlib import Path\nx = 1" (gethash "source" (car ipynb--cells))))
            (should (equal "Path" (gethash "source" (cadr ipynb--cells))))))
      (when (buffer-live-p editor) (with-current-buffer editor (set-buffer-modified-p nil)) (kill-buffer editor))
      (when (buffer-live-p parent) (with-current-buffer parent (set-buffer-modified-p nil)) (kill-buffer parent)))))
