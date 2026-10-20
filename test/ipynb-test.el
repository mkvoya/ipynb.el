;;; ipynb-test.el --- Notebook regression tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'ipynb)

(defmacro ipynb-test-with-notebook (&rest body)
  `(with-temp-buffer
     (ipynb-mode)
     ,@body))

(ert-deftest ipynb-cell-operations ()
  (ipynb-test-with-notebook
   (puthash "source" "first" (car ipynb--cells))
   (ipynb-insert-cell)
   (puthash "source" "second" (ipynb--current-cell))
   (ipynb-move-cell-up)
   (should (equal "second" (gethash "source" (car ipynb--cells))))
   (ipynb-copy-cell) (ipynb-paste-cell)
   (should (= 3 (length ipynb--cells)))
   (should-not (equal (gethash "id" (car ipynb--cells)) (gethash "id" (cadr ipynb--cells))))
   (ipynb-set-cell-type "markdown")
   (should-not (gethash "outputs" (ipynb--current-cell)))
   (ipynb-delete-cell) (should (= 2 (length ipynb--cells)))
   (ipynb-undo) (should (= 3 (length ipynb--cells)))
   (ipynb-redo) (should (= 2 (length ipynb--cells)))))

(ert-deftest ipynb-roundtrip-preserves-rich-json ()
  (let* ((file (make-temp-file "ipynb-test" nil ".ipynb"))
         (source "{\"nbformat\":4,\"nbformat_minor\":5,\"custom\":null,\"metadata\":{\"flag\":false,\"nested\":{}},\"cells\":[{\"id\":\"abc\",\"cell_type\":\"markdown\",\"metadata\":{},\"source\":[\"# Hi\\n\",\"世界\"],\"attachments\":{\"a\":{\"image/png\":\"AAAA\"}}},{\"id\":\"def\",\"cell_type\":\"code\",\"metadata\":{},\"source\":\"print(1)\",\"execution_count\":null,\"outputs\":[{\"output_type\":\"display_data\",\"data\":{\"text/html\":\"<b>ok</b>\"},\"metadata\":{}}]}]}"))
    (unwind-protect
        (with-temp-buffer
          (insert source) (setq buffer-file-name file) (ipynb-mode)
          (ipynb--write)
          (should (equal (ipynb--json (ipynb--parse source))
                         (ipynb--json (ipynb--parse (with-temp-buffer (insert-file-contents file) (buffer-string)))))))
      (delete-file file))))

(ert-deftest ipynb-cell-editor-save-and-cancel ()
  (let ((notebook (generate-new-buffer " *ipynb-test*")) editor)
    (unwind-protect
        (with-current-buffer notebook
          (ipynb-mode) (ipynb-edit-cell) (setq editor (current-buffer))
          (insert "x = 1\n") (ipynb-cell-save)
          (should (equal "x = 1\n" (gethash "source" (buffer-local-value 'ipynb--cell editor))))
          (insert "discard me") (ipynb-cell-abort)
          (with-current-buffer notebook
            (should (equal "x = 1\n" (gethash "source" (car ipynb--cells))))
            (ipynb-undo) (should (equal "" (gethash "source" (car ipynb--cells))))))
      (when (buffer-live-p editor) (kill-buffer editor))
      (when (buffer-live-p notebook) (with-current-buffer notebook (set-buffer-modified-p nil)) (kill-buffer notebook)))))

(ert-deftest ipynb-output-routing-and-display-update ()
  (ipynb-test-with-notebook
   (setq ipynb--requests (make-hash-table :test #'equal)
         ipynb--displays (make-hash-table :test #'equal))
   (let ((cell (car ipynb--cells)))
     (puthash "request" cell ipynb--requests)
     (ipynb--event (ipynb--parse "{\"event\":\"message\",\"id\":\"request\",\"type\":\"execute_input\",\"content\":{\"execution_count\":7}}"))
     (ipynb--event (ipynb--parse "{\"event\":\"message\",\"id\":\"request\",\"type\":\"display_data\",\"content\":{\"data\":{\"text/plain\":\"old\"},\"metadata\":{},\"transient\":{\"display_id\":\"d1\"}}}"))
     (ipynb--event (ipynb--parse "{\"event\":\"message\",\"type\":\"update_display_data\",\"content\":{\"data\":{\"text/plain\":\"new\"},\"metadata\":{},\"transient\":{\"display_id\":\"d1\"}}}"))
     (should (equal "new" (gethash "text/plain" (gethash "data" (aref (gethash "outputs" cell) 0)))))
     (should-not (gethash "transient" (aref (gethash "outputs" cell) 0)))
     (ipynb--event (ipynb--parse "{\"event\":\"message\",\"id\":\"request\",\"type\":\"clear_output\",\"content\":{\"wait\":true}}"))
     (should (= 1 (length (gethash "outputs" cell))))
     (ipynb--event (ipynb--parse "{\"event\":\"message\",\"id\":\"request\",\"type\":\"stream\",\"content\":{\"name\":\"stdout\",\"text\":\"hello\"}}"))
     (should (= 1 (length (gethash "outputs" cell))))
     (should (equal "hello" (gethash "text" (aref (gethash "outputs" cell) 0)))))))

(ert-deftest ipynb-invalid-file-keeps-original-text ()
  (with-temp-buffer
    (insert "{bad json")
    (should-error (ipynb-mode))
    (should (equal "{bad json" (buffer-string)))
    (should-not buffer-read-only)))

(ert-deftest ipynb-empty-notebook-and-fold ()
  (ipynb-test-with-notebook
   (ipynb-toggle-fold) (should ipynb--folded)
   (ipynb-toggle-fold) (should-not ipynb--folded)
   (ipynb-delete-cell) (should-not ipynb--cells)
   (ipynb-insert-cell-above) (should (= 1 (length ipynb--cells)))))

(ert-deftest ipynb-revert-reads-json ()
  (let ((file (make-temp-file "ipynb-revert" nil ".ipynb")))
    (unwind-protect
        (ipynb-test-with-notebook
         (setq buffer-file-name file)
         (ipynb--write)
         (puthash "source" "unsaved" (car ipynb--cells))
         (ipynb--revert t t)
         (should (equal "" (gethash "source" (car ipynb--cells)))))
      (delete-file file))))

(ert-deftest ipynb-render-preserves-cursor-inside-cell ()
  (ipynb-test-with-notebook
   (puthash "source" "abc\ndef\n" (car ipynb--cells))
   (ipynb--render 0)
   (forward-line 1) (forward-char 2)
   (let ((position (point)))
     (ipynb--render)
     (should (= position (point))))))
