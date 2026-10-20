;;; ipynb-lsp.el --- Cross-cell language server support -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; One virtual document per notebook; cell-local UI uses UTF-16 mappings.
;;; Code:
(require 'ipynb)
(require 'jsonrpc)
(require 'xref)
(require 'flymake)
(require 'url-util)
(require 'project)

(defcustom ipynb-lsp-server-command nil
  "Language server command, or nil to discover a server for this notebook.
Set buffer-locally for notebooks that use different environments."
  :type '(repeat string) :group 'ipynb)
(defcustom ipynb-lsp-servers
  '(("python" ("basedpyright-langserver" "--stdio") ("pyright-langserver" "--stdio") ("pylsp"))
    ("r" ("R" "--slave" "-e" "languageserver::run()"))
    ("julia" ("julia" "--startup-file=no" "--history-file=no" "-e" "using LanguageServer; runserver()"))
    ("javascript" ("typescript-language-server" "--stdio"))
    ("typescript" ("typescript-language-server" "--stdio")))
  "Candidate server commands by notebook language, tried in order."
  :type '(alist :key-type string :value-type (repeat (repeat string))) :group 'ipynb)
(defcustom ipynb-lsp-settings nil
  "LSP workspace settings as a plist, e.g. (:python (:pythonPath \"...\"))."
  :type 'sexp :group 'ipynb)
(defcustom ipynb-lsp-idle-delay 0.3 "Delay before syncing cell edits to LSP."
  :type 'number :group 'ipynb)
(defvar-local ipynb-lsp--connection nil)
(defvar-local ipynb-lsp--uri nil)
(defvar-local ipynb-lsp--text "")
(defvar-local ipynb-lsp--segments nil)
(defvar-local ipynb-lsp--version 0)
(defvar-local ipynb-lsp--capabilities nil)
(defvar-local ipynb-lsp--timer nil)
(defvar-local ipynb-lsp--diagnostics nil)
(defvar-local ipynb-lsp--report nil)
(defvar ipynb-lsp--inhibit nil)

(defun ipynb-lsp--parent ()
  (if ipynb-cell-edit-mode
      (if (buffer-live-p ipynb--parent) ipynb--parent (user-error "Notebook closed"))
    (if (derived-mode-p 'ipynb-mode) (current-buffer) (user-error "Not a notebook or cell"))))
(defun ipynb-lsp--live-p ()
  (and ipynb-lsp--connection (jsonrpc-running-p ipynb-lsp--connection)))
(defun ipynb-lsp--editor (cell)
  (cl-find-if (lambda (buffer) (and (buffer-live-p buffer)
                                   (eq (buffer-local-value 'ipynb--cell buffer) cell))) ipynb--editors))
(defun ipynb-lsp--source (cell)
  (let ((editor (ipynb-lsp--editor cell)))
    (if editor (with-current-buffer editor (buffer-substring-no-properties (point-min) (point-max)))
      (ipynb--text (gethash "source" cell)))))
(defun ipynb-lsp--build ()
  "Return (TEXT . SEGMENTS), incorporating unsaved cell editors.
Each segment is (CELL START END SOURCE), with zero-based character offsets."
  (let ((offset 0) chunks segments)
    (dolist (cell ipynb--cells)
      (when (equal (gethash "cell_type" cell) "code")
        (let ((source (ipynb-lsp--source cell)))
          (push (list cell offset (+ offset (length source)) source) segments)
          (push source chunks) (push "\n\n" chunks)
          (setq offset (+ offset (length source) 2)))))
    (cons (apply #'concat (nreverse chunks)) (nreverse segments))))
(defun ipynb-lsp--position (text offset)
  "Convert character OFFSET in TEXT to a UTF-16 LSP position."
  (unless (<= 0 offset (length text)) (error "Invalid source offset"))
  (let ((line 0) (start 0) (pos 0))
    (while (and (< pos offset) (string-match "\n" text pos) (< (match-beginning 0) offset))
      (setq line (1+ line) start (match-end 0) pos start))
    (list :line line :character (cl-loop for char across (substring text start offset) sum (if (> char #xffff) 2 1)))))
(defun ipynb-lsp--offset (text position)
  "Convert UTF-16 POSITION in TEXT to a character offset, rejecting invalid ranges."
  (let ((line (plist-get position :line)) (units (plist-get position :character)) (offset 0))
    (unless (and (natnump line) (natnump units)) (error "Invalid LSP position"))
    (dotimes (_ line)
      (unless (string-match "\n" text offset) (error "LSP line outside document"))
      (setq offset (match-end 0)))
    (while (> units 0)
      (when (or (>= offset (length text)) (= (aref text offset) ?\n))
        (error "LSP column outside line"))
      (setq units (- units (if (> (aref text offset) #xffff) 2 1)) offset (1+ offset)))
    (unless (= units 0) (error "LSP position splits a surrogate pair"))
    offset))
(defun ipynb-lsp--segment-at (offset)
  (cl-find-if (lambda (seg) (<= (nth 1 seg) offset (nth 2 seg))) ipynb-lsp--segments))
(defun ipynb-lsp--file-uri (file)
  (concat "file://" (mapconcat #'url-hexify-string (split-string (expand-file-name file) "/") "/")))
(defun ipynb-lsp--extension ()
  (or (cdr (assoc (downcase (ipynb--language)) '(("python" . "py") ("julia" . "jl") ("r" . "R")
                                              ("javascript" . "js") ("typescript" . "ts") ("bash" . "sh")))) "txt"))
(defun ipynb-lsp--schedule (&rest _)
  (unless ipynb-lsp--inhibit
    (let ((parent (if ipynb-cell-edit-mode ipynb--parent (current-buffer))))
      (when (buffer-live-p parent)
       (with-current-buffer parent
        (when (timerp ipynb-lsp--timer) (cancel-timer ipynb-lsp--timer))
        (when (ipynb-lsp--live-p)
          (setq ipynb-lsp--timer
                (run-at-time ipynb-lsp-idle-delay nil
                             (lambda () (when (buffer-live-p parent)
                                          (with-current-buffer parent
                                            (when (ipynb-lsp--live-p) (ipynb-lsp--sync)))))))))))))
(defun ipynb-lsp--sync ()
  (let ((built (ipynb-lsp--build)))
    (setq ipynb-lsp--segments (cdr built))
    (unless (equal (car built) ipynb-lsp--text)
      (setq ipynb-lsp--text (car built) ipynb-lsp--version (1+ ipynb-lsp--version)
            ipynb-lsp--diagnostics nil)
      (when (ipynb-lsp--live-p)
        (jsonrpc-notify ipynb-lsp--connection :textDocument/didChange
                        (list :textDocument (list :uri ipynb-lsp--uri :version ipynb-lsp--version)
                              :contentChanges (vector (list :text ipynb-lsp--text)))))
      (ipynb-lsp--report-diagnostics))))
(defun ipynb-lsp--notification (_connection method params)
  (when (and (eq method 'textDocument/publishDiagnostics)
             (equal (plist-get params :uri) ipynb-lsp--uri)
             (or (null (plist-get params :version)) (= (plist-get params :version) ipynb-lsp--version))
             (equal (car (ipynb-lsp--build)) ipynb-lsp--text))
    (setq ipynb-lsp--diagnostics (append (plist-get params :diagnostics) nil))
    (ipynb-lsp--report-diagnostics)))
(defun ipynb-lsp--server-request (_connection method params)
  (pcase method
    ('workspace/configuration
     (vconcat (mapcar (lambda (item)
                       (let ((settings ipynb-lsp-settings))
                         (dolist (key (split-string (or (plist-get item :section) "") "\\." t))
                           (setq settings (plist-get settings (intern (concat ":" key)))))
                         settings)) (append (plist-get params :items) nil))))
    ('workspace/workspaceFolders
     (vector (list :uri (ipynb-lsp--file-uri default-directory) :name (file-name-nondirectory (directory-file-name default-directory)))))
    ;; Edits only apply through a user command that owns a source snapshot.
    ('workspace/applyEdit (list :applied :json-false :failureReason "Use ipynb-lsp-rename to apply notebook edits"))
    (_ nil)))

;;;###autoload
(defun ipynb-lsp-start (&optional command)
  "Start a shared language server for all code cells.
With prefix, read COMMAND as a shell-style list (no shell is invoked)."
  (interactive (list (when current-prefix-arg (split-string-and-unquote (read-string "Server command: ")))))
  (with-current-buffer (ipynb-lsp--parent)
    (when (file-remote-p default-directory) (user-error "LSP requires a local notebook"))
    (when (ipynb-lsp--live-p) (user-error "Language server already running"))
    (let* ((parent (current-buffer))
           (command (or command ipynb-lsp-server-command
                        (cl-find-if (lambda (cmd) (executable-find (car cmd)))
                                    (cdr (assoc (downcase (ipynb--language)) ipynb-lsp-servers)))))
           (project (project-current nil))
           (root (if project (project-root project) default-directory))
           (name (concat "ipynb-lsp-" (ipynb--id)))
           (built (ipynb-lsp--build)))
      (unless command (user-error "Install a language server or set ipynb-lsp-server-command"))
      (setq ipynb-lsp--uri (ipynb-lsp--file-uri
                            (expand-file-name (format ".%s-%s.%s" (file-name-nondirectory (or buffer-file-name "notebook.ipynb"))
                                                      (ipynb--id) (ipynb-lsp--extension)) default-directory))
            ipynb-lsp--text (car built) ipynb-lsp--segments (cdr built) ipynb-lsp--version 1)
      (condition-case err
          (progn
            (setq ipynb-lsp--connection
                  (make-instance 'jsonrpc-process-connection :name name
                                 :process (lambda () (make-process :name name :command command :connection-type 'pipe
                                                                    :coding 'utf-8-unix :noquery t
                                                                    :stderr (get-buffer-create (format "*%s stderr*" name))))
                                 :notification-dispatcher
                                 (lambda (conn method params) (when (buffer-live-p parent)
                                                               (with-current-buffer parent (ipynb-lsp--notification conn method params))))
                                 :request-dispatcher
                                 (lambda (conn method params) (when (buffer-live-p parent)
                                                               (with-current-buffer parent (ipynb-lsp--server-request conn method params))))))
            (let ((result (jsonrpc-request
                           ipynb-lsp--connection :initialize
                           (list :processId (emacs-pid) :rootUri (ipynb-lsp--file-uri root)
                                 :capabilities
                                 '(:general (:positionEncodings ["utf-16"])
                                   :workspace (:configuration t :workspaceFolders t)
                                   :textDocument (:synchronization (:dynamicRegistration :json-false)
                                                  :completion (:completionItem (:snippetSupport :json-false))
                                                  :hover (:contentFormat ["plaintext" "markdown"])
                                                  :publishDiagnostics (:versionSupport t)))
                                 :workspaceFolders (vector (list :uri (ipynb-lsp--file-uri root) :name (file-name-nondirectory (directory-file-name root)))))
                           :timeout 30)))
              (setq ipynb-lsp--capabilities (plist-get result :capabilities))
              (unless (member (plist-get ipynb-lsp--capabilities :positionEncoding) '(nil "utf-16"))
                (user-error "Server did not negotiate UTF-16 positions")))
            (jsonrpc-notify ipynb-lsp--connection :initialized (make-hash-table))
            (jsonrpc-notify ipynb-lsp--connection :workspace/didChangeConfiguration (list :settings (or ipynb-lsp-settings (make-hash-table))))
            (jsonrpc-notify ipynb-lsp--connection :textDocument/didOpen
                            (list :textDocument (list :uri ipynb-lsp--uri :languageId (downcase (ipynb--language))
                                                     :version 1 :text ipynb-lsp--text)))
            (add-hook 'ipynb-after-change-hook #'ipynb-lsp--schedule nil t)
            (add-hook 'kill-buffer-hook #'ipynb-lsp-stop nil t)
            (dolist (editor ipynb--editors)
              (when (buffer-live-p editor) (with-current-buffer editor (ipynb-lsp--setup-editor))))
            (message "Notebook LSP: %s" (car command)))
        (error (ipynb-lsp-stop) (signal (car err) (cdr err)))))))

;;;###autoload
(defun ipynb-lsp-stop ()
  "Stop the notebook language server and remove its diagnostics."
  (interactive)
  (with-current-buffer (ipynb-lsp--parent)
    (when (timerp ipynb-lsp--timer) (cancel-timer ipynb-lsp--timer))
    (when (ipynb-lsp--live-p)
      (ignore-errors
        (jsonrpc-notify ipynb-lsp--connection :textDocument/didClose (list :textDocument (list :uri ipynb-lsp--uri)))
        (jsonrpc-request ipynb-lsp--connection :shutdown nil :timeout 1)
        (jsonrpc-notify ipynb-lsp--connection :exit nil))
      (let ((deadline (+ (float-time) 2)))
        (while (and (ipynb-lsp--live-p) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (jsonrpc-shutdown ipynb-lsp--connection))
    (setq ipynb-lsp--connection nil ipynb-lsp--diagnostics nil)
    (ipynb-lsp--report-diagnostics)))

(defun ipynb-lsp--context ()
  (let* ((parent (ipynb-lsp--parent))
         (cell (if ipynb-cell-edit-mode ipynb--cell (ipynb--current-cell)))
         (offset (if ipynb-cell-edit-mode (1- (point))
                   (max 0 (- (point) (save-excursion (ipynb--goto (ipynb--current-index)) (forward-line 1) (point)))))))
    (with-current-buffer parent
      (unless (ipynb-lsp--live-p) (user-error "Start LSP with M-x ipynb-lsp-start"))
      (ipynb-lsp--sync)
      (let ((seg (assq cell ipynb-lsp--segments)))
        (unless seg (user-error "LSP is available in code cells"))
        (list parent cell (min offset (length (nth 3 seg)))
              (list :textDocument (list :uri ipynb-lsp--uri)
                    :position (ipynb-lsp--position ipynb-lsp--text (+ (nth 1 seg) (min offset (length (nth 3 seg)))))))))))
(defun ipynb-lsp--request (method &optional extra)
  (let ((context (ipynb-lsp--context)))
    (with-current-buffer (car context)
      (let* ((snapshot ipynb-lsp--text)
             (result (jsonrpc-request ipynb-lsp--connection method (append (nth 3 context) extra) :timeout 10)))
        (unless (equal snapshot (car (ipynb-lsp--build))) (user-error "Notebook changed during LSP request; retry"))
        result))))

(defun ipynb-lsp--plan-edits (edits text segments)
  "Validate EDITS against TEXT and SEGMENTS, returning new sources per cell."
  (let ((groups (make-hash-table :test #'eq)) result)
    (mapc (lambda (edit)
            (let* ((range (plist-get edit :range))
                   (start (ipynb-lsp--offset text (plist-get range :start)))
                   (end (ipynb-lsp--offset text (plist-get range :end)))
                   (seg (cl-find-if (lambda (s) (<= (nth 1 s) start end (nth 2 s))) segments)))
              (unless (and seg (stringp (plist-get edit :newText)))
                (user-error "LSP edit crosses a cell boundary; no changes applied"))
              (push (list (- start (nth 1 seg)) (- end (nth 1 seg)) (plist-get edit :newText)) (gethash (car seg) groups)))) edits)
    (maphash (lambda (cell changes)
               (let* ((source (nth 3 (assq cell segments))) (previous (1+ (length source))))
                 (dolist (change (sort changes (lambda (a b) (> (car a) (car b)))))
                   (when (> (nth 1 change) previous) (user-error "Overlapping LSP edits; no changes applied"))
                   (setq source (concat (substring source 0 (car change)) (nth 2 change) (substring source (nth 1 change)))
                         previous (car change)))
                 (push (cons cell source) result))) groups)
    result))
(defun ipynb-lsp--commit-plan (plan)
  "Commit validated PLAN as one notebook undo operation, updating open editors."
  (when plan
    (let ((ipynb-lsp--inhibit t))
      (ipynb--snapshot)
      (dolist (change plan)
        (puthash "source" (cdr change) (car change))
        (let ((editor (ipynb-lsp--editor (car change))))
          (when editor
            (with-current-buffer editor
              (let ((position (point)))
                (atomic-change-group (erase-buffer) (insert (cdr change)))
                (goto-char (min position (point-max))))
              (setq ipynb--original-source (cdr change))
              (set-buffer-modified-p nil)))))
      (ipynb--changed))
    (ipynb-lsp--sync)))
(defun ipynb-lsp--workspace-edits (workspace)
  (let (edits)
    (let ((changes (plist-get workspace :changes)))
      (while changes
        (let ((uri (pop changes)) (value (pop changes)))
          (unless (equal (if (keywordp uri) (substring (symbol-name uri) 1) uri) ipynb-lsp--uri)
            (user-error "Rename also changes another file; no changes applied"))
          (setq edits (append edits (append value nil))))))
    (mapc (lambda (change)
            (let ((doc (plist-get change :textDocument)))
              (unless (and doc (equal (plist-get doc :uri) ipynb-lsp--uri)
                           (or (null (plist-get doc :version)) (eq (plist-get doc :version) :null)
                               (= (plist-get doc :version) ipynb-lsp--version)))
                (user-error "Unsupported or outdated workspace edit; no changes applied"))
              (setq edits (append edits (append (plist-get change :edits) nil)))))
          (plist-get workspace :documentChanges))
    edits))
;;;###autoload
(defun ipynb-lsp-rename (name)
  "Rename the symbol at point to NAME across notebook code cells."
  (interactive (list (read-string "Rename to: " (thing-at-point 'symbol t))))
  (let ((result (ipynb-lsp--request :textDocument/rename (list :newName name))))
    (with-current-buffer (ipynb-lsp--parent)
      (ipynb-lsp--commit-plan (ipynb-lsp--plan-edits (ipynb-lsp--workspace-edits result) ipynb-lsp--text ipynb-lsp--segments)))))

(defun ipynb-lsp--markup (contents)
  (cond ((stringp contents) contents)
        ((vectorp contents) (mapconcat #'ipynb-lsp--markup contents "\n\n"))
        ((listp contents) (or (plist-get contents :value) "")) (t "")))
;;;###autoload
(defun ipynb-lsp-hover () "Show language-server documentation at point." (interactive)
  (let ((text (ipynb-lsp--markup (plist-get (ipynb-lsp--request :textDocument/hover) :contents))))
    (with-help-window "*ipynb LSP hover*" (princ (if (string-empty-p text) "No documentation" text)))))
(defun ipynb-lsp-eldoc (callback &rest _)
  (when (and ipynb-cell-edit-mode (buffer-live-p ipynb--parent)
             (with-current-buffer ipynb--parent (ipynb-lsp--live-p)))
    (let* ((context (ipynb-lsp--context)) (editor (current-buffer)) (tick (buffer-chars-modified-tick)))
      (with-current-buffer (car context)
        (jsonrpc-async-request ipynb-lsp--connection :textDocument/hover (nth 3 context) :timeout 3
                              :success-fn (lambda (result)
                                            (when (and (buffer-live-p editor) (= tick (buffer-chars-modified-tick editor)))
                                              (funcall callback (ipynb-lsp--markup (plist-get result :contents))))))))
    t))

(defun ipynb-lsp-completion-at-point ()
  "Complete against all notebook code, without requiring kernel execution."
  (when (and ipynb-cell-edit-mode (buffer-live-p ipynb--parent)
             (with-current-buffer ipynb--parent (ipynb-lsp--live-p)))
    (let* ((context (ipynb-lsp--context)) (parent (car context)) (cell (nth 1 context))
           (bounds (or (bounds-of-thing-at-point 'symbol) (cons (point) (point))))
           (result (ipynb-lsp--request :textDocument/completion))
           (items (if (vectorp result) (append result nil) (append (plist-get result :items) nil)))
           (snapshot (buffer-local-value 'ipynb-lsp--text parent))
           (segments (buffer-local-value 'ipynb-lsp--segments parent))
           (seg (assq cell segments)) start end entries)
      (dolist (item items)
        (let* ((edit (plist-get item :textEdit))
               (range (or (plist-get edit :range) (plist-get edit :replace)))
               (a (if range (1+ (- (ipynb-lsp--offset snapshot (plist-get range :start)) (nth 1 seg))) (car bounds)))
               (b (if range (1+ (- (ipynb-lsp--offset snapshot (plist-get range :end)) (nth 1 seg))) (cdr bounds)))
               (text (or (plist-get edit :newText) (plist-get item :insertText) (plist-get item :label))))
          (when (and (stringp text) (<= 1 a b (point-max))
                     (/= (or (plist-get item :insertTextFormat) 1) 2)
                     (or (null start) (and (= a start) (= b end))))
            (setq start a end b)
            (push (cons text item) entries))))
      (when entries
        (setq entries (nreverse entries))
        (list start end (mapcar #'car entries) :exclusive 'no
              :annotation-function (lambda (candidate) (let ((detail (plist-get (cdr (assoc candidate entries)) :detail)))
                                                         (if detail (concat " " detail) "")))
              :exit-function
              (lambda (candidate status)
                (when (eq status 'finished)
                  (with-current-buffer parent
                    (condition-case err
                        (let* ((item (cdr (assoc candidate entries)))
                               (item (if (and (plist-get item :data)
                                              (plist-get (plist-get ipynb-lsp--capabilities :completionProvider) :resolveProvider))
                                         (jsonrpc-request ipynb-lsp--connection :completionItem/resolve item :timeout 3) item))
                               (extra (plist-get item :additionalTextEdits)))
                          (when extra
                            (let* ((primary (list :range (list :start (ipynb-lsp--position snapshot (+ (nth 1 seg) (1- start)))
                                                              :end (ipynb-lsp--position snapshot (+ (nth 1 seg) (1- end))))
                                                  :newText candidate))
                                   (expected (ipynb-lsp--plan-edits (list primary) snapshot segments)))
                              (unless (cl-every (lambda (s) (equal (ipynb-lsp--source (car s))
                                                                  (or (cdr (assq (car s) expected)) (nth 3 s)))) segments)
                                (user-error "Notebook changed during completion"))
                              (ipynb-lsp--commit-plan (ipynb-lsp--plan-edits (cons primary (append extra nil)) snapshot segments)))))
                      (error (message "Completion extra edits not applied: %s" (error-message-string err))))))))))))

(defun ipynb-lsp--ensure-editor (cell)
  (or (ipynb-lsp--editor cell)
      (let ((parent (current-buffer)))
        (save-window-excursion
          (with-current-buffer parent (ipynb--goto (cl-position cell ipynb--cells)) (ipynb-edit-cell)))
        (ipynb-lsp--editor cell))))
(defun ipynb-lsp--xrefs (locations)
  (with-current-buffer (ipynb-lsp--parent)
    (mapcar
     (lambda (location)
       (let* ((uri (or (plist-get location :uri) (plist-get location :targetUri)))
              (range (or (plist-get location :range) (plist-get location :targetSelectionRange)))
              (position (plist-get range :start)))
         (if (equal uri ipynb-lsp--uri)
             (let* ((offset (ipynb-lsp--offset ipynb-lsp--text position))
                    (seg (ipynb-lsp--segment-at offset)))
               (unless seg (user-error "Server location is between cells"))
               (xref-make (format "Cell %d: %s" (1+ (cl-position (car seg) ipynb--cells))
                                  (car (split-string (substring ipynb-lsp--text offset) "\n")))
                          (xref-make-buffer-location (ipynb-lsp--ensure-editor (car seg)) (1+ (- offset (nth 1 seg))))))
           (unless (string-prefix-p "file://" uri) (user-error "Unsupported definition URI: %s" uri))
           (let* ((file (decode-coding-string (url-unhex-string (substring uri 7)) 'utf-8))
                  (buffer (find-file-noselect file)))
             (with-current-buffer buffer
               (xref-make file (xref-make-buffer-location buffer (1+ (ipynb-lsp--offset (buffer-substring-no-properties (point-min) (point-max)) position)))))))))
     (cond ((vectorp locations) (append locations nil)) ((plist-get locations :uri) (list locations)) (t locations)))))
(defun ipynb-lsp-xref-backend ()
  (when (and ipynb-cell-edit-mode (buffer-live-p ipynb--parent)
             (with-current-buffer ipynb--parent (ipynb-lsp--live-p))) 'ipynb-lsp))
(cl-defmethod xref-backend-identifier-at-point ((_backend (eql ipynb-lsp))) (thing-at-point 'symbol t))
(cl-defmethod xref-backend-definitions ((_backend (eql ipynb-lsp)) _identifier)
  (ipynb-lsp--xrefs (ipynb-lsp--request :textDocument/definition)))
(cl-defmethod xref-backend-references ((_backend (eql ipynb-lsp)) _identifier)
  (ipynb-lsp--xrefs (ipynb-lsp--request :textDocument/references '(:context (:includeDeclaration t)))))
;;;###autoload
(defun ipynb-lsp-definition () "Find definition across cells or project files." (interactive)
  (xref-push-marker-stack)
  (let ((items (ipynb-lsp--xrefs (ipynb-lsp--request :textDocument/definition))))
    (unless items (user-error "No definition found"))
    (xref-pop-to-location (car items))))
;;;###autoload
(defun ipynb-lsp-references () "List references across notebook cells." (interactive)
  (let ((items (ipynb-lsp--xrefs (ipynb-lsp--request :textDocument/references '(:context (:includeDeclaration t))))))
    (xref-show-xrefs (lambda () items) nil)))

(defun ipynb-lsp--cell-diagnostics (cell editor)
  (let ((seg (assq cell ipynb-lsp--segments)) result)
    (when seg
      (dolist (diag ipynb-lsp--diagnostics)
        (condition-case nil
            (let* ((range (plist-get diag :range))
                   (start (ipynb-lsp--offset ipynb-lsp--text (plist-get range :start)))
                   (end (ipynb-lsp--offset ipynb-lsp--text (plist-get range :end))))
              (when (<= (nth 1 seg) start (nth 2 seg))
                (push (flymake-make-diagnostic editor (1+ (- start (nth 1 seg)))
                                               (1+ (- (min end (nth 2 seg)) (nth 1 seg)))
                                               (pcase (plist-get diag :severity) (1 :error) (2 :warning) (_ :note))
                                               (plist-get diag :message)) result)))
          (error nil)))) result))
(defun ipynb-lsp--report-diagnostics ()
  (let ((parent (current-buffer)))
    (dolist (editor ipynb--editors)
      (when (buffer-live-p editor)
        (let* ((cell (buffer-local-value 'ipynb--cell editor))
               (diags (ipynb-lsp--cell-diagnostics cell editor)))
          (with-current-buffer editor
            (when (and ipynb-lsp--report (eq ipynb--parent parent)) (funcall ipynb-lsp--report diags))))))))
(defun ipynb-lsp-flymake (report &rest _)
  (setq ipynb-lsp--report report)
  (let ((cell ipynb--cell) (editor (current-buffer)))
    (if (buffer-live-p ipynb--parent)
        (with-current-buffer ipynb--parent
          (funcall report (ipynb-lsp--cell-diagnostics cell editor)))
      (funcall report nil))))
(put 'ipynb-lsp-flymake 'flymake-always-safe t) ; Only reports already received diagnostics.
(defun ipynb-lsp--setup-editor ()
  (when (and ipynb-cell-edit-mode (equal (gethash "cell_type" ipynb--cell) "code"))
    (add-hook 'after-change-functions #'ipynb-lsp--schedule nil t)
    (add-hook 'kill-buffer-hook #'ipynb-lsp--schedule nil t)
    (add-hook 'completion-at-point-functions #'ipynb-lsp-completion-at-point nil t)
    (add-hook 'xref-backend-functions #'ipynb-lsp-xref-backend nil t)
    (add-hook 'eldoc-documentation-functions #'ipynb-lsp-eldoc nil t)
    (setq-local flymake-diagnostic-functions '(ipynb-lsp-flymake))
    (flymake-mode 1) (eldoc-mode 1)))
(add-hook 'ipynb-cell-edit-mode-hook #'ipynb-lsp--setup-editor)

;;;###autoload
(defun ipynb-format-cell (&optional all)
  "Format the current code cell, or ALL code cells with a prefix.
Each cell is formatted independently to preserve notebook boundaries."
  (interactive "P")
  (let* ((context (ipynb-lsp--context)) (current (nth 1 context)))
    (with-current-buffer (car context)
      (unless (plist-get ipynb-lsp--capabilities :documentFormattingProvider)
        (user-error "This language server does not provide document formatting"))
      (let ((snapshot ipynb-lsp--text) plans)
        (dolist (seg (if all ipynb-lsp--segments (list (assq current ipynb-lsp--segments))))
          (let ((uri (concat (substring ipynb-lsp--uri 0 (- (length (ipynb-lsp--extension))))
                             "cell-" (gethash "id" (car seg)) "." (ipynb-lsp--extension)))
                (source (nth 3 seg)))
            (unwind-protect
                (progn
                  (jsonrpc-notify ipynb-lsp--connection :textDocument/didOpen
                                  (list :textDocument (list :uri uri :languageId (downcase (ipynb--language)) :version 1 :text source)))
                  (setq plans (append plans (ipynb-lsp--plan-edits
                                            (jsonrpc-request ipynb-lsp--connection :textDocument/formatting
                                                             (list :textDocument (list :uri uri) :options '(:tabSize 4 :insertSpaces t)) :timeout 15)
                                            source (list (list (car seg) 0 (length source) source))))))
              (jsonrpc-notify ipynb-lsp--connection :textDocument/didClose (list :textDocument (list :uri uri))))))
        (unless (equal snapshot (car (ipynb-lsp--build))) (user-error "Notebook changed during formatting; retry"))
        (ipynb-lsp--commit-plan plans)))))

(provide 'ipynb-lsp)
;;; ipynb-lsp.el ends here
