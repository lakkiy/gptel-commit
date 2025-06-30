;;; gptel-commit.el --- Generate commit message with gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Liu Bo

;; Author: Liu Bo
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))
;; Keywords: vc, convenience
;; URL: https://github.com/lakkiy/gptel-commit

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides functions to generate Git commit messages using GPTel.
;; It analyzes staged changes and generates appropriate commit messages following
;; conventional Git commit formats.
;;
;; Main functions:
;; - `gptel-commit': Generate commit message directly
;; - `gptel-commit-rationale': Generate with optional context/rationale
;;
;; The package supports streaming responses and excludes specified file patterns
;; from diff analysis to focus on relevant changes.

;;; Code:

(require 'gptel)

(defgroup gptel-commit nil
  "Generate commit messages with GPTel."
  :group 'vc
  :group 'gptel)

(defcustom gptel-commit-stream t
  "Whether to stream commit message generation.
Set to nil if your backend doesn't support streaming."
  :type 'boolean
  :group 'gptel-commit)

(defvar gptel-commit-prompt
  "You are an expert at writing Git commit messages.
Generate **only** the commit message, nothing else.

DECISION PROCESS:
1. Count changed files
2. If 1 file: check if change is simple or complex
3. Apply the appropriate format

FORMAT RULES:

A. Single File + Simple Change (one clear purpose):
   * path/to/file: Description. (≤72 chars)

   NO subject line, NO blank lines, JUST this one line.

B. Single File + Complex Change (multiple purposes/major refactor):
   Subject line (≤50 chars, imperative mood, NO period)

   Optional body paragraph explaining why (wrap at 72 chars).

   * path/to/file (func1, func2): Description.

C. Multiple Files (2+ files changed):
   Subject line (≤50 chars, imperative mood, NO period)

   Optional body paragraph explaining why (wrap at 72 chars).

   * path/to/file1 (func1): Description.
   * path/to/file2 (func2): Another description.

D. Trivial Changes:
   Add `; ` prefix for typos/comments/docs.
   Example: `; * file: Fix typo.`

SIMPLE vs COMPLEX (single file):
- Simple: one function, one clear fix/addition
- Complex: multiple functions, refactoring, or architectural change"
  "A prompt adapted from Emacs.")

(defvar gptel-commit-after-insert-hook nil
  "Hook run when gptel insert commit message.")

(defvar gptel-commit-backend gptel-backend
  "The backend used for generating commit messages with `gptel-commit`.
This can be set to a lightweight or free model (e.g., via OpenRouter),
so it won't interfere with your default `gptel` usage for general chat.")

(defvar gptel-commit-diff-excludes
  '("pnpm-lock.yaml"
    "*.lock"
    "ent/**/*.go")
  "List of file globs to exclude from commit diff analysis.")

(defvar gptel-commit--current-buffer nil
  "Buffer where commit message is being generated.")

(defvar gptel-commit-rationale-buffer "*GPTel Commit Rationale*"
  "Buffer name for entering rationale for commit message generation.")

(defvar gptel-commit--insert-position nil
  "Position where commit message should be inserted.")

(defun gptel-commit--wildcard-to-regexp (glob)
  "Convert shell glob GLOB to a regular expression."
  (let ((glob (replace-regexp-in-string "\\.\\*" ".*" glob)))
    (wildcard-to-regexp glob)))

(defun gptel-commit--excluded-file-p (filename)
  "Check if FILENAME matches any pattern in `gptel-commit-diff-excludes`."
  (cl-some (lambda (pat)
             (string-match-p (gptel-commit--wildcard-to-regexp pat) filename))
           gptel-commit-diff-excludes))

(defun gptel-commit--filtered-diff ()
  "Return a filtered diff string of staged changes, excluding patterns."
  (let* ((files (split-string
                 (shell-command-to-string "git diff --name-only --cached")
                 "\n" t))
         (included-files (cl-remove-if #'gptel-commit--excluded-file-p files))
         (diffs '()))
    (dolist (file included-files)
      (let ((diff (shell-command-to-string (format "git diff --cached -- %s" file))))
        (when (not (string-empty-p diff))
          (push (format "===== %s =====\n%s" file diff) diffs))))
    (string-join (nreverse diffs) "\n\n")))

(defun gptel-commit--find-commit-buffer ()
  "Find the appropriate buffer for commit message."
  (or (get-buffer "COMMIT_EDITMSG")
      (and (derived-mode-p 'text-mode 'git-commit-mode) (current-buffer))
      (user-error "No commit message buffer found")))

(defun gptel-commit--setup-request-args ()
  "Setup request arguments based on gptel-commit configuration."
  (list :callback #'gptel-commit--handle-response))

(defun gptel-commit--handle-response (response _info)
  "Handle the response from gptel.
RESPONSE is the generated commit message or chunk.
INFO is a plist with additional information."
  (when-let* ((buffer gptel-commit--current-buffer))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (cond
         ((and gptel-commit-stream (stringp response))
          (save-excursion
            (goto-char gptel-commit--insert-position)
            (insert response)
            (setq gptel-commit--insert-position (point))))
         ((and (not gptel-commit-stream) (stringp response))
          (goto-char (point-min))
          (insert response))
         ((and gptel-commit-stream (not (stringp response)))
          (run-hooks 'gptel-commit-after-insert-hook))))))
  (when (and (not gptel-commit-stream) (stringp response))
    (run-hooks 'gptel-commit-after-insert-hook)))

(defun gptel-commit--generate-message (rationale)
  "Generate a commit message based on staged changes and optional RATIONALE."
  (let* ((changes (gptel-commit--filtered-diff))
         (prompt (if (and rationale (not (string-empty-p rationale)))
                     (format "Context: %s\n\nChanges:\n%s" rationale changes)
                   changes))
         (gptel-backend gptel-commit-backend)
         (buffer (gptel-commit--find-commit-buffer)))
    (setq gptel-commit--current-buffer buffer)
    (with-current-buffer buffer
      (setq gptel-commit--insert-position (point))
      (gptel-request prompt
        :system gptel-commit-prompt
        :stream gptel-commit-stream
        :callback #'gptel-commit--handle-response))))

(define-derived-mode gptel-commit-rationale-mode text-mode "GPTel-Commit-Rationale"
  "Mode for entering commit rationale before GPTel generates commit message."
  (local-set-key (kbd "C-c C-c") #'gptel-commit--submit-rationale)
  (local-set-key (kbd "C-c C-k") #'gptel-commit--cancel-rationale))

(defun gptel-commit--setup-rationale-buffer ()
  "Setup the rationale buffer with proper guidance."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert ";;; WHY are you making these changes? (optional)\n")
    (insert ";;; Press C-c C-c to generate commit message, C-c C-k to cancel\n")
    (insert ";;; Leave empty to generate without rationale\n")
    (insert ";;; ────────────────────────────────────────────────────────\n")
    (add-text-properties (point-min) (point)
                         '(face font-lock-comment-face read-only t))
    (insert "\n")
    (goto-char (point-max))))

(defun gptel-commit--submit-rationale ()
  "Submit the rationale buffer content and proceed with GPTel commit generation."
  (interactive)
  (let ((rationale (string-trim
                    (buffer-substring-no-properties
                     (save-excursion
                       (goto-char (point-min))
                       (while (and (not (eobp))
                                   (get-text-property (point) 'read-only))
                         (forward-char))
                       (point))
                     (point-max)))))
    (quit-window t)
    (gptel-commit--generate-message rationale)))

(defun gptel-commit--cancel-rationale ()
  "Cancel rationale input and abort GPTel commit generation."
  (interactive)
  (quit-window t)
  (message "GPTel commit generation canceled."))

;;;###autoload
(defun gptel-commit ()
  "Generate commit message with gptel."
  (interactive)
  (gptel-commit--generate-message nil))

;;;###autoload
(defun gptel-commit-rationale ()
  "Prompt user for rationale and generate commit message with GPTel."
  (interactive)
  (when (or (get-buffer "COMMIT_EDITMSG")
            (derived-mode-p 'text-mode 'git-commit-mode))
    (setq gptel-commit--current-buffer (current-buffer)))
  (let ((buffer (get-buffer-create gptel-commit-rationale-buffer)))
    (with-current-buffer buffer
      (gptel-commit-rationale-mode)
      (gptel-commit--setup-rationale-buffer))
    (pop-to-buffer buffer)))

(provide 'gptel-commit)

;;; gptel-commit.el ends here
