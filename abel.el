;;; abel.el --- abbrevs for Elisp

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/abel
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((diminish "0.44"))

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file defines `abel-mode': an add-on on top of `abbrev-mode'
;; for `emacs-lisp-mode'. The abbrevs defined here will not expand in
;; strings or comments.

;;; Code:
(defgroup abel nil
  "Abbrevs for Elisp."
  :group 'abbrev
  :prefix "abel-")

(defvar abel-mode nil
  "Determines if Abel is currently active.")

(defun abel-mode (&optional arg)
  "Minor mode refining `abbrev-mode' for Elisp by adding `abel-abbrevs'."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq abel-mode
        (if (eq arg 'toggle)
            (not abel-mode)
          (> (prefix-numeric-value arg) 0)))
  (if abel-mode
      (progn
        (abbrev-mode 1)
        (diminish 'abbrev-mode "Abel"))
    (diminish-undo 'abbrev-mode))
  (mapc #'abel-define abel-abbrevs))

(defun abel-define (x)
  "Define an abbrev based on X.
X should have the format of the elements of `abel-abbrevs'."
  (let ((name (car x))
        (body (cadr x)))
    (define-abbrev emacs-lisp-mode-abbrev-table name
      abel-mode
      #'abel-expand
      :system t
      :enable-function #'abel-p)
    (when abel-mode
      (puthash name body abel--table))
    (setq abbrevs-changed nil)))

(defvar abel--table (make-hash-table :test #'equal)
  "Can't trust `abbrev-mode' with our precious expansions.
Store them here instead.")

(defun abel-p ()
  "Don't expand in strings, comments and function arguments."
  (or (memq this-command '(expand-abbrev aya-open-line))
      (let ((ppss (syntax-ppss)))
        (unless (or (elt ppss 3) (elt ppss 4))
          (save-match-data
            (and (looking-back "([[:alnum:]]*")
                 (save-excursion
                   (goto-char (match-beginning 0))
                   (and
                    (not (or (looking-back "(lambda *")
                             (looking-back "(defun +\\(?:\\sw\\|\\s_\\)+ +")))
                    (condition-case nil
                        (progn
                          (up-list)
                          (backward-sexp)
                          (not
                           (or
                            (looking-back "(let\\*? *"))))
                      (error t))))))))))

(defun abel-expand ()
  "Expand the abbrev before point."
  (when (= (length (this-command-keys-vector)) 1)
    (insert-char (aref (this-command-keys-vector) 0))
    (undo-boundary)
    (backward-delete-char 1))
  (unless (memq (aref (this-command-keys-vector) 0) '(?- ?+ ?/ ?_))
    (when (looking-at "[])} ]")
      (let ((pt (point)))
        (skip-chars-backward "[[:alnum:]]")
        (let* ((name (buffer-substring-no-properties (point) pt))
               (body (gethash name abel--table)))
          (delete-region (point) pt)
          (insert body))))))

(defcustom abel-abbrevs
  '(
    ;; basics
    ("a" "and")
    ("bp" "boundp")
    ("c" "concat")
    ("fp" "fboundp")
    ("ii" "interactive")
    ("i" "insert")
    ("l" "lambda")
    ("m" "message")
    ("n" "not")
    ("f" "format")
    ("u" "unless")
    ("up" "unwind-protect")
    ("w" "when")
    ("wl" "while")
    ("r" "require")
    ("ci" "call-interactively")
    ("cc" "condition-case")
    ("pg" "plist-get")
    ("sa" "save-excursion")
    ("sr" "save-restriction")
    ("smd" "save-match-data")
    ;; defines
    ("de" "declare-function")
    ("df" "defface")
    ("da" "defmacro")
    ("du" "defcustom")
    ("dv" "defvar")
    ;; everything with char
    ("bc" "backward-char")
    ("scb" "skip-chars-backward")
    ("scf" "skip-chars-forward")
    ("gc" "goto-char")
    ("fc" "forward-char")
    ("dc" "delete-char")
    ("ca" "char-after")
    ;; everything with region
    ("ra" "region-active-p")
    ("rb" "region-beginning")
    ("re" "region-end")
    ("ntr" "narrow-to-region")
    ("dr" "delete-region")
    ("ir" "indent-region")
    ;; error related
    ("ie" "ignore-errors")
    ("e" "error")
    ;; regex match related
    ("la" "looking-at")
    ("lb" "looking-back")
    ("mb" "match-beginning")
    ("me" "match-end")
    ("ms" "match-string")
    ("msn" "match-string-no-properties")
    ("rm" "replace-match")
    ("ro" "regexp-opt")
    ("rq" "regexp-quote")
    ("rr" "replace-regexp-in-string")
    ("rsb" "re-search-backward")
    ("rsf" "re-search-forward")
    ("sf" "search-forward")
    ("sm" "string-match")
    ;; words
    ("fw" "forward-word")
    ("bw" "backward-word")
    ;; lines
    ("eol" "end-of-line")
    ("fl" "forward-line")
    ("lbp" "line-beginning-position")
    ("lep" "line-end-position")
    ("nai" "newline-and-indent")
    ;; buffer
    ("bfn" "buffer-file-name")
    ("bn" "buffer-name")
    ("bs" "buffer-substring")
    ("bsn" "buffer-substring-no-properties")
    ("cb" "current-buffer")
    ("wcb" "with-current-buffer")
    ("wtb" "with-temp-buffer")
    ("efn" "expand-file-name")
    ("ff" "find-file")
    ("ffn" "find-file-noselect")
    ;; window
    ("ow" "other-window")
    ("sw" "selected-window")
    ;; string
    ("ssn" "substring-no-properties")
    ("ss" "substring")
    ("si" "split-string")
    ("se" "string=")
    ("sl" "string<")
    ("sp" "stringp")
    ;; point
    ("pi" "point-min")
    ("pa" "point-max")
    ("p" "point")
    ;; key
    ("gk" "global-set-key")
    ("dk" "define-key")
    ;; rest
    ("ah" "add-hook")
    ("atl" "add-to-list")
    ("bod" "beginning-of-defun")
    ("bol" "beginning-of-line")
    ("dm" "deactivate-mark")
    ("fs" "forward-sexp")
    ("jos" "just-one-space")
    ("kn" "kill-new")
    ("lp" "load-path")
    ("mm" "major-mode")
    ("sic" "self-insert-command")
    ("sn" "symbol-name")
    ("tap" "thing-at-point")
    ("tc" "this-command")
    ("ul" "up-list"))
  "List of (ABBREV EXPANSION) used by `abel'."
  :set (lambda (symbol value)
         "Update abbrevs accoring to `abel-abbrevs'."
         (set symbol value)
         (mapc #'abel-define value))
  :group 'abel)

(provide 'abel)

;;; abel.el ends here
