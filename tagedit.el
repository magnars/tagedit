;;; tagedit.el --- Some paredit-like features for html-mode

;; Copyright (C) 2012 Magnar Sveen <magnars@gmail.com>

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; So far there's only tagedit-forward-slurp-tag and -barf-tag

;;; Code:

;;;###autoload
(defun tagedit-forward-slurp-tag ()
  (interactive)
  (let* ((current-tag (tagedit--current-tag))
         (next-sibling (tagedit--next-sibling current-tag)))
    (tagedit--move next-sibling (tagedit--inside-end current-tag))))

;;;###autoload
(defun tagedit-forward-barf-tag ()
  (interactive)
  (let* ((current-tag (tagedit--current-tag))
         (last-child (tagedit--last-child current-tag)))
    (tagedit--move last-child (aget current-tag :end))))

(defun tagedit--move (tag pos)
  (save-excursion
    (goto-char pos)
    (let ((blank-lines (looking-at "\n\n"))
          (contents (tagedit--contents tag)))
      (save-excursion (tagedit--delete tag))
      (setq beg (point))
      (when (eq :block (aget tag :type))
        (tagedit--just-one-blank-line))
      (when blank-lines (newline))
      (insert contents)
      (when blank-lines (newline))
      (indent-region beg (point)))))

(defun tagedit--just-one-blank-line ()
  (newline 2)
  (forward-line -1)
  (delete-blank-lines))

(defun tagedit--contents (tag)
  (buffer-substring (aget tag :beg)
                    (aget tag :end)))

(defun tagedit--delete (tag)
  (goto-char (aget tag :beg))
  (delete-region (aget tag :beg)
                 (aget tag :end))
  (when (eq :block (aget tag :type))
    (delete-blank-lines)))

(defun tagedit--inside-end (tag)
  (- (aget tag :end)
     (length (aget tag :name))
     3))

(defun tagedit--current-tag ()
  (save-excursion
    (let* ((context (tagedit--get-context))
           (name (sgml-tag-name context))
           (type (if (looking-back "^\\s *") :block :inline))
           (beg (sgml-tag-start context))
           (end (when (sgml-skip-tag-forward 1) (point))))
      `((:name . ,name)
        (:type . ,type)
        (:beg . ,beg)
        (:end . ,end)))))

(defun tagedit--get-context ()
  (let ((context (car (sgml-get-context))))
    (when (string= "close" (sgml-tag-type context))
      (forward-char 1)
      (sgml-skip-tag-backward 1)
      (forward-char 1)
      (setq context (car (sgml-get-context))))
    context))

(defun tagedit--last-child (tag)
  (save-excursion
    (goto-char (aget tag :end))
    (search-backward "</")
    (search-backward ">")
    (tagedit--current-tag)))

(defun tagedit--next-sibling (tag)
  (save-excursion
    (goto-char (aget tag :end))
    (search-forward "<")
    (tagedit--current-tag)))

(provide 'tagedit)

;;; tagedit.el ends here
