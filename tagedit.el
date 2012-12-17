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

;; A collection of paredit-like functions for editing in html-mode.

;; ## Functions
;;
;; This is it at the moment:
;;
;;  - `tagedit-forward-slurp-tag` moves the next sibling into this tag.
;;  - `tagedit-forward-barf-tag` moves the last child out of this tag.
;;  - `tagedit-kill-attribute` kills the html attribute at point.

;; ## Setup
;;
;; I won't presume to know which keys you want these functions bound to,
;; so you'll have to set that up for yourself. Here's some example code,
;; which incidentally is what I use:
;;
;; ```cl
;; (define-key html-mode-map (kbd "s-<right>") 'tagedit-forward-slurp-tag)
;; (define-key html-mode-map (kbd "s-<left>") 'tagedit-forward-barf-tag)
;; (define-key html-mode-map (kbd "s-k") 'tagedit-kill-attribute)
;; ```

;; ## Other conveniences
;;
;; It also expands one-line tags into multi-line tags for you, when you
;; press refill-paragraph. Like this:
;;
;; ```html
;; <p>My one very long text inside a tag that I'd like to refill</p>
;; ```
;;
;; then after `M-q`:
;;
;; ```html
;; <p>
;;   My one very long text inside a tag that
;;   I'd like to refill
;; </p>
;; ```
;;
;; You can disable this behavior by setting
;; `tagedit-expand-one-line-tags` to nil.

;; ## Todo
;;
;; Right now the commands only care about tags. Free floating text is
;; ignored. My guess is that you'd like to slurp and barf that stuff too.

;;; Code:

;;;###autoload
(defun tagedit-forward-slurp-tag ()
  (interactive)
  (let* ((current-tag (tagedit--current-tag))
         (next-sibling (tagedit--next-sibling current-tag)))
    (tagedit--move next-sibling (tagedit--inner-end current-tag))))

;;;###autoload
(defun tagedit-forward-barf-tag ()
  (interactive)
  (let* ((current-tag (tagedit--current-tag))
         (last-child (tagedit--last-child current-tag)))
    (tagedit--move last-child (aget current-tag :end))))

;;;###autoload
(defun tagedit-kill-attribute ()
  (interactive)
  (when (and (tagedit--inside-tag)
             (not (looking-at ">")))
    (tagedit--select-attribute)
    (kill-region (region-beginning) (region-end))
    (just-one-space)
    (when (looking-at ">")
      (delete-char -1))))

(defun tagedit--select-attribute ()
  (search-forward "\"")
  (when (nth 3 (syntax-ppss)) ; inside string
    (forward-char -1)
    (forward-sexp 1))
  (set-mark (point))
  (forward-sexp -1)
  (search-backward " ")
  (forward-char 1))

(defun tagedit--inside-tag ()
  (let ((context (save-excursion (tagedit--get-context))))
    (and context
         (> (point) (sgml-tag-start context))
         (< (point) (sgml-tag-end context)))))


(defvar tagedit-expand-one-line-tags t
  "Should tagedit change one-line tags into multi-line tags?
This happens when you press refill-paragraph.")

(defadvice fill-paragraph (before tagedit-maybe-expand-tag activate)
  (tagedit--maybe-expand-tag))

(defun tagedit--maybe-expand-tag ()
  (when (and tagedit-expand-one-line-tags
             (derived-mode-p 'sgml-mode))
    (let ((current-tag (tagedit--current-tag)))
      (when (tagedit--is-one-line-tag current-tag)
        (tagedit--one->multi-line-tag current-tag)))))

(defun tagedit--is-one-line-tag (tag)
  (when tag
    (save-excursion
      (goto-char (aget tag :beg))
      (= (line-number-at-pos)
         (progn
           (goto-char (aget tag :end))
           (line-number-at-pos))))))

(defun tagedit--one->multi-line-tag (tag)
  (save-excursion
    (goto-char (tagedit--inner-end tag))
    (let ((end (point)))
      (insert "\n")
      (goto-char (tagedit--inner-beg tag))
      (insert "\n")
      (indent-region (point) (+ 2 end)))))

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

(defun tagedit--inner-beg (tag)
  (save-excursion
    (goto-char (aget tag :beg))
    (forward-list 1)
    (point)))

(defun tagedit--inner-end (tag)
  (- (aget tag :end)
     (length (aget tag :name))
     3))

(defun tagedit--current-tag ()
  (ignore-errors
    (save-excursion
      (let* ((context (tagedit--get-context))
             (name (sgml-tag-name context))
             (type (if (looking-back "^\\s *") :block :inline))
             (beg (sgml-tag-start context))
             (end (when (sgml-skip-tag-forward 1) (point))))
        `((:name . ,name)
          (:type . ,type)
          (:beg . ,beg)
          (:end . ,end))))))

(defun tagedit--get-context ()
  (let ((context (car (sgml-get-context))))
    (when (and context (string= "close" (sgml-tag-type context)))
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
