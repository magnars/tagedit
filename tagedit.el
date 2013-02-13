;;; tagedit.el --- Some paredit-like features for html-mode

;; Copyright (C) 2012 Magnar Sveen <magnars@gmail.com>

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 1.2.0
;; Keywords: convenience
;; Package-Requires: ((s "1.3.1") (dash "1.0.3"))

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

;; ## Installation
;;
;; I highly recommended installing tagedit through elpa.
;;
;; It's available on [marmalade](http://marmalade-repo.org/) and
;; [melpa](http://melpa.milkbox.net/):
;;
;;     M-x package-install tagedit
;;
;; You can also install the dependencies on your own, and just dump
;; tagedit in your path somewhere:
;;
;;  - <a href="https://github.com/magnars/s.el">s.el</a>
;;  - <a href="https://github.com/magnars/dash.el">dash.el</a>

;; ## Functions
;;
;; This is it at the moment:
;;
;;  - `tagedit-forward-slurp-tag` moves the next sibling into this tag.
;;  - `tagedit-forward-barf-tag` moves the last child out of this tag.
;;  - `tagedit-raise-tag` replaces the parent tag with this tag.
;;  - `tagedit-kill-attribute` kills the html attribute at point.

;; ## Setup
;;
;; If you want tagedit to bind to the same keys as paredit, there's this:
;;
;; ```cl
;; (eval-after-load "sgml-mode"
;;   '(progn
;;      (require 'tagedit)
;;      (tagedit-add-paredit-like-keybindings)))
;; ```
;;
;; Or you can cherry-pick functions and bind them however you want:
;;
;; ```cl
;; (define-key html-mode-map (kbd "C-<right>") 'tagedit-forward-slurp-tag)
;; (define-key html-mode-map (kbd "C-<left>") 'tagedit-forward-barf-tag)
;; (define-key html-mode-map (kbd "M-r") 'tagedit-raise-tag)
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

;;; Code:

(require 'assoc)
(require 's)

;;;###autoload
(defun tagedit-forward-slurp-tag ()
  (interactive)
  (when (tagedit--is-self-closing (tagedit--current-tag))
    (save-excursion (tagedit--open-self-closing-tag (tagedit--current-tag))))
  (save-excursion
    (let* ((current-tag (tagedit--current-tag))
           (next-sibling (tagedit--next-sibling current-tag)))
      (if next-sibling
          (tagedit--move-end-tag current-tag (aget next-sibling :end))
        (let ((parent (tagedit--parent-tag current-tag)))
          (if (not parent)
              (error "Nothing to slurp")
            (goto-char (aget parent :beg))
            (tagedit-forward-slurp-tag))))))
  (save-excursion (tagedit--ensure-proper-multiline (tagedit--current-tag)))
  (tagedit--indent (tagedit--current-tag)))

;;;###autoload
(defun tagedit-forward-barf-tag ()
  (interactive)
  (save-excursion
   (let* ((current-tag (tagedit--current-tag))
          (last-child (tagedit--last-child current-tag)))
     (if (not last-child)
         (error "Nothing to barf")
       (goto-char (aget last-child :beg))
       (skip-syntax-backward " >")
       (tagedit--move-end-tag current-tag (point)))))
  (save-excursion (tagedit--ensure-proper-multiline (tagedit--current-tag)))
  (tagedit--indent (tagedit--parent-tag (tagedit--current-tag))))

(defun tagedit--open-self-closing-tag (tag)
  (goto-char (aget tag :end))
  (forward-char -1)
  (delete-char -1)
  (forward-char 1)
  (tagedit--insert-closing-tag tag))

(defun tagedit--ensure-proper-multiline (tag)
  (when (tagedit--is-multiline tag)
    (goto-char (aget tag :end))
    (unless (looking-at "$")
      (newline))
    (backward-sexp)
    (unless (looking-back "^\s*")
      (newline))
    (goto-char (aget tag :beg))
    (unless (looking-back "^\s*")
      (newline))
    (forward-sexp)
    (unless (looking-at "$")
      (newline))))

(defun tagedit--is-multiline (tag)
  (not (= (line-number-at-pos (aget tag :beg))
          (line-number-at-pos (aget tag :end)))))

(defun tagedit--insert-closing-tag (tag)
  (insert "</" (aget tag :name) ">"))

(defun tagedit--move-end-tag (tag pos)
  (let ((tag-start-line (line-number-at-pos (point))))
    (goto-char pos)
    (save-excursion
      (tagedit--delete-end-tag tag))
    (tagedit--insert-closing-tag tag)))

(defun tagedit--delete-end-tag (tag)
  (goto-char (aget tag :end))
  (if (save-excursion ;; end tag is alone on line
        (beginning-of-line)
        (looking-at (concat "^\s*</" (aget tag :name) ">$")))
      (delete-char (- 0 (current-column) 1)) ;; then delete entire line
    (backward-sexp)
    (delete-region (point) (aget tag :end)))) ;; otherwise just the end tag

;;;###autoload
(defun tagedit-kill-attribute ()
  (interactive)
  (when (and (tagedit--inside-tag)
             (not (looking-at ">")))
    (tagedit--select-attribute)
    (kill-region (1- (region-beginning)) (region-end))
    (just-one-space)
    (when (looking-at ">")
      (delete-char -1))))

;;;###autoload
(defun tagedit-toggle-multiline-tag ()
  (interactive)
  (let ((current-tag (tagedit--current-tag)))
    (if (tagedit--is-self-closing current-tag)
        (message "Can't toggle multiline for self-closing tags.")
      (if (tagedit--is-one-line-tag current-tag)
          (tagedit--one->multi-line-tag current-tag)))))

;;;###autoload
(defun tagedit-raise-tag ()
  (interactive)
  (let* ((current (tagedit--current-tag))
         (contents (tagedit--contents current))
         (parent (tagedit--parent-tag current)))
    (save-excursion
      (tagedit--delete parent)
      (let ((beg (point)))
        (insert contents)
        (indent-region beg (point))))))

;;;###autoload
(defun tagedit-add-paredit-like-keybindings ()
  (interactive)

  ;; paredit lookalikes
  (define-key html-mode-map (kbd "C-<right>") 'tagedit-forward-slurp-tag)
  (define-key html-mode-map (kbd "C-)") 'tagedit-forward-slurp-tag)
  (define-key html-mode-map (kbd "C-<left>") 'tagedit-forward-barf-tag)
  (define-key html-mode-map (kbd "C-}") 'tagedit-forward-barf-tag)
  (define-key html-mode-map (kbd "M-r") 'tagedit-raise-tag)

  ;; no paredit equivalents
  (define-key html-mode-map (kbd "s-k") 'tagedit-kill-attribute)
  (define-key html-mode-map (kbd "s-<return>") 'tagedit-toggle-multiline-tag))

(defun tagedit--indent (tag)
  (indent-region (aget tag :beg)
                 (aget tag :end)))

(defvar tagedit--self-closing-tags
  '("img" "hr" "br" "input"))

(defun tagedit--is-self-closing (tag)
  (or (eq :t (aget tag :self-closing))
      (member (aget tag :name)
              tagedit--self-closing-tags)))

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
      (indent-region (point) (+ 3 end)))))

(defun tagedit--parent-tag (tag)
  (save-excursion
    (goto-char (1- (aget tag :beg)))
    (let ((parent (tagedit--current-tag)))
      (when (and parent
                 (not (= (aget parent :beg)
                         (aget tag :beg))))
        parent))))

(defun tagedit--just-one-blank-line ()
  (newline 2)
  (forward-line -1)
  (delete-blank-lines))

(defun tagedit--contents (tag)
  (buffer-substring (aget tag :beg)
                    (aget tag :end)))

(defun tagedit--inner-contents (tag)
  (if (tagedit--is-self-closing tag)
      ""
    (buffer-substring (tagedit--inner-beg tag)
                      (tagedit--inner-end tag))))

(defun tagedit--delete (tag)
  (goto-char (aget tag :beg))
  (delete-region (aget tag :beg)
                 (aget tag :end)))

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
             (end (when (sgml-skip-tag-forward 1) (point)))
             (self-closing (if (looking-back "/>") :t :f)))
        `((:name . ,(if self-closing (s-chop-suffix "/" name) name))
          (:type . ,type)
          (:self-closing . ,self-closing)
          (:beg . ,beg)
          (:end . ,end))))))

(defun tagedit--current-text-node ()
  (save-excursion
    (let* ((beg (progn
                  (search-backward ">")
                  (forward-char 1)
                  (skip-syntax-forward " >")
                  (point)))
           (type (if (looking-back "^\\s *") :block :inline))
           (end (progn
                  (search-forward "<")
                  (forward-char -1)
                  (skip-syntax-backward " >")
                  (point))))
      `((:name . "text-node")
        (:type . ,type)
        (:self-closing :t)
        (:beg . ,beg)
        (:end . ,end)))))

(defun tagedit--get-context ()
  (when (looking-at "<") (forward-char 1))
  (let ((context (car (sgml-get-context))))
    (when (and context (string= "close" (sgml-tag-type context)))
      (forward-char 1)
      (sgml-skip-tag-backward 1)
      (forward-char 1)
      (setq context (car (sgml-get-context))))
    context))

(defun tagedit--last-child (tag)
  (unless (tagedit--empty-tag tag)
    (save-excursion
      (goto-char (aget tag :end))
      (backward-sexp)
      (skip-syntax-backward " >")
      (if (looking-back ">")
          (progn
            (backward-char 1)
            (tagedit--current-tag))
        (tagedit--current-text-node)))))

(defun tagedit--empty-tag (tag)
  (equal "" (s-trim (tagedit--inner-contents tag))))

(defun tagedit--looking-at-parents-end-tag (tag)
  (save-excursion
    (let ((here (point))
          (parent (tagedit--parent-tag tag)))
      (when parent
        (goto-char (aget parent :end))
        (backward-sexp)
        (= here (point))))))

(defun tagedit--next-sibling (tag)
  (save-excursion
    (goto-char (aget tag :end))
    (skip-syntax-forward " >")
    (unless (eobp)
      (if (looking-at "<")
          (unless (tagedit--looking-at-parents-end-tag tag)
            (forward-char 1)
            (tagedit--current-tag))
        (tagedit--current-text-node)))))

(provide 'tagedit)

;;; tagedit.el ends here
