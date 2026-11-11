;;; markdown-phscroll.el --- Apply horizontal scroll to markdown-mode tables -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Enable horizontal scrolling of wide tables in `markdown-mode' (and
;; `gfm-mode') buffers while the rest of the buffer wraps
;; (`truncate-lines' nil).  Modeled on org-phscroll.el.
;;
;; Table lines are identified with markdown-mode's own
;; `markdown-table-at-point-p', so code blocks are excluded and gfm-mode's
;; looser table syntax (no leading "|") is recognized.

;;; Usage:

;; (with-eval-after-load "markdown-mode"
;;   (require 'markdown-phscroll))

;;; Code:

(require 'markdown-mode)
(require 'phscroll)

(defvar-local markdown-phscroll-font-lock-keywords
    (list '(markdown-phscroll--fontify)))

;;;###autoload
(define-minor-mode markdown-phscroll-mode
  "Apply phscroll to markdown tables."
  :init-value nil
  :group 'markdown-phscroll
  (if markdown-phscroll-mode
      (progn
        ;; Do not update on modification-hooks.  Update on font-lock.
        (setq-local phscroll-update-area-display-on-modified nil)
        (phscroll-mode 1)
        (font-lock-add-keywords nil markdown-phscroll-font-lock-keywords t)
        (font-lock-flush))
    (font-lock-remove-keywords nil markdown-phscroll-font-lock-keywords)
    (setq-local phscroll-update-area-display-on-modified t)
    (phscroll-delete-all)
    (phscroll-mode -1)))

(defun markdown-phscroll--table-line-p ()
  "Return non-nil if the line at point is part of a markdown table."
  (and (save-excursion
         ;; Cheap pre-check before markdown-mode's (slower) test.
         (re-search-forward "|" (line-end-position) t))
       (markdown-table-at-point-p)))

(defun markdown-phscroll--apply (beg end table-p first-p last-p)
  "Cover (TABLE-P) or clear (otherwise) phscroll areas over BEG..END.
FIRST-P / LAST-P mean the segment touches the start / end of the range
being fontified, where a table may continue outside that range."
  (when (< beg end)
    (if (not table-p)
        (phscroll-remove-region beg end)
      ;; Join an area that continues before/after the fontified range,
      ;; so a table split across font-lock chunks stays one area.
      (when (and first-p
                 (> beg (point-min))
                 (phscroll-enum-area (phscroll-line-begin (1- beg))
                                     (phscroll-line-end (1- beg))))
        (setq beg (phscroll-line-begin (1- beg))))
      (when (and last-p
                 (phscroll-enum-area (phscroll-line-begin end)
                                     (phscroll-line-end end)))
        (setq end (min (point-max) (1+ (phscroll-line-end end)))))
      (phscroll-cover-region beg end))))

(defun markdown-phscroll--fontify (limit)
  "Font-lock matcher: create, move or delete phscroll areas up to LIMIT."
  (when markdown-phscroll-mode
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)
          ;; The width of text may have changed due to earlier keywords.
          (phscroll-invalidate-region (point) limit)
          (goto-char (phscroll-line-begin))
          (let* ((start (point))
                 (phscroll-fontify-range (cons start limit))
                 (seg-beg start)
                 (seg-table (markdown-phscroll--table-line-p)))
            ;; Split the range into runs of table / non-table lines.
            (while (and (< (point) limit)
                        (= (forward-line 1) 0)
                        (< (point) limit))
              (let ((table-p (markdown-phscroll--table-line-p)))
                (unless (eq (and table-p t) (and seg-table t))
                  (markdown-phscroll--apply seg-beg (point) seg-table
                                            (= seg-beg start) nil)
                  (setq seg-beg (point)
                        seg-table table-p))))
            ;; Final run extends to the end of the fontified range.
            (markdown-phscroll--apply seg-beg (max seg-beg limit) seg-table
                                      (= seg-beg start) t))))))
  ;; Never report a match; this keyword only has side effects.
  nil)

;;;###autoload
(defun markdown-phscroll-activate ()
  "Enable `markdown-phscroll-mode' in all markdown buffers."
  (interactive)
  (add-hook 'markdown-mode-hook #'markdown-phscroll-mode))

;;;###autoload
(defun markdown-phscroll-deactivate ()
  "Stop enabling `markdown-phscroll-mode' in new markdown buffers."
  (interactive)
  (remove-hook 'markdown-mode-hook #'markdown-phscroll-mode))

(with-eval-after-load "markdown-mode"
  (markdown-phscroll-activate))

(provide 'markdown-phscroll)
;;; markdown-phscroll.el ends here
