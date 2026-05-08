;;; consult-flycheck.el --- Provides the command `consult-flycheck' -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Daniel Mendler

;; Author: Daniel Mendler and Consult contributors
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2020
;; Version: 1.1
;; Package-Requires: ((emacs "29.1") (consult "2.8") (flycheck "35"))
;; URL: https://github.com/minad/consult-flycheck
;; Keywords: languages, tools, completion

;; This program is free software: you can redistribute it and/or modify
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

;; Provides the command `consult-flycheck'.  This is an extra package,
;; since the consult.el package only depends on Emacs core components.

;;; Code:

(require 'consult)
(require 'flycheck)

(defcustom consult-flycheck-focus-at-point nil
  "When non-nil, pre-select the error at or nearest after point."
  :type 'boolean
  :group 'consult-flycheck)

(defconst consult-flycheck--narrow
  '((?e . "Error")
    (?w . "Warning")
    (?i . "Info")))

(defun consult-flycheck--sort-predicate (x y)
  "Compare X and Y by filename, severity, then by location.
In contrast to `flycheck-error-level-<' sort errors first."
  (let* ((lx (flycheck-error-level x))
         (ly (flycheck-error-level y))
         (sx (flycheck-error-level-severity lx))
         (sy (flycheck-error-level-severity ly))
         (fx (if-let* ((file (flycheck-error-filename x)))
                 (file-name-nondirectory file)
               (buffer-name (flycheck-error-buffer x))))
         (fy (if-let* ((file (flycheck-error-filename y)))
                 (file-name-nondirectory file)
               (buffer-name (flycheck-error-buffer y)))))
    (if (string= fx fy)
        (if (= sx sy)
            (if (string= lx ly)
                (flycheck-error-< x y)
              (string< lx ly))
          (> sx sy))
      (string< fx fy))))

(defun consult-flycheck--candidates ()
  "Return flycheck errors as alist."
  (consult--forbid-minibuffer)
  (unless flycheck-current-errors
    (user-error "No flycheck errors (Status: %s)" flycheck-last-status-change))
  (let* ((errors (mapcar
                  (lambda (err)
                    (list
                     (if-let* ((file (flycheck-error-filename err)))
                         (file-name-nondirectory file)
                       (buffer-name (flycheck-error-buffer err)))
                     (number-to-string (flycheck-error-line err))
                     (symbol-name (flycheck-error-level err))
                     err))
                  (seq-sort #'consult-flycheck--sort-predicate flycheck-current-errors)))
         (file-width (apply #'max (mapcar (lambda (x) (length (car x))) errors)))
         (line-width (apply #'max (mapcar (lambda (x) (length (cadr x))) errors)))
         (level-width (apply #'max (mapcar (lambda (x) (length (caddr x))) errors)))
         (fmt (format "%%%ds %%%ds %%-%ds\t%%s\t(%%s)" file-width line-width level-width)))
    (mapcar
     (pcase-lambda (`(,file ,line ,level-name ,err))
       (let* ((level (flycheck-error-level err))
              (filename (flycheck-error-filename err))
              (err-copy (copy-flycheck-error err))
              (buffer (if filename
                          (find-file-noselect filename 'nowarn)
                        (flycheck-error-buffer err))))
         (when (buffer-live-p buffer)
           ;; Update buffer in case the source of the error resides in
           ;; a different file from where it was detected (i.e., the
           ;; filename field of the error is different than the
           ;; buffer).
           (setf (flycheck-error-buffer err-copy) buffer))
         (propertize
          (format fmt
                  (propertize file 'face 'flycheck-error-list-filename)
                  (propertize line 'face 'flycheck-error-list-line-number)
                  (propertize level-name 'face (flycheck-error-level-error-list-face level))
                  (propertize (subst-char-in-string ?\n ?\s
                                                    (flycheck-error-message err))
                              'face 'flycheck-error-list-error-message)
                  (propertize (symbol-name (flycheck-error-checker err))
                              'face 'flycheck-error-list-checker-name))
          'consult--candidate
          (let* ((range (flycheck-error-region-for-mode
                         err-copy
                         (or flycheck-highlighting-mode 'lines)))
                 (beg (car range))
                 (end (cdr range)))
            (list (set-marker (make-marker) beg buffer)
                  (cons 0 (- end beg))))
          'consult--type
          (pcase level-name
            ((rx (and (0+ nonl)
                      "error"
                      (0+ nonl)))
             ?e)
            ((rx (and (0+ nonl)
                      "warning"
                      (0+ nonl)))
             ?w)
            (_ ?i)))))
     errors)))

(defun consult-flycheck--reorder-at-point (candidates)
  "Reorder CANDIDATES so the error at or nearest after point comes first.
Candidates before point are appended after, preserving relative order."
  (let ((pos (point))
        (buf (current-buffer))
        (best-idx 0)
        (best-dist most-positive-fixnum)
        (idx 0))
    (dolist (cand candidates)
      (when-let* ((data (get-text-property 0 'consult--candidate cand))
                  (marker (car data)))
        (when (and (eq (marker-buffer marker) buf)
                   (marker-position marker))
          (let ((dist (- (marker-position marker) pos)))
            (when (or (and (>= dist 0) (< dist best-dist))
                      (and (< best-dist 0) (> dist best-dist)))
              (setq best-idx idx
                    best-dist dist)))))
      (cl-incf idx))
    (if (= best-idx 0)
        candidates
      (append (nthcdr best-idx candidates)
              (seq-take candidates best-idx)))))

;;;###autoload
(defun consult-flycheck ()
  "Jump to flycheck error."
  (interactive)
  (let ((candidates (consult--with-increased-gc (consult-flycheck--candidates))))
    (when consult-flycheck-focus-at-point
      (setq candidates (consult-flycheck--reorder-at-point candidates)))
    (consult--read
     candidates
     :prompt "Flycheck error: "
     :category 'consult-flycheck-error
     :default (when consult-flycheck-focus-at-point (car candidates))
     :history t ;; disable history
     :require-match t
     :sort nil
     :group (consult--type-group consult-flycheck--narrow)
     :narrow (consult--type-narrow consult-flycheck--narrow)
     :lookup #'consult--lookup-candidate
     :state (consult--jump-state))))

(provide 'consult-flycheck)
;;; consult-flycheck.el ends here
