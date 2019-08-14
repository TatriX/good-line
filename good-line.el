;;; good-line.el --- A minimal mode-line inspired by mood-line. -*- lexical-binding: t; -*-

;; Author: TatriX <tatrics@gmail.com>
;; Original Author: Jessie Hildebrandt <jessieh.net>
;; Homepage: https://github.com/TatriX/good-line
;; Keywords: mode-line faces
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; good-line is a minimal mode-line configuration that aims to replicate
;; some of the features of the doom-modeline package.
;;
;; Features offered:
;; * Clean, minimal design
;; * Anzu and multiple-cursors counter
;; * Version control status indicator
;; * Flycheck status indicator
;; * Lightweight with no dependencies
;;
;; To enable good-line:
;; (good-line-mode)

;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Code:

;;
;; Variable declarations
;;

(defvar good-line--current-window)
(defvar flycheck-current-errors)
(defvar anzu--state)
(defvar multiple-cursors-mode)

;;
;; Function prototypes
;;

(declare-function flycheck-count-errors "flycheck" (errors))
(declare-function anzu--update-mode-line "anzu" ())
(declare-function mc/num-cursors "multiple-cursors" ())

;;
;; Config
;;

(defgroup good-line nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'mode-line)

(defcustom good-line-show-point nil
  "If t, the value of `point' will be displayed next to the cursor position in the mode-line."
  :group 'good-line
  :type 'boolean)

(defface good-line-status-grayed-out
  '((t (:inherit (font-lock-doc-face))))
  "Face used for neutral or inactive status indicators in the mode-line."
  :group 'good-line)

(defface good-line-status-info
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for generic status indicators in the mode-line."
  :group 'good-line)

(defface good-line-status-success
  '((t (:inherit (success))))
  "Face used for success status indicators in the mode-line."
  :group 'good-line)

(defface good-line-status-warning
  '((t (:inherit (warning))))
  "Face for warning status indicators in the mode-line."
  :group 'good-line)

(defface good-line-status-error
  '((t (:inherit (error))))
  "Face for error stauts indicators in the mode-line."
  :group 'good-line)

(defface good-line-unimportant
  '((t (:inherit (font-lock-doc-face))))
  "Face used for less important mode-line elements."
  :group 'good-line)


(defface good-line-modified
  '((t (:inherit (error))))
  "Face used for the 'modified' indicator symbol in the mode-line."
  :group 'good-line)

;;
;; Helper functions
;;

(defun good-line-format (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT, aligned respectively."
  (let ((reserve (length right)))
    (when (and (display-graphic-p) (eq 'right (get-scroll-bar-mode)))
      (setq reserve (- reserve 3)))
    (concat
     left
     " "
     (propertize  " "
                  'display `((space :align-to (- (+ right right-fringe right-margin) ,(+ reserve 0)))))
     right)))

;; Define a helper function to determine whether or not the current window is active.
(defsubst good-line-is-active ()
  "Return \"t\" if the current window is active, \"nil\" if it is not."
  (eq (selected-window) good-line--current-window))

;;
;; Update functions
;;

;; Window update function
(defvar-local good-line--current-window (frame-selected-window))
(defun good-line--update-selected-window (&rest _)
  "Update the `good-line--current-window' variable."
  (when (frame-selected-window)
    (let ((win (frame-selected-window)))
      (unless (minibuffer-window-active-p win)
        (setq good-line--current-window win)))))

;; VC update function
(defvar-local good-line--vc-text nil)
(defun good-line--update-vc-segment (&rest _)
  "Update `good-line--vc-text' against the current VCS state."
  (setq good-line--vc-text
        (when (and vc-mode buffer-file-name)
          (let ((backend (vc-backend buffer-file-name))
                (state (vc-state buffer-file-name (vc-backend buffer-file-name))))
            (let ((face 'mode-line-inactive)
                  (active (good-line-is-active)))
              (concat (cond ((memq state '(edited added))
                             (if active (setq face 'good-line-status-info))
                             (propertize "✚" 'face face))
                            ((eq state 'needs-merge)
                             (if active (setq face 'good-line-status-warning))
                             (propertize "●" 'face face))
                            ((eq state 'needs-update)
                             (if active (setq face 'good-line-status-warning))
                             (propertize "⬆" 'face face))
                            ((memq state '(removed conflict unregistered))
                             (if active (setq face 'good-line-status-error))
                             (propertize "✖" 'face face))
                            (t
                             (if active (setq face 'good-line-status-grayed-out))
                             (propertize "✔" 'face face)))
                      " "
                      (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                                  'face (if active face))
                      "  "))))))

;; Flycheck update function
(defvar-local good-line--flycheck-text nil)
(defun good-line--update-flycheck-segment (&optional status)
  "Update `good-line--flycheck-text' against the reported flycheck STATUS."
  (setq good-line--flycheck-text
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (propertize (concat "✚ Issues: "
                                                 (number-to-string sum)
                                                 "  ")
                                         'face (if .error
                                                   'good-line-status-error
                                                 'good-line-status-warning))))
                       (propertize "✔ Good  " 'face 'good-line-status-success)))
          ('running (propertize "● Checking  " 'face 'good-line-status-info))
          ('no-checker "")
          ('errored (propertize "✖ Error  " 'face 'good-line-status-error))
          ('interrupted (propertize "⏸ Paused  " 'face 'good-line-status-grayed-out)))))

;;
;; Segments
;;

(defun good-line-segment-modified ()
    "Displays a color-coded buffer modification indicator in the mode-line."
    (propertize
     (if (string-match-p "\\*.*\\*" (buffer-name))
         "   "
       (if buffer-read-only
           " ∅ "
         (if (buffer-modified-p) " ∗ " "   ")))
     'face 'good-line-modified))

(defun good-line-segment-buffer-name ()
  "Displays the name of the current buffer in the mode-line."
  (concat (propertize "%b" 'face 'mode-line-buffer-id) "  "))

(defun good-line-segment-anzu ()
  "Displays color-coded anzu status information in the mode-line (if available)."
  (when (and (boundp 'anzu--state) anzu--state)
    (concat (anzu--update-mode-line) "  ")))

(defun good-line-segment-multiple-cursors ()
  "Displays the number of active multiple-cursors in the mode-line (if available)."
  (when (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
    (concat "MC:"
            (format #("%d" 0 2 (face font-lock-warning-face)) (mc/num-cursors))
            "  ")))

(defun good-line-segment-position ()
  "Displays the current cursor position in the mode-line."
  (concat "%l:%c"
          (when good-line-show-point
            (concat ":"
                    (propertize (format "%d" (point)) 'face (if (good-line-is-active)
                                                                'good-line-unimportant
                                                              'mode-line-inactive))))
          " "
          (propertize "%p%%" 'face (if (good-line-is-active)
                                       'good-line-unimportant
                                     'mode-line-inactive))
          "  "))

(defun good-line-segment-encoding ()
  "Displays the encoding and EOL style of the buffer in the mode-line."
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF  ")
            (1 "CRLF  ")
            (2 "CR  "))
          (let ((sys (coding-system-plist buffer-file-coding-system)))
            (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                   "UTF-8")
                  (t (upcase (symbol-name (plist-get sys :name))))))
          "  "))

(defun good-line-segment-vc ()
  "Displays color-coded version control information in the mode-line."
  good-line--vc-text)

(defun good-line-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (propertize "%m  "
              'face (if (good-line-is-active)
                        'bold
                      'good-line-status-grayed-out)))

(defun good-line-segment-global-mode-string ()
  "Displays the current value of `global-mode-string' in the mode-line."
  (let ((global (format-mode-line global-mode-string 'good-line-status-grayed-out)))
    (unless (string-empty-p global)
      (concat global " "))))

(defun good-line-segment-flycheck ()
  "Displays color-coded flycheck information in the mode-line (if available)."
  good-line--flycheck-text)

(defun good-line-segment-process ()
  "Displays the current value of `mode-line-process' in the mode-line."
  (when mode-line-process
    (list mode-line-process "  ")))

;;
;; Activation function
;;

;; Store the default mode-line format
(defvar good-line--default-mode-line mode-line-format)

;;;###autoload
(define-minor-mode good-line-mode
  "Toggle good-line on or off."
  :group 'good-line
  :global t
  :lighter nil
  (if good-line-mode
      (progn

        ;; Setup flycheck hooks
        (add-hook 'flycheck-status-changed-functions #'good-line--update-flycheck-segment)
        (add-hook 'flycheck-mode-hook #'good-line--update-flycheck-segment)

        ;; Setup VC hooks
        (add-hook 'find-file-hook #'good-line--update-vc-segment)
        (add-hook 'after-save-hook #'good-line--update-vc-segment)
        (advice-add #'vc-refresh-state :after #'good-line--update-vc-segment)

        ;; Setup window update hooks
        (add-hook 'window-configuration-change-hook #'good-line--update-selected-window)
        (add-hook 'focus-in-hook #'good-line--update-selected-window)
        (advice-add #'handle-switch-frame :after #'good-line--update-selected-window)
        (advice-add #'select-window :after #'good-line--update-selected-window)

        ;; Set the new mode-line-format
        (setq-default mode-line-format
                      '((:eval
                         (good-line-format
                          ;; Left
                          (format-mode-line
                           '((:eval (good-line-segment-modified))
                             (:eval (good-line-segment-buffer-name))
                             (:eval (good-line-segment-anzu))
                             (:eval (good-line-segment-multiple-cursors))
                             (:eval (good-line-segment-position))))

                          ;; Right
                          (format-mode-line
                           '((:eval (good-line-segment-vc))
                             (:eval (good-line-segment-major-mode))
                             (:eval (good-line-segment-global-mode-string))
                             (:eval (good-line-segment-flycheck))
                             (:eval (good-line-segment-process))
                             " ")))))))
    (progn

      ;; Remove flycheck hooks
      (remove-hook 'flycheck-status-changed-functions #'good-line--update-flycheck-segment)
      (remove-hook 'flycheck-mode-hook #'good-line--update-flycheck-segment)

      ;; Remove VC hooks
      (remove-hook 'file-find-hook #'good-line--update-vc-segment)
      (remove-hook 'after-save-hook #'good-line--update-vc-segment)
      (advice-remove #'vc-refresh-state #'good-line--update-vc-segment)

      ;; Remove window update hooks
      (remove-hook 'window-configuration-change-hook #'good-line--update-selected-window)
      (remove-hook 'focus-in-hook #'good-line--update-selected-window)
      (advice-remove #'handle-switch-frame #'good-line--update-selected-window)
      (advice-remove #'select-window #'good-line--update-selected-window)

      ;; Restore the original mode-line format
      (setq-default mode-line-format good-line--default-mode-line))))

;;
;; Provide good-line
;;

(provide 'good-line)

;;; good-line.el ends here
