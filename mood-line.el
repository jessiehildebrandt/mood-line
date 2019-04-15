;;; mood-line.el --- A minimal mode-line inspired by doom-modeline. -*- lexical-binding: t; -*-

;; Author: Jessie Hildebrandt <jessieh.net>
;; Homepage: https://gitlab.com/jessieh/mood-line
;; Keywords: mode-line faces
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; mood-line is a minimal mode-line configuration that aims to replicate
;; some of the features of the doom-modeline package.
;;
;; Features offered:
;; * Clean, minimal design
;; * Anzu and multiple-cursors counter
;; * Version control status indicator
;; * Flycheck status indicator
;; * Lightweight with no dependencies
;;
;; To enable mood-line:
;; (mood-line-activate)

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
;; Config
;;

(defgroup mood-line nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'mode-line)

(defface mood-line-status-grayed-out
  '((t (:inherit (font-lock-doc-face))))
  "Face used for neutral or inactive status indicators in the mode-line."
  :group 'mood-line)

(defface mood-line-status-info
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for generic status indicators in the mode-line."
  :group 'mood-line)

(defface mood-line-status-good
  '((t (:inherit (success))))
  "Face used for success status indicators in the mode-line."
  :group 'mood-line)

(defface mood-line-status-warning
  '((t (:inherit (warning))))
  "Face for warning status indicators in the mode-line."
  :group 'mood-line)

(defface mood-line-status-error
  '((t (:inherit (error))))
  "Face for error stauts indicators in the mode-line."
  :group 'mood-line)

(defface mood-line-unimportant
  '((t (:inherit (font-lock-doc-face))))
  "Face used for less important mode-line elements."
  :group 'mood-line)


(defface mood-line-modified
  '((t (:inherit (error))))
  "Face used for the 'modified' indicator symbol in the mode-line."
  :group 'mood-line)

;;
;; Helper functions
;;

;;;###autoload
(defun mood-line-format (left right)
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
(defsubst mood-line-is-active ()
  "Return \"t\" if the current window is active, \"nil\" if it is not."
  (eq (selected-window) mood-line--current-window))

;;
;; Update functions
;;

;; Window update function
(defvar-local mood-line--current-window (frame-selected-window))
(defun mood-line--update-selected-window (&rest _)
  "Update the `mood-line--current-window' variable."
  (when (frame-selected-window)
    (let ((win (frame-selected-window)))
      (unless (minibuffer-window-active-p win)
        (setq mood-line--current-window win)))))

;; VC update function
(defvar-local mood-line--vc-text nil)
(defun mood-line--update-vc-segment (&rest _)
  "Update `mood-line--vc-text' against the current VCS state."
  (setq mood-line--vc-text
        (when (and vc-mode buffer-file-name)
          (let ((backend (vc-backend buffer-file-name))
                (state (vc-state buffer-file-name (vc-backend buffer-file-name))))
            (let ((face 'mode-line-inactive)
                  (active (mood-line-is-active)))
              (concat (cond ((memq state '(edited added))
                             (if active (setq face 'mood-line-status-info))
                             (propertize "✚" 'face face))
                            ((eq state 'needs-merge)
                             (if active (setq face 'mood-line-status-warning))
                             (propertize "●" 'face face))
                            ((eq state 'needs-update)
                             (if active (setq face 'mood-line-status-warning))
                             (propertize "⬆" 'face face))
                            ((memq state '(removed conflict unregistered))
                             (if active (setq face 'mood-line-status-error))
                             (propertize "✖" 'face face))
                            (t
                             (if active (setq face 'mood-line-status-grayed-out))
                             (propertize "✔" 'face face)))
                      " "
                      (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                                  'face (if active face))
                      "  "))))))

;; Flycheck update function
(defvar-local mood-line--flycheck-text nil)
(defun mood-line--update-flycheck-segment (&optional status)
  "Update `mood-line--flycheck-text' against the reported flycheck STATUS."
  (setq mood-line--flycheck-text
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (propertize (concat "✚ Issues: "
                                                 (number-to-string sum)
                                                 "  ")
                                         'face (if .error
                                                   'mood-line-status-error
                                                 'mood-line-status-warning))))
                       (propertize "✔ Good  " 'face 'mood-line-status-good)))
          ('running (propertize "● Checking  " 'face 'mood-line-status-info))
          ('no-checker "")
          ('errored (propertize "✖ Error  " 'face 'mood-line-status-error))
          ('interrupted (propertize "⏸ Paused  " 'face 'mood-line-status-grayed-out)))))

;;
;; Segments
;;

;;;###autoload
(defun mood-line-segment-modified ()
  "Displays a color-coded buffer modification indicator in the mode-line."
  (propertize
   (if (and
        (buffer-modified-p)
        (not (string-match-p "\\*.*\\*" (buffer-name))))
       " ● "
     "   ")
   'face 'mood-line-modified))

;;;###autoload
(defun mood-line-segment-buffer-name ()
  "Displays the name of the current buffer in the mode-line."
  (concat (propertize "%b" 'face 'mode-line-buffer-id) "  "))

;;;###autoload
(defun mood-line-segment-anzu ()
  "Displays color-coded anzu status information in the mode-line (if available)."
  (when anzu--state
    (concat (anzu--update-mode-line) "  ")))

;;;###autoload
(defun mood-line-segment-multiple-cursors ()
  "Displays the number of active multiple-cursors in the mode-line (if available)."
  (when multiple-cursors-mode
    (concat "MC:"
            (format #("%d" 0 2 (face font-lock-warning-face)) (mc/num-cursors))
            "  ")))

;;;###autoload
(defun mood-line-segment-position ()
  "Displays the current cursor position in the mode-line."
  (concat "%l:%c "
          (propertize "%p%%" 'face (if (mood-line-is-active)
                                       'mood-line-unimportant
                                     'mode-line-inactive))
          "  "))

;;;###autoload
(defun mood-line-segment-encoding ()
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

;;;###autoload
(defun mood-line-segment-vc ()
  "Displays color-coded version control information in the mode-line."
  mood-line--vc-text)

;;;###autoload
(defun mood-line-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (propertize "%m  "
              'face (if (mood-line-is-active)
                        'bold
                      'mood-line-status-grayed-out)))

;;;###autoload
(defun mood-line-segment-flycheck ()
  "Displays color-coded flycheck information in the mode-line (if available)."
  mood-line--flycheck-text)

;;
;; Activation function
;;

;;;###autoload
(defun mood-line-activate ()
  "Replace the current mode-line with mood-line."
  (interactive)

  ;; Setup flycheck hooks (if available)
  (add-hook 'flycheck-status-changed-functions #'mood-line--update-flycheck-segment)
  (add-hook 'flycheck-mode-hook #'mood-line--update-flycheck-segment)

  ;; Setup VC hooks (if available)
  (add-hook 'find-file-hook #'mood-line--update-vc-segment)
  (add-hook 'after-save-hook #'mood-line--update-vc-segment)
  (advice-add #'vc-refresh-state :after #'mood-line--update-vc-segment)

  ;; Setup window update hooks
  (add-hook 'window-configuration-change-hook #'mood-line--update-selected-window)
  (add-hook 'focus-in-hook #'mood-line--update-selected-window)
  (advice-add #'handle-switch-frame :after #'mood-line--update-selected-window)
  (advice-add #'select-window :after #'mood-line--update-selected-window)

  ;; Set the new mode-line-format
  (setq-default mode-line-format
                '((:eval
                   (mood-line-format
                    ;; Left
                    (format-mode-line
                     '((:eval (mood-line-segment-modified))
                       (:eval (mood-line-segment-buffer-name))
                       (:eval (mood-line-segment-anzu))
                       (:eval (mood-line-segment-multiple-cursors))
                       (:eval (mood-line-segment-position))))

                    ;; Right
                    (format-mode-line
                     '((:eval (mood-line-segment-vc))
                       (:eval (mood-line-segment-major-mode))
                       (:eval (mood-line-segment-flycheck))
                       " ")))))))

;;
;; Provide mood-line
;;

(provide 'mood-line)

;;; mood-line.el ends here
