;;; auto-scroll-mode.el --- Auto scroll buffer -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-03-01 18:47:41 stardiviner>

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "25"))
;; Package-Version: 0.1
;; Keywords: docs wp
;; homepage: https://github.com/stardiviner/auto-scroll-mode

;; auto-scroll-mode is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; auto-scroll-mode is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; 
;;; Usage:
;;; [M-x auto-scroll-mode]

;;; Code:

(defcustom auto-scroll-lps 0.5
  "Auto scroll lines per second."
  :type 'float
  :safe #'floatp
  :group 'auto-scroll-mode)

(defcustom auto-scroll-highlight-line-face '((foreground-color . "black")
                                             (background-color . "orange"))
  "The face spec for auto-scroll-mode highlight line."
  :type 'list
  :safe #'listp
  :group 'auto-scroll-mode)

(defvar auto-scroll--timer nil)
(defvar auto-scroll--line-position nil)
(defvar auto-scroll--overlay nil)

(defun auto-scroll--update ()
  "Auto scroll downl N lines per second of buffer."
  (let* ((line-begin (line-beginning-position))
         ;; move line forward. NOTE This forwarding must be here before moving overlay forward.
         (_move (forward-line 1))
         (line-end (line-end-position)))
    (if (eobp) ; reached end of buffer.
        (progn
          (auto-scroll-mode -1)
          (setq auto-scroll--line-position nil))
      ;; create line overlay to highlight current reading line.
      (unless auto-scroll--overlay
        (setq auto-scroll--overlay (make-overlay line-begin line-end)))
      ;; scroll down line
      (when auto-scroll--overlay
        (move-overlay auto-scroll--overlay line-begin line-end))
      (overlay-put auto-scroll--overlay
                   'face auto-scroll-highlight-line-face)
      (forward-line 1))))

(defun auto-scroll-start ()
  "Start auto scroll."
  (interactive)
  (read-only-mode 1)
  ;; resume from paused position
  (if auto-scroll--line-position
      (goto-line auto-scroll--line-position)
    (goto-char (point-min)))
  (setq auto-scroll--timer
        (run-with-timer 0 (/ 1.0 auto-scroll-lps) #'auto-scroll--update))
  (message "auto-scroll-mode started..."))

(defun auto-scroll-stop ()
  "Stop auto scroll."
  (interactive)
  (when auto-scroll--timer
    (cancel-timer auto-scroll--timer)
    (setq auto-scroll--timer nil)
    (delete-overlay auto-scroll--overlay))
  (read-only-mode -1)
  (message "auto-scroll-mode stopped."))

(defun auto-scroll-pause-or-resume ()
  "Pause or resume auto-scroll-mode."
  (interactive)
  (if auto-scroll--timer
      (auto-scroll-stop)
    (auto-scroll-start)))

(defun auto-scroll-quit ()
  "Disable auto-scroll-mode."
  (interactive)
  (auto-scroll-mode -1))

(defvar auto-scroll-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'auto-scroll-quit)
    (define-key map (kbd "SPC") #'auto-scroll-pause-or-resume)
    (define-key map [remap keyboard-quit] #'auto-scroll-quit)
    map)
  "Keymap for auto-scroll-mode.")

(define-minor-mode auto-scroll-mode
  "Auto scroll buffer minor mode."
  :init nil
  :lighter " Auto/Scroll "
  :keymap auto-scroll-mode-map
  (if auto-scroll-mode
      (auto-scroll-start)
    (auto-scroll-stop)))


(provide 'auto-scroll-mode)

;;; auto-scroll-mode.el ends here
