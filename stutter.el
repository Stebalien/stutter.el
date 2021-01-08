;;; stutter-mode.el --- pauses redisplay on heavy output -*- lexical-binding: t -*-

;; Copyright 2020 Steven Allen <steven@stebalien.com>

;; Author: Steven Allen <steven@stebalien.com>
;; URL: https://github.com/Stebalien/stutter.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.0"))
;; Keywords: systemd shell

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; An emacs package to improve performance of emacs-based shells when outputting
;; large amounts of text.

;;; Requirements:

;; Emacs 27.0.0

;;; Code:
(require 'timer)

(defvar-local stutter--resume-timer nil)
(defvar-local stutter--old-buffer-size 0)
(defvar-local stutter--last-check-time 0)

(defun stutter--resume (buf)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq stutter--resume-timer nil
            inhibit-redisplay nil
            stutter--last-check-time (float-time)
            stutter--old-buffer-size (buffer-size)))))

(defun stutter (&rest _ignored)
  (unless stutter--resume-timer
    (let ((now (float-time))
          (new-size (buffer-size)))
      (unless (< now (+ stutter--last-check-time 0.05))
        (when (> new-size (+ stutter--old-buffer-size 4096))
          (setq inhibit-redisplay t
                stutter--resume-timer (run-at-time 0.1 nil
                                                   #'stutter--resume
                                                   (current-buffer))))
        (setq stutter--old-buffer-size new-size
              stutter--last-check-time now)))))

(define-minor-mode stutter-mode
  "Defer redisplay while large buffer changes are underway"
  :lighter " Stutter"
  (let ((hook (cond
               ((eq major-mode 'eshell-mode) 'eshell-output-filter-functions)
               ((derived-mode-p 'comint-mode) 'comint-output-filter-functions)
               (t 'after-change-functions))))
    (if stutter-mode
        (add-hook hook #'stutter 100 'local)
      (remove-hook hook #'stutter 'local))))

(provide 'stutter-mode)
