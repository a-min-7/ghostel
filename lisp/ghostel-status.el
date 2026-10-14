;;; ghostel-status.el --- Bell and notifications for ghostel -*- lexical-binding: t; -*-

;; Author: Daniel Kraus <daniel@kraus.my>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; What a program running in the terminal reports about itself outside
;; the grid: the bell (BEL) and desktop notifications (OSC 9, OSC 777).
;; The native module calls `ghostel--bell' and
;; `ghostel--handle-notification'; both run user hooks.

;;; Code:

(require 'compat)
(require 'subr-x)

(declare-function alert "alert")
(declare-function ghostel--defer "ghostel")
(declare-function ghostel--run-hook-safely "ghostel")


;;; Customization

(defcustom ghostel-bell-functions '(ghostel-ding)
  "Hook run when the terminal receives BEL.
Each function is called with no arguments and the terminal buffer
current.  Remove `ghostel-ding' to silence the bell."
  :type 'hook
  :group 'ghostel)

(define-obsolete-variable-alias 'ghostel-notification-function
  'ghostel-notification-functions "0.58.0")

(defcustom ghostel-notification-functions '(ghostel-default-notify)
  "Hook run for OSC 9 / OSC 777 desktop notifications.
Each function is called with two string arguments, TITLE and BODY,
with the originating terminal buffer current.  TITLE is empty for
iTerm2-style OSC 9.  The hook runs asynchronously via `run-at-time',
so handlers may block or spawn processes without stalling the
terminal."
  :type 'hook
  :group 'ghostel)


;;; Bell

(defun ghostel-ding ()
  "Beep without terminating a running keyboard macro."
  (ding t))

(defun ghostel--bell ()
  "Run `ghostel-bell-functions' for a BEL from the native VT parser."
  (ghostel--run-hook-safely 'ghostel-bell-functions))


;;; Notifications

(defun ghostel-default-notify (title body)
  "Default handler for OSC 9 / OSC 777 notifications.
Uses the `alert' package (https://github.com/jwiegley/alert) when
available - it picks a sensible backend per platform (osascript on
macOS, libnotify on Linux, Growl, terminal-notifier, etc.).  Falls
back to `message' when alert isn't installed.  TITLE is the
notification summary; when empty (iTerm2-style OSC 9) the buffer
name is used.  BODY is the notification text.

Runs with the originating ghostel buffer current, so an empty
TITLE falls back to that buffer's name."
  (let ((summary (if (or (null title) (string-empty-p title))
                     (buffer-name)
                   title)))
    (if (and (require 'alert nil t) (fboundp 'alert))
        (alert body :title summary)
      (message "%s: %s" summary body))))

(defun ghostel-tty-forward-notify (title body)
  "Forward OSC 9 / OSC 777 notifications to the outer terminal.
On a tty frame, re-emit TITLE and BODY as OSC 9 (empty TITLE) or
OSC 777 to the frame showing this buffer, else the selected frame.
On a GUI frame, fall back to `ghostel-default-notify'.
Use in place of `ghostel-default-notify' in
`ghostel-notification-functions'."
  (let ((frame (window-frame (get-buffer-window nil t))))
    (if (tty-type frame)
        (send-string-to-terminal
         (if (string-empty-p title)
             (format "\e]9;%s\e\\" body)
           (format "\e]777;notify;%s;%s\e\\" title body))
         frame)
      (ghostel-default-notify title body))))

(defun ghostel--handle-notification (title body)
  "Run `ghostel-notification-functions' with TITLE and BODY.
Deferred via `ghostel--defer' so a slow backend (DBus, osascript)
does not block terminal output."
  (ghostel--defer #'ghostel--run-hook-safely
                  'ghostel-notification-functions title body))

(provide 'ghostel-status)
;;; ghostel-status.el ends here
