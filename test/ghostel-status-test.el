;;; ghostel-status-test.el --- Tests for ghostel: program status -*- lexical-binding: t; -*-

;;; Commentary:

;; OSC 7501 program status records, the OSC 9;4 mapping onto them, the
;; mode-line and notification members, and the title precedence.

;;; Code:

(require 'ghostel-test-helpers)

(defmacro ghostel-status-test--with-log (log &rest body)
  "Run BODY in a temp buffer with the status hook pushing (ID OLD NEW) onto LOG."
  (declare (indent 1))
  `(let* ((,log nil)
          (ghostel-program-status-functions
           (list (lambda (id old new) (push (list id old new) ,log)))))
     (with-temp-buffer ,@body)))

(defun ghostel-status-test--report (state &optional id kind progress app title msg)
  "Apply a report with STATE, ID, KIND, PROGRESS, APP, TITLE and MSG."
  (ghostel--osc-program-status state id kind progress app title msg))

(ert-deftest ghostel-status-test-report-replaces-record ()
  "A report replaces its record completely and the hook sees old and new."
  (ghostel-status-test--with-log log
    (ghostel-status-test--report "working" nil nil 40 "cargo")
    (should (equal '(("" :state working :progress 40 :app "cargo"))
                   ghostel-program-status))
    (ghostel-status-test--report "done")
    (should (equal '(("" :state done)) ghostel-program-status))
    (should (equal '(("" (:state working :progress 40 :app "cargo") (:state done))
                     ("" nil (:state working :progress 40 :app "cargo")))
                   log))))

(ert-deftest ghostel-status-test-unknown-state-ignored ()
  "A state the module does not know leaves the store alone."
  (ghostel-status-test--with-log log
    (ghostel-status-test--report "bogus")
    (should-not ghostel-program-status)
    (should-not log)))

(ert-deftest ghostel-status-test-clear-removes-subtree ()
  "`clear' removes the record and its children; without an id, every record."
  (ghostel-status-test--with-log log
    (ghostel-status-test--report "working")
    (ghostel-status-test--report "working" "deploy")
    (ghostel-status-test--report "blocked" "deploy/eu" "permission")
    (ghostel-status-test--report "working" "deployer")
    (setq log nil)
    (ghostel-status-test--report "clear" "deploy")
    (should (equal '("deployer" "") (mapcar #'car ghostel-program-status)))
    (should (equal '(("deploy" (:state working) nil)
                     ("deploy/eu" (:state blocked :kind permission) nil))
                   log))
    (ghostel-status-test--report "clear")
    (should-not ghostel-program-status)))

(ert-deftest ghostel-status-test-record-cap-evicts-oldest ()
  "Past 256 records the least recently updated one is evicted."
  (ghostel-status-test--with-log log
    (dotimes (i 257)
      (ghostel-status-test--report "idle" (format "r%d" i)))
    (should (= 256 (length ghostel-program-status)))
    (should-not (assoc "r0" ghostel-program-status))
    (should (member '("r0" (:state idle) nil) log))))

(ert-deftest ghostel-status-test-drop-running-keeps-finished ()
  "A new prompt or exit drops working and blocked records, keeps done and error."
  (ghostel-status-test--with-log log
    (ghostel-status-test--report "working" "a")
    (ghostel-status-test--report "blocked" "b")
    (ghostel-status-test--report "done" "c")
    (ghostel-status-test--report "error" "d")
    (ghostel-status--drop-running)
    (should (equal '("d" "c") (mapcar #'car ghostel-program-status)))))

(ert-deftest ghostel-status-test-input-acknowledges-finished ()
  "User input drops done and error records unless the option is off."
  (ghostel-status-test--with-log log
    (ghostel-status-test--report "done" "c")
    (ghostel-status-test--report "working" "a")
    (let ((ghostel-status-acknowledge-on-input nil))
      (ghostel-status--acknowledge)
      (should (= 2 (length ghostel-program-status))))
    (ghostel-status--acknowledge)
    (should (equal '("a") (mapcar #'car ghostel-program-status)))))

(ert-deftest ghostel-status-test-osc9-progress-maps-to-root ()
  "OSC 9;4 states become root record states."
  (ghostel-status-test--with-log log
    (pcase-dolist (`(,state ,progress ,record)
                   '(("set" 50 (:state working :progress 50))
                     ("set" nil (:state working :progress 0))
                     ("indeterminate" nil (:state working))
                     ("pause" 10 (:state blocked :progress 10))
                     ("error" 99 (:state error))))
      (ghostel--osc-progress state progress)
      (should (equal record (cdr (assoc "" ghostel-program-status)))))
    (ghostel--osc-progress "remove" nil)
    (should-not ghostel-program-status)
    (ghostel--osc-progress "bogus" 1)
    (should-not ghostel-program-status)))

(ert-deftest ghostel-status-test-osc9-progress-ignored-after-osc7501 ()
  "Once a program used OSC 7501, OSC 9;4 is ignored until a full reset."
  (ghostel-status-test--with-log log
    (ghostel-status-test--report "working")
    (ghostel--osc-progress "set" 50)
    (should (equal '(:state working) (cdr (assoc "" ghostel-program-status))))
    (ghostel--osc-program-status "clear" nil nil nil nil nil nil)
    (ghostel--program-status-reset)
    (should-not ghostel-program-status)
    (ghostel--osc-progress "set" 50)
    (should (equal '(:state working :progress 50)
                   (cdr (assoc "" ghostel-program-status))))))

(defmacro ghostel-status-test--with-mode-line (&rest body)
  "Run BODY in a temp buffer with only the mode-line member and no spinner."
  (declare (indent 0))
  `(let ((ghostel-program-status-functions '(ghostel-status-mode-line))
         (ghostel-spinner-type nil))
     (with-temp-buffer ,@body)))

(ert-deftest ghostel-status-test-mode-line-labels ()
  "The root record shows as a bracketed label per state, the message as tooltip.
A percent sign is doubled so the mode line prints it."
  (ghostel-status-test--with-mode-line
    (pcase-dolist (`(,state ,kind ,progress ,text)
                   '(("working" nil 42 " [42%%]")
                     ("working" nil nil " [...]")
                     ("blocked" "permission" nil " [blocked: permission]")
                     ("blocked" nil nil " [blocked]")
                     ("done" nil nil " [done]")
                     ("error" nil nil " [error]")))
      (ghostel-status-test--report state nil kind progress)
      (should (equal text ghostel--mode-line-status))
      (should (equal (list text) mode-line-process))
      (should-not (get-text-property 0 'help-echo ghostel--mode-line-status)))
    (ghostel-status-test--report "blocked" nil nil nil nil nil "Need approval")
    (should (equal "Need approval"
                   (get-text-property 0 'help-echo ghostel--mode-line-status)))
    (ghostel-status-test--report "idle")
    (should (null mode-line-process))))

(ert-deftest ghostel-status-test-mode-line-blocked-child-overrides-root ()
  "Blocked and error records show over the root, counted, with a tooltip line each."
  (ghostel-status-test--with-mode-line
    (ghostel-status-test--report "working" nil nil 42 "deploy")
    (ghostel-status-test--report "blocked" "eu" "permission" nil nil nil "Approve eu")
    (should (equal " [blocked: permission]" ghostel--mode-line-status))
    (should (equal "Approve eu"
                   (get-text-property 0 'help-echo ghostel--mode-line-status)))
    (ghostel-status-test--report "error" "us")
    (should (equal " [error ×2]" ghostel--mode-line-status))
    ;; The error child without text names its inherited app.
    (should (equal "deploy\nApprove eu"
                   (get-text-property 0 'help-echo ghostel--mode-line-status)))
    (ghostel-status-test--report "done" "us")
    (should (equal " [blocked: permission]" ghostel--mode-line-status))
    (ghostel-status-test--report "clear" "eu")
    (should (equal " [42%%]" ghostel--mode-line-status))))

(ert-deftest ghostel-status-test-app-inherited-from-ancestors ()
  "A record without an app takes the nearest ancestor's, down to the root."
  (with-temp-buffer
    (let ((ghostel-program-status-functions nil))
      (ghostel-status-test--report "working" nil nil nil "root")
      (ghostel-status-test--report "working" "a/b" nil nil "mid")
      (ghostel-status-test--report "working" "a/b/c/d")
      (ghostel-status-test--report "working" "x")
      (should (equal "mid" (ghostel-status-app "a/b")))
      (should (equal "mid" (ghostel-status-app "a/b/c/d")))
      (should (equal "root" (ghostel-status-app "x")))
      (should (equal "root" (ghostel-status-app "missing/child")))
      (ghostel-status-test--report "clear")
      (should-not (ghostel-status-app "x")))))

(ert-deftest ghostel-status-test-mode-line-keeps-input-mode-tag ()
  "Status text composes with the input-mode tag in `mode-line-process'."
  (ghostel-status-test--with-mode-line
    (setq ghostel--mode-line-tag ":Line")
    (ghostel--mode-line-refresh)
    (ghostel--osc-progress "set" 42)
    (should (equal '(":Line" (" [42%%]")) mode-line-process))
    (ghostel--osc-progress "remove" nil)
    (should (equal ":Line" mode-line-process))))

(ert-deftest ghostel-status-test-spinner-while-working-without-percentage ()
  "Working without a percentage starts spinner.el once; anything else stops it."
  (let ((starts nil)
        (stops 0))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'spinner-start)
               (lambda (&rest args) (push args starts)))
              ((symbol-function 'spinner-stop)
               (lambda (&rest _) (cl-incf stops))))
      (let ((ghostel-program-status-functions '(ghostel-status-mode-line))
            (ghostel-status--spinner-available t)
            (ghostel-spinner-type 'progress-bar))
        (with-temp-buffer
          (setq ghostel--mode-line-tag ":Char")
          (ghostel--osc-progress "indeterminate" nil)
          (ghostel--osc-progress "indeterminate" nil)
          (should (equal '((progress-bar)) starts))
          (should ghostel--spinner-active)
          (should (equal '(":Char" spinner--mode-line-construct) mode-line-process))
          (ghostel--osc-progress "set" 75)
          (should (= 1 stops))
          (should-not ghostel--spinner-active)
          (should (equal '(":Char" (" [75%%]")) mode-line-process))
          (ghostel--osc-progress "remove" nil)
          (should (equal ":Char" mode-line-process)))))))

(ert-deftest ghostel-status-test-notify-on-transition-when-not-looked-at ()
  "Any record entering a notify state while unfocused notifies, once."
  (let ((calls nil)
        (focused nil))
    (cl-letf (((symbol-function 'ghostel--handle-notification)
               (lambda (title body) (push (cons title body) calls)))
              ((symbol-function 'ghostel--buffer-focused-p)
               (lambda (_buf) focused)))
      (let ((ghostel-program-status-functions '(ghostel-status-notify)))
        (with-temp-buffer
          (ghostel-status-test--report "working" nil nil nil "cargo")
          (should-not calls)
          (ghostel-status-test--report "blocked" nil "permission" nil "cargo" nil "Apply plan?")
          (ghostel-status-test--report "blocked" nil "permission" nil "cargo" nil "Apply plan?")
          (should (equal '(("cargo" . "Apply plan?")) calls))
          (ghostel-status-test--report "blocked" nil "question")
          (should (equal '(("cargo" . "Apply plan?")) calls))
          ;; The root's last report had no app, so the child inherits none.
          (ghostel-status-test--report "done" "child")
          (should (equal '(("" . "done") ("cargo" . "Apply plan?")) calls))
          (ghostel-status-test--report "done")
          (should (equal '(("" . "done") ("" . "done") ("cargo" . "Apply plan?"))
                         calls))
          (setq focused t)
          (ghostel-status-test--report "error")
          (should (= 3 (length calls)))
          (setq focused nil)
          (let ((ghostel-status-notify-states '(blocked)))
            (ghostel-status-test--report "done")
            (should (= 3 (length calls)))
            (ghostel-status-test--report "blocked" nil "auth")
            (should (equal '("" . "blocked: auth") (car calls)))))))))

(ert-deftest ghostel-status-test-root-title-precedes-osc2 ()
  "The root record's title shows as `ghostel-title' and falls back to OSC 2."
  (let ((ghostel-program-status-functions nil))
    (with-temp-buffer
      (ghostel--set-title "shell")
      (should (equal "shell" ghostel-title))
      (ghostel-status-test--report "working" nil nil nil nil "Deploying")
      (should (equal "Deploying" ghostel-title))
      (ghostel--set-title "zsh")
      (should (equal "Deploying" ghostel-title))
      (ghostel-status-test--report "clear")
      (should (equal "zsh" ghostel-title)))))

(ert-deftest ghostel-status-test-osc7501-report-reaches-store ()
  "An OSC 7501 report from the PTY becomes a record with decoded text."
  :tags '(native)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test--with-raw-echo-buffer (buf proc)
      (let ((ghostel-program-status-functions nil))
        (ghostel--write-vt
         ghostel--term
         (format "\e]7501;state=blocked:kind=permission:app=terraform:progress=40:title=%s:msg=%s\e\\"
                 (base64-encode-string (encode-coding-string "等待批准 ✓" 'utf-8) t)
                 (base64-encode-string "Apply plan?")))
        (ghostel-test--wait-until (lambda () ghostel-program-status) proc 5)
        (should (equal '(("" :state blocked :kind permission :progress 40
                          :app "terraform" :title "等待批准 ✓" :msg "Apply plan?"))
                       ghostel-program-status))
        ;; Text arrives decoded as UTF-8, not as raw bytes.
        (should (multibyte-string-p
                 (plist-get (cdr (assoc "" ghostel-program-status)) :title)))
        (ghostel--write-vt ghostel--term "\e]7501;state=clear\e\\")
        (ghostel-test--wait-until (lambda () (null ghostel-program-status)) proc 5)))))

(ert-deftest ghostel-status-test-full-reset-clears-records ()
  "RIS removes every record and lets OSC 9;4 drive the root record again."
  :tags '(native)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test--with-raw-echo-buffer (buf proc)
      (let ((ghostel-program-status-functions nil))
        (ghostel--write-vt ghostel--term "\e]7501;state=working\e\\")
        (ghostel-test--wait-until (lambda () ghostel-program-status) proc 5)
        (ghostel--write-vt ghostel--term "\ec")
        (ghostel-test--wait-until
         (lambda () (and (null ghostel-program-status)
                         (not ghostel-status--osc7501-seen)))
         proc 5)
        (ghostel--write-vt ghostel--term "\e]9;4;1;50\e\\")
        (ghostel-test--wait-until (lambda () ghostel-program-status) proc 5)
        (should (equal '(:state working :progress 50)
                       (cdr (assoc "" ghostel-program-status))))))))

(ert-deftest ghostel-status-test-prompt-start-drops-running ()
  "An OSC 133 A prompt drops a working record and keeps a done one."
  :tags '(native)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test--with-raw-echo-buffer (buf proc)
      (let ((ghostel-program-status-functions nil))
        (ghostel--write-vt ghostel--term
                           "\e]7501;state=working\e\\\e]7501;state=done:id=x\e\\")
        (ghostel-test--wait-until
         (lambda () (= 2 (length ghostel-program-status))) proc 5)
        (ghostel--write-vt ghostel--term "\e]133;A\e\\")
        (ghostel-test--wait-until
         (lambda () (= 1 (length ghostel-program-status))) proc 5)
        (should (equal '("x") (mapcar #'car ghostel-program-status)))))))

(ert-deftest ghostel-status-test-osc7501-query-reply ()
  "The support query, ST- or BEL-terminated, is answered with the states and kinds."
  :tags '(native)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test--with-exec-buffer
        (buf proc (ghostel-test--python)
             (list "-c" ghostel-test--pty-reply-probe-script
                   (ghostel-test--fixture-directory) "0.15"
                   (ghostel-test--hex-encode-string "\e]7501;?\e\\")
                   (ghostel-test--hex-encode-string "\e]7501;?\a")))
      ;; The reply ends with the query's own terminator.
      (cl-loop for terminator in '("\e\\" "\a")
               for i from 0
               do (should (equal (downcase
                                  (ghostel-test--hex-encode-string
                                   (concat "\e]7501;?:states=idle,working,done,blocked,error"
                                           ":kinds=permission,question,auth" terminator)))
                                 (ghostel-test--wait-for-pty-reply i proc 6)))))))

(provide 'ghostel-status-test)
;;; ghostel-status-test.el ends here
