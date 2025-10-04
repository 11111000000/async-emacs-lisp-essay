;; -*- lexical-binding: t; -*-
;;; test-examples.el --- ERT tests for async Emacs Lisp examples
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Peter Kosov <11111000000@email.com>
;; Copyright (c) 2025 Peter Kosov <11111000000@email.com>

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

;; Compatibility helpers for skipping
(unless (get 'ert-test-skipped 'error-conditions)
  (define-error 'ert-test-skipped "ERT test skipped"))
(unless (fboundp 'ert-skip)
  (defun ert-skip (&optional fmt &rest args)
    (signal 'ert-test-skipped (list (apply #'format (or fmt "skipped") args)))))

(defun test-wait-for (pred &optional timeout step)
  "Wait until PRED returns non-nil, with optional TIMEOUT (sec) and STEP."
  (let ((t0 (float-time))
        (timeout (or timeout 3.0))
        (step (or step 0.02)))
    (while (and (not (funcall pred))
                (< (- (float-time) t0) timeout))
      (accept-process-output nil step))
    (funcall pred)))

(defun test-network-available-p ()
  "Rough check for network; returns non-nil if HTTPS to example.org responds."
  (condition-case _
      (let ((ok nil) (done nil))
        (require 'url)
        (let ((url-request-timeout 2))
          (url-retrieve
           "https://example.org"
           (lambda (status)
             (setq ok (not (plist-get status :error)))
             (setq done t))))
        (test-wait-for (lambda () done) 3 0.05)
        ok)
    (error nil)))

(defmacro test-with-temp-dir (name &rest body)
  "Create a temporary directory bound to NAME and run BODY, then cleanup."
  (declare (indent 1))
  `(let* ((,name (make-temp-file "ex-dir-" t)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory ,name t)))))

(defun test-write-file (dir rel content)
  (let* ((path (expand-file-name rel dir)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert content))
    path))

(defun have-exec (name)
  (executable-find name))

;;; --- Section: make-process demo-echo
(ert-deftest ex-make-process-echo ()
  (let ((out "") (done nil))
    (make-process
     :name "demo-echo"
     :buffer nil
     :command (list (or (executable-find "printf") "/usr/bin/printf") "hi")
     :noquery t
     :connection-type 'pipe
     :coding 'utf-8
     :filter (lambda (_p s) (setq out (concat out s)))
     :sentinel (lambda (p _ev)
                 (when (memq (process-status p) '(exit signal))
                   (setq done t))))
    (should (test-wait-for (lambda () done)))
    (should (string-match-p "hi" out))))

;;; --- Section: timers run-at-time / run-with-idle-timer
(ert-deftest ex-run-at-time-once ()
  (let ((flag nil))
    (run-at-time 0.05 nil (lambda () (setq flag t)))
    (should (test-wait-for (lambda () flag) 1.0))))

(ert-deftest ex-run-with-idle-timer-once ()
  (when noninteractive
    (ert-skip "Idle timers are unreliable in --batch"))
  (let ((flag nil))
    (run-with-idle-timer 0.05 nil (lambda () (setq flag t)))
    (should (test-wait-for (lambda () flag) 1.0))))

;;; --- Section: threads UI hop
(ert-deftest ex-thread-ui-hop ()
  (let ((ui nil))
    (make-thread
     (lambda ()
       (sleep-for 0.05)
       (run-at-time 0 nil (lambda () (setq ui t)))))
    (should (test-wait-for (lambda () ui) 1.0))))

;;; --- Section: accept-process-output sequential wait
(ert-deftest ex-accept-process-output ()
  (let ((done nil))
    (make-process
     :name "echo2" :buffer nil
     :command (list (or (executable-find "printf") "/usr/bin/printf") "ok")
     :noquery t :connection-type 'pipe :coding 'utf-8
     :sentinel (lambda (_ _) (setq done t)))
    (while (not done)
      (accept-process-output nil 0.05))
    (should done)))

;;; --- Section: url-retrieve
(ert-deftest ex-url-retrieve-basic ()
  (unless (test-network-available-p)
    (ert-skip "Network not available"))
  (require 'url)
  (let ((done nil) (err nil) (len 0))
    (url-retrieve
     "https://example.org/"
     (lambda (status)
       (setq err (plist-get status :error))
       (unwind-protect
           (unless err
             (goto-char (point-min))
             (re-search-forward "\r?\n\r?\n" nil t)
             (setq len (- (point-max) (point))))
         (when (buffer-live-p (current-buffer))
           (kill-buffer (current-buffer))))
       (setq done t)))
    (should (test-wait-for (lambda () done) 5.0))
    (when err (ert-skip "HTTP failed: %S" err))
    (should (> len 0))))

;;; --- Section: async.el basic
(ert-deftest ex-async-start-42 ()
  (unless (require 'async nil t)
    (ert-skip "async not installed"))
  (let ((res nil))
    (async-start
     (lambda () (sleep-for 0.05) (* 6 7))
     (lambda (r) (setq res r)))
    (should (test-wait-for (lambda () res)))
    (should (eql res 42))))

;;; --- Quick start Recipe 1: ripgrep pipeline
(ert-deftest ex-recipe-rg-collect-output ()
  (unless (have-exec "rg")
    (ert-skip "ripgrep (rg) not found"))
  (test-with-temp-dir dir
                      (test-write-file dir "a.txt" "foo\nTODO here\nbar\n")
                      (let* ((buf (generate-new-buffer " *rg*"))
                             (old-rpom read-process-output-max)
                             (proc nil)
                             (done nil)
                             (timeout-timer nil)
                             (size 0))
                        (setq read-process-output-max (* 512 1024))
                        (unwind-protect
                            (progn
                              (setq proc
                                    (make-process
                                     :name "ripgrep"
                                     :buffer buf
                                     :command (list (executable-find "rg")
                                                    "--line-number" "--color" "never" "TODO" dir)
                                     :noquery t
                                     :connection-type 'pipe
                                     :coding 'utf-8
                                     :filter (lambda (_p chunk)
                                               (when (buffer-live-p buf)
                                                 (with-current-buffer buf
                                                   (setq buffer-undo-list t)
                                                   (goto-char (point-max))
                                                   (insert chunk))))
                                     :sentinel (lambda (p _ev)
                                                 (when (memq (process-status p) '(exit signal))
                                                   (setq done t)
                                                   (when (buffer-live-p buf)
                                                     (with-current-buffer buf
                                                       (setq size (buffer-size)))
                                                     (kill-buffer buf))
                                                   (setq read-process-output-max old-rpom)))))
                              (setq timeout-timer
                                    (run-at-time
                                     5 nil
                                     (lambda ()
                                       (unless done
                                         (when (process-live-p proc)
                                           (delete-process proc))))))

                              (should (test-wait-for (lambda () done) 5.0))
                              (should (> size 0)))
                          (setq read-process-output-max old-rpom)
                          (when timeout-timer (ignore-errors (cancel-timer timeout-timer)))
                          (when (and proc (process-live-p proc)) (ignore-errors (delete-process proc)))
                          (when (buffer-live-p buf) (kill-buffer buf))))))

;;; --- Recipe 3: periodic with coalescing via async
(ert-deftest ex-recipe-periodic-coalesce ()
  (unless (require 'async nil t)
    (ert-skip "async not installed"))
  (let ((running nil)
        (finished 0)
        (t0 (float-time)))
    (run-at-time
     0 0.02
     (lambda ()
       (when (not running)
         (setq running t)
         (async-start
          (lambda () (sleep-for 0.07) 'ok)
          (lambda (_)
            (setq running nil)
            (cl-incf finished))))))
    ;; Wait ~0.3s, we expect a small number of completions due to coalescing
    (while (< (- (float-time) t0) 0.35)
      (accept-process-output nil 0.02))
    (should (>= finished 1))
    (should (<= finished 5))))

;;; --- Backpressure trimming
(ert-deftest ex-backpressure-trim ()
  (let* ((buf (generate-new-buffer " *stream*"))
         (max-size (* 4 1024)) ; 4 KiB
         (old-rpom read-process-output-max)
         (trimmed nil)
         (done nil))
    (setq read-process-output-max (* 128 1024))
    (unwind-protect
        (progn
          (make-process
           :name "stream"
           :buffer buf
           :command (list (or (executable-find "bash") "/bin/bash")
                          "-lc"
                          "yes X | head -c 16384") ; ~16 KiB
           :noquery t
           :connection-type 'pipe
           :coding 'utf-8
           :filter (lambda (_p chunk)
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (setq buffer-undo-list t)
                         (goto-char (point-max))
                         (insert chunk)
                         (when (> (buffer-size) max-size)
                           (setq trimmed t)
                           (save-excursion
                             (goto-char (- (point-max) max-size))
                             (delete-region (point-min) (point)))))))
           :sentinel (lambda (p _ev)
                       (when (memq (process-status p) '(exit signal))
                         (setq done t)
                         (setq read-process-output-max old-rpom)
                         (when (buffer-live-p buf) (kill-buffer buf)))))
          (should (test-wait-for (lambda () done) 3.0))
          (should trimmed))
      (setq read-process-output-max old-rpom)
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;; --- Timers + external process (du)
(ert-deftest ex-idle-timer-du ()
  (unless (and (have-exec "du") (or (have-exec "bash") t))
    (ert-skip "du or shell not available"))
  (let ((running nil)
        (res nil)
        (done nil)
        (timer-fn (if noninteractive #'run-at-time #'run-with-idle-timer)))
    (funcall
     timer-fn
     0.05 nil
     (lambda ()
       (unless running
         (setq running t)
         (let* ((buf (generate-new-buffer " *du*")))
           (make-process
            :name "du"
            :buffer buf
            :command (list (or (executable-find "du") "du") "-sh" default-directory)
            :noquery t
            :connection-type 'pipe
            :coding 'utf-8
            :sentinel (lambda (p _)
                        (when (memq (process-status p) '(exit signal))
                          (when (buffer-live-p buf)
                            (with-current-buffer buf
                              (setq res (string-trim (buffer-string))))
                            (kill-buffer buf))
                          (run-at-time 0 nil (lambda () (setq done t)))
                          (setq running nil))))))))
    (should (test-wait-for (lambda () done) 3.0))
    (should (and (stringp res) (> (length res) 0)))))

;;; --- my-async-run-at-time wrapper
(ert-deftest ex-my-async-run-at-time ()
  (unless (require 'async nil t)
    (ert-skip "async not installed"))
  (cl-labels
      ((my-async-run-at-time
         (time repeat form &key callback coalesce)
         (let ((running nil)
               (timer nil))
           (setq timer
                 (run-at-time
                  time repeat
                  (lambda ()
                    (when (or (not coalesce) (not running))
                      (setq running t)
                      (condition-case _
                          (async-start
                           `(lambda ()
                              (condition-case err
                                  (progn ,form)
                                (error (cons :error (error-message-string err)))))
                           (lambda (res)
                             (setq running nil)
                             (when callback
                               (if (and (consp res) (eq (car res) :error))
                                   (funcall callback :error)
                                 (funcall callback res)))))
                        (error
                         (setq running nil)
                         (when callback (funcall callback :error))))))))
           (cons timer (lambda () (cancel-timer timer))))))
    ;; Normal run
    (let ((got nil))
      (my-async-run-at-time
       0.05 nil
       '(progn (sleep-for 0.02) (+ 3 4))
       :callback (lambda (r) (setq got r))
       :coalesce t)
      (should (test-wait-for (lambda () got) 2.0))
      (should (eql got 7)))
    ;; Cancellation
    (let ((called nil))
      (pcase-let ((`(,timer . ,cancel) (my-async-run-at-time
                                        1000 nil '(+ 1 2)
                                        :callback (lambda (_r) (setq called t))
                                        :coalesce t)))
        (funcall cancel)
        (ignore timer))
      (should-not (test-wait-for (lambda () called) 0.2)))))

;;; --- async-start-process uname
(ert-deftest ex-async-start-process-uname ()
  (unless (and (require 'async nil t) (have-exec "uname"))
    (ert-skip "async or uname not available"))
  (let ((done nil) (txt nil))
    (async-start-process
     "uname" (executable-find "uname")
     (lambda (obj)
       (let ((buf (cond
                   ((bufferp obj) obj)
                   ((processp obj) (process-buffer obj))
                   ((and (stringp obj) (get-buffer obj)) (get-buffer obj))
                   (t nil))))
         (unwind-protect
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq txt (string-trim (buffer-string)))))
           (when (buffer-live-p buf) (kill-buffer buf))))
       (setq done t))
     "-s")
    (should (test-wait-for (lambda () done) 5.0))
    (should (and (stringp txt) (> (length txt) 0)))))

;;; --- plz HTTP
(ert-deftest ex-plz-get-json ()
  (unless (and (require 'plz nil t) (have-exec "curl") (test-network-available-p))
    (ert-skip "plz/curl not installed or network not available"))
  (let ((done nil) (err nil) (origin nil))
    ;; Use :as 'string for broad plz compatibility; parse JSON ourselves.
    (plz 'get "https://httpbin.org/get"
      :as 'string
      :then (lambda (s)
              (let* ((json
                      (if (fboundp 'json-parse-string)
                          (json-parse-string s :object-type 'alist)
                        (let ((json-object-type 'alist)
                              (json-key-type 'symbol))
                          (json-read-from-string s)))))
                (setq origin (or (alist-get 'origin json)
                                 (alist-get "origin" json))))
              (setq done t))
      :else (lambda (_e) (setq err t) (setq done t)))
    (should (test-wait-for (lambda () done) 5.0))
    (when err (ert-skip "plz request failed"))
    (should (stringp origin))))

;;; --- Threads + mutex example (simplified)
(ert-deftest ex-threads-mutex ()
  (let ((mtx (make-mutex "demo"))
        (result nil))
    (make-thread
     (lambda ()
       (thread-yield)
       (mutex-lock mtx)
       (setq result "готово")
       (mutex-unlock mtx)
       (run-at-time 0 nil #'ignore)))
    (should (test-wait-for
             (lambda ()
               (let (r)
                 (mutex-lock mtx)
                 (setq r result)
                 (mutex-unlock mtx)
                 r))
             1.0))))

;;; --- External ping example is network-dependent; skip
(ert-deftest ex-ping-skip ()
  (ert-skip "ping example intentionally skipped in CI"))

;;; End of tests
(provide 'test-examples)
;;; test-examples.el ends here
