;; -*- lexical-binding: t; -*-
;;; test-runner.el --- Batch runner for ERT tests

(setq debug-on-error t)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-examples.el" dir)))
;; Allow selecting tests via env var ERT or ERT_SELECTOR.
(let* ((sel-str (or (getenv "ERT_SELECTOR") (getenv "ERT")))
       (selector (if sel-str (car (read-from-string sel-str)) t)))
  (ert-run-tests-batch-and-exit selector))
