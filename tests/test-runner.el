;; -*- lexical-binding: t; -*-
;;; test-runner.el --- Batch runner for ERT tests

(setq debug-on-error t)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-examples.el" dir)))
(ert-run-tests-batch-and-exit)
