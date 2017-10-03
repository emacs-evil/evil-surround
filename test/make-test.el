(setq load-prefer-newer t)

(let ((current-directory (file-name-directory load-file-name)))
  (setq evil-surround-test-path (expand-file-name "." current-directory))
  (setq evil-surround-root-path (expand-file-name ".." current-directory)))

(add-to-list 'load-path evil-surround-root-path)
(add-to-list 'load-path evil-surround-test-path)

(load (concat (file-name-as-directory evil-surround-test-path) "evil-surround-test.el") nil t)

(ert-run-tests-batch-and-exit)
