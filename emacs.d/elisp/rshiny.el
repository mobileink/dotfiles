(defun shiny ()
  "run shiny R application in new shell buffer

  if there is displayed buffer that have shell it will use that window"
  (interactive)
  (let* ((R (concat "shiny::runApp('" default-directory "')"))
         (name "*shiny*")
         (new-buffer (get-buffer-create name))
         (script-proc-buffer
          (apply 'make-comint-in-buffer "script" new-buffer "R" nil `("-e" ,R)))
         (window (get-window-with-mode '(comint-mode eshell-mode)))
       (script-proc (get-buffer-process script-proc-buffer)))
    (if window
        (set-window-buffer window new-buffer)
      (switch-to-buffer-other-window new-buffer))))


(defun search-window-buffer (fn)
  "return first window for witch given function return non nil value"
  (let ((buffers (buffer-list))
        (value))
    (dolist (buffer buffers value)
      (let ((window (get-buffer-window buffer)))
        (if (and window (not value) (funcall fn buffer window))
            (setq value window))))))

(defun get-window-with-mode (modes)
  "return window with given major modes"
  (search-window-buffer (lambda (buff window)
                          (let ((mode (with-current-buffer buffer major-mode)))
                            (member mode modes)))))

