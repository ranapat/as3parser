(defvar as3parser-client-path "~/Projects/as3parser/client.py")
(defvar as3parser-server-path "~/Projects/as3parser/server.py --force &")

(defun as3parser-find-git-root ()
  (interactive)
  (setq git-root-folder "")
  (setq git-root (locate-dominating-file (file-name-as-directory (file-name-directory buffer-file-name)) ".git"))
  (if (= (length git-root) 0)
      (message "as3parser-find-git-root Git root not found!")
    (progn
      (message (concat "as3parser-find-git-root Git root located" git-root))
      (setq git-root-parts (delete "" (split-string git-root "/")))
      (dolist (line git-root-parts)
	(setq git-root-folder line))
      (message git-root-folder)))
  git-root-folder)

(defun as3parser-start-server ()
  "Starts the auto complete server."
  (interactive)
  (setq command as3parser-server-path)
  (shell-command command)
  (message (concat "as3parser-start-server Server Started!")))

(defun as3parser (mode)
  "Tries to autocomplete or remind"
  (interactive)
  (setq command (concat as3parser-client-path " " mode " " "\"" (replace-regexp-in-string "%" "#percent#" (replace-regexp-in-string "\r" "" (replace-regexp-in-string "\n" "" (replace-regexp-in-string "\"" "'" (thing-at-point 'line))))) "\"" " " "\"" (replace-regexp-in-string "%" "#percent#" (replace-regexp-in-string "\r" "" (replace-regexp-in-string "\n" "" (replace-regexp-in-string "\"" "'" (buffer-string))))) "\""))

  (setq result (shell-command-to-string command))

  (setq split (delete "" (split-string result "\n")))
  (setq type (pop split))

  (setq popup-items '())
  (dolist (line split)
    (setq split-line (delete "" (split-string line "@@@")))
    (setq to-show (pop split-line))
    (setq to-write (pop split-line))
    (add-to-list 'popup-items (popup-make-item to-show :value to-write)))

  (if (< 0 (length popup-items))
      (progn
	(setq to-complete (popup-menu* popup-items))
	(insert-before-markers to-complete))
    (message "as3parser-complete Nothing to complete!")))

(defun as3parser-complete ()
     "Tries to autocomplete"
     (interactive)
     (as3parser "complete"))
(global-set-key (kbd "C-c SPC") 'as3parser-complete)

(defun as3parser-remind ()
     "Tries to remind"
     (interactive)
     (as3parser "remind"))
(global-set-key (kbd "C-c C-SPC") 'as3parser-remind)

(defun as3parser-set-project (name)
  "Sets project scope. Current session cache is lost."
  (interactive "sProject name <leave empty for .git root>: ")
  (if (= (length name) 0)
      (progn
	(message "as3parser-set-project No project specified! Will use the current git project!")
	(setq name (as3parser-find-git-root)))
    (message "as3parser-set-project Project name passed!"))
  (setq command (concat as3parser-client-path " " "set-project" " " name))
  (shell-command command)
  (message (concat "as3parser-set-project Current project set to " name)))
(global-set-key (kbd "C-c C-p") 'as3parser-set-project)

(defun as3parser-load-from-cache ()
  "Loads class definition from the cache."
  (interactive)
  (setq command (concat as3parser-client-path " " "load-from-cache"))
  (shell-command command)
  (message "as3parser-load-from-cache Complete!"))
(global-set-key (kbd "C-c C-l") 'as3parser-load-from-cache)

(defun as3parser-save-to-cache ()
  "Saves class definition to the cache."
  (interactive)
  (setq command (concat as3parser-client-path " " "save-to-cache"))
  (shell-command command)
  (message "as3parser-save-to-cache Complete!"))
(global-set-key (kbd "C-c C-s") 'as3parser-save-to-cache)

(defun as3parser-reset-cache ()
  "Resets the session cache."
  (interactive)
  (setq command (concat as3parser-client-path " " "reset-cache"))
  (shell-command command)
  (message "as3parser-reset-cache Complete!"))
(global-set-key (kbd "C-c C-r") 'as3parser-reset-cache)

(defun as3parser-reset-class (name)
  "Removes class definition from the cache."
  (interactive "sClass name: ")
  (setq command (concat as3parser-client-path " " "reset-class" " " name))
  (shell-command command)
  (message "as3parser-reset-class Complete!"))
(global-set-key (kbd "C-c C-c") 'as3parser-reset-class)

(defun as3parser-load-from-file (name)
  "Loads class definition from a source file."
  (interactive "sFile name: ")
  (setq command (concat as3parser-client-path " " "load-from-file" " " name))
  (shell-command command)
  (message "as3parser-load-from-file Complete!"))
(global-set-key (kbd "C-c C-f") 'as3parser-load-from-file)

(defun as3parser-load-from-directory (name)
  "Loads class definition form a source directory."
  (interactive "sDirectory name: ")
  (setq command (concat as3parser-client-path " " "load-from-directory" " " name))
  (shell-command command)
  (message "as3parser-load-from-directory Complete!"))
(global-set-key (kbd "C-c C-d") 'as3parser-load-from-directory)

(defun as3parser-load-from-content ()
  "Loads class definition from a content."
  (interactive)
  (setq command (concat as3parser-client-path " " "load-from-content" " " "\"" (replace-regexp-in-string "%" "#percent#" (replace-regexp-in-string "\r" "" (replace-regexp-in-string "\n" "" (replace-regexp-in-string "\"" "'" (buffer-string))))) "\""))
  (shell-command command)
  (message "as3parser-load-from-content Complete!"))
(global-set-key (kbd "C-c C-b") 'as3parser-load-from-content)
