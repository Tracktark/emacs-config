;;; uva.el --- Emacs Client for the UVa Online Judge
;;; Commentary:

;;; Code:
(defvar uva-cache-directory "~/.cache/uvaonlinejudge/"
  "Cache directory for problems.")
(defvar uva-solution-directory "~/projects/java/onlinejudge/")

(defun uva--get-current-problem ()
  "Return currently visited problem."
  (and buffer-file-name
       (cond ((string-match (rx bol (literal (expand-file-name uva-solution-directory)) (group (+ digit)) ".java" eol)
                            buffer-file-name)
              (match-string 1 buffer-file-name))
             ((string-match (rx bol (literal (expand-file-name uva-cache-directory)) (group (+ digit)) "/" (+ nonl) eol)
                            buffer-file-name)
              (match-string 1 buffer-file-name)))))
    

(defun uva--get-path (type num)
  "Get path of TYPE for the NUM problem.
TYPE should be one of `directory', `pdf', `input', `output', `solution'."
  (let ((directory (concat uva-cache-directory num "/")))
    (cl-ecase type
      (directory directory)
      (pdf (concat directory "problem.pdf"))
      (input (concat directory "input"))
      (output (concat directory "output"))
      (solution (concat uva-solution-directory num ".java")))))

(defun uva--download-pdf (num)
  "Download NUM problem pdf."
  (make-directory (uva--get-path 'directory num) t)
  (let* ((url (format "https://onlinejudge.org/external/%s/%s.pdf" (substring num 0 -2) num))
         (command (format "curl -s -o %s '%s'" (uva--get-path 'pdf num) url)))
    (shell-command command)))

(defun uva--extract-samples (num)
  "Extract input and output samples from NUM pdf into files."
  (with-temp-buffer
    (shell-command (format "pdftotext -q -nopgbrk %s -" (uva--get-path 'pdf num)) (current-buffer))
    (goto-char (point-min))
    (re-search-forward "Sample Input")
    (forward-line)
    (let ((beg-of-input (point)))
        (re-search-forward "Sample Output")
        (beginning-of-line)
        (write-region beg-of-input (point) (uva--get-path 'input num)))
    (forward-line)
    (write-region (point) (point-max) (uva--get-path 'output num))))
    
(defun uva-find-pdf (num)
  "Download and display the pdf for problem NUM."
  (interactive "i")
  (setq num (or num (uva--get-current-problem)
                (completing-read "Problem Number: "
                                 (directory-files uva-cache-directory nil directory-files-no-dot-files-regexp))))
  (when (not (string-match (rx bol (>= 3 digit) eol) num))
    (user-error "Invalid Problem Number"))
  
  (let ((pdf-file (uva--get-path 'pdf num)))
    (if (file-exists-p pdf-file)
        (find-file pdf-file)
      (uva--download-pdf num)
      (uva--extract-samples num)
      (delete-other-windows)
      (find-file pdf-file)
      (select-window (split-window-right))
      (find-file (uva--get-path 'input num))
      (select-window (split-window-below))
      (find-file (uva--get-path 'output num)))))

(defun uva--compilation-finished (comp-buf status)
  "Function called when compilation finished ooga booga."
  (setq compilation-finish-functions (delete 'uva--compilation-finished compilation-finish-functions))
  (when (s-starts-with-p "finished" status)
    (quit-window t (get-buffer-window comp-buf))
    (diff (uva--get-path 'output (uva--get-current-problem)) "/tmp/uva-output")))

(defun uva-run ()
  "Run the current problem."
  (interactive)
  (let ((num (uva--get-current-problem)))
    (when (not num) (user-error "Not in a problem"))

    (if current-prefix-arg
        (compile (concat "java " (uva--get-path 'solution num)) t)
      (add-to-list 'compilation-finish-functions 'uva--compilation-finished)
      (compile (format "java %s < %s > /tmp/uva-output"
                       (uva--get-path 'solution num)
                       (uva--get-path 'input num))))))

(defun uva-run-interactive ()
  "Run problem in comint mode."
  (interactive)
  (let ((current-prefix-arg 4))
    (uva-run)))

(defun uva--edit-file (type num &optional create)
  "Edit TYPE file for problem NUM."
  (setq num (or num (uva--get-current-problem) (read-string "Problem Number: ")))
  (when (not (string-match (rx bol (>= 3 digit) eol) num))
    (user-error "Invalid Problem Number"))

  (let ((file-name (uva--get-path type num)))
    (when (and (not create) (not (file-exists-p file-name)))
      (user-error "File for problem doesn't exist"))
    (find-file file-name)))

(defun uva-edit-input ()
  "Edit input for current problem."
  (interactive)
  (uva--edit-file 'input nil))

(defun uva-edit-output ()
  "Edit output for current problem."
  (interactive)
  (uva--edit-file 'output nil))

(defun uva-find-solution ()
  "Edit solution for current problem."
  (interactive)
  (uva--edit-file 'solution nil t))

(provide 'uva)
;;; uva.el ends here
