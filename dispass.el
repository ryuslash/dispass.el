;;; dispass.el --- Emacs wrapper for DisPass

;; Copyright (C) 2012 Tom Willemsen <tom@ryuslash.org>

;; Author: Tom Willemsen <tom@ryuslash.org>
;; Created: Jun 8, 2012
;; Version: 1.1.1
;; Keywords: processes
;; URL: http://ryuslash.org/projects/dispass.el.html

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the
;; above copyright notice and this permission notice appear in all
;; copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
;; CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
;; OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
;; NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; dispass.el is an emacs wrapper around DisPass
;; (http://dispass.babab.nl).  For more information see the README.org
;; and NEWS files.

;;; Code:
(defgroup dispass nil
  "Customization options for the DisPass wrapper."
  :group 'external)

(defcustom dispass-default-length 30
  "The default length to use when generating passphrases."
  :package-version '(dispass . "1")
  :group 'dispass
  :type '(integer))

(defcustom dispass-executable "dispass"
  "The location of the dispass executable."
  :package-version '(dispass . "0.1a7.3")
  :group 'dispass
  :type '(string)
  :risky t)

(defcustom dispass-labels-executable "dispass-label"
  "The location of the dispass-label executable."
  :package-version '(dispass . "1.1")
  :group 'dispass
  :type 'string
  :risky t)

(defvar dispass-labels-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "c" 'dispass-create)
    (define-key map "a" 'dispass-add-label)
    (define-key map "d" 'dispass-remove-label)
    map)
  "Keymap for `dispass-labels-mode', uses
  `tabulated-list-mode-map' as its parent.")

(defun dispass-process-sentinel (proc status)
  "Report PROC's status change to STATUS."
  (let ((status (substring status 0 -1))
        (buffer (process-buffer proc)))
    (unless (string-equal status "finished")
      (message "dispass %s" status))

    (unless (eq (current-buffer) proc)
      (kill-buffer buffer))))

(defun dispass-erase-buffer (buffer)
  "Completely erase the contents of BUFFER"
  (save-current-buffer
    (set-buffer buffer)
    (buffer-disable-undo buffer)
    (kill-buffer buffer)))

(defun dispass-label-at-point ()
  "When in `dispass-labels-mode', get the label at `point'."
  (let ((labels-mode-p (eq major-mode 'dispass-labels-mode)))
    (tabulated-list-get-id)))

(defun dispass-process-filter-for (label)
  "Create a function that will process any lines whilst keeping
an eye out for LABEL."
  `(lambda (proc string)
     "Process STRING coming from PROC."
     (cond ((string-match "^\\(Password[^:]*\\|Again\\): ?$" string)
            (process-send-string
             proc
             (concat (read-passwd
                      (concat (replace-regexp-in-string
                               "^[ \t\n]+\\|[ \t\n]+$" "" string) " ")
                      nil) "\n")))

           ((string-match (concat "^[ \t]*" ,label "[ \t]*\\(.+\\)$")
                          string)
            (let ((buffer (process-buffer proc)))
              (with-current-buffer buffer
                (insert (match-string 1 string))
                (clipboard-kill-ring-save (point-min) (point-max))
                (message "Password copied to clipboard.")))))))

(defun dispass-start-process (label create length)
  "Start dispass process.  When CREATE is non-nil send along the
  -c switch to make it ask for a password twice.  When LENGTH is
  an integer and greater than 0, send along the -l switch with
  LENGTH."
  (let ((args `("-o" ,label))
        proc)
    (when create
      (setq args (append '("-c") args)))

    (when (and (integerp length) (> length 0))
      (setq args (append `("-l" ,(number-to-string length)) args)))

    (setq proc (apply 'start-process "dispass" "*dispass*"
                      dispass-executable args))
    (set-process-sentinel proc 'dispass-process-sentinel)
    (set-process-filter proc (dispass-process-filter-for label))))

(defun dispass-get-labels ()
  "Get the list of labels and their information."
  (let ((result '()))
    (with-temp-buffer
      (insert (shell-command-to-string
               (concat dispass-labels-executable " -l --script")))
      (goto-char (point-min))
      (while (re-search-forward
              "^\\(\\(?:\\sw\\|\\s_\\)+\\) +\\([0-9]+\\) +\\(\\(?:\\sw\\|\\s_\\)+\\)"
              nil t)
        (let ((label (match-string 1))
              (length (match-string 2))
              (hashmethod (match-string 3)))
          (add-to-list 'result
                       (list label
                          `[(,label
                             face link
                             help-echo ,(concat "Generate passphrase for " label)
                             follow-link t
                             dispass-label ,label
                             dispass-length ,length
                             action dispass-from-button)
                            ,length
                            ,hashmethod])))))
    result))

;;;###autoload
(defun dispass-create (label &optional length)
  "Create a new password for LABEL."
  (interactive "MLabel: \nP")
  (let ((length (or length dispass-default-length)))
    (dispass-start-process label t length)))

;;;###autoload
(defun dispass (label &optional length)
  "Recreate a password previously used."
  (interactive (list
                (completing-read
                 "Label: " (mapcar (lambda (elm) (elt elm 0))
                                   (dispass-get-labels)))
                current-prefix-arg))
  (let ((length (or length dispass-default-length)))
    (dispass-start-process label nil length)))

;; Labels management
;;;###autoload
(defun dispass-add-label (label length hashtype)
  "Add LABEL with length LENGTH and hashtype HASHTYPE to DisPass."
  (interactive
   (list (read-from-minibuffer "Label: ")
         (read-from-minibuffer
          (format "Length (%d): " dispass-default-length) nil nil t nil
          (number-to-string dispass-default-length))
         (symbol-name (read-from-minibuffer
                       "Algorithm (dispass1): " nil nil t nil "dispass1"))))
  (shell-command
   (format "%s --add %s:%d:%s" dispass-labels-executable label length
           hashtype)))

;;;###autoload
(defun dispass-remove-label (label)
  "Remove LABEL from DisPass, if LABEL is not given
`tabulated-list-get-id' will be used to get the currently
pointed-at label. If neither LABEL is not found an error is
thrown."
  (interactive
   (list (or (dispass-label-at-point)
             (completing-read
              "Label: " (mapcar (lambda (elm) (elt elm 0))
                                (dispass-get-labels))))))

  (shell-command
   (format "%s --remove %s" dispass-labels-executable label)))

(defun dispass-from-button (button)
  "Call dispass with information from BUTTON."
  (dispass (button-get button 'dispass-label)
           (button-get button 'dispass-length)))

(defun dispass-labels--refresh ()
  "Reload labels from dispass."
  (setq tabulated-list-entries nil)

  (let ((tmp-list '()))
    (setq tabulated-list-entries (dispass-get-labels))))

(define-derived-mode dispass-labels-mode tabulated-list-mode "DisPass"
  "Major mode for listing dispass labels.

\\<dispass-labels-mode-map>
\\{dispass-labels-mode-map}"
  (setq tabulated-list-format [("Label" 30 t)
                               ("Length" 6 nil)
                               ("Hash" 0 t)]
        tabulated-list-sort-key '("Label" . nil))
  (add-hook 'tabulated-list-revert-hook 'dispass-labels--refresh)
  (tabulated-list-init-header))

;;;###autoload
(defun dispass-list-labels ()
  "Display a list of labels for dispass."
  (interactive)
  (let ((buffer (get-buffer-create "*DisPass Labels*")))
    (with-current-buffer buffer
      (dispass-labels-mode)
      (dispass-labels--refresh)
      (tabulated-list-print))
    (switch-to-buffer-other-window buffer))
  nil)

(provide 'dispass)

;;; dispass.el ends here
