;;; dispass.el --- Generate and disperse/dispell passwords

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

;; dispass.el is an emacs wrapper around dispass
;; (http://dispass.babab.nl).

;;; Installation:

;; Place this file somewhere on your filesystem, either in your
;; `load-path' or somewhere else which you will have to add to your
;; `load-path', like so:

;;     (add-to-list 'load-path "/location/of/dispass.el")

;; And then `load', `require' or `autoload' it in your emacs init
;; file, for example:

;;     (require 'dispass)

;; _Or_ if you have package.el you could use `package-install-file'.

;;; Customization:

;; dispass.el only offers customization of the `dispass-executable'
;; and `dispass-file' variables for the moment.  This is the location
;; where the dispass executable is located.

;;; Usage:

;; Using dispass.el is simple, once installed.  Either call `dispass'
;; to recall a priviously generated password or call `dispass-create'
;; to generate a new password.

;; The only real difference between the two is that `dispass-create'
;; asks to confirm the password.  Both will ask for a label.

;; When a numeric argument is used when calling either
;; `dispass-create' or `dispass', that argument is sent to the dispass
;; program along with the -l switch.  This cuts the length of the
;; password to that many characters.  For example:

;;     C-5 M-x dispass<RET>

;; will generate a password of 5 characters for label "test" and
;; password "test".

;; Once a password has been generated it is inserted into the kill
;; ring and the system's clipboard so it can be easily inserted into
;; password field, this makes the generated password easy to see in
;; plaintext in the `kill-ring' variable, though.

;;; Change Log:

;; 0.1a7 - Initial release.

;; 0.1a7.1 - Don't show password, copy directly into the clipboard.

;; 0.1a7.2 - Kill buffer whenever we're finished with it.

;; 0.1a7.3 - Add the possility to cut passwords short by using a
;;           numeric prefix argument.

;;         - Add `dispass-executable' which holds the location of the
;;           dispass executable script.  It can be changed through the
;;           emacs customization interface.

;;         - Add a customization group named dispass, it is found
;;           under the "External" group.

;; 1 - Add `dispass-list-labels' which shows a list of all the labels
;;     in `dispass-file'.  Some management of labels is possible in
;;     this list, namely adding and deleting labels.

;;   - `dispass-create' will automatically add "created" labels to
;;     `dispass-file'.

;;   - Store a default length in `dispass-default-length'. When no
;;     length is given or found in the labels file, this length will
;;     be passed on to DisPass.

;;   - Fix the regular expression used in `dispass-process-filter-for'
;;     to support DisPass v0.1a8.

;; 1.1 - Use `dispass-label' to get a list of labels the user has
;;       made, use this for `dispass-list-labels' and adding
;;       completion options for `dispass'.

;; 1.1.1 - Clean up the output from dispass when prompting, trim
;;         whitespace from the front and end of the prompt.

;;       - Don't have the filter try and kill the buffer, since the
;;         sentinal should handle this.

;;       - Don't call `dispass-add-label' in `dispass-create'.  Let
;;         DisPass handle it.

;;       - Use `shell-command-to-string' in combination with `insert'
;;         to prevent a random window from being opened.

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

(defcustom dispass-file "~/.dispass"
  "The location of your dispass file."
  :package-version '(dispass . "1")
  :group 'dispass
  :type '(file))

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
  "Add LABEL with length LENGTH and hashtype HASHTYPE to `dispass-file'."
  (interactive "MLabel: \nnLength: \nMHash: ")
  (with-temp-buffer
    (insert (format "%s length=%d hash=%s\n" label length hashtype))
    (append-to-file (point-min) (point-max) dispass-file))
  (when (eq major-mode 'dispass-labels-mode)
    (revert-buffer)))

(defun dispass-remove-label (&optional label)
  "Remove LABEL from `dispass-file', if LABEL is not given
`tabulated-list-get-id' will be used to get the currently
pointed-at label. If neither LABEL is not found an error is
thrown."
  (interactive)
  (let* ((labels-mode-p (eq major-mode 'dispass-labels-mode))
         (label (or label (when labels-mode-p (tabulated-list-get-id)))))
    (unless label
      (error
       "LABEL required or must be called from `dispass-labels-mode'."))

    (with-temp-buffer
      (insert-file-contents dispass-file)
      (when (re-search-forward (concat "^" label) nil t)
        (kill-whole-line)
        (write-file dispass-file)))

    (when labels-mode-p
      (revert-buffer))))

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
