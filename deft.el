;;; deft.el --- quickly browse, filter, and edit plain text notes

;; Copyright (C) 2011 Jason R. Blevins <jrblevin@sdf.org>

;; Author: Jason R. Blevins <jrblevin@sdf.org>
;; Keywords: plain text, notes, Simplenote, Notational Velocity
;; URL: http://jblevins.org/projects/deft/

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Deft is an Emacs mode for slicing and dicing directories of plain
;; text notes, inspired by Notational Velocity.

;; Deft has two basic modes of operation.  The first, and default, is
;; the file browser mode, which is similar in spirit to dired-mode in
;; that it presents the user with a list of files in the Deft
;; directory which can be browsed, opened, renamed, or deleted.

;; This list consists of the title of each file followed by a short
;; summary.  The title is taken to be the first line of the file and
;; the summary is extracted from the text that follows.  By default,
;; Deft lists all files in the Deft directory in terms of the last
;; modified date, from newest to oldest.

;; The second mode of operation is filtering.  The list of files can
;; be filtered or searched using a string, which will match either the
;; title and/or the body text.  To initiate a filter, simply press `l`
;; and begin typing.  Filtering happens interactively and the file
;; list is updated on the fly with all files that match the current
;; string.

;; Pressing TAB while filtering will trigger a completion.  If there
;; are matching files, the first file in the list will be opened.  If
;; there are no matching files, a new file will be created using the
;; search string as the title.  Pressing RET while filtering will
;; return to the file list for further manipulation or later
;; filtering.

;; Static filtering is also available by pressing `L`.  This is
;; sometimes useful on its own, and it may be preferable in some
;; situations, such as over slow connections or on older systems,
;; where interactive filtering performance is poor.

;; File Browser
;; ------------

;; * Use `n` and `p` to move down and up in the file list.
;; * Alternatively, `TAB` and `S-TAB` or the mouse can be used for
;;   navigation.
;; * Press `c` to create a new file and be prompted for the filename.
;; * Press `C` to quickly create a new file with an automatically
;;   generated filename.
;; * In both cases, if a filter string is currently applied it will
;;   be used as the title of the newly created file.
;; * Press `r` to rename a file or `d` to delete a file.
;; * Press `f` to find a file in the Deft directory using the
;;   minibuffer.
;; * If anything unusual happens, press `g` to refresh the file browser.

;; Filtering
;; ---------

;; * Press `l` to start or continue filtering.
;; * While filtering, press `TAB` to complete.
;; * Completion opens the first matching file, if possible, or creates
;;   a new file with the filter string as the title.
;; * Pressing `RET` while filtering returns to the file list with the
;;   current filter string applied.
;; * Filtering can be resumed by pressing `l` again.
;; * Press `C-l` at any time to clear the filter and show all files.
;; * On slower systems, press `L` instead for static filtering, which
;;   updates the only list once, when `RET` is pressed.

;; Customization
;; -------------

;; Customize the `deft' group to change the functionality.

;; By default, Deft looks for notes by searching for files with the
;; extension `.txt` in the `~/.deft` directory.  You can customize
;; both the file extension and the Deft directory by running
;; `M-x customize-group` and typing `deft`.  Alternatively, you can
;; configure them in your `.emacs` file:

;;     (setq deft-extension "txt")
;;     (setq deft-directory "~/Dropbox/notes")

;; You can also customize the major mode that Deft uses to edit files,
;; either through `M-x customize-group` or by adding something like
;; the following to your `.emacs` file:

;;     (setq deft-text-mode 'markdown-mode)

;; You can easily set up a global keyboard binding for Deft.  For
;; example, to bind it to F8, add the following code to your `.emacs`
;; file:

;;     (global-set-key [f8] 'deft)

;; Acknowledgements
;; ----------------

;; Thanks to Konstantinos Efstathiou for writing simplnote.el, from
;; which I borrowed liberally, and to Zachary Schneirov for writing
;; Notational Velocity, which I have never had the pleasure of using,
;; but whose functionality and spirit I wanted to bring to Emacs and
;; Linux.

;; History
;; -------

;; The initial version of Deft was written by Jason Blevins and
;; released on August 6, 2011.


;;; Code:

(require 'widget)

;; Customization

(defcustom deft-directory (expand-file-name "~/.deft/")
  "Deft directory."
  :type 'directory
  :safe 'stringp
  :group 'deft)

(defcustom deft-extension "txt"
  "Deft file extension."
  :type 'string
  :safe 'stringp
  :group 'deft)

(defcustom deft-text-mode 'text-mode
  "Default mode used for editing files."
  :type 'function
  :group 'deft)

;; Constants

(defconst deft-buffer "*Deft*"
  "Deft buffer name.")

(defconst deft-separator " --- "
  "Text used to separate file titles and summaries.")

(defconst deft-line-width 80
  "Total width of lines in file browser.")

;; Global variables

(defvar deft-mode-hook nil
  "Hook run when entering Deft mode.")

(defvar deft-filter-regexp nil
  "Current filter regexp used by Deft.")

(defvar deft-current-files nil
  "List of files matching current filter.")

(defvar deft-all-files nil
  "List of files matching current filter.")

(defvar deft-hash-contents nil
  "Hash containing complete cached file contents, keyed by filename.")

(defvar deft-hash-mtimes nil
  "Hash containing cached file modification times, keyed by filename.")

(defvar deft-hash-titles nil
  "Hash containing cached file titles, keyed by filename.")

(defvar deft-hash-summaries nil
  "Hash containing cached file summaries, keyed by filename.")

;; File processing

(defun deft-chomp (str)
  "Trim leading and trailing whitespace from STR."
  (let ((s str))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun deft-base-filename (file)
  "Strip the path and extension from filename FILE."
  (setq file (file-name-nondirectory file))
  (setq file (replace-regexp-in-string (concat "\." deft-extension "$") "" file)))

(defun deft-find-all-files ()
  "Return a list of all files in the Deft directory."
  (if (file-exists-p deft-directory)
      (let (files result)
        ;; List all files
        (setq files
              (directory-files deft-directory t
                               (concat "\." deft-extension "$") t))
        ;; Filter out files that are not readable or are directories
        (dolist (file files)
          (when (and (file-readable-p file)
                     (not (file-directory-p file)))
            (setq result (cons file result))))
        result)))

(defun deft-parse-title (contents)
  "Parse the given file CONTENTS and determine the title.
The title is taken to be the first non-empty line of a file."
  (let ((begin (string-match "^.+$" contents)))
    (when begin
      (substring contents begin (min (match-end 0)
                                 (+ begin deft-line-width))))))

(defun deft-parse-summary (contents title)
  "Parse the file CONTENTS, given the TITLE, and extract a summary.
The summary is a string extracted from the contents following the
title."
  (let* ((contents (replace-regexp-in-string "\n" " " contents))
         (begin (when title (string-match (regexp-quote title) contents)))
         (size (- deft-line-width (length deft-separator) (match-end 0))))
    (when begin
      (when (< 0 size)
        (setq contents (substring contents (match-end 0) (length contents)))
        (setq contents (deft-chomp contents))
        (substring contents 0 (min size (length contents)))))))

(defun deft-cache-file (file)
  "Update file cache if FILE exists."
  (when (file-exists-p file)
    (let ((mtime-cache (deft-file-mtime file))
          (mtime-file (nth 5 (file-attributes file))))
      (if (or (not mtime-cache)
              (time-less-p mtime-cache mtime-file))
          (deft-cache-newer-file file mtime-file)))))

(defun deft-cache-newer-file (file mtime)
  "Update cached information for FILE with given MTIME."
  ;; Modification time
  (puthash file mtime deft-hash-mtimes)
  (let (contents title)
    ;; Contents
    (with-current-buffer (get-buffer-create "*Deft temp*")
      (insert-file-contents file nil nil nil t)
      (setq contents (concat (buffer-string))))
    (puthash file contents deft-hash-contents)
    ;; Title
    (setq title (deft-parse-title contents))
    (puthash file title deft-hash-titles)
    ;; Summary
    (puthash file (deft-parse-summary contents title) deft-hash-summaries))
  (kill-buffer "*Deft temp*"))

(defun deft-file-newer-p (file1 file2)
  "Return non-nil if FILE1 was modified since FILE2 and nil otherwise."
  (let (time1 time2)
    (setq time1 (deft-file-mtime file1))
    (setq time2 (deft-file-mtime file2))
    (time-less-p time2 time1)))

(defun deft-cache-initialize ()
  "Initialize hash tables for caching files."
  (setq deft-hash-contents (make-hash-table :test 'equal))
  (setq deft-hash-mtimes (make-hash-table :test 'equal))
  (setq deft-hash-titles (make-hash-table :test 'equal))
  (setq deft-hash-summaries (make-hash-table :test 'equal)))

(defun deft-cache-update ()
  "Update cached file information."
  (setq deft-all-files (deft-find-all-files))             ; List all files
  (mapc 'deft-cache-file deft-all-files)                  ; Cache contents
  (setq deft-all-files (deft-sort-files deft-all-files))) ; Sort by mtime

;; Cache access

(defun deft-file-contents (file)
  "Retrieve complete contents of FILE from cache."
  (gethash file deft-hash-contents))

(defun deft-file-mtime (file)
  "Retrieve modified time of FILE from cache."
  (gethash file deft-hash-mtimes))

(defun deft-file-title (file)
  "Retrieve title of FILE from cache."
  (gethash file deft-hash-titles))

(defun deft-file-summary (file)
  "Retrieve summary of FILE from cache."
  (gethash file deft-hash-summaries))

;; File list display

(defun deft-print-header ()
  "Prints the *Deft* buffer header."
  (widget-insert "Deft")
  (when deft-filter-regexp
    (widget-insert (concat ": " deft-filter-regexp)))
  (widget-insert "\n\n"))

(defun deft-buffer-setup ()
  "Render the file browser in the *Deft* buffer."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (deft-print-header)

  ;; Print the files list
  (if (not (file-exists-p deft-directory))
      (widget-insert (deft-no-directory-message))
    (if deft-current-files
        (progn
          (mapc 'deft-file-widget deft-current-files))
      (widget-insert (deft-no-files-message))))

  (use-local-map deft-mode-map)
  (widget-setup)
  (goto-char 1)
  (forward-line 2))

(defun deft-file-widget (file)
  "Add a line to the file browser for the given FILE."
  (when file
    (let ((key (file-name-nondirectory file))
          (text (deft-file-contents file))
          (title (deft-file-title file))
          (summary (deft-file-summary file)))
      (widget-create 'link
                     :button-prefix ""
                     :button-suffix ""
                     :format "%[%v%]"
                     :tag file
                     :help-echo "Edit this file"
                     :notify (lambda (widget &rest ignore)
                               (deft-open-file (widget-get widget :tag)))
                     (or title "[Empty file]"))
      (when summary
        (widget-insert (propertize deft-separator 'face 'shadow))
        (widget-insert (propertize summary 'face 'shadow)))
      (widget-insert "\n"))))

(defun deft-refresh ()
  "Refresh the *Deft* buffer in the background."
  (interactive)
  (when (get-buffer deft-buffer)
    (set-buffer deft-buffer)
    (deft-cache-update)
    (deft-filter-update)
    (deft-buffer-setup)))

(defun deft-no-directory-message ()
  "Return a short message to display when the Deft directory does not exist."
  (concat "Directory " deft-directory " does not exist.\n"))

(defun deft-no-files-message ()
  "Return a short message to display if no files are found."
  (if deft-filter-regexp
      "No files match the current filter string.\n"
    "No files found."))

;; File list file management actions

(defun deft-open-file (file)
  "Open FILE in a new buffer and setting its mode."
  (prog1 (find-file file)
    (funcall deft-text-mode)
    (add-hook 'after-save-hook
              (lambda () (save-excursion (deft-refresh)))
              nil t)))

(defun deft-find-file (file)
  "Find FILE interactively using the minibuffer."
  (interactive "F")
  (deft-open-file file))

(defun deft-new-file-named (file)
  "Create a new file named FILE (or interactively prompt for a filename).
If the filter string is non-nil, use it as the title."
  (interactive "sNew filename (without extension): ")
  (setq file (concat (file-name-as-directory deft-directory)
                     file "." deft-extension))
  (if (file-exists-p file)
      (message (concat "Aborting, file already exists: " file))
    (when deft-filter-regexp
      (write-region deft-filter-regexp nil file nil))
    (deft-open-file file)))

(defun deft-new-file ()
  "Create a new file quickly, with an automatically generated filename.
If the filter string is non-nil, use it as the title."
  (interactive)
  (let (fmt filename counter temp-buffer)
    (setq counter 0)
    (setq fmt (concat "deft-%d." deft-extension))
    (setq filename (concat deft-directory (format fmt counter)))
    (while (or (file-exists-p filename)
               (get-file-buffer filename))
      (setq counter (1+ counter))
      (setq filename (concat deft-directory (format fmt counter))))
    (when deft-filter-regexp
      (write-region (concat deft-filter-regexp "\n\n") nil filename nil))
    (deft-open-file filename)
    (with-current-buffer (get-file-buffer filename)
      (end-of-buffer))))

(defun deft-delete-file ()
  "Delete the file represented by the widget at the point.
If the point is not on a file widget, do nothing.  Prompts before
proceeding."
  (interactive)
  (let ((filename (widget-get (widget-at) :tag)))
    (when filename
      (when (y-or-n-p
             (concat "Delete file " (file-name-nondirectory filename) "? "))
        (delete-file filename)
        (delq filename deft-current-files)
        (delq filename deft-all-files)
        (deft-refresh)))))

(defun deft-rename-file ()
  "Rename the file represented by the widget at the point.
If the point is not on a file widget, do nothing."
  (interactive)
  (let (old-filename new-filename old-name new-name)
    (setq old-filename (widget-get (widget-at) :tag))
    (when old-filename
      (setq old-name (deft-base-filename old-filename))
      (setq new-name (read-string
                      (concat "Rename " old-name " to (without extension): ")))
      (setq new-filename
            (concat (file-name-as-directory deft-directory)
                    new-name "." deft-extension))
      (rename-file old-filename new-filename)
      (deft-refresh))))

;; File list filtering

(defun deft-sort-files (files)
  "Sort FILES in reverse order by modified time."
  (sort files '(lambda (f1 f2) (deft-file-newer-p f1 f2))))

(defun deft-filter-initialize ()
  "Initialize the filter string (nil) and files list (all files)."
  (interactive)
  (setq deft-filter-regexp nil)
  (setq deft-current-files deft-all-files))

(defun deft-filter-update ()
  "Update the filtered files list using the current filter regexp."
  (if (not deft-filter-regexp)
      (setq deft-current-files deft-all-files)
    (setq deft-current-files (mapcar 'deft-filter-match-file deft-all-files))
    (setq deft-current-files (delq nil deft-current-files))))

(defun deft-filter-match-file (file)
  "Return FILE if FILE matches the current filter regexp."
  (if (or (string-match deft-filter-regexp (deft-file-title file))
          (string-match deft-filter-regexp (deft-file-contents file)))
      file))

;; Filters that cause a refresh

(defun deft-filter-clear ()
  "Clear the current filter string and refresh the file browser."
  (interactive)
  (when deft-filter-regexp
    (setq deft-filter-regexp nil)
    (setq deft-current-files deft-all-files)
    (deft-refresh))
  (message "Filter cleared."))

(defun deft-filter-static (str)
  "Set the filter string to STR and update the file browser."
  (interactive "sFilter: ")
  (if (= (length str) 0)
      (setq deft-filter-regexp nil)
    (setq deft-filter-regexp str)
    (setq deft-current-files (mapcar 'deft-filter-match-file deft-all-files))
    (setq deft-current-files (delq nil deft-current-files)))
  (deft-refresh))

(defun deft-filter-increment (char)
  "Append character CHAR to the filter regexp and update `deft-current-files'."
  (setq char (char-to-string char))
  (setq deft-filter-regexp (concat deft-filter-regexp char))
  (setq deft-current-files (mapcar 'deft-filter-match-file deft-current-files))
  (setq deft-current-files (delq nil deft-current-files))
  (deft-refresh))

(defun deft-filter-decrement (char)
  "Remove character CHAR from the filter regexp and update `deft-current-files'."
  (if (> (length deft-filter-regexp) 1)
      (deft-filter-static (substring deft-filter-regexp 0 -1))
    (deft-filter-clear)))

(defun deft-filter ()
  "Interactively filter the files list."
  (interactive)
  (let (char exit)
    (while (not exit)
      ;; Read an additional character and handle it accordingly
      (setq char (read-char (concat "Deft (TAB to complete): " deft-filter-regexp)))
      (cond
       ;; Complete (open first file or create a new file) (TAB, C-i)
       ((char-equal char ?\t)
        (if (not deft-current-files)
            (deft-new-file)
          (deft-open-file (car deft-current-files)))
        (setq exit t))
       ;; Finish (RET, C-m)
       ((char-equal char ?\r)
        (setq exit t))
       ;; Clear filter (C-l)
       ((char-equal char ?\f)
        (deft-filter-clear)
        (setq exit t))
       ;; Backspace (remove the last character from the filter regexp)
       ((or (char-equal char ?\b) (char-equal char ?\d))
        (deft-filter-decrement char))
       ;; Default
       (t
        (deft-filter-increment char))))))

;;; Mode definition

(defun deft-setup ()
  "Prepare environment by creating the Deft notes directory."
  (interactive)
  (when (not (file-exists-p deft-directory))
    (make-directory deft-directory t))
  (deft-refresh))

(defvar deft-mode-map
  (let ((map (copy-keymap widget-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "f") 'deft-find-file)
    (define-key map (kbd "l") 'deft-filter)
    (define-key map (kbd "C-l") 'deft-filter-clear)
    (define-key map (kbd "g") 'deft-refresh)
    (define-key map (kbd "c") 'deft-new-file-named)
    (define-key map (kbd "C") 'deft-new-file)
    (define-key map (kbd "d") 'deft-delete-file)
    (define-key map (kbd "r") 'deft-rename-file)
    (define-key map (kbd "n") 'widget-forward)
    (define-key map (kbd "p") 'widget-backward)
    map)
  "Keymap for Deft mode.")

(defun deft-mode ()
  "Major mode for quickly browsing, filtering, and editing plain text notes.
Turning on `deft-mode' runs the hook `deft-mode-hook'.

\\{deft-mode-map}."
  (interactive)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq default-directory deft-directory)
  (use-local-map deft-mode-map)
  (deft-cache-initialize)
  (deft-cache-update)
  (deft-filter-initialize)
  (setq major-mode 'deft-mode)
  (setq mode-name "Deft")
  (deft-buffer-setup)
  (run-mode-hooks 'deft-mode-hook))

(put 'deft-mode 'mode-class 'special)

;;;###autoload
(defun deft ()
  "Switch to *Deft* buffer and load files."
  (interactive)
  (switch-to-buffer deft-buffer)
  (if (not (eq major-mode 'deft-mode))
      (deft-mode)))

(provide 'deft)

;;; deft.el ends here
