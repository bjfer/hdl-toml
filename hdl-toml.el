;;; hdl-toml.el --- hdl toml file -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Bruno Fernandes
;; Maintainer: Bruno Fernandes <br.fernandes@pm.me>
;; URL: https://github.com/bjfer/hdl-toml
;; Keywords: vhdl, toml, rust_hdl

 ;;; Commentary:
 ;;; Functions to create and maintain toml files used by the rust_hdl language server 

   			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst hdl~toml-name "vhdl_ls.toml"
  "The name of the toml file used by rust hdl.")

(defconst hdl~toml-reference (concat "# TOML file for rust hdl https://github.com/VHDL-LS/rust_hdl\n"
   				     "# Generated with hdl-toml https://github.com/bjfer/hdl-toml\n")
  "Information in toml's header.")

(defconst hdl~toml-home-filename (concat (getenv "HOME") "/." hdl~toml-name)
  "Home folder for .vhdl_ls.toml.")

(defconst hdl~toml-custom-filename "VHDL_LS_CONFIG"
  "Environment variable with file name for toml file.")

(defconst hdl~toml-vhdl-standards '("2008" "1993" "2019")
  "VHDL revisions supported by the rust hdl server.")

   			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup hdl-toml nil
  "hdl-toml commands to generate toml files for rust hdl"
  :prefix "hdl~")

(defcustom hdl~toml-single-file nil
  "All projects are in single toml file. See variable hdl~toml-default-toml."
  :type 'boolean
  :group 'hdl-toml)

(defcustom hdl~toml-custom-file nil
  (concat "When t, add projects to file name define in environment variable " hdl~toml-custom-filename". If nil, add projects to $HOME/." hdl~toml-name ". See variable hdl~toml-single-file.")
  :type 'boolean
  :group 'hdl-toml)

(defcustom hdl~toml-ask-regexp nil
  "Ask per project to use regexp"
  :type 'boolean
  :group 'hdl-toml)

(defcustom hdl~toml-default-regexp t
  "Use regexp for sources by default"
  :type 'boolean
  :group 'hdl-toml)

(defcustom hdl~toml-ask-relative nil
  "Ask per project to use relative"
  :type 'boolean
  :group 'hdl-toml)

(defcustom hdl~toml-default-relative t
  "Use relative paths for sources by default"
  :type 'boolean
  :group 'hdl-toml)

(defcustom hdl~toml-ask-standard nil
  "Ask per project which vhdl standard to use in toml file"
  :type 'boolean
  :group 'hdl-toml)

(defcustom hdl~toml-default-standard "2008"
  "Default VHDL revision to use by the server for parsing and analysis."
  :type 'string
  :options hdl~toml-vhdl-standards
  :group 'hdl-toml
  )

(defvar vhdl~proj-def nil
  "VHDL project definition for hdl toml")

(defvar hdl~entry-limit nil
  "Limit of current project entry being edited")

(defun hdl~toml-default-file ()
  "Environment variable with file name for toml file."
  (and hdl~toml-single-file (or (and hdl~toml-custom-file (getenv hdl~toml-custom-filename))
   				hdl~toml-home-filename)))
   			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VHDL proj def functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bjf/cleanup-name (name)
  (replace-regexp-in-string "\s+" "_" (string-trim name)))

(defun setq--vhdl-proj-def (project-folder &optional toml-file custom-proj-name)
  "Get full definition of vhdl files in a project. If file-or dir is not under version control,
   					   then treat the path as the project folder. Custom toml path should be an absolute path"
  (let ((project-folder (file-name-as-directory (expand-file-name project-folder))))
    (setq vhdl~proj-def
   	  ;; project name, no whitespace allowed
   	  (list
   	   (bjf/cleanup-name (or custom-proj-name
   				 (file-name-nondirectory
   				  (directory-file-name project-folder))))
   	   ;; project home folder
   	   project-folder
   	   ;; toml file
   	   (or toml-file (concat project-folder hdl~toml-name))
   	   ;; relative paths
   	   (directory-files-recursively
   	    project-folder "^[^.a-Z].*vhd$" t
   	    (lambda (arg) (not (string-prefix-p "." (file-name-nondirectory (directory-file-name arg))))))))))

(defun vhdl--proj-name () "Returns vhdl project name" (car vhdl~proj-def))
(defun vhdl--proj-path () "Returns vhdl project path" (cadr vhdl~proj-def))
(defun vhdl--toml-file () "Returns project toml file" (caddr vhdl~proj-def))
(defun vhdl--toml-path () "Returns project toml path" (file-name-directory (vhdl--toml-file)))
(defun vhdl--proj-files () "Returns vhdl project files" (cadddr vhdl~proj-def))

(defun hdl-toml-relative () (or (and hdl~toml-ask-relative (y-or-n-p "Use relative paths?"))
   				(and hdl~toml-default-relative (not hdl~toml-ask-relative))))
(defun hdl-toml-regexp () (or (and hdl~toml-ask-regexp (y-or-n-p "Use regexp paths?"))
   			      (and hdl~toml-default-regexp (not hdl~toml-ask-regexp))))
(defun hdl-toml-standard () (or (and hdl~toml-ask-standard
				     (completing-read "VHDL standard: " hdl~toml-vhdl-standards nil t))
				hdl~toml-default-standard))


(defun bjf/get-vhdl-proj-rfiles ()
  "Returns vhdl project files path relative to the project path"
  (mapcar (lambda (arg) (file-relative-name arg (vhdl--toml-path)))
   	  (vhdl--proj-files)))

(defun bjf/get-uniq-vhdl-folders (&optional use-relative-paths)
  "Return list of folders with vhdl sources"
  (seq-uniq (mapcar 'file-name-directory
   		    (if use-relative-paths
   			(bjf/get-vhdl-proj-rfiles)
   		      (vhdl--proj-files)))))

(defun bjf/get-vhdl-files (&optional use-folders use-relative-paths)
  (cond (use-folders (bjf/get-uniq-vhdl-folders use-relative-paths))
   	(use-relative-paths (bjf/get-vhdl-proj-rfiles))
   	(t (vhdl--proj-files))))

(defun bjf/init-vhdl-toml (&optional vhdl-std)
  (insert (concat hdl~toml-reference "\n"))
  (insert (concat "standard = " (prin1-to-string (or vhdl-std "2008")) "\n\n[libraries]\n\n")))

(defun bjf/vhdl-sources (&optional use-folders use-relative-path)
  (let ((file-termination (concat (when use-folders "*.vhd") "',\n"))
   	(vhdl-sources (bjf/get-vhdl-files use-folders use-relative-path))
   	(result))
    (dolist (source vhdl-sources result)
      (setq result (cons (concat  "'" source file-termination) result)))))

(defun bjf/print-vhdl-sources (&optional use-folders use-relative-path)
  (insert (concat "# " (vhdl--proj-path) "\n"))
  (mapcar 'insert (bjf/vhdl-sources use-folders use-relative-path))
  (save-current-buffer))

(defun bjf/write-vhdl-proj-toml (&optional use-folders use-relative-paths)
  (if (not (vhdl--proj-files))
      (message (concat "No vhdl file for project " (vhdl--proj-path)))
    (progn
      (insert (concat (vhdl--proj-name) ".files = [\n"))
      (bjf/print-vhdl-sources use-folders use-relative-paths)
      (insert "]\n\n"))))

(defun bjf/new-toml-file (&optional use-folders use-relative-paths vhdl-std)
  (with-temp-file (vhdl--toml-file)
    ;; in case the buffer already exists
    (bjf/init-vhdl-toml)
    (bjf/write-vhdl-proj-toml use-folders use-relative-paths)))

(defun bjf/get-toml-entries (target-toml)
  (with-current-buffer (find-file-noselect target-toml)
    (let (matches)
      (save-excursion
   	(beginning-of-buffer)
   	(while (re-search-forward "\\(\\w+\\)\.files" nil t)
   	  (push (match-string-no-properties 1) matches))
   	matches))))

(defun bjf/get-project-entries (target-toml project-name)
  (with-current-buffer (find-file-noselect target-toml)
    (save-excursion
      (bjf/found-toml-entry project-name)
      (let ((limit-entry (save-excursion (search-forward-regexp "^]")))
   	    (matches))
   	(while (re-search-forward "# \\(.*\\)$" limit-entry t)
   	  (print (match-string-no-properties 1))
   	  (push (match-string-no-properties 1) matches))
   	matches))))

(defun bjf/found-toml-entry (&optional vhdl-proj-name)
  (beginning-of-buffer)
  (search-forward (concat (or vhdl-proj-name (vhdl--proj-name)) ".files") nil t))

(defun bjf/found-proj-entry (&optional vhdl-proj-name vhdl-proj-entry)
  (bjf/found-toml-entry vhdl-proj-name)
  (search-forward (concat "# " (or vhdl-proj-entry (vhdl--proj-path))) nil t))

(defun bjf/update-toml-entry (&optional use-folders use-relative-paths)
  "Update entry. It assumes that found-toml-entry was run before"
  (kill-region (progn (beginning-of-line) (point)) (goto-char (1- (search-forward-regexp ",\s*\n\\(#\\|]\\)"))))
  (bjf/print-vhdl-sources use-folders use-relative-paths))

(defun bjf/append-toml-entry (&optional use-folders use-relative-paths vhdl-std)
  "Append/updates sources to toml entry. It assumes that found-toml-entry was run before"
  (let ((limit-entry (save-excursion (search-forward-regexp "^]"))))
    ;; sources already exist, update sources
    (if (search-forward (concat "# " (vhdl--proj-path)) limit-entry t)
   	(kill-region (progn (beginning-of-line) (point))
   		     (goto-char (1- (search-forward-regexp ",\s*\n\\(#\\|]\\)"))))
      (goto-char (1- limit-entry)))
    (bjf/print-vhdl-sources use-folders use-relative-paths)))

(defun bjf/read-add-vhdl-proj-toml (vhdl-folder &optional toml-file use-folders
						use-relative-paths vhdl-std custom-proj-name)
  "Init vhdl~proj-def varible and add project to a toml"
  (setq--vhdl-proj-def vhdl-folder toml-file custom-proj-name)
  (bjf/add-vhdl-proj-toml use-folders use-relative-paths vhdl-std))

(defun bjf/add-vhdl-proj-toml (&optional use-folders use-relative-paths vhdl-std)
  "Add project definition to an already existing toml file."
  (let ((target-file (vhdl--toml-file))
	(update nil)
	(custom nil))
    (if (not (file-exists-p target-file))
   	(bjf/new-toml-file use-folders use-relative-paths vhdl-std)
      (with-current-buffer (find-file-noselect target-file)
   	(and (setq update (bjf/found-toml-entry))
   	     (setq custom (not (y-or-n-p (concat "Entry " (vhdl--proj-name)
   				  " exists, update? (if no, a custom name must be provided)"))))
	     (setcar vhdl~proj-def (bjf/cleanup-name (read-string "Enter custom name: "))))
	(if (and update (not custom))
	    (progn
	    (bjf/found-proj-entry)
	    (bjf/update-toml-entry use-folders use-relative-paths))
   	  (progn
   	    (end-of-buffer)
   	    (bjf/write-vhdl-proj-toml use-folders use-relative-paths)))))))

(defun bjf/add-sources-to-proj (toml-file vhdl-proj-name &optional use-folders use-relative-paths vhdl-std)
  "Add sources to existing toml file."
  (if (not (file-exists-p toml-file))
      (and (y-or-n-p (concat "Target toml file " toml-file "does not exist, create?"))
   	   (bjf/new-toml-file use-folders use-relative-paths vhdl-std))
    (with-current-buffer (find-file-noselect toml-file)
      (if (bjf/found-toml-entry vhdl-proj-name)
   	  (bjf/append-toml-entry use-folders use-relative-paths)
   	(message (concat "Project " vhdl-proj-name " not found."))))))

(defun bjf/vhdl-projs-folder-toml (vhdl-projs-folder toml-file single-proj-name &optional use-folders use-relative-paths vhdl-std)
  (let ((single-proj-init))
    (dolist (vhdl-proj (directory-files vhdl-projs-folder t directory-files-no-dot-files-regexp))
      (when (file-directory-p vhdl-proj)
   	(setq--vhdl-proj-def vhdl-proj toml-file (and (not single-proj-init)
   						      single-proj-name))
   	;; custom-proj-name means it is a single proj
   	(if (and single-proj-init single-proj-name)
   	    (bjf/add-sources-to-proj (vhdl--toml-file) single-proj-name use-folders
   				     use-relative-paths vhdl-std)
   	  (bjf/add-vhdl-proj-toml use-folders use-relative-paths))
   	(setq single-proj-init t)))))

(defun bjf/current-vhdl-folder ()
  (or (and buffer-file-name
   	   (or (vc-git-root (buffer-file-name)) (file-name-directory buffer-file-name)))
      gnus-home-directory))

(defun bjf/request-vhdl-folder (&optional prompt)
  (read-directory-name (or prompt "VHDL top directory: ") (bjf/current-vhdl-folder) nil))

(defun bjf/request-toml-file ()
  (expand-file-name (or (hdl~toml-default-file)
   			(read-file-name "Target TOML file: " (bjf/current-vhdl-folder) nil nil hdl~toml-name))))

(defun bjf/delete-proj (toml-file vhdl-proj-name &optional vhdl-proj-entry)
  (with-current-buffer (find-file-noselect toml-file)
    (if (not vhdl-proj-entry)
   	(progn
   	  (bjf/found-toml-entry vhdl-proj-name)
   	  (kill-region (progn (beginning-of-line) (point))
   		       (search-forward "]")))
      (progn
   	(bjf/found-proj-entry vhdl-proj-name vhdl-proj-entry)
   	(kill-region (progn (beginning-of-line) (point))
   		     (goto-char (1- (search-forward-regexp ",\s*\n\\(#\\|]\\)"))))))
    (beginning-of-buffer)
    ;; clean up the file of empty vertical space
    (replace-regexp "^\n+" "\n")
    (save-current-buffer)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hdl-toml ()
  "Generate/update toml file for rust hdl language server."
  (interactive)
  (bjf/read-add-vhdl-proj-toml
   (bjf/request-vhdl-folder)
   (hdl~toml-default-file) (hdl-toml-regexp) (hdl-toml-relative) (hdl-toml-standard))
  (message (concat "Updated toml file " (vhdl--toml-file))))

(defun hdl-toml-add-sources ()
  "Add sources from a project to a toml project definition"
  (interactive)
  (let* ((toml-file (bjf/request-toml-file))
   	 (toml-entries (bjf/get-toml-entries toml-file))
   	 (vhdl-folder (read-directory-name "VHDL project to add : " (file-name-directory toml-file)))
 	 (project-name (if (= (seq-count #'stringp toml-entries) 1)
   			   (car toml-entries)
 			 (completing-read "Pick project to add sources:" toml-entries nil t))))
    ;; read project with new sources
    (setq--vhdl-proj-def vhdl-folder toml-file)
    (bjf/add-sources-to-proj
     toml-file
     ;; select project to add source
     project-name
     (hdl-toml-regexp)
     (hdl-toml-relative)
     (hdl-toml-standard))
    (message (concat "Added sources to project " project-name " in " (vhdl--toml-file)))))

(defun hdl-toml-update-sources ()
  "Update sources from a project in a toml project definition"
  (interactive)
  (let* ((toml-file (bjf/request-toml-file))
   	 (toml-entries (bjf/get-toml-entries toml-file))
   	 (toml-selected-project-entry (if (= (seq-count #'stringp toml-entries) 1)
   					  (car toml-entries)
   					(completing-read "Pick project to read sources:" toml-entries nil t)))
   	 (toml-project-entries (bjf/get-project-entries
   				toml-file
   				toml-selected-project-entry))
   	 (toml-project-sources-path (if (= (seq-count #'stringp toml-project-entries) 1)
   					(car toml-project-entries)
   				      (completing-read "Pick sources to update:" toml-project-entries nil t))))
    ;; read project with new sources
    (setq--vhdl-proj-def toml-project-sources-path toml-file)
    (with-current-buffer (find-file-noselect toml-file)
      (bjf/found-proj-entry toml-selected-project-entry toml-project-sources-path)
      (bjf/update-toml-entry (hdl-toml-regexp) (hdl-toml-relative)))
    (message (concat "Updated project " toml-selected-project-entry " sources in " (vhdl--toml-file)))))

(defun hdl-toml-add-folder ()
  "Add VHDL project folder to a toml file"
  (interactive)
  (let ((proj-folder (bjf/request-vhdl-folder)))
    (bjf/read-add-vhdl-proj-toml
     proj-folder
     (bjf/request-toml-file)
     (hdl-toml-regexp)
     (hdl-toml-relative)
     (hdl-toml-standard))
    (message (concat "Created project " (vhdl--proj-name) " in toml file " (vhdl--toml-file)))))

(defun hdl-toml-read-folder ()
  "Add all VHDL projects available in a folder to a single toml file"
  (interactive)
  (let ((projs-folder (bjf/request-vhdl-folder "Folder with all VHDL folder projects: "))
   	(single-proj (y-or-n-p "Single VHDL project?")))
    (bjf/vhdl-projs-folder-toml
     projs-folder
     (bjf/request-toml-file)
     (and single-proj (read-string "Enter custom project name: "))
     (hdl-toml-regexp)
     (hdl-toml-relative)
     (hdl-toml-standard))
    (message (concat "Updated toml file " (vhdl--toml-file)))))

(defun hdl-toml-delete ()
  "Delete a project definition or sources in a toml file."
  (interactive)
  (let* ((toml-file (bjf/request-toml-file))
   	 (toml-entries (bjf/get-toml-entries toml-file))
   	 (toml-selected-project-entry (if (= (seq-count #'stringp toml-entries) 1)
   					  (car toml-entries)
   					(completing-read "Pick project to delete:" toml-entries nil t))))
    (bjf/delete-proj toml-file
   		     toml-selected-project-entry
   		     (and (not (or (= (seq-count #'stringp toml-entries) 1) (y-or-n-p "Delete entire project?")))
   			  (completing-read "Pick sources to update:" (bjf/get-project-entries
   								      toml-file
   								      toml-selected-project-entry) nil t)))
    (message (concat "Deleted project (or sources) in toml file " (vhdl--toml-file)))))

(defun hdl-toml-bare ()
  "Generate/update toml file for rust hdl language server, asking for all options"
  (interactive)
  (let ((vhdl-proj-folder (bjf/request-vhdl-folder)))
    (bjf/read-add-vhdl-proj-toml
     vhdl-proj-folder
     (read-file-name "Target TOML file: " vhdl-proj-folder nil nil hdl~toml-name)
     (y-or-n-p "Use regexp for sources?")
     (y-or-n-p "Use relative paths?")
     (completing-read "VHDL standard:" hdl~toml-vhdl-standards nil t)
     (and (y-or-n-p "Use custom project name?")
 	  (read-string "Enter custom project name: ")))
    (message (concat "Updated toml file " (vhdl--toml-file)))))

(provide 'hdl-toml)
