;;; abas.el --- Tab management -*- lexical-binding: t; -*-

;; Tab management for emacs
;; Copyright (C) 2018 Renato Ferreira
;;
;; Author: Renato Ferreira <renatofdds at gmail dot com>
;; Maintainer: Renato Ferreira <renatofdds at gmail dot com>
;; Keywords: convenience
;; Version: 1.0

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides the abas-mode global minor mode to display a tab bar in
;; the header line of Emacs.
;;

;;; Code:

(require 'map)
(require 'seq)
(require 'rx)
(require 'cl-seq)

(eval-when-compile
	(require 'subr-x)
	(require 'cl-macs)
	(require 'url)
	)

(cl-eval-when (compile)
	(require 'tramp))


;;; Consts

(defconst abas--tl-format '(:eval (abas-tl-sync)))
(defconst abas--overview-group-name "-abas-groups-")
(defconst abas--fallback-group-name "···")
(defconst abas--tramp-file-regexp (rx bos "/" (+ (not (| ?/ ?:))) ?: (* (not (| ?/ ?:))) ?:))
(defconst abas--url-file-regexp (rx bos (| "file" "http" "https" "ftp" "nfs" "ssh" "scp" "rsync" "telnet") "://"))


;;; Options

(defgroup abas nil
  "Display a tab bar in the header line."
  :group 'convenience)

(defcustom abas-groups-matchers '()
  "Tab groups definitions."
  :group 'abas
  :type
  '(alist
		:key-type (string :tag "Group")
		:value-type
		(set
		 (group :inline t (const :tag "File name" :file-names) (repeat regexp))
		 (group :inline t (const :tag "Buffer name" :buffer-names) (repeat regexp))
		 (group :inline t (const :tag "Buffer mode" :modes) (repeat symbol)))))

(defvar abas-buffers-blacklist-regexp nil)

(defcustom abas-blacklist-functions '()
  "List of blacklist buffer predicates"
  :group 'abas
  :type '(repeat function))

(defcustom abas-buffers-blacklist '()
  "List of blacklisted buffers."
  :group 'abas
  :type '(repeat regexp)
	:set (lambda (var val)
         (set var val)
         (setq abas-buffers-blacklist-regexp
							 (rx-to-string (cons '| (mapcar (lambda (regex) (list 'regexp regex)) val))))))

(defcustom abas-modes-blacklist '(messages-buffer-mode ibuffer-mode speedbar-mode calc-mode calc-trail-mode ediff-mode erc-status-sidebar-mode helm-major-mode flycheck-error-list-mode treemacs-mode)
  "List of blacklisted major modes."
  :group 'abas
  :type '(repeat symbol))

(defcustom abas-project-modes '()
  "List of buffer modes belonging to projects."
  :group 'abas
  :type '(repeat (symbol :tag "Major mode")))

(defcustom abas-separator (cons " " (cons 'space '(:width (15) :height (40) :ascent (- (26)))))
  "Separator used between tabs."
  :group 'abas
  :type '(cons
					(string :tag "Label")
					(sexp :tag "Display")))


;;; Faces

(defgroup abas-faces nil
  "Faces used by abas-mode."
  :group 'abas)

(defface abas-default
  '((t :background "#000000" :height 102))
  "Default face in the tab bar."
  :group 'abas-faces)

(defface abas-tab
  '((t :inherit (abas-default variable-pitch) :foreground "#7C7C7C" :weight semi-bold))
  "Face for tabs."
  :group 'abas-faces)

(defface abas-selected
  '((t (:inherit abas-tab :foreground "#DCDCCC")))
  "Face for the selected tab."
  :group 'abas-faces)

(defface abas-modified
  '((t (:inherit abas-tab :foreground "#FF8C00")))
  "Face for unsaved tabs."
  :group 'abas-faces)

(defface abas-selected-modified
  '((t (:inherit abas-tab :foreground "#DCDCCC")))
  "Face for unsaved and selected tabs."
  :group 'abas-faces)

(defface abas-highlight
  '((t (:inherit abas-tab :foreground "#DCDCCC")))
  "Face to highlight a tab during mouse-overs."
  :group 'abas-faces)

(defface abas-separator
  '((t (:inherit abas-default)))
  "Face for separators between tabs."
  :group 'abas-faces)


;;; Variables

(defvar abas--scope 'buffers)
(defvar abas--tabs-store nil)
(defvar abas--groups-store nil)
(defvar abas--overview-group nil)
(defvar abas--flash-timer nil)
(defvar abas--separator nil)
(defvar abas--initialized nil)
(defvar-local abas--old-hlf nil)
(defvar-local abas--view nil)
(put 'abas--view 'permanent-local t)
(defvar-local abas-mode nil "Non-nil if Abas mode is enabled.
Use the command `abas-mode' to change this variable.")


;;; Helpers

(defmacro abas-buffer-lambda (&rest body)
  `(lambda (b)
		 (when (buffer-live-p b)
			 (with-current-buffer b
				 ,@body))))

(defun abas-buffer-project-name (buffer)
  (with-current-buffer buffer
		(and (fboundp 'projectile-project-name)
				 (or buffer-file-name
						 (memq major-mode abas-project-modes)
						 (bound-and-true-p magit-buffer-file-name))
				 (not (string= "-" (projectile-project-name)))
				 (concat "⌥ " (projectile-project-name)))))

(defun abas-buffer-group-names (buf)
  (or
	 (with-current-buffer buf
		 (cl-delete-duplicates
			(delq
			 nil
			 (let ((bn (buffer-name buf))
						 (fn (or buffer-file-name default-directory "")))
				 (append
					(list
					 (cond
						((string-match-p abas--tramp-file-regexp fn)
						 (let* ((vec (tramp-dissect-file-name fn))
										(method (tramp-file-name-method vec))
										(user-domain (tramp-file-name-user-domain vec))
										(host-port (tramp-file-name-host-port vec)))
							 (if (not (zerop (length user-domain)))
									 (format "%s@%s [%s]" user-domain host-port method)
								 (format "%s [%s]" host-port method))))
						((string-match-p abas--url-file-regexp fn)
						 (let ((url (url-generic-parse-url fn)))
							 (format "%s: %s" (url-type url) (url-host url))))
						((abas-buffer-project-name buf))))
					(map-keys
					 (map-filter
						(lambda (_ def)
							(or (memq major-mode (plist-get def :modes))
									(seq-some (lambda (regex) (string-match-p regex fn))
														(plist-get def :file-names))
									(seq-some (lambda (regex) (string-match-p regex bn))
														(plist-get def :buffer-names))))
						abas-groups-matchers)))))
			:test 'equal))
	 (list abas--fallback-group-name)))

(defun abas-buffers-group-names (buffers)
  (cl-loop
   for b in buffers
   for groups = (map-merge-with
								 'hash-table
								 #'append
								 groups
								 (seq-map (lambda (g) (list g b)) (abas-buffer-group-names b)))
   finally return (map-into groups 'list)))


;;; Stores

(defun abas-all (store)
  (map-values store))

(defun abas-get (store key)
  (gethash key store))

(defun abas-store (store key obj)
  (puthash key obj store))

(defun abas-delete (store key)
  (remhash key store))


;;; Tabs

(cl-defstruct (abas-tab (:constructor abas-tab-make))
  -target
	-pinned)

(define-inline abas-tab-store ()
  (inline-quote abas--tabs-store))

(cl-defgeneric abas-tab-store-key (obj)
  (:method ((obj abas-tab)) (abas-tab--target obj))
  (:method ((obj buffer)) obj))

(defun abas-tab-create (target &optional pinned)
  (abas-store (abas-tab-store)
							(abas-tab-store-key target)
							(abas-tab-make :-target target :-pinned pinned)))

(defun abas-tab-delete (obj)
  (abas-delete (abas-tab-store) (abas-tab-store-key obj)))

(define-inline abas-tab-of (obj)
  (inline-quote
   (abas-get (abas-tab-store) (abas-tab-store-key ,obj))))

(cl-defgeneric abas-tab-target-modified-p (tab target)
  )

(cl-defgeneric abas-tab-target-live-p (tab target)
  )

(cl-defgeneric abas-tab-switch-to-target (tab)
  )

(defun abas-tab-modified-p (tab)
  (abas-tab-target-modified-p tab (abas-tab--target tab)))

(defun abas-tab-live-p (tab)
  (abas-tab-target-live-p tab (abas-tab--target tab)))

(defun abas-tab-switch-to (tab)
  (abas-tab-switch-to-target (abas-tab--target tab)))


;;; Buffer Tabs

(defun abas-buffers ()
  (seq-filter #'bufferp (seq-map #'abas-tab--target (abas-all (abas-tab-store)))))

(defun abas-buffers-killed ()
  (seq-remove #'buffer-live-p (abas-buffers)))

(defun abas-buffer-tabs-prune-killed ()
  (seq-map #'abas-tab-delete (abas-buffers-killed)))

(cl-defgeneric abas-buffer-modified-p (buffer _mode)
  (buffer-modified-p buffer))

(cl-defmethod abas-tab-target-modified-p (_ (buf buffer))
  (abas-buffer-modified-p buf (buffer-local-value 'major-mode buf)))

(cl-defmethod abas-tab-target-live-p (_ (buffer buffer))
  (buffer-live-p buffer))

(cl-defmethod abas-tab-switch-to-target ((buffer buffer))
  (switch-to-buffer buffer))


;;; Groups

(cl-defstruct (abas-group (:constructor abas-group--make))
  name tabs)

(define-inline abas-group-store ()
  (inline-quote abas--groups-store))

(cl-defgeneric abas-group-store-key (obj)
  (:method ((obj abas-group)) (abas-group-name obj))
  (:method ((obj string)) obj))

(define-inline abas-groups ()
  (inline-quote (abas-all (abas-group-store))))

(cl-defmethod abas-tab-store-key ((group abas-group))
  (abas-group-name group))

(defun abas-group-tabs-sorted (group)
	(seq-map
	 #'cdr
	 (seq-sort-by
		#'car
		#'value<
		(seq-map-indexed
		 (lambda (tab i) (cons (* i (if (abas-tab--pinned tab) -1 1)) tab))
		 (abas-group-tabs group))))
	)

(defun abas-group-sort-tabs (g)
	(setf (abas-group-tabs g) (abas-group-tabs-sorted g)))

(defun abas-group-create (name &optional tabs)
	(let ((g (abas-group--make :name name :tabs tabs)))
		(abas-tab-create g)
		(abas-store (abas-group-store) name g)))

(defun abas-group-delete (group)
	(abas-tab-delete group)
  (abas-delete (abas-group-store) (abas-group-store-key group)))

(defun abas-group-of (obj)
  (abas-get (abas-group-store) (abas-group-store-key obj)))

(defun abas-group-has (group obj)
  (memq (abas-tab-of obj) (abas-group-tabs group)))

(defun abas-group-remove-tab (group obj)
  (setf (abas-group-tabs group)
				(remq obj (abas-group-tabs group)))
  (unless (abas-group-tabs group)
		(abas-group-delete group)))

(defun abas-group-invalidate (group)
  (when-let ((g (abas-group-of group)))
		(seq-do
		 (abas-buffer-lambda (abas-view-invalidate t))
		 (seq-map #'abas-tab--target (abas-group-tabs g)))))

(defun abas-group-tabs-left-of (group tab)
  (nreverse (cdr (memq tab (reverse (abas-group-tabs group))))))

(defun abas-group-tabs-right-of (group tab)
  (cdr (memq tab (abas-group-tabs group))))

(defun abas-group-add (group obj)
  (unless (abas-group-has group obj)
		(push (abas-tab-of obj) (abas-group-tabs group))))

(defun abas-groups-update (group-names-to-buffers-alist)
  (cl-loop
   for (group-name . buffers) in group-names-to-buffers-alist
   for group = (abas-group-of group-name)
   for tabs = (delq nil (seq-map #'abas-tab-of buffers))
   if (not group)
   collect (abas-group-create group-name tabs)
   else
   do (seq-do (lambda (tab) (abas-group-add group tab)) tabs)
   and collect group))

(defun abas-groups-digest (buffers)
  (abas-groups-update (abas-buffers-group-names buffers)))

(defun abas-groups-containing (obj)
  (seq-filter (lambda (g) (abas-group-has g obj)) (abas-groups)))

(defun abas-groups-siblings-for (buffer &optional unpinned)
  (cl-delete-duplicates
   (delq
		buffer
		(seq-map
		 #'abas-tab--target
		 (seq-filter
			(if unpinned (lambda (tab) (not (abas-tab--pinned tab))) #'always)
			(apply
			 #'append
			 (seq-map
				#'abas-group-tabs
				(abas-groups-containing buffer))))))))

(defun abas-groups-prune-killed ()
  (seq-doseq (g (map-values (abas-group-store)))
		(setf (abas-group-tabs g)
					(seq-filter
					 (lambda (tab) (and tab (buffer-live-p (abas-tab--target tab))))
					 (abas-group-tabs g)))
		(unless (abas-group-tabs g)
			(abas-group-delete g))))


;;; Group tabs

(cl-defmethod abas-tab-target-modified-p (_ (g abas-group))
  (seq-some #'abas-tab-modified-p (abas-group-tabs g)))

(cl-defmethod abas-tab-target-live-p (_ (g abas-group))
  (abas-group-tabs g))

(cl-defmethod abas-tab-switch-to-target ((g abas-group))
  (abas-tab-switch-to (car (abas-group-tabs g))))


;;; Group overview

(defun abas-overview-group ()
  (or abas--overview-group
			(let ((tabs (seq-map #'abas-tab-of (abas-groups))))
				(setq abas--overview-group
							(abas-group--make
							 :name abas--overview-group-name
							 :tabs tabs)))))

(defun abas-overview-group-invalidate ()
	(when abas--overview-group
		(let ((siblings (seq-map #'abas-tab--target
														 (apply #'append
																		(seq-map #'abas-group-tabs
																						 (seq-map #'abas-tab--target
																											(abas-group-tabs abas--overview-group)))))))
			(setq abas--overview-group nil)
			(seq-do
			 (abas-buffer-lambda (abas-view-invalidate))
			 siblings))))


;;; Scope

(define-inline abas-scope ()
  (inline-quote abas--scope))

(define-inline abas-scope-buffers-p ()
  (inline-quote (eq (abas-scope) 'buffers)))

(define-inline abas-scope-groups-p ()
  (inline-quote (eq (abas-scope) 'groups)))

(defun abas-scope-set (scope)
  (pcase scope
		('groups
		 (setf (abas-scope) 'groups)
		 (abas-overview-group-invalidate))
		('buffers
		 (setf (abas-scope) 'buffers)))
	(abas-view-invalidate t)
	(force-window-update (selected-window)))


;;; Buffer Local View

(cl-defstruct (abas-view (:constructor abas-view--make))
  -buffer
  -group-names
  -current-group-name
  -scroll-index
  -cached-tab-line)

(defun abas-view-create ()
	(abas-buffer-tabs-prune-killed)
	(abas-groups-prune-killed)
	(unless (abas-tab-of (current-buffer))
		(abas-tab-create (current-buffer)))
  (let ((group-names (seq-map #'abas-group-name (abas-groups-digest (list (current-buffer))))))
		(prog1 (setq abas--view
								 (abas-view--make
									:-buffer (current-buffer)
									:-group-names group-names
									:-current-group-name (car group-names)
									:-scroll-index nil
									:-cached-tab-line nil))
			(abas-view-invalidate-siblings t))))

(define-inline abas-view-ensure ()
  (inline-quote (or abas--view (abas-view-create))))

(define-inline abas-view-buffer ()
  (inline-quote (abas-view--buffer (abas-view-ensure))))

(define-inline abas-view-current-group-name ()
  (inline-quote (abas-view--current-group-name (abas-view-ensure))))

(define-inline abas-view-group-names ()
  (inline-quote (abas-view--group-names (abas-view-ensure))))

(define-inline abas-view-cache ()
  (inline-quote (abas-view--cached-tab-line (abas-view-ensure))))

(define-inline abas-view-scroll ()
  (inline-quote (abas-view--scroll-index (abas-view-ensure))))

(defun abas-view-buffer-tab ()
  (abas-tab-of (abas-view-buffer)))

(defun abas-view-current-group ()
  (when-let ((cgn (abas-view-current-group-name)))
		(abas-group-of cgn)))

(defun abas-view-current-group-tab ()
  (if-let ((cg (abas-view-current-group)))
			(abas-tab-of cg)
		(abas-view-buffer-tab)))

(defun abas-view-scope-group ()
  (if (abas-scope-buffers-p)
			(abas-view-current-group)
		(abas-overview-group)))

(defun abas-view-scope-current-tab ()
  (if (abas-scope-buffers-p)
			(abas-view-buffer-tab)
		(abas-view-current-group-tab)))

(defun abas-view-groups ()
  (seq-map #'abas-group-of (abas-view-group-names)))

(defun abas-view-scroll-by (count)
  (setf (abas-view-scroll) (+ (abas-view-scroll) count)))

(defun abas-view-tabs ()
	(if-let ((g (abas-view-scope-group)))
			(abas-group-tabs g)
		(list (abas-view-buffer-tab))))

(defun abas-view-tabs-from-scroll-index ()
  (nthcdr (abas-view-scroll) (abas-view-tabs)))

(defun abas-view-tabs-left-of-current ()
  (when-let (sg (abas-view-scope-group))
		(abas-group-tabs-left-of sg (abas-view-scope-current-tab))))

(defun abas-view-tabs-right-of-current ()
  (when-let (sg (abas-view-scope-group))
		(abas-group-tabs-right-of sg (abas-view-scope-current-tab))))

(defun abas-view-tabs-first ()
  (car (abas-view-tabs)))

(defun abas-view-tabs-last ()
  (car (last (abas-view-tabs))))

(defun abas-view-tab-next (&optional cycle)
  (or (car (abas-view-tabs-right-of-current))
			(and cycle (abas-view-tabs-first))))

(defun abas-view-tab-previous (&optional cycle)
  (or (car (last (abas-view-tabs-left-of-current)))
			(and cycle (abas-view-tabs-last))))

(defun abas-view-invalidate (&optional invalidate-position)
  (when abas--view
		(setf (abas-view-cache) nil)
		(when invalidate-position
			(setf (abas-view-scroll) nil))))

(defun abas-view-invalidate-siblings (&optional invalidate-position)
  (when-let ((siblings (abas-groups-siblings-for (abas-view-buffer))))
		(seq-do (abas-buffer-lambda (abas-view-invalidate invalidate-position))
						siblings)
		(force-mode-line-update t)))

(defun abas-view-overwrite (tabs)
	(when-let ((g (abas-view-scope-group)))
		(setf (abas-group-tabs g) tabs)
		(abas-view-invalidate-siblings t)))

(defun abas-view-delete ()
  (when-let ((tab (abas-view-buffer-tab)))
		(let* ((cg (abas-view-current-group))
					 (siblings (abas-groups-siblings-for (abas-view-buffer)))
					 (replace-with
						(when-let (cg (sibling-tab (or (car (abas-group-tabs-right-of cg tab))
																					 (car (last (abas-group-tabs-left-of cg tab)))))
													(sibling (abas-tab--target sibling-tab)))
							(when (buffer-live-p sibling) sibling))))
			(seq-doseq (g (abas-groups-containing tab))
				(abas-group-remove-tab g tab))
			(abas-tab-delete tab)
			(seq-do
			 (abas-buffer-lambda (abas-view-invalidate t))
			 siblings)
			(abas-overview-group-invalidate)
			(when replace-with
				(dolist (wnd (get-buffer-window-list))
					(let ((prev-buffers (window-prev-buffers wnd)))
						(set-window-prev-buffers
						 wnd
						 (cons
							(or (assoc replace-with prev-buffers)
									(let ((m (with-current-buffer replace-with
														 (point-marker))))
										(list replace-with m m)))
							(assq-delete-all replace-with prev-buffers)))))))))

(defun abas-view-track-renamed (new-name)
	(dolist (buf (let ((tn (abbreviate-file-name (file-truename new-name))))
								 (seq-filter
									(lambda (buf) (string= (buffer-local-value 'buffer-file-truename buf) tn))
									(buffer-list))))
		(when-let (((buffer-local-value 'abas--view buf)))
			(with-current-buffer buf
				(when-let* ((cgs (abas-view-group-names))
										(ngs (abas-buffer-group-names buf)))
					(cl-loop
					 for ng in (seq-difference ngs cgs) do
					 (abas-groups-update (list (cons ng (list buf)))))
					(cl-loop
					 with tab = (abas-tab-of buf)
					 for old-group in (seq-difference cgs ngs) do
					 (abas-group-remove-tab (abas-group-of old-group) tab)
					 (abas-group-invalidate old-group))
					(setf (abas-view-group-names) ngs)
					(unless (member (abas-view-current-group-name) ngs)
						(setf (abas-view-current-group-name) (car ngs)))
					(cl-loop
					 for g in ngs do
					 (abas-group-invalidate g))))))
	(force-window-update (selected-window)))

(defun abas-view-track-saved ()
	(abas-view-invalidate)
  (abas-view-invalidate-siblings))

(defun abas-view-track-dirty ()
  (abas-view-invalidate-siblings))

(defun abas-view-track-killed ()
  (when abas--view (abas-view-delete)))

(defun abas-view-track-clone-buffer ()
  (abas-view-create))


;;; Switch

(defun abas-switch-to (tab)
  (pcase (abas-tab--target tab)
		((and (cl-struct abas-group
										 (tabs (seq (cl-struct abas-tab (-target buf)))))
					(app abas-group-name group-name))
		 (with-current-buffer buf
			 (setf (abas-view-current-group-name) group-name)
			 (abas-view-invalidate t))
		 (abas-tab-switch-to-target buf))
		((and (pred bufferp) buf)
		 (when-let ((group-name (abas-view-current-group-name)))
			 (with-current-buffer buf
				 (when (abas-view-current-group-name)
					 (setf (abas-view-current-group-name) group-name))
				 (abas-view-invalidate t))
			 (abas-tab-switch-to tab)))))


;;; Mouse

(defun abas-mouse-select-tab (event)
  (interactive "@e")
  (let ((target (posn-string (event-start event))))
    (abas-switch-to
		 (get-text-property (cdr target) 'abas-tab (car target)))))

(defun abas-mouse-kill-tab (event)
  (interactive "@e")
  (let* ((target (posn-string (event-start event)))
				 (tab (get-text-property (cdr target) 'abas-tab (car target)))
				 (tab-target (abas-tab--target tab)))
    (cond
		 ((bufferp tab-target)
			(kill-buffer tab-target))
		 ((abas-group-p tab-target)
			(seq-do #'kill-buffer (seq-map #'abas-tab--target (abas-group-tabs tab-target)))))))


;;; Header line

(cl-defgeneric abas-tl-selected-target-p (target)
  (:method ((target buffer)) (eq target (current-buffer)))
  (:method ((target abas-group)) (eq (abas-group-name target) (abas-view-current-group-name))))

(defun abas-tl-separator ()
  (or abas--separator
			(setq abas--separator
						(propertize (car abas-separator)
												'face 'abas-separator
												'display (cdr abas-separator)))))

(defun abas-tl-label (tab)
  (format
   "%.70s"
   (if (bufferp (abas-tab--target tab))
			 (let ((bn (buffer-name (abas-tab--target tab))))
				 (if (> 80 (length bn))
						 bn
					 (concat (substring bn 0 30) "⋯" (substring bn -50 nil))))
		 (abas-group-name (abas-tab--target tab)))))

(defconst abas-tl-tab-keymap
	(define-keymap
		"<tab-line> <follow-link>" #'ignore
		"<tab-line> <mouse-1>" #'abas-mouse-select-tab
		"<tab-line> <mouse-2>" #'abas-mouse-kill-tab
		))

(defun abas-tl-tab (tab)
  (let ((is-selected (abas-tl-selected-target-p (abas-tab--target tab)))
				(is-modified (abas-tab-modified-p tab))
				(is-pinned (abas-tab--pinned tab)))
		(propertize
		 (abas-tl-label tab)
		 'abas-tab tab
		 'mouse-face 'abas-highlight
		 'pointer 'hand
		 'local-map abas-tl-tab-keymap
		 'help-echo "mouse-1: switch to\nmouse-2: kill"
		 'face (append
						(when is-pinned (list '(:underline t)))
						(list (cond
									 ((and is-selected is-modified) 'abas-selected-modified)
									 (is-selected 'abas-selected)
									 (is-modified 'abas-modified)
									 (t 'abas-tab)))))))

(defun abas-tl-view ()
	(when-let ((g (and (abas-scope-buffers-p) (abas-view-current-group))))
		(abas-group-sort-tabs g))
  (list
   (abas-tl-separator)
   (cl-loop
		for tab in (abas-view-tabs-from-scroll-index)
		when (abas-tab-live-p tab)
		collect (abas-tl-tab tab)
		and collect (abas-tl-separator))))

(defun abas-tl-tabs-visible-filter (tabs)
  (let ((v (abas-view-ensure)))
		(with-temp-buffer
			(let ((abas--view v)
						(inhibit-modification-hooks t)
						(left-fringe-width 0)
						(right-fringe-width 0)
						truncate-partial-width-windows
						truncate-lines)
				(insert
				 (format-mode-line
					(abas-tl-separator)))
				(seq-take-while
				 (lambda (tab)
					 (goto-char (point-max))
					 (insert (abas-tl-tab tab) (abas-tl-separator))
					 (goto-char (point-min))
					 (< (vertical-motion 1) 1))
				 tabs)))))

(defun abas-tl-sync-scroll ()
  (setf (abas-view-scroll) 0)
  (let ((tab (abas-view-scope-current-tab))
				(tabs (abas-view-tabs)))
		(when (memq tab tabs)
			(while (and (< (abas-view-scroll) (length tabs))
									(not (memq tab (abas-tl-tabs-visible-filter
																	(abas-view-tabs-from-scroll-index)))))
				(abas-view-scroll-by 1)))))

(defun abas-tl-sync (&optional force)
	(when (not (and (eq (abas-view-buffer) (current-buffer))
									(abas-view-buffer-tab)))
		(abas-view-create))
  (let ((use-cache-p (and (abas-view-cache)
													(not force))))
		(unless (abas-view-scroll)
			(abas-tl-sync-scroll))
		(if use-cache-p
				(abas-view-cache)
			(setf (abas-view-cache) (abas-tl-view)))))


;;; Commands

(defun abas-flash-groups (&optional time)
  (interactive)
  (when abas--flash-timer
		(cancel-timer abas--flash-timer))
  (unless (abas-scope-groups-p)
		(abas-scope-set 'groups))
  (setq
   abas--flash-timer
   (run-with-timer
		(or time 1) nil
		(lambda ()
			(setq abas--flash-timer nil)
			(unless (abas-scope-buffers-p)
				(abas-scope-set 'buffers))))))

(defun abas-select (n)
  (interactive "p")
  (when-let ((tab (nth n (abas-view-tabs))))
		(abas-switch-to tab)
		(when abas--flash-timer
			(abas-flash-groups))))

(defun abas-cycle (arg)
  (interactive "p")
  (let* ((f (if (> arg 0) #'abas-view-tab-next #'abas-view-tab-previous))
				 (tab (funcall f t)))
		(when tab
			(abas-switch-to tab)
			(when abas--flash-timer
				(abas-flash-groups)))))

(defun abas-backward ()
  (interactive)
  (abas-cycle -1))

(defun abas-forward ()
  (interactive)
  (abas-cycle 1))

(defun abas-backward-group ()
  (interactive)
  (abas-scope-set 'groups)
  (abas-cycle -1)
	(abas-flash-groups))

(defun abas-forward-group ()
  (interactive)
  (abas-scope-set 'groups)
  (abas-cycle 1)
	(abas-flash-groups))

(defun abas-kill-other-tabs ()
  (interactive)
  (seq-do #'kill-buffer (abas-groups-siblings-for (abas-view-buffer) t))
	(abas-view-invalidate t)
	(force-window-update (selected-window)))

(defun abas-kill-group ()
  (interactive)
  (abas-kill-other-tabs)
  (kill-buffer (current-buffer)))

(defun abas-toggle-pinned ()
  (interactive)
	(setf (abas-tab--pinned (abas-view-buffer-tab))
				(not (abas-tab--pinned (abas-view-buffer-tab))))
	(abas-view-invalidate t)
	(force-window-update (selected-window)))

(defun abas-move-left ()
  (interactive)
  (abas-view-overwrite
	 (delq
		nil
		(append
		 (butlast (abas-view-tabs-left-of-current))
		 (list (abas-view-scope-current-tab))
		 (last (abas-view-tabs-left-of-current))
		 (abas-view-tabs-right-of-current))))
	(abas-view-invalidate t)
	(force-window-update (selected-window)))

(defun abas-move-right ()
  (interactive)
  (abas-view-overwrite
   (delq
		nil
		(append
		 (abas-view-tabs-left-of-current)
		 (list (car (abas-view-tabs-right-of-current))
					 (abas-view-scope-current-tab))
		 (cdr (abas-view-tabs-right-of-current)))))
  (abas-view-invalidate t)
  (force-window-update (selected-window)))


;;; Integrations

(defvar erc-modified-channels-alist)
(defvar erc-server-ping-timer-alist)

(cl-defmethod abas-buffer-modified-p (buf (_ (eql erc-mode)))
  (assoc buf erc-modified-channels-alist))

(defun abas-erc-track ()
  (seq-do
   (abas-buffer-lambda
		(abas-view-invalidate)
		(abas-view-invalidate-siblings))
   (map-keys erc-server-ping-timer-alist)))

(add-hook 'erc-track-list-changed-hook #'abas-erc-track)

(defun abas-after-set-visited-file-name ()
	(when buffer-file-name
		(run-with-idle-timer 0 nil #'abas-view-track-renamed buffer-file-name)))

(add-hook 'after-set-visited-file-name-hook #'abas-after-set-visited-file-name 90)

(advice-add
 'erc-update-mode-line-buffer :around
 (lambda (fn buf)
   (funcall fn buf)
   (unless (buffer-local-value 'tab-line-format buf)
		 (with-current-buffer buf
			 (setq-local tab-line-format abas--tl-format)))))

(advice-add 'rename-buffer :after
						(lambda (&rest _) (abas-view-invalidate))
						'((name . abas-view-invalidate)))

(advice-add 'rename-file :after
						(lambda (_ new-name &optional _)
							(when new-name
								(run-with-idle-timer 0 nil #'abas-view-track-renamed new-name)))
						'((name . abas-view-invalidate)))


;;; Abas mode

(defvar-keymap abas-mode-map
	"<tab-line> <down-mouse-3>" #'menu-bar-open
	"<tab-line> <wheel-down>" #'abas-backward
	"<tab-line> <wheel-up>" #'abas-forward
	"<tab-line> C-<wheel-down>" #'abas-backward-group
	"<tab-line> C-<wheel-up>" #'abas-forward-group)

(defvar abas-cleanup-timer nil)
(defun abas-prune-killed ()
  (abas-buffer-tabs-prune-killed)
  (abas-groups-prune-killed)
  (setq abas-cleanup-timer nil)
  (add-hook 'buffer-list-update-hook #'abas-track-buffer-list-update))

(defun abas-track-buffer-list-update ()
  (unless abas-cleanup-timer
		(setq abas-cleanup-timer (run-with-idle-timer 1 nil #'abas-prune-killed)))
  (remove-hook 'buffer-list-update-hook #'abas-track-buffer-list-update))

(defun global-abas-mode-global-init ()
  (setq
   abas--initialized t
   abas--tabs-store (make-hash-table :size 256 :test #'equal)
   abas--groups-store (make-hash-table :size 32 :test #'equal)
   abas--scope 'buffers
   abas--separator nil))

(defun global-abas-mode-cleanup ()
  (abas-buffer-tabs-prune-killed)
  (abas-groups-prune-killed)
  (seq-map
   (abas-buffer-lambda
		(setq abas--view nil)
		(kill-local-variable 'tab-line-format))
   (abas-buffers))
  (setq
   abas--initialized nil
   abas--tabs-store nil
   abas--groups-store nil
	 abas--overview-group nil))

;;;###autoload
(define-minor-mode abas-mode
  "Display a tab bar in header line"
  :group 'abas
  :keymap abas-mode-map
  (unless abas--initialized
		(global-abas-mode-global-init))
  (if abas-mode
			(progn
				(setq abas--old-hlf (or abas--old-hlf
																(if (equal tab-line-format abas--tl-format)
																		(default-value 'tab-line-format)
																	tab-line-format)))
				(setq tab-line-format abas--tl-format)
				(add-hook 'first-change-hook #'abas-view-track-dirty nil t)
				(add-hook 'after-save-hook #'abas-view-track-saved nil t)
				(add-hook 'clone-buffer-hook #'abas-view-track-clone-buffer nil t)
				(add-hook 'clone-indirect-buffer-hook #'abas-view-track-clone-buffer nil t)
				(add-hook 'kill-buffer-hook #'abas-view-track-killed -10 t))
		(setq-local tab-line-format abas--old-hlf)
		(remove-hook 'first-change-hook #'abas-view-track-dirty t)
		(remove-hook 'after-save-hook #'abas-view-track-saved t)
		(remove-hook 'clone-buffer-hook #'abas-view-track-clone-buffer t)
		(remove-hook 'clone-indirect-buffer-hook #'abas-view-track-clone-buffer t)
		(remove-hook 'kill-buffer-hook #'abas-view-track-killed t)))

(defun abas-mode-should-enable-p (buffer)
  (let ((bn (buffer-name buffer)))
		(not
		 (or
			(eq (aref bn 0) ?\s)
			(let ((mm (buffer-local-value 'major-mode buffer)))
				(or (and tab-line-format (eq (get mm 'mode-class) 'special))
						(memq mm abas-modes-blacklist)))
			(string-match-p abas-buffers-blacklist-regexp bn)
			(run-hook-with-args-until-success 'abas-blacklist-functions buffer)))))

;;;###autoload
(defun turn-on-abas-mode ()
  (interactive)
  (when (abas-mode-should-enable-p (current-buffer))
		(abas-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-abas-mode abas-mode
  turn-on-abas-mode)

(add-hook 'global-abas-mode-off-hook #'global-abas-mode-cleanup)

(defun abas-invalidate-all (&rest _)
  (interactive)
  (global-abas-mode -1)
  (global-abas-mode 1)
  (force-window-update (selected-window))
	(seq-do
	 (abas-buffer-lambda (abas-tl-sync))
	 (seq-filter
		(lambda (buf) (buffer-local-value 'abas-mode buf))
		(buffer-list)))
	)

(provide 'abas)

;;; abas.el ends here
