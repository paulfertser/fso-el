;; fso.el --- Emacs interface to freesmartphone API

;; Copyright (c) 2010 Paul Fertser <fercerpav@gmail.com>

;; Inspired by fso.el script by John Sullivan
;; Copyright (c) 2009 John Sullivan <john@wjsullivan.net>

;; Emacs Lisp Archive Entry
;; Filename: fso.el
;; Version: 0.1
;; Keywords:
;; Author: Paul Fertser <fercerpav@gmail.com>
;; Maintainer: Paul Fertser <fercerpav@gmail.com>
;; Description: Use Emacs for accessing smartphone functions.
;; URL: http://wiki.github.com/paulfertser/fso-el/
;; Bugs: http://github.com/paulfertser/fso-el/issues
;; Compatibility: Emacs23

;; This file is not part of GNU Emacs.

;; fso-el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; fso-el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with fso-el; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'dbus)

(define-derived-mode fso-mode fundamental-mode "FSO"
  "Major mode for interfacing with FSO-compatible smartphones.")

(defgroup fso nil
  "Customization group for FSO interface"
  :group 'applications)

(defcustom fso-log-file "~/.fso-el.log"
  "Log file."
  :type 'file
  :group 'fso)

(defcustom fso-gsm-pin ""
  "PIN to use to unlock the SIM card"
  :type 'string
  :group 'fso)

(defcustom fso-pdp-apn ""
  "APN name for the GPRS service."
  :type 'string
  :group 'fso)

(defcustom fso-pdp-username ""
  "Username for the GPRS service."
  :type 'string
  :group 'fso)

(defcustom fso-pdp-password ""
  "Password for the GPRS service."
  :type 'string
  :group 'fso)

(defcustom fso-auto-register t
  "Set to false to not register to the network on startup"
  :type 'boolean
  :group 'fso)

(defcustom fso-gsm-popup-calls t
  "Set to false to not switch to the calls buffer on call events"
  :type 'boolean
  :group 'fso)

(defcustom fso-gsm-request-receipt nil
  "Set to true to request message receipts by default"
  :type 'boolean
  :group 'fso)

(defcustom fso-gsm-status-properties '("provider" "strength" "registration" "act")
  "List of Network.Status properties to be displayed in the Status buffer"
  :type '(repeat string)
  :group 'fso)

(defcustom fso-hooker-image-path ""
  "Provide the path to the hooker image, e.g. ~/.fso-el/hooker.jpg"
  :type 'file
  :group 'fso)

;; --

(defvar fso-status-buffer "*FSO Status*")
(defvar fso-calllist-buffer "*FSO Calllist*")
(defvar fso-contacts-buffer "*FSO Contacts*")
(defvar fso-calls-buffer "*FSO Calls*")
(defvar fso-messages-buffer "*FSO Messages*")
(defvar fso-gsm-current-network-status nil
  "Provides information about current gsm network status in an assoc list")
(defvar fso-gsm-current-pdp-status nil
  "Provides information about current pdp (GPRS) status in an assoc list")
(defvar fso-gsm-current-network-status-hooks nil
  "Hooks to run on network status change")
(defvar fso-pim-current-unread-messages nil
  "The amount of currently unread messages")
(defvar fso-pim-current-unread-messages-hooks nil
  "Hooks to run on new message arrival")
(defvar fso-pim-current-missed-calls nil
  "The amount of missed calls")
(defvar fso-pim-current-missed-calls-hooks nil
  "Hooks to run on new missed call")
(defvar fso-pim-contacts nil
  "Variable holding an assoc list of (EntryId . ContactData) where
ContactData is an assoc list of (Field . Value)")
(defvar fso-pim-calls nil
  "Variable holding an assoc list of (EntryId . CallData) where
CallData is an assoc list of (Field . Value)")
(defvar fso-pim-messages nil
  "Variable holding an assoc list of (EntryId . Message) where
Message is an assoc list of (Field . Value)")
(defvar fso-gsm-calls nil
  "Holds GSM-level calls info (id . ((status . s) (peer . s) ...))")

;; --

(defun fso-register-signal-network (method function)
  (dbus-register-signal
   :system
   "org.freesmartphone.ogsmd"
   "/org/freesmartphone/GSM/Device"
   "org.freesmartphone.GSM.Network"
   method
   function))

(defun fso-call-gsm-network (method &rest args)
  (apply 'dbus-call-method
         :system
         "org.freesmartphone.ogsmd"
         "/org/freesmartphone/GSM/Device"
         "org.freesmartphone.GSM.Network"
         method :timeout 60000 args))

(defun fso-register-signal-gsm-call (method function)
  (dbus-register-signal
   :system
   "org.freesmartphone.ogsmd"
   "/org/freesmartphone/GSM/Device"
   "org.freesmartphone.GSM.Call"
   method
   function))

(defun fso-call-gsm-call (method &rest args)
  (apply 'dbus-call-method
         :system
         "org.freesmartphone.ogsmd"
         "/org/freesmartphone/GSM/Device"
         "org.freesmartphone.GSM.Call"
         method :timeout 60000 args))

(defun fso-register-signal-gsm-pdp (method function)
  (dbus-register-signal
   :system
   "org.freesmartphone.ogsmd"
   "/org/freesmartphone/GSM/Device"
   "org.freesmartphone.GSM.PDP"
   method
   function))

(defun fso-call-gsm-pdp (method &rest args)
  (apply 'dbus-call-method
         :system
         "org.freesmartphone.ogsmd"
         "/org/freesmartphone/GSM/Device"
         "org.freesmartphone.GSM.PDP"
         method :timeout 60000 args))

(defun fso-call-gsm-sms (method &rest args)
  (apply 'dbus-call-method
         :system
         "org.freesmartphone.ogsmd"
         "/org/freesmartphone/GSM/Device"
         "org.freesmartphone.GSM.SMS"
         method :timeout 60000 args))

(defun fso-call-gsm-device (method &rest args)
  (apply 'dbus-call-method
         :system
         "org.freesmartphone.ogsmd"
         "/org/freesmartphone/GSM/Device"
         "org.freesmartphone.GSM.Device"
         method :timeout 60000 args))

(defun fso-call-usage (method &rest args)
  (apply 'dbus-call-method
         :system
         "org.freesmartphone.ousaged"
	 "/org/freesmartphone/Usage"
	 "org.freesmartphone.Usage"
         method :timeout 60000 args))

(defun fso-call-pim-contacts (method &rest args)
  (apply 'dbus-call-method
         :system
         "org.freesmartphone.opimd"
         "/org/freesmartphone/PIM/Contacts"
         "org.freesmartphone.PIM.Contacts"
         method :timeout 60000 args))

(defun fso-call-pim-contacts-query (query-path method &rest args)
  (apply 'dbus-call-method
         :system
         "org.freesmartphone.opimd"
	 query-path
         "org.freesmartphone.PIM.ContactQuery"
         method :timeout 60000 args))

(defun fso-call-pim-calls-query (query-path method &rest args)
  (apply 'dbus-call-method
         :system
         "org.freesmartphone.opimd"
	 query-path
         "org.freesmartphone.PIM.CallQuery"
         method :timeout 60000 args))

(defun fso-call-pim-calls (method &rest args)
  (apply 'dbus-call-method
         :system
         "org.freesmartphone.opimd"
         "/org/freesmartphone/PIM/Calls"
         "org.freesmartphone.PIM.Calls"
         method :timeout 60000 args))

(defun fso-register-signal-pim-calls (method function)
  (dbus-register-signal
   :system
   "org.freesmartphone.opimd"
   "/org/freesmartphone/PIM/Calls"
   "org.freesmartphone.PIM.Calls"
   method
   function))

(defun fso-call-pim-call (calln method &rest args)
  (apply 'dbus-call-method
         :system
         "org.freesmartphone.opimd"
         (concat "/org/freesmartphone/PIM/Calls/"
		 (number-to-string calln))
         "org.freesmartphone.PIM.Call"
         method :timeout 60000 args))

(defun fso-register-signal-pim-messages (method function)
  (dbus-register-signal
   :system
   "org.freesmartphone.opimd"
   "/org/freesmartphone/PIM/Messages"
   "org.freesmartphone.PIM.Messages"
   method
   function))

(defun fso-call-pim-message (id method &rest args)
  (apply 'dbus-call-method
         :system
         "org.freesmartphone.opimd"
         (concat "/org/freesmartphone/PIM/Messages/"
		 (number-to-string id))
         "org.freesmartphone.PIM.Message"
         method :timeout 60000 args))

(defun fso-call-pim-messages (method &rest args)
  (apply 'dbus-call-method
         :system
         "org.freesmartphone.opimd"
         "/org/freesmartphone/PIM/Messages"
         "org.freesmartphone.PIM.Messages"
         method :timeout 60000 args))

(defun fso-call-pim-messages-query (query-path method &rest args)
  (apply 'dbus-call-method
         :system
         "org.freesmartphone.opimd"
	 query-path
         "org.freesmartphone.PIM.MessageQuery"
         method :timeout 60000 args))

(defun fso-register-signals ()
  (setq fso-registered-signals
	(list
	 (fso-register-signal-network "Status" 'fso-gsm-handle-status-change)
	 (fso-register-signal-pim-messages "UnreadMessages" 'fso-pim-handle-unread-messages)
	 (fso-register-signal-pim-calls "NewMissedCalls" 'fso-pim-handle-new-missed-calls)
	 (fso-register-signal-gsm-call "CallStatus" 'fso-gsm-handle-call-status)
	 (fso-register-signal-gsm-pdp "ContextStatus" 'fso-gsm-handle-pdp-status)
	 (fso-register-signal-network "IncomingUssd" 'fso-gsm-handle-incoming-ussd)
	 (fso-register-signal-pim-calls "UpdatedCall" 'fso-pim-handle-updated-call)
	 (fso-register-signal-pim-calls "NewCall" 'fso-pim-handle-new-call)
	 (fso-register-signal-pim-calls "DeletedCall" 'fso-pim-handle-deleted-call)
	 (fso-register-signal-pim-messages "UpdatedMessage" 'fso-pim-handle-updated-message)
	 (fso-register-signal-pim-messages "NewMessage" 'fso-pim-handle-new-message)
	 (fso-register-signal-pim-messages "DeletedMessage" 'fso-pim-handle-deleted-message))))

(defun fso-unregister-signals ()
  (mapc 'dbus-unregister-object fso-registered-signals))

;; O tail-recursion, where art thou
(defun fso-pim-entry-to-assoc (pimentry)
  (defun fso-pim-entry-to-assoc-r (pimentry entryid entry)
    (let ((field (car pimentry)))
      (cond
       ((not pimentry)
	(cons entryid entry))
       ((string= (car field) "EntryId")
	(fso-pim-entry-to-assoc-r (cdr pimentry) (car (cadr field)) entry))
       (t
	(fso-pim-entry-to-assoc-r (cdr pimentry) entryid (cons (cons (car field) (car (cadr field))) entry))))))
  (fso-pim-entry-to-assoc-r pimentry nil nil))

(defun fso-dbus-dict-to-assoc (dict)
  (mapcar
   (lambda (f) (cons (car f) (car (cadr f))))
   dict))

;; --

(defun fso-gsm-handle-incoming-ussd (mode message)
  (with-output-to-temp-buffer "Incoming USSD" (print message)))

(defun fso-gsm-handle-pdp-status (status properties)
  (setq fso-gsm-current-pdp-status
	(cons (cons "status" status)
	      (fso-dbus-dict-to-assoc properties)))
  (run-hooks 'fso-gsm-current-network-status-hooks))

(defun fso-gsm-get-context-status ()
  (apply 'fso-gsm-handle-pdp-status (fso-call-gsm-pdp "GetContextStatus")))

(defun fso-pim-path-to-id (path)
  (string-match "[0-9]*$" path)
  (string-to-number (match-string 0 path)))

(defun fso-num-or-na (d)
  (if d
      (format "%d" d)
    "n/a"))

(defun fso-status-buffer-update ()
  (save-excursion
    (set-buffer fso-status-buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (mapc (lambda (f)
	      (let ((prop (assoc f fso-gsm-current-network-status)))
		(insert (format "%s: %s\n" (car prop) (cdr prop)))))
	    fso-gsm-status-properties)
      (insert (format "GPRS: %s   " (cdr (assoc "status" fso-gsm-current-pdp-status))))
      (insert-button "On" 'action 'fso-gsm-pdp-on 'follow-link t)
      (insert "   ")
      (insert-button "Off" 'action 'fso-gsm-pdp-off 'follow-link t)
      (insert "\n\n")
      (insert-button (format "Unread messages: %s" (fso-num-or-na fso-pim-current-unread-messages))
		     'action (lambda (x) (interactive) (fso-pim-show-messages))
		     'follow-link t)
      (insert "  ")
      (insert-button (format "Missed calls: %s" (fso-num-or-na fso-pim-current-missed-calls))
		     'action (lambda (x) (interactive) (fso-pim-show-calls))
		     'follow-link t)
      (if fso-hooker-image
	  (progn
	    (insert "\n\n         ")
	    (insert-image fso-hooker-image))))))

(defun fso-gsm-pdp-on (b)
  (interactive)
  (fso-call-gsm-pdp "ActivateContext"))

(defun fso-gsm-pdp-off (b)
  (interactive)
  (fso-call-gsm-pdp "DeactivateContext"))

(defun fso-gsm-no-calls-p ()
  (defun fso-gsm-no-calls-p-r (l)
    (cond
     ((not l) t)
     ((not
       (equal "RELEASE" (cdr (assoc "status" (car l))))) nil)
     (t (fso-gsm-no-calls-p-r (cdr l)))))
(fso-gsm-no-calls-p-r fso-gsm-calls))

(defun fso-gsm-calls-add-new-id (id status properties)
  (setq fso-gsm-calls
	(assq-delete-all id fso-gsm-calls))
  (setq fso-gsm-calls
	(append (list (cons id
			    (append (list (cons "status" status))
				    (fso-dbus-dict-to-assoc properties))))
		      fso-gsm-calls)))

(defun fso-gsm-handle-call-status (id status properties)
  (save-excursion
    (fso-gsm-calls-add-new-id id status properties)
    (if fso-gsm-popup-calls
	(if (fso-gsm-no-calls-p)
	    (replace-buffer-in-windows fso-calls-buffer)
	  (fso-gsm-show-calls))
      (if (not (get-buffer fso-calls-buffer))
	  (fso-create-calls-buffer)))
    (set-buffer fso-calls-buffer)
    (if (ewoc-collect buffer-ewoc
		      (lambda (x) (eq id x)))
	(ewoc-map (lambda (x) (eq id x))
		  buffer-ewoc)
      (ewoc-enter-first buffer-ewoc id))))

(defun fso-gsm-call-lambda (methodname callid)
  (list 'lambda '(x) '(interactive) `(fso-call-gsm-call ,methodname :int32 ,callid)))

(defun calls-pp (call-id)
  (if call-id
      (progn
	(let ((call (cdr (assq call-id fso-gsm-calls))))
	  (mapc (lambda (f)
		  (insert (format "%s: %s\n" (car f) (cdr f)))
		  (if (equal "peer" (car f))
		      (let ((query-path
			     (fso-call-pim-contacts "Query" `((:dict-entry "$phonenumber" (:variant ,(cdr f)))))))
			(condition-case nil
			    (insert (format "Name: %s\n"
					    (cdr
					     (assoc "Name" (fso-pim-entry-to-assoc
							    (fso-call-pim-contacts-query query-path "GetResult"))))))
			  (error nil))
			(fso-call-pim-contacts-query query-path "Dispose"))))
		  call))
	(insert-button "Activate" 'action (fso-gsm-call-lambda "Activate" call-id)
		       'follow-link t)
	(insert "  ")
	(insert-button "Release" 'action (fso-gsm-call-lambda "Release" call-id)
		       'follow-link t)
	(insert "     ")
	(insert-button "Conference" 'action (fso-gsm-call-lambda "Conference" call-id)
		       'follow-link t))))

(defun fso-gsm-hold-active ()
  (interactive)
  (fso-call-gsm-call "HoldActive"))

(defun fso-gsm-release-held ()
  (interactive)
  (fso-call-gsm-call "ReleaseHeld"))

(defun fso-gsm-release-all ()
  (interactive)
  (fso-call-gsm-call "ReleaseAll"))

(defun fso-gsm-join ()
  (interactive)
  (fso-call-gsm-call "Join"))

(defun fso-create-calls-buffer ()
  (save-excursion
    (set-buffer (get-buffer-create fso-calls-buffer))
    (fso-mode)
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (let ((ewoc (ewoc-create 'calls-pp)))
      (set (make-local-variable 'buffer-ewoc) ewoc))
    (setq header-line-format
	  (concat
	   (propertize "HoldActive"
		       'keymap '(keymap (header-line keymap (mouse-1 . fso-gsm-hold-active)))
		       'mouse-face 'mode-line-highlight)
	   "  "
	   (propertize "ReleaseHeld"
		       'keymap '(keymap (header-line keymap (mouse-1 . fso-gsm-release-held)))
		       'mouse-face 'mode-line-highlight)
	   "  "
	   (propertize "ReleaseAll"
		       'keymap '(keymap (header-line keymap (mouse-1 . fso-gsm-release-all)))
		       'mouse-face 'mode-line-highlight)
	   "    "
	   (propertize "Join"
		       'keymap '(keymap (header-line keymap (mouse-1 . fso-gsm-join)))
		       'mouse-face 'mode-line-highlight)))))

(defmacro fso-do-in-list (buffer func &rest args)
  `(save-excursion
    (condition-case nil
	(progn
	  (set-buffer ,buffer)
	  (funcall ,func ,@args))
      (error nil))))

(defun fso-pim-handle-new-call (path)
  (fso-pim-handle-new path fso-calllist-buffer 'fso-call-pim-call fso-pim-calls))

(defun fso-pim-handle-updated-call (path query)
  (fso-pim-handle-updated path fso-calllist-buffer 'fso-call-pim-call fso-pim-calls))

(defun fso-pim-handle-deleted-call (path)
  (fso-pim-handle-deleted path fso-calllist-buffer fso-pim-calls))

(defun fso-pim-handle-new-message (path)
  (fso-pim-handle-new path fso-messages-buffer 'fso-call-pim-message fso-pim-messages))

(defun fso-pim-handle-updated-message (path query)
  (fso-pim-handle-updated path fso-messages-buffer 'fso-call-pim-message fso-pim-messages))

(defun fso-pim-handle-deleted-message (path)
  (fso-pim-handle-deleted path fso-messages-buffer fso-pim-messages))

(defun fso-pim-handle-new (path buffer getcontentp storage)
  (let ((entryid (fso-pim-path-to-id path)))
    (setq storage
	  (nconc storage
		 (list (fso-pim-entry-to-assoc (funcall getcontentp entryid "GetContent")))))
    (fso-do-in-list buffer 'ewoc-invalidate
		    buffer-ewoc (ewoc-enter-first buffer-ewoc entryid))))

(defun fso-pim-handle-updated (path buffer getcontentp storage)
  (let* ((entryid (fso-pim-path-to-id path))
	 (entry (assq entryid storage)))
    (if entry
	(progn
	  (setcdr entry
		  (cdr (fso-pim-entry-to-assoc (funcall getcontentp entryid "GetContent"))))
	  (fso-do-in-list buffer 'ewoc-map
			  (lambda (e id) (eq e id)) buffer-ewoc entryid)))))

(defun fso-pim-handle-deleted (path buffer storage)
  (let ((entryid (fso-pim-path-to-id path)))
    (assq-delete-all entryid storage)
    (fso-do-in-list buffer 'ewoc-filter
		    buffer-ewoc (lambda (e id) (not (eq e id))) entryid)))

(defun fso-pim-calls-mark-old ()
  (interactive)
  (fso-call-pim-call
   (ewoc-data (ewoc-locate buffer-ewoc))
   "Update"
   '((:dict-entry "New" (:variant 0)))))

(defun calllist-pp (callentry-id)
  (if callentry-id
      (let ((callentry (cdr (assq callentry-id fso-pim-calls))))
	(insert
	 (propertize
	  (format "%s: %s %s %s\n%s\n"
		  (or (cdr (assoc "Name"
				  (assq (cdr (assoc "@Contacts" callentry)) fso-pim-contacts)))
		      (cdr (assoc "Peer" callentry)))
		  (cdr (assoc "Direction" callentry))
		  (if (eq (cdr (assoc "Answered" callentry)) 1)
		      "answered"
		    "unanswered")
		  (if (eq (cdr (assoc "New" callentry)) 1)
		      "new"
		    "old")
		  (condition-case nil
		      (format-time-string "%c"
					  (seconds-to-time
					   (float
					    (cdr (assoc "Timestamp" callentry)))))
		    (error (format "%d" callentry-id))))
	  'keymap '(keymap (mouse-1 . fso-pim-calls-mark-old) (13 . fso-pim-calls-mark-old)))))))
;;	(mapc (lambda (f)
;;		(insert (format "%s: %s\n" (car f) (cdr f)))
;;		(if (string= (car f) "@Contacts")
;;		    (insert (format "N: %s\n" (cdr (assoc "Name" (assq (cdr f) fso-pim-contacts)))))))
;;	      callentry)))

(defun fso-pim-call-selected-call ()
  (interactive)
  (fso-gsm-initiate-call
   (cdr (assoc "Peer" (assq (ewoc-data (ewoc-locate buffer-ewoc)) fso-pim-calls)))))

(defun fso-pim-delete-selected-call ()
  (interactive)
  (fso-call-pim-call (ewoc-data (ewoc-locate buffer-ewoc)) "Delete"))

(defun fso-create-calllist-buffer ()
  (message "FSO: retrieving call log")
  (fso-pim-get-all-calls)
  (save-excursion
    (set-buffer (get-buffer-create fso-calllist-buffer))
    (fso-mode)
    (let ((keymap (copy-keymap (current-local-map))))
      (define-key keymap "r" 'fso-pim-call-selected-call)
      (define-key keymap "d" 'fso-pim-delete-selected-call)
      (use-local-map keymap))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (let ((ewoc (ewoc-create 'calllist-pp)))
      (set (make-local-variable 'buffer-ewoc) ewoc)
      (mapc (lambda (x) (ewoc-enter-last ewoc (car x)))
	    fso-pim-calls))
    (setq header-line-format
	  (concat
	   (propertize "Status"
		       'keymap '(keymap (header-line keymap (mouse-1 . fso-show-status)))
		       'mouse-face 'mode-line-highlight)
	   "  "
	   (propertize "Call"
		       'keymap '(keymap (header-line keymap (mouse-1 . fso-pim-call-selected-call)))
		       'mouse-face 'mode-line-highlight)
	   "  "
	   (propertize "Delete"
		       'keymap '(keymap (header-line keymap (mouse-1 . fso-pim-delete-selected-call)))
		       'mouse-face 'mode-line-highlight)))))

(defun contact-pp (contact-id)
  (if contact-id
      (let ((contactentry (cdr (assq contact-id fso-pim-contacts))))
	(insert (format "%s: %s\n"
			(cdr (assoc "Name" contactentry))
			(cdr (assoc "Phone" contactentry)))))))

(defun fso-gsm-initiate-call (number)
  (interactive "sNumber to dial: ")
  (if (let ((len (length number)))
	(or (<= len 2)
	    (char-equal ?# (elt number (- len 1)))))
      (fso-call-gsm-network "SendUssdRequest" number)
    (fso-call-gsm-call "Initiate" number "voice")))

(defun fso-gsm-contacts-call ()
  (interactive)
  (fso-gsm-initiate-call (cdr (assoc "Phone" (cdr (assq (ewoc-data (ewoc-locate buffer-ewoc)) fso-pim-contacts))))))

(defun fso-gsm-contacts-message ()
  (interactive)
  (fso-gsm-send-message nil
			(cdr (assoc "Phone" (cdr (assq (ewoc-data (ewoc-locate buffer-ewoc)) fso-pim-contacts))))
			(read-string "Message: ")))

(defun fso-create-contacts-buffer ()
  (save-excursion
    (set-buffer (get-buffer-create fso-contacts-buffer))
    (fso-mode)
    (let ((keymap (copy-keymap (current-local-map))))
      (define-key keymap "\r" 'fso-gsm-contacts-call)
      (use-local-map keymap))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (let ((ewoc (ewoc-create 'contact-pp)))
      (set (make-local-variable 'buffer-ewoc) ewoc)
      (let ((contacts (sort (copy-sequence fso-pim-contacts)
			    (lambda (f s) (string<
					   (cdr (assoc "Name" f))
					   (cdr (assoc "Name" s)))))))
	(mapc (lambda (x) (ewoc-enter-last ewoc (car x))) contacts)))
    (setq header-line-format
	  (concat
	   (propertize "Delete"
		       'keymap '(keymap (header-line keymap (mouse-1 . fso-gsm-contacts-call)))
		       'mouse-face 'mode-line-highlight)
	   "    "
	   (propertize "Call"
		       'keymap '(keymap (header-line keymap (mouse-1 . fso-gsm-contacts-call)))
		       'mouse-face 'mode-line-highlight)
	   "  "
	   (propertize "Message"
		       'keymap '(keymap (header-line keymap (mouse-1 . fso-gsm-contacts-message)))
		       'mouse-face 'mode-line-highlight)
	   "    "
	   (propertize "Status"
		       'keymap '(keymap (header-line keymap (mouse-1 . (lambda () (interactive) (switch-to-buffer fso-status-buffer)))))
		       'mouse-face 'mode-line-highlight)))))

(defun fso-gsm-send-message (toggle-receipt peer content)
  (interactive "P\nsNumber to send to: \nsContent: ")
  (let* ((message (encode-coding-string content 'utf-8))
	 (request-receipt
	  (if toggle-receipt (not fso-gsm-request-receipt) fso-gsm-request-receipt))
	 (gsmid (car (fso-call-gsm-sms "SendTextMessage" peer message request-receipt))))
    (fso-call-pim-messages "Add" (append
				  `((:dict-entry "Peer" (:variant ,peer))
				    (:dict-entry "Content" (:variant ,message))
				    (:dict-entry "Timestamp" (:variant ,(float-time)))
				    (:dict-entry "New" (:variant 0))
				    (:dict-entry "Direction" (:variant "out"))
				    (:dict-entry "Source" (:variant "SMS")))
				  (if request-receipt
				   `((:dict-entry "SMS-message-reference" (:variant ,gsmid)))
				   nil)))))
(defun fso-pim-messages-mark-old ()
  (interactive)
  (fso-call-pim-message
   (ewoc-data (ewoc-locate buffer-ewoc))
   "Update"
   '((:dict-entry "New" (:variant 0)))))

(defun messages-pp (messageentry-id)
  (if messageentry-id
      (let ((messageentry (cdr (assq messageentry-id fso-pim-messages))))
	(insert
	 (propertize
	  (format "%s %s %s %s\n%s\n"
		  (or (cdr (assoc "Name"
				  (assq (cdr (assoc "@Contacts" messageentry)) fso-pim-contacts)))
		      (cdr (assoc "Peer" messageentry)))
		  (cdr (assoc "Direction" messageentry))
		  (if (eq (cdr (assoc "New" messageentry)) 1)
		      "new"
		    "old")
		  (condition-case nil
		      (format-time-string "%c"
					  (seconds-to-time
					   (float
					    (cdr (assoc "Timestamp" messageentry)))))
		    (error (format "%d" messageentry-id)))
		  (cdr (assoc "Content" messageentry)))
	  'keymap '(keymap (mouse-1 . fso-pim-messages-mark-old) (13 . fso-pim-messages-mark-old)))))))

(defun fso-pim-call-selected-message ()
  (interactive)
  (fso-gsm-initiate-call
   (cdr (assoc "Peer" (assq (ewoc-data (ewoc-locate buffer-ewoc)) fso-pim-messages)))))

(defun fso-pim-delete-selected-message ()
  (interactive)
  (fso-call-pim-message (ewoc-data (ewoc-locate buffer-ewoc)) "Delete"))

(defun fso-pim-reply-selected-message ()
  (interactive)
  (fso-gsm-send-message nil
			(cdr (assoc "Peer" (assq (ewoc-data (ewoc-locate buffer-ewoc)) fso-pim-messages)))
			(read-string "Message: ")))

(defun fso-create-messages-buffer ()
  (message "FSO: retrieving messages")
  (fso-pim-get-all-messages)
  (save-excursion
    (set-buffer (get-buffer-create fso-messages-buffer))
    (fso-mode)
    (let ((keymap (copy-keymap (current-local-map))))
      (define-key keymap "r" 'fso-pim-reply-selected-message)
      (define-key keymap "C" 'fso-pim-call-selected-message)
      (define-key keymap "d" 'fso-pim-delete-selected-message)
      (use-local-map keymap))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (let ((ewoc (ewoc-create 'messages-pp)))
      (set (make-local-variable 'buffer-ewoc) ewoc)
      (mapc (lambda (x) (ewoc-enter-last ewoc (car x))) fso-pim-messages))
    (setq header-line-format
	  (concat
	   (propertize "Delete"
		       'keymap '(keymap (header-line keymap (mouse-1 . fso-pim-delete-selected-message)))
		       'mouse-face 'mode-line-highlight)
	   "    "
	   (propertize "Reply"
		       'keymap '(keymap (header-line keymap (mouse-1 . fso-pim-reply-selected-message)))
		       'mouse-face 'mode-line-highlight)
	   "  "
	   (propertize "Call"
		       'keymap '(keymap (header-line keymap (mouse-1 . fso-pim-call-selected-message)))
		       'mouse-face 'mode-line-highlight)
	   "  "
	   (propertize "Status"
		       'keymap '(keymap (header-line keymap (mouse-1 . fso-show-status)))
		       'mouse-face 'mode-line-highlight)))))

(defun fso-gsm-handle-status-change (status)
  (setq fso-gsm-current-network-status
	(fso-dbus-dict-to-assoc status))
  (run-hooks 'fso-gsm-current-network-status-hooks))

(defun fso-gsm-get-network-status ()
  (fso-gsm-handle-status-change (fso-call-gsm-network "GetStatus")))

(defun fso-pim-handle-unread-messages (amount)
  (setq fso-pim-current-unread-messages amount)
  (run-hooks 'fso-pim-unread-messages-hooks))

(defun fso-pim-get-unread-messages ()
  (fso-pim-handle-unread-messages (fso-call-pim-messages "GetUnreadMessages")))

(defun fso-pim-handle-new-missed-calls (amount)
  (setq fso-pim-current-missed-calls amount)
  (run-hooks 'fso-pim-new-missed-calls-hooks))

(defun fso-pim-get-new-missed-calls ()
  (fso-pim-handle-new-missed-calls (fso-call-pim-calls "GetNewMissedCalls")))

(defun fso-pim-get-all-contacts ()
  (let ((query-path (fso-call-pim-contacts "Query" '(:array :signature "{sv}"))))
    (setq fso-pim-contacts
	  (mapcar 'fso-pim-entry-to-assoc
		  (fso-call-pim-contacts-query query-path "GetMultipleResults" -1)))
    (fso-call-pim-contacts-query query-path "Dispose")))

(defun fso-pim-get-all-messages ()
  (let ((query-path (fso-call-pim-messages "Query" '((:dict-entry "_resolve_phonenumber" (:variant t))
						  (:dict-entry "_limit" (:variant 100))
						  (:dict-entry "_sortdesc" (:variant t))
						  (:dict-entry "_sortby" (:variant "Timestamp"))))))
    (setq fso-pim-messages
	  (mapcar 'fso-pim-entry-to-assoc
		  (fso-call-pim-messages-query query-path "GetMultipleResults" -1)))
    (fso-call-pim-messages-query query-path "Dispose")))

(defun fso-pim-get-all-calls ()
  (let ((query-path (fso-call-pim-calls "Query" '((:dict-entry "_resolve_phonenumber" (:variant t))
						  (:dict-entry "_limit" (:variant 100))
						  (:dict-entry "_sortdesc" (:variant t))
						  (:dict-entry "_sortby" (:variant "Timestamp"))))))
    (setq fso-pim-calls
	  (mapcar 'fso-pim-entry-to-assoc
		  (fso-call-pim-calls-query query-path "GetMultipleResults" -1)))
    (fso-call-pim-calls-query query-path "Dispose")))

(defun fso-usage-request-resource (resource)
  (fso-call-usage "RequestResource" resource))

(defun fso-usage-release-resource (resource)
  (fso-call-usage "ReleaseResource" resource))

(defun fso-usage-request-resource-gsm ()
  (fso-usage-request-resource "GSM"))

(defun fso-usage-release-resource-gsm ()
  (fso-usage-release-resource "GSM"))

(defun fso-gsm-set-functionality (s)
  (fso-call-gsm-device "SetFunctionality" s t fso-gsm-pin))

(defun fso-alive-p ()
  "Return t if FSO-el is running."
  (and (boundp 'fso-status-buffer)
       (get-buffer fso-status-buffer)
       (save-excursion
         (set-buffer fso-status-buffer)
         (eq major-mode 'fso-mode))))

(defun fso-show-status ()
  (interactive)
  (switch-to-buffer fso-status-buffer))

(defun fso-gsm-show-calls ()
  (interactive)
  (if (not (get-buffer fso-calls-buffer))
      (fso-create-calls-buffer))
  (switch-to-buffer fso-calls-buffer))

(defun fso-pim-show-calls ()
  (interactive)
  (if (not (get-buffer fso-calllist-buffer))
      (fso-create-calllist-buffer))
  (switch-to-buffer fso-calllist-buffer))

(defun fso-pim-show-contacts ()
  (interactive)
  (if (not (get-buffer fso-contacts-buffer))
      (fso-create-contacts-buffer))
  (switch-to-buffer fso-contacts-buffer))

(defun fso-pim-show-messages ()
  (interactive)
  (if (not (get-buffer fso-messages-buffer))
      (fso-create-messages-buffer))
  (switch-to-buffer fso-messages-buffer))

(defun fso-create-status-buffer ()
    (switch-to-buffer fso-status-buffer)
    (fso-mode)
    (buffer-disable-undo)
    (message "FSO: requesting GSM resource")
    (fso-usage-request-resource-gsm)
    (if fso-auto-register
	(fso-gsm-set-functionality "full"))
    (add-hook 'fso-gsm-current-network-status-hooks 'fso-status-buffer-update)
    (add-hook 'fso-pim-new-missed-calls-hooks 'fso-status-buffer-update)
    (add-hook 'fso-pim-unread-messages-hooks 'fso-status-buffer-update)
    (fso-gsm-get-network-status)
    (fso-pim-get-unread-messages)
    (fso-pim-get-new-missed-calls)
    (message "FSO: retrieving phonebook")
    (fso-pim-get-all-contacts)
    (setq header-line-format
	  (concat
	   (propertize "Contacts"
		      'keymap '(keymap (header-line keymap (mouse-1 . fso-pim-show-contacts)))
		      'mouse-face 'mode-line-highlight)
	   " "
	   (propertize "Calls"
		      'keymap '(keymap (header-line keymap (mouse-1 . fso-gsm-show-calls)))
		      'mouse-face 'mode-line-highlight)))
    (setq buffer-read-only t)
    (add-hook 'kill-buffer-hook 'fso-kill-buffer-hook))

(defun fso-kill-buffer-hook ()
  (if (equal (buffer-name) fso-status-buffer)
      (progn
	(remove-hook 'kill-buffer-hook 'fso-kill-buffer-hook)
	(fso-quit))))

(defun fso-kill-buffer (b)
  (if (get-buffer b)
      (kill-buffer b)))

(defun fso-quit ()
  (interactive)
  (fso-unregister-signals)
  (condition-case nil (fso-usage-release-resource-gsm)
    (error nil))
  (fso-kill-buffer fso-calls-buffer)
  (fso-kill-buffer fso-contacts-buffer)
  (fso-kill-buffer fso-calllist-buffer)
  (fso-kill-buffer fso-status-buffer)
  (fso-kill-buffer fso-messages-buffer)
  (setq fso-pim-contacts nil)
  (setq fso-pim-calls nil))

(defun fso ()
  "Start FSO or switch to the running FSO buffer."
  (interactive)
  (if (fso-alive-p)
      (switch-to-buffer fso-status-buffer)
    (if (and fso-hooker-image-path
	     (not (equal "" fso-hooker-image-path)))
	(defvar fso-hooker-image
	  (find-image `((:type jpeg :file ,fso-hooker-image-path)))))
    (fso-create-status-buffer)
    (fso-create-calllist-buffer)
    (fso-create-contacts-buffer)
    (fso-create-calls-buffer)
    (fso-create-messages-buffer)
    (if fso-pdp-apn
	(fso-call-gsm-pdp "SetCredentials" fso-pdp-apn
			  fso-pdp-username fso-pdp-password))
    (fso-register-signals)
    (message "FSO initialized successfully")))

(defun fso-ewoc-next ()
  (interactive)
  (if buffer-ewoc
      (ewoc-goto-next buffer-ewoc 1)))

(defun fso-ewoc-prev ()
  (interactive)
  (if buffer-ewoc
      (ewoc-goto-prev buffer-ewoc 1)))

(suppress-keymap fso-mode-map)
(define-key fso-mode-map "n" 'fso-ewoc-next)
(define-key fso-mode-map "p" 'fso-ewoc-prev)
(define-key fso-mode-map "s" 'fso-show-status)
(define-key fso-mode-map "m" 'fso-pim-show-messages)
(define-key fso-mode-map "c" 'fso-pim-show-contacts)
(define-key fso-mode-map "l" 'fso-pim-show-calls)

(provide 'fso)
