;;; planner-timeclock-summary-proj.el --- timeclock project for the Emacs planner

;; Copyright (C) 2004 Pascal Quesseveur
;; Parts copyright (C) 2005 Free Software Foundation, Inc.
;; Parts copyright (C) 2005 Trent Buck

;; Author: Kambiz Darabi <darabi at web.de>
;; Time-stamp: <2007-08-17 08:39>
;; Description: Summary timeclock of a project on a per day basis

(require 'planner-timeclock-summary-proj)

(defun planner-timeclock-proj-make-alist (proj-name)
  "Return an association list for PROJ-NAME.
Each association is of the form (TASK . DURATION). TASK is a task
name defined inside PROJ-NAME and DURATION is the total time
computed for that task. When PROJ-NAME is nil, each TASK is a
project name, and DURATION is the time spent on that project."
  (let ((projects (planner-timeclock-proj-entries proj-name))
        (proj-alist))
    ;; Looping on project data. The project is made of tasks, and for each
    ;; task there can be several time intervals.
    (while projects
      (let* ((entry (car projects))
             (task (car entry))
             (task-data (cdr entry))
             (task-time 0))
        ;; We compute the time spent on task TASK
        (setq task-time 0)
        (while task-data
          (let ((task-entry (car task-data)))
            (progn
              (setq task-time (+ task-time
                                 (timeclock-entry-length task-entry)))
              (setq task-data (cdr task-data)))))
        ;; compute the name
        (if (string-match ": *" task)
            (if (and (< (match-end 0) (length task)) proj-name)
                (setq task (substring task (match-end 0)))
              (setq task (substring task 0 (match-beginning 0)))))
        ;; record the cons (task . time)
        (if proj-alist
            (let ((proj-time 0)
                  (proj-data-cell (assoc task proj-alist)))
              (if proj-data-cell
                  (progn
                    (setq proj-time (cdr proj-data-cell))
                    (setcdr proj-data-cell (+ task-time proj-time)))
              (add-to-list 'proj-alist (cons task task-time))))
          (setq proj-alist (list (cons task task-time))))
      (setq projects (cdr projects))))
    proj-alist))


(defun planner-timeclock-proj-make-day-table (proj-name)
"Return an association list for PROJ-NAME.
Each association is of the form (TASK . DURATION). TASK is a task
name defined inside PROJ-NAME and DURATION is the total time
computed for that task. When PROJ-NAME is nil, each TASK is a
project name, and DURATION is the time spent on that project."
  (let ((projects (planner-timeclock-proj-entries proj-name))
        (proj-alist)
	(day-alist)
        (day-table (makehash)))
    ;; Looping on project data. The project is made of tasks, and for each
    ;; task there can be several time intervals.
    (while projects
      (let* ((entry (car projects))
             (task (car entry))
             (task-data (cdr entry))
             (task-time 0))
        ;; compute the name
        (if (string-match ": *" task)
            (if (and (< (match-end 0) (length task)) proj-name)
                (setq task-name (substring task (match-end 0)))
              (setq task-name (substring task 0 (match-beginning 0)))))
        (if (string-match "\\(.*\\) {{.*" task-name)
	    (setq task-name (substring task-name 0 (match-end 1))))
        ;; We compute the time spent on task TASK
        (setq task-time 0)
        (while task-data
          (let* ((task-entry (car task-data))
		 (entry-length (timeclock-entry-length task-entry))
		 (entry-day (timeclock-day-begin task-entry)))
	    (setq task-time (+ task-time entry-length))
;; 	    (princ task-name)
;; 	    (princ " ")
;; 	    (princ entry-day)
;; 	    (princ " ")
;; 	    (princ entry-length)
;; 	    (princ "\n")
	    (let ((day-task-table (gethash entry-day day-table)))
	      (if day-task-table
		  ;; default day-task-time is zero
		  (let ((day-task-time (gethash task-name day-task-table 0)))
		    (puthash task-name (+ day-task-time entry-length) day-task-table))
		;; there is no hashtable associated with this day
		(progn
		  (setq new-day-task-table (makehash))
		  (puthash task-name entry-length new-day-task-table)
		  (puthash entry-day new-day-task-table day-table)))))
	    (setq task-data (cdr task-data)))
	    
	(setq projects (cdr projects))))
    day-table))

(defvar *proj-total* 0)

(defun map-day-table (day task-table)
  "used to map the values in day-table, which is returned by 
planner-timeclock-proj-make-day-table"
  (maphash (lambda (task duration) 
	     (princ (format "%s\t%s\t%.2f\n" 
			    (format-time-string "%Y-%m-%d" `(,day 0 0)) 
			    task 
			    (/ duration 3600)) 
		    (get-buffer "out"))
	     (setq *proj-total* (+ *proj-total* duration)))
	     task-table))

(timeclock-time-to-date '(18088 0 0))

(current-time)
(defun map-day-task-table (day task-table)
  "used to map the values in day-table, which is returned by 
planner-timeclock-proj-make-day-table"
  ;;
)



(defun planner-timeclock-proj-entries (proj-name)
  "Return entries from `timeclock-project-alist' for PROJ-NAME.
If PROJ-NAME is nil, return `timeclock-project-alist'."
  (let ((projects)
        (entry-list (timeclock-project-alist)))
    ;; Looping on entries. Each entry is in the form (PROJECT (TASKS
    ;; DATA)). We keep only entries for which PROJECT-NAME matches
    ;; PROJECT.
    (if (not proj-name)
        entry-list
      (while entry-list
        (let* ((proj (car entry-list))
               (proj-entry-name (car proj)))
          (if (and proj-name proj-entry-name
                   (string-match (concat "^\\[\\[" proj-name "\\]\\]")
                                 proj-entry-name))
              (if projects
                  (add-to-list 'projects proj)
                (setq projects (list proj))))
          (setq entry-list (cdr entry-list))))
      projects)))

(defun planner-timeclock-summary-proj-insinuate ()
  "Insinuate planner-timeclock-summary-proj with the rest of Planner."
  (add-hook 'planner-mode-hook
            (lambda ()
              (add-hook
               (if (boundp 'write-file-functions)
                   'write-file-functions
                 'write-file-hooks)
               'planner-timeclock-summary-proj-section nil t))))

(defun planner-timeclock-proj-seconds-to-string (seconds)
  "Convert the floating point number SECONDS to a string.
The string is in the form [WWw] [DDd] hh:ss."
  (let* ((workday (* planner-timeclock-workhours-per-day 3600))
         (days (floor (/ seconds workday)))
         (secs (floor (- seconds (* days workday)))))
    (if (> days planner-timeclock-workdays-per-week)
        (let ((weeks (/ days planner-timeclock-workdays-per-week))
              (dys (% days planner-timeclock-workdays-per-week)))
          (if (> dys 0)
              (format "%dw %dd %s" weeks dys
                      (timeclock-seconds-to-string secs))
            (format "%dw %s" weeks
                    (timeclock-seconds-to-string secs))))
      (if (> days 0)
          (format "%dd %s" days
                  (timeclock-seconds-to-string secs))
        (format "%s" (timeclock-seconds-to-string secs))))))

(defun planner-timeclock-summary-daily (project)
  "Insert daily time report for PROJECT in the *Messages* buffer."
  (interactive "sProject: ")
  (setq *proj-total* 0)
  (maphash 'map-day-table (planner-timeclock-proj-make-day-table project))
  (print (/ *proj-total* 3600)))

(provide 'planner-timeclock-summary-proj-daily)

;;; planner-timeclock-summary-proj-daily.el ends here


;; my current projects

;; (planner-timeclock-proj-make-alist "paybox-iran")
;; (planner-timeclock-proj-entries  "paybox-iran")

; (maphash 'map-day-table (planner-timeclock-proj-make-day-table "paybox-wizard"))

(planner-timeclock-summary-daily "lufthansa-cdmonitor")


(planner-timeclock-summary-daily "paybox-iran")
(planner-timeclock-summary-daily "paybox-coding")

