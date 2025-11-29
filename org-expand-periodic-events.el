;;; org-expand-periodic-events.el --- Expand periodic events into individual entries -*- lexical-binding: t; -*-

;;; Commentary:
;; Expand <start +repeater>--<end> into multiple one-time event entries

;;; Code:

(require 'org)
(require 'calendar)

(defun org-expand-periodic-event-at-point ()
  "Expand periodic event notation at point into multiple entries.
Copies the entire org subtree (including all content) for each date."
  (interactive)
  (save-excursion
    ;; First, find the timestamp pattern within the current subtree
    (let ((subtree-start (save-excursion 
                           (condition-case nil
                               (progn (org-back-to-heading t) (point))
                             (error (point-min)))))
          (subtree-end (save-excursion 
                         (condition-case nil
                             (progn (org-end-of-subtree t t) (point))
                           (error (point-max)))))
          found-match)
      
      (goto-char subtree-start)
      (when (re-search-forward "<\\([^>]+\\)>--<\\([^>]+\\)>" subtree-end t)
        (setq found-match t)
        (let* ((start-str (match-string 1))
               (end-str (match-string 2))
               (pattern-start (match-beginning 0))
               (pattern-end (match-end 0)))
          
          (message "Found pattern: <%s>--<%s>" start-str end-str)
          
          ;; Parse and generate events
          (let* ((start-parts (org-expand-parse-simple start-str))
                 (end-parts (org-expand-parse-simple end-str))
                 (events (org-expand-generate-simple start-parts end-parts)))
            
            (message "Generated %d events" (length events))
            
            ;; Get the entire subtree content
            (goto-char subtree-start)
            (let* ((subtree-content (buffer-substring-no-properties subtree-start subtree-end))
                   (expanded-entries nil))
              
              ;; Create a copy for each event, replacing the timestamp pattern
              (dolist (event events)
                (let ((new-entry (replace-regexp-in-string
                                  "<[^>]+>--<[^>]+>"
                                  event
                                  subtree-content
                                  t t)))
                  (push new-entry expanded-entries)))
              
              ;; Delete the original subtree
              (delete-region subtree-start subtree-end)
              
              ;; Insert all expanded entries at the same location
              (goto-char subtree-start)
              (dolist (entry (nreverse expanded-entries))
                (insert entry))
              
              (message "Expanded into %d complete entries" (length events))))))
      
      (unless found-match
        (message "No pattern like <...>--<...> found in this entry")))))

(defun org-expand-parse-simple (timestamp-str)
  "Parse timestamp string into components."
  (let* ((parts (split-string timestamp-str " "))
         (date-str (car parts))
         (date-parts (mapcar #'string-to-number (split-string date-str "-")))
         (year (nth 0 date-parts))
         (month (nth 1 date-parts))
         (day (nth 2 date-parts))
         (time nil)
         (repeater nil))
    
    (dolist (part parts)
      (when (string-match "^[0-9]+:[0-9]+" part)
        (setq time part)))
    
    (dolist (part parts)
      (when (string-match "^\\+[0-9]+[dwmy]$" part)
        (setq repeater part)))
    
    (list :year year :month month :day day :time time :repeater repeater)))

(defun org-expand-generate-simple (start-plist end-plist)
  "Generate list of timestamp strings from START-PLIST to END-PLIST."
  (let* ((start-year (plist-get start-plist :year))
         (start-month (plist-get start-plist :month))
         (start-day (plist-get start-plist :day))
         (end-year (plist-get end-plist :year))
         (end-month (plist-get end-plist :month))
         (end-day (plist-get end-plist :day))
         (time (plist-get start-plist :time))
         (repeater (plist-get start-plist :repeater))
         (interval (org-expand-get-interval repeater))
         (results nil))
    
    ;; Use encode-time and decode-time instead of calendar functions
    (let* ((start-time (encode-time 0 0 0 start-day start-month start-year))
           (end-time (encode-time 0 0 0 end-day end-month end-year))
           (current-time start-time)
           (one-day-seconds (* 24 60 60)))
      
      ;; Generate dates - loop while current date <= end date
      (while (let ((cur-decoded (decode-time current-time))
                   (end-decoded (decode-time end-time)))
               (or (< (nth 5 cur-decoded) (nth 5 end-decoded))  ; year less
                   (and (= (nth 5 cur-decoded) (nth 5 end-decoded))  ; same year
                        (or (< (nth 4 cur-decoded) (nth 4 end-decoded))  ; month less
                            (and (= (nth 4 cur-decoded) (nth 4 end-decoded))  ; same month
                                 (<= (nth 3 cur-decoded) (nth 3 end-decoded)))))))  ; day less or equal
        (let* ((decoded (decode-time current-time))
               (d (nth 3 decoded))
               (m (nth 4 decoded))
               (y (nth 5 decoded))
               (dow (nth 6 decoded))
               (dow-names ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"])
               (dow-name (aref dow-names dow))
               (time-part (if time (concat " " time) ""))
               (timestamp (format "<%04d-%02d-%02d %s%s>" y m d dow-name time-part)))
          (push timestamp results)
          ;; Add interval days
          (setq current-time (time-add current-time (seconds-to-time (* interval one-day-seconds))))))
      
      (nreverse results))))

(defun org-expand-get-interval (repeater)
  "Get interval in days from REPEATER string like '+1w' or '+2w'."
  (if (not repeater)
      7
    (let* ((num-str (substring repeater 1 (1- (length repeater))))
           (num (string-to-number num-str))
           (unit (substring repeater -1)))
      (cond
       ((string= unit "d") num)
       ((string= unit "w") (* num 7))
       ((string= unit "m") (* num 30))
       ((string= unit "y") (* num 365))
       (t 7)))))

(defun org-expand-insert-expanded-event (headline start end time repeater)
  "Insert expanded periodic events."
  (interactive "sHeadline: 
sStart date (YYYY-MM-DD): 
sEnd date (YYYY-MM-DD): 
sTime (HH:MM-HH:MM, optional): 
sRepeater (+1w/+2w/+1d, optional): ")
  
  (let* ((start-parts (split-string start "-"))
         (end-parts (split-string end "-"))
         (start-plist (list :year (string-to-number (nth 0 start-parts))
                           :month (string-to-number (nth 1 start-parts))
                           :day (string-to-number (nth 2 start-parts))
                           :time (if (string-empty-p time) nil time)
                           :repeater (if (string-empty-p repeater) "+1w" repeater)))
         (end-plist (list :year (string-to-number (nth 0 end-parts))
                         :month (string-to-number (nth 1 end-parts))
                         :day (string-to-number (nth 2 end-parts))))
         (events (org-expand-generate-simple start-plist end-plist)))
    
    (dolist (event events)
      (insert (format "** %s\n%s\n" headline event)))
    
    (message "Inserted %d events" (length events))))

(defun org-expand-periodic-events-in-buffer ()
  "Expand all periodic event notations in current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "<[^>]+>--<[^>]+>" nil t)
        (goto-char (match-beginning 0))
        (org-expand-periodic-event-at-point)
        (setq count (1+ count)))
      (message "Expanded %d periodic event(s)" count))))

(provide 'org-expand-periodic-events)
;;; org-expand-periodic-events.el ends here
