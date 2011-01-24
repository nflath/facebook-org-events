 (require 'url)
(require 'json)
(require 'assoc)

;;https://www.facebook.com/dialog/oauth?client_id=YOUR_APP_ID&redirect_uri=YOUR_URL&scope=email,read_stream
;;http://YOUR_URL?code=A_CODE_GENERATED_BY_SERVER

;;https://www.facebook.com/dialog/oauth?client_id=YOUR_APP_ID&redirect_uri=YOUR_URL&response_type=token


(defun callback (&rest args)
  (save-window-excursion
    (search-forward "{")
    (backward-char)
    (kill-ring-save (point) (point-max))
    (let ((events (cdr (assoc 'data (json-read))))
          (x 0))
      (switch-to-buffer "*FB Events*.org")
      (while (< x (length events))
        (let* ((event (elt events x))
               (title (concat "* " (cdr (assoc 'name event)) "\n"))
               (location (if (assoc 'location event)
                             (concat (cdr (assoc 'location event))) (insert "\n")
                             ""))
               (start_date (date-for-fb-time (cdr (assoc 'start_time event))))
               (end_date (date-for-fb-time (cdr (assoc 'end_time event))))
               (start_time (time-for-fb-time (cdr (assoc 'end_time event))))
               (end_time (time-for-fb-time (cdr (assoc 'end_time event)))))
          (if (string-equal start_date end_date)
              ;;single-day event
              (insert (concat title
                              (time-string-single-day
                               start_date
                               start_time
                               end_time)
                              "\n"
                              location
                              "\n"))
            ;;multi-day event
            (let ((times (org-get-days-for-event start_date start_time end_date end_time)))
              (while (car times)
                (insert (concat title
                                (car times)
                                "\n"
                                location
                                "\n"))
                (setq times (cdr times)))))
          (setq x (1+ x)))))))

(format-time-string "%Y-%m-%d" (org-time-from-absolute (org-time-string-to-absolute "2010-10-09")))

(defun org-get-days-for-event (start_date start_time end_date end_time)
  (let ((retn (list (time-string-single-day start_date start_time "24:00")))
        (cur (1+ (org-time-string-to-absolute start_date))))
    (while (not (equal (org-time-string-to-absolute end_date) cur))
      (add-to-list
       'retn
       (time-string-single-day
        (format-time-string "%Y-%m-%d" (org-time-from-absolute cur))
        "00:00"
        "24:00"))
      (setq cur (1+ cur)))
    (append (list (time-string-single-day end_date "00:00" end_time)) retn)))

(defun date-for-fb-time (time)
  (substring time 0 10))

(defun time-for-fb-time (time)
  (substring time 11 16))

(defun time-string-single-day (date start_time end_time)
  (concat "<"
          date
          " "
          start_time
          "-"
          end_time
          ">"))