;;; -*- lexical-binding: t; -*-
;;; Code for nejla projects

(require 'sql)

(defconst ukaa-services
  '( "eskilstuna"
     "halmstad"
     "are"
     "borlange"
     "harnosand"
     "jonkoping2"
     "lulea"
     "ostersund"
     "alvsbyn"
     "linkoping"
     "norrkoping"
     "laholm"
     "orebro"
     "uppsala"
     "tibro"
     "test"
     "vasteras"
     "ornskoldsvik"
     "sundsvall"
     "tranas"
     ))


(defun ukaa-backend-database (service)
  "Connect to a ukaa database SERVICE via Docker."
  (interactive
   (list (completing-read "Choose service: "
                          ukaa-services nil nil nil)))
  (let* ((sql-user "postgres")
         (sql-database "postgres")
         (sql-server "localhost")
         (buf-name (format "*SQL: ukaa-%s*" service))
         (default-directory
          (format "/ssh:ukaa.se|sudo:root@ukaa.se|docker:ukaa-database-%s-1:/" service)))
    (if (get-buffer buf-name)
        (pop-to-buffer buf-name)
      (sql-postgres)
      (rename-buffer buf-name))))


(require 'nejla-share)

(provide 'nejla)
