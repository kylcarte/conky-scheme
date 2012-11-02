
(define-syntax test-conky
  (syntax-rules ()
    ((_ e)
     (let ((file "nothing-should-ever-be-named-this.conf"))
       (with-output-to-file file
         (lambda ()
           (conf)
           (display e))
         'replace)
       (system (format "conky -c ~a" file))
       (delete-file file)))))

(define-syntax conky
  (syntax-rules ()
    ((_ f e)
     (write-conky f
       (lambda ()
         e)))))

(define-syntax if_
  (syntax-rules ()
    ((_ (v . as) c a)
     (if->string 'v (list . as) c a))))

(define-syntax if_match
  (syntax-rules ()
    ((_ (e1 r e2) c a)
     (if_ (match (str e1) 'r (str e2)) c a))))

(define-syntax case_
  (syntax-rules (else)
    ((_ (else a)) a)
    ((_ (t c) e* ...)
     (if_ t c (case_ e* ...)))))

(define-syntax var
  (syntax-rules ()
    ((_ v . args)
     (var->string 'v (list . args)))))

(define var->string
  (lambda (v args)
    (format "${~a~a" v
            (let loop ((args args))
              (cond
                ((null? args) "}")
                (else (format " ~a~a" (car args)
                              (loop (cdr args)))))))))

(define nothing
  (lambda as ""))

(define all
  (lambda as
    (apply string-append
      (map (lambda (a) (format "~a" a)) as))))

(define if->string
  (lambda (t args c a)
    (format "~a~a${else}~a${endif}"
            (var->string (format "if_~a" t) args)
            c a)))

(define-syntax color
  (syntax-rules ()
    ((_ c e)
     (format "^fg(~a)~a^fg()" 'c e))))

(define clickable
  (lambda (b c e)
    (format "^ca(~a,~a)~a^ca()" b c e)))

(define str
  (lambda (e)
    (format "\"~a\"" e)))

(define color-val
  (lambda (v badval badout pre low lm med mh high post)
    (if_match (v == badval)
      badout
      (all
        pre
        (if_match (v == 100)
          high
          (if_match (v < lm)
            low
            (if_match (v < mh)
              med
              high)))
        post))))

(define battery-guage
  (lambda (lm mh)
    (let ((batt (var battery_percent)))
      (color-val batt
                 (nothing)
                 (nothing)
                 (nothing)
                 (color red batt)
                 lm
                 (color yellow batt)
                 mh
                 (color green batt)
                 (nothing)))))

(define wifi-guage
  (lambda (lm mh)
    (let* ((iface 'wlan0)
           (wlan (var wireless_link_qual_perc iface))
           (wlan-perc (all wlan "%")))
      (if_ (up 'wlan0)
           (color-val wlan
                      "unk"
                      (color yellow " DOWN")
                      (all " (" (var wireless_essid iface) ") ")
                      (color red wlan-perc)
                      lm
                      (color yellow wlan-perc)
                      mh
                      (color green wlan-perc)
                      (all " : " (var addr iface)))
           (color red "WIFI OFF")))))

(define battery-status
  (lambda (c f d u)
    (case_
      ((existing batt-file "Charging") c)
      ((existing batt-file "Full") f)
      ((existing batt-file "Discharging") d)
      (else u))))

(define network
  (lambda (iface up down)
    (if_match ((var addr iface) == "No Address")
              down
              up)))

(define file-match
  (lambda (f e c a)
    (if_match ((var head f 1) == (all e "\n"))
              c
              a)))

(define conf
  (lambda ()
    (for-each
      (lambda (s)
        (begin
          (display s)
          (newline)))
      '("background no"
        "out_to_console yes"
        "out_to_x no"
        "update_interval 1.0"
        "total_run_times 0"
        "use_spacer none"
        "short_units on"
        ""
        "TEXT"))))

(define write-conky
  (lambda (file th)
    (with-output-to-file file
      (lambda ()
        (conf)
        (display (th)))
      'replace)))

(define batt-file "/sys/class/power_supply/BAT0/status")
(define eth0-file "/sys/devices/pci0000:00/0000:00:19.0/net/eth0/operstate")
(define wlan0-file "/sys/devices/pci0000:00/0000:00:1c.1/0000:03:00.0/net/wlan0/operstate")

