
;; User commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-colors
  (syntax-rules ()
    ((_ (col c) (col* c*) ...)
     (begin
       (set! col
         (lambda (e)
           (color c e)))
       (set! col*
         (lambda (e)
           (color c* e)))
       ...))))

(define-syntax define-colors-hex
  (syntax-rules ()
    ((_ (col hex) (c* h*) ...)
     (begin
       (set! col
         (lambda (e)
           (color-hex hex e)))
       (set! c*
         (lambda (e)
           (color-hex h* e)))
       ...))))

;; Write out conky config to file
(define-syntax conky
  (syntax-rules ()
    ((_ f (c* ...) e)
     (write-conky f '(c* ...)
       (lambda ()
         e)))
    ((_ f e)
     (write-conky f #f
       (lambda ()
         e)))))

;; Test some conky on the fly!
(define-syntax test-conky
  (syntax-rules ()
    ((_ (c* ...) e)
     (let ((file "this-is-not-the-name-of-this-file.conf"))
       (conky file (c* ...) e)
       (system (format "conky -c ~a" file))
       (delete-file file)))
    ((_ e)
     (let ((file "this-is-not-the-name-of-this-file.conf"))
       (conky file e)
       (system (format "conky -c ~a" file))
       (delete-file file)))))

;; All if expressions: if_match, if_up, etc.
(define-syntax if_
  (syntax-rules ()
    ((_ (v . as) c a)
     (if->string 'v (list . as) c a))))

;; Convenient wrapper for if_match, using conky's string comparison
(define-syntax if_match
  (syntax-rules ()
    ((_ (e1 r e2) c a)
     (if_ (match (str e1) 'r (str e2)) c a))))

;; Nested if_s
(define-syntax case_
  (syntax-rules (else)
    ((_ (else a)) a)
    ((_ (t c) e* ...)
     (if_ t c (case_ e* ...)))))

;; Basic var formatting
(define-syntax var
  (syntax-rules ()
    ((_ v . args)
     (var->string 'v (list . args)))))

;; No-op
(define nothing
  (lambda as ""))

;; Concat all expressions
(define all
  (lambda as
    (apply string-append
      (map (lambda (a) (format "~a" a)) as))))

;; Wrap expression in double quotes
(define str
  (lambda (e)
    (format "\"~a\"" e)))

;; YMMV
(define batt-file "/sys/class/power_supply/BAT0/status")
(define eth0-file "/sys/devices/pci0000:00/0000:00:19.0/net/eth0/operstate")
(define wlan0-file "/sys/devices/pci0000:00/0000:00:1c.1/0000:03:00.0/net/wlan0/operstate")

;; Dzen2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Colorize an expression
(define-syntax color
  (syntax-rules ()
    ((_ c e)
     (format "^fg(~a)~a^fg()" 'c e))))

;; Colorize an expression, with a hex value
(define-syntax color-hex
  (syntax-rules ()
    ((_ c e)
     (format "^fg(\\#~a)~a^fg()" c e))))

;; Make a clickable area that runs a command
;;   upon being clicked with button b (1, 2, or 3)
(define clickable
  (lambda (b c e)
    (format "^ca(~a,~a)~a^ca()" b c e)))

;; Handy Compound Expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax color-val
  (syntax-rules (bad low high pre post)
    ((_ v (bad badval badout)
          (pre pre-stuff)
          (post post-stuff)
          (low c0)
          (l* c*)
          ...
          (high cHI))
     (all
       pre-stuff
       (case_
         ((match (str v) '== (str badval)) badout)
         ((match (str v) '== (str 0)) c0)
         ((match (str v) '< (str l*)) c*)
         ...
         (else cHI))
       post-stuff))))


(define-syntax battery-gauge
  (syntax-rules ()
    ((_ c0 (l* c*) ... cHI)
     (let ((batt (var battery_percent)))
       (color-val batt
         (bad (nothing) (nothing))
         (pre (nothing))
         (post (nothing))
         (low (c0 batt))
         (l* (c* batt))
         ...
         (high (cHI batt)))))))

(define wifi-gauge
  (lambda (lm mh)
    (let* ((iface 'wlan0)
           (wlan (var wireless_link_qual_perc iface))
           (wlan-perc (all wlan "%")))
      (if_ (up 'wlan0)
           (color-val wlan
             (bad "unk" (yellow " DOWN"))
             (pre (all " (" (var wireless_essid iface) ") "))
             (post (all " : " (var addr iface)))
             (low  (red wlan-perc))
             (lm   (yellow wlan-perc))
             (high (green wlan-perc)))
           (red "WIFI OFF")))))

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

;; Lib Helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; format an if expression
(define if->string
  (lambda (t args c a)
    (format "~a~a${else}~a${endif}"
            (var->string (format "if_~a" t) args)
            c a)))

;; format a var
(define var->string
  (lambda (v args)
    (format "${~a~a" v
            (let loop ((args args))
              (cond
                ((null? args) "}")
                (else (format " ~a~a" (car args)
                              (loop (cdr args)))))))))

;; Output ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define write-conf
  (lambda (cs)
    (for-each
      (lambda (s)
        (display (car s))
        (let loop ((s (cdr s)))
          (cond
            ((null? s) (newline))
            (else (begin
                    (printf " ~a" (car s))
                    (loop (cdr s)))))))
      (append cs '(("") ("TEXT"))))))

(define default-conf
  '((background no)
    (out_to_console yes)
    (out_to_x no)
    (update_interval 1.0)
    (total_run_times 0)
    (use_spacer none)
    (short_units on)))

(define merge-conf
  (lambda (new def)
    (fold-right
      (lambda (c cs)
        (cons c
              (cond
                ((assq (car c) cs)
                 => (lambda (p)
                      (remove p cs)))
                (else cs))))
      def
      new)))

(define write-conky
  (lambda (file extra-conf th)
    (with-output-to-file file
      (lambda ()
        (write-conf
          (merge-conf
            (or extra-conf '())
            default-conf))
        (display (th)))
      'replace)))

;; Default colors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-colors
  (yellow        yellow      )
  (orange        orange      )
  (red           red         )
  (magenta       magenta     )
  (violet        violet      )
  (blue          blue        )
  (cyan          cyan        )
  (green         green       )
  (lightyellow   lightyellow )
  (lightorange   lightorange )
  (lightred      lightred    )
  (lightmagenta  lightmagenta)
  (lightviolet   lightviolet )
  (lightblue     lightblue   )
  (lightcyan     lightcyan   )
  (lightgreen    lightgreen  )
  (white         white       )
  (black         black       ))

