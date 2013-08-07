
;; User commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-colors
  (syntax-rules ()
    ((_ (col c) (col* c*) ...)
     (begin
       (set! col
         (lambda (b)
           (lambda (e)
             (if b
               (fg-color-fmt c e)
               (bg-color-fmt c e)))))
       (set! col*
         (lambda (b)
           (lambda (e)
             (if b
               (fg-color-fmt c* e)
               (bg-color-fmt c* e)))))
       ...))))


(define-syntax define-colors-hex
  (syntax-rules ()
    ((_ (col c) (col* c*) ...)
     (begin
       (set! col
         (lambda (b)
           (lambda (e)
             (if b
               (fg-color-hex-fmt c e)
               (bg-color-hex-fmt c e)))))
       (set! col*
         (lambda (b)
           (lambda (e)
             (if b
               (fg-color-hex-fmt c* e)
               (bg-color-hex-fmt c* e)))))
       ...))))


;; Write out conky config to file
(define-syntax conky
  (syntax-rules ()
    ((_ f (conf* ...) e)
     (write-conky f '(conf* ...)
       (lambda ()
         e)))
    ((_ f e)
     (write-conky f #f
       (lambda ()
         e)))))

;; Test some conky on the fly!
(define-syntax test-conky
  (syntax-rules ()
    ((_ (conf* ...) e)
     (let ((file "/tmp/conky.conf"))
       (conky file (conf* ...) e)
       (system (format-catch-bad "conky -c ~a" file))
       (delete-file file)))
    ((_ e)
     (let ((file "/tmp/conky.conf"))
       (conky file e)
       (system (format-catch-bad "conky -c ~a" file))
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
      (map (lambda (a) (format-catch-bad "~a" a)) as))))

;; Wrap expression in double quotes
(define str
  (lambda (e)
    (format-catch-bad "\"~a\"" e)))

;; YMMV
(define batt-file "/sys/class/power_supply/BAT0/status")
(define eth0-file "/sys/devices/pci0000:00/0000:00:19.0/net/eth0/operstate")
(define wlan0-file "/sys/devices/pci0000:00/0000:00:1c.1/0000:03:00.0/net/wlan0/operstate")

;; Dzen2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fg
  (lambda (c)
    (c #t)))

(define bg
  (lambda (c)
    (c #f)))

;; Colorize an expression
(define-syntax fg-color-fmt
  (syntax-rules ()
    ((_ c e)
     (format-catch-bad "^fg(~a)~a^fg()" 'c e))))

;; Colorize an expression, with a hex value
(define-syntax fg-color-hex-fmt
  (syntax-rules ()
    ((_ c e)
     (format-catch-bad "^fg(\\#~a)~a^fg()" c e))))

;; Colorize an expression
(define-syntax bg-color-fmt
  (syntax-rules ()
    ((_ c e)
     (format-catch-bad "^bg(~a)~a^bg()" 'c e))))

;; Colorize an expression, with a hex value
(define-syntax bg-color-hex-fmt
  (syntax-rules ()
    ((_ c e)
     (format-catch-bad "^bg(\\#~a)~a^bg()" c e))))

;; Make a clickable area that runs a command
;;   upon being clicked with button b (1, 2, or 3)
(define clickable
  (lambda (b c e)
    (format-catch-bad "^ca(~a,~a)~a^ca()" b c e)))

;; Handy Compound Expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax color-val
  (syntax-rules (bad high)
    ((_ v (bad badval badout)
        (l* c*)
        ...
        (high cHI))
     (case_
       ((match (str v) '== (str badval)) badout)
       ((match (str v) '< (str l*)) c*)
       ...
       (else cHI)))))


(define-syntax battery-gauge
  (syntax-rules ()
    ((_ (l* c*) ... cHI)
     (let ((batt (var battery_percent)))
       (color-val batt
         (bad (nothing) (nothing))
         (l* (c* batt))
         ...
         (high (cHI batt)))))))

(define-syntax wifi-gauge
  (syntax-rules (unk off)
    ((_ iface (unk unkc) (off offc) (l* c*) ... cHI)
     (let* ((wlan (var wireless_link_qual_perc iface))
            (wlan-perc (all wlan "%")))
       (if_ (up 'wlan0)
            (all
              (all " (" (var wireless_essid iface) ") ")
              (color-val wlan
                         (bad "unk" (unkc " DOWN"))
                         (l*   (c* wlan-perc))
                         ...
                         (high (cHI wlan-perc)))
              (all " : " (var addr iface)))
            (offc "WIFI OFF"))))))

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
    (format-catch-bad "~a~a${else}~a${endif}"
            (var->string (format-catch-bad "if_~a" t) args)
            c a)))

;; format a var
(define var->string
  (lambda (v args)
    (format-catch-bad "${~a~a" v
            (let loop ((args args))
              (cond
                ((null? args) "}")
                (else (format-catch-bad " ~a~a" (car args)
                              (loop (cdr args)))))))))

(define both
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;; Output ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (define format-catch-bad
;   (lambda (fstr . arg*)
;     (apply format fstr arg*)))

(define format-catch-bad
  (lambda (fstr . arg*)
    (let ((res (find-errors arg*)))
      (let ((p (car res))
            (args (cdr res)))
        (if p
          (error 'conky (format "Tried to print a procedure in: ~s"
                                (apply format fstr args)))
          (apply format fstr args))))))

(define find-errors
  (lambda (args)
    (cond
      ((null? args) '(#f . ()))
      (else
        (let ((arg (car args))
              (rest (cdr args)))
          (if (procedure? arg)
            (let ((res (find-errors `(,(arg "???") . ,rest))))
              `(#t . ,(cdr res)))
            (let ((res (find-errors rest)))
              `(,(car res) ,arg . ,(cdr res)))))))))

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
    (let ((output (th)))
      (with-output-to-file file
         (lambda ()
           (write-conf
             (merge-conf
               (or extra-conf '())
               default-conf))
           (display output))
         'replace))))

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

