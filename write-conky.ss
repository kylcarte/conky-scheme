(load "conky.ss")

;; Solarized theme in .Xresources
(define-colors-hex
  (yellow        "b58900")
  (orange        "cb4b16")
  (red           "dc322f")
  (magenta       "d33682")
  (violet        "6c71c4")
  (blue          "268bd2")
  (cyan          "2aa198")
  (green         "84b000")
  (base03        "002b36")
  (base02        "073642")
  (base01        "586e75")
  (base00        "657b83")
  (base0         "839496")
  (base1         "93a1a1")
  (base2         "eee8d5")
  (base3         "fdf6e3"))

(define bat
  (battery-gauge
    (10 (both (bg black) (fg red)))
    (25 (fg red))
    (75 (fg yellow))
    (fg green)))

(define wifi
  (wifi-gauge 'wlan0
    (unk (fg red))
    (off (fg red))
    (35 (bg black))
    (50 (fg red))
    (80 (fg yellow))
    (fg green)))

(conky "/home/kcarter/.conkytop"
  (all
    (network 'eth0
      (all
        "ETH : "
        (var addr 'eth0))
      (network 'wlan0
        (all
          "WIFI"
          wifi)
        ((fg red) "NO CONN")))
    " | "
    (battery-status
      (all "AC " bat)
      ((fg green) "BATT FULL")
      (all "BATT " bat)
      ((fg green) "BATT FULL"))))

(conky "/home/kcarter/.conkytime"
  ((fg orange) (var time "%I:%M %p %A, %d %B %Y")))

