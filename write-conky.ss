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
    red
    (25 red)
    (75 yellow)
    green))

(conky "/home/kcarter/.conkytop"
  (all
    (network 'eth0
      (all
        "ETH : "
        (var addr 'eth0))
      (network 'wlan0
        (all
          "WIFI"
          (wifi-gauge 25 75))
        (red "NO CONN")))
    " | "
    (battery-status
      (all "AC " bat)
      (green "BATT FULL")
      (all "BATT " bat)
      (green "BATT FULL"))))

(conky "/home/kcarter/.conkytime"
  (orange (var time "%I:%M %p %A, %d %B %Y")))

