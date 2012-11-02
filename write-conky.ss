(load "conky.ss")

(conky ".conkytop"
  (all
    (network 'eth0
      (all
        "ETH : "
        (var addr 'eth0))
      (network 'wlan0
        (all
          "WIFI"
          (wifi-guage 25 75))
        (color red "NO CONN")))
    " | "
    (battery-status
      (all "AC " (battery-guage 25 75))
      (color green "BATT FULL")
      (all "BATT " (battery-guage 25 75))
      (color green "BATT FULL"))))

(conky ".conkytime"
  (color orange (var time "%I:%M %p %A, %d %B %Y")))

