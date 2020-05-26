# Digital Photo Frame Project using Raspi

1. Aliexpress: Latumab New Kit for LP156WF1-TLA1 TV+HDMI+VGA+USB LCD LED screen Controller Driver Board
2. Salvaged LCD screen from DELL 1555
3. 

## Slideshow controls for Raspberry Pi

1. Added following scripts to */etc/xdg/lxsession/LXDE-pi/autostart* for startup

```
@/bin/sh /home/pi/runslideshow.sh
@/bin/sh /home/pi/slideshow/slide_control.sh
```

2. Integrated with home assistant to make REST endpoint calls for PREV/PAUSE/NEXT controls on feh

## TODO: 

- Add AI based photo cropping - DONE - YaY 😊
- Add Resume Power-on state after reboot



