[![EN](https://user-images.githubusercontent.com/9499881/33184537-7be87e86-d096-11e7-89bb-f3286f752bc6.png)](https://github.com/TrueOpenVR/SteamVR-TrueOpenVR) 
[![RU](https://user-images.githubusercontent.com/9499881/27683795-5b0fbac6-5cd8-11e7-929c-057833e01fb1.png)](https://github.com/TrueOpenVR/SteamVR-TrueOpenVR/blob/master/README.RU.md) 
# SteamVR
An SteamVR driver with TrueOpenVR standard support.


**[Download](https://github.com/TrueOpenVR/SteamVR-TrueOpenVR/releases)**
## Centring
**HMD** - "CTRL" + "ALT" + "R" or "Numpad 5"

**HMD and controllers** - "Grip" + "System" on two controllers.

## Configuration file options
Name | Description
------------ | -------------
DistanceBetweenEyes | The distance between stereo images, the larger the closer.
DistortionK1, DistortionK2 | Lens distortion factors.
ScreenOffsetX | Horizontal stereo image shift.
ZoomHeight, ZoomWidth | Scaling factors of stereo images.
FOV | Degree of field of view. You can zoom in, depending on the VR headset lenses.
displayFrequency | Screen refresh rate.
renderWidth, renderHeight | Image rendering resolution for one eye (for a mono mode can specify the full resolution). Depending on the lens distortion, it can be increased. The recommended value is the monitor resolution (1080, 1440, and so on) multiplied by 1.235 (the number must be an integer).
windowWidth, windowHeight | The height and width of the window for the VR display, should meet the display resolution.
windowX, windowY | Window offset is required for display on other monitors. For example, to display on the second display, which is displayed on the right, you need to specify the value 1920 (provided that the first display has a resolution of 1920 by 1080). The exact data can be viewed using the [MultiMonitorTool utility](https://www.nirsoft.net/utils/multi_monitor_tool.html), and also with it you can turn off and turn on the second monitor via a bat file.
Stereo | Stereo or mono mode (common to both eyes) for testing, using HMD without lenses and partitions, and so on.
DebugMode | Debug mode, locked at 30 FPS. After checking, it is recommended to set it to false (disable).

Config path - "...\Steam\steamapps\common\SteamVR\drivers\tovr\resources\settings\default.vrsettings".


## Setup
1. Install [SteamVR](https://store.steampowered.com/app/250820/SteamVR/).
2. SteamVR starts and in the SteamVR status window click -> Room settings -> Small room -> Calibration -> 170 cm. The SteamVR tutorial can be closed, and SteamVR Home is disabled in the SteamVR settings.
3. Put on HMD and controllers, then centering them if necessary.