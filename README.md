# SteamVR
An SteamVR driver with TrueOpenVR standard support.


**[Download](https://github.com/TrueOpenVR/SteamVR-TrueOpenVR/releases)**
## Centring
**HMD** - "CTRL" + "ALT" + "R" or "Numpad 5"

**HMD and controllers** - "Grip" + "System" on two controllers.

## Configuration file options
Name | Description
------------ | -------------
DistanceBetweenEyes | the distance between stereo images, the larger the closer.
DistortionK1, DistortionK2 | lens distortion factors.
ScreenOffsetX | horizontal image shift.
ZoomHeight, ZoomWidth | scaling factors of stereo images.
displayFrequency | screen refresh rate.
renderWidth, renderHeight | image rendering resolution for one eye.
windowWidth, windowHeight | height and width of the displayed window.
windowX, windowY | window offset is required for display on other monitors. For example, to display on the second display, which is displayed on the left, you need to specify the value 1920 (provided that the first display we have is 1920 by 1080).
Stereo | stereo or mono mode (two on one eye) for testing or etc.
DebugMode | debug mode, locked at 30 FPS. After checking, it is recommended to set it to false (disable).

Config path - "...\Steam\steamapps\common\SteamVR\drivers\tovr\resources\settings".
