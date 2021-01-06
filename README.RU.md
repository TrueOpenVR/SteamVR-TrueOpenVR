[![EN](https://user-images.githubusercontent.com/9499881/33184537-7be87e86-d096-11e7-89bb-f3286f752bc6.png)](https://github.com/TrueOpenVR/SteamVR-TrueOpenVR) 
[![RU](https://user-images.githubusercontent.com/9499881/27683795-5b0fbac6-5cd8-11e7-929c-057833e01fb1.png)](https://github.com/TrueOpenVR/SteamVR-TrueOpenVR/blob/master/README.RU.md) 
# SteamVR
SteamVR драйвер, с поддержкой TrueOpenVR


**[Загрузить](https://github.com/TrueOpenVR/SteamVR-TrueOpenVR/releases)**
## Центрирование
**Шлем** - "CTRL" + "ALT" + "R" or "Numpad 5"

**Шлем и контроллеры** - "Grip" + "System" на обоих контроллерах.

## Параметры конфигурационного файла
Название | Описание
------------ | -------------
DistanceBetweenEyes | Дистанция между стерео изображениями, чем больше, тем ближе.
DistortionK1, DistortionK2 | Коэффициенты дисторсии линз.
ScreenOffsetX | Горизонтальный сдвиг стерео изображений
ZoomHeight, ZoomWidth | Коэффициенты масштабирования стереоизображений.
FOV | Градус поля зрения. Можно увеличить, в зависимости от линз VR гарнитуры.
displayFrequency | Частота обновления экрана.
renderWidth, renderHeight | Разрешение рендеринга изображения для одного глаза (для монорежима можно указать полное разрешение). В зависимости от дисторсии линз может быть увеличен. Рекомендуемое значение это разрешение монитора (1080, 1440 и так далее), умноженное на 1.235 (число должно быть целым).
windowWidth, windowHeight | Высота и ширина окна для VR дисплея, должно соотвествовать разрешению дисплея.
windowX, windowY | Смещение окна, требуется для отображения на других мониторах (расширенных). Например, для отображения на втором дисплее, который отображается справа, нужно указать значение 1920 (при условии, что первый дисплей имеет разрешение 1920 на 1080). Точные данные можно просмотреть, с помощью [MultiMonitorTool утилиты](https://www.nirsoft.net/utils/multi_monitor_tool.html), которая также может выключать и включить второй монитор, через bat-файл.
Stereo | Режим стерео или моно (общий на оба глаза) для тестирования, использования шлемов без линз и перегородки, и так далее.
DebugMode | Режим отладки, залоченный на 30 FPS. После проверки рекомендуется отключить его, задав ему значение false (выключено).

Путь конфига - "...\Steam\steamapps\common\SteamVR\drivers\tovr\resources\settings\default.vrsettings".


## Настройка
1. Установите [SteamVR](https://store.steampowered.com/app/250820/SteamVR/).
2. Запускаем SteamVR и окне статуса SteamVR жмём -> Настройки комнаты -> Маленькая комната -> Калибровка -> 170 см. SteamVR демонстрация может быть закрыта, а запуск SteamVR Home может быть отключен в настройках SteamVR.
3. Put on HMD and controllers, then centering them if necessary.