# Lazarus_MineSweeper
Игра "Сапёр" на Object Pascal.

Lazarus Version 3.4, Developed and checked on OpenSuSe Linux 15.5 at 2024 year.

Для начала будет написана на Lazarus LCL, позднее адаптирую под Delphi CE FMX.

_______________________________________________________________________________

![alt text](https://github.com/adm-academic/Lazarus_MineSweeper/blob/main/screenshots/screen-1.gif?raw=true)
_______________________________________________________________________________

Стартовое состояние, после загрузки в память объекта класса T_Mine_Sweeper:
![alt text](https://github.com/adm-academic/Lazarus_MineSweeper/blob/main/screenshots/start.png?raw=true)


Состояние игры. Открыты пустые ячейки, клетки с цифрами, некоторые клетки помечены флагами.

![alt text](https://github.com/adm-academic/Lazarus_MineSweeper/blob/main/screenshots/play.png?raw=true)


Состояние проигрыша. Игрок нажал на ячейку с миной и она (условно) взорвалась. При этом все ячейки открылись.

![alt text](https://github.com/adm-academic/Lazarus_MineSweeper/blob/main/screenshots/lose.png?raw=true)


Игрок выиграл! Он открыл все ячейки кроме мин, и получил победу!

![alt text](https://github.com/adm-academic/Lazarus_MineSweeper/blob/main/screenshots/win.png?raw=true)

После победы никнейм игрока внесён в локальный список рекордов.

![alt text](https://github.com/adm-academic/Lazarus_MineSweeper/blob/main/screenshots/records.png?raw=true)


Метрики cloc для версии 1.0:
![alt text](https://github.com/adm-academic/Lazarus_MineSweeper/blob/main/screenshots/cloc.png?raw=true)


