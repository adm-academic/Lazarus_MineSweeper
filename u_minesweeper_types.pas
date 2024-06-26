﻿{
 Для исключения циклических зависимостей в модулях программы, выделил основные типы данных
 в этот отдельный модуль.
}
unit u_minesweeper_types;

{$ifdef FPC}

{$mode delphi}
{$H+}

{$endif}

interface

uses
{$ifdef FPC}
   Classes, SysUtils, Graphics ;
{$else}
   Classes, SysUtils, FMX.Graphics ;
{$endif}


type
  { Тип перечисления для обозначения сложности игры }
  T_Game_Difficulty = ( GD_EASY=1,
                        GD_MEDIUM=2,
                        GD_HARD=3,
                        GD_CUSTOM=4 );

  // здесь хранится состояние игры, состояния игры - взаимоисключающие, игра может
  // находится только в одном из указаных состояний
  TGame_State = ( GS_IDLE,
                  GS_PLAY,
                  GS_WIN,
                  GS_LOSE
                );

  T_Mine_Sweeper_State_Change_Callback = procedure( new_state : TGame_State )of object; // тип коллбэка

  { Я предлагаю рассматривать игровое поле как комбинацию двух наложеных
   друг на друга матриц из ячеек. }
  // Нижняя матрица хранит значения открытых клеток где каждая ячеек может
  // иметь одно из значений перечислимого типа TBottom_Cell
  TBottom_Cell = ( BC_EMPTY  = 0, // ячеека не соседствует с минами
                   BC_NEAR_1 = 1, // ячеека соседствует с 1 миной
                   BC_NEAR_2 = 2, // ячеека соседствует с 2 минами
                   BC_NEAR_3 = 3, // ячеека соседствует с 3 минами
                   BC_NEAR_4 = 4, // ячеека соседствует с 4 минами
                   BC_NEAR_5 = 5, // ячеека соседствует с 5 минами
                   BC_NEAR_6 = 6, // ячеека соседствует с 6 минами
                   BC_NEAR_7 = 7, // ячеека соседствует с 7 минами
                   BC_NEAR_8 = 8, // ячеека соседствует с 8 минами
                   BC_INSTALLED_MINE = 10, // это ячейка с установленой миной
                   BC_EXPLODED_MINE  = 1010 // это ячейка с подорвавшейся миной
                 );
  // Вехняя матрица хранит ячейки типа TTop_Cell, который также перечислимый тип
  TTop_Cell    = ( TC_UNKNOWN  = 0, // пока неизвестная ячейка, закрытая
                   TC_FLAGGED  = 1, // ячейка помеченая флагом
                   TC_OPENED   = 10 // ячейка открытая. То есть такая ячейка не прорисовывается, а вместо
                                     // неё прорисовывается соответсвующая ячейка из нижней матрицы
                 );
  // собственно тип нижней матрицы. динамический двумерный массив
  TBottom_Matrix = array of array of TBottom_Cell;
  // также и тип верхней матрицы. динамический двумерный массив
  TTop_Matrix    = array of array of TTop_Cell;

  TCoord = record // тип для хранения двумерных координат
    y : integer;
    x : integer;
  end;

  TChordCoord = record // тип для хранения ячеек аккорда
    y : integer;
    x : integer;
    has_flag : boolean; // истина есть это ячейка аккорда с флагом
  end;

  TChord_Array = array of TChordCoord; // тип для хранения и передачи аккордов внутри приложения,
                                       // динамический массив

  { тип для хранения состояния ЗАЖАТЫХ клавиш мыши, нужен для корректной работы функции OnMouseMove }
  TMouse_Keys_Active = (
                      MKA_NONE  = 0,
                      MKA_LEFT  = 1,
                      MKA_RIGHT = 2,
                      MKA_BOTH  = 3
  );

  {$IfDef FPC}
  PPortableNetworkGraphic = ^TPortableNetworkGraphic;// указатель на объект PNG-картинки
  {$Else}
  // ...
  {$EndIf}




implementation

end.

