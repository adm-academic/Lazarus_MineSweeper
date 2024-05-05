unit u_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, Types, Math;

type

  { Tf_main }

  Tf_main = class(TForm)
    b_start: TButton;
    dg_game: TDrawGrid;
    lb_gamestate: TLabel;
    Timer1: TTimer;
    procedure b_startClick(Sender: TObject);
    procedure dg_gameDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure dg_gameMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;


var
  f_main: Tf_main;


type
  //..
  TGame_State = ( GS_IDLE,
                  GS_PLAY,
                  GS_WIN,
                  GS_LOSE
                );
  //...
  TBottom_Cell = ( BC_EMPTY  = 0,
                   BC_NEAR_1 = 1,
                   BC_NEAR_2 = 2,
                   BC_NEAR_3 = 3,
                   BC_NEAR_4 = 4,
                   BC_NEAR_5 = 5,
                   BC_NEAR_6 = 6,
                   BC_NEAR_7 = 7,
                   BC_NEAR_8 = 8,
                   BC_INSTALLED_MINE = 10,
                   BC_EXPLODED_MINE  = 1010
                 );
  TTop_Cell    = ( TC_UNKNOWN  = 0,
                   TC_FLAGGED  = 1,
                   TC_OPENED   = 10
                 );
  TBottom_Matrix = array of array of TBottom_Cell;
  TTop_Matrix    = array of array of TTop_Cell;
  //...
  EOutsideOfMatrix = class(Exception);
  //...

var
  bottom_matrix : TBottom_Matrix; // нижняя матрица
  top_matrix : TTop_Matrix;       // верхняя матрица
  bottom_matrix_width, bottom_matrix_height, // размеры нижней матрицы
  top_matrix_width, top_matrix_height: integer; // размеры верхней матрицы
  N_mines : integer = 7; // количество располагаемых в нижней матрице мин
  game_state : TGame_State = GS_IDLE;
  first_game_turn : boolean = False;

implementation

{$R *.lfm}

{ Tf_main }

///------------------------------------------------------
function in_cell_mine(y,x:integer):boolean;
/// возвращает истину если в ячейке есть мина
/// генерирует исключение при выходе координат за пределы размерности матрицы
begin
     if ( (y<0)or(y>=bottom_matrix_height) ) or
        ( (x<0)or(x>=bottom_matrix_width)  ) then
        raise EOutsideOfMatrix.Create('Координаты y=' + IntToStr(y) +
                                      ',x=' + IntToStr(x) + ' выходят за пределы матрицы, имеющей размер '+
                                      IntToStr(bottom_matrix_height)+'x'+IntToStr(bottom_matrix_width)
                                      );
     if ( bottom_matrix[y,x]=BC_INSTALLED_MINE ) or
        ( bottom_matrix[y,x]=BC_EXPLODED_MINE )
        then
           Result:=True
     else
           Result:=False;
end;

procedure throw_mine(y,x:integer);
/// устанавливает мину в ячейку, даже если там есть другое значение, или другая мина
/// генерирует исключение при выходе координат за пределы размерности матрицы
begin
   if ( (y<0)or(y>=bottom_matrix_height) ) or
        ( (x<0)or(x>=bottom_matrix_width)  ) then
        raise EOutsideOfMatrix.Create('Координаты y=' + IntToStr(y) +
                                      ',x=' + IntToStr(x) + ' выходят за пределы матрицы, имеющей размер '+
                                      IntToStr(bottom_matrix_height)+'x'+IntToStr(bottom_matrix_width)
                                      );
   bottom_matrix[y,x]:=BC_INSTALLED_MINE;
end;

function get_neighbors_count(y,x:integer): integer;
/// генерирует исключение при выходе координат за пределы размерности матрицы
var
  dy, dx,
  ny, nx : integer;
  neighbors_counter : integer;
begin
   if ( (y<0)or(y>=bottom_matrix_height) ) or
        ( (x<0)or(x>=bottom_matrix_width)  ) then
        raise EOutsideOfMatrix.Create('Координаты y=' + IntToStr(y) +
                                      ',x=' + IntToStr(x) + ' выходят за пределы матрицы, имеющей размер '+
                                      IntToStr(bottom_matrix_height)+'x'+IntToStr(bottom_matrix_width)
                                      );
   // перед началом подсчёта установим счётчик соседних мин в ноль
   neighbors_counter:=0;
   // у ячейки есть 8 соседей, если она находится не на границе матрицы.
   // Проверяем на наличие мин всех соседних ячеек, с учётом границы.
   // Очевидно что соседние клетки могут быть определены через таблицу смещений координат.
   // (-1, -1), (-1, 0), (-1,+1)
   // ( 0, -1), ( y, x), ( 0,+1)
   // (+1, -1), (+1, 0), (+1,+1)
   // Мы выполним вычисления прямым заданием в коде, не прибегая к массиву смещений.
   // Это нарушит принцип DRY и сделает код повторяющимся,
   // но зато код будет более наглядным для понимания.
   dy:=-1; dx:=-1;
   ny:=y+dy; nx:=x+dx;
   if ( (ny>=0) and (ny<bottom_matrix_height) ) and
      ( (nx>=0) and (nx<bottom_matrix_width) ) then
        if ( in_cell_mine(ny,nx) )  then
           inc( neighbors_counter );

   dy:= 0; dx:=-1;
   ny:=y+dy; nx:=x+dx;
   if ( (ny>=0) and (ny<bottom_matrix_height) ) and
      ( (nx>=0) and (nx<bottom_matrix_width) ) then
        if ( in_cell_mine(ny,nx) )  then
           inc( neighbors_counter );

   dy:=+1; dx:=-1;
   ny:=y+dy; nx:=x+dx;
   if ( (ny>=0) and (ny<bottom_matrix_height) ) and
      ( (nx>=0) and (nx<bottom_matrix_width) ) then
        if ( in_cell_mine(ny,nx) )  then
           inc( neighbors_counter );

   dy:=+1; dx:= 0;
   ny:=y+dy; nx:=x+dx;
   if ( (ny>=0) and (ny<bottom_matrix_height) ) and
      ( (nx>=0) and (nx<bottom_matrix_width) ) then
        if ( in_cell_mine(ny,nx) )  then
           inc( neighbors_counter );

   dy:=+1; dx:=+1;
   ny:=y+dy; nx:=x+dx;
   if ( (ny>=0) and (ny<bottom_matrix_height) ) and
      ( (nx>=0) and (nx<bottom_matrix_width) ) then
        if ( in_cell_mine(ny,nx) )  then
           inc( neighbors_counter );

   dy:= 0; dx:=+1;
   ny:=y+dy; nx:=x+dx;
   if ( (ny>=0) and (ny<bottom_matrix_height) ) and
      ( (nx>=0) and (nx<bottom_matrix_width) ) then
        if ( in_cell_mine(ny,nx) )  then
           inc( neighbors_counter );

   dy:=-1; dx:=+1;
   ny:=y+dy; nx:=x+dx;
   if ( (ny>=0) and (ny<bottom_matrix_height) ) and
      ( (nx>=0) and (nx<bottom_matrix_width) ) then
        if ( in_cell_mine(ny,nx) )  then
           inc( neighbors_counter );

   dy:=-1; dx:= 0;
   ny:=y+dy; nx:=x+dx;
   if ( (ny>=0) and (ny<bottom_matrix_height) ) and
      ( (nx>=0) and (nx<bottom_matrix_width) ) then
        if ( in_cell_mine(ny,nx) )  then
           inc( neighbors_counter );

   // положим в возвращаемую ячейку памяти подсчитанное нами число соседей для ячейки
   Result:=neighbors_counter;
end;

procedure generate_bottom_matrix( mheight,mwidth:integer );
/// генерируем нижнюю матрицу
var
  i, yy, xx : integer;
  mine_installed: boolean = False;
begin
  /// установим размерность нижней матрицы и перераспределим оперативную память
  bottom_matrix_width:=mwidth;
  bottom_matrix_height:=mheight;
  SetLength(bottom_matrix, bottom_matrix_height , bottom_matrix_width );

  /// окей, теперь нужно очистить нижнюю матрицу, заполнив её
  /// безмятежно чистою водою в каждой из ячеек матрицы
  for yy:=0 to bottom_matrix_height-1 do
    for xx:=0 to bottom_matrix_width-1 do
      begin
         bottom_matrix[yy,xx] := BC_EMPTY;
      end;

  // теперь расположим на нижней матрице N_mines мин, но так чтобы ни одна мина
  // не попала на другую при установке и чтобы их на матрице было ровно N_mines
  for i:=1 to N_mines do
    begin
       mine_installed:=False;
       while not mine_installed do
       begin
          yy := RandomRange(0,bottom_matrix_height);
          xx := RandomRange(0,bottom_matrix_width);
          if ( in_cell_mine(yy,xx) ) then
             continue;
          throw_mine(yy,xx);
          mine_installed := True;
       end;
    end;
  /// на нижней матрице расставлены мины !

  // теперь инициализируем ячейки с циферками !
  for yy:=0 to bottom_matrix_height-1 do
    for xx:=0 to bottom_matrix_width-1 do
      begin
         if not in_cell_mine(yy,xx) then // если в текущей ячейке нету мины
           if get_neighbors_count(yy,xx)<>0 then // если кол-во соседей для текущей ячеки неравно нулю
             bottom_matrix[yy,xx]:= // В текущую ячейку нужно поместить перечисление, которое кодирует число соседей
                   TBottom_Cell(get_neighbors_count(yy,xx)); // Воспользуемся нашими хитрыми значениями указанными для перечисления
                                                             // TBottom_Cell и с помощью приведения типов преобразуем число соседей
                                                             // текущей ячейки в конкретное значение перечисления.

      end;
end;

procedure generate_top_matrix( mheight,mwidth:integer );
var
   i, yy, xx : integer;
begin
  /// установим размерность верхней матрицы и перераспределим оперативную память
  top_matrix_width:=mwidth;
  top_matrix_height:=mheight;
  SetLength(top_matrix, top_matrix_height , top_matrix_width );

  /// окей, теперь нужно очистить верхнюю матрицу, заполнив её
  /// пустыми блоками.
  for yy:=0 to top_matrix_height-1 do
    for xx:=0 to top_matrix_width-1 do
      begin
         top_matrix[yy,xx] := TC_UNKNOWN;
      end;
end;

// рекурсивная функция открывает текущую и все соседние
// клетки, в которых нет мин
// клетку с миной эта процедура не обрабатывает !
procedure open_cell( y, x : integer);
begin
 if (y<0) or (y>=top_matrix_height) or  // если координата неправильная - выходим из процедуры
    (x<0) or (x>=top_matrix_width) or
             (y>=bottom_matrix_height) or
             (y>=bottom_matrix_width) then
               exit;

 if (top_matrix[y,x]=TC_OPENED) then  //если клетка уже открытая - выходим из процедуры
   exit;

 if ( first_game_turn ) then  // первый ход должен не попадать на мину,
                              // если попал - перегенерируем всю нижнюю матрицу
   begin
     while in_cell_mine(y,x) do
       begin
         generate_bottom_matrix(bottom_matrix_height,bottom_matrix_width);
       end;
     first_game_turn := false;
   end;

 if (bottom_matrix[y,x] = BC_EMPTY) then
   begin
    top_matrix[y,x] := TC_OPENED;
    // (-1, -1), (-1, 0), (-1,+1)
    // ( 0, -1), ( y, x), ( 0,+1)
    // (+1, -1), (+1, 0), (+1,+1)
    open_cell(y-1,x-1);open_cell(y-1,x+0);open_cell(y-1,x+1);
    open_cell(y+0,x-1);                   open_cell(y+0,x+1);
    open_cell(y+1,x-1);open_cell(y+1,x+0);open_cell(y+1,x+1);
   end
 else if ( QWord(bottom_matrix[y,x])>=QWord(BC_NEAR_1) )
     and ( QWord(bottom_matrix[y,x])<=QWord(BC_NEAR_8) ) then
   begin
    top_matrix[y,x] := TC_OPENED;
   end;
end;

///------------------------------------------------------

procedure Tf_main.dg_gameDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  y,x,i: integer;
  painting_str : string;
  ts : TTextStyle;
begin

  if (game_state<>GS_PLAY) and
     (game_state<>GS_WIN)  and
     (game_state<>GS_LOSE) then
    exit;

  y := aRow;
  x := aCol;

  case top_matrix[y,x] of
   TC_UNKNOWN : painting_str:='[ ]';
   TC_FLAGGED : painting_str:='P';
   TC_OPENED  : begin
                  case bottom_matrix[y,x] of
                    BC_EMPTY  : painting_str:='~' ;
                    BC_NEAR_1 : painting_str:='1' ;
                    BC_NEAR_2 : painting_str:='2' ;
                    BC_NEAR_3 : painting_str:='3' ;
                    BC_NEAR_4 : painting_str:='4' ;
                    BC_NEAR_5 : painting_str:='5' ;
                    BC_NEAR_6 : painting_str:='6' ;
                    BC_NEAR_7 : painting_str:='7' ;
                    BC_NEAR_8 : painting_str:='8' ;
                    BC_INSTALLED_MINE : painting_str:='*' ;
                    BC_EXPLODED_MINE  : painting_str:='X' ;
                  end;
                end;
  end;
  ts :=  dg_game.Canvas.TextStyle;
  ts.Alignment:=taCenter;
  ts.Layout:=tlCenter;

  dg_game.Canvas.TextRect(aRect,
                        aRect.Left+aRect.Width div 2,
                        aRect.Top+aRect.Height div 2,
                        painting_str,
                        ts
                        );

end;

procedure Tf_main.dg_gameMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  aRow, aCol: integer;
  yy, xx : integer;
begin
   // мышь обрабатываем только если режим игры GS_PLAY;
   if ( game_state<>GS_PLAY) then exit;
   // в эту процедуру передаются абсолютные координаты мыши всего DrawGrid-а
   // нам нужно получить координаты ячейки, по которой щёлкнул пользователь
   // что мы и делаем ниже...
   aRow := Y div self.dg_game.DefaultRowHeight;
   if (aRow<0) then aRow:=0;
   if (aRow>=self.dg_game.DefaultRowHeight-1) then aRow:=self.dg_game.DefaultRowHeight-1;
   aCol := X div self.dg_game.DefaultColWidth;
   if (aCol<0) then aCol:=0;
   if (aCol>=self.dg_game.DefaultColWidth-1) then aCol:=self.dg_game.DefaultRowHeight-1;

   // здесь нам нужно ввести основной геймплей, то есть реакцию сапёра на нажатие кнопок
   // будут работать только лэфт-клик и райт-клик, одновременный клик двух клавиш из оригинального
   // сапёра я реализовывать не буду из-за относительной сложности нужного для этого кода.

   // открываем ячейку, если внизу пусто, то рекурсивно открываем всех соседний ячейки
   // всё в точности как в оригинальном сапёре
   if ( Button=mbLeft ) then
     if ( top_matrix[aRow,aCol]=TC_UNKNOWN ) then
       begin
        if ( not in_cell_mine(aRow,aCol) ) then // если внизу нету мины
           open_cell(aRow,aCol) // откроем ячейку, рекурсивно если это нужно
        else             // иначе, если внизу мина
           begin
             top_matrix[aRow,aCol]:=TC_OPENED;  // откроем ячейку
             bottom_matrix[aRow,aCol]:=BC_EXPLODED_MINE; // мину переключим на подорваную мину
             //!!!!!!!--------- обработка проигрыша в игре ------------------
             game_state:=GS_LOSE;
             for yy:=0 to top_matrix_height-1 do
             for xx:=0 to top_matrix_width-1 do
               top_matrix[yy,xx]:=TC_OPENED;
             self.Repaint;
             //!!!!!!!--------- обработка проигрыша в игре ------------------
           end;
       end;

   // помечаем ячейку флагом или убираем флаг
   if ( Button=mbRight ) then
     if ( top_matrix[aRow,aCol]=TC_UNKNOWN ) then
       top_matrix[aRow,aCol]:=TC_FLAGGED
     else if ( top_matrix[aRow,aCol]=TC_FLAGGED ) then
       top_matrix[aRow,aCol]:=TC_UNKNOWN;

   //!!!!!!!----------- обработка выигрыша в игре ----------------

   //!!!!!!!----------- обработка выигрыша в игре ----------------

   // даём команду на перерисовку всего грида
   self.dg_game.Repaint;

end;

procedure Tf_main.b_startClick(Sender: TObject);
begin

  generate_bottom_matrix(self.dg_game.RowCount,self.dg_game.ColCount);
  generate_top_matrix(self.dg_game.RowCount,self.dg_game.ColCount);
  game_state:=GS_PLAY;
  first_game_turn:=True;
  self.Repaint;
end;


procedure Tf_main.FormCreate(Sender: TObject);
begin
  Randomize;
end;

procedure Tf_main.FormPaint(Sender: TObject);
var
  str_state: string = '';
begin
  case game_state of
    GS_IDLE : str_state:='ОЖИДАНИЕ';
    GS_PLAY : str_state:='ИГРАЕМ';
    GS_LOSE : str_state:='ИГРА ПРОИГРАНА !';
    GS_WIN  : str_state:='ВЫ ВЫИГРАЛИ !';
  end;
  self.lb_gamestate.Caption := str_state;
end;

procedure Tf_main.Timer1Timer(Sender: TObject);
begin
  self.dg_game.Repaint;
end;

end.

