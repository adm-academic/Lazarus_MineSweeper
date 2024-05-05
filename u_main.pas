unit u_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, Types, Math;

type

  { Tf_main }

  Tf_main = class(TForm)
    b_bottom_matrix_generate: TButton;
    b_top_matrix_generate: TButton;
    dg_game: TDrawGrid;
    Timer1: TTimer;
    tb_draw_bottom_matrix: TToggleBox;
    tb_draw_top_matrix: TToggleBox;
    tb_draw_mixed_matrix: TToggleBox;
    procedure b_bottom_matrix_generateClick(Sender: TObject);
    procedure b_top_matrix_generateClick(Sender: TObject);
    procedure dg_gameDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure tb_draw_bottom_matrixChange(Sender: TObject);
    procedure tb_draw_mixed_matrixChange(Sender: TObject);
    procedure tb_draw_top_matrixChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;


var
  f_main: Tf_main;


type
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
  paint_bottom_matrix : boolean = False; // флаг отрисовки нижней матрицы
  paint_top_matrix : boolean = False;    // флаг отрисовки верхнйе матрицы
  paint_mixed_matrix : boolean = False;  // флаг смешаной отрисовки матриц, как полагается игре сапёр
  N_mines : integer = 7; // количество располагаемых в нижней матрице мин

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

///------------------------------------------------------

procedure Tf_main.dg_gameDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
/// генерирует исключение при выходе координат за пределы размерности матрицы
var
  y,x,i: integer;
  painting_str : string;
  ts : TTextStyle;
begin
  //dg_game.Canvas.Brush.Style:=bsSolid;
  //dg_game.Canvas.Brush.Color:=RandomRange( -$7FFFFFFF-1,$7FFFFFFF );
  //dg_game.Canvas.Ellipse(aRect);
  y := aRow;
  x := aCol;

  if paint_mixed_matrix then
    begin
      ShowMessage('paint mixed matrix');
    end
  else if paint_top_matrix then
    begin
      ShowMessage('paint top matrix');
    end
  else if paint_bottom_matrix then
    begin
      if ( (y<0)or(y>=bottom_matrix_height) ) or
        ( (x<0)or(x>=bottom_matrix_width)  ) then
        raise EOutsideOfMatrix.Create('Координаты y=' + IntToStr(y) +
                                      ',x=' + IntToStr(x) + ' выходят за пределы матрицы, имеющей размер '+
                                      IntToStr(bottom_matrix_height)+'x'+IntToStr(bottom_matrix_width)
                                      );
      case bottom_matrix[y,x] of
        BC_EMPTY  : painting_str:='~~~' ;
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


      dg_game.Canvas.Brush.Style:=bsSolid;
      dg_game.Canvas.Brush.Color:=clAqua;
      dg_game.Canvas.FillRect(aRect);

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

end;

procedure Tf_main.b_bottom_matrix_generateClick(Sender: TObject);
begin
  generate_bottom_matrix(self.dg_game.RowCount,self.dg_game.ColCount);
end;

procedure Tf_main.b_top_matrix_generateClick(Sender: TObject);
begin
  generate_top_matrix(self.dg_game.RowCount,self.dg_game.ColCount);
end;

procedure Tf_main.tb_draw_mixed_matrixChange(Sender: TObject);
begin
  if ( (Sender as TToggleBox).Checked ) then
    paint_mixed_matrix:=true
  else
    paint_mixed_matrix:=false;
end;

procedure Tf_main.tb_draw_bottom_matrixChange(Sender: TObject);
begin
  if ( (Sender as TToggleBox).Checked ) then
    paint_bottom_matrix:=true
  else
    paint_bottom_matrix:=false;
end;

procedure Tf_main.tb_draw_top_matrixChange(Sender: TObject);
begin
  if ( (Sender as TToggleBox).Checked ) then
    paint_top_matrix:=true
  else
    paint_top_matrix:=false;
end;


procedure Tf_main.FormCreate(Sender: TObject);
begin
  Randomize;
end;



procedure Tf_main.Timer1Timer(Sender: TObject);
begin
  self.dg_game.Repaint;
end;

end.

