unit u_mine_sweeper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  Grids, ExtCtrls, StdCtrls, Types, Math
  ,u_minesweeper_types
  ,u_asset_pack;

const
  { временно здесь константы игры, которые потом будут браться из объекта настроек или непосредственно из интерфейса, надо будет подумать ! }
  bottom_matrix_width = 10;
  bottom_matrix_height= 10;
  top_matrix_width  = bottom_matrix_width;
  top_matrix_height = bottom_matrix_width;
  matrix_width = bottom_matrix_width;
  matrix_height = bottom_matrix_height;
  N_mines = 7;

  {
    чтобы найти координаты соседей у клеки с координатами py, px достаточно прибавить к py,px смещения
    указанные ниже. Для более быстрого доступа к этим смещениям определена типизированная константа-массив
    (-1, -1), (-1, 0),   (-1,+1)
    ( 0, -1), ( py, px), ( 0,+1)
    (+1, -1), (+1, 0),   (+1,+1)
  }
  square_offsets : array [1..8] of TCoord =(
                                          (y:-1;x:0; ), (y:-1;x:-1;), (y:0; x:-1;),
                                          (y:+1;x:-1;), (y:+1;x:0; ), (y:+1;x:+1;),
                                          (y:0; x:+1;), (y:-1;x:+1;)
                                           );

type

  // класс эксепшена для вызова в случае, если в подпрограмму переданы некорректные координаты ячеек верхней или нижней матрицы
  EOutsideOfMatrix = class(Exception);

  PForm = ^TForm; // тип указетеля для формы
  PDrawGrid = ^TDrawGrid; // тип указателя на дро-грид

  { Класс (подразумевается единичное создание, может потом реализую синглтон) игровой логики
    и игровой отрисовки для игры "MineSweeper". В это классе реализована как игровая логика, так и отрисовка
    и даже настройка компонентов формы и отображения. В общем такой жирный класс с жирными функциями.
    Для запуска класса ему нужны указатели на форму и на TDrawGrid. Вся игра ведётся в TDrawGrid, который
    настраивает и событиями которого пользуется этот класс. }
  
  { T_Mine_Sweeper }

  T_Mine_Sweeper = class(TObject)
    private
       f_game_form  : TForm;    // объект формы, на которой расположено игровое поле, передаётся из объемлющего кода в конструктор
       dg_game_grid : TDrawGrid;// объект дро-грида, который работает как игровое поле, передаётся из объемлющего кода в конструктор
       lb_game_state : TLabel;  // объект лэйбла, в котором отображается текущее состояние игры
       actual_tile_size : dword;// актуальный размер тайлов, отображаемых на игровом гриде

       asset_pack_name : string;         // имя загруженного ассет пака
       asset_pack_object : T_Asset_Pack; // объект загруженного ассет пака

       game_state : TGame_State; // состояние игры
       first_game_turn : boolean; // выставлен в истину если сейчас первый ход игры,
                                  // во всех других случаях выставлен в ложь

       top_matrix : TTop_Matrix;       // динамический массив верхней матрицы
       bottom_matrix : TBottom_Matrix; // динамический массив нижней матрицы

       chord_is_active : boolean; // флажок зажатого "аккорда", это когда зажаты одновременно левая и правая кнопки мыши
       chord_cells : TChord_Array; // зажатый аккорд, массив из ячеек входящих в аккорд. длинну получать функцией length()

       procedure configure_grid; // в этой процедуре мы производим все требуемые настройки грида

       function  in_cell_mine(py,px:integer):boolean; // возвращает истину если по указанных координатам есть мина
       procedure throw_mine(py,px:integer); // устанавливает мину по указанным координатам
       function  get_neighbors_count(py,px:integer): integer; // возвращает число мин, соседствующих с ячейкой py,px
       procedure open_cell( py, px : integer); // открывает ячейку с координатами py,px. Если нужно - то рекурсивно соседние
       procedure generate_top_matrix; // генерирует новую верхнюю матрицу, размеры берутся из констант, ПЕРЕДЕЛАТЬ !!!!!!!!!!!!!!!
       procedure generate_bottom_matrix; // генерирует новую нижнюю матрицу, размеры берутся из констант, ПЕРЕДЕЛАТЬ !!!!!!!!!!!!!!!!
       procedure move_mine_to_free_cell(py,px:integer); // перемещает мину в координатах cy,cx в первую попавшуюся пустую ячейку

       function chrord_get_cells_count:integer; // возвращает количество ячеек аккорда
       function chord_get_flags_count:integer;  // возвращает количество флагов в аккорде



    protected

    public
      constructor Create( game_form: TForm; game_grid: TDrawGrid; game_state_label: TLabel;
                          name_of_asset_pack: string ); // конструктор
      destructor Destroy;override; // деструктор

      procedure drawgrid_OnDrawCell(Sender: TObject; aCol,
                aRow: Integer; aRect: TRect; aState: TGridDrawState);// обработчик перерисовки ячеек игрового грида
      procedure drawgrid_OnMouseDown(Sender: TObject; Button: TMouseButton;// обработчик нажатия мышки на игровом гриде
                Shift: TShiftState; X, Y: Integer);
      procedure drawgrid_OnMouseUp(Sender: TObject;
                Button: TMouseButton; Shift: TShiftState; X, Y: Integer);// обработчик отпускания мышки на игровом гриде,
                                                                         // нужен для обработки аккордов

      procedure start_game; // стартует игру с текущими настройками
      procedure change_game_state(new_game_state:TGame_State);// переключает состояние игры


      procedure process_user_win; // вызывается в случае победы юзера
      procedure process_user_lose;// вызывается в случае поражения юзера
  end;

implementation

{ T_Mine_Sweeper }

procedure T_Mine_Sweeper.configure_grid;
{
У Lazarus 3.2(а может и у ранних) есть баг, неправильно отрабатывает установка
свойства ScrollBars в коде программе. Поэтому для игрового грида свойство 'ScrollBars'
должно быть установлено в 'ssNone' в режиме дизайна на игровой форме.
}
begin
  ///!!!!!!!! сейчас размер тайлов берётся от высоты грида, но иногда нужно будет !!!!!!!!
  //!!!!!!!!! брать от ширины, это необходимо проработать !!!!!!!!!!!
  //!!!!!!!!! эту игровую логику следует затащить в программу из проекта KMines !!!!!!!!!!!
  self.actual_tile_size := self.dg_game_grid.Height div matrix_height;

  self.dg_game_grid.AutoEdit:=False;
  self.dg_game_grid.ColCount:=matrix_width;
  self.dg_game_grid.RowCount:=matrix_height;
  self.dg_game_grid.DefaultColWidth:=self.actual_tile_size;
  self.dg_game_grid.DefaultRowHeight:=self.actual_tile_size;
  self.dg_game_grid.DoubleBuffered:=True;
  self.dg_game_grid.ExtendedSelect:=False;
  self.dg_game_grid.FixedCols:=0;
  self.dg_game_grid.FixedRows:=0;
  self.dg_game_grid.GridLineWidth:=0;
  { запрет на стандартную отрисовку дро-грида вся отрисовка только вручную }
  self.dg_game_grid.DefaultDrawing := False;
  { подгоним размер дро-грида под (размер тайлов*число тайлов + 2 пикселя) }
  self.dg_game_grid.Width:=self.actual_tile_size * matrix_width + 2;
  self.dg_game_grid.Height:=self.actual_tile_size * matrix_height + 2;

  { свяжем обработчики событий дро-грида }
  self.dg_game_grid.OnDrawCell:=@self.drawgrid_OnDrawCell;
  self.dg_game_grid.OnMouseDown:=@self.drawgrid_OnMouseDown;
  self.dg_game_grid.OnMouseUp:=@self.drawgrid_OnMouseUp;

  // перерисовка дро-грида после настройки всех его свойств
  self.dg_game_grid.Repaint;
end;


function T_Mine_Sweeper.in_cell_mine(py,px:integer):boolean;
{ возвращает истину если в ячейке есть мина
 генерирует исключение при выходе координат за пределы размерности матрицы }
begin
     { проверим переданные координаты на попадание в диапазоны НИЖНЕЙ матрицы }
     if ( (py<0)or(py>=bottom_matrix_height) ) or
        ( (px<0)or(px>=bottom_matrix_width)  ) then
        raise EOutsideOfMatrix.Create('Координаты y=' + IntToStr(py) +
                                      ',x=' + IntToStr(px) + ' выходят за пределы матрицы, имеющей размер '+
                                      IntToStr(bottom_matrix_height)+'x'+IntToStr(bottom_matrix_width)
                                      );
     // Формируем возвращаемое значение о нахождени мины по координатам py,px
     if ( bottom_matrix[py,px]=BC_INSTALLED_MINE ) or
        ( bottom_matrix[py,px]=BC_EXPLODED_MINE )
        then
           Result:=True
     else
           Result:=False;
end;

procedure T_Mine_Sweeper.throw_mine(py,px:integer);
{ устанавливает мину в ячейку, даже если там есть другое значение, или другая мина.
 генерирует исключение при выходе координат за пределы размерности матрицы }
begin
   { проверим переданные координаты на попадание в диапазоны НИЖНЕЙ матрицы }
   if ( (py<0)or(py>=bottom_matrix_height) ) or
        ( (px<0)or(px>=bottom_matrix_width)  ) then
        raise EOutsideOfMatrix.Create('Координаты y=' + IntToStr(py) +
                                      ',x=' + IntToStr(px) + ' выходят за пределы матрицы, имеющей размер '+
                                      IntToStr(bottom_matrix_height)+'x'+IntToStr(bottom_matrix_width)
                                      );
   bottom_matrix[py,px]:=BC_INSTALLED_MINE; // установим мину по координатам py,px
end;

function T_Mine_Sweeper.get_neighbors_count(py,px:integer): integer;
{ генерирует исключение при выходе координат за пределы размерности матрицы. }
{
    !!!!!!!!!!!!! пееработать на вариант с массивом смещений и циклом !!!!!!!!!!!!!!!!!!!!
}
var
  dy, dx,
  cy, cx : integer;
  neighbors_counter : integer;
begin
   { проверим переданные координаты на попадание в диапазоны НИЖНЕЙ матрицы }
   if ( (py<0)or(py>=bottom_matrix_height) ) or
        ( (px<0)or(px>=bottom_matrix_width)  ) then
        raise EOutsideOfMatrix.Create('Координаты y=' + IntToStr(py) +
                                      ',x=' + IntToStr(px) + ' выходят за пределы матрицы, имеющей размер '+
                                      IntToStr(bottom_matrix_height)+'x'+IntToStr(bottom_matrix_width)
                                      );
   // перед началом подсчёта установим счётчик соседних мин в ноль
   neighbors_counter:=0;
   // у ячейки есть 8 соседей, если она находится не на границе матрицы.
   // Проверяем на наличие мин всех соседних ячеек, с учётом границы.
   // Очевидно что соседние клетки могут быть определены через таблицу смещений координат.
   // (-1, -1), (-1, 0), (-1,+1)
   // ( 0, -1), ( py, px), ( 0,+1)
   // (+1, -1), (+1, 0), (+1,+1)
   // Мы выполним вычисления прямым заданием в коде, не прибегая к массиву смещений.
   // Это нарушит принцип DRY и сделает код повторяющимся,
   // но зато код будет более наглядным для понимания.
   dy:=-1; dx:=-1;
   cy:=py+dy; cx:=px+dx;
   if ( (cy>=0) and (cy<bottom_matrix_height) ) and
      ( (cx>=0) and (cx<bottom_matrix_width) ) then
        if ( in_cell_mine(cy,cx) )  then
           inc( neighbors_counter );

   dy:= 0; dx:=-1;
   cy:=py+dy; cx:=px+dx;
   if ( (cy>=0) and (cy<bottom_matrix_height) ) and
      ( (cx>=0) and (cx<bottom_matrix_width) ) then
        if ( in_cell_mine(cy,cx) )  then
           inc( neighbors_counter );

   dy:=+1; dx:=-1;
   cy:=py+dy; cx:=px+dx;
   if ( (cy>=0) and (cy<bottom_matrix_height) ) and
      ( (cx>=0) and (cx<bottom_matrix_width) ) then
        if ( in_cell_mine(cy,cx) )  then
           inc( neighbors_counter );

   dy:=+1; dx:= 0;
   cy:=py+dy; cx:=px+dx;
   if ( (cy>=0) and (cy<bottom_matrix_height) ) and
      ( (cx>=0) and (cx<bottom_matrix_width) ) then
        if ( in_cell_mine(cy,cx) )  then
           inc( neighbors_counter );

   dy:=+1; dx:=+1;
   cy:=py+dy; cx:=px+dx;
   if ( (cy>=0) and (cy<bottom_matrix_height) ) and
      ( (cx>=0) and (cx<bottom_matrix_width) ) then
        if ( in_cell_mine(cy,cx) )  then
           inc( neighbors_counter );

   dy:= 0; dx:=+1;
   cy:=py+dy; cx:=px+dx;
   if ( (cy>=0) and (cy<bottom_matrix_height) ) and
      ( (cx>=0) and (cx<bottom_matrix_width) ) then
        if ( in_cell_mine(cy,cx) )  then
           inc( neighbors_counter );

   dy:=-1; dx:=+1;
   cy:=py+dy; cx:=px+dx;
   if ( (cy>=0) and (cy<bottom_matrix_height) ) and
      ( (cx>=0) and (cx<bottom_matrix_width) ) then
        if ( in_cell_mine(cy,cx) )  then
           inc( neighbors_counter );

   dy:=-1; dx:= 0;
   cy:=py+dy; cx:=px+dx;
   if ( (cy>=0) and (cy<bottom_matrix_height) ) and
      ( (cx>=0) and (cx<bottom_matrix_width) ) then
        if ( in_cell_mine(cy,cx) )  then
           inc( neighbors_counter );

   // положим в возвращаемую ячейку памяти подсчитанное нами число соседей для ячейки
   Result:=neighbors_counter;
end;

{ рекурсивная функция открывает текущую и все соседние клетки, в которых нет мин.
  Клетку с миной эта процедура не обрабатывает откроет, это нужно делать в объемлющем коде!
}
procedure T_Mine_Sweeper.open_cell( py, px : integer);
begin
 if (py<0) or (py>=top_matrix_height) or  // если координата неправильная - выходим из процедуры
    (px<0) or (px>=top_matrix_width) or   // райзить здесь эксепшн не будем, так как такой подход позволит проще
             (py>=bottom_matrix_height) or  // обрабатывать случаи выхода за пределы диапазонов матриц
             (py>=bottom_matrix_width) then
               exit;

 if (top_matrix[py,px]=TC_OPENED) then  //если клетка уже открытая - выходим из процедуры
   exit;

 if (bottom_matrix[py,px] = BC_EMPTY) then
   begin
    top_matrix[py,px] := TC_OPENED;
    // (-1, -1), (-1, 0), (-1,+1)
    // ( 0, -1), (py,px), ( 0,+1)
    // (+1, -1), (+1, 0), (+1,+1)
    open_cell(py-1,px-1);open_cell(py-1,px+0);open_cell(py-1,px+1);
    open_cell(py+0,px-1);                     open_cell(py+0,px+1);
    open_cell(py+1,px-1);open_cell(py+1,px+0);open_cell(py+1,px+1);
   end
 else if ( QWord(bottom_matrix[py,px])>=QWord(BC_NEAR_1) )
     and ( QWord(bottom_matrix[py,px])<=QWord(BC_NEAR_8) ) then
   begin
    top_matrix[py,px] := TC_OPENED;
   end;
end;

procedure T_Mine_Sweeper.generate_top_matrix;
var
   iy, ix : integer;
begin
  /// установим размерность верхней матрицы и перераспределим оперативную память
  SetLength(top_matrix, top_matrix_height , top_matrix_width );

  /// окей, теперь нужно очистить верхнюю матрицу, заполнив её
  /// пустыми блоками.
  for iy:=0 to top_matrix_height-1 do
    for ix:=0 to top_matrix_width-1 do
      begin
         top_matrix[iy,ix] := TC_UNKNOWN;
      end;
end;

procedure T_Mine_Sweeper.generate_bottom_matrix;
var
   i, iy, ix, cy, cx: integer;
   mine_installed: boolean = False;
begin
  Randomize;
  /// установим размерность нижней матрицы и перераспределим оперативную память
  SetLength(bottom_matrix, bottom_matrix_height , bottom_matrix_width );

  /// окей, теперь нужно очистить нижнюю матрицу, заполнив её
  /// безмятежно чистою водою в каждой из ячеек матрицы
  for iy:=0 to bottom_matrix_height-1 do
    for ix:=0 to bottom_matrix_width-1 do
      begin
         bottom_matrix[iy,ix] := BC_EMPTY;
      end;

  // теперь расположим на нижней матрице N_mines мин, но так чтобы ни одна мина
  // не попала на другую при установке и чтобы их на матрице было ровно N_mines
  for i:=1 to N_mines do
    begin
       mine_installed:=False;
       while not mine_installed do
       begin
          cy := RandomRange(0,bottom_matrix_height);
          cx := RandomRange(0,bottom_matrix_width);
          if ( in_cell_mine(cy,cx) ) then
             continue;
          throw_mine(cy,cx);
          mine_installed := True;
       end;
    end;
  /// на нижней матрице расставлены мины !

  // теперь инициализируем ячейки с циферками !
  for iy:=0 to bottom_matrix_height-1 do
    for ix:=0 to bottom_matrix_width-1 do
      begin
         if not in_cell_mine(iy,ix) then // если в текущей ячейке нету мины
           if get_neighbors_count(iy,ix)<>0 then // если кол-во соседей для текущей ячеки неравно нулю
             bottom_matrix[iy,ix]:= // В текущую ячейку нужно поместить перечисление, которое кодирует число соседей
                   TBottom_Cell(get_neighbors_count(iy,ix)); // Воспользуемся нашими хитрыми значениями указанными для перечисления
                                                             // TBottom_Cell и с помощью приведения типов преобразуем число соседей
                                                             // текущей ячейки в конкретное значение перечисления.

      end;

end;

procedure T_Mine_Sweeper.move_mine_to_free_cell(py, px: integer);
var
   iy, ix : integer;
   mcnt: integer;
   nmy, nmx: integer;
   new_position_for_mine_found : boolean = False;
begin

  if (not in_cell_mine(py,px) ) then exit; // если по координатам мины нет - выходим из процедуры

  { найдём свободную ячейку, в которую будем переносить мину }
  for iy:=0 to matrix_height-1 do
    begin
    for ix:=0 to matrix_width-1 do
      if (not in_cell_mine(iy,ix)) then
        begin
          nmy:=iy; nmx:=ix; // найденные в цикле координаты для переноса мины запомним в переменных
          { прервём цикл, иначе наустоновим лишних мин, что поломает всю логику работы игры }
          new_position_for_mine_found:=True;// флаг для прерывания цикла for по iy, так как Break прервёт только цикл по ix
          break; // бряк для ix
        end;
        if ( new_position_for_mine_found ) then
          break; // бряк для iy
    end;

  { выловим мину по координатам py,px и установим мину в координаты nmy,nmx }
  self.bottom_matrix[py,px]:=BC_EMPTY; //выловили мину по координатам py,px
  self.throw_mine(nmy,nmx); // установили мину по координатам nmy, mnx

  { хорошо, мы перенесли мину из py,px в свободное место, но у нас теперь получились
    некорректные числа, обозначающие количество мин. пересчитаем их ниже }
  for iy:=0 to matrix_height-1 do
    for ix:=0 to matrix_width-1 do
         if (not in_cell_mine(iy,ix) ) then
           begin
             mcnt:= self.get_neighbors_count(iy,ix);
             if (mcnt=0) then
                self.bottom_matrix[iy,ix]:=BC_EMPTY
             else
                begin
                  bottom_matrix[iy,ix]:= // В текущую ячейку нужно поместить перечисление, которое кодирует число соседей
                   TBottom_Cell(get_neighbors_count(iy,ix)); // Воспользуемся нашими хитрыми значениями указанными для перечисления
                                                             // TBottom_Cell и с помощью приведения типов преобразуем число соседей
                                                             // текущей ячейки в конкретное значение перечисления.

                end;
           end;

end;

function T_Mine_Sweeper.chrord_get_cells_count: integer;
begin
  Result:=Length(self.chord_cells);
end;

function T_Mine_Sweeper.chord_get_flags_count: integer;
var
   i: integer;
   cnt: integer;
begin
  cnt:=0;
  for i:=0 to Length(self.chord_cells)-1 do
    if ( self.chord_cells[i].has_flag )  then
      inc(cnt);
  Result:=cnt;
end;

procedure T_Mine_Sweeper.change_game_state(new_game_state: TGame_State);
{ изменяет состояние игры на одно из допустимых, при этом меняет текст в лейбле игровых
 состояний, который передётся в конструктор при инициализации объекта этого класса }
var
   str_state : string; // временная переменная со строкой состояния для лэйбла
begin
  case new_game_state of   // определяем строку для каждого из возможных состояний
    GS_IDLE : str_state:='ОЖИДАНИЕ';
    GS_PLAY : str_state:='ИГРАЕМ';
    GS_LOSE : str_state:='ИГРА ПРОИГРАНА !';
    GS_WIN  : str_state:='ВЫ ВЫИГРАЛИ !';
  end;
  self.game_state:=new_game_state; // меняем значения поля состояния объекта на переданный в этот метод
  self.lb_game_state.Caption:=str_state; // помещаем текстовое описание состояния в лэйбл игровых состояний
  self.lb_game_state.Repaint; // перерисовываем лэйбл с игровыми состояниями

  case new_game_state of  // в зависимости от состояния игры вызываем один из двух методов
                          // "победа" или "проигрыш"
    GS_WIN : self.process_user_win;
    GS_LOSE: self.process_user_lose;
    else ;
  end;

end;


constructor T_Mine_Sweeper.Create(game_form: TForm; game_grid: TDrawGrid; game_state_label: TLabel;
  name_of_asset_pack: string);
begin
  inherited Create;            // вызовем родительский конструктор
  Randomize; // проинициализируем генератор случайных чисел
  self.f_game_form:= game_form; // сохраним объект игровой формы
  self.dg_game_grid:= game_grid; // сохраним объект игрового грида
  self.lb_game_state:=game_state_label; // сохраним объект показа игровых состояний


  self.asset_pack_name := name_of_asset_pack;   // сохраним имя ассет пака
  self.asset_pack_object := T_Asset_Pack.Create( self.asset_pack_name ); // загрузим ассет пак

  self.game_state:= GS_IDLE; // состояние игры по умолчанию

  self.configure_grid; // настроим игровой грид

  self.first_game_turn:= true; // выставим факт первого хода в игре в истину

  self.chord_is_active:=False; //по умолчанию аккорд неактивен
  SetLength(self.chord_cells,0); // установим нулевую длинну для массива ячеек аккорда
  //... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
end;

destructor T_Mine_Sweeper.Destroy;
begin
  // отключим обработчики событий в игровых контролах
  self.dg_game_grid.OnDrawCell:=nil;
  self.dg_game_grid.OnMouseDown:=nil;
  self.dg_game_grid.OnMouseUp:=nil;
  // обнулим лэйбл игрового состояния
  self.lb_game_state.Caption:='';
  // перерисуем игровые контролы
  self.dg_game_grid.Repaint;
  self.lb_game_state.Repaint;

  // обнулим ссылки на игровые контролы формы
  self.lb_game_state:=nil;
  self.dg_game_grid:=nil;
  self.f_game_form:=nil;

  // выгрузим объект ассет пака из памяти
  self.asset_pack_object.Free;
  self.asset_pack_object:=nil;

  //... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...

  inherited Destroy; // вызовем родительский деструктор
end;

procedure T_Mine_Sweeper.drawgrid_OnDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
{ это кастомный обработчик отрисовки ячеек в игровом DrawGrid,
  всё отображение и вся отрисовка внутри DrawGrid-а находится здесь }
var
   cy, cx : integer;
   i: integer;
   paint_tile : TPortableNetworkGraphic; // указатель на картинку, для простоты и быстроты
begin
  if (self.game_state<>GS_PLAY) and  // в неигровом режиме отрисовывается специальный тайл - 'default'
     (self.game_state<>GS_WIN)  and
     (self.game_state<>GS_LOSE) then
    begin
       dg_game_grid.Canvas.StretchDraw(aRect,self.asset_pack_object.tile_default);
      exit;
    end;

  { для единообразия отранслируем координаты в другие названия переменных }
  cy := aRow;
  cx := aCol;

  { в зависимости от состояния текущей верхней ячейки показываем либо верхнюю ячейку либо нижнюю, что под ней,
    для показа этот код получает объект тайла в зависимости от ячеек обеих матриц }
  case top_matrix[cy,cx] of
   TC_UNKNOWN : paint_tile:=self.asset_pack_object.tile_tc_unknown;
   TC_FLAGGED : paint_tile:=self.asset_pack_object.tile_tc_flagged;
   TC_OPENED  : paint_tile:=self.asset_pack_object.get_bottom_tile_by_code(bottom_matrix[cy,cx]);
  end;

  { если юзер зажал аккорд, и это ячейка аккорда, да ещё и без флага то мы
     должны отрисовать тайл аккорда }
  if (self.chord_is_active) then  // если аккорда активен
     if (self.chrord_get_cells_count>0) then  // если длинна аккорда не равна нулю
        begin
          { для хранения списка ячеек аккорда я использовал не умные коллекции,
            а простой массив, поэтому для определения нахождения текущей координаты
            в аккорде нужен полный перебор массива аккорда }
          for i:=0 to length(self.chord_cells)-1 do // переберём все ячейки массива аккорда
            if (self.chord_cells[i].y=cy) and
               (self.chord_cells[i].x=cx) then  // эти два условия  в if истинны если текущая ячейка cy,cx входит в аккорд
                  if  ( not self.chord_cells[i].has_flag ) then // это условие истинно если текущая ячейка аккорда y,x НЕ помечена флагом
                    begin
                      paint_tile := self.asset_pack_object.tile_tc_chord; // в тайл для отрисовки положим объект картинки с рисунком аккорда
                      break;// прервём цикл, так как в этой процедуре за раз отрисовывается одна клетка, и для неё мы установили
                            // тайл аккорда
                    end;
        end;

  try  // отрисовывать будем в блоке try..except
    self.dg_game_grid.Canvas.StretchDraw(aRect,paint_tile); // отрисовка предельно простая на данный момент
  except On E: Exception Do // ловим все эксепшены
            ;  // и никак не реагируем ни на один эксепшн! это для устойчивости игры
  end;

  //... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
end;

procedure T_Mine_Sweeper.drawgrid_OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
{ это кастомный обработчик нажатий кнопок мыши в игровом DrawGrid,
  всё взаимодействие мышью с игровым DrawGrid-е находится здесь }
var
  cy, cx: integer;
  iy, ix : integer;
  i: integer;
  yy, xx : integer;
  flags_count: integer;
  closed_count : integer;
begin
   // мышь обрабатываем только если режим игры GS_PLAY
   if ( game_state<>GS_PLAY) then exit;
   // в эту процедуру передаются абсолютные координаты мыши всего DrawGrid-а
   // нам нужно получить координаты ячейки, по которой щёлкнул пользователь
   // что мы и делаем ниже...
   cy := Y div self.dg_game_grid.DefaultRowHeight;
   if (cy<0) then cy:=0;
   if (cy>=self.dg_game_grid.DefaultRowHeight-1) then cy:=self.dg_game_grid.DefaultRowHeight-1;
   cx := X div self.dg_game_grid.DefaultColWidth;
   if (cx<0) then cx:=0;
   if (cx>=self.dg_game_grid.DefaultColWidth-1) then cx:=self.dg_game_grid.DefaultRowHeight-1;


   { проверяем аккорд,то есть нажаты ли две клавиши одновременно.
     код проверки двух клавиш найден на просторах Интернета! }
   if ((button = mbLeft) and (ssRight in Shift)) or
      ((button = mbRight) and (ssLeft in Shift))  then
        begin

           SetLength(self.chord_cells,0);

           self.chord_is_active:=True;
           for i:=1 to 8 do // для перебора всех соседей клетки  бежим по массиву смещений
             begin
               { имея индекс массива смещений вычислим координаты очередной из восьми соседних клеток для cx,cy и положим их в yy,xx }
               yy := square_offsets[i].y + cy;
               if ( yy<0 ) or ( yy>=matrix_height ) then continue; // если координата соседней ячейки yy лежит за пределами матрицы - перейдём к следующей итерации цикла
               xx := square_offsets[i].x + cx;
               if ( xx<0 ) or ( xx>=matrix_width ) then continue; // проверяем координату xx, аналогично как и yy

               { здесь мы имеем валидные координаты yy,xx, решим, входят-ли они в аккорд }
               if ( top_matrix[yy,xx]=TC_UNKNOWN ) or
                  ( top_matrix[yy,xx]=TC_FLAGGED ) then // если по вычисленным координатам yy,xx закрытая клетка, даже
                                                        // помеченая флагом, то выполним блок кода, добавляющий её в аккорд.
                 begin
                   SetLength(self.chord_cells,length(self.chord_cells)+1); // увеличим длинну массива аккорда на 1 единицу
                   { положим координаты очередной вычисленной ячейки в последнюю, добавленную ячейку массива аккорда }
                   self.chord_cells[length(self.chord_cells)-1].y:= yy;
                   self.chord_cells[length(self.chord_cells)-1].x:= xx;
                   self.chord_cells[length(self.chord_cells)-1].has_flag:=(top_matrix[yy,xx]=TC_FLAGGED);// запишем истину в ячейку аккорда
                                                                                                         // если в ней есть флаг
                 end;
             end;
           self.dg_game_grid.Repaint;
           exit;
        end;
   //!!!!!!!!!!!! else if
//   else
//     begin
//        { организуем отжатие аккорда }
//     self.chord_is_active:=False;
  //      SetLength(self.chord_cells,0);
  //    end;

   // открываем ячейку, если внизу пусто, то рекурсивно открываем все соседние ячейки
   // всё в точности как в оригинальном сапёре
   if ( Button=mbLeft ) then
     if ( top_matrix[cy,cx]=TC_UNKNOWN ) then
       begin
          if ( in_cell_mine(cy,cx) ) then // В ЯЧЕЙКЕ МИНА - нужно обработать этот случай
            begin
              if ( self.first_game_turn ) then // первый ход не должен подрывать мину !
                begin
                  self.move_mine_to_free_cell(cy,cx);  // переместим мину в первую свободную ячейку
                  open_cell(cy,cx); // откроем ячейку
                  self.first_game_turn:=False; // снимем флажок первого хода
                  { выйдем из процедуры, иначе ошибочно выполнится код ниже и установит подорваную
                    мину в клетку cy,cx. Вот такие вот в программировании есть неожиданности.
                    Перед выходом перерисуем дро-грид }
                  self.dg_game_grid.Repaint;
                  exit;
                end;

              top_matrix[cy,cx]:=TC_OPENED;  // откроем ячейку
              bottom_matrix[cy,cx]:=BC_EXPLODED_MINE; // мину переключим на подорваную мину
              { обработка ПРОИГРЫША в игре }
              for iy:=0 to top_matrix_height-1 do // в циклах откроем всю верхнюю матрицу
                 for ix:=0 to top_matrix_width-1 do
                     top_matrix[iy,ix]:=TC_OPENED;
              self.f_game_form.Repaint;  // перерисуем игровой дро-грид
              self.change_game_state(GS_LOSE); // переключим состояние игры на ПРОИГРЫШ
            end
          else  // В ЯЧЕЙКЕ НЕТ МИНЫ - просто откроем ячейку, если надо - то рекурсивно ей соседей
            begin
              open_cell(cy,cx);
            end;
          self.first_game_turn:=False; // и здесь снимем флажок первого хода, вот такая развесистая клюква иногда бывает, сдобреная дебаггером
       end;

   // помечаем ячейку флагом или убираем флаг
   if ( Button=mbRight ) then
     if ( top_matrix[cy,cx]=TC_UNKNOWN ) then
       top_matrix[cy,cx]:=TC_FLAGGED
     else if ( top_matrix[cy,cx]=TC_FLAGGED ) then
       top_matrix[cy,cx]:=TC_UNKNOWN;


   { подсчитаем количество флагов, выставленных игроком }
   flags_count:=0;
   for iy:=0 to matrix_height-1 do
     for ix:=0 to matrix_width-1 do
      if top_matrix[iy,ix]=TC_FLAGGED then
       inc(flags_count);
   // !!!!!!!!!! вывод количества флагов в интерфейсе !!!!!!!!!

   { Обработка ВЫИГРЫША в игре.
     Игра выиграна если на поле остались закрытыми(TC_UNKNOWN или TC_FLAGGED) все
     ячейки с минами(BC_INSTALLED_MINE) и кроме этого нет других закрытых ячеек.
     То есть количество закрытых ячеек будет равно количеству мин.
    }
   // подсчитаем количество закрытых клеток на поле
   closed_count:=0;
   for iy:=0 to matrix_height-1 do
     for ix:=0 to matrix_width-1 do
       if (top_matrix[iy,ix]=TC_UNKNOWN) or (top_matrix[iy,ix]=TC_FLAGGED) then
         inc(closed_count);
   // если верен вышеописаный признак победы в игре, то
   if ( closed_count=N_mines ) then
    begin
      for iy:=0 to top_matrix_height-1 do // в циклах откроем всю верхнюю матрицу
        for ix:=0 to top_matrix_width-1 do
           top_matrix[iy,ix]:=TC_OPENED;
      self.f_game_form.Repaint;  // перерисуем игровой дро-грид
      self.change_game_state(GS_WIN); // переключим состояние игры на ВЫИГРЫШ
    end;

   // даём команду на перерисовку всего грида
   self.dg_game_grid.Repaint;
end;

procedure T_Mine_Sweeper.drawgrid_OnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     { организуем отжатие аккорда }
     self.chord_is_active:=False;
     SetLength(self.chord_cells,0);
     self.dg_game_grid.Repaint;
end;

procedure T_Mine_Sweeper.start_game;
{
  эта команда стартует игру в указанных при создании в методе Create контролах
  и с текущими игровыми настройками.
}
begin
   self.generate_bottom_matrix; // проинициализируем нижнюю матрицу
   self.generate_top_matrix; // проинициализируем верхнюю матрицу
   self.first_game_turn:= True; // установим флажок первого хода в истину
   self.change_game_state(GS_PLAY); // переключает состояние объекта на 'Игра'
end;

procedure T_Mine_Sweeper.process_user_win;
begin
   ShowMessage('Вы победили в этом раунде "Сапёра" ! ');
end;

procedure T_Mine_Sweeper.process_user_lose;
begin
  ShowMessage('Вы проиграли этот раунд "Сапёра" ! ');
end;


end.

