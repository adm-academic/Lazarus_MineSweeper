unit u_records_manager;

{$ifdef FPC}

{$mode delphi}
{$H+}

{$endif}

interface

uses
  Classes, SysUtils,
  generics.Collections,Generics.Defaults, // подключим делфовые дженерики
  u_minesweeper_types;

const
  records_filename =  'records.file';  // имя бинарного файла со списком рекордов
  records_count    =  10; // длинна списка рекордов, вернее все списков, всего их 4 в соотвествии с 4-мя уровнями сложности

type
  T_Gamer_Record=record  // запись для хранения рекордов игрока
    level : T_Game_Difficulty;  // сложность игры
    nickname : String[50];      // никнэйм игрока, строка длинною 50 символов
    gametime_seconds : integer; // секунд потрачено на партию
    field_height : integer;     // высота поля в ячейках
    field_width : integer;      // ширина поля в ячейках
    field_N_mines : integer;    // количество мин на поле
  end; // в записи не указывается позиция игрока, так как позицию будем брать из коллекции записей


  T_GR_List = TList<T_Gamer_Record>;  // список рекордов, коллекция записей,  дженерик ( в C++ - шаблонный класс ).

  { Менеджер рекордов:
     * хранит списки рекордов +
     * читает из файла  +
     * записывает в файл +
     * детектирования изменения файла нет, для чтения-записи нужно вручную дёргать методы +
     * предоставляет индексированный доступ к спискам рекордов +
     * поддерживает декомпозицию по уровню сложности, предоставляя 4 разных набора данных,
       для каждого уровня сложности свой. +
     * определяет факт достижения рекорда +
     * регистрирует рекорд, вставляя к конец списка и сортируя список потом, записи которые
       имеют и индекс больше чем размер списка - удаляются  +
     * автоматически сортирует коллекцию рекордов, поддерживая их возрастание (только при записи,
       может нужно сделать и при чтении ) +
     * "подрезает" список рекордов, обеспечивая постоянную длинну не более records_count записей  +
    }
  
  { T_Records_Manager }

  T_Records_Manager = class

    private
     { Менеджер рекордов ведёт сразу 4 коллекции с рекордами,
       по одной коллекции для каждого уровня сложности.
       Записываются в файл они также по-порядку.

       GD_EASY,
       GD_MEDIUM,
       GD_HARD,
       GD_CUSTOM
      }
     gamers_records_easy  : T_GR_List;
     gamers_records_medium: T_GR_List;
     gamers_records_hard  : T_GR_List;
     gamers_records_custom: T_GR_List;

    public
     { методы читают и пишут из/в файл с рекордами, с указанием уровня сложности.
       в зависимости от уровня сложности берётся одна из четырёх коллекций с рекордами.}
     procedure f_read_records_by_diff(diff:T_Game_Difficulty);
     procedure f_write_records_by_diff(diff:T_Game_Difficulty);

     { методы читают и пишут из/в файл с рекордами все списки рекордом, которые
       поддерживает этот класс менеджера рекордов }
     procedure f_read_all_records;
     procedure f_write_all_records;


     constructor Create;  // конструктор
     destructor Destroy;override; // дестуктор

     { метод возвращает истину если переданное значения time_value для уровня сложности diff
       является рекордом }
     function this_value_is_a_record( diff:T_Game_Difficulty; time_value: integer ): boolean;

     { регистрирует рекорд,предварительно проверив его на факт достижения рекорда  }
     procedure register_record( new_record: T_Gamer_Record );

     { очищает все рекорды }
     procedure clear_all_records;

     { выстави наружу коллекции с рекордами по уровням сложности, правда они будут не защищены от
       изменения "пользователем", но достаточно и такого подхода }
     property records_easy  : T_GR_List read gamers_records_easy;
     property records_medium: T_GR_List read gamers_records_medium;
     property records_hard  : T_GR_List read gamers_records_hard;
     property records_custom: T_GR_List read gamers_records_custom;

  end;



implementation
uses Dialogs, Forms;

{ T_Records_Manager }

procedure T_Records_Manager.f_read_records_by_diff(diff: T_Game_Difficulty);
var
  rec_file : File of T_Gamer_Record;
  rec: T_Gamer_Record;
  file_path, file_name : string;
  i: integer;
  records_collection: T_GR_List;
begin
  { переключимся на коллекцию исходя из параметра }
  case diff of
    GD_EASY:   records_collection:=self.gamers_records_easy;
    GD_MEDIUM: records_collection:=self.gamers_records_medium;
    GD_HARD:   records_collection:=self.gamers_records_hard;
    GD_CUSTOM: records_collection:=self.gamers_records_custom;
  end;

  file_path:=ExtractFilePath(Application.ExeName); //  получим путь к папке с программой
  file_name:=file_path + PathDelim + records_filename; // определим имя файла с рекордами
  AssignFile( rec_file, file_name  ); // выполним связывание имени файла с переменной
  Reset(rec_file);  // откроем файл для записи, это нужно чтобы не следить за пустым файлом

  records_collection.Clear; // очистим ТЕКУЩУЮ коллекцию рекордов

  while (not EOF(rec_file) ) do   // повторять пока не достигнут конец файла
  begin
    read(rec_file, rec);   // читаем по одной записи из файла
    { и добавляем эту запись в ТЕКУЩУЮ коллецию рекордов  ТОЛЬКО ЕСЛИ уровни сложности совпадают с
     переданным в метод }
    if (rec.level=diff) then
       records_collection.Add(rec);
  end;

  CloseFile(rec_file);  // закрываем файл

  records_collection:=nil; // освободим указатель на коллекцию
end;

procedure T_Records_Manager.f_write_records_by_diff(diff: T_Game_Difficulty);
var
  rec_file : File of T_Gamer_Record;
  rec: T_Gamer_Record;
  file_path, file_name : string;
  i: integer;
  records_collection: T_GR_List;
begin
  { переключимся на коллекцию исходя из параметра }
  case diff of
    GD_EASY:   records_collection:=self.gamers_records_easy;
    GD_MEDIUM: records_collection:=self.gamers_records_medium;
    GD_HARD:   records_collection:=self.gamers_records_hard;
    GD_CUSTOM: records_collection:=self.gamers_records_custom;
  end;

  file_path:=ExtractFilePath(Application.ExeName);   //  получим путь к папке с программой
  file_name:=file_path + PathDelim + records_filename; // определим имя файла с рекордами
  AssignFile( rec_file, file_name  ); // выполним связывание имени файла с переменной
  FileMode:=2;
  Reset(rec_file); // откроем файл для записи
  Seek(rec_file, FileSize(rec_file)); //перейдём в конец файла

  for i:=0 to records_collection.Count-1 do // бежим по всем записям ТЕКУЩЕЙ коллекции рекордов
  begin
    rec := records_collection[i] ;  //очередной рекорд копируем в переменную
    write( rec_file, rec );  // и пишем эту переменную в  файл
  end;

  CloseFile(rec_file); // закрываем файл

  records_collection:=nil;  // освободим указатель на коллекцию
end;

procedure T_Records_Manager.f_read_all_records;
var
  rec_file : File of T_Gamer_Record;
  file_path, file_name : string;
begin
  { создадим пустой файл если его ещё нет }
  file_path:=ExtractFilePath(Application.ExeName);   //  получим путь к папке с программой
  file_name:=file_path + PathDelim + records_filename; // определим имя файла с рекордами
  if (not FileExists(file_name)) then
     begin
      AssignFile( rec_file, file_name  ); // выполним связывание имени файла с переменной
      Rewrite( rec_file );
      CloseFile( rec_file );
     end;

  { читаем все рекорды по порядку по уровням сложности }
  self.f_read_records_by_diff(GD_EASY);
  self.f_read_records_by_diff(GD_MEDIUM);
  self.f_read_records_by_diff(GD_HARD);
  self.f_read_records_by_diff(GD_CUSTOM);
end;

procedure T_Records_Manager.f_write_all_records;
var
  rec_file : File of T_Gamer_Record;
  file_path, file_name : string;
begin
  { сбросим файл в ноль перед записью }
  file_path:=ExtractFilePath(Application.ExeName);   //  получим путь к папке с программой
  file_name:=file_path + PathDelim + records_filename; // определим имя файла с рекордами
  AssignFile( rec_file, file_name  ); // выполним связывание имени файла с переменной
  Rewrite(rec_file);
  CloseFile(rec_file);

  { пишем все рекорды по порядку по уровню сложности }
  self.f_write_records_by_diff(GD_EASY);
  self.f_write_records_by_diff(GD_MEDIUM);
  self.f_write_records_by_diff(GD_HARD);
  self.f_write_records_by_diff(GD_CUSTOM);
end;

constructor T_Records_Manager.Create;
begin
  inherited Create;
  self.gamers_records_easy   := T_GR_List.Create;
  self.gamers_records_medium := T_GR_List.Create;
  self.gamers_records_hard   := T_GR_List.Create;
  self.gamers_records_custom := T_GR_List.Create;
end;

destructor T_Records_Manager.Destroy;
begin
  FreeAndNil( self.gamers_records_easy   );
  FreeAndNil( self.gamers_records_medium );
  FreeAndNil( self.gamers_records_hard   );
  FreeAndNil( self.gamers_records_custom );
  inherited Destroy;
end;

function T_Records_Manager.this_value_is_a_record(diff: T_Game_Difficulty;
  time_value: integer): boolean;
var
  has_record: boolean;
  i: integer;
  records_collection: T_GR_List;
  already_occupied_list_count : integer;
begin
  { переключимся на коллекцию исходя из параметра }
   case diff of
     GD_EASY:   records_collection:=self.gamers_records_easy;
     GD_MEDIUM: records_collection:=self.gamers_records_medium;
     GD_HARD:   records_collection:=self.gamers_records_hard;
     GD_CUSTOM: records_collection:=self.gamers_records_custom;
   end;

   { в цикле определим что time_value это рекорд для сложности diff }
   already_occupied_list_count:=0;
   has_record:= False;
   for i:= 0 to records_collection.Count-1 do
   begin
     Inc( already_occupied_list_count );
     if ( time_value<records_collection[i].gametime_seconds ) then
        begin
          records_collection:=nil;
          has_record:=True;
          Result:=has_record;
          exit;
        end;
   end;

   { если есть место внизу таблицы рекордов, то посчитаем переданное значение за рекорд }
   if (has_record=False) and (already_occupied_list_count<records_count) then
      begin
        records_collection:=nil;
        Result:=True;
        exit;
      end;

   records_collection:=nil;
end;

procedure T_Records_Manager.register_record( new_record: T_Gamer_Record );
var
  records_collection: T_GR_List;
  i,j: integer;
  tmp_rec: T_Gamer_Record;
  nmin : integer;
begin
  { если переданная запись не является рекордом, то по-тихому выходим из метода }
  if ( not self.this_value_is_a_record( new_record.level, new_record.gametime_seconds ) ) then  exit;

  { определим нужную коллекцию }
  case new_record.level of
     GD_EASY:   records_collection:=self.gamers_records_easy;
     GD_MEDIUM: records_collection:=self.gamers_records_medium;
     GD_HARD:   records_collection:=self.gamers_records_hard;
     GD_CUSTOM: records_collection:=self.gamers_records_custom;
   end;

  { сделаем просто, вставим рекорд в конец коллекции и отсортируем }
  records_collection.Add( new_record );

  { сортируем коллекцию }
  for i:=0 to (records_collection.Count-1-1) do
    begin
      nmin:=i;
      for j:=i+1 to (records_collection.Count-1) do
        if (records_collection[j].gametime_seconds) < (records_collection[nmin].gametime_seconds) then
           begin
             nmin:=j;
             tmp_rec:=records_collection[i];
             records_collection[i]:=records_collection[nmin];
             records_collection[nmin]:=tmp_rec;
           end;
    end;
  { теперь "подрежем" конец коллекции }
  if (records_collection.Count>records_count) then // если длинна больше максимальной
     begin
       for i:=records_collection.Count-1 downto records_count do // цикл с конца массива по максимальную длинну
         begin
            records_collection.Delete(i); // удалим лишний с конца элемент
         end;
     end;


  records_collection:=nil;
end;

procedure T_Records_Manager.clear_all_records;
begin
  self.gamers_records_easy.Clear;
  self.gamers_records_medium.Clear;
  self.gamers_records_hard.Clear;
  self.gamers_records_custom.Clear;
  self.f_write_all_records;
end;



end.

