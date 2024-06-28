{
Объект для доступа к ассет паку. Тайлам и конфигурации. Это сделано для скорости и
гибкости итоговой программы.
}
unit u_asset_pack;
{$ifdef FPC}

{$mode delphi}
{$H+}

{$endif}

interface

uses
{$ifdef FPC}
  Classes, SysUtils, Graphics
  ,generics.Collections,Generics.Defaults // подключим делфовые дженерики
  ,u_minesweeper_types;
{$else}
  Classes, SysUtils, FMX.Graphics
  ,u_minesweeper_types;
{$endif}

const
   assets_ini_filename='asset.ini';

type
  { E_Asset_Files_Error }
  E_Asset_Files_Error = class(Exception); // эксепшн доступа к файлам ассет пака

  { тип коллекции для загрузки, хранения и удобного доступа к загруженым тайлам в формате TBitmap:
    [tile_size] [tile_name]
  }
  T_Tiles_One_Size_Dict = TObjectDictionary<string,TBitmap>;
  T_Tiles_Dict = TObjectDictionary< string, T_Tiles_One_Size_Dict >;


  { T_Asset_Pack }
  T_Asset_Pack = class( TObject ) // класс для управления и кеширования ассет пака
    { внутренние константы класса }
    private const
        { запрограммированное количество размеров тайлов }
        total_sizes_count = 4;
        { перечень размеров тайлов, в формате строк, так как так удобнее и проще в программировании }
        tile_sizes : array[0..total_sizes_count-1] of string =
          ( '32',
            '64',
            '128',
            '256'
          );
        { ожидаемое количество тайлов в любом ассет паке  }
        total_tile_files_count = 15;
        { перечень имён тайлов в любом ассет паке используется для проверки корректности набора тайлов. }
        tile_files : array [0..total_tile_files_count-1] of string =
          ( 'default.bmp',
            'tc_unknown.bmp',
            'tc_flagged.bmp',
            'tc_chord.bmp',
            'bc_empty.bmp',
            'bc_near_1.bmp',
            'bc_near_2.bmp',
            'bc_near_3.bmp',
            'bc_near_4.bmp',
            'bc_near_5.bmp',
            'bc_near_6.bmp',
            'bc_near_7.bmp',
            'bc_near_8.bmp',
            'bc_installed_mine.bmp',
            'bc_exploded_mine.bmp' );

    private
      { имя ассет-пака }
      asset_pack_dirname                  : string; // имя текущего набора ассетов
      { директории }
      assets_containing_dirname_full_path : string; // имя директории, содержащей папки с наборами ассетов
      asset_pack_current_full_patch       : string; // полный путь до текущего набора ассетов
      asset_pack_images_dirname_full_path : string; // полный путь до директории с картинками
      asset_pack_sounds_dirname_full_path : string; // полный путь до директории со звуками и музыкой
      { файлы }
      asset_current_ini_filename_full_path: string; // настройки текущего набора ассетов, имя файла

      required_size : integer; // текущий требуемый размер тайлов. В конструкторе Create устанавливается равным 32

      { хранилище тайлов, индексированное строками }
      tiles_dict : T_Tiles_Dict;


      { инициализирует переменные с путями до директорий и файлов ассет пака }
      procedure init_dir_and_file_names( par_asset_pack_dirname:String );


      { загружает все ассеты в ОЗУ }
      procedure load_all_asset_files;


    public
      { битмапы в паблике для ускорения отрисовки }
      tile_default : TBitmap;
      tile_tc_unknown : TBitmap;
      tile_tc_flagged : TBitmap;
      tile_tc_chord : TBitmap;
      tile_bc_empty :  TBitmap;
      tile_bc_near_1 : TBitmap;
      tile_bc_near_2 : TBitmap;
      tile_bc_near_3 : TBitmap;
      tile_bc_near_4 : TBitmap;
      tile_bc_near_5 : TBitmap;
      tile_bc_near_6 : TBitmap;
      tile_bc_near_7 : TBitmap;
      tile_bc_near_8 : TBitmap;
      tile_bc_installed_mine : TBitmap;
      tile_bc_exploded_mine : TBitmap;


    protected

    public
      constructor Create( par_asset_pack_dirname:String );// конструктор, загружаем ассет пак по имени его папки
      destructor Destroy; override; // деструктор

      procedure actualize_size( par_required_size:integer );

      function get_top_tile_by_code(tile_code:TTop_Cell):TBitmap; // возвращает объект тайла верхней матрицы по его коду
      function get_bottom_tile_by_code(tile_code:TBottom_Cell):TBitmap;// возвращает объект тайла нижней матрицы по его коду

  end;

implementation
uses
{$ifdef FPC}
  Dialogs,Forms
  , u_settings_manager;
{$else}
  FMX.Dialogs,FMX.Forms
  , u_settings_manager;
{$endif}



{ T_Asset_Pack }
constructor T_Asset_Pack.Create( par_asset_pack_dirname: String );
var
  i,j: integer;
  str_key: string;
begin
   inherited Create;

   { настраивает переменные-пути для набора ассетов }
   self.init_dir_and_file_names( par_asset_pack_dirname );


   { ... ... ... загрузить конфигурацию из ini-файла ... ... ... }


   { проинициализируем коллекцию tiles_dict [tile_size] [tile_name] }
   self.tiles_dict:=T_Tiles_Dict.Create(total_sizes_count);
   for i:=0 to self.total_sizes_count-1 do
   begin
     str_key:=tile_sizes[i]; // получим строку key из массива-константы класса
     { создадим объект-дженерик для строки str_key }
     self.tiles_dict.AddOrSetValue(str_key, T_Tiles_One_Size_Dict.Create(self.total_tile_files_count) );
   end;

   { установим сейчас требуемый размер тайлов, это делаем именно здесь, так как
     ниже это значение будет использовано в процессе инициализации объекта класса }
   self.required_size:=32;

   { загружает ассеты-тайлы }
   self.load_all_asset_files;


   { ... ... ... загрузить ассеты-звуки и музыку ... ... ... }


end;

destructor T_Asset_Pack.Destroy;
var
  i,j:integer;
  str_key: string;
  size_key_str, tile_key_str : string;
  tile_files_dict : T_Tiles_One_Size_Dict;
begin
  { NEW уничтожим объекты-тайлы }
  for  size_key_str in self.tiles_dict.Keys do  // итерируем по ключам первого уровня
     begin
     tile_files_dict := self.tiles_dict[size_key_str]; // объект второго уровня с тайлами-битмапами
     for tile_key_str in tile_files_dict.Keys do // итерируем по ключам второго уровня
         begin
         tile_files_dict[tile_key_str].Free;
         end;
     end;

  { NEW уничтожим коллеции уровня 2}
  for str_key in self.tiles_dict.Keys do
  begin
    self.tiles_dict[str_key].Clear;
    self.tiles_dict.Remove(str_key);
  end;
  { NEW уничтожим коллекцию уровня 1 }
  self.tiles_dict.Clear;
  FreeAndNil(self.tiles_dict);

  inherited Destroy;
end;

{ обновляет ссылки tile_*, присваивая им тайлы из коллекции tiles_dict с наиболее подходящим размером }
procedure T_Asset_Pack.actualize_size(par_required_size: integer);
var
  requred_size_dirname : string;
  i: integer;
  tiles_subdict: T_Tiles_One_Size_Dict;
begin
  { вычисляем подходящий размер для переданного числа }
  if (par_required_size>=0) and (par_required_size<=48) then
     begin
       self.required_size:=32;
     end
  else if (par_required_size>48) and (par_required_size<=96) then
     begin
       self.required_size:=64;
     end
  else if (par_required_size>96) and (par_required_size<=192) then
     begin
       self.required_size:=128;
     end
  else if (par_required_size>192) and (par_required_size<MaxInt) then
     begin
       self.required_size:=256;
     end
  else
      self.required_size:=32;

  { преобразуем число в строку для использования в качестве ключа }
  requred_size_dirname:=inttostr(self.required_size);
  { получим словарь второго уровня }
  tiles_subdict:=self.tiles_dict[requred_size_dirname];

  { долго и муторно по одной штуке обновляем ссылки на тайлы
    актуального размера }
  self.tile_default    := tiles_subdict['default.bmp'];

  self.tile_tc_unknown := tiles_subdict['tc_unknown.bmp'];
  self.tile_tc_flagged := tiles_subdict['tc_flagged.bmp'];
  self.tile_tc_chord   := tiles_subdict['tc_chord.bmp'];

  self.tile_bc_empty   := tiles_subdict['bc_empty.bmp'];
  self.tile_bc_near_1  := tiles_subdict['bc_near_1.bmp'];
  self.tile_bc_near_2  := tiles_subdict['bc_near_2.bmp'];
  self.tile_bc_near_3  := tiles_subdict['bc_near_3.bmp'];
  self.tile_bc_near_4  := tiles_subdict['bc_near_4.bmp'];
  self.tile_bc_near_5  := tiles_subdict['bc_near_5.bmp'];
  self.tile_bc_near_6  := tiles_subdict['bc_near_6.bmp'];
  self.tile_bc_near_7  := tiles_subdict['bc_near_7.bmp'];
  self.tile_bc_near_8  := tiles_subdict['bc_near_8.bmp'];
  self.tile_bc_installed_mine := tiles_subdict['bc_installed_mine.bmp'];
  self.tile_bc_exploded_mine  := tiles_subdict['bc_exploded_mine.bmp'];

end;

procedure T_Asset_Pack.init_dir_and_file_names(par_asset_pack_dirname: String);
begin
  self.asset_pack_dirname:=par_asset_pack_dirname; // сохраним имя директории ассет пака

  self.assets_containing_dirname_full_path:=settings_manager.get_root_application_dir +
        PathDelim + 'asset_packs'  + PathDelim; // директория в которой лежат ассет-паки
  if (not DirectoryExists(self.assets_containing_dirname_full_path)) then
     raise E_Asset_Files_Error.Create('Directory named "asset_packs" which must containing asset packs not found!');

  self.asset_pack_current_full_patch:=self.assets_containing_dirname_full_path +
        PathDelim + self.asset_pack_dirname + PathDelim; // коренная директория текущего ассет-пака
  if (not DirectoryExists(self.asset_pack_current_full_patch) ) then
     raise E_Asset_Files_Error.Create('Asset Pack With Name "'+
                                      self.asset_pack_dirname
                                      +'" not found!');

  self.asset_pack_images_dirname_full_path:=self.asset_pack_current_full_patch +
        PathDelim + 'images' + PathDelim; // директория с картинками текущего ассет-пака
  if (not DirectoryExists(self.asset_pack_images_dirname_full_path)) then
     raise E_Asset_Files_Error.Create('In asset pack directory with images not found!');

  self.asset_pack_sounds_dirname_full_path:=self.asset_pack_current_full_patch +
        PathDelim + 'sounds' + PathDelim; // директория со звуками и музыкой текущего ассет-пака
  if (not DirectoryExists(self.asset_pack_sounds_dirname_full_path)) then
     raise E_Asset_Files_Error.Create('In asset pack directory with sounds not found!');

  self.asset_current_ini_filename_full_path:=self.asset_pack_current_full_patch +
        PathDelim  + assets_ini_filename; // файл конфигурации текущего ассет-пака
  if (not FileExists(self.asset_current_ini_filename_full_path)) then
     raise E_Asset_Files_Error.Create('In asset pack asset ".ini" file not found!');

end;

procedure T_Asset_Pack.load_all_asset_files; // загружает тайлы и настройки из ассет пака в ОЗУ
var
  i,j: integer;
  str_dir, str_file: string;
  size_key_str, tile_key_str : string;
  tile_files_dict : T_Tiles_One_Size_Dict;
  tmp_bmp : TBitmap;
begin
  // проверяем чтобы в ассет паке был ини-файл
  if ( not FileExists(self.asset_current_ini_filename_full_path) ) then
     raise  E_Asset_Files_Error.Create('In asset pack asset ".ini" file not found!');
  // ... ... ... загружаем ini-файл, если это нужно  ... ... ...

  { проверяем чтобы в ассет паке были все ожидаемые нами тайлы, в цикле просматриваем все каталоги
    с размерами и проверяем каждый файл }
  for i:=0 to total_sizes_count-1 do
    begin
    str_dir:=self.asset_pack_images_dirname_full_path + PathDelim + self.tile_sizes[i];
    if (not DirectoryExists(str_dir) ) then
       raise E_Asset_Files_Error.Create('Asset subdir with sizes "' +
                                         self.tile_sizes[i] +
                                         '" not found! ');
    for j:=0 to total_tile_files_count-1 do
       begin
       str_file:=str_dir + PathDelim  +  self.tile_files[j];
       if ( not FileExists( str_file ) ) then
          raise E_Asset_Files_Error.Create('Asset tile "' +
                                           self.tile_files[j] +
                                           '" file not found!');
       end;
    end;

  { проитерируем весь двумерный словарь тайлов и загрузим в него все BMP-картинки
    в объекты TBitmap, которые также создадим }
  for size_key_str in self.tiles_dict.Keys do  // итерируем по ключам первого уровня, эти ключи уже заполнены выше в конструкторе
     begin
     tile_files_dict := self.tiles_dict[size_key_str]; // возьмём ссылку на словарь второго уровня
     str_dir:=self.asset_pack_images_dirname_full_path + PathDelim + size_key_str ; // выясним директорию с файлами
     for i:=0 to self.total_tile_files_count-1 do // цикл по всем ожидаемым именам тайла, используем константу из класса
        begin
           str_file:=str_dir + PathDelim  +  self.tile_files[i]; // выясним имя файла
           tmp_bmp:=nil;
           tmp_bmp:=TBitmap.Create;  //создадим новый пустой объект-битмап
           tmp_bmp.LoadFromFile(str_file);  // загрузим в этот объект очередную картинку из ассет-пака
           tile_files_dict.AddOrSetValue(self.tile_files[i],tmp_bmp); // создадим новый элемент в словаре и поместим туда вновь созданный битмап
        end;
     end;

  { актуализируем ссылки на объекты тайлов TBitmap под значение
    self.required_size }
  self.actualize_size(self.required_size);

end;


// возвращает тайл (то есть объект-картинку) верхней матрицы по его коду.
// Тайла для ячеек верхней матрицы с кодом TC_OPENED не существует, так как
// это код для открытых ячеек верхней матрицы, под которыми юзер видит ячейки нижней матрицы
function T_Asset_Pack.get_top_tile_by_code(tile_code: TTop_Cell): TBitmap;
begin
   case tile_code of
     TC_UNKNOWN: Result:=self.tile_tc_unknown;
     TC_FLAGGED: Result:=self.tile_tc_flagged;
     TC_OPENED: Result:=nil;
   end;
end;

// возвращает тайл нижней матрицы по его коду.
function T_Asset_Pack.get_bottom_tile_by_code(tile_code: TBottom_Cell): TBitmap;
begin
   case tile_code of
    BC_EMPTY  : Result:=self.tile_bc_empty ;
    BC_NEAR_1 : Result:=self.tile_bc_near_1 ;
    BC_NEAR_2 : Result:=self.tile_bc_near_2 ;
    BC_NEAR_3 : Result:=self.tile_bc_near_3 ;
    BC_NEAR_4 : Result:=self.tile_bc_near_4 ;
    BC_NEAR_5 : Result:=self.tile_bc_near_5 ;
    BC_NEAR_6 : Result:=self.tile_bc_near_6 ;
    BC_NEAR_7 : Result:=self.tile_bc_near_7 ;
    BC_NEAR_8 : Result:=self.tile_bc_near_8 ;
    BC_INSTALLED_MINE : Result:=self.tile_bc_installed_mine;
    BC_EXPLODED_MINE  : Result:=self.tile_bc_exploded_mine;
   end;
end;


end.

