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
  Classes, SysUtils, Graphics
  ,u_minesweeper_types;

const
   assets_ini_filename='asset.ini';

type
  { E_Asset_Files_Error }
  E_Asset_Files_Error = class(Exception); // эксепшн доступа к файлам ассет пака

  { T_Asset_Pack }
  P_Asset_Pack = ^T_Asset_Pack;
  T_Asset_Pack = class( TObject ) // класс для управления и кеширования ассет пака
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


      { инициализирует переменные с путями до директорий и файлов ассет пака }
      procedure init_dir_and_file_names( par_asset_pack_dirname:String );


      { загружает все ассеты в ОЗУ }
      procedure load_all_asset_files;


    public
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

      function get_top_tile_by_code(tile_code:TTop_Cell):TBitmap; // возвращает объект тайла верхней матрицы по его коду
      function get_bottom_tile_by_code(tile_code:TBottom_Cell):TBitmap;// возвращает объект тайла нижней матрицы по его коду

  end;

implementation
uses
  Dialogs,Forms;

{ T_Asset_Pack }
constructor T_Asset_Pack.Create( par_asset_pack_dirname: String );
begin
   inherited Create;

   { настраивает переменные-пути для набора ассетов }
   self.init_dir_and_file_names( par_asset_pack_dirname );

   { ... ... ... загрузить конфигурацию из ini-файла ... ... ... }

   { загружает ассеты-тайлы }
   self.load_all_asset_files;

   { ... ... ... загрузить ассеты-звуки и музыку ... ... ... }

end;

destructor T_Asset_Pack.Destroy;
begin
  // выгрузим тайлы из памяти
  FreeAndNil( tile_default );

  FreeAndNil( tile_tc_unknown );
  FreeAndNil( tile_tc_flagged );
  FreeAndNil( tile_tc_chord );

  FreeAndNil( tile_bc_empty );
  FreeAndNil( tile_bc_near_1 );
  FreeAndNil( tile_bc_near_2 );
  FreeAndNil( tile_bc_near_3 );
  FreeAndNil( tile_bc_near_4 );
  FreeAndNil( tile_bc_near_5 );
  FreeAndNil( tile_bc_near_6 );
  FreeAndNil( tile_bc_near_7 );
  FreeAndNil( tile_bc_near_8 );
  FreeAndNil( tile_bc_installed_mine );
  FreeAndNil( tile_bc_exploded_mine );
  inherited Destroy;
end;

{  ...
   объект настроек лучше всего сделать через var, как делаются формы, тогда
   важные настройки приложения можно будет хранить в одном месте и
   отовсюду к ним просто обращаться. Это нужно к примеру, чтобы кроссплатформенно
   и единнобразно получать путь до главной директории приложения, что понадобится при
   конвертации в код библиотеки FMX
   ...
}

procedure T_Asset_Pack.init_dir_and_file_names(par_asset_pack_dirname: String);
begin
  self.asset_pack_dirname:=par_asset_pack_dirname; // сохраним имя директории ассет пака

  self.assets_containing_dirname_full_path:= ExtractFilePath(Application.ExeName) +
        PathDelim + 'asset_packs'  + PathDelim; // директория в которой лежат ассет-паки

  self.asset_pack_current_full_patch:=self.assets_containing_dirname_full_path +
        PathDelim + self.asset_pack_dirname + PathDelim; // коренная директория текущего ассет-пака

  self.asset_pack_images_dirname_full_path:=self.asset_pack_current_full_patch +
        PathDelim + 'images' + PathDelim; // директория с картинками текущего ассет-пака

  self.asset_pack_sounds_dirname_full_path:=self.asset_pack_current_full_patch +
        PathDelim + 'sounds' + PathDelim; // директория со звуками и музыкой текущего ассет-пака

  self.asset_current_ini_filename_full_path:=self.asset_pack_current_full_patch +
        PathDelim  + assets_ini_filename; // файл конфигурации текущего ассет-пака

end;

procedure T_Asset_Pack.load_all_asset_files; // загружает тайлы и настройки из ассет пака в ОЗУ
{ не стал использовать ассоциативные массивы Map,Dict в коде этого класса,
  и об этом нужно крепко подумать, на настоящий момент их используют все и где угодно.
  выиграть от их игнорирования можно только на совсем уж маленьких и редко вызываемых
  участках кода.
}
const
   total_tile_files_count = 15;   // ожидаемое количество тайлов в любом ассет паке
   tile_files : array [0..total_tile_files_count-1] of string = // перечень имён тайлов в любом ассет паке
                                                                // используется для проверки корректности
                                                                // набора тайлов.
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
var
  i: integer;
begin
  // проверяем чтобы в ассет паке был ини-файл
  if ( not FileExists(self.asset_current_ini_filename_full_path) ) then
     raise  E_Asset_Files_Error.Create('Asset ".ini" file not found!');
  // ... ... ... загружаем ini-файл, если это нужно  ... ... ...

  // проверяем чтобы в ассет паке были все ожидаемые нами тайлы, в цикле просматриваем весь массив tile_files[]
  for i:=0 to total_tile_files_count-1 do
     if ( not FileExists( self.asset_pack_images_dirname_full_path + tile_files[i] ) ) then
        raise E_Asset_Files_Error.Create('Asset tile "'+tile_files[i]+'" file not found!');

  // ниже долго и муторно загружаем все тайлы по одной штуке
  self.tile_default := TBitmap.Create;
  self.tile_default.LoadFromFile( self.asset_pack_images_dirname_full_path + 'default.bmp' );

  self.tile_tc_unknown := TBitmap.Create;
  self.tile_tc_unknown.LoadFromFile( self.asset_pack_images_dirname_full_path + 'tc_unknown.bmp' );

  self.tile_tc_flagged := TBitmap.Create;
  self.tile_tc_flagged.LoadFromFile( self.asset_pack_images_dirname_full_path + 'tc_flagged.bmp' );

  self.tile_tc_chord := TBitmap.Create;
  self.tile_tc_chord.LoadFromFile( self.asset_pack_images_dirname_full_path +  'tc_chord.bmp' );

  self.tile_bc_empty :=  TBitmap.Create;
  self.tile_bc_empty.LoadFromFile( self.asset_pack_images_dirname_full_path + 'bc_empty.bmp' );

  self.tile_bc_near_1 := TBitmap.Create;
  self.tile_bc_near_1.LoadFromFile( self.asset_pack_images_dirname_full_path + 'bc_near_1.bmp' );

  self.tile_bc_near_2 := TBitmap.Create;
  self.tile_bc_near_2.LoadFromFile( self.asset_pack_images_dirname_full_path + 'bc_near_2.bmp' );

  self.tile_bc_near_3 := TBitmap.Create;
  self.tile_bc_near_3.LoadFromFile( self.asset_pack_images_dirname_full_path + 'bc_near_3.bmp' );

  self.tile_bc_near_4 := TBitmap.Create;
  self.tile_bc_near_4.LoadFromFile( self.asset_pack_images_dirname_full_path + 'bc_near_4.bmp' );

  self.tile_bc_near_5 := TBitmap.Create;
  self.tile_bc_near_5.LoadFromFile( self.asset_pack_images_dirname_full_path + 'bc_near_5.bmp' );

  self.tile_bc_near_6 := TBitmap.Create;
  self.tile_bc_near_6.LoadFromFile( self.asset_pack_images_dirname_full_path + 'bc_near_6.bmp' );

  self.tile_bc_near_7 := TBitmap.Create;
  self.tile_bc_near_7.LoadFromFile( self.asset_pack_images_dirname_full_path + 'bc_near_7.bmp' );

  self.tile_bc_near_8 := TBitmap.Create;
  self.tile_bc_near_8.LoadFromFile( self.asset_pack_images_dirname_full_path + 'bc_near_8.bmp' );

  self.tile_bc_installed_mine := TBitmap.Create;
  self.tile_bc_installed_mine.LoadFromFile( self.asset_pack_images_dirname_full_path + 'bc_installed_mine.bmp' );

  self.tile_bc_exploded_mine := TBitmap.Create;
  self.tile_bc_exploded_mine.LoadFromFile( self.asset_pack_images_dirname_full_path + 'bc_exploded_mine.bmp' );

end;


// возвращает тайл (то есть объект-картинку) верхней матрицы по его коду.
// Тайла для ячеек верхней матрицы с кодом TC_OPENED не существует, так как
// это код для открытых ячеек верхней матрицы, под которыми юзер видит ячейки нижней матрицы
function T_Asset_Pack.get_top_tile_by_code(tile_code: TTop_Cell
  ): TBitmap;
begin
   case tile_code of
     TC_UNKNOWN: Result:=self.tile_tc_unknown;
     TC_FLAGGED: Result:=self.tile_tc_flagged;
     TC_OPENED: Result:=nil;
   end;
end;

// возвращает тайл нижней матрицы по его коду.
function T_Asset_Pack.get_bottom_tile_by_code(tile_code: TBottom_Cell
  ): TBitmap;
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

