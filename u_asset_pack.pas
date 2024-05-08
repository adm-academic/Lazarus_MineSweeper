{
Объект для доступа к ассет паку. Тайлам и конфигурации. Это сделано для скорости и
гибкости итоговой программы.
}
unit u_asset_pack;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics
  ,u_minesweeper_types;

type
  { E_Asset_Files_Error }
  E_Asset_Files_Error = class(Exception); // эксепшн доступа к файлам ассет пака

  { T_Asset_Pack }
  P_Asset_Pack = ^T_Asset_Pack;
  T_Asset_Pack = class( TObject ) // класс для управления и кеширования ассет пака
    private
      assets_containing_dirname : string;// имя директории, содержащей папки с наборами ассетов
      asset_pack_dirname : string;// имя текущего абора ассетов
      asset_pack_full_patch : string;// полный путь до текущего набора ассетов
      asset_ini_filename : string; // полное имя файла с настройками текущего набора ассетов

      procedure load_all_asset_files; // загружает все ассеты в ОЗУ

    public
      tile_default : TPortableNetworkGraphic;
      tile_tc_unknown : TPortableNetworkGraphic;
      tile_tc_flagged : TPortableNetworkGraphic;
      tile_bc_empty :  TPortableNetworkGraphic;
      tile_bc_near_1 : TPortableNetworkGraphic;
      tile_bc_near_2 : TPortableNetworkGraphic;
      tile_bc_near_3 : TPortableNetworkGraphic;
      tile_bc_near_4 : TPortableNetworkGraphic;
      tile_bc_near_5 : TPortableNetworkGraphic;
      tile_bc_near_6 : TPortableNetworkGraphic;
      tile_bc_near_7 : TPortableNetworkGraphic;
      tile_bc_near_8 : TPortableNetworkGraphic;
      tile_bc_installed_mine : TPortableNetworkGraphic;
      tile_bc_exploded_mine : TPortableNetworkGraphic;

    protected

    public
      constructor Create( par_asset_pack_dirname:String );// конструктор, загружаем ассет пак из папки
      destructor Destroy; override; // деструктор

      function get_top_tile_by_code(tile_code:TTop_Cell):TPortableNetworkGraphic; // возвращает объект тайла верхней матрицы по его коду
      function get_bottom_tile_by_code(tile_code:TBottom_Cell):TPortableNetworkGraphic;// возвращает объект тайла нижней матрицы по его коду

      procedure debug; // всякий грязный код и отладка

  end;

implementation
uses
  Dialogs;

{ T_Asset_Pack }
constructor T_Asset_Pack.Create( par_asset_pack_dirname: String );
begin
   inherited Create;
   self.assets_containing_dirname:= 'asset_packs'; // папка содержащая субдиректории с ассетами
   self.asset_pack_dirname:= par_asset_pack_dirname; // имя субдиректории с загружаемым набором ассетов
   self.asset_pack_full_patch:=
     ExtractFilePath(ParamStr(0))
     + self.assets_containing_dirname + PathDelim
     + self.asset_pack_dirname + PathDelim; // полный путь до папки с загружаемым набором ассетов

   self.load_all_asset_files; // загружает все ассеты: ini-файл с конфигурацией и требуемые тайлы из ассет пака
end;

destructor T_Asset_Pack.Destroy;
begin
  // выгрузим тайлы из памяти
  tile_default.Free; tile_default:=nil;
  tile_tc_unknown.Free;   tile_tc_unknown:=nil;
  tile_tc_flagged.Free;   tile_tc_flagged:=nil;
  tile_bc_empty.Free;    tile_bc_empty:=nil;
  tile_bc_near_1.Free;   tile_bc_near_1:=nil;
  tile_bc_near_2.Free;   tile_bc_near_2:=nil;
  tile_bc_near_3.Free;   tile_bc_near_3:=nil;
  tile_bc_near_4.Free;   tile_bc_near_4:=nil;
  tile_bc_near_5.Free;   tile_bc_near_5:=nil;
  tile_bc_near_6.Free;   tile_bc_near_6:=nil;
  tile_bc_near_7.Free;   tile_bc_near_7:=nil;
  tile_bc_near_8.Free;   tile_bc_near_8:=nil;
  tile_bc_installed_mine.Free;   tile_bc_installed_mine:=nil;
  tile_bc_exploded_mine.Free;    tile_bc_exploded_mine:=nil;
  inherited Destroy;
end;

// возвращает тайл (то есть объект-картинку) верхней матрицы по его коду.
// Тайла для ячеек верхней матрицы с кодом TC_OPENED не существует, так как
// это код для открытых ячеек верхней матрицы, под которыми юзер видит ячейки нижней матрицы
function T_Asset_Pack.get_top_tile_by_code(tile_code: TTop_Cell
  ): TPortableNetworkGraphic;
begin
   case tile_code of
     TC_UNKNOWN: Result:=self.tile_tc_unknown;
     TC_FLAGGED: Result:=self.tile_tc_flagged;
     TC_OPENED: Result:=nil;
   end;
end;

// возвращает тайл нижней матрицы по его коду.
function T_Asset_Pack.get_bottom_tile_by_code(tile_code: TBottom_Cell
  ): TPortableNetworkGraphic;
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

procedure T_Asset_Pack.debug;
begin
  // ... процедура для отладки этого класса ...
end;

procedure T_Asset_Pack.load_all_asset_files; // загружает тайлы и настройки из ассет пака в ОЗУ
{ я намерено не стал использовать ассоциированные массивы (Map,Dict) в этом классе, поскольку я намереваюсь
 бэкпортировать всю эту игру в старые версии FPC,Lazarus и Delphi. }
const
   total_tile_files_count = 14;   // ожидаемое количество тайлов в любом ассет паке
   tile_files : array [0..total_tile_files_count-1] of string = // перечень имён тайлов в любом ассет паке
                                                                // используется для проверки корректности
                                                                // набора тайлов.
    ( 'default.png',
      'tc_unknown.png',
      'tc_flagged.png',
      'bc_empty.png',
      'bc_near_1.png',
      'bc_near_2.png',
      'bc_near_3.png',
      'bc_near_4.png',
      'bc_near_5.png',
      'bc_near_6.png',
      'bc_near_7.png',
      'bc_near_8.png',
      'bc_installed_mine.png',
      'bc_exploded_mine.png' );
var
  i: integer;
begin
  // проверяем чтобы в ассет паке был ини-файл
  self.asset_ini_filename:= 'asset.ini';
  if ( not FileExists(self.asset_pack_full_patch + self.asset_ini_filename) ) then
     raise  E_Asset_Files_Error.Create('Asset ".ini" file not found!');
  // ... --- загружаем ini-файл, если это нужно --- ...

  // проверяем чтобы в ассет паке были все ожидаемые нами тайлы, в цикле просматриваем весь массив tile_files[]
  for i:=0 to total_tile_files_count-1 do
     if ( not FileExists( self.asset_pack_full_patch + tile_files[i] ) ) then
        raise E_Asset_Files_Error.Create('Asset tile "'+tile_files[i]+'" file not found!');

  // ниже долго и муторно загружаем все тайлы по одной штуке
  self.tile_default := TPortableNetworkGraphic.Create;
  self.tile_default.LoadFromFile( self.asset_pack_full_patch + 'default.png' );

  self.tile_tc_unknown := TPortableNetworkGraphic.Create;
  self.tile_tc_unknown.LoadFromFile( self.asset_pack_full_patch + 'tc_unknown.png' );

  self.tile_tc_flagged := TPortableNetworkGraphic.Create;
  self.tile_tc_flagged.LoadFromFile( self.asset_pack_full_patch + 'tc_flagged.png' );

  self.tile_bc_empty :=  TPortableNetworkGraphic.Create;
  self.tile_bc_empty.LoadFromFile( self.asset_pack_full_patch + 'bc_empty.png' );

  self.tile_bc_near_1 := TPortableNetworkGraphic.Create;
  self.tile_bc_near_1.LoadFromFile( self.asset_pack_full_patch + 'bc_near_1.png' );

  self.tile_bc_near_2 := TPortableNetworkGraphic.Create;
  self.tile_bc_near_2.LoadFromFile( self.asset_pack_full_patch + 'bc_near_2.png' );

  self.tile_bc_near_3 := TPortableNetworkGraphic.Create;
  self.tile_bc_near_3.LoadFromFile( self.asset_pack_full_patch + 'bc_near_3.png' );

  self.tile_bc_near_4 := TPortableNetworkGraphic.Create;
  self.tile_bc_near_4.LoadFromFile( self.asset_pack_full_patch + 'bc_near_4.png' );

  self.tile_bc_near_5 := TPortableNetworkGraphic.Create;
  self.tile_bc_near_5.LoadFromFile( self.asset_pack_full_patch + 'bc_near_5.png' );

  self.tile_bc_near_6 := TPortableNetworkGraphic.Create;
  self.tile_bc_near_6.LoadFromFile( self.asset_pack_full_patch + 'bc_near_6.png' );

  self.tile_bc_near_7 := TPortableNetworkGraphic.Create;
  self.tile_bc_near_7.LoadFromFile( self.asset_pack_full_patch + 'bc_near_7.png' );

  self.tile_bc_near_8 := TPortableNetworkGraphic.Create;
  self.tile_bc_near_8.LoadFromFile( self.asset_pack_full_patch + 'bc_near_8.png' );

  self.tile_bc_installed_mine := TPortableNetworkGraphic.Create;
  self.tile_bc_installed_mine.LoadFromFile( self.asset_pack_full_patch + 'bc_installed_mine.png' );

  self.tile_bc_exploded_mine := TPortableNetworkGraphic.Create;
  self.tile_bc_exploded_mine.LoadFromFile( self.asset_pack_full_patch + 'bc_exploded_mine.png' );

end;



end.

