unit u_settings_manager;

{$ifdef FPC}

{$mode delphi}
{$H+}

{$endif}

interface

uses
  Classes, SysUtils, IniFiles,
  u_minesweeper_types;

const
   filename = 'settings.ini';
   code_name = 'Lazarus_MineSweeper';
   {$ifdef FPC}
     code_platform = 'LCL';
   {$else}
     code_platform = 'FMX';
   {$EndIf}

type

  {
    Менеджер настроек, все настройки хранятся в ini-файле и автоматически
    читаются-записываются из-в него.

    Этот класс существует в одном экземпляре и объявлен в разделе var секции implementation
    этого модуля. Инициализирует и уничтожает объект этого класса главная форма f_main из
    модуля uf_main
  }

  { эксепшн если программа при запуске не нашла директорию с файлами для программы }
  E_Application_Files_Not_Found = class(Exception);

  { T_Settings_Manager }

  T_Settings_Manager = class
  private
    asset_packs_list : TStringList;
    root_application_dir : string;

    function get_config_filename : string;
    { ищет и возвращает главную директорию приложения по файлу конфигурации settings.ini
      главная директория определяется наличием этого файла и строки в нём:
           name=Lazarus_MineSweeper.
      и определённый код, к примеру:
           code=LCL
      Таким образом решается проблема компиляторов и IDE, которые размещают исполняемый файл
      в теневой папке компиляции }
    function  find_and_get_main_application_dir: string;
    { изменяет главную директорию на найденную предыдущей функцией }
  protected
  public
    function get_root_application_dir : string; // возвращает текущий коренной каталог приложения

    constructor Create; // конструктор
    destructor Destroy; override; // деструктор

    procedure chdir_to_root_application_dir;

    function  get_asset_pack_name:String; // геттер для текущего имени ассет пака
    procedure set_asset_pack_name(asset_pack_name:String);// сеттер для текущего имени ассет пака

    function  get_asset_packs_list : TStringList; // список всех доступных ассет паков

    function  get_music_state_on : boolean;    // геттер для вкл. музыки
    procedure set_music_state_on( music_on : boolean ); // сеттер для вко. музыки

    function  get_sounds_state_on : boolean; // геттер для вкл. звуков
    procedure set_sounds_state_on( sounds_on : boolean ); // сеттер для вкл. звуков


    function  get_sounds_volume : integer;  // геттер для громкости звуков
    procedure set_sounds_volume( volume: integer ); // сеттер для громкости звуков


    function  get_music_volume : integer; // геттер для громкоси музыки
    procedure set_music_volume( volume: integer ); // сеттер для громкости музыки

  end;

var
  settings_manager : T_Settings_Manager;

implementation
uses

{$ifdef FPC}
   Dialogs,Forms;
{$else}
   FMX.Dialogs,FMX.Forms;
{$endif}

{ T_Settings_Manager }

constructor T_Settings_Manager.Create;
begin
  inherited Create;
  self.asset_packs_list:=nil;

  self.chdir_to_root_application_dir;

end;

destructor T_Settings_Manager.Destroy;
begin

  inherited Destroy;
end;

function  T_Settings_Manager.get_config_filename: string;
begin
  Result := self.root_application_dir + PathDelim + filename;
end;

{ ищет главную директорию приложения, в которой расположены все его файлы,
  и возвращает путь. Если директория не найдена - возвращает пустую строку }
function T_Settings_Manager.find_and_get_main_application_dir: string;

{ ищет в директории it и возвращает истину если найдено }
function find_in_it( it:string ) : boolean;
var
  IniF:TINIFile;
  ff: string;
  c_code, c_platform : string;
begin
  Result:=False;
  ff:= it+PathDelim+filename;
  if ( not FileExists(ff) ) then exit;
  IniF:=TIniFile.Create(ff);

  c_code:= INiF.ReadString('general','code_name','');
  c_platform:= INiF.ReadString('general','code_platform','');

  if (c_code=code_name) and (c_platform=code_platform) then
    Result:=true;

  FreeAndNil(IniF);
end;

{ ищет во всех поддиректориях первого уровня it и возвращает истину
  если найдено и абсолютный путь в found }
function find_in_one_subdirs( it:string; var found: string ) : boolean;
var
  sr: TSearchRec;
  path: string;

begin
  path:=it;
  found:='';
  Result:=False;

  if FindFirst(path + '*', faAnyFile, sr) = 0 then
  begin
    repeat
      if (sr.Attr and faDirectory) <> 0 then  // если найденный файл - папка
        begin
        if (sr.Name <> '.') and (sr.Name <> '..') then  // игнорировать служебные папки
          begin
            if ( find_in_it(it+PathDelim+sr.Name) ) then
              begin
                found:=it+PathDelim+sr.Name+PathDelim;
                Result:=true;
                break;
              end;
          end;
        end;
    until FindNext(sr) <> 0;
  end;
  FindClose(sr);
end;

var
   start_dir_str : string;
   found_dir_str : string;
begin
  {$ifdef FPC}
  { FreePascal }
      start_dir_str:=ExtractFilePath(Application.ExeName);
  {$Else}
  { Delphi }
     start_dir_str:=ExtractFilePath(ParamStr(0));
  {$EndIf}

  Result:='';

  if ( find_in_it(start_dir_str) ) then
    begin
      Result:=ExpandFileName( start_dir_str );
    end
  else if ( find_in_it(start_dir_str+PathDelim+'..' ) ) then
    begin
      Result:=ExpandFileName( start_dir_str+PathDelim+'..' );
    end
  else if ( find_in_it(start_dir_str+PathDelim+'..'+PathDelim+'..' ) ) then
    begin
      Result:=ExpandFileName( start_dir_str+PathDelim+'..'+PathDelim+'..' );
    end;

  Result:=Result + PathDelim;

end;

function T_Settings_Manager.get_root_application_dir: string;
begin
  Result:=self.root_application_dir;
end;

procedure T_Settings_Manager.chdir_to_root_application_dir;
var
   tmp_s_path: string;
begin
  tmp_s_path:=self.find_and_get_main_application_dir;
  if (tmp_s_path='') then
    raise E_Application_Files_Not_Found.Create('При запуске программы не найден каталог'+
               ' с необходимыми файлами программы');
  self.root_application_dir:=tmp_s_path + PathDelim;
  ChDir(self.root_application_dir);
end;

function T_Settings_Manager.get_asset_pack_name: String;
var
  IniF:TINIFile;
  ff: string;
begin
  ff:=self.get_config_filename;
  if ( not FileExists(ff) ) then exit;
  IniF:=TIniFile.Create(ff);

  Result:= INiF.ReadString('general','asset_pack_name','asset_pack_blue');

  FreeAndNil(IniF);
end;

procedure T_Settings_Manager.set_asset_pack_name(asset_pack_name: String);
var
  IniF:TINIFile;
  ff: string;
begin
  ff:=self.get_config_filename;
  IniF:=TIniFile.Create(ff);

  IniF.WriteString('general','asset_pack_name',asset_pack_name);

  FreeAndNil(IniF);
end;

function T_Settings_Manager.get_asset_packs_list: TStringList;
var
  sr: TSearchRec;
  path: string;
begin
  if ( Assigned(self.asset_packs_list) ) then // если уже есть список строк для ассет-паков, то удалим его
    FreeAndNil(self.asset_packs_list);

  self.asset_packs_list:=TStringList.Create; // создадим список строк для ассет-паков

  path:=self.root_application_dir + 'asset_packs' + PathDelim;


  if FindFirst(path + '*', faAnyFile, sr) = 0 then
  begin
    repeat
      if (sr.Attr and faDirectory) <> 0 then  // если найденный файл - папка
        begin
        if (sr.Name <> '.') and (sr.Name <> '..') then  // игнорировать служебные папки
          begin
            self.asset_packs_list.Append(sr.Name);
          end;
        end;
    until FindNext(sr) <> 0;
  end;
  FindClose(sr);

  Result:=self.asset_packs_list;

end;

function T_Settings_Manager.get_music_state_on: boolean;
var
  IniF:TINIFile;
  ff: string;
begin
  ff:=self.get_config_filename;
  if ( not FileExists(ff) ) then exit;
  IniF:=TIniFile.Create(ff);

  Result:= LowerCase( INiF.ReadString('sound','music_state','off') ) = 'on';

  FreeAndNil(IniF);
end;

procedure T_Settings_Manager.set_music_state_on(music_on: boolean);
var
  IniF:TINIFile;
  ff: string;
  state : string;
begin
  ff:=self.get_config_filename;
  IniF:=TIniFile.Create(ff);

  if music_on then
    state:='on'
  else
    state:='off';

  IniF.WriteString('sound','music_state',state);

  FreeAndNil(IniF);
end;

function T_Settings_Manager.get_sounds_state_on: boolean;
var
  IniF:TINIFile;
  ff: string;
begin
  ff:=self.get_config_filename;
  if ( not FileExists(ff) ) then exit;
  IniF:=TIniFile.Create(ff);

  Result:= LowerCase( INiF.ReadString('sound','sounds_state','off') ) = 'on';

  FreeAndNil(IniF);
end;

procedure T_Settings_Manager.set_sounds_state_on(sounds_on: boolean);
var
  IniF:TINIFile;
  ff: string;
  state : string;
begin
  ff:=self.get_config_filename;
  IniF:=TIniFile.Create(ff);

  if sounds_on then
    state:='on'
  else
    state:='off';

  IniF.WriteString('sound','sounds_state',state);

  FreeAndNil(IniF);
end;

function T_Settings_Manager.get_sounds_volume: integer;
var
  IniF:TINIFile;
  ff: string;
begin
  ff:=self.get_config_filename;
  if ( not FileExists(ff) ) then exit;
  IniF:=TIniFile.Create(ff);

  Result:= IniF.ReadInteger('sound','sounds_volume',50);
  if (Result<0)or (Result>100) then Result:=50;

  FreeAndNil(IniF);
end;

procedure T_Settings_Manager.set_sounds_volume(volume: integer);
var
  IniF:TINIFile;
  ff: string;
  tmp_vol: integer;
begin
  ff:=self.get_config_filename;
  IniF:=TIniFile.Create(ff);

  if (volume<0) or (volume>100) then
    tmp_vol:=50
  else
    tmp_vol:=volume;


  IniF.WriteInteger('sound','sounds_volume',tmp_vol);

  FreeAndNil(IniF);
end;

function T_Settings_Manager.get_music_volume: integer;
var
  IniF:TINIFile;
  ff: string;
begin
  ff:=self.get_config_filename;
  if ( not FileExists(ff) ) then exit;
  IniF:=TIniFile.Create(ff);

  Result:= IniF.ReadInteger('sound','music_volume',50);
  if (Result<0)or (Result>100) then Result:=50;

  FreeAndNil(IniF);
end;

procedure T_Settings_Manager.set_music_volume(volume: integer);
var
  IniF:TINIFile;
  ff: string;
  tmp_vol: integer;
begin
  ff:=self.get_config_filename;
  IniF:=TIniFile.Create(ff);

  if (volume<0) or (volume>100) then
    tmp_vol:=50
  else
    tmp_vol:=volume;


  IniF.WriteInteger('sound','music_volume',tmp_vol);

  FreeAndNil(IniF);
end;


end.

