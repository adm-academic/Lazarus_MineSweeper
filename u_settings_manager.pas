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

type

  { Менеджер настроек, все настройки хранятся в ini-файле и автоматически
    читаются-записываются из-в него
  }

  { T_Settings_Manager }

  T_Settings_Manager = class
  private
    asset_packs_list : TStringList;
    function get_config_filename : string;
  protected
  public
    constructor Create; // конструктор
    destructor Destroy; override; // деструктор

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

implementation
uses
  Dialogs,Forms;

{ T_Settings_Manager }

function T_Settings_Manager.get_config_filename: string;
begin
  Result := ExtractFilePath(Application.ExeName) + PathDelim + filename;
end;

constructor T_Settings_Manager.Create;
begin
  inherited Create;
  self.asset_packs_list:=nil;

end;

destructor T_Settings_Manager.Destroy;
begin

  inherited Destroy;
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

  path:=ExtractFilePath(Application.ExeName) + 'asset_packs' + PathDelim;

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

