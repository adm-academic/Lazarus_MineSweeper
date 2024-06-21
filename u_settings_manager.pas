unit u_settings_manager;

{$ifdef FPC}

{$mode delphi}
{$H+}

{$endif}

interface

uses
  Classes, SysUtils,
  u_minesweeper_types;

type

  { Менеджер настроек, все настройки хранятся в ini-файле и автоматически
    читаются-записываются из-в него
  }

  { T_Settings_Manager }

  T_Settings_Manager = class
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ T_Settings_Manager }

constructor T_Settings_Manager.Create;
begin
  inherited Create;
  //...
end;

destructor T_Settings_Manager.Destroy;
begin
  //...
  inherited Destroy;
end;

end.

