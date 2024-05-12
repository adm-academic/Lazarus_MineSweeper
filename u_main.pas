unit u_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, Menus, Types, LCLType, Math
  , u_start_new
  , u_records
  , u_difficulty_level
  , u_settings
  , u_about_development_env
  , u_about_program
  , u_minesweeper_types
  , u_mine_sweeper
  , u_asset_pack
  , u_settings_manager ;

type

  { Tf_main }

  Tf_main = class(TForm)
    b_unload_refactored: TButton;
    b_start_refactored: TButton;
    dg_refactored_game: TDrawGrid;
    lb_game_state: TLabel;
    mi_exit: TMenuItem;
    mi_records: TMenuItem;
    mi_start_game: TMenuItem;
    mi_settings: TMenuItem;
    mi_difficulty_level: TMenuItem;
    mi_about_lazarus: TMenuItem;
    mi_about: TMenuItem;
    mi_m_help: TMenuItem;
    mi_m_settings: TMenuItem;
    mi_m_game: TMenuItem;
    mm_main: TMainMenu;
    procedure b_start_refactoredClick(Sender: TObject);
    procedure b_unload_refactoredClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure mi_aboutClick(Sender: TObject);
    procedure mi_about_lazarusClick(Sender: TObject);
    procedure mi_difficulty_levelClick(Sender: TObject);
    procedure mi_exitClick(Sender: TObject);
    procedure mi_recordsClick(Sender: TObject);
    procedure mi_settingsClick(Sender: TObject);
    procedure mi_start_gameClick(Sender: TObject);
  private

  public
     mine_sweeper : T_Mine_Sweeper; // это объект инкапсулирующий всю игровую логику


  end;


var
  f_main: Tf_main;

implementation

{$R *.lfm}

{ Tf_main }

procedure Tf_main.b_unload_refactoredClick(Sender: TObject);
begin
  self.mine_sweeper.Free;
  self.mine_sweeper := nil;
end;

procedure Tf_main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  Reply, BoxStyle: Integer;
begin
  BoxStyle := MB_ICONQUESTION + MB_YESNO;
  Reply := Application.MessageBox('Вы действительно хотите выйти из программы?', 'Подтверждение выхода', BoxStyle);
  if Reply=IDYES then
    CanClose:=True
  else
    CanClose:=False;
end;

procedure Tf_main.FormCreate(Sender: TObject);
begin
  { все неглавные формы я создаю в обработчике FormCreate, таким образом в
    большинстве обработчиков главной формы Tf_main эти формы будут доступны уже
    проинициализированными. Самы переменные этих форм, в с соответвии с соглашением
    Delphi и Lazarus по умолчанию находятся в публичной секции своих модулей }
  Application.CreateForm(Tf_start_new, f_start_new);
  Application.CreateForm(Tf_records, f_records);
  Application.CreateForm(Tf_difficulty_level, f_difficulty_level);
  Application.CreateForm(Tf_settings, f_settings);
  Application.CreateForm(Tf_about_development_env, f_about_development_env);
  Application.CreateForm(Tf_about_program, f_about_program);
end;

procedure Tf_main.mi_aboutClick(Sender: TObject);
begin
  f_about_program.ShowModal;
end;

procedure Tf_main.mi_about_lazarusClick(Sender: TObject);
begin
  f_about_development_env.ShowModal;
end;

procedure Tf_main.mi_difficulty_levelClick(Sender: TObject);
begin
  f_difficulty_level.ShowModal;
end;

procedure Tf_main.mi_exitClick(Sender: TObject);
begin
  self.Close;
end;

procedure Tf_main.mi_recordsClick(Sender: TObject);
begin
  f_records.ShowModal;
end;

procedure Tf_main.mi_settingsClick(Sender: TObject);
begin
  f_settings.ShowModal;
end;

procedure Tf_main.mi_start_gameClick(Sender: TObject);
begin
  f_start_new.ShowModal;
end;

procedure Tf_main.b_start_refactoredClick(Sender: TObject);
begin
  if self.mine_sweeper<>nil then // если в памяти уже есть объект T_Mine_Sweeper, то его нужно выгрузить
    begin
      self.mine_sweeper.Free;
      self.mine_sweeper := nil;
    end;
  self.mine_sweeper := T_Mine_Sweeper.Create( self, self.dg_refactored_game,
                       self.lb_game_state, 'asset_pack_blue' ); // создадим объект T_Mine_Sweeper
  self.mine_sweeper.start_game; // стартуем игру
end;


end.

