unit u_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, Types, Math
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
    procedure b_start_refactoredClick(Sender: TObject);
    procedure b_unload_refactoredClick(Sender: TObject);
  private

  public
     mine_sweeper : T_Mine_Sweeper;
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

