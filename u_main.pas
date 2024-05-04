unit u_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  Types, Math;

type

  { Tf_main }

  Tf_main = class(TForm)
    DrawGrid1: TDrawGrid;
    Timer1: TTimer;
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;


var
  f_main: Tf_main;


type
  //...
  TBottom_Cell = ( BC_EMPTY  = 0,
                   BC_NEAR_1 = 1,
                   BC_NEAR_2 = 2,
                   BC_NEAR_3 = 3,
                   BC_NEAR_4 = 4,
                   BC_NEAR_5 = 5,
                   BC_NEAR_6 = 6,
                   BC_NEAR_7 = 7,
                   BC_NEAR_8 = 8,
                   BC_INSTALLED_MINE = 10,
                   BC_EXPLODED_MINE  = 1010
                 );
  TTop_Cell    = ( TC_CLOSED  = 0,
                   TC_FLAGGED = 1,
                   TC_OPENED  = 10
                 );
  TBottom_Matrix = array of array of TBottom_Cell;
  TTop_Matrix    = array of array of TTop_Cell;
  //...

implementation

{$R *.lfm}

{ Tf_main }

procedure Tf_main.DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  DrawGrid1.Canvas.Brush.Style:=bsSolid;
  DrawGrid1.Canvas.Brush.Color:=RandomRange( -$7FFFFFFF-1,$7FFFFFFF );
  DrawGrid1.Canvas.Ellipse(aRect);
end;


procedure Tf_main.FormCreate(Sender: TObject);
begin
  Randomize;
end;

procedure Tf_main.Timer1Timer(Sender: TObject);
begin
  self.DrawGrid1.Repaint;
end;

end.

