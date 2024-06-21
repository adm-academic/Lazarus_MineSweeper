unit uf_your_difficulty_level;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin;

type

  { Tf_your_difficulty_level }

  Tf_your_difficulty_level = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    se_rowcount: TSpinEdit;
    se_N_mines: TSpinEdit;
    se_colcount: TSpinEdit;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  f_your_difficulty_level: Tf_your_difficulty_level;

implementation

{$R *.lfm}

{ Tf_your_difficulty_level }

procedure Tf_your_difficulty_level.Button1Click(Sender: TObject);
begin
  self.Close;
end;

end.

