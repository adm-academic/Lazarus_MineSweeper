unit uf_about_program;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { Tf_about_program }

  Tf_about_program = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
  private

  public

  end;

var
  f_about_program: Tf_about_program;

implementation

{$R *.lfm}

end.

