unit u_about_program;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { Tf_about_program }

  Tf_about_program = class(TForm)
    Button1: TButton;
    Label1: TLabel;
  private

  public

  end;

var
  f_about_program: Tf_about_program;

implementation

{$R *.lfm}

end.

