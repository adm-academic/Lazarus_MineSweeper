unit uf_about_development_env;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, CheckLst,
  ComCtrls, StdCtrls;

type

  { Tf_about_development_env }

  Tf_about_development_env = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Image3: TImage;
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    TabSheet1: TTabSheet;
  private

  public

  end;

var
  f_about_development_env: Tf_about_development_env;

implementation

{$R *.lfm}

end.

