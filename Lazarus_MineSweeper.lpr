program Lazarus_MineSweeper;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uf_main,
uf_start_new, uf_records, uf_your_difficulty_level, uf_settings,
uf_about_development_env, uf_about_program, u_records_manager
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(Tf_main, f_main);
  Application.Run;
end.

