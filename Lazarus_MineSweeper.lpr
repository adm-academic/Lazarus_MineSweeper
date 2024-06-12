program Lazarus_MineSweeper;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, u_main, u_start_new, u_records, u_your_difficulty_level, u_settings, 
u_about_development_env, u_about_program, u_records_manager
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(Tf_main, f_main);
  Application.Run;
end.

