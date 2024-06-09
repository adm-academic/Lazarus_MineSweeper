unit u_start_new;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls
  ,u_your_difficulty_level;

type

  { Тип перечисления для обозначения сложности игры }
  T_Game_Difficulty = ( GD_EASY,
                        GD_MEDIUM,
                        GD_HARD,
                        GD_CUSTOM );
  { Tf_start_new }
  Tf_start_new = class(TForm)
    b_set_configuration: TButton;
    b_start: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lb_custom_string: TLabel;
    rb_easy: TRadioButton;
    rb_medium: TRadioButton;
    rb_hard: TRadioButton;
    rb_custom_configuration: TRadioButton;
    procedure b_set_configurationClick(Sender: TObject);
    procedure b_startClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private

  public
     diff_level_code : T_Game_Difficulty; // выбранная в окне сложность игры
     diff_field_width : integer ; // ширина поля игры
     diff_field_height : integer ; // высота поля игры
     diff_field_N_mines : integer ; // количество мин на поле
  end;

var
  f_start_new: Tf_start_new;

implementation

{$R *.lfm}

{ Tf_start_new }

procedure Tf_start_new.b_startClick(Sender: TObject);
begin
  if ( rb_easy.Checked ) then
  begin
    self.diff_level_code:=GD_EASY;
    self.diff_field_width:=9;
    self.diff_field_height:=9;
    self.diff_field_N_mines:=10;
    self.ModalResult:=mrOK;
  end
  else if ( rb_medium.Checked ) then
  begin
    self.diff_level_code:=GD_MEDIUM;
    self.diff_field_width:=16;
    self.diff_field_height:=16;
    self.diff_field_N_mines:=40;
    self.ModalResult:=mrOK;
  end
  else if ( rb_hard.Checked ) then
  begin
    self.diff_level_code:=GD_HARD;
    self.diff_field_width:=30;
    self.diff_field_height:=16;
    self.diff_field_N_mines:=99;
    self.ModalResult:=mrOK;
  end
  else if ( rb_custom_configuration.Checked ) then
  begin
    self.diff_level_code:=GD_CUSTOM;
    self.diff_field_width:=f_your_difficulty_level.se_colcount.Value;
    self.diff_field_height:=f_your_difficulty_level.se_rowcount.Value;
    self.diff_field_N_mines:=f_your_difficulty_level.se_N_mines.Value;
    self.ModalResult:=mrOK;
  end;
  self.Hide;
end;

procedure Tf_start_new.FormCreate(Sender: TObject);
begin
   { Значения для формы по-умолчанию при запуске программы,
     выставим лёгкий уровень }
   self.rb_easy.Checked:=True;
   self.diff_level_code:=GD_EASY;
   self.diff_field_width:=9;
   self.diff_field_height:=9;
   self.diff_field_N_mines:=10;
end;

procedure Tf_start_new.FormPaint(Sender: TObject);
begin
  self.lb_custom_string.Caption:='СВОЯ: Столбцов='+inttostr(f_your_difficulty_level.se_colcount.Value) + '; '
                                + 'Строк=' + inttostr(f_your_difficulty_level.se_rowcount.Value) + '; '
                                + 'Мин='+ inttostr(f_your_difficulty_level.se_N_mines.Value) + ';';
end;

procedure Tf_start_new.b_set_configurationClick(Sender: TObject);
var
  tmp_modal_result:integer;
begin
  rb_custom_configuration.Checked:=True;
  self.Repaint;
  Application.ProcessMessages;
  f_your_difficulty_level.ShowModal;
end;

end.

