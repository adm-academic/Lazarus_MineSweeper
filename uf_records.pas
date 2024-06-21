unit uf_records;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ExtCtrls, u_records_manager, Types;

type

  { Tf_records }

  Tf_records = class(TForm)
    StringGrid1: TStringGrid;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public
    records_manager : T_Records_Manager;
  end;

var
  f_records: Tf_records;

implementation
uses u_minesweeper_types;

{$R *.lfm}

{ Tf_records }



procedure Tf_records.FormCreate(Sender: TObject);
begin
  self.records_manager:=T_Records_Manager.Create;
end;

procedure Tf_records.FormShow(Sender: TObject);
var
  total_i, i: integer;
  str, diff_str: string;
  totalwidth, curwidth, maxwidth : integer;
  column, row : integer;
begin
  { прочитаем все записи рекордов из файла }
  self.records_manager.f_read_all_records;

  { выведем рекорды в StringGrid }
  self.StringGrid1.RowCount:=1;
  for i:=0 to self.records_manager.records_easy.Count-1 do
  begin
    self.StringGrid1.RowCount:=self.StringGrid1.RowCount+1;
    self.StringGrid1.Cells[0,self.StringGrid1.RowCount-1]:='Лёгкая';
    self.StringGrid1.Cells[1,self.StringGrid1.RowCount-1]:=self.records_manager.records_easy[i].nickname;
    self.StringGrid1.Cells[2,self.StringGrid1.RowCount-1]:=inttostr(self.records_manager.records_easy[i].gametime_seconds);
    self.StringGrid1.Cells[3,self.StringGrid1.RowCount-1]:=inttostr(self.records_manager.records_easy[i].field_height);
    self.StringGrid1.Cells[4,self.StringGrid1.RowCount-1]:=inttostr(self.records_manager.records_easy[i].field_width);
    self.StringGrid1.Cells[5,self.StringGrid1.RowCount-1]:=inttostr(self.records_manager.records_easy[i].field_N_mines);
  end;


  { выведем рекорды в StringGrid }
  for i:=0 to self.records_manager.records_medium.Count-1 do
  begin
    self.StringGrid1.RowCount:=self.StringGrid1.RowCount+1;
    self.StringGrid1.Cells[0,self.StringGrid1.RowCount-1]:='Средняя';
    self.StringGrid1.Cells[1,self.StringGrid1.RowCount-1]:=self.records_manager.records_medium[i].nickname;
    self.StringGrid1.Cells[2,self.StringGrid1.RowCount-1]:=inttostr(self.records_manager.records_medium[i].gametime_seconds);
    self.StringGrid1.Cells[3,self.StringGrid1.RowCount-1]:=inttostr(self.records_manager.records_medium[i].field_height);
    self.StringGrid1.Cells[4,self.StringGrid1.RowCount-1]:=inttostr(self.records_manager.records_medium[i].field_width);
    self.StringGrid1.Cells[5,self.StringGrid1.RowCount-1]:=inttostr(self.records_manager.records_medium[i].field_N_mines);
  end;

  { выведем рекорды в StringGrid }
  for i:=0 to self.records_manager.records_hard.Count-1 do
  begin
    self.StringGrid1.RowCount:=self.StringGrid1.RowCount+1;
    self.StringGrid1.Cells[0,self.StringGrid1.RowCount-1]:='Высокая';
    self.StringGrid1.Cells[1,self.StringGrid1.RowCount-1]:=self.records_manager.records_hard[i].nickname;
    self.StringGrid1.Cells[2,self.StringGrid1.RowCount-1]:=inttostr(self.records_manager.records_hard[i].gametime_seconds);
    self.StringGrid1.Cells[3,self.StringGrid1.RowCount-1]:=inttostr(self.records_manager.records_hard[i].field_height);
    self.StringGrid1.Cells[4,self.StringGrid1.RowCount-1]:=inttostr(self.records_manager.records_hard[i].field_width);
    self.StringGrid1.Cells[5,self.StringGrid1.RowCount-1]:=inttostr(self.records_manager.records_hard[i].field_N_mines);
  end;


  { выведем рекорды в StringGrid }
  for i:=0 to self.records_manager.records_custom.Count-1 do
  begin
    self.StringGrid1.RowCount:=self.StringGrid1.RowCount+1;
    self.StringGrid1.Cells[0,self.StringGrid1.RowCount-1]:='Своя';
    self.StringGrid1.Cells[1,self.StringGrid1.RowCount-1]:=self.records_manager.records_custom[i].nickname;
    self.StringGrid1.Cells[2,self.StringGrid1.RowCount-1]:=inttostr(self.records_manager.records_custom[i].gametime_seconds);
    self.StringGrid1.Cells[3,self.StringGrid1.RowCount-1]:=inttostr(self.records_manager.records_custom[i].field_height);
    self.StringGrid1.Cells[4,self.StringGrid1.RowCount-1]:=inttostr(self.records_manager.records_custom[i].field_width);
    self.StringGrid1.Cells[5,self.StringGrid1.RowCount-1]:=inttostr(self.records_manager.records_custom[i].field_N_mines);
  end;

  { растянем все столбцы под имеющийся в них текст максимальной длинны }
  totalwidth:=0;
  for column:=0 to self.StringGrid1.ColCount-1 do
   begin
      maxwidth:=0;
      for row:=0 to self.StringGrid1.RowCount-1 do // перебираем строки
      begin
          curwidth:=StringGrid1.Canvas.TextWidth(StringGrid1.Cells[column, row] + '    ');
          if ( curwidth>maxwidth ) then maxwidth:=curwidth;
      end;
      self.StringGrid1.ColWidths[column]:=maxwidth;
      totalwidth:=totalwidth+maxwidth;
   end;

  self.StringGrid1.Width:=totalwidth;
  self.Width:=totalwidth;

  self.Position:=poMainFormCenter;
  self.StringGrid1.Repaint;

end;


procedure Tf_records.FormDestroy(Sender: TObject);
begin
  FreeAndNil( self.records_manager );
end;

end.

