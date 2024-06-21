unit uf_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, Menus, Types, LCLType, Math
  , uf_start_new
  , uf_records
  , u_records_manager
  , uf_your_difficulty_level
  , uf_settings
  , uf_about_development_env
  , uf_about_program
  , u_minesweeper_types
  , u_mine_sweeper
  , u_asset_pack
  , u_settings_manager ;

type

  { Tf_main }

  Tf_main = class(TForm)
    dg_game: TDrawGrid;
    lb_game_state: TLabel;
    lb_game_mines: TLabel;
    lb_game_flags: TLabel;
    lb_game_time: TLabel;
    mi_clear_records: TMenuItem;
    mi_exit: TMenuItem;
    mi_records: TMenuItem;
    mi_start_game: TMenuItem;
    mi_settings: TMenuItem;
    mi_about_lazarus: TMenuItem;
    mi_about: TMenuItem;
    mi_m_help: TMenuItem;
    mi_m_settings: TMenuItem;
    mi_m_game: TMenuItem;
    mm_main: TMainMenu;
    pn_statuses: TPanel;
    pn_game: TPanel;
    Separator1: TMenuItem;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mi_clear_recordsClick(Sender: TObject);
    procedure mi_aboutClick(Sender: TObject);
    procedure mi_about_lazarusClick(Sender: TObject);
    procedure mi_exitClick(Sender: TObject);
    procedure mi_recordsClick(Sender: TObject);
    procedure mi_settingsClick(Sender: TObject);
    procedure mi_start_gameClick(Sender: TObject);
  private
    function get_form_current_tile_size : integer;

  public
     mine_sweeper : T_Mine_Sweeper; // это объект инкапсулирующий всю игровую логику
     procedure On_Mine_Sweeper_State_Change(  new_state : TGame_State  );

  end;


var
  f_main: Tf_main;

implementation

{$R *.lfm}

{ Tf_main }

procedure Tf_main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  Reply, BoxStyle: Integer;
begin
  BoxStyle := MB_ICONQUESTION + MB_YESNO;
  Reply := Application.MessageBox('Вы действительно хотите выйти из программы?', 'Подтверждение выхода', BoxStyle);
  if Reply=IDYES then
    CanClose:=True
  else
    CanClose:=False;
end;

procedure Tf_main.FormCreate(Sender: TObject);
begin
  { все неглавные формы я создаю в обработчике FormCreate, таким образом в
    большинстве обработчиков главной формы Tf_main эти формы будут доступны уже
    проинициализированными. Самы переменные этих форм, в с соответвии с соглашением
    Delphi и Lazarus по умолчанию находятся в публичной секции своих модулей }
  Application.CreateForm(Tf_your_difficulty_level, f_your_difficulty_level);
  Application.CreateForm(Tf_start_new, f_start_new);
  Application.CreateForm(Tf_records, f_records);
  Application.CreateForm(Tf_settings, f_settings);
  Application.CreateForm(Tf_about_development_env, f_about_development_env);
  Application.CreateForm(Tf_about_program, f_about_program);
  { объект T_Mine_Sweeper по умолчанию nil }
  self.mine_sweeper:=nil;
  { создадим игровой объект с параметрами из формы f_start_new }
  self.mine_sweeper := T_Mine_Sweeper.Create(
                                      self,   // передадим объект игровой формы
                                      self.dg_game, // передадим объект игрового грида
                                      self.lb_game_mines,
                                      self.lb_game_flags,
                                      self.lb_game_time,
                                      self.lb_game_state, // передадим объект лэйбл игрвого состояния
                                      @self.On_Mine_Sweeper_State_Change,
                                      'asset_pack_blue', // передадим имя эссет-пака
                                      self.get_form_current_tile_size, // передадим размер тайла
                                      f_start_new.diff_field_height, // передадим высоту поля в тайлах
                                      f_start_new.diff_field_width, // передадим ширину поля в тайлах
                                      f_start_new.diff_field_N_mines // передадим количество мин на поле
                                    ); // создадим объект T_Mine_Sweeper с параметрами, которые
                                       // выбрал пользователь в окне старта игры f_start_new
  self.mine_sweeper.start_game; // и сразу же стартуем игру с новыми настройками
  self.FormResize(self); //вызовем ОН-ресайз формы, там позиционируется игровой грид
end;

procedure Tf_main.FormResize(Sender: TObject);
begin
  if (self.mine_sweeper <> nil) then
    begin
     self.mine_sweeper.resize_game_grid( self.get_form_current_tile_size );
    end;

  { отцентрируем игровой дро-грид }
  self.dg_game.Left:=(self.pn_game.Width div 2)-(self.dg_game.Width div 2);
  self.dg_game.Top:=(self.pn_game.Height div 2)-(self.dg_game.Height div 2);

  self.Repaint;
end;

procedure Tf_main.mi_clear_recordsClick(Sender: TObject);
var
  Reply, BoxStyle: Integer;
begin
  BoxStyle := MB_ICONQUESTION + MB_YESNO;
  Reply := Application.MessageBox('Вы действительно хотите очистить список рекордов?', 'Очищиние списка рекордов.', BoxStyle);
  if Reply=IDYES then
    f_records.records_manager.clear_all_records;
end;

procedure Tf_main.mi_aboutClick(Sender: TObject);
begin
  f_about_program.ShowModal;
end;

procedure Tf_main.mi_about_lazarusClick(Sender: TObject);
begin
  f_about_development_env.ShowModal;
end;

procedure Tf_main.mi_exitClick(Sender: TObject);
begin
  self.Close;
end;

procedure Tf_main.mi_recordsClick(Sender: TObject);
begin
  f_records.ShowModal;
end;

procedure Tf_main.mi_settingsClick(Sender: TObject);
begin
  f_settings.ShowModal;
end;

function Tf_main.get_form_current_tile_size : integer;
var
  potential_size_h, potential_size_w, min_tile_size, tile_size: integer;
begin
  { Определим размер тайлов в зависимости от текущего размера формы и игровой панели }
  potential_size_h:= self.pn_game.Height div f_start_new.diff_field_height;
  potential_size_w:= self.pn_game.Width div f_start_new.diff_field_width;
  min_tile_size:=Min(potential_size_h,potential_size_w);
  tile_size:=min_tile_size;
  Result:=tile_size;
end;


procedure Tf_main.mi_start_gameClick(Sender: TObject);
begin
  f_start_new.ShowModal;
  if ( f_start_new.ModalResult=mrOK ) then
    begin
      { Если игровой объект уже проиницализован - то уничтожим его и очистим ссылку  }
      if ( self.mine_sweeper<>nil ) then
         FreeAndNil(self.mine_sweeper);
      { Создадим игровой объект с актуальными параметрами }
      self.mine_sweeper := T_Mine_Sweeper.Create(
                                          self,   // передадим объект игровой формы
                                          self.dg_game, // передадим объект игрового грида
                                          self.lb_game_mines,
                                          self.lb_game_flags,
                                          self.lb_game_time,
                                          self.lb_game_state, // передадим объект лэйбл игрвого состояния
                                          @self.On_Mine_Sweeper_State_Change,
                                          'asset_pack_blue', // передадим имя эссет-пака
                                          self.get_form_current_tile_size, // передадим размер тайла
                                          f_start_new.diff_field_height, // передадим высоту поля в тайлах
                                          f_start_new.diff_field_width, // передадим ширину поля в тайлах
                                          f_start_new.diff_field_N_mines // передадим количество мин на поле
                                        ); // создадим объект T_Mine_Sweeper с параметрами, которые
                                           // выбрал пользователь в окне старта игры f_start_new
      self.mine_sweeper.start_game; // и сразу же стартуем игру с новыми настройками
      self.FormResize(self); //вызовем ОН-ресайз формы, там позиционируется игровой грид
    end;
end;



procedure Tf_main.On_Mine_Sweeper_State_Change(new_state: TGame_State);
var
  raw_str: string;
  itog_str: string[50];
  tmp_rec : T_Gamer_Record;
begin

  { если условие ниже истинно - это значит игрок выиграл и достиг рекорда }
  if  (new_state=GS_WIN) and ( f_records.records_manager.this_value_is_a_record( f_start_new.diff_level_code,
                                                                                 self.mine_sweeper.get_game_duration_in_seconds )
                             ) then
     begin
       { Выведем input box для ввода имени для регистрации в таблице рекордов }
       raw_str:= InputBox('Ввод имени для регистрации рекорда', 'Ваш результат является рекордом, введите никнейм', ' ');
       { Длинна никнейма не более 50 символов }
       itog_str:=Copy ( raw_str, 1 , Min(Length(raw_str),50) ) ;
       { Теперь заполним переменную-запись с описанием рекорда, данные берём в разных местах,
         но они между собой должны быть констистентны из-за структуры интерфейса базирующегося на формах }
       tmp_rec.level:=f_start_new.diff_level_code;
       tmp_rec.gametime_seconds:=self.mine_sweeper.get_game_duration_in_seconds;
       tmp_rec.nickname:=itog_str;
       tmp_rec.field_height:=f_start_new.diff_field_height;
       tmp_rec.field_width:=f_start_new.diff_field_width;
       tmp_rec.field_N_mines:=f_start_new.diff_field_N_mines;
       { перечитаем все рекорды из файла, чтобы иметь их актуальное представление в ОЗУ }
       f_records.records_manager.f_read_all_records;
       { зарегистрируем рекорд в таблице рекордов }
       f_records.records_manager.register_record(tmp_rec);
       { запишем таблицу рекордов в файл }
       f_records.records_manager.f_write_all_records;
       { отобразим форму с таблицей рекордов }
       f_records.ShowModal;
     end;





end;



end.

