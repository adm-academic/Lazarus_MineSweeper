unit uf_settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  u_settings_manager;

type

  { Tf_settings }

  Tf_settings = class(TForm)
    bt_save_all: TButton;
    cb_play_music: TCheckBox;
    cb_select_asset_pack: TComboBox;
    cb_play_sounds: TCheckBox;
    lb_music_volume: TLabel;
    lb_sounds_volume: TLabel;
    lb_select_asset_pack: TLabel;
    tb_music_volume: TTrackBar;
    tb_sounds_volume: TTrackBar;
    procedure bt_save_allClick(Sender: TObject);
    procedure cb_play_musicChange(Sender: TObject);
    procedure cb_play_soundsChange(Sender: TObject);
    procedure cb_select_asset_packChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tb_music_volumeChange(Sender: TObject);
    procedure tb_sounds_volumeChange(Sender: TObject);
  private

  public
    settings_manager : T_Settings_Manager;
  end;

var
  f_settings: Tf_settings;

implementation

{$R *.lfm}

{ Tf_settings }

procedure Tf_settings.FormCreate(Sender: TObject);// при создании формы
begin
  self.settings_manager:=T_Settings_Manager.Create;
end;

procedure Tf_settings.cb_select_asset_packChange(Sender: TObject); // при изменении значения в комбо-боксе
begin
  self.settings_manager.set_asset_pack_name(self.cb_select_asset_pack.Text);
end;

procedure Tf_settings.bt_save_allClick(Sender: TObject);  // при нажатии на кнопку "Сохранить Всё"
begin
  { сохраняем по-порядочку из контролов на форме }
  self.settings_manager.set_asset_pack_name(self.cb_select_asset_pack.Text);
  self.settings_manager.set_music_state_on(self.cb_play_music.Checked);
  self.settings_manager.set_sounds_state_on(self.cb_play_sounds.Checked);
  self.settings_manager.set_sounds_volume(self.tb_sounds_volume.Position);
  self.settings_manager.set_music_volume(self.tb_music_volume.Position);
end;

procedure Tf_settings.cb_play_musicChange(Sender: TObject);
begin
  self.settings_manager.set_music_state_on(self.cb_play_music.Checked);
end;

procedure Tf_settings.cb_play_soundsChange(Sender: TObject);
begin
 self.settings_manager.set_sounds_state_on(self.cb_play_sounds.Checked);
end;

procedure Tf_settings.FormDestroy(Sender: TObject); // при уничтожении формы
begin
 FreeAndNil(self.settings_manager);
end;

procedure Tf_settings.FormShow(Sender: TObject); // при показе формы
var
  i: integer;
  apn : string;
begin
  { заполняем комбо-бокс с ассет паками }
  self.cb_select_asset_pack.Items.SetStrings(self.settings_manager.get_asset_packs_list);
  apn :=  self.settings_manager.get_asset_pack_name;
  i:= self.cb_select_asset_pack.Items.IndexOf( apn );
  self.cb_select_asset_pack.ItemIndex:=i;
  { заполняем чек-боксы }
  self.cb_play_music.Checked:=self.settings_manager.get_music_state_on;
  self.cb_play_sounds.Checked:=self.settings_manager.get_sounds_state_on;
  { заполняем трак-бары }
  self.tb_sounds_volume.Position:=self.settings_manager.get_sounds_volume;
  self.tb_music_volume.Position:=self.settings_manager.get_music_volume;
end;

procedure Tf_settings.tb_music_volumeChange(Sender: TObject);
begin
  self.settings_manager.set_music_volume(self.tb_music_volume.Position);
end;

procedure Tf_settings.tb_sounds_volumeChange(Sender: TObject);
begin
  self.settings_manager.set_sounds_volume(self.tb_sounds_volume.Position);
end;

end.

