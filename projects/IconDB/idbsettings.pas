unit idbSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
  Spin, EditBtn, Buttons,
  idbGlobal, idbDatamodule;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    btnWriteMetadata: TBitBtn;
    ButtonPanel: TButtonPanel;
    rbVariableThumbnailSize: TRadioButton;
    rbFixedThumbnailSize: TRadioButton;
    edDatabaseFolder: TDirectoryEdit;
    gbDBGrid: TGroupBox;
    gbDatabase: TGroupBox;
    gbThumbnails: TGroupBox;
    Label1: TLabel;
    lblMultiply: TLabel;
    lblBorder: TLabel;
    lblDatabaseFolder: TLabel;
    seGridRowHeights: TSpinEdit;
    seFixedThumbnailWidth: TSpinEdit;
    seFixedThumbnailHeight: TSpinEdit;
    seThumbnailBorder: TSpinEdit;
    procedure btnWriteMetadataClick(Sender: TObject);
  private

  public
    procedure ControlsToSettings;
    procedure SettingsToControls;

  end;

var
  SettingsForm: TSettingsForm;

implementation

{$R *.lfm}

procedure TSettingsForm.btnWriteMetadataClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  try
    MainDatamodule.WriteMetaDataFiles(false, true);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TSettingsForm.ControlsToSettings;
begin
  if edDatabaseFolder.Text = Application.Location + 'data' then
    Settings.DatabaseFolder := ''
  else
    Settings.DatabaseFolder := edDatabaseFolder.Text;
  Settings.RowLines := seGridRowHeights.Value;
  Settings.FixedThumbnailSize := rbFixedThumbnailSize.Checked;
  Settings.ThumbnailWidth := seFixedThumbnailWidth.Value;
  Settings.ThumbnailHeight := seFixedThumbnailHeight.Value;
  Settings.ThumbnailBorder := seThumbnailBorder.Value;
end;

procedure TSettingsForm.SettingsToControls;
begin
  if Settings.DatabaseFolder = '' then
    edDatabaseFolder.Text := Application.Location + 'data'
  else
    edDatabaseFolder.Text := Settings.DatabaseFolder;
  seGridRowHeights.Value := Settings.RowLines;
  if Settings.FixedThumbnailSize then
    rbFixedThumbnailSize.Checked := true
  else
    rbVariableThumbnailSize.Checked := true;
  seFixedThumbnailWidth.Value := Settings.ThumbnailWidth;
  seFixedThumbnailHeight.Value := Settings.ThumbnailHeight;
  seThumbnailBorder.Value := Settings.ThumbnailBorder;
end;

end.

