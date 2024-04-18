unit idbSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
  Spin, EditBtn,
  idbGlobal;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    ButtonPanel: TButtonPanel;
    edDatabaseFolder: TDirectoryEdit;
    gbDBGrid: TGroupBox;
    gbDatabase: TGroupBox;
    Label1: TLabel;
    lblDatabaseFolder: TLabel;
    seGridRowHeights: TSpinEdit;
  private

  public
    procedure ControlsToSettings;
    procedure SettingsToControls;

  end;

var
  SettingsForm: TSettingsForm;

implementation

{$R *.lfm}

procedure TSettingsForm.ControlsToSettings;
begin
  if edDatabaseFolder.Text = Application.Location + 'data' then
    Settings.DatabaseFolder := ''
  else
    Settings.DatabaseFolder := edDatabaseFolder.Text;
  Settings.RowLines := seGridRowHeights.Value;
end;

procedure TSettingsForm.SettingsToControls;
begin
  if Settings.DatabaseFolder = '' then
    edDatabaseFolder.Text := Application.Location + 'data'
  else
    edDatabaseFolder.Text := Settings.DatabaseFolder;
  seGridRowHeights.Value := Settings.RowLines;
end;

end.

