unit idbSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
  Spin,
  idbGlobal;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    ButtonPanel: TButtonPanel;
    gbDBGrid: TGroupBox;
    Label1: TLabel;
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
  Settings.RowHeight := seGridRowHeights.Value;
end;

procedure TSettingsForm.SettingsToControls;
begin
  seGridRowHeights.Value := Settings.RowHeight;
end;

end.

