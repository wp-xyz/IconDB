unit ilSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
  CheckLst;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    clbFolders: TCheckListBox;
    lblIncludedFolders: TLabel;
  private

  public
    procedure IconFoldersFromSettings(AList: TStrings);
    procedure IconFoldersToSettings(AList: TStrings);

  end;

var
  SettingsForm: TSettingsForm;

implementation

{$R *.lfm}

procedure TSettingsForm.IconFoldersFromSettings(AList: TStrings);
var
  i: Integer;
  folder: String;
begin
  AList.BeginUpdate;
  try
    AList.Clear;
    for i := 0 to clbFolders.Items.Count-1 do
    begin
      folder := clbFolders.Items[i];
      if not clbFolders.Checked[i] then
        folder := '-' + folder;
      AList.Add(folder);
    end;
  finally
    AList.EndUpdate;
  end;
end;


procedure TSettingsForm.IconFoldersToSettings(AList: TStrings);
var
  i: Integer;
  folder: String;
  excluded: Boolean;
begin
  clbFolders.Items.BeginUpdate;
  try
    clbFolders.Items.Clear;
    for i := 0 to AList.Count-1 do
    begin
      folder := AList[i];
      excluded := folder[1] = '-';
      if excluded then Delete(folder, 1, 1);
      clbFolders.Items.Add(folder);
      clbFolders.Checked[i] := not excluded;
    end;
  finally
    clbFolders.Items.EndUpdate;
  end;
end;

end.

