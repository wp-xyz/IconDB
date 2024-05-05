unit ilSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
  CheckLst, ExtCtrls,
  ilGlobal;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    btnDeleteFolder: TButton;
    btnMoveFolderUp: TButton;
    btnMoveFolderDown: TButton;
    ButtonPanel1: TButtonPanel;
    clbFolders: TCheckListBox;
    FolderPanel: TGroupBox;
    procedure btnDeleteFolderClick(Sender: TObject);
    procedure btnMoveFolderDownClick(Sender: TObject);
    procedure btnMoveFolderUpClick(Sender: TObject);
    procedure clbFoldersSelectionChange(Sender: TObject; User: boolean);
  private

  public
    procedure GetIconFolders(AList: TStrings);
    procedure SetIconFolders(AList: TStrings);

  end;

var
  SettingsForm: TSettingsForm;

implementation

{$R *.lfm}

procedure TSettingsForm.btnDeleteFolderClick(Sender: TObject);
var
  res: TModalResult;
begin
  if clbFolders.ItemIndex > -1 then
  begin
    res := MessageDlg('Do you really want to remove this folder from the icon library?',
      mtConfirmation, [mbYes, mbNo], 0);
    if res = mrYes then
      clbFolders.Items.Delete(clbFolders.ItemIndex);
  end;
end;

procedure TSettingsForm.btnMoveFolderDownClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := clbFolders.ItemIndex;
  if (idx > -1) and (idx < clbFolders.Items.Count-1) then
  begin
    clbFolders.Items.Move(idx, idx+1);
    clbFolders.ItemIndex := idx+1;
  end;
end;

procedure TSettingsForm.btnMoveFolderUpClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := clbFolders.ItemIndex;
  if idx > 0 then
  begin
    clbFolders.Items.Move(idx, idx-1);
    clbFolders.ItemIndex := idx-1;
  end;
end;

procedure TSettingsForm.clbFoldersSelectionChange(Sender: TObject;
  User: boolean);
var
  idx: Integer;
begin
  idx := clbFolders.ItemIndex;
  btnDeleteFolder.Enabled := idx > -1;
  btnMoveFolderUp.Enabled := idx > 0;
  btnMoveFolderDown.Enabled := (idx > -1) and (idx < clbFolders.Items.Count-1);
end;

{ Copies the listbox entries to the given list which will be passed on to the
  IconViewer's IconList by the caller. }
procedure TSettingsForm.GetIconFolders(AList: TStrings);
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
        folder := HIDDEN_FOLDER_FLAG + folder;
      AList.Add(folder);
    end;
  finally
    AList.EndUpdate;
  end;
end;

procedure TSettingsForm.SetIconFolders(AList: TStrings);
var
  i: Integer;
  folder: String;
  isHidden: Boolean;
begin
  clbFolders.Items.BeginUpdate;
  try
    clbFolders.Items.Clear;
    for i := 0 to AList.Count-1 do
    begin
      folder := AList[i];
      isHidden := folder[1] = HIDDEN_FOLDER_FLAG;
      if isHidden then System.Delete(folder, 1, 1);
      clbFolders.Items.Add(folder);
      clbFolders.Checked[i] := not isHidden;
    end;
  finally
    clbFolders.Items.EndUpdate;
  end;
end;

end.

