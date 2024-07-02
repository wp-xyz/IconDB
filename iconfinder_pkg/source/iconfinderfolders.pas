{
 *****************************************************************************
  This file is part of a Lazarus Package, IconLib.

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.
 *****************************************************************************

 A form for management of the folders from which icons are included in the
 icon library.
}

unit IconFinderFolders;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
  CheckLst, ExtCtrls,
  IconLibStrConsts;

type

  { TIconFolderForm }

  TIconFolderForm = class(TForm)
    CenterBevel: TBevel;
    btnDeleteFolder: TButton;
    btnMoveFolderUp: TButton;
    btnMoveFolderDown: TButton;
    btnAddFolder: TButton;
    ButtonPanel1: TButtonPanel;
    clbFolders: TCheckListBox;
    FolderPanel: TGroupBox;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    procedure btnDeleteFolderClick(Sender: TObject);
    procedure btnMoveFolderDownClick(Sender: TObject);
    procedure btnMoveFolderUpClick(Sender: TObject);
    procedure btnAddFolderClick(Sender: TObject);
    procedure clbFoldersSelectionChange(Sender: TObject; User: boolean);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure GetIconFolders(AList: TStrings);
    procedure SetIconFolders(AList: TStrings);
    procedure UpdateLanguage;

  end;

implementation

{$R *.lfm}

procedure TIconFolderForm.btnDeleteFolderClick(Sender: TObject);
var
  res: TModalResult;
begin
  if clbFolders.ItemIndex > -1 then
  begin
    res := MessageDlg(RSFolders_ConfirmDeleteFolderMsg, mtConfirmation, [mbYes, mbNo], 0);
    if res = mrYes then
      clbFolders.Items.Delete(clbFolders.ItemIndex);
  end;
end;

procedure TIconFolderForm.btnMoveFolderDownClick(Sender: TObject);
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

procedure TIconFolderForm.btnMoveFolderUpClick(Sender: TObject);
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

procedure TIconFolderForm.btnAddFolderClick(Sender: TObject);
var
  idx: Integer;
begin
  if SelectDirectoryDialog.Execute then
  begin
    idx := clbFolders.Items.Add(SelectDirectoryDialog.FileName);
    clbFolders.Checked[idx] := true;
  end;
end;

procedure TIconFolderForm.clbFoldersSelectionChange(Sender: TObject;
  User: boolean);
var
  idx: Integer;
begin
  idx := clbFolders.ItemIndex;
  btnDeleteFolder.Enabled := idx > -1;
  btnMoveFolderUp.Enabled := idx > 0;
  btnMoveFolderDown.Enabled := (idx > -1) and (idx < clbFolders.Items.Count-1);
end;

procedure TIconFolderForm.FormCreate(Sender: TObject);
begin
  UpdateLanguage;
end;

{ Copies the listbox entries to the given list which will be passed on to the
  IconViewer's IconList by the caller.
  Hidden folders have non-nil Objects property in the outpist list. }
procedure TIconFolderForm.GetIconFolders(AList: TStrings);
const
  HIDDEN: Array[boolean] of PtrUInt = (0, 1);
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
      AList.AddObject(folder, TObject(HIDDEN[not clbFolders.Checked[i]]));
    end;
  finally
    AList.EndUpdate;
  end;
end;

{ Copies the provided list entries to the checklistbox items. Entries which
  have a non-zero Objects property are unchecked in the checklistbox. }
procedure TIconFolderForm.SetIconFolders(AList: TStrings);
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
      folder := AList.Strings[i];
      isHidden := AList.Objects[i] <> nil;
      clbFolders.Items.Add(folder);
      clbFolders.Checked[i] := not isHidden;
    end;
  finally
    clbFolders.Items.EndUpdate;
  end;
end;

procedure TIconFolderForm.UpdateLanguage;
begin
  Caption := RSFolders_IconLibFolders;
  FolderPanel.Caption := RSFolders_IconLibFolders;
  btnAddFolder.Caption := RSFolders_Add;
  btnDeleteFolder.Caption := RSFolders_Delete;
  btnMoveFolderUp.Caption := RSFolders_MoveUp;
  btnMoveFolderDown.Caption := RSFolders_MoveDown;
end;

end.

