unit ilMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  Forms, Controls, Graphics, Dialogs, ComCtrls, ActnList,
  ExtCtrls, StdCtrls, FileUtil, LazFileUtils, Buttons, Clipbrd, Menus,
  IconThumbnails, IconViewer,
  ilUtils, ilMetadata, ilSettings;

type

  { TMainForm }

  TMainForm = class(TForm)
    acAddFolder: TAction;
    acEditMetadata: TAction;
    acExit: TAction;
    acDeleteIcon: TAction;
    acCopyToClipboard: TAction;
    acSettings: TAction;
    acWriteMetadata: TAction;
    ActionList: TActionList;
    Images: TImageList;
    IconFoldersDropdownMenu: TPopupMenu;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure acAddFolderExecute(Sender: TObject);
    procedure acCopyToClipboardExecute(Sender: TObject);
    procedure acCopyToClipboardUpdate(Sender: TObject);
    procedure acDeleteIconExecute(Sender: TObject);
    procedure acDeleteIconUpdate(Sender: TObject);
    procedure acEditMetadataExecute(Sender: TObject);
    procedure acEditMetadataUpdate(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acSettingsExecute(Sender: TObject);
    procedure acWriteMetadataExecute(Sender: TObject);
    procedure acWriteMetadataUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IconFoldersDropdownMenuPopup(Sender: TObject);
  private
    FViewer: TIconViewerFrame;
    procedure EditMetadata(AIcon: TIconItem);
    procedure IconViewerDblClickHandler(Sender: TObject);
    procedure IconViewerFilterHandler(Sender: TObject);

    procedure ReadIni;
    procedure WriteIni;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  FPImage, FPReadPNG, FPReadBMP;

const
  METADATA_FILENAME = 'metadata.txt';
  APP_CAPTION = 'Icon library';
  APP_CAPTION_MASK = 'Icon library (%d icons out of %d)';

{ TMainForm }

procedure TMainForm.acAddFolderExecute(Sender: TObject);
var
  folder: String;
begin
  if SelectDirectoryDialog.Execute then
  begin
    Screen.Cursor := crHourglass;
    try
      folder := AppendPathDelim(SelectDirectoryDialog.FileName);
      FViewer.AddIconFolder(folder);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainForm.acCopyToClipboardExecute(Sender: TObject);
begin
  if FViewer.SelectedIcon <> nil then
    Clipboard.Assign(FViewer.SelectedIcon.Picture);
end;

procedure TMainForm.acCopyToClipboardUpdate(Sender: TObject);
begin
  acCopyToClipboard.Enabled := (FViewer.SelectedIcon <> nil);
end;

procedure TMainForm.acDeleteIconExecute(Sender: TObject);
begin
  FViewer.DeleteSelectedIcon;
end;

procedure TMainForm.acDeleteIconUpdate(Sender: TObject);
begin
  acDeleteIcon.Enabled := (FViewer.SelectedIcon <> nil);
end;

procedure TMainForm.acEditMetadataExecute(Sender: TObject);
begin
  EditMetaData(FViewer.SelectedIcon);
end;

procedure TMainForm.acEditMetadataUpdate(Sender: TObject);
begin
  acEditMetadata.Enabled := (FViewer.SelectedIcon <> nil);
end;

procedure TMainForm.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.acSettingsExecute(Sender: TObject);
var
  F: TSettingsForm;
  list: TStrings;
begin
  list := TStringList.Create;
  F := TSettingsForm.Create(self);
  try
    F.Position := poMainFormCenter;
    FViewer.IconViewer.WriteIconFolders(list);
    F.SetIconFolders(list);
    if F.ShowModal = mrOK then
    begin
      Screen.Cursor := crHourglass;
      try
        F.GetIconFolders(list);
        FViewer.ReadIconFolders(list);
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure TMainForm.acWriteMetadataExecute(Sender: TObject);
begin
  FViewer.IconViewer.WriteMetadataFiles;
end;

procedure TMainForm.acWriteMetadataUpdate(Sender: TObject);
begin
  acWriteMetadata.Enabled := FViewer.TotalCount > 0;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WriteIni;
end;

{ Opens the metadata editor for specifying the keywords and the style of the
  currently selected icon. The data are copied to all icons sharing the same
  name base. }
procedure TMainForm.EditMetadata(AIcon: TIconItem);
var
  F: TMetadataForm;
begin
  if AIcon = nil then
    exit;

  F := TMetadataForm.Create(nil);
  try
    F.Position := poMainFormCenter;
    F.MetaDataToControls(AIcon);
    if F.ShowModal = mrOK then
    begin
      F.ControlsToMetaData(AIcon);
      FViewer.CopyMetadataToNameBase(AIcon);
    end;
  finally
    F.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FViewer := TIconViewerFrame.Create(self);
  FViewer.Align := alClient;
  FViewer.Parent := self;
  FViewer.OnIconDblClick := @IconViewerDblClickHandler;
  FViewer.OnFilter := @IconViewerFilterHandler;

  ReadIni;
end;

procedure TMainForm.IconFoldersDropdownMenuPopup(Sender: TObject);
begin
  FViewer.IconViewer.PopulateIconFoldersMenu(IconFoldersDropdownMenu);
end;

procedure TMainForm.IconViewerDblClickHandler(Sender: TObject);
begin
  EditMetadata(FViewer.SelectedIcon);
end;

procedure TMainForm.IconViewerFilterHandler(Sender: TObject);
begin
  if FViewer.TotalCount = 0 then
    Caption := APP_CAPTION
  else
    Caption := Format(APP_CAPTION_MASK, [FViewer.FilteredCount, FViewer.TotalCount]);
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
  L, T, W, H: Integer;
  R: TRect;
  list: TStrings;
  i: Integer;
  s: String;
begin
  ini := CreateIniFile;
  try
    L := ini.ReadInteger('MainForm', 'Left', Left);
    T := ini.ReadInteger('MainForm', 'Top', Top);
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    R := Monitor.WorkAreaRect;
    if W > R.Width then W := R.Width;
    if H > R.Height then H := R.Height;
    if L + W > R.Right then L := R.Right - W;
    if T + H > R.Bottom then T := R.Bottom - H;
    if L < R.Left then L := R.Left;
    if T < R.Top then T := R.Top;
    SetBounds(L, T, W, H);

    FViewer.IconViewer.LockFilter;
    try
      list := TStringList.Create;
      try
        ini.ReadSection('IconFolders', list);
        for i := 0 to list.Count-1 do
          list[i] := ini.ReadString('IconFolders', list[i], '');
        FViewer.IconViewer.ReadIconFolders(list);

        list.Clear;
        ini.ReadSection('KeywordsHistory', list);
        for i := 0 to list.Count-1 do
          list[i] := ini.ReadString('KeywordsHistory', list[i], '');
        for i := list.Count-1 downto 0 do
          if list[i] = '' then list.Delete(i);
        FViewer.cmbFilterByKeywords.Items.Assign(list);
      finally
        list.Free;
      end;

      i := ini.ReadInteger('Filter', 'FilterBySize', 0);
      FViewer.UpdateIconSizes(i);

      i := ini.ReadInteger('Filter', 'FilterByStyle', 0);
      FViewer.UpdateIconStyles(i);
    finally
      FViewer.IconViewer.UnlockFilter;
    end;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;
  L, T, W, H: Integer;
  R: TRect;
  i: Integer;
  list: TStrings;
begin
  ini := CreateIniFile;
  try
    if WindowState = wsNormal then
    begin
      ini.WriteInteger('MainForm', 'Left', Left);
      ini.WriteInteger('MainForm', 'Top', Top);
      ini.WriteInteger('MainForm', 'Width', Width);
      ini.WriteInteger('MainForm', 'Height', Height);
    end;

    list := TStringList.Create;
    try
      FViewer.IconViewer.WriteIconFolders(list);
      for i := 0 to list.Count-1 do
        ini.WriteString('IconFolders', 'Folder' + IntToStr(i), list[i]);

      list.Clear;
      list.Assign(FViewer.cmbFilterByKeywords.Items);
      for i := 0 to list.Count-1 do
        ini.WriteString('KeywordsHistory', 'Item' + IntToStr(i), list[i]);
    finally
      list.Free;
    end;

    ini.WriteInteger('Filter', 'FilterBySize', FViewer.cmbFilterBySize.ItemIndex);
    ini.WriteInteger('Filter', 'FilterByStyle', FViewer.cmbFilterByStyle.ItemIndex);
  finally
    ini.Free;
  end;
end;

end.

