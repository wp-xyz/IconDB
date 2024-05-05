unit ilMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  Forms, Controls, Graphics, Dialogs, ComCtrls, ActnList,
  ExtCtrls, StdCtrls, FileUtil, LazFileUtils, FileCtrl, Buttons, Clipbrd, Menus,
  IconThumbnails, IconKeywordFilterEditor,
  ilGlobal, ilUtils, ilMetadata, ilSettings;

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
    Bevel1: TBevel;
    Bevel2: TBevel;
    cmbFilterBySize: TComboBox;
    cmbFilterByStyle: TComboBox;
    cmbFilterByKeywords: TComboBox;
    Images: TImageList;
    infoFileName: TLabel;
    infoKeywords: TLabel;
    infoSize: TLabel;
    infoStyle: TLabel;
    lblFileName: TLabel;
    lblKeywords: TLabel;
    lblSize: TLabel;
    lblStyle: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    IconFoldersDropdownMenu: TPopupMenu;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    btnKeywordEditor: TSpeedButton;
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
    procedure btnKeywordEditorClick(Sender: TObject);
    procedure cmbFilterByKeywordsChange(Sender: TObject);
    procedure cmbFilterBySizeChange(Sender: TObject);
    procedure cmbFilterByStyleChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IconFoldersDropdownMenuPopup(Sender: TObject);
  private
    FActivated: Boolean;
    FIconViewer: TIconViewer;
    procedure EditMetadata(AIcon: TIconItem);
    procedure IconViewerDblClickHandler(Sender: TObject);
    procedure IconViewerSelectHandler(Sender: TObject);
    procedure UpdateCmds;
    procedure UpdateIconCount;
    procedure UpdateIconDetails;
    procedure UpdateIconSizes(ASizeIndex: Integer);
    procedure UpdateIconStyles(AStyleIndex: Integer);

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
  filterByStyle: Integer;
  filterBySize: Integer;
begin
  filterByStyle := cmbFilterByStyle.ItemIndex;
  if filterByStyle = -1 then filterByStyle := 0;
  filterBySize := cmbFilterBySize.ItemIndex;
  if filterBySize = -1 then filterBySize := 0;

  if SelectDirectoryDialog.Execute then
  begin
    Screen.Cursor := crHourglass;
    try
      folder := AppendPathDelim(SelectDirectoryDialog.FileName);
      FIconViewer.AddIconFolder(folder);
      FIconViewer.Invalidate;
      UpdateIconSizes(filterBySize);
      UpdateIconStyles(filterByStyle);
      UpdateIconCount;
      UpdateIconDetails;
      UpdateCmds;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainForm.acCopyToClipboardExecute(Sender: TObject);
begin
  if FIconViewer.SelectedIcon <> nil then
    Clipboard.Assign(FIconViewer.SelectedIcon.Picture);
end;

procedure TMainForm.acCopyToClipboardUpdate(Sender: TObject);
begin
  acCopyToClipboard.Enabled := (FIconViewer.SelectedIcon <> nil);
end;

procedure TMainForm.acDeleteIconExecute(Sender: TObject);
var
  res: TModalResult;
begin
  res := MessageDlg('Do you really want to delete the selected icon from the library?',
    mtConfirmation, [mbYes, mbNo], 0);
  if res = mrYes then
  begin
    FIconViewer.DeleteIcon(FIconViewer.SelectedIcon);
    UpdateIconCount;
    UpdateIconDetails;
  end;
end;

procedure TMainForm.acDeleteIconUpdate(Sender: TObject);
begin
  acDeleteIcon.Enabled := (FIconViewer.SelectedIcon <> nil);
end;

procedure TMainForm.acEditMetadataExecute(Sender: TObject);
begin
  EditMetaData(FIconViewer.SelectedIcon);
end;

procedure TMainForm.acEditMetadataUpdate(Sender: TObject);
begin
  acEditMetadata.Enabled := (FIconViewer.SelectedIcon <> nil);
end;

procedure TMainForm.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.acSettingsExecute(Sender: TObject);
var
  F: TSettingsForm;
  list: TStrings;
  sizeFilter, styleFilter: Integer;
begin
  styleFilter := cmbFilterByStyle.ItemIndex;
  if styleFilter = -1 then styleFilter := 0;
  sizeFilter := cmbFilterBySize.ItemIndex;
  if sizeFilter = -1 then sizeFilter := 0;

  list := TStringList.Create;
  F := TSettingsForm.Create(self);
  try
    F.Position := poMainFormCenter;
    FIconViewer.WriteIconFolders(list);
    F.SetIconFolders(list);
    if F.ShowModal = mrOK then
    begin
      Screen.Cursor := crHourglass;
      FIconViewer.LockFilter;
      try
        F.GetIconFolders(list);
        FIconViewer.ReadIconFolders(list);
        UpdateIconSizes(sizeFilter);
        UpdateIconStyles(stylefilter);
      finally
        FIconViewer.UnlockFilter;
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure TMainForm.acWriteMetadataExecute(Sender: TObject);
begin
  FIconViewer.WriteMetadataFiles;
end;

procedure TMainForm.acWriteMetadataUpdate(Sender: TObject);
begin
  acWriteMetadata.Enabled := FIconViewer.IconCount > 0;
end;

procedure TMainForm.btnKeywordEditorClick(Sender: TObject);
var
  F: TKeywordFilterEditorForm;
  L: TStringList;
begin
  F := TKeywordFilterEditorForm.Create(Self);
  L := TStringList.Create;
  try
    F.Position := poOwnerFormCenter;
    F.Filter := cmbFilterByKeywords.Text;
    FIconViewer.GetKeywordsAsStrings(L);
    F.Keywords := L;
    if F.ShowModal = mrOK then
    begin
      cmbFilterByKeywords.Text := F.Filter;
      FIconViewer.FilterByIconKeywords := F.Filter;
      FIconViewer.Invalidate;
    end;
  finally
    L.Free;
    F.Free;
  end;
end;

procedure TMainForm.cmbFilterByKeywordsChange(Sender: TObject);
begin
  FIconViewer.FilterByIconKeywords := cmbFilterByKeywords.Text;
  FIconViewer.Invalidate;
  UpdateIconCount;
end;

procedure TMainForm.cmbFilterBySizeChange(Sender: TObject);
begin
  if cmbFilterBySize.ItemIndex = 0 then    // Filter by any size
    FIconViewer.FilterByIconSize := ''
  else
    FIconViewer.FilterByIconSize := cmbFilterBySize.Items[cmbFilterBySize.ItemIndex];
  FIconViewer.Invalidate;
  UpdateIconCount;
end;

procedure TMainForm.cmbFilterByStyleChange(Sender: TObject);
begin
  FIconViewer.FilterByIconStyle := TIconStyle(cmbFilterByStyle.ItemIndex);
  FIconViewer.Invalidate;
  UpdateIconCount;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WriteIni;
end;

procedure TMainForm.IconFoldersDropdownMenuPopup(Sender: TObject);
begin
  FIconViewer.PopulateIconFoldersMenu(IconFoldersDropdownMenu);
end;

procedure TMainForm.FormActivate(Sender: TObject);
var
  w: Integer;
begin
  if not FActivated then
  begin
    FActivated := true;
    w := lblFileName.Width;
    if w < lblSize.Width then w := lblSize.Width;
    if w < lblKeywords.Width then w := lblKeywords.Width;
    infoFileName.BorderSpacing.Left := w + 8;
    infoSize.BorderSpacing.Left := infoFileName.BorderSpacing.Left;
    infoKeywords.BorderSpacing.Left := infoFileName.BorderSpacing.Left;
  end;
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
      // Copy these metadata to all icons having the same namebase.
      FIconViewer.CopyMetadataToNameBase(AIcon);
      UpdateIconDetails;
      UpdateIconCount;
    end;
  finally
    F.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FIconViewer := TIconViewer.Create(self);
  FIconViewer.Align := alClient;
  FIconViewer.Parent := self;
  FIconViewer.OnDblClick := @IconViewerDblClickHandler;
  FIconViewer.OnSelect := @IconViewerSelectHandler;

  ReadIni;

  UpdateIconDetails;
  UpdateIconCount;
end;

procedure TMainForm.IconViewerDblClickHandler(Sender: TObject);
begin
  EditMetadata(FIconViewer.SelectedIcon);
end;

procedure TMainForm.IconViewerSelectHandler(Sender: TObject);
begin
  UpdateIconDetails;
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
  L, T, W, H: Integer;
  R: TRect;
  list: TStrings;
  i: Integer;
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

    FIconViewer.LockFilter;
    try
      list := TStringList.Create;
      try
        ini.ReadSection('IconFolders', list);
        for i := 0 to list.Count-1 do
          list[i] := ini.ReadString('IconFolders', list[i], '');
        FIconViewer.ReadIconFolders(list);
      finally
        list.Free;
      end;

      i := ini.ReadInteger('Filter', 'FilterBySize', 0);
      UpdateIconSizes(i);

      i := ini.ReadInteger('Filter', 'FilterByStyle', 0);
      UpdateIconStyles(i);
    finally
      FIconViewer.UnlockFilter;
    end;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.UpdateCmds;
begin
  btnKeywordEditor.Enabled := FIconViewer.IconCount > 0;
end;

procedure TMainForm.UpdateIconCount;
begin
  if FIconViewer.ThumbnailCount = 0 then
    Caption := APP_CAPTION
  else
    Caption := Format(APP_CAPTION_MASK, [FIconViewer.ThumbnailCount, FIconViewer.IconCount]);
end;

procedure TMainForm.UpdateIconDetails;
var
  keywordList: TStrings;
begin
  if FIconViewer.SelectedIcon <> nil then
  begin
    infoFileName.Hint := FIconViewer.SelectedIcon.FileName;
    infoFileName.Caption := MinimizeName(infoFileName.Hint, Canvas, infoFileName.Width - infoFileName.BorderSpacing.Right);
    infoSize.Caption := FIconViewer.SelectedIcon.SizeAsString;
    infoStyle.Caption := FIconViewer.SelectedIcon.StyleAsString;
    keywordList := TStringList.Create;
    try
      FIconViewer.SelectedIcon.ExportKeywordsToStrings(keywordList);
      keywordList.Delimiter := ';';
      keywordList.StrictDelimiter := true;
      infoKeywords.Caption := StringReplace(keywordList.DelimitedText, ';', '; ', [rfReplaceAll]);
    finally
      keywordList.Free;
    end;
  end else
  begin
    infoFileName.Caption := '';
    infoFileName.Hint := '';
    infoStyle.Caption := '';
    infoSize.Caption := '';
    infoKeywords.Caption := '';
  end;
end;

procedure TMainForm.UpdateIconSizes(ASizeIndex: Integer);
begin
  FIconViewer.GetIconSizesAsStrings(cmbFilterBySize.Items);
  cmbFilterBySize.Items.Insert(0, '(any size)');
  if ASizeIndex < 0 then ASizeIndex := 0;
  cmbFilterBySize.ItemIndex := ASizeIndex;
  if ASizeIndex = 0 then
    FIconViewer.FilterByIconSize := ''
  else
    FIconViewer.FilterByIconSize := cmbFilterBySize.Items[ASizeIndex];
end;

procedure TMainForm.UpdateIconStyles(AStyleIndex: Integer);
begin
  IconStylesToStrings(cmbFilterByStyle.Items);
  if AStyleIndex < 0 then AStyleIndex := 0;
  cmbFilterByStyle.ItemIndex := AStyleIndex;
  if AStyleIndex = 0 then
    FIconViewer.FilterByIconStyle := isAnyStyle
  else
    FIconViewer.FilterByIconStyle := TIconStyle(AStyleIndex);
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
      FIconViewer.WriteIconFolders(list);
      for i := 0 to list.Count-1 do
        ini.WriteString('IconFolders', 'Folder' + IntToStr(i), list[i]);
    finally
      list.Free;
    end;

    ini.WriteInteger('Filter', 'FilterBySize', cmbFilterBySize.ItemIndex);
    ini.WriteInteger('Filter', 'FilterByStyle', cmbFilterByStyle.ItemIndex);
  finally
    ini.Free;
  end;
end;

end.

