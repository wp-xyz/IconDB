unit IconViewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  LazFileUtils,
  Forms, Controls, Graphics, StdCtrls, ExtCtrls, FileCtrl, Buttons, Dialogs, ImgList,
  IconThumbnails, IconKeywordFilterEditor;

type

  { TIconViewerFrame }

  TIconViewerFrame = class(TFrame)
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnKeywordEditor: TSpeedButton;
    cmbFilterByKeywords: TComboBox;
    cmbFilterBySize: TComboBox;
    cmbFilterByStyle: TComboBox;
    FilterPanel: TPanel;
    infoFileName: TLabel;
    infoKeywords: TLabel;
    infoSize: TLabel;
    infoStyle: TLabel;
    lblFileName: TLabel;
    lblKeywords: TLabel;
    lblSize: TLabel;
    lblStyle: TLabel;
    IconDetailsPanel: TPanel;
    btnExecuteFilter: TSpeedButton;
    btnClearFilter: TSpeedButton;
    procedure btnClearFilterClick(Sender: TObject);
    procedure btnExecuteFilterClick(Sender: TObject);
    procedure btnKeywordEditorClick(Sender: TObject);
    procedure cmbFilterByKeywordsEditingDone(Sender: TObject);
    procedure cmbFilterBySizeChange(Sender: TObject);
    procedure cmbFilterByStyleChange(Sender: TObject);
    procedure IconDetailsPanelResize(Sender: TObject);

  private
    FIconViewer: TIconViewer;
    FLayoutFixed: Boolean;
    FOnFilter: TNotifyEvent;
    FOnIconDblClick: TNotifyEvent;
    function GetFilteredCount: Integer;
    function GetImageIndex(AIndex: Integer): TImageIndex;
    function GetImageList: TCustomImageList;
    function GetSelectedIcon: TIconItem;
    function GetTotalCount: Integer;
    procedure SetImageIndex(AIndex: Integer; AValue: TImageIndex);
    procedure SetImageList(AValue: TCustomImageList);

  protected
    procedure AddKeywordFilterToHistory(AFilter: String);
    procedure CreateHandle; override;
    procedure DoIconViewerDblClick(Sender: TObject);
    procedure DoIconViewerFilter(Sender: TObject);
    procedure DoIconViewerSelect(Sender: TObject);
    procedure UpdateCmds;
    procedure UpdateIconDetails;

  public
    constructor Create(AOwner: TComponent); override;
    procedure AddIconFolder(AFolder: String; Hidden: Boolean = false);
    procedure CopyMetadataToNameBase(AIcon: TIconItem);
    procedure DeleteSelectedIcon;
    procedure GetKeywordsHistory(AList: TStrings);
    procedure ReadIconFolders(AList: TStrings);
    procedure SetKeywordsHistory(AList: TStrings);
    procedure UpdateIconSizes(ASizeIndex: Integer);
    procedure UpdateIconStyles(AStyleIndex: Integer);

    property FilteredCount: Integer read GetFilteredCount;
    property IconViewer: TIconViewer read FIconViewer;
    property ImageIndex_ExecuteFilter: TImageIndex index 0 read GetImageIndex write SetImageIndex;
    property ImageIndex_ClearFilter: TImageIndex index 1 read GetImageIndex write SetImageIndex;
    property ImageList: TCustomImageList read GetImageList write SetImageList;
    property SelectedIcon: TIconItem read GetSelectedIcon;
    property TotalCount: Integer read GetTotalCount;
    property OnFilter: TNotifyEvent read FOnFilter write FOnFilter;
    property OnIconDblClick: TNotifyEvent read FOnIconDblClick write FOnIconDblClick;


  end;

implementation

{$R *.lfm}

const
  MAX_KEYWORDS_HISTORY = 20;

{ TIconViewerFrame }

constructor TIconViewerFrame.Create(AOwner: TComponent);
begin
  inherited;
  FIconViewer := TIconViewer.Create(self);
  FIconViewer.Align := alClient;
  FIconViewer.FocusedColor := clWindowText;
  FIconViewer.Parent := self;
  FIconViewer.OnDblClick := @DoIconViewerDblClick;
  FIconViewer.OnFilter := @DoIconViewerFilter;
  FIconViewer.OnSelect := @DoIconViewerSelect;

  cmbFilterBySize.Hint := 'Filter by icon size';
  cmbFilterByStyle.Hint := 'Filter by icon style';
  cmbFilterByKeywords.Hint := 'Expression to filter by keywords';
  btnKeywordEditor.Hint := 'Enter/edit a keyword filter';
  btnClearFilter.Hint := 'Clears the keyword filter';
  btnExecuteFilter.Hint := 'Filters by keywords';

  UpdateIconDetails;
end;

procedure TIconViewerFrame.AddIconFolder(AFolder: String; Hidden: Boolean = false);
var
  filterByStyle: Integer;
  filterBySize: Integer;
begin
  filterByStyle := cmbFilterByStyle.ItemIndex;
  if filterByStyle = -1 then filterByStyle := 0;
  filterBySize := cmbFilterBySize.ItemIndex;
  if filterBySize = -1 then filterBySize := 0;

  AFolder := AppendPathDelim(SwitchPathDelims(AFolder, true));
  IconViewer.AddIconFolder(AFolder, Hidden);

  UpdateIconSizes(filterBySize);
  UpdateIconStyles(filterByStyle);
  UpdateIconDetails;
  UpdateCmds;
end;

procedure TIconViewerFrame.AddKeywordFilterToHistory(AFilter: String);
var
  idx: Integer;
begin
  if AFilter = '' then
    exit;

  idx := cmbFilterByKeywords.Items.IndexOf(AFilter);
  if idx = -1 then
    cmbFilterByKeywords.Items.Insert(0, AFilter)
  else
    cmbFilterByKeywords.Items.Move(idx, 0);

  while cmbFilterByKeywords.Items.Count > MAX_KEYWORDS_HISTORY do
    cmbFilterByKeywords.Items.Delete(cmbFilterByKeywords.Items.Count-1);
end;

procedure TIconViewerFrame.btnKeywordEditorClick(Sender: TObject);
var
  F: TKeywordFilterEditorForm;
  L: TStringList;
  P: TPoint;
begin
  F := TKeywordFilterEditorForm.Create(Self);
  L := TStringList.Create;
  try
    F.Position := poDesigned;
    P := ClientToScreen(Point(cmbFilterByKeywords.Left, cmbFilterByKeywords.Top + cmbFilterByKeywords.Height));
    F.SetBounds(P.X, P.Y, F.Width, F.Height);
    F.Filter := cmbFilterByKeywords.Text;
    FIconViewer.GetKeywordsAsStrings(L);
    F.Keywords := L;
    if F.ShowModal = mrOK then
    begin
      cmbFilterByKeywords.Text := F.Filter;
      FIconViewer.FilterByIconKeywords := F.Filter;
      AddKeywordFilterToHistory(F.Filter);
      //FIconViewer.Invalidate;
    end;
  finally
    L.Free;
    F.Free;
  end;
end;

procedure TIconViewerFrame.btnClearFilterClick(Sender: TObject);
begin
  cmbFilterByKeywords.Text := '';
  FIconViewer.FilterByIconKeywords := '';
end;

procedure TIconViewerFrame.btnExecuteFilterClick(Sender: TObject);
var
  filter: String;
begin
  filter := cmbFilterByKeywords.Text;
  FIconViewer.FilterByIconKeywords := filter;
  AddKeywordFilterToHistory(filter);
  cmbFilterByKeywords.Text := filter;   // Must be after AddKeywordFilterToHistory!
end;

procedure TIconViewerFrame.cmbFilterByKeywordsEditingDone(Sender: TObject);
begin
  btnExecuteFilterClick(nil);
end;

procedure TIconViewerFrame.cmbFilterBySizeChange(Sender: TObject);
begin
  if cmbFilterBySize.ItemIndex = 0 then    // Filter by any size
    FIconViewer.FilterByIconSize := ''
  else
    FIconViewer.FilterByIconSize := cmbFilterBySize.Items[cmbFilterBySize.ItemIndex];
  FIconViewer.Invalidate;
end;

procedure TIconViewerFrame.cmbFilterByStyleChange(Sender: TObject);
begin
  FIconViewer.FilterByIconStyle := TIconStyle(cmbFilterByStyle.ItemIndex);
  FIconViewer.Invalidate;
end;

procedure TIconViewerFrame.CopyMetadataToNameBase(AIcon: TIconItem);
begin
  IconViewer.CopyMetadataToNameBase(AIcon);
  IconViewer.Invalidate;
  UpdateIconDetails;
end;

procedure TIconViewerFrame.CreateHandle;
var
  w: Integer;
begin
  inherited;

  if not FLayoutFixed then
  begin
    FLayoutFixed := true;
    w := lblFileName.Width;
    if w < lblSize.Width then w := lblSize.Width;
    if w < lblKeywords.Width then w := lblKeywords.Width;
    inc(w, 8);
    infoFileName.BorderSpacing.Left := w;
    infoSize.BorderSpacing.Left := w;
    infoStyle.BorderSpacing.Left := w;
    infoKeywords.BorderSpacing.Left := w;
  end;
end;

procedure TIconViewerFrame.DeleteSelectedIcon;
var
  res: TModalResult;
begin
  res := MessageDlg('Do you really want to delete the selected icon from the library?',
    mtConfirmation, [mbYes, mbNo], 0);
  if res = mrYes then
  begin
    IconViewer.DeleteIcon(IconViewer.SelectedIcon);
    UpdateIconDetails;
  end;
end;

procedure TIconViewerFrame.DoIconViewerDblClick(Sender: TObject);
begin
  if Assigned(FOnIconDblClick) then
    FOnIconDblClick(Self);
end;

procedure TIconViewerFrame.DoIconViewerFilter(Sender: TObject);
begin
  if Assigned(FOnFilter) then
    FOnFilter(Self);
end;

procedure TIconViewerFrame.DoIconViewerSelect(Sender: TObject);
begin
  UpdateIconDetails;
end;

function TIconViewerFrame.GetFilteredCount: Integer;
begin
  Result := FIconViewer.ThumbnailCount;
end;

function TIconViewerFrame.GetImageIndex(AIndex: Integer): TImageIndex;
begin
  case AIndex of
    0: Result := btnExecuteFilter.ImageIndex;
    1: Result := btnClearFilter.ImageIndex;
  end;
end;

function TIconViewerFrame.GetImageList: TCustomImageList;
begin
  Result := btnExecuteFilter.Images;
end;

procedure TIconViewerFrame.GetKeywordsHistory(AList: TStrings);
begin
  AList.Assign(cmbFilterByKeywords.Items);
end;

function TIconViewerFrame.GetSelectedIcon: TIconItem;
begin
  Result := FIconViewer.SelectedIcon;
end;

function TIconViewerFrame.GetTotalCount: Integer;
begin
  Result := FIconViewer.IconCount;
end;

procedure TIconViewerFrame.IconDetailsPanelResize(Sender: TObject);
begin
  if FIconViewer.SelectedIcon <> nil then
  begin
    infoFileName.Hint := FIconViewer.SelectedIcon.FileName;
    infoFileName.Caption := MinimizeName(infoFileName.Hint, Canvas, infoFileName.Width - infoFileName.BorderSpacing.Right);
  end;
end;

{ Reads the icons from the directories contained in AList and adds them to the
  library. }
procedure TIconViewerFrame.ReadIconFolders(AList: TStrings);
var
  sizeFilter, styleFilter: Integer;
begin
  styleFilter := cmbFilterByStyle.ItemIndex;
  if styleFilter = -1 then styleFilter := 0;
  sizeFilter := cmbFilterBySize.ItemIndex;
  if sizeFilter = -1 then sizeFilter := 0;

  IconViewer.LockFilter;
  try
    IconViewer.ReadIconFolders(AList);
    UpdateIconSizes(sizeFilter);
    UpdateIconStyles(stylefilter);
  finally
    IconViewer.UnlockFilter;
  end;
end;

procedure TIconViewerFrame.SetImageIndex(AIndex: Integer; AValue: TImageIndex);
begin
  case AIndex of
    0: btnExecuteFilter.ImageIndex := AValue;
    1: btnClearFilter.ImageIndex := AValue;
  end;
end;

procedure TIconViewerFrame.SetImageList(AValue: TCustomImageList);
begin
  btnExecuteFilter.Images := AValue;
  btnClearFilter.Images := AValue;
end;

procedure TIconViewerFrame.SetKeywordsHistory(AList: TStrings);
begin
  cmbFilterByKeywords.Items.BeginUpdate;
  try
    cmbFilterByKeywords.Items.Assign(AList);
    while cmbFilterByKeywords.Items.Count > MAX_KEYWORDS_HISTORY do
      cmbFilterByKeywords.Items.Delete(cmbFilterByKeywords.Items.Count-1);
  finally
    cmbFilterByKeywords.Items.EndUpdate;
  end;
end;

procedure TIconViewerFrame.UpdateCmds;
begin
  btnKeywordEditor.Enabled := TotalCount > 0;
end;

procedure TIconViewerFrame.UpdateIconDetails;
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

procedure TIconViewerFrame.UpdateIconSizes(ASizeIndex: Integer);
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

procedure TIconViewerFrame.UpdateIconStyles(AStyleIndex: Integer);
begin
  IconStylesToStrings(cmbFilterByStyle.Items);
  if AStyleIndex < 0 then AStyleIndex := 0;
  cmbFilterByStyle.ItemIndex := AStyleIndex;
  if AStyleIndex = 0 then
    FIconViewer.FilterByIconStyle := isAnyStyle
  else
    FIconViewer.FilterByIconStyle := TIconStyle(AStyleIndex);
end;

end.

