unit IconViewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  LazFileUtils,
  Forms, Controls, Graphics, StdCtrls, ExtCtrls, FileCtrl, Buttons, Dialogs,
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
    function GetSelectedIcon: TIconItem;
    function GetTotalCount: Integer;

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
    procedure AddIconFolder(AFolder: String);
    procedure CopyMetadataToNameBase(AIcon: TIconItem);
    procedure DeleteSelectedIcon;
    procedure ReadIconFolders(AList: TStrings);
    procedure UpdateIconSizes(ASizeIndex: Integer);
    procedure UpdateIconStyles(AStyleIndex: Integer);

    property FilteredCount: Integer read GetFilteredCount;
    property IconViewer: TIconViewer read FIconViewer;
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
  UpdateIconDetails;
end;

procedure TIconViewerFrame.AddIconFolder(AFolder: String);
var
  filterByStyle: Integer;
  filterBySize: Integer;
begin
  filterByStyle := cmbFilterByStyle.ItemIndex;
  if filterByStyle = -1 then filterByStyle := 0;
  filterBySize := cmbFilterBySize.ItemIndex;
  if filterBySize = -1 then filterBySize := 0;

  AFolder := AppendPathDelim(SwitchPathDelims(AFolder, true));
  IconViewer.AddIconFolder(AFolder);

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
      FIconViewer.Invalidate;
    end;
  finally
    L.Free;
    F.Free;
  end;
end;

procedure TIconViewerFrame.cmbFilterBySizeChange(Sender: TObject);
begin
  if cmbFilterBySize.ItemIndex = 0 then    // Filter by any size
    FIconViewer.FilterByIconSize := ''
  else
    FIconViewer.FilterByIconSize := cmbFilterBySize.Items[cmbFilterBySize.ItemIndex];
  FIconViewer.Invalidate;
end;

procedure TIconViewerFrame.cmbFilterByKeywordsEditingDone(Sender: TObject);
var
  filter: String;
  idx: Integer;
begin
  filter := cmbFilterByKeywords.Text;
  FIconViewer.FilterByIconKeywords := filter;

  // Add to history list
  AddKeywordFilterToHistory(filter);
  cmbFilterByKeywords.Text := filter;
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
    infoFileName.BorderSpacing.Left := w + 8;
    infoSize.BorderSpacing.Left := infoFileName.BorderSpacing.Left;
    infoKeywords.BorderSpacing.Left := infoFileName.BorderSpacing.Left;
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

