unit IconThumbNails;

{$mode ObjFPC}{$H+}
{$define OVERLAY_ICONS}

// Select one of these:
{.$define METADATA_TXT}
{$define METADATA_XML}

interface

uses
  Classes, SysUtils, fgl, FPImage, StrUtils,
  {$ifdef METADATA_XML}
  laz2_dom, laz2_xmlread, laz2_xmlwrite,
  {$endif}
  FileUtil, LazFileUtils, Graphics, Controls, Dialogs, Menus, Forms,
  BasicThumbnails;

type
  TIconItem = class;
  TIconViewer = class;

  TIconStyle = (isAnyStyle, isClassic, isFlat, isOutline, isOutline2);

  TIconThumbnail = class(TBasicThumbnail)
  private
    FItem: TIconItem;
  public
    constructor Create(AItem: TIconItem); reintroduce;
    procedure DrawToCanvas(ACanvas: TCanvas; ARect: TRect); override;
    property Item: TIconItem read FItem;
  end;

  TIconItem = class(TObject)
  private
    FFileName: String;   // including path
    FWidth: Integer;
    FHeight: Integer;
    FStyle: TIconStyle;
    FKeywords: TStrings;
    FPicture: TPicture;
    FHidden: Boolean;
    FViewer: TIconViewer;
    procedure SetStyleAsString(AValue: String);
  protected
    function GetDirectory: String;
    function GetKeywordCount: Integer;
    function GetKeywords(AIndex: Integer): String;
    function GetKeywordsAsString: String;
    function GetName: String;
    function GetNameBase: String;
    function GetPicture: TPicture;
    function GetSizeAsString: String;
    function GetStyleAsString: String;
  public
    constructor Create(AFileName, AKeywords: String; AStyle: TIconStyle; AWidth, AHeight: Integer);
    destructor Destroy; override;
    procedure CopyMetadataFrom(AIcon: TIconItem);
    procedure ExportKeywordsToStrings(AList: TStrings);
    function HasKeyword(AKeyword: String): Boolean;
    function HasKeywordPart(AKeywordPart: String): Boolean;
    procedure SetKeywordsFromStrings(AList: TStrings);

    property Directory: String read GetDirectory;
    property FileName: String read FFileName;
    property Height: Integer read FHeight;
    property Hidden: Boolean read FHidden write FHidden;
    property KeywordCount: Integer read GetKeywordCount;
    property Keywords[AIndex: Integer]: String read GetKeywords;
    property KeywordsAsString: String read GetKeywordsAsString;
    property Name: String read GetName;
    property NameBase: String read GetNameBase;
    property Picture: TPicture read GetPicture;
    property SizeAsString: String read GetSizeAsString;
    property Style: TIconStyle read FStyle write FStyle;
    property StyleAsString: String read GetStyleAsString write SetStyleAsString;
    property Width: Integer read FWidth;
  end;

  TIconList = class(specialize TFPGObjectList<TIconItem>)
  public
    function IndexOfFileName(AFileName: String): Integer;
  end;

  TIconFolderItem = class
    FolderName: String;
    Hidden: Boolean;
    Dirty: Boolean;
  end;

  TIconFolderList = class(specialize TFPGObjectlist<TIconFolderItem>)
  public
    function AddFolder(AFolderName: String; IsHidden: Boolean): Integer;
    procedure Hide(AFolder: String);
    procedure Hide(AIndex: Integer);
    function IndexOf(AFolder: String): Integer;
    function IsHidden(AFolder: string): Boolean;
    function IsHidden(AIndex: Integer): Boolean;
    procedure Show(AFolder: string);
    procedure Show(AIndex: Integer);
    procedure Toggle(AFolder: String);
    procedure Toggle(AIndex: Integer);
  end;

  TIconViewer = class(TBasicThumbnailViewer)
  private
    FIconFolders: TIconFolderList;
    FIconList: TIconList;
    FSizes: TStrings;
    FFilterByIconSize: String;
    FFilterByIconStyle: TIconStyle;
    FFilterByIconWidth: Integer;
    FFilterByIconHeight: Integer;
    FFilterByIconKeywords: String;
    FLargestIconWidth: Integer;
    FLargestIconHeight: Integer;
    FAutoThumbnailSize: Boolean;
    FFilterLock: Integer;
    FOnFilter: TNotifyEvent;
    function GetIconCount: Integer;
    procedure IconFolderClicked(Sender: TObject);
    procedure SetFilterByIconKeywords(AValue: String);
    procedure SetFilterByIconSize(AValue: String);
    procedure SetFilterByIconStyle(AValue: TIconStyle);

  protected
    FSelectedIcon: TIconItem;
    {$ifdef OVERLAY_ICONS}
    FOverlayIcons: array[0..2] of TGraphic;
    procedure DrawThumbnail(AThumbnail: TBasicThumbnail; ARect: TRect); override;
    {$endif}
    function AcceptIcon(AIcon: TIconItem): Boolean; virtual;
    function AcceptKeywords(AIcon: TIconItem): Boolean;
    function AddIcon(AFileName, AKeywords: String; AStyle: TIconStyle; AWidth, AHeight: Integer): TIconItem;
    procedure DeleteIconFolder(AFolder: String);
    procedure FilterIcons;
    function MetadataDirty: Boolean;
    procedure ReadIconFolder(AFolder: String);
    procedure ReadIcons(AFolder: String; AHidden: Boolean);
    procedure ReadMetadataFile(AFileName: String; AHidden: Boolean);
    procedure SetSelectedIndex(AValue: Integer); override;
    function ThumbnailMarked(AThumbnail: TBasicThumbnail): Boolean; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddIconFolder(AFolder: String);
    procedure Clear; override;
    procedure CopyMetadataToNameBase(AIcon: TIconItem);
    procedure DeleteIcon(AIcon: TIconItem);
    function FilterLocked: Boolean;
    function FindIconSize(AIcon: TIconItem; AWidth, AHeight: Integer): TIconItem;
    function FindLargestIcon(AIcon: TIconItem): TIconItem;
    procedure GetIconSizesAsStrings(AList: TStrings);
    procedure GetKeywordsAsStrings(AList: TStrings);
    function IndexOfThumbnail(AIcon: TIconItem): Integer;
    procedure LockFilter;
    procedure PopulateIconFoldersMenu(AMenu: TMenu);
    procedure ReadIconFolders(AList: TStrings);
    function SelectIconInFile(AFileName: String): Boolean;
    procedure UnlockFilter;
    procedure UpdateIconFolders;
    procedure WriteIconFolders(AList: TStrings);
    procedure WriteMetadataFiles;

    property AutoThumbnailSize: Boolean read FAutoThumbnailSize write FAutoThumbnailSize default true;
    property FilterByIconKeywords: string read FFilterByIconKeywords write SetFilterByIconKeywords;
    property FilterByIconSize: string read FFilterByIconSize write SetFilterByIconSize;
    property FilterByIconStyle: TIconStyle read FFilterByIconStyle write SetFilterByIconStyle;
    property IconCount: Integer read GetIconCount;
    property IconFolders: TIconFolderList read FIconFolders;
    property LargestIconWidth: Integer read FLargestIconWidth;
    property LargestIconHeight: Integer read FLargestIconHeight;
    property SelectedIcon: TIconItem read FSelectedIcon;
    property OnFilter: TNotifyEvent read FOnFilter write FOnFilter;

  end;

procedure IconStylesToStrings(AList: TStrings);
function StrToIconStyle(AText: String): TIconStyle;


implementation

{$ifdef OVERLAY_ICONS}
 {$R overlay.res}
{$endif}

const
  ICON_MARGIN = 8;  // or, more precisely: double of margin
  {$ifdef METADATA_TXT}
  METADATA_FILENAME = 'metadata.txt';
  {$endif}
  {$ifdef METADATA_XML}
  METADATA_FILENAME = 'metadata.xml';
  {$endif}
  ICONSTYLE_NAMES: Array[TIconStyle] of String = (
    '(any style)', 'classic', 'flat', 'outline', 'outline 2-color'
  );
  IMAGES_MASK = '*.png;*.bmp';

procedure IconStylesToStrings(AList: TStrings);
var
  style: TIconStyle;
begin
  AList.Clear;
  for style in TIconStyle do
    AList.Add(ICONSTYLE_NAMES[style]);
end;

function StrToIconStyle(AText: String): TIconStyle;
begin
  case lowercase(AText) of
    '':
      Result := isAnyStyle;
    'classic':
      Result := isClassic;
    'flat':
      Result := isFlat;
    'outline':
      Result := isOutline;
    'outline2', 'outline 2-color':
      Result := isOutline2;
    else
      raise Exception.Create('[StrToIconStyle] Unknown icon style');
  end;
end;


{ TIconThumbnail }

constructor TIconThumbnail.Create(AItem: TIconItem);
begin
  inherited Create;
  FItem := AItem;  // Do not destroy this item!
end;

procedure TIconThumbnail.DrawToCanvas(ACanvas: TCanvas; ARect: TRect);
var
  pic: TPicture;
  x, y: Integer;
begin
  pic := FItem.Picture;
  if pic <> nil then
  begin
    x := (Viewer.ThumbnailWidth - pic.Width) div 2;
    y := (Viewer.ThumbnailHeight - pic.Height) div 2;
    ACanvas.Draw(ARect.Left + x, ARect.Top + y, pic.Bitmap);
  end;
end;


{ TIconItem }

constructor TIconItem.Create(AFileName, AKeywords: String; AStyle: TIconStyle;
  AWidth, AHeight: Integer);
var
  i: Integer;
begin
  inherited Create;
  FFileName := AFileName;
  FStyle := AStyle;
  FWidth := AWidth;
  FHeight := AHeight;
  FKeywords := TStringList.Create;
  TStringList(FKeywords).Sorted := true;
  TStringList(FKeywords).CaseSensitive := false;
  TStringList(FKeywords).Duplicates := dupIgnore;
  FKeywords.Delimiter := ';';
  FKeywords.StrictDelimiter := true;
  FKeywords.DelimitedText := AKeywords;
  for i := FKeywords.Count-1 downto 0 do  // No empty keywords
    if FKeywords[i] = '' then FKeywords.Delete(i);
end;

destructor TIconItem.Destroy;
begin
  FKeywords.Free;
  FreeAndNil(FPicture);
  inherited;
end;

procedure TIconItem.CopyMetadataFrom(AIcon: TIconItem);
var
  folder: String;
  idx: Integer;
begin
  FKeywords.Assign(AIcon.FKeywords);
  FStyle := AIcon.FStyle;

  // Mark the folder of the icon as "dirty" so that it can be re-written.
  folder := AIcon.GetDirectory;
  idx := FViewer.IconFolders.IndexOf(folder);
  if idx > -1 then
    FViewer.IconFolders[idx].Dirty := true;
end;

function TIconItem.GetDirectory: String;
begin
  Result := ExtractFilePath(FFileName);
end;

function TIconItem.GetKeywordCount: Integer;
begin
  Result := FKeywords.Count;
end;

function TIconItem.GetKeywords(AIndex: Integer): String;
begin
  Result := FKeywords[AIndex];
end;

function TIconItem.GetKeywordsAsString: String;
var
  i: Integer;
begin
  if FKeywords.Count = 0 then
    Result := ''
  else
  begin
    Result := FKeywords[0];
    for i := 1 to FKeywords.Count-1 do
      Result := Result + ';' + FKeywords[i];
  end;
end;

function TIconItem.GetName: String;
begin
  Result := ChangeFileExt(ExtractFileName(FFileName), '');
end;

function TIconItem.GetNameBase: String;
var
  p, n: Integer;
  suffix: String;
begin
  Result := GetName;
  p := RPos('_', Result);
  if p > 0 then
  begin
    suffix := Copy(Result, p+1);
    if TryStrToInt(suffix, n) then
      Result := Copy(Result, 1, p-1);
  end;
end;

function TIconItem.GetPicture: TPicture;
begin
  if FPicture = nil then
  begin
    FPicture := TPicture.Create;
    FPicture.LoadFromFile(FFileName);
  end;
  Result := FPicture;
end;

function TIconItem.GetSizeAsString: String;
begin
  Result := Format('%d x %d', [FWidth, FHeight]);
end;

function TIconItem.GetStyleAsString: String;
begin
  Result := ICONSTYLE_NAMES[FStyle];
end;

function TIconItem.HasKeyword(AKeyword: String): Boolean;
begin
  Result := FKeywords.IndexOf(AKeyword) <> -1;
end;

function TIconItem.HasKeywordPart(AKeywordPart: String): Boolean;
var
  i: Integer;
begin
  Result := true;
  AKeywordPart := Lowercase(AKeywordPart);
  for i := 0 to FKeywords.Count-1 do
    if pos(AKeywordPart, Lowercase(FKeywords[i])) = 1 then
      exit;
  Result := false;
end;

procedure TIconItem.ExportKeywordsToStrings(AList: TStrings);
begin
  Assert(AList <> nil);
  AList.Assign(FKeywords);
end;

procedure TIconItem.SetKeywordsFromStrings(AList: TStrings);
var
  i, j: Integer;
begin
  FKeywords.BeginUpdate;
  try
    FKeywords.Clear;
    for i := 0 to AList.Count-1 do
    begin
      j := FKeywords.IndexOf(AList[i]);
      if j = -1 then
        FKeywords.Add(AList[i]);
    end;
  finally
    FKeywords.EndUpdate;
  end;
end;

procedure TIconItem.SetStyleAsString(AValue: String);
begin
  FStyle := StrToIconStyle(AValue);
end;


{ TIconList }

function TIconList.IndexOfFileName(AFileName: String): Integer;
var
  i: Integer;
  item: TIconItem;
begin
  for i := 0 to Count-1 do
  begin
    item := Items[i];
    if item.FileName = AFileName then
    begin
      Result := i;
      exit;
    end;
  end;
  Result := -1;
end;


{ TIconFolderList

  A list with the folder names stored in TIconFolderItem instances.
  Besides the folder names, each item contains a flag to hide the icons of
  that folder, as well as a Dirty flag to indicate that there are icons in
  that folder with modified metadata and that the folder's metadata need
  re-saving.
}
function TIconFolderList.AddFolder(AFolderName: String; IsHidden: Boolean): Integer;
var
  item: TIconFolderItem;
begin
  item := TIconFolderItem.Create;
  item.FolderName := AFolderName;
  item.Hidden := IsHidden;
  item.Dirty := false;
  Result := Add(item);
end;

procedure TIconFolderList.Hide(AFolder: String);
var
  idx: Integer;
begin
  idx := IndexOf(AFolder);
  if idx > -1 then
    Hide(idx);
end;

procedure TIconFolderList.Hide(AIndex: Integer);
begin
  Items[AIndex].Hidden := true;
end;

function TIconFolderList.IndexOf(AFolder: String): Integer;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
  begin
    if Items[i].FolderName = AFolder then
    begin
      Result := i;
      exit;
    end;
  end;
  Result := -1;
end;

function TIconFolderList.IsHidden(AFolder: string): Boolean;
var
  idx: Integer;
begin
  idx := IndexOf(AFolder);
  if idx > -1 then
    Result := Items[idx].Hidden
  else
    Result := false;
end;

function TIconFolderList.IsHidden(AIndex: Integer): Boolean;
begin
  Result := Items[AIndex].Hidden;
end;

procedure TIconFolderList.Show(AFolder: string);
var
  idx: Integer;
begin
  idx := IndexOf(AFolder);
  if idx > -1 then
    Show(idx);
end;

procedure TIconFolderList.Show(AIndex: Integer);
begin
  Items[AIndex].Hidden := false;
end;

procedure TIconFolderList.Toggle(AFolder: String);
var
  idx: Integer;
begin
  idx := IndexOf(AFolder);
  if idx > -1 then
  begin
    Toggle(idx);
  end;
end;

procedure TIconFolderList.Toggle(AIndex: Integer);
begin
  Items[AIndex].Hidden := not Items[AIndex].Hidden;
end;


{ TIconViewer }

constructor TIconViewer.Create(AOwner: TComponent);
begin
  inherited;
  FIconFolders := TIconFolderList.Create;
  FIconList := TIconList.Create;
  FSizes := TStringList.Create;
  TStringList(FSizes).Sorted := true;
  FAutoThumbnailSize := true;
  {$ifdef OVERLAY_ICONS}
  FOverlayIcons[0] := TPortableNetworkGraphic.Create;
  FOverlayIcons[1] := TPortableNetworkGraphic.Create;
  FOverlayIcons[2] := TPortableNetworkGraphic.Create;
  FOverlayIcons[0].LoadFromResourceName(HINSTANCE, 'ovl_H_8');
  FOverlayIcons[1].LoadFromResourceName(HINSTANCE, 'ovl_H_12');
  FOverlayIcons[2].LoadFromResourceName(HINSTANCE, 'ovl_H_16');
  {$endif}
end;

destructor TIconViewer.Destroy;
var
  res: Integer;
begin
  {$ifdef OVERLAY_ICONS}
  FOverlayIcons[2].Free;
  FOverlayIcons[1].Free;
  FOverlayIcons[0].Free;
  {$endif}
  if MetadataDirty then
  begin
    res := MessageDlg('Metadata have been changed. Save?', mtConfirmation, [mbYes, mbNo], 0);
    if res = mrYes then
      WriteMetadataFiles;
  end;
  FSizes.Free;
  FIconList.Free;
  FIconFolders.Free;
  inherited;
end;

{ Main filtering method: Compares the icon properties with the filter conditions
  and returns true when the icon can be displayed as a thumbnail. }
function TIconViewer.AcceptIcon(AIcon: TIconItem): Boolean;
begin
  Result := false;
  if AIcon.Hidden then
    exit;
  if (FFilterByIconSize <> '') and ((AIcon.Width <> FFilterByIconWidth) or (AIcon.Height <> FFilterByIconHeight)) then
    exit;
  if (FFilterByIconStyle <> isAnyStyle) and (AIcon.Style <> FFilterByIconStyle) then
    exit;
  if (FFilterByIconKeywords <> '') and not AcceptKeywords(AIcon) then
    exit;
  Result := true;
  if Result then
  begin
    if FLargestIconWidth < AIcon.Width then FLargestIconWidth := AIcon.Width;
    if FLargestIconHeight < AIcon.Height then FLargestIconHeight := AIcon.Height;
  end;
end;

{ Implements a simple parser for logical expressions between keywords, e.g.
     keyword1 AND keyword2
     keyword1 OR NOT keyword2 AND keyword3 AND NOT keyword4
     keyword1 keyword2  -- defaults to AND
  No brackets allowed! }
function TIconViewer.AcceptKeywords(AIcon: TIconItem): Boolean;
type
  TOperationKind = (opkAND, opkOR, opkNOT);
  TOperation = record
    Kind: TOperationKind;
    Left, Right: Boolean;
    Complete: Boolean;
  end;
const
  DEFAULT_OPERATION = opkAND;
var
  operation: TOperation;
  i: Integer;
  parts: TStringArray;
begin
  Result := True;
  parts := FFilterByIconKeywords.Split(' ');

  operation := Default(TOperation);
  operation.Kind := DEFAULT_OPERATION;
  operation.Left := true;

  i := 0;
  while i < Length(parts) do
  begin
    case Uppercase(parts[i]) of
      'AND': operation.Kind := opkAND;
      'OR': operation.Kind := opkOR;
      'NOT':
        begin
          inc(i);
          operation.Right := not AIcon.HasKeywordPart(parts[i]);
          operation.Complete := true;
        end;
      else
        operation.Right := AIcon.HasKeywordPart(parts[i]);
        operation.Complete := true;
    end;
    if operation.Complete then
    begin
      case operation.Kind of
        opkAND: operation.Left := operation.Left and operation.Right;
        opkOR:  operation.Left := operation.Left or operation.Right;
      end;
      operation.Complete := false;
      operation.Kind := DEFAULT_OPERATION;
    end;
    inc(i);
  end;
  Result := operation.Left;
end;

{ Adds the associated icon to the unfiltered icon list. }
function TIconViewer.AddIcon(AFileName, AKeywords: String; AStyle: TIconStyle;
  AWidth, AHeight: Integer): TIconItem;
var
  sizeStr: String;
  idx: Integer;
begin
  idx := FIconList.IndexOfFileName(AFileName);
  if idx = -1 then
  begin
    Result := TIconItem.Create(AFileName, AKeywords, AStyle, AWidth, AHeight);
    Result.FViewer := Self;
    FIconList.Add(Result);
  end else
    Result := FIconList[idx];

  sizeStr := Format('%d x %d', [AWidth, AHeight]);
  if FSizes.IndexOf(sizeStr) = -1 then
    FSizes.Add(sizestr);
end;

procedure TIconViewer.AddIconFolder(AFolder: String);
begin
  AFolder := AppendPathDelim(AFolder);
  if FIconFolders.IndexOf(AFolder) > -1 then   // Avoid duplicates
    DeleteIconFolder(AFolder);
  ReadIconFolder(AFolder);
  if FIconFolders.IndexOf(AFolder) = -1 then
    FIconFolders.AddFolder(AFolder, false);

  FilterIcons;
  SelectedIndex := -1;
end;

procedure TIconViewer.Clear;
begin
  inherited;
  FLargestIconWidth := 0;
  FLargestIconHeight := 0;
  FSizes.Clear;
  FIconList.Clear;
  FIconFolders.Clear;
end;

{ Copies the metadata from the given icon to all other icons sharing the
  same name base (= part in name before the last '_') and directory. }
procedure TIconViewer.CopyMetadataToNameBase(AIcon: TIconItem);
var
  i: Integer;
  item: TIconItem;
  itemDir, iconDir: String;
  iconNameBase: String;
begin
  iconNameBase := AIcon.NameBase;
  iconDir := AIcon.Directory;
  for i := 0 to FIconList.Count-1 do
  begin
    item := FIconList[i];
    itemDir := FIconList[i].Directory;
    if SameText(iconNameBase, item.NameBase) and SameText(iconDir, itemDir) and (item <> AIcon) then
      item.CopyMetadataFrom(AIcon);
  end;
end;

procedure TIconViewer.DeleteIcon(AIcon: TIconItem);
var
  selIdx: Integer = -1;
  iconIdx: Integer;
begin
  if AIcon = SelectedIcon then
    selIdx := SelectedIndex;

  iconIdx := FIconList.IndexOf(AIcon);
  if iconIdx <> -1 then
  begin
    FIconList.Delete(iconIdx);
    LockFilter;
    try
      if (selIdx <> -1) then
      begin
        if selIdx >= ThumbnailCount then selIdx := ThumbnailCount-1;
        SelectedIndex := selIdx;
      end;
    finally
      UnlockFilter;
    end;
  end;
end;

procedure TIconViewer.DeleteIconFolder(AFolder: String);
var
  i: Integer;
  folder: String;
begin
  for i := FIconFolders.Count-1 downto 0 do
    if FIconFolders[i].FolderName = AFolder then
      FIconFolders.Delete(i);

  for i := FIconList.Count-1 downto 0 do
  begin
    folder := ExtractFilePath(FIconList[i].FileName);
    if folder = AFolder then
      FIconList.Delete(i);
  end;
end;

{$ifdef OVERLAY_ICONS}
procedure TIconViewer.DrawThumbnail(AThumbnail: TBasicThumbnail; ARect: TRect);
var
  ovl: TGraphic;
  ppi: Integer;
  item: TIconItem;
begin
  inherited;

  item := TIconThumbnail(AThumbnail).Item;
  if (item.KeywordCount = 0) or (item.Style = isAnyStyle) then
  begin
    ppi := Font.PixelsPerInch;
    if ppi < 120 then
      ovl := FOverlayIcons[0]
    else if ppi < 168 then
      ovl := FOverlayIcons[1]
    else
      ovl := FOverlayIcons[2];
    Canvas.Draw(ARect.Left+1, ARect.Top+1, ovl);
  end;
end;
{$endif}

procedure TIconViewer.FilterIcons;
var
  i: Integer;
  item: TIconItem;
  oldNameBase: String = '';
begin
  if FilterLocked then
    exit;

  if SelectedIcon <> nil then
    oldNameBase := SelectedIcon.NameBase;

  ThumbnailList.Clear;

  for i := 0 to FIconList.Count-1 do
  begin
    item := FIconList[i];
    if AcceptIcon(item) then
      Add(TIconThumbnail.Create(item));
  end;

  if FAutoThumbnailSize then
  begin
    if FFilterByIconSize = '' then
    begin
      FThumbnailWidth := FLargestIconWidth + ICON_MARGIN;
      FThumbnailHeight := FLargestIconHeight + ICON_MARGIN;
    end else
    begin
      FThumbnailWidth := FFilterByIconWidth + ICON_MARGIN;
      FThumbnailHeight := FFilterByIconHeight + ICON_MARGIN;
    end;
  end;

  LayoutThumbnails;

  if oldNameBase <> '' then
    for i := 0 to ThumbnailCount-1 do
      if (TIconThumbnail(Thumbnail[i]).Item.NameBase = oldNameBase) then
      begin
        SelectedIndex := i;
        exit;
      end;

  SelectedIndex := -1;

  if Assigned(FOnFilter) then
    FOnFilter(self);
end;

function TIconViewer.FilterLocked: Boolean;
begin
  Result := FFilterLock <> 0;
end;

{ Finds the icon list entry for the specified item which has the same
  namebase, but the specified size.
  When AWidth = -1 and AHeight = -1 then the file without appendix is used. }
function TIconViewer.FindIconSize(AIcon: TIconItem; AWidth, AHeight: Integer): TIconItem;
var
  i: Integer;
  iconNameBase: String;
  item: TIconItem;
  itemNameBase: String;
begin
  iconNameBase := AIcon.NameBase;
  for i := 0 to FIconList.Count-1 do
  begin
    item := FIconList[i];
    itemNameBase := item.NameBase;
    if SameText(itemNameBase, iconNameBase) then
    begin
      // Items with appendix
      if Length(item.Name) <> Length(itemNameBase) then
      begin
        if (item.Width = AWidth) and (item.Height = AHeight) then
        begin
          Result := item;
          exit;
        end;
      end else
      begin
        // Item without appendix
        Result := item;
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TIconViewer.FindLargestIcon(AIcon: TIconItem): TIconItem;
var
  i: Integer;
  iconNameBase: String;
  item: TIconItem;
  itemNameBase: String;
  w, h: Integer;
begin
  Result := nil;
  iconNameBase := AIcon.NameBase;
  w := 0;
  for i := 0 to FIconList.Count-1 do
  begin
    item := FIconList[i];
    itemNameBase := item.NameBase;
    if SameText(itemNameBase, iconNameBase) then
    begin
      if item.Width > w then  // considering only width
      begin
        w := item.Width;
        Result := item;
      end;
    end;
  end;
end;

{ Returns the number of unfiltered icons loaded. }
function TIconViewer.GetIconCount: Integer;
begin
  Result := FIconList.Count;
end;

{ Returns a sorted list with all available icon sizes, formatted as "width x height" }
procedure TIconViewer.GetIconSizesAsStrings(AList: TStrings);
begin
  AList.Assign(FSizes);
end;

procedure TIconViewer.GetKeywordsAsStrings(AList: TStrings);
var
  i, j, k: Integer;
  keyword: String;
  item: TIconItem;
  list: TStringList;
begin
  list := TStringList.Create;
  try
    list.CaseSensitive := false;
    list.Sorted := true;
    for i := 0 to FIconList.Count-1 do
    begin
      item := FIconList[i];
      for j := 0 to item.KeywordCount-1 do
      begin
        keyword := item.Keywords[j];
        if not list.Find(keyword, k) then
          list.Add(keyword);
      end;
    end;
    AList.Assign(list);
  finally
    list.Free;
  end;
end;

{ OnClick handler for the menu items created by PopulateIconFoldersMenu to
  show/hide the icons of the clicked folder.
  The FIconFolders index of the folder is stored in the Tag of the menu item.
  Special tags: -1 --> show all folders, -2 --> hide all folders. }
procedure TIconViewer.IconFolderClicked(Sender: TObject);
var
  i, idx: Integer;
begin
  if TMenuItem(Sender).Tag < 0 then
  begin
    for i := 0 to FIconFolders.Count -1 do
    begin
      case TMenuItem(Sender).Tag of
        -1: // "Show all"
            TIconFolderList(FIconFolders).Show(i);
        -2: // "Hide all"
            TIconFolderList(FIconFolders).Hide(i);
        else
            exit;
      end;
    end;
  end else
  begin
    idx := TMenuItem(Sender).Tag;
    TIconFolderList(FIconFolders).Toggle(idx);
  end;

//  LockFilter;
//  try
    UpdateIconFolders;
//  finally
//    UnlockFilter;
//  end;
end;

function TIconViewer.IndexOfThumbnail(AIcon: TIconItem): Integer;
var
  i: Integer;
begin
  Result := -1;
  if AIcon = nil then
    exit;

  for i := 0 to ThumbnailCount-1 do
    if TIconThumbnail(Thumbnail[i]).Item = AIcon then
    begin
      Result := i;
      exit;
    end;
end;

procedure TIconViewer.LockFilter;
begin
  inc(FFilterLock);
end;

{ Returns true if at least one of the icon folders has been marked as "dirty"
  after changing its metadata.
  Called when the IconViewer is destroyed. Is evaluated for re-writing the metadata. }
function TIconViewer.MetadataDirty: Boolean;
var
  i: Integer;
begin
  for i := 0 to FIconFolders.Count-1 do
    if FIconFolders[i].Dirty then
    begin
      Result := true;
      exit;
    end;
  Result := false;
end;

{ Populates the given menu with the names of all folders from which icons have
  been loaded. Hidden folders (having a non-nil Objects property in the
  IconFolder list) are not checked in the menu. }
procedure TIconViewer.PopulateIconFoldersMenu(AMenu: TMenu);
var
  i: Integer;
  folder: String;
  menuitem: TMenuItem;

begin
  AMenu.Items.Clear;

  menuItem := TMenuItem.Create(AMenu);
  menuItem.Caption := 'Show all';
  menuItem.Tag := -1;
  menuItem.OnClick := @IconFolderClicked;
  AMenu.Items.Add(menuItem);

  menuItem := TMenuItem.Create(AMenu);
  menuItem.Caption := 'Hide all';
  menuItem.Tag := -2;
  menuItem.OnClick := @IconFolderClicked;
  AMenu.Items.Add(menuItem);

  menuItem := TMenuItem.Create(AMenu);
  menuItem.Caption := '-';
  AMenu.Items.Add(menuItem);

  for i := 0 to FIconFolders.Count-1 do
  begin
    menuItem := TMenuItem.Create(AMenu);
    menuItem.Checked := not FIconFolders[i].Hidden;
    menuItem.Caption := FIconFolders[i].FolderName;
    menuItem.AutoCheck := true;
    menuItem.Tag := i;
    menuItem.OnClick := @IconFolderClicked;
    AMenu.Items.Add(menuItem);
  end;
end;

{ Reads the icons found in the specified folder. }
procedure TIconViewer.ReadIconFolder(AFolder: String);
var
  isHidden: Boolean;
begin
  if AFolder = '' then
    exit;
  isHidden := TIconFolderList(FIconFolders).IsHidden(AFolder);
  if not DirectoryExists(AFolder) then
    exit;

  AFolder := AppendPathDelim(AFolder);
  if FileExists(AFolder + METADATA_FILENAME) then
    ReadMetadataFile(AFolder + METADATA_FILENAME, isHidden)
  else
    ReadIcons(AFolder, isHidden);
end;

{ Read the icons found in the folders of the given list.
  List items with a non-nil Objects property are marked as being hidden.
  Their names are stored but their icons are not displayed. }
procedure TIconViewer.ReadIconFolders(AList: TStrings);
var
  i: Integer;
  selectedIconFileName: String = '';
  folder: String;
  isHidden: Boolean;
begin
  if SelectedIcon <> nil then
    selectedIconFileName := SelectedIcon.FileName;;
  SelectedIndex := -1;  // this sets FSelectedIcon to nil.
  FIconFolders.Clear;
  FIconList.Clear;
  for i := 0 to AList.Count-1 do
  begin
    folder := AList[i];
    isHidden := AList.Objects[i] <> nil;
    if FIconFolders.IndexOf(folder) > -1 then    // Avoid duplicates
      DeleteIconFolder(folder);
    TIconFolderList(FIconFolders).AddFolder(folder, isHidden);
    ReadIconFolder(AList[i]);
  end;
  FilterIcons;
  SelectIconInFile(selectedIconFileName);
end;

{ Looks for image files (*.png, *.bmp) in the given folder and adds them to
  the viewer.
  When AHidden is true all icons are marked as hidden, i.e. are not displayed. }
procedure TIconViewer.ReadIcons(AFolder: String; AHidden: Boolean);
var
  files: TStrings;
  reader: TFPCustomImageReaderClass;
  stream: TStream;
  i, w, h: Integer;
begin
  files := TStringList.Create;
  try
    FindAllFiles(files, AFolder, IMAGES_MASK, false);
    for i := 0 to files.Count-1 do
    begin
      stream := TFileStream.Create(files[i], fmOpenRead or fmShareDenyNone);
      try
        reader := TFPCustomImage.FindReaderFromStream(stream);
        if reader <> nil then
        begin
          stream.Position := 0;
          with reader.ImageSize(stream) do
          begin
            w := X;
            h := Y;
          end;
          AddIcon(files[i], '', isAnyStyle, w, h).Hidden := AHidden;
        end;
      finally
        stream.Free;
      end;
    end;
  finally
    files.Free;
  end;
end;

procedure TIconViewer.ReadMetadataFile(AFileName: String; AHidden: Boolean);
{$ifdef METADATA_TXT}
{ Reads the given metadata file which contains a list of all icons and their
  metadata to be included by the viewer.
  When AHidden is true the icons, however, are marked as being hidden and are
  not displayed.

  metadata.txt is a text file in which the lines have the following structure:
    filename|width|height|style|keyword1;keyword2;...
    Allowed styles: classic, flat, outline, outline 2-color }
var
  lines: TStrings;
  i: Integer;
  s: String;
  parts: TStringArray;
  style: TIconStyle;
  w, h: Integer;
  folder: String;
  fn: String;
begin
  folder := ExtractFilePath(AFileName);
  lines := TStringList.Create;
  try
    lines.LoadFromFile(AFileName);
    for i := 0 to lines.Count-1 do
    begin
      s := lines[i];
      if (s = '') or (s[1] = '#') then Continue;
      parts := s.Split('|');
      if Length(parts) = 5 then
      begin
        fn := folder + parts[0];
        if FileExists(fn) then
        begin
          w := StrToInt(parts[1]);
          h := StrToInt(parts[2]);
          style := StrToIconStyle(parts[3]);
          AddIcon(fn, parts[4], style, w, h).Hidden := AHidden;
        end;
      end;
    end;
  finally
    lines.Free;
  end;
end;
{$endif}

{$ifdef METADATA_XML}
var
  doc: TXMLDocument = nil;
  root: TDOMNode;
  iconsNode, iconNode: TDOMNode;
  keywordsNode, keywordNode: TDOMNode;
  folder, fn: String;
  i: Integer;
  w, h: Integer;
  style: TIconStyle;
  s: String;
  keywords: String;
  files: TStringList;
  stream: TStream;
  reader: TFPCustomImageReaderClass;
begin
  folder := ExtractFilePath(AFileName);
  files := TStringList.Create;
  try
    files.Sorted := true;
    FindAllFiles(files, folder, IMAGES_MASK, false);

    ReadXMLFile(doc, AFileName);
    iconsNode := doc.DocumentElement.FindNode('icons');
    iconNode := iconsNode.FindNode('icon');
    while iconNode <> nil do begin
      fn := '';
      style := isAnystyle;
      if iconNode.HasAttributes then
        for i := 0 to iconNode.Attributes.Length-1 do
        begin
          s := iconNode.Attributes[i].NodeValue;
          case iconNode.Attributes[i].NodeName of
            'filename': fn := s;
            'width': w := StrToIntDef(s, 0);
            'height': h := StrToIntDef(s, 0);
            'style': style := StrToIconStyle(s);
          end;
        end;
      keywords := '';
      keywordsNode := iconNode.FindNode('keywords');
      if keywordsNode <> nil then
      begin
        keywordNode := keywordsNode.FindNode('keyword');
        while keywordNode <> nil do
        begin
          s := keywordNode.TextContent;
          keywords := keywords + ';' + s;
          keywordNode := keywordNode.NextSibling;
        end;
      end;
      if keywords <> '' then
        System.Delete(keywords, 1, 1);

      if (fn <> '') then
      begin
        fn := folder + fn;
        if FileExists(fn) then   // ignore metadata entries for which the files do not exist any more.
          AddIcon(fn, keywords, style, w, h).Hidden := AHidden;

        // Delete the processed filename from the files list
        i := files.IndexOf(fn);
        if i > -1 then files.Delete(i);
      end;

      iconNode := iconNode.NextSibling;
    end;

    // Every image which exists in the metadata file has been deleted from
    // the files list. The entries which are left identify new files. Add them
    // to the metafile
    for i := 0 to files.Count-1 do
    begin
      fn := files[i];
      stream := TFileStream.Create(files[i], fmOpenRead or fmShareDenyNone);
      try
        reader := TFPCustomImage.FindReaderFromStream(stream);
        if reader <> nil then
        begin
          stream.Position := 0;
          with reader.ImageSize(stream) do
          begin
            w := X;
            h := Y;
          end;
          AddIcon(files[i], '', isAnyStyle, w, h).Hidden := AHidden;
        end;
      finally
        stream.Free;
      end;
    end;

  finally
    doc.Free;
    files.Free;
  end;
end;
{$endif}

procedure TIconViewer.SetFilterByIconKeywords(AValue: String);
begin
  if FFilterByIconKeywords <> AValue then
  begin
    FFilterByIconKeywords := AValue;
    if not FilterLocked then
    begin
      FilterIcons;
      Invalidate;
    end;
  end;
end;

procedure TIconViewer.SetFilterByIconSize(AValue: string);
var
  sa: TStringArray;
begin
  if FFilterByIconSize <> AValue then
  begin
    FFilterByIconSize := AValue;
    if AValue = '' then
    begin
      FFilterByIconWidth := -1;
      FFilterByIconHeight := -1;
    end else
    begin
      sa := AValue.Split('x');
      FFilterByIconWidth := StrToInt(Trim(sa[0]));
      FFilterByIconHeight := StrToInt(Trim(sa[1]));
    end;
    if not FilterLocked then
    begin
      FilterIcons;
      Invalidate;
    end;
  end;
end;

procedure TIconViewer.SetFilterByIconStyle(AValue: TIconStyle);
begin
  if FFilterByIconStyle <> AValue then
  begin
    FFilterByIconStyle := AValue;
    if not FilterLocked then
    begin
      FilterIcons;
      Invalidate;
    end;
  end;
end;

{ Selected the icon among all visible thumbnails which is assigned to the
  given filename. }
function TIconViewer.SelectIconInFile(AFileName: String): Boolean;
var
  i, idx: Integer;
  folder: String;
begin
  // Find the index of the icon with the given filename among all thumbnails.
  idx := -1;
  for i := 0 to ThumbnailCount-1 do
  begin
    if TIconThumbnail(Thumbnail[i]).Item.FileName = AFileName then
    begin
      idx := i;
      break;
    end;
  end;

  if idx > -1 then
  begin
    // Make sure that the folder is not hidden
    folder := ExtractFilePath(AFileName);
    if not TIconFolderList(FIconFolders).IsHidden(folder) then
    begin
      SelectedIndex := idx;
      exit;
    end;
  end;

  SelectedIndex := -1;
end;

procedure TIconViewer.SetSelectedIndex(AValue: Integer);
var
  thumb: TIconThumbnail;
begin
  if AValue = SelectedIndex then
    exit;

  if (AValue > -1) and (AValue < ThumbnailCount) then
  begin
    thumb := Thumbnail[AValue] as TIconThumbnail;
    FSelectedIcon := thumb.Item;
  end else
    FSelectedIcon := nil;

  inherited;
end;

{ This is for emphasizing icons in the viewer which do not yet have keywords
  or have an unspecified style. }
function TIconViewer.ThumbnailMarked(AThumbnail: TBasicThumbnail): Boolean;
var
  item: TIconItem;
begin
  item := TIconThumbnail(AThumbnail).Item;
  Result := (item.KeywordCount = 0) or (item.Style = isAnyStyle);
end;

procedure TIconViewer.UnlockFilter;
begin
  dec(FFilterLock);
  if FFilterLock = 0 then
  begin
    FilterIcons;
    Invalidate;
  end;
end;

{ Folders and all their icons can be hidden by setting the Hidden flag of the
  folder record to true.
  This procedure iterates over all icons and sets their Hidden flag when
  their folder is hidden. }
procedure TIconViewer.UpdateIconFolders;
var
  i, j: Integer;
  hiddenFolders: TStringList;
  folder: String;
  item: TIconItem;
begin
  hiddenFolders := TStringList.Create;
  try
    // Collect all hidden folders...
    hiddenFolders.Sorted := true;
    for i := 0 to FIconFolders.count-1 do
    begin
      folder := FIconFolders[i].FolderName;
      if FIconFolders[i].Hidden then
        hiddenFolders.Add(AppendPathDelim(folder));
    end;

    // ... find the icons in the hidden folders and set their Hidden flag.
    for i := 0 to FIconList.Count-1 do
    begin
      item := FIconList[i];
      folder := item.Directory;
      item.Hidden := hiddenfolders.Find(folder, j);
    end;

    FilterIcons;
    Invalidate;
  finally
    hiddenFolders.Free;
  end;
end;

{ Copies the names of the stored icon folders to the given list. Hidden folders
  are marked by putting a non-nil value in the Objects of the output list. }
procedure TIconViewer.WriteIconFolders(AList: TStrings);
var
  i: Integer;
  folder: String;
  isHidden: Boolean;
begin
  for i := 0 to FIconFolders.Count-1 do
  begin
    folder := FIconFolders[i].FolderName;
    isHidden := TIconFolderList(FIconFolders).IsHidden(i);
    if isHidden then
      AList.AddObject(folder, TObject(PtrUInt(1)))
    else
      AList.Add(folder);
  end;
end;

procedure TIconViewer.WriteMetadataFiles;
{$ifdef METADATA_TXT}
var
  folder: String;
  metadata: TStringList;
  item: TIconItem;
  i, j: Integer;
  filename: String;
  style: String;
begin
  Screen.Cursor := crHourglass;
  try
    for i := 0 to FIconFolders.Count-1 do
    begin
      if not FIconFolders[i].Dirty then
        Continue;
      folder := AppendPathDelim(FIconFolders[i].FileName);
      metadata := TStringList.Create;
      try
        if FileExists(folder + METADATA_FILENAME) then
          CopyFile(folder + METADATA_FILENAME, folder + METADATA_FILENAME + '.bak');
        metadata.Add('# IconLib Metadata file, v1.0');
        metadata.Add('# File structure: "filename|width|height|style|keyword1;keyword2;..."');
        metadata.Add('# Allowed styles: classic, flat, outline, outline 2-color');
        metadata.Add('#');
        for j := 0 to FIconList.Count-1 do
        begin
          item := FIconList[j];
          filename := ExtractFileName(item.FileName);
          if ExtractFilePath(item.FileName) = folder then
          begin
            if item.Style = isAnyStyle then style := '' else style := item.StyleAsString;
            metadata.Add('%s|%d|%d|%s|%s', [
              fileName, item.Width, item.Height, style, item.KeywordsAsString]);
          end;
        end;
        metadata.SaveToFile(folder + METADATA_FILENAME);
        FIconFolders[i].Dirty := false;
      finally
        metadata.Free;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;
{$endif}
{$ifdef METADATA_XML}
var
  folder, filename: String;
  doc: TXMLDocument;
  root, iconsNode, iconNode, keywordsNode, keywordNode: TDOMNode;
  i, j, k: Integer;
  item: TIconItem;
begin
  Screen.Cursor := crHourglass;
  try
    Application.ProcessMessages;
    for i := 0 to FIconFolders.Count-1 do
    begin
      folder := AppendPathDelim(FIconFolders[i].FolderName);

      // Write only unmodified metadata
      if not FIconFolders[i].Dirty then
        Continue;

      doc := TXMLDocument.Create;
      try
        root := doc.CreateElement('metadata');
        doc.AppendChild(root);
        iconsNode := doc.CreateElement('icons');
        root.AppendChild(iconsNode);
        for j := 0 to FIconList.Count-1 do
        begin
          item := FIconList[j];
          filename := ExtractFileName(item.FileName);
          if ExtractFilePath(item.FileName) = folder then
          begin
            iconNode := doc.CreateElement('icon');
            iconsNode.AppendChild(iconNode);
            TDOMElement(iconNode).SetAttribute('filename', filename);
            TDOMElement(iconNode).SetAttribute('width', IntToStr(item.Width));
            TDOMElement(iconNode).SetAttribute('height', IntToStr(item.Height));
            if item.Style <> isAnyStyle then
              TDOMElement(iconNode).SetAttribute('style', item.StyleAsString);
            if item.KeywordCount > 0 then
            begin
              keywordsNode := doc.CreateElement('keywords');
              iconNode.AppendChild(keywordsNode);
              for k := 0 to item.KeywordCount-1 do
              begin
                keywordNode := doc.CreateElement('keyword');
                keywordsNode.AppendChild(keywordNode);
                keywordNode.AppendChild(doc.CreateTextNode(item.Keywords[k]));
              end;
            end;
          end;
        end;
        if FileExists(folder + METADATA_FILENAME) then
          RenameFile(folder + METADATA_FILENAME, folder + METADATA_FILENAME + '.bak');

        WriteXMLFile(doc, folder + METADATA_FILENAME);
        FIconFolders[i].Dirty := false;
      finally
        doc.Free;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;
{$endif}

end.

