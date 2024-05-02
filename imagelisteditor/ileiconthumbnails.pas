unit ileIconThumbNails;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, StrUtils, LazFileUtils, Graphics,
  ileBasicThumbnails;

type
  TIconItem = class;

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
  protected
    function GetKeywords(AIndex: Integer): String;
    function GetKeywordCount: Integer;
    function GetName: String;
    function GetNameBase: String;
    function GetPicture: TPicture;
    function GetSizeAsString: String;
    function GetStyleAsString: String;
  public
    constructor Create(AFileName, AKeywords: String; AStyle: TIconStyle; AWidth, AHeight: Integer);
    destructor Destroy; override;
    function HasKeyword(AKeyword: String): Boolean;
    function HasKeywordPart(AKeywordPart: String): Boolean;
    procedure KeywordsAsStrings(AList: TStrings);

    property FileName: String read FFileName;
    property Height: Integer read FHeight;
    property KeywordCount: Integer read GetKeywordCount;
    property Keywords[AIndex: Integer]: String read GetKeywords;
    property Name: String read GetName;
    property NameBase: String read GetNameBase;
    property Picture: TPicture read GetPicture;
    property SizeAsString: String read GetSizeAsString;
    property Style: TIconStyle read FStyle;
    property StyleAsString: String read GetStyleAsString;
    property Width: Integer read FWidth;
  end;

  TIconList = specialize TFPGObjectList<TIconItem>;

  TIconViewer = class(TBasicThumbnailViewer)
  private
    FIconFolders: TStrings;
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
    procedure SetFilterByIconKeywords(AValue: String);
    procedure SetFilterByIconSize(AValue: String);
    procedure SetFilterByIconStyle(AValue: TIconStyle);

  protected
    FSelectedIcon: TIconItem;
    function AcceptIcon(AIcon: TIconItem): Boolean; virtual;
    function AcceptKeywords(AIcon: TIconItem): Boolean;
    function AddIcon(AFileName, AKeywords: String; AStyle: TIconStyle; AWidth, AHeight: Integer): TIconItem;
    procedure FilterIcons;
    function ReadMetadataFile(AFileName: String): Boolean;
    procedure SetSelectedIndex(AValue: Integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddIconFolder(AFolder: String);
    procedure Clear; override;
    function FindIconSize(AIcon: TIconItem; AWidth, AHeight: Integer): TIconItem;
    function FindLargestIcon(AIcon: TIconItem): TIconItem;
    procedure GetIconSizesAsStrings(AList: TStrings);
    procedure GetKeywordsAsStrings(AList: TStrings);
    function IndexOfThumbnail(AIcon: TIconItem): Integer;

    property AutoThumbnailSize: Boolean read FAutoThumbnailSize write FAutoThumbnailSize default true;
    property FilterByIconKeywords: string read FFilterByIconKeywords write SetFilterByIconKeywords;
    property FilterByIconSize: string read FFilterByIconSize write SetFilterByIconSize;
    property FilterByIconStyle: TIconStyle read FFilterByIconStyle write SetFilterByIconStyle;
    property LargestIconWidth: Integer read FLargestIconWidth;
    property LargestIconHeight: Integer read FLargestIconHeight;
    property SelectedIcon: TIconItem read FSelectedIcon;

  end;

procedure IconStylesToStrings(AList: TStrings);
function StrToIconStyle(AText: String): TIconStyle;


implementation

const
  ICON_MARGIN = 8;  // or, more precisely: double of margin
  METADATA_FILE_NAME = 'metadata.txt';
  ICONSTYLE_NAMES: Array[TIconStyle] of String = (
    '(any style)', 'classic', 'flat', 'outline', 'outline 2-color'
  );

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
begin
  inherited Create;
  FFileName := AFileName;
  FStyle := AStyle;
  FKeywords := TStringList.Create;
  TStringList(FKeywords).Sorted := true;
  TStringList(FKeywords).CaseSensitive := false;
  TStringList(FKeywords).Duplicates := dupIgnore;
  FKeywords.Delimiter := ';';
  FKeywords.StrictDelimiter := true;
  FKeywords.DelimitedText := AKeywords;
  FWidth := AWidth;
  FHeight := AHeight;
end;

destructor TIconItem.Destroy;
begin
  FKeywords.Free;
  FreeAndNil(FPicture);
  inherited;
end;

function TIconItem.GetKeywords(AIndex: Integer): String;
begin
  Result := FKeywords[AIndex];
end;

function TIconItem.GetKeywordCount: Integer;
begin
  Result := FKeywords.Count;
end;

function TIconItem.GetName: String;
begin
  Result := ChangeFileExt(ExtractFileName(FFileName), '');
end;

function TIconItem.GetNameBase: String;
var
  p: Integer;
begin
  Result := GetName;
  p := RPos('_', Result);
  if p > 0 then
    Result := Copy(Result, 1, p-1);
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
    if pos(AKeywordPart, Lowercase(FKeywords[i])) <> 0 then
      exit;
  Result := false;
end;

procedure TIconItem.KeywordsAsStrings(AList: TStrings);
begin
  Assert(AList <> nil);
  AList.Assign(FKeywords);
end;


{ TIconViewer }

constructor TIconViewer.Create(AOwner: TComponent);
begin
  inherited;
  FIconFolders := TStringList.Create;
  FIconList := TIconList.Create;
  FSizes := TStringList.Create;
  TStringList(FSizes).Sorted := true;
  FAutoThumbnailSize := true;
end;

destructor TIconViewer.Destroy;
begin
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
begin
  Result := TIconItem.Create(AFileName, AKeywords, AStyle, AWidth, AHeight);
  FIconList.Add(Result);
  sizeStr := Format('%d x %d', [AWidth, AHeight]);
  if FSizes.IndexOf(sizeStr) = -1 then
    FSizes.Add(sizestr);
end;

procedure TIconViewer.AddIconFolder(AFolder: String);
var
  metadataFile: String;
begin
  AFolder := AppendPathDelim(AFolder);
  metadataFile := AFolder + METADATA_FILE_NAME;
  if ReadMetadataFile(metadataFile) then
  begin
    FIconFolders.Add(AFolder);
    FilterIcons;
    SelectedIndex := -1;
  end;
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

procedure TIconViewer.FilterIcons;
var
  i: Integer;
  item: TIconItem;
begin
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

{ Structure of metadata.txt lines:
    filename|width|height|style|keyword1;keyword2;...
    Allowed styles: classic, flat, outline, outline 2-color }
function TIconViewer.ReadMetadataFile(AFileName: String): Boolean;
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
  Result := false;
  if not FileExists(AFileName) then
    exit;
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
          AddIcon(fn, parts[4], style, w, h);
        end;
      end;
    end;
    Result := true;
  finally
    lines.Free;
  end;
end;

procedure TIconViewer.SetFilterByIconKeywords(AValue: String);
begin
  if FFilterByIconKeywords <> AValue then
  begin
    FFilterByIconKeywords := AValue;
    FilterIcons;
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
    FilterIcons;
  end;
end;

procedure TIconViewer.SetFilterByIconStyle(AValue: TIconStyle);
begin
  if FFilterByIconStyle <> AValue then
  begin
    FFilterByIconStyle := AValue;
    FilterIcons;
  end;
end;

procedure TIconViewer.SetSelectedIndex(AValue: Integer);
var
  thumb: TIconThumbnail;
begin
  if AValue = SelectedIndex then
    exit;

  if (AValue > -1) then
  begin
    thumb := Thumbnail[AValue] as TIconThumbnail;
    FSelectedIcon := thumb.Item;
  end else
    FSelectedIcon := nil;

  inherited;
end;

end.

