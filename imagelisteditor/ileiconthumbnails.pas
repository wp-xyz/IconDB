unit ileIconThumbNails;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazFileUtils, Graphics,
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
    function GetPicture: TPicture;
    function GetSizeAsString: String;
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
    property Picture: TPicture read GetPicture;
    property SizeAsString: String read GetSizeAsString;
    property Style: TIconStyle read FStyle;
    property Width: Integer read FWidth;
  end;

  TIconList = specialize TFPGList<TIconItem>;

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
    FFilterDirty: Boolean;
    FLargestIconWidth: Integer;
    FLargestIconHeight: Integer;
    FAutoThumbnailSize: Boolean;
    procedure SetFilterByIconKeywords(AValue: String);
    procedure SetFilterByIconSize(AValue: String);
    procedure SetFilterByIconStyle(AValue: TIconStyle);

  protected
    function AcceptIcon(AIcon: TIconItem): Boolean; virtual;
    function AcceptKeywords(AIcon: TIconItem): Boolean;
    function AddIcon(AFileName, AKeywords: String; AStyle: TIconStyle; AWidth, AHeight: Integer): TIconItem;
    procedure FilterIcons;
    procedure Paint; override;
    function ReadMetadataFile(AFileName: String): Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddIconFolder(AFolder: String);
    procedure Clear; override;
    procedure GetIconSizesAsStrings(AList: TStrings);

    property AutoThumbnailSize: Boolean read FAutoThumbnailSize write FAutoThumbnailSize default true;
    property FilterByIconKeywords: string read FFilterByIconKeywords write SetFilterByIconKeywords;
    property FilterByIconSize: string read FFilterByIconSize write SetFilterByIconSize;
    property FilterByIconStyle: TIconStyle read FFilterByIconStyle write SetFilterByIconStyle;
    property LargestIconWidth: Integer read FLargestIconWidth;
    property LargestIconHeight: Integer read FLargestIconHeight;

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
  Result := Format('%dx%d', [FWidth, FHeight]);
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

function TIconViewer.AcceptKeywords(AIcon: TIconItem): Boolean;
begin
  Result := AIcon.HasKeywordPart(FFilterByIconKeywords);
end;

{ Adds the associated icon to the unfiltered icon list. }
function TIconViewer.AddIcon(AFileName, AKeywords: String; AStyle: TIconStyle;
  AWidth, AHeight: Integer): TIconItem;
var
  sizeStr: String;
begin
  Result := TIconItem.Create(AFileName, AKeywords, AStyle, AWidth, AHeight);
  FIconList.Add(Result);
  sizeStr := Format('%dx%d', [AWidth, AHeight]);
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
    FIconFolders.Add(AFolder);
  FFilterDirty := true;
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

procedure TIconViewer.GetIconSizesAsStrings(AList: TStrings);
begin
  AList.Assign(FSizes);
end;

procedure TIconViewer.Paint;
begin
  if FFilterDirty then
  begin
    FilterIcons;
    FFilterDirty := false;
  end;
  inherited;
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
    FFilterDirty := true;
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
    FFilterDirty := true;
  end;
end;

procedure TIconViewer.SetFilterByIconStyle(AValue: TIconStyle);
begin
  if FFilterByIconStyle <> AValue then
  begin
    FFilterByIconStyle := AValue;
    FFilterDirty := true;
  end;
end;

end.

