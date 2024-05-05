unit IconThumbNails;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, FPImage, StrUtils,
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
    procedure CopyKeywordsAndStyleFrom(AIcon: TIconItem);
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
    FMetadataDirty: Boolean;
    function GetIconCount: Integer;
    procedure IconFolderClicked(Sender: TObject);
    procedure SetFilterByIconKeywords(AValue: String);
    procedure SetFilterByIconSize(AValue: String);
    procedure SetFilterByIconStyle(AValue: TIconStyle);

  protected
    FSelectedIcon: TIconItem;
    function AcceptIcon(AIcon: TIconItem): Boolean; virtual;
    function AcceptKeywords(AIcon: TIconItem): Boolean;
    function AddIcon(AFileName, AKeywords: String; AStyle: TIconStyle; AWidth, AHeight: Integer): TIconItem;
    procedure FilterIcons;
    procedure ReadIconDir(AFolder: String);
    procedure ReadMetadataFile(AFileName: String);
    procedure SetSelectedIndex(AValue: Integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddIconFolder(AFolder: String);
    procedure Clear; override;
    procedure CopyMetadataToNameBase(AIcon: TIconItem);
    procedure DeleteIcon(AIcon: TIconItem);
    function FindIconSize(AIcon: TIconItem; AWidth, AHeight: Integer): TIconItem;
    function FindLargestIcon(AIcon: TIconItem): TIconItem;
    procedure GetIconSizesAsStrings(AList: TStrings);
    procedure GetKeywordsAsStrings(AList: TStrings);
    function IndexOfThumbnail(AIcon: TIconItem): Integer;
    procedure PopulateIconFoldersMenu(AMenu: TMenu);
    procedure UpdateIconFolders;
    procedure WriteMetadataFiles;

    property AutoThumbnailSize: Boolean read FAutoThumbnailSize write FAutoThumbnailSize default true;
    property FilterByIconKeywords: string read FFilterByIconKeywords write SetFilterByIconKeywords;
    property FilterByIconSize: string read FFilterByIconSize write SetFilterByIconSize;
    property FilterByIconStyle: TIconStyle read FFilterByIconStyle write SetFilterByIconStyle;
    property IconCount: Integer read GetIconCount;
    property IconFolders: TStrings read FIconFolders;
    property LargestIconWidth: Integer read FLargestIconWidth;
    property LargestIconHeight: Integer read FLargestIconHeight;
    property SelectedIcon: TIconItem read FSelectedIcon;

  end;

procedure IconStylesToStrings(AList: TStrings);
function StrToIconStyle(AText: String): TIconStyle;


implementation

const
  ICON_MARGIN = 8;  // or, more precisely: double of margin
  METADATA_FILENAME = 'metadata.txt';
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

procedure TIconItem.CopyKeywordsAndStyleFrom(AIcon: TIconItem);
begin
  FKeywords.Assign(AIcon.FKeywords);
  FStyle := AIcon.FStyle;
  FViewer.FMetadataDirty := true;
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
    if pos(AKeywordPart, Lowercase(FKeywords[i])) <> 0 then
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
var
  res: Integer;
begin
  if FMetadataDirty then
  begin
    res := MessageDlg('Save modifications?', mtConfirmation, [mbYes, mbNo], 0);
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
var
  metadataFile: String;
begin
  AFolder := AppendPathDelim(AFolder);
  metadataFile := AFolder + METADATA_FILENAME;
  if FileExists(metadataFile) then
    ReadMetaDataFile(metadataFile)
  else
    ReadIconDir(AFolder);
  if FIconFolders.IndexOf(AFolder) = -1 then
    FIconFolders.Add(AFolder);
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

{ Copies the metadata from the given icon the all other icons sharing the
  same name base (= part in name before the last '_'). }
procedure TIconViewer.CopyMetadataToNameBase(AIcon: TIconItem);
var
  i: Integer;
  item: TIconItem;
  iconNameBase: String;
begin
  iconNameBase := AIcon.NameBase;
  for i := 0 to FIconList.Count-1 do
  begin
    item := FIconList[i];
    if SameText(iconNameBase, item.NameBase) and (item <> AIcon) then
      item.CopyKeywordsAndStyleFrom(AIcon);
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
    FilterIcons;
    if (selIdx <> -1) then
    begin
      if selIdx >= ThumbnailCount then selIdx := ThumbnailCount-1;
      SelectedIndex := selIdx;
    end;
  end;
end;

procedure TIconViewer.FilterIcons;
var
  i: Integer;
  item: TIconItem;
  oldNameBase: String = '';
begin
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

procedure TIconViewer.IconFolderClicked(Sender: TObject);
var
  i, idx: Integer;
  folder: String;
begin
  if TMenuItem(Sender).Tag < 0 then
  begin
    for i := 0 to FIconFolders.Count -1 do
    begin
      folder := FIconFolders[i];
      case TMenuItem(Sender).Tag of
        -1: // "Show all"
            if folder[1] = '-' then System.Delete(folder, 1, 1);
        -2: // "Hide all"
            if folder[1] <> '-' then folder := '-' + folder;
        else
            exit;
      end;
      FIconFolders[i] := folder;
    end;
  end else
  begin
    idx := TMenuItem(Sender).Tag;
    folder := FIconFolders[idx];
    if TMenuItem(Sender).Checked then
    begin
      if folder[1] = '-' then System.Delete(folder, 1, 1);
    end else
    begin
      if folder[1] <> '-' then folder := '-' + folder;
    end;
    FIconFolders[idx] := folder;
  end;

  UpdateIconFolders;
  FilterIcons;
  Invalidate;
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
    folder := FIconFolders[i];
    menuItem.Checked := folder[1] <> '-';
    if folder[1] = '-' then
      System.Delete(folder, 1, 1);
    menuItem.Caption := folder;
    menuItem.AutoCheck := true;
    menuItem.Tag := i;
    menuItem.OnClick := @IconFolderClicked;
    AMenu.Items.Add(menuItem);
  end;
end;

procedure TIconViewer.ReadIconDir(AFolder: String);
var
  files: TStrings;
  reader: TFPCustomImageReaderClass;
  stream: TStream;
  i, w, h: Integer;
begin
  files := TStringList.Create;
  try
    FindAllFiles(files, AFolder, '*.png;*.bmp', false);
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
          AddIcon(files[i], '', isAnyStyle, w, h);
        end;
      finally
        stream.Free;
      end;
    end;
  finally
    files.Free;
  end;
end;

{ Structure of metadata.txt lines:
    filename|width|height|style|keyword1;keyword2;...
    Allowed styles: classic, flat, outline, outline 2-color }
procedure TIconViewer.ReadMetadataFile(AFileName: String);
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
          AddIcon(fn, parts[4], style, w, h);
        end;
      end;
    end;
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

{ Folders and all their icons can be hidding when the folder name in the
  IconFolder list is made to begin with a '-'. This procedure iterates over
  all icons and sets their Hidden flag when their folder is hidden. }
procedure TIconViewer.UpdateIconFolders;
var
  i, j: Integer;
  hiddenFolders: TStringList;
  folder: String;
  item: TIconItem;
begin
  hiddenFolders := TStringList.Create;
  try
    hiddenFolders.Sorted := true;
    for i := 0 to FIconFolders.count-1 do
    begin
      folder := FIconFolders[i];
      if folder[1] = '-' then
      begin
        System.Delete(folder, 1,1);
        hiddenFolders.Add(AppendPathDelim(folder));
      end;
    end;

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

procedure TIconViewer.WriteMetadataFiles;
var
  folder: String;
  metadata: TStringList;
  item: TIconItem;
  i, j: Integer;
  filename: String;
begin
  Screen.Cursor := crHourglass;
  try
    for i := 0 to FIconFolders.Count-1 do
    begin
      folder := AppendPathDelim(FIconFolders[i]);
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
            metadata.Add('%s|%d|%d|%s|%s', [
              fileName, item.Width, item.Height, item.StyleAsString, item.KeywordsAsString]);
        end;
        metadata.SaveToFile(folder + METADATA_FILENAME);
        FMetadataDirty := false;
      finally
        metadata.Free;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.

