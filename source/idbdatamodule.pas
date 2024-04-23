unit idbDatamodule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  FileUtil, Graphics, LazFileUtils, Dialogs, Forms, Controls,
  db, dbf;

type

  TProgressEvent = procedure (Sender: TObject; AMin, AValue, AMax: Integer) of object;

  { TMainDatamodule }

  TMainDatamodule = class(TDataModule)
    Database: TDbf;
    MainImages: TImageList;
    procedure DoAfterDelete(Dataset: TDataset);
    procedure DoAfterOpen(DataSet: TDataSet);
    procedure DoAfterPost(DataSet: TDataSet);
    procedure DoAfterScroll(DataSet: TDataSet);
  private
    FNameField: TField;
    FNameBaseField: TField;
    FNameSuffixField: TField;
    FWidthField: TField;
    FFilterByKeywords: String;
    FFilterBySize: String;
    FFilterByStyle: String;
    FHashes: TStrings;
    FKeywords: TStrings;
    FHeightField: TField;
    FKeywordsField: TField;
    FIconField: TField;
    FIconIDField: TField;
    FIconHashField: TField;
    FIconTypeField: TField;
    FSizes: TStrings;
    FSizeField: TField;
    FStyleField: TField;
    FOnAfterDelete: TDatasetNotifyEvent;
    FOnAfterOpen: TDatasetNotifyEvent;
    FOnAfterPost: TDatasetNotifyEvent;
    FOnAfterScroll: TDatasetNotifyEvent;
    FOnProgress: TProgressEvent;
    function GetDataset: TDataset;

  protected
    procedure CreateDataset(ADataset: TDbf);
    function AddIconFromFile(const AFileName: String;
      ADuplicatesList: TStrings; AStyle: Integer; AKeywords: String): Boolean;
    procedure FillLists;
    procedure FilterDataset;
    function GetFilterByKeywords(const AKeywords: String): String;
    function GetFilterBySize(const AWidth, AHeight: Integer): String;
    function GetFilterByStyle(const AStyle: Integer): String;
    function GetIconHash(AStream: TStream): String;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddIconsFromDirectory(const ADirectory: String; ADuplicatesList: TStrings): Integer;
    procedure AddKeywords(AKeywords: String);
    procedure AddSize(ASize: String);
    procedure ChangeDatabase(ANewDatabase: String);
    procedure DeleteIcon;
    procedure DoProgress(AMin, AValue, AMax: Integer);
    procedure EditKeywordsAndStyle(const AKeywords: String; const AStyle: Integer);
    procedure FilterByKeywords(const AKeywords: String);
    procedure FilterBySize(const AWidth, AHeight: Integer);
    procedure FilterByStyle(const AStyle: Integer);
    procedure LoadPicture(APicture: TPicture);
    procedure OpenDataset;

    property Dataset: TDataset read GetDataset;
    property ImageSizes: TStrings read FSizes;
    property Keywords: TStrings read FKeywords;
    property AfterDelete: TDatasetNotifyEvent read FOnAfterDelete write FOnAfterDelete;
    property AfterOpen: TDatasetNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property AfterPost: TDatasetNotifyEvent read FOnAfterPost write FOnAfterPost;
    property AfterScroll: TDatasetNotifyEvent read FOnAfterScroll write FOnAfterScroll;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;

    property HeightField: TField read FHeightField;
    property IconIDField: TField read FIconIDField;
    property IconField: TField read FIconField;
    property IconTypeField: TField read FIconTypeField;
    property KeywordsField: TField read FKeywordsField;
    property NameField: TField read FNameField;
    property SizeField: TField read FSizeField;
    property StyleField: TField read FStyleField;
    property WidthField: TField read FWidthField;

  end;

var
  MainDatamodule: TMainDatamodule;

implementation

{$R *.lfm}

uses
  Variants, FPImage, MD5,
  idbGlobal, idbDuplicates;

constructor TMainDatamodule.Create(AOwner: TComponent);
var
  path: String;
begin
  inherited;

  FHashes := TStringList.Create;
  TStringList(FHashes).Sorted := true;

  FSizes := TStringList.Create;
  TStringList(FSizes).Sorted := true;

  FKeywords := TStringList.Create;
  TStringList(FKeywords).Sorted := true;

  path := Settings.DatabaseFolder;
  if path = '' then
    path := Application.Location + 'data';

  Database.FilePath := AppendPathDelim(path);
  Database.Tablename := DBF_FILENAME;

  if not FileExists(Database.FilePath + Database.TableName) then
    CreateDataset(Database);

  Database.AfterDelete := @DoAfterDelete;
  Database.AfterOpen := @DoAfterOpen;
  Database.AfterScroll := @DoAfterScroll;
  Database.AfterPost := @DoAfterPost;
end;

destructor TMainDatamodule.Destroy;
begin
  FKeywords.Free;
  FSizes.Free;
  FHashes.Free;
  inherited;
end;

function TMainDatamodule.AddIconFromFile(const AFileName: String;
  ADuplicatesList: TStrings; AStyle: Integer; AKeywords: String): Boolean;
var
  stream: TMemoryStream;
  reader: TFPCustomImageReaderClass;
  s, sBase, sNumber: String;
  p, n: Integer;
  sz: TPoint;
  hash: String;
begin
  Result := true;
  stream := TMemoryStream.Create;
  try
    stream.LoadFromFile(AFileName);
    if stream.Size <> 0 then
    begin
      stream.Position := 0;
      // Check the image format (without raising an exception)
      reader := TFPCustomImage.FindReaderFromStream(stream);
      if reader <> nil then
      begin
        // Get the hash value of the image
        stream.Position := 0;
        hash := GetIconHash(stream);
        stream.Position := 0;

        // If the hash is stored in FHashes then the image has already been stored earlier.
        // Replace such an image.
        if FHashes.IndexOf(hash) > -1 then
        begin
          ADuplicatesList.Add(ExtractFileName(AFileName));
          Database.Edit
        end else
          Database.Insert;

        // Add/replace the image
        s := ExtractFileExt(AFileName);
        FIconTypeField.AsString := s;
        s := ChangeFileExt(ExtractFileName(AFileName), '');
        FNameField.AsString := s;
        FNameBaseField.AsString := s;
        FNameSuffixField.Clear;
        p := RPos('_', s);
        if p > 0 then
        begin
          sBase := Copy(s, 1, p-1);
          sNumber := Copy(s, p+1, MaxInt);
          if TryStrToInt(sNumber, n) then
          begin
            FNameSuffixField.AsInteger := n;
            FNameBaseField.AsString := sBase;
          end;
        end;
        sz := reader.ImageSize(stream);
        FWidthField.AsInteger := sz.X;
        FHeightField.AsInteger := sz.Y;
        FSizeField.AsString := Format('%d x %d', [sz.X, sz.Y]);
        FIconHashField.AsString := hash;
        FStyleField.AsInteger := AStyle;
        FKeywordsField.AsString := AKeywords;
        stream.Position := 0;
        TBlobField(FIconField).LoadFromStream(stream);
        Database.Post;

        // Store hash value of the new image.
        FHashes.Add(hash);
      end;
    end;
  finally
    stream.Free;
  end;
end;


procedure TMainDatamodule.AddKeywords(AKeywords: String);
var
  strArray: TStringArray;
  i: Integer;
  s: String;
begin
  strArray := AKeywords.Split(KEYWORD_SEPARATOR);
  for i := 0 to High(strArray) do
  begin
    s := trim(strArray[i]);
    if FKeywords.IndexOf(s) < 0 then
      FKeywords.Add(s);
  end;
end;

function TMainDatamodule.AddIconsFromDirectory(const ADirectory: String;
  ADuplicatesList: TStrings): Integer;
var
  List: TStrings;
  infos: TStrings;
  i: Integer;
  style: Integer;
  keywordsOfFile: String;

  procedure FindInfo(AFileName: String; out AStyle: Integer; out AKeywords: String);
  var
    i, j: Integer;
    strArray: TStringArray;
  begin
    AStyle := -1;
    AKeywords := '';
    for i := 0 to infos.Count-1 do
    begin
      if (infos[i] = '') or (pos('#', infos[i]) = 1) then
        continue;
      strArray := infos[i].Split('|');
      if SameText(AFileName, strArray[0]) then
      begin
        for j := 0 to LAST_STYLE do
          if SameText(GetStyleName(j), strArray[1]) then
          begin
            AStyle := j;
            break;
          end;
        AKeywords := StringReplace(strArray[2], '; ', ';', [rfReplaceAll]);
        exit;
      end;
    end;
  end;

begin
  List := TStringList.Create;
  infos := TStringList.Create;
  Database.DisableControls;
  try
    FindAllFiles(List, ADirectory, ICON_FILE_MASK, false);
    infos.LoadFromFile(AppendPathDelim(ADirectory) + INFO_FILE_NAME);
    Result := 0;
    for i := 0 to List.Count-1 do
    begin
      FindInfo(ExtractFileName(List[i]), style, keywordsOfFile);
      if AddIconFromFile(List[i], ADuplicatesList, style, keywordsOfFile) then
        inc(Result);
      DoProgress(0, i, List.Count-1);
    end;
  finally
    Database.EnableControls;
    infos.Free;
    List.Free;
  end;
end;

procedure TMainDatamodule.AddSize(ASize: String);
begin
  if FSizes.IndexOf(ASize) < 0 then
    FSizes.Add(ASize);
end;

procedure TMainDatamodule.ChangeDatabase(ANewDatabase: String);
var
  path: String;
begin
  if ANewDatabase = '' then
    path := Application.Location + 'data'
  else
    path := ANewDatabase;
  path := AppendPathDelim(path);

  if AnsiSameText(path, Database.FilePath) then
    exit;

  Database.Close;
  Database.FilePath := path;
  Database.TableName := DBF_FILENAME;

  if not FileExists(Database.FilePath + Database.TableName) then
    CreateDataset(Database);

  Database.Open;
  Database.IndexName := 'idxByName';
  Database.First;
end;

procedure TMainDatamodule.DoAfterDelete(Dataset: TDataset);
begin
  if Assigned(FOnAfterDelete) then FOnAfterDelete(Dataset);
end;

procedure TMainDatamodule.DoAfterOpen(DataSet: TDataSet);
begin
  FNameField := Dataset.FieldByName('NAME');
  FNameBaseField := Dataset.FieldByName('NAMEBASE');
  FNameSuffixField := Dataset.FieldByName('NAMESUFFIX');
  FWidthField := Dataset.FieldByName('WIDTH');
  FHeightField := Dataset.FieldByName('HEIGHT');
  FKeywordsField := Dataset.FieldByName('KEYWORDS');
  FIconField := Dataset.FieldByName('ICON');
  FIconIDField := Dataset.FieldByName('ICONID');
  FIconHashField := Dataset.FieldByName('ICONHASH');
  FIconTypeField := Dataset.FieldByName('ICONTYPE');
  FSizeField := Dataset.FieldByName('SIZE');
  FStyleField := Dataset.FieldByName('STYLE');

  if Assigned(FOnAfterOpen) then FOnAfterOpen(Dataset);
end;

procedure TMainDatamodule.DoAfterPost(DataSet: TDataSet);
begin
  if Assigned(FOnAfterPost) then
    FOnAfterPost(Dataset);
end;

procedure TMainDatamodule.DoAfterScroll(DataSet: TDataSet);
begin
  if Assigned(FOnAfterScroll) then
    FOnAfterScroll(Dataset);
end;

procedure TMainDatamodule.CreateDataset(ADataset: TDbf);
begin
  ForceDirectories(ADataset.FilePath);

  ADataset.Close;
  ADataset.Exclusive := true;
  ADataset.TableLevel := 7;

  ADataset.FieldDefs.Clear;
  ADataset.FieldDefs.Add('ICONID', ftAutoInc, 0, true);
  ADataset.FieldDefs.Add('NAME', ftString, 40, true);
  ADataset.FieldDefs.Add('NAMEBASE', ftString, 35);
  ADataset.FieldDefs.Add('NAMESUFFIX', ftInteger);
  ADataset.FieldDefs.Add('ICONHASH', ftString, 64);
  ADataset.FieldDefs.Add('ICONTYPE', ftString, 8);
  ADataset.FieldDefs.Add('WIDTH', ftInteger, 0, true);
  ADataset.FieldDefs.Add('HEIGHT', ftInteger, 0, true);
  ADataset.FieldDefs.Add('SIZE', ftString, 12);
  ADataset.FieldDefs.Add('STYLE', ftInteger);
  ADataset.FieldDefs.Add('KEYWORDS', ftString, 255);
  ADataset.FieldDefs.Add('ICON', ftBlob);
  ADataset.CreateTable;

  ADataset.Open;

  ADataset.IndexDefs.Clear;
  ADataset.AddIndex('idxByIconID', 'ICONID', [ixPrimary, ixUnique]);
  ADataset.AddIndex('idxByName', 'NAME', [ixCaseInsensitive]);
  ADataset.AddIndex('idxNameBase', 'NAMEBASE', [ixCaseInsensitive]);
  ADataset.AddIndex('idxByWidth', 'WIDTH', []);
  ADataset.AddIndex('idxByHeight', 'HEIGHT', []);
  ADataset.AddIndex('idxByIconHash', 'ICONHASH', [ixCaseInsensitive]);
  ADataset.AddIndex('idxByStyle', 'STYLE', []);

  ADataset.Close;
end;

procedure TMainDatamodule.DeleteIcon;
var
  hash: String;
  idx: Integer;
begin
  // Delete the hash value of the icon
  hash := FIconHashField.AsString;
  idx := FHashes.IndexOf(hash);
  if idx > -1 then
    FHashes.Delete(idx);

  // Delete the icon record
  Database.Delete;
  DoAfterDelete(Dataset);
end;

procedure TMainDatamodule.DoProgress(AMin, AValue, AMax: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(self, AMin, AValue, AMax);
end;

procedure TMainDatamodule.EditKeywordsAndStyle(const AKeywords: String; const AStyle: Integer);

  procedure KeywordsToField(AKeywords: String; AField: TField);
  begin
    if AKeywords = '' then
      AField.Clear
    else
      AField.AsString := AKeywords;
  end;

  procedure StyleToField(AStyle: Integer; AField: TField);
  begin
    if AStyle < 0 then
      AField.Clear
    else
      AField.AsInteger := AStyle;
  end;

var
  iconNameBase: String;
  oldFilter: String;
  wasFiltered: Boolean;
  id: Integer;
begin
  // Post given keywords and style to current record
  Database.Edit;
    KeywordsToField(AKeywords, FKeywordsField);
    StyleToField(AStyle, FStyleField);
  Database.Post;

  // Post the same keywords and style to all records having the same NAMEBASE.
  iconNameBase := FNameBaseField.AsString;
  if iconNameBase <> '' then
  begin
    id := IconIDField.AsInteger;
    Database.DisableControls;
    oldFilter := Database.Filter;
    wasFiltered := Database.Filtered;
    try
      Database.FilterOptions := [foCaseInsensitive];
      Database.Filter := Format('NAMEBASE = %s', [QuotedStr(iconNameBase + '*')]);
      Database.Filtered := true;
      Database.First;
      while not Database.EoF do
      begin
        Database.Edit;
          KeywordsToField(AKeywords, FKeywordsField);
          StyleToField(AStyle, FStylefield);
        Database.Post;
        Database.Next;
      end;
    finally
      Database.Filter := oldFilter;
      Database.Filtered := wasFiltered;
      Database.EnableControls;
      Database.Locate(IconIDField.FieldName, id, []);   // Workaround for bookmark not valid here (why?)
    end;
  end;
end;

procedure TMainDatamodule.FillLists;
var
  bm: TBookmark;
  j: Integer;
begin
  FHashes.Clear;
  FSizes.Clear;

  bm := Database.GetBookmark;
  Database.DisableControls;
  try
    Database.First;
    while not Database.EoF do
    begin
      if not FIconHashField.IsNull then
        FHashes.Add(FIconHashField.AsString);
      if not FSizeField.IsNull then
        AddSize(FSizeField.AsString);
      if not FKeywordsField.IsNull then
        AddKeywords(FKeywordsField.AsString);
      Database.Next;
    end;
  finally
    Database.GotoBookmark(bm);
    Database.FreeBookmark(bm);
    Database.EnableControls;
  end;
end;

procedure TMainDatamodule.FilterByKeywords(const AKeywords: String);
begin
  if AKeywords <> '' then
    FFilterByKeywords := GetFilterByKeywords(AKeywords)
  else
    FFilterByKeywords := '';
  FilterDataset;
end;

procedure TMainDatamodule.FilterBySize(const AWidth, AHeight: Integer);
begin
  FFilterBySize := GetFilterBySize(AWidth, AHeight);
  FilterDataset;
end;

procedure TMainDatamodule.FilterByStyle(const AStyle: Integer);
begin
  FFilterByStyle := GetFilterByStyle(AStyle);
  FilterDataset;
end;

procedure TMainDatamodule.FilterDataset;
var
  filter: String;
begin
  filter := '';
  if (FFilterBySize <> '') then
    filter := Format('%s AND (%s)', [filter, FFilterBySize]);
  if (FFilterByStyle <> '') then
    filter := Format('%s AND (%s)', [filter, FFilterByStyle]);
  if (FFilterByKeywords <> '') then
    filter := Format('%s AND (%s)', [filter, FFilterByKeywords]);
  if filter <> '' then
  begin
    Delete(filter, 1, 5);
    Database.Filter := filter;
    Database.Filtered := True;
  end else
  begin
    Database.Filter := '';
    Database.Filtered := False;
  end;
end;

function TMainDatamodule.GetDataset: TDataset;
begin
  Result := Database;
end;

function TMainDatamodule.GetFilterByKeywords(const AKeywords: String): String;
var
  L: TStrings;
  s: String;
  i: Integer;
  cmd: String = '';
begin
  Result := '';
  L := TStringList.Create;
  try
    L.Delimiter := ' ';
    L.strictDelimiter := true;
    L.DelimitedText := AKeywords;
    for i := 0 to L.Count-1 do
    begin
      if L[i] <> '' then
      begin
        if SameText(L[i], 'AND') then
          cmd := ' AND '
        else if SameText(L[i], 'OR') then
          cmd := ' OR '
        else if SameText(L[i], 'NOT') then
        begin
          if cmd = '' then
            cmd := 'AND NOT '
          else
            cmd := cmd + 'NOT ';
        end else
        begin
          s := Format('(KEYWORDS = %s)', [QuotedStr('*' + L[i] + '*')]);
          if Result = '' then
            Result := s
          else
          begin
            if cmd = '' then cmd := ' AND ';
            Result := Result + cmd + s;
          end;
          cmd := '';
        end;
      end;
    end;
  finally
    L.Free;
  end;
end;

function TMainDatamodule.GetFilterBySize(const AWidth, AHeight: Integer): String;
begin
  if (AWidth = -1) and (AHeight = -1) then
    Result := ''
  else
  if (AWidth = -2) and (AHeight = -2) then
    Result := '((WIDTH=16) OR (WIDTH=24) OR (WIDTH=32)) AND ((HEIGHT=16) OR (HEIGHT=24) OR (HEIGHT=32))'
  else
  if (AWidth = -3) and (AHeight = -3) then
    Result := '((WIDTH=24) OR (WIDTH=36) OR (WIDTH=48)) AND ((HEIGHT=24) OR (HEIGHT=36) OR (HEIGHT=48))'
  else
    Result := Format('(WIDTH = %d) AND (HEIGHT = %d)', [AWidth, AHeight]);
end;

function TMainDatamodule.GetFilterByStyle(const AStyle: Integer): String;
begin
  if AStyle = -1 then
    Result := ''
  else
    Result := 'STYLE = ' + IntToStr(AStyle);
end;

function TMainDatamodule.GetIconHash(AStream: TStream): String;
var
  pic: TPicture;
  md5: TMDContext;
  md5Digest: TMDDigest;
  buf: PByte;
begin
  pic := TPicture.Create;
  try
    pic.LoadFromStream(AStream);
    buf := pic.Bitmap.RawImage.Data;
    MDInit(md5, MD_VERSION_5);
    MDUpdate(md5, buf^, pic.Bitmap.RawImage.DataSize);
    MDFinal(md5, md5Digest);
    Result := Lowercase(MDPrint(md5Digest));
  finally
    pic.Free;
  end;
end;

procedure TMainDatamodule.LoadPicture(APicture: TPicture);
var
  stream: TStream;
begin
  stream := Database.CreateBlobStream(FIconField, bmRead);
  try
    if stream.Size = 0 then
    begin
      APicture.Clear;
      exit;
    end;
    stream.Position := 0;
    APicture.LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TMainDatamodule.OpenDataset;
begin
  Database.Open;
//  Database.IndexName := 'idxByName';
  Database.IndexFieldNames := 'idxByNameBase;idxWidth';
  Database.Last;
  Database.First;

  FillLists;
end;

end.

