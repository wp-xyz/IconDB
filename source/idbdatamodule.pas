unit idbDatamodule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  FileUtil, Graphics, LazFileUtils, IntegerList, Dialogs, Forms, Controls,
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
    FDirectoryField: TField;
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
    FMetadataModifiedField: TField;
    FMetadataDirty: Boolean;
    FSizes: TStrings;
    FSizeField: TField;
    FStyleField: TField;
    FOnAfterDelete: TDatasetNotifyEvent;
    FOnAfterOpen: TDatasetNotifyEvent;
    FOnAfterPost: TDatasetNotifyEvent;
    FOnAfterScroll: TDatasetNotifyEvent;
    FOnProgress: TProgressEvent;
    function GetDatabaseName: String;
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
    procedure SortBy(AValue: Integer);
    procedure WriteMetadataFiles(ModifiedOnly: Boolean);

    property DatabaseName: String read GetDatabaseName;
    property Dataset: TDataset read GetDataset;
    property ImageSizes: TStrings read FSizes;
    property Keywords: TStrings read FKeywords;
    property MetadataDirty: Boolean read FMetadataDirty;

    property AfterDelete: TDatasetNotifyEvent read FOnAfterDelete write FOnAfterDelete;
    property AfterOpen: TDatasetNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property AfterPost: TDatasetNotifyEvent read FOnAfterPost write FOnAfterPost;
    property AfterScroll: TDatasetNotifyEvent read FOnAfterScroll write FOnAfterScroll;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;

    property DirectoryField: TField read FDirectoryField;
    property HeightField: TField read FHeightField;
    property IconIDField: TField read FIconIDField;
    property IconField: TField read FIconField;
    property IconTypeField: TField read FIconTypeField;
    property KeywordsField: TField read FKeywordsField;
    property MetadataModifiedField: TField read FMetadataModifiedField;
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
begin
  inherited;

  FHashes := TStringList.Create;
  TStringList(FHashes).Sorted := true;

  FSizes := TStringList.Create;
  TStringList(FSizes).Sorted := true;

  FKeywords := TStringList.Create;
  TStringList(FKeywords).Sorted := true;

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
        hash := GetIconHash(stream);

        // If the hash is stored in FHashes then the image has already been stored earlier.
        // Replace such an image.
        if FHashes.IndexOf(hash) > -1 then
        begin
          ADuplicatesList.Add(ExtractFileName(AFileName));
          Database.Edit;
        end else
          Database.Insert;

        // Add/replace the image and its field values
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
        FDirectoryField.AsString := ExtractFileDir(AFileName);
        sz := reader.ImageSize(stream);
        FWidthField.AsInteger := sz.X;
        FHeightField.AsInteger := sz.Y;
        FSizeField.AsString := Format('%d x %d', [sz.X, sz.Y]);
        FIconHashField.AsString := hash;
        FStyleField.AsInteger := AStyle;
        FKeywordsField.AsString := AKeywords;
        FMetadataModifiedField.AsInteger := 0;
        stream.Position := 0;
        TBlobField(FIconField).LoadFromStream(stream);

        // Post the record
        Database.Post;

        // Store hash value of the new image in internal list for quick access
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
  infoFile: String;

  procedure GetMetadata(AFileName: String; out AStyle: Integer; out AKeywords: String);
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
        // Style
        for j := 0 to LAST_STYLE do
          if SameText(GetStyleName(j), strArray[1]) then
          begin
            AStyle := j;
            break;
          end;
        // Keywords
        AKeywords := StringReplace(strArray[2], '; ', ';', [rfReplaceAll]);
        exit;
      end;
    end;
  end;

begin
  List := TStringList.Create;
  infos := TStringList.Create;
  infoFile := AppendPathDelim(ADirectory) + INFO_FILE_NAME;
  Database.DisableControls;
  try
    FindAllFiles(List, ADirectory, ICON_FILE_MASK, false);
    if FileExists(infoFile) then
      infos.LoadFromFile(infoFile);
    Result := 0;
    for i := 0 to List.Count-1 do
    begin
      GetMetaData(ExtractFileName(List[i]), style, keywordsOfFile);
      if AddIconFromFile(List[i], ADuplicatesList, style, keywordsOfFile) then
        inc(Result);
      DoProgress(0, i, List.Count-1);
    end;
    FillLists;
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
  Database.IndexName := 'idxName';
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
  FDirectoryfield := Dataset.FieldByName('DIRECTORY');
  FWidthField := Dataset.FieldByName('WIDTH');
  FHeightField := Dataset.FieldByName('HEIGHT');
  FKeywordsField := Dataset.FieldByName('KEYWORDS');
  FIconField := Dataset.FieldByName('ICON');
  FIconIDField := Dataset.FieldByName('ICONID');
  FIconHashField := Dataset.FieldByName('ICONHASH');
  FIconTypeField := Dataset.FieldByName('ICONTYPE');
  FSizeField := Dataset.FieldByName('SIZE');
  FStyleField := Dataset.FieldByName('STYLE');
  FMetadataModifiedField := Dataset.FieldByName('METADATAMODIFIED');

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
  ADataset.FieldDefs.Add('DIRECTORY', ftString, 255, true);
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
  ADataset.FieldDefs.Add('METADATAMODIFIED', ftInteger);
  ADataset.CreateTable;

  ADataset.Open;

  ADataset.IndexDefs.Clear;
  ADataset.AddIndex('idxIconID', 'ICONID', [ixPrimary, ixUnique]);
  ADataset.AddIndex('idxIconIDDesc', 'ICONID', [ixDescending]);
  ADataset.AddIndex('idxName', 'NAME', [ixCaseInsensitive]);
  ADataset.AddIndex('idxNameDesc', 'NAME', [ixCaseInsensitive, ixDescending]);
  ADataset.AddIndex('idxNameBase', 'NAMEBASE', [ixCaseInsensitive]);
//  ADataset.AddIndex('idxDirectory', 'DIRECTORY', [ixCaseInsensitive]);     // too long, max 100 chars allowed
  ADataset.AddIndex('idxWidth', 'WIDTH', []);
  ADataset.AddIndex('idxIconHash', 'ICONHASH', [ixCaseInsensitive]);
  ADataset.AddIndex('idxStyle', 'STYLE', []);
  ADataset.AddIndex('idxMetaMod', 'METADATAMODIFIED', []);

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
    MetadataModifiedField.AsInteger := 1;
  Database.Post;
  FMetadataDirty := true;

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
          MetadataModifiedField.AsInteger := 1;
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

function TMainDatamodule.GetDatabaseName: String;
begin
  Result := Database.FilePathFull + DBF_FILENAME;
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
    AStream.Position := 0;
    pic.LoadFromStream(AStream);
    AStream.Position := 0;
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
var
  path: String;
begin
  path := Settings.DatabaseFolder;
  if path = '' then
    path := Application.Location + 'data';

  Database.FilePath := AppendPathDelim(path);
  Database.Tablename := DBF_FILENAME;

  if not FileExists(Database.FilePath + Database.TableName) then
    CreateDataset(Database);

  Database.Open;

  Database.IndexName := 'idxMetaMod';
  FMetaDataDirty := Database.Locate(FMetadataModifiedField.FieldName, 1, []);

  Database.IndexName := 'idxName';
  // Database.IndexFieldNames := 'NAMEBASE;WIDTH' not working
  Database.Last;
  Database.First;

  FillLists;
end;

procedure TMainDatamodule.SortBy(AValue: Integer);
begin
  case AValue of
    0: Database.IndexName := 'idxName';
    1: Database.IndexName := 'idxNameDesc';
    2: Database.IndexName := 'idxNameBase';
    3: Database.IndexName := 'idxIconID';
    4: Database.IndexName := 'idxIconIDDesc';
    5: Database.IndexName := 'idxStyle';
    6: Database.IndexName := 'idxWidth';
    else raise Exception.Create('[TMainDatamodule.SortBy] Unhandled sort index.');
  end;
end;

procedure TMainDatamodule.WriteMetadataFiles(ModifiedOnly: Boolean);

  procedure CollectDirectories(AList: TStrings);
  var
    dir: String;
    infoFileName: String;
    infoFile: TStrings;
  begin
    Database.First;
    while not Database.EoF do
    begin
      dir := AppendPathDelim(DirectoryField.AsString);
      if AList.IndexOf(dir) = -1 then
      begin
        infoFileName := dir + INFO_FILE_NAME;
        infoFile := TStringList.Create;
        if FileExists(infoFileName) then
          infoFile.LoadFromFile(infoFileName)
        else
        begin
          infoFile.Add('#File structure: "filename|style|keyword1;keyword2;..."');
          infoFile.Add('#Allowed styles: classic, flat, outline, outline 2-color');
          infoFile.Add('#');
        end;
        AList.AddObject(dir, infoFile);
      end;
      Database.Next;
    end;
  end;

  function FindInfoFile(AList: TStrings; ADirectory: String): TStringList;
  var
    i: Integer;
  begin
    for i := 0 to AList.Count-1 do
    begin
      if AList[i] = ADirectory then begin
        Result := TStringList(AList.Objects[i]);
        exit;
      end;
    end;
    Result := nil;
  end;

var
  id: Integer;
  savedFilter: String;
  wasFiltered: Boolean;
  modList: TIntegerList;
  directories: TStringList;
  infoFile: TStringList;
  iconFileName: String;
  iconFileNameInInfo: String;
  iconDir: String;
  keywrds: String;
  style: String;
  s: String;
  i: Integer;
  found: Boolean;
begin
  id := IconIDField.AsInteger;
  savedFilter := Database.Filter;
  wasFiltered := Database.Filtered;
  directories := TStringList.Create;
  modList := TIntegerList.Create;
  try
    Database.Filtered := false;
    if ModifiedOnly then
    begin
      Database.Filter := 'METADATAMODIFIED = 1';
      Database.Filtered := true;
    end;

    CollectDirectories(directories);
    Database.First;
    while not Database.EoF do
    begin
      if ModifiedOnly and (FMetadataModifiedField.AsInteger = 0) then
      begin
        Database.Next;
        Continue;
      end;
      iconDir := AppendPathDelim(DirectoryField.AsString);
      infoFile := FindInfoFile(directories, iconDir);
      if infoFile = nil then
        raise Exception.Create('info.txt file not found.');
      iconFileName := NameField.AsString + IconTypeField.AsString;
      keywrds := KeywordsField.AsString;
      style := GetStyleName(StyleField.AsInteger);
      found := false;
      // Find icon's entry in 'info.txt' file and replace it
      for i := 0 to infoFile.Count-1 do
      begin
        s := infoFile[i];
        if (s = '') or (s[1] = '#') then
          Continue;
        iconFileNameInInfo := Copy(s, 1, pos('|', s)-1);
        if iconFileNameInInfo = iconFileName then
        begin
          found := true;
          infoFile[i] := Format('%s|%s|%s', [iconFileName, style, keywrds]);
          break;
        end;
      end;
      if not found then
        infoFile.Add(Format('%s|%s|%s', [iconFileName, style, keywrds]));

      // Remove "modified" flag
      if ModifiedOnly then
        ModList.Add(IconIDField.AsInteger)
      else
      begin
        if MetadataModifiedField.AsInteger = 1 then
        begin
          Database.Edit;
          MetadataModifiedField.AsInteger := 0;
          Database.Post;
        end;
      end;
      Database.Next;
    end;

    // Remove "modified" flag
    // In case of "ModifiedOnly", this must be done with an unfiltered dataset
    // because otherwise we would not catch all datasets.
    if ModifiedOnly then
    begin
      Database.Filtered := false;
      for i := 0 to modList.Count-1 do
      begin
        Dataset.Locate(IconIDField.FieldName, modList[i], []);
        Database.Edit;
        MetadataModifiedField.AsInteger := 0;
        Database.Post;
      end;
    end;

    for i := 0 to directories.Count-1 do
    begin
      infoFile := TStringList(directories.Objects[i]);
      infoFile.SaveToFile(directories[i] + INFO_FILE_NAME);
    end;

    FMetadataDirty := false;
  finally
    for i := directories.Count-1 downto 0 do
      TStrings(directories.Objects[i]).Free;
    directories.Free;
    Database.Filtered := false;
    Database.Filter := savedFilter;
    Database.Filtered := wasFiltered;
    Database.Locate(IconIDField.FieldName, id, []);
  end;
end;

end.

