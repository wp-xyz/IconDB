unit idbDatamodule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, FileUtil, Graphics, LazFileUtils, Dialogs,
  db, dbf,
  Forms;

type

  TProgressEvent = procedure (Sender: TObject; AMin, AValue, AMax: Integer) of object;

  { TMainDatamodule }

  TMainDatamodule = class(TDataModule)
    Dbf1: TDbf;
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
    FFilterByStyle: String;
    FHashes: TStringList;
    FHeightField: TField;
    FKeywordsField: TField;
    FIconField: TField;
    FIconIDField: TField;
    FIconHashField: TField;
    FIconTypeField: TField;
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
    function AddIconFromFile(const AFileName: String; ADuplicatesList: TStrings): Boolean;
    procedure FillHashes;
    procedure FilterDataset;
    function GetFilterByKeywords(const AKeywords: String): String;
    function GetFilterByStyle(const AStyle: Integer): String;
    function GetIconHash(AStream: TStream): String;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddIconsFromDirectory(const ADirectory: String; ADuplicatesList: TStrings): Integer;
    procedure ChangeDatabase(ANewDatabase: String);
    procedure DeleteIcon;
    procedure DoProgress(AMin, AValue, AMax: Integer);
    procedure EditKeywordsAndStyle(const AKeywords: String; const AStyle: Integer);
    procedure FilterByKeywords(const AKeywords: String);
    procedure FilterByStyle(const AStyle: Integer);
    procedure LoadPicture(APicture: TPicture);
    procedure OpenDataset;

    property Dataset: TDataset read GetDataset;
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
  FHashes.Sorted := true;

  path := Settings.DatabaseFolder;
  if path = '' then
    path := Application.Location + 'data';

  Dbf1.FilePath := AppendPathDelim(path);
  Dbf1.Tablename := DBF_FILENAME;

  if not FileExists(Dbf1.FilePath + Dbf1.TableName) then
    CreateDataset(Dbf1);

  Dbf1.AfterDelete := @DoAfterDelete;
  Dbf1.AfterOpen := @DoAfterOpen;
  Dbf1.AfterScroll := @DoAfterScroll;
  Dbf1.AfterPost := @DoAfterPost;
end;

destructor TMainDatamodule.Destroy;
begin
  FHashes.Free;
  inherited;
end;

function TMainDatamodule.AddIconFromFile(const AFileName: String;
  ADuplicatesList: TStrings): Boolean;
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
        // Skip such an image.
        if FHashes.IndexOf(hash) > -1 then
        begin
          ADuplicatesList.Add(ExtractFileName(AFileName));
          Result := false;
          exit;
        end;

        // Add the new image
        Dbf1.Insert;
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
        stream.Position := 0;
        TBlobField(FIconField).LoadFromStream(stream);
        Dbf1.Post;

        // Store hash value of the new image.
        FHashes.Add(hash);
      end;
    end;
  finally
    stream.Free;
  end;
end;

function TMainDatamodule.AddIconsFromDirectory(const ADirectory: String;
  ADuplicatesList: TStrings): Integer;
var
  List: TStrings;
  i: Integer;
begin
  List := TStringList.Create;
  Dbf1.DisableControls;
  try
    FindAllFiles(List, ADirectory, ICON_FILE_MASK, false);
    Result := 0;
    for i := 0 to List.Count-1 do
    begin
      if AddIconFromFile(List[i], ADuplicatesList) then
        inc(Result);
      DoProgress(0, i, List.Count-1);
    end;
  finally
    Dbf1.EnableControls;
    List.Free;
  end;
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

  if AnsiSameText(path, Dbf1.FilePath) then
    exit;

  Dbf1.Close;
  Dbf1.FilePath := path;
  Dbf1.TableName := DBF_FILENAME;

  if not FileExists(Dbf1.FilePath + Dbf1.TableName) then
    CreateDataset(Dbf1);

  Dbf1.Open;
  Dbf1.IndexName := 'idxByName';
  Dbf1.First;
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
  Dbf1.Delete;
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
  bm: TBookmark;
  oldFilter: String;
  wasFiltered: Boolean;
begin
  // Post given keywords and style to current record
  Dbf1.Edit;
    KeywordsToField(AKeywords, FKeywordsField);
    StyleToField(AStyle, FStyleField);
  Dbf1.Post;

  // Post the same keywords to all records having the same NAMEBASE.
  iconNameBase := FNameBaseField.AsString;
  if iconNameBase <> '' then
  begin
    bm := Dbf1.GetBookmark;
    Dbf1.DisableControls;
    oldFilter := Dbf1.Filter;
    wasFiltered := Dbf1.Filtered;
    try
      Dbf1.FilterOptions := [foCaseInsensitive];
      Dbf1.Filter := Format('NAMEBASE = %s', [QuotedStr(iconNameBase + '*')]);
      Dbf1.Filtered := true;
      Dbf1.First;
      while not Dbf1.EoF do
      begin
        Dbf1.Edit;
          KeywordsToField(AKeywords, FKeywordsField);
          StyleToField(AStyle, FStylefield);
        Dbf1.Post;
        Dbf1.Next;
      end;
    finally
      Dbf1.Filter := oldFilter;
      Dbf1.Filtered := wasFiltered;
      if Dbf1.BookmarkValid(bm) then
        Dbf1.GoToBookmark(bm);
      Dbf1.FreeBookmark(bm);
      Dbf1.EnableControls;
    end;
  end;
end;

procedure TMainDatamodule.FillHashes;
var
  bm: TBookmark;
  hash: String;
begin
  FHashes.Clear;

  bm := Dbf1.GetBookmark;
  Dbf1.DisableControls;
  try
    Dbf1.First;
    while not Dbf1.EoF do
    begin
      hash := FIconHashField.AsString;
      FHashes.Add(hash);
      Dbf1.Next;
    end;
  finally
    Dbf1.GotoBookmark(bm);
    Dbf1.FreeBookmark(bm);
    Dbf1.EnableControls;
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

procedure TMainDatamodule.FilterByStyle(const AStyle: Integer);
begin
  if AStyle < 0 then
    FFilterByStyle := ''
  else
    FFilterByStyle := GetFilterByStyle(AStyle);
  FilterDataset;
end;

procedure TMainDatamodule.FilterDataset;
var
  filter: String;
begin
  if (FFilterByStyle <> '') and (FFilterByKeywords <> '') then
    filter := Format('(%s) AND (%s)', [FFilterByStyle, FFilterByKeywords])
  else
  if (FFilterByStyle <> '') then
    filter := FFilterByStyle
  else
  if (FFilterByKeywords <> '') then
    filter := FFilterByKeywords
  else
    filter := '';
  if filter <> '' then
  begin
    Dbf1.Filter := filter;
    Dbf1.Filtered := True;
  end else
  begin
    Dbf1.Filter := '';
    Dbf1.Filtered := False;
  end;
end;

function TMainDatamodule.GetDataset: TDataset;
begin
  Result := Dbf1;
end;

function TMainDatamodule.GetFilterByKeywords(const AKeywords: String): String;
var
  L: TStrings;
  s: String;
  i: Integer;
begin
  Result := '';
  L := TStringList.Create;
  try
    L.Delimiter := ';';
    L.strictDelimiter := true;
    L.DelimitedText := AKeywords;
    for i := 0 to L.Count-1 do
    begin
      if L[i] <> '' then
      begin
        s := Format('KEYWORDS = %s', [QuotedStr('*' + L[i] + '*')]);
        if Result = '' then
          Result := s
        else
          Result := Result + ' or ' + s;
      end;
    end;
  finally
    L.Free;
  end;
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
  stream := Dbf1.CreateBlobStream(FIconField, bmRead);
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
  Dbf1.Open;
//  Dbf1.IndexName := 'idxByName';
  Dbf1.IndexFieldNames := 'idxByNameBase;idxWidth';
  Dbf1.Last;
  Dbf1.First;

  FillHashes;
end;

end.

