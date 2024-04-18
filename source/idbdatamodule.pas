unit idbDatamodule;

{$mode ObjFPC}{$H+}

interface

uses       LazLogger,
  Classes, SysUtils, StrUtils, FileUtil, Graphics, LazFileUtils, Dialogs,
  db, dbf, BufDataset,
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
    FHashTable: TBufDataset;
    FHeightField: TField;
    FKeywordsField: TField;
    FIconField: TField;
    FIconIDField: TField;
    FIconHashField: TField;
    FIconTypeField: TField;
    FSizeField: TField;
    FOnAfterDelete: TDatasetNotifyEvent;
    FOnAfterOpen: TDatasetNotifyEvent;
    FOnAfterPost: TDatasetNotifyEvent;
    FOnAfterScroll: TDatasetNotifyEvent;
    FOnProgress: TProgressEvent;
    function GetDataset: TDataset;

  protected
    procedure CreateDataset(ADataset: TDbf);
    function CreateHashTable: TBufDataset;
    function AddIconFromFile(const AFileName: String; ADuplicatesList: TStrings): Boolean;
    procedure FillHashTable;
    function GetFilterByKeywords(const AKeywords: String): String;
    function GetIconHash(AStream: TStream): String;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddIconsFromDirectory(const ADirectory: String; ADuplicatesList: TStrings): Integer;
    procedure ChangeDatabase(ANewDatabase: String);
    procedure DeleteIcon;
    procedure DoProgress(AMin, AValue, AMax: Integer);
    procedure EditKeywords(const AKeywords: String);
    procedure FilterByKeywords(const AKeywords: String);
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
  FHashTable.Free;
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
      reader := TFPCustomImage.FindReaderFromStream(stream);
      if reader <> nil then
      begin
        stream.Position := 0;
        hash := GetIconHash(stream);
        FHashTable.First;
        if FHashTable.Locate('IconHash', hash, []) then
        begin
          ADuplicatesList.Add(ExtractFileName(AFileName));
          Result := false;
          exit;
        end;
        stream.Position := 0;
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

        FHashTable.Insert;
        FHashTable.FieldByName('IconID').AsInteger := FIconIDField.AsInteger;
        FHashTable.FieldByName('IconHash').AsString := hash;
        FHashTable.Post;
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

  ADataset.Close;
end;

function TMainDatamodule.CreateHashTable: TBufDataset;
begin
  Result := TBufDataset.Create(self);
  Result.FieldDefs.Add('IconID', ftInteger);
  Result.FieldDefs.Add('IconHash', ftString, 64);
  Result.CreateDataset;
  Result.Open;
  Result.AddIndex('idxByHash', 'IconHash', []);
end;

procedure TMainDatamodule.DeleteIcon;
var
  id: Integer;
begin
  id := FIconIDField.AsInteger;
  Dbf1.Delete;
  FHashTable.Locate('IconID', id, []);
  FHashTable.Delete;
  DoAfterDelete(Dataset);
end;

procedure TMainDatamodule.DoProgress(AMin, AValue, AMax: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(self, AMin, AValue, AMax);
end;

procedure TMainDatamodule.EditKeywords(const AKeywords: String);
var
  iconNameBase: String;
  bm: TBookmark;
  oldFilter: String;
  wasFiltered: Boolean;
begin
  // Post given keywords to current record
  Dbf1.Edit;
  if AKeywords = '' then
    FKeywordsField.Clear
  else
    FKeywordsField.AsString := AKeywords;
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
        if AKeywords = '' then
          FKeywordsField.Clear
        else
          FKeywordsField.AsString := AKeywords;
        Dbf1.Post;
        Dbf1.Next;
      end;
    finally
      Dbf1.Filter := oldFilter;
      Dbf1.Filtered := wasFiltered;
      Dbf1.GoToBookmark(bm);
      Dbf1.FreeBookmark(bm);
      Dbf1.EnableControls;
    end;
  end;
end;

procedure TMainDatamodule.FillHashTable;
var
  bm: TBookmark;
  id: Integer;
  hash: String;
begin
  FHashTable.Free;
  FHashTable := CreateHashTable;
  FHashTable.IndexName := '';

  bm := Dbf1.GetBookmark;
  Dbf1.DisableControls;
  try
    Dbf1.First;
    while not Dbf1.EoF do
    begin
      id := FIconIDField.AsInteger;
      hash := FIconHashField.AsString;
      FHashTable.Append;
      FHashTable.FieldByName('IconID').AsInteger := id;
      FHashTable.FieldByName('IconHash').AsString := hash;
      FHashTable.Post;
      Dbf1.Next;
    end;
    FHashTable.IndexName := 'idxByHash';
  finally
    Dbf1.GotoBookmark(bm);
    Dbf1.FreeBookmark(bm);
    Dbf1.EnableControls;
  end;
end;

procedure TMainDatamodule.FilterByKeywords(const AKeywords: String);
begin
  if AKeywords <> '' then
  begin
    Dbf1.Filter := GetFilterByKeywords(AKeywords);
    Dbf1.Filtered := true;
  end else
  begin
    Dbf1.Filter := '';
    Dbf1.Filtered := false;
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

  FillHashTable;
end;

end.

