unit idbDatamodule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, FileUtil, Graphics,
  db, dbf,
  Forms;

type

  TProgressEvent = procedure (Sender: TObject; AMin, AValue, AMax: Integer) of object;

  { TMainDatamodule }

  TMainDatamodule = class(TDataModule)
    Dbf1: TDbf;
    procedure DoAfterOpen(DataSet: TDataSet);
    procedure DoAfterPost(DataSet: TDataSet);
    procedure DoAfterScroll(DataSet: TDataSet);
  private
    FNameField: TField;
    FNameBaseField: TField;
    FNameSuffixField: TField;
    FWidthField: TField;
    FHeightField: TField;
    FKeywordsField: TField;
    FIconField: TField;
    FIconIDField: TField;
    FIconHashField: TField;
    FIconTypeField: TField;
    FSizeField: TField;
    FOnAfterPost: TDatasetNotifyEvent;
    FOnAfterScroll: TDatasetNotifyEvent;
    FOnProgress: TProgressEvent;
    function GetDataset: TDataset;

  protected
    procedure CreateDataset(ADataset: TDbf);
    procedure AddIconFromFile(const AFileName: String);
    function GetFilterByKeywords(const AKeywords: String): String;

  public
    constructor Create(AOwner: TComponent); override;

    function AddIconsFromDirectory(const ADirectory: String): Integer;
    procedure DeleteIcon;
    procedure DoProgress(AMin, AValue, AMax: Integer);
    procedure EditKeywords(const AKeywords: String);
    procedure FilterByKeywords(const AKeywords: String);
    procedure LoadPicture(APicture: TPicture);

    property Dataset: TDataset read GetDataset;
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
  FPImage,
  idbGlobal;

constructor TMainDatamodule.Create(AOwner: TComponent);
begin
  inherited;

  Dbf1.FilePath := Application.Location + 'data/';
  Dbf1.Tablename := 'icon_db.dbf';

  if not FileExists(Dbf1.FilePath + Dbf1.TableName) then
    CreateDataset(Dbf1);

  Dbf1.AfterOpen := @DoAfterOpen;
  Dbf1.AfterScroll := @DoAfterScroll;
  Dbf1.AfterPost := @DoAfterPost;

  Dbf1.Open;
  Dbf1.IndexName := 'idxByName';
  Dbf1.First;
end;

procedure TMainDatamodule.AddIconFromFile(const AFileName: String);
var
  stream: TMemoryStream;
  reader: TFPCustomImageReaderClass;
  s, sBase, sNumber: String;
  p, n: Integer;
  sz: TPoint;
begin
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
        stream.Position := 0;
        TBlobField(FIconField).LoadFromStream(stream);
        Dbf1.Post;
      end;
    end;
  finally
    stream.Free;
  end;
end;

function TMainDatamodule.AddIconsFromDirectory(const ADirectory: String): Integer;
var
  List: TStrings;
  i: Integer;
begin
  List := TStringList.Create;
  Dbf1.DisableControls;
  try
    FindAllFiles(List, ADirectory, ICON_FILE_MASK, false);
    Result := List.Count;
    for i := 0 to List.Count-1 do
    begin
      AddIconFromFile(List[i]);
      DoProgress(0, i, List.Count-1);
    end;
  finally
    Dbf1.EnableControls;
    List.Free;
  end;
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

  ADataset.AddIndex('idxByIconID', 'ICONID', [ixPrimary, ixUnique]);
  ADataset.AddIndex('idxByName', 'NAME', [ixCaseInsensitive]);
  ADataset.AddIndex('idxNameBase', 'NAMEBASE', [ixCaseInsensitive]);
  ADataset.AddIndex('idxByWidth', 'WIDTH', []);
  ADataset.AddIndex('idxByHeight', 'HEIGHT', []);
  ADataset.AddIndex('idxByIconHash', 'ICONHASH', [ixCaseInsensitive]);

  ADataset.Close;
end;

procedure TMainDatamodule.DeleteIcon;
begin
  Dbf1.Delete;
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

end.

