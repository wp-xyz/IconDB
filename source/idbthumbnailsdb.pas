unit idbThumbnailsDB;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Graphics,
  idbThumbnails;

type
  TDBThumbnail = class(TBasicThumbnail)
  private
    FID: Integer;
    FPicture: TPicture;
    FImageName: String;
    FDescription: String;
    FClassification: Variant;
  protected
    function GetDataset: TDataset;
  public
    constructor Create(ARecordID: Integer);
    destructor Destroy; override;
    procedure DrawToCanvas(ACanvas: TCanvas; ARect: TRect); override;
    property Classification: variant read FClassification write FClassification;
    property Description: String read FDescription write FDescription;
    property ID: Integer read FID write FID;
    property ImageName: string read FImageName write FImageName;
  end;

  TDBThumbnailViewer = class(TBasicThumbnailViewer)
  private
    FDataset: TDataset;
    FIDFieldName: String;
    FImageFieldName: String;
    FImageNameFieldName: String;
    FClassificationFieldName: String;
    FDescriptionFieldName: String;
    FClassificationField: TField;
    FDescriptionField: TField;
    FIDField: TField;
    FImageField: TField;
    FImageNameField: TField;
    procedure SetClassificationFieldName(AValue: String);
    procedure SetDataset(AValue: TDataset);
    procedure SetDescriptionFieldName(AValue: String);
    procedure SetIDFieldName(AValue: String);
    procedure SetImageFieldName(AValue: String);
    procedure SetImageNameFieldName(AValue: String);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure SetSelectedIndex(AValue: Integer); override;
  public
    procedure Add(ARecordID: Integer); reintroduce;
    procedure Populate;
    property ClassificationFieldName: String read FClassificationFieldName write FClassificationFieldName;
    property Dataset: TDataset read FDataset write SetDataset;
    property DescriptionFieldName: String read FDescriptionFieldName write FDescriptionFieldName;
    property IDFieldName: String read FIDFieldName write SetIDFieldName;
    property ImageFieldName: String read FImageFieldName write SetImageFieldName;
    property ImageNameFieldName: String read FImageNameFieldName write SetImageNameFieldName;

    property ClassificationField: TField read FClassificationField;
    property DescriptionField: TField read FDescriptionField;
    property IDField: TField read FIDField;
    property ImageField: TField read FImageField;
    property ImageNameField: TField read FImageNameField;
  end;


implementation

{ TDBThumbnail }

constructor TDBThumbnail.Create(ARecordID: Integer);
begin
  inherited Create;
  FID := ARecordID;
end;

destructor TDBThumbnail.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TDBThumbnail.DrawToCanvas(ACanvas: TCanvas; ARect: TRect);
var
  x, y: Integer;
  stream: TStream;
  dataset: TDataset;
  view: TDBThumbnailViewer;
begin
  dataset := GetDataset;
  view := TDBThumbnailViewer(Viewer);

  if dataset = nil then
    exit;
  if (view.IDFieldName = '') or (view.ImageFieldName = '') then
    exit;

  if FPicture = nil then
  begin
    dataset.Locate(view.IDFieldName, FID, []);
    if (view.ImageNameField = nil) or view.ImageNameField.IsNull then
      FImageName := ''
    else
      FImageName := view.ImageNameField.AsString;
    if (view.ClassificationField = nil) then
      FClassification := varNull
    else
      FClassification := view.ClassificationField.Value;
    if view.ImageField <> nil then
    begin
      stream := dataset.CreateBlobStream(view.ImageField, bmRead);
      try
        if stream.Size <> 0 then
        begin
          stream.Position := 0;
          FPicture := TPicture.Create;
          FPicture.LoadFromStream(stream);
        end;
      finally
        stream.Free;
      end;
    end;
  end;

  if FPicture <> nil then
  begin
    x := (Viewer.ThumbnailWidth - FPicture.Width) div 2;
    y := (Viewer.ThumbnailHeight - FPicture.Height) div 2;
    ACanvas.Draw(ARect.Left + x, ARect.Top + y, FPicture.Bitmap);
  end;
end;

function TDBThumbnail.GetDataset: TDataset;
begin
  Result := TDBThumbnailViewer(Viewer).Dataset;
end;

{ TDBThumbnailViewer }

procedure TDBThumbnailViewer.Add(ARecordID: Integer);
begin
  inherited Add(TDBThumbnail.Create(ARecordID));
end;

procedure TDBThumbnailViewer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDataset) then
  begin
    FDataset := nil;
    Clear;
  end;
end;

procedure TDBThumbnailViewer.Paint;
var
  bm: TBookmark;
begin
  bm := FDataset.GetBookmark;
  try
    inherited;
  finally
    FDataset.GotoBookmark(bm);
    FDataset.FreeBookmark(bm);
  end;
end;

procedure TDBThumbnailViewer.Populate;
var
  bm: TBookmark;
begin
  Clear;

  if FDataset = nil then
  begin
    FClassificationField := nil;
    FDescriptionField := nil;
    FIDField := nil;
    FImageField := nil;
    FImageNameField := nil;
    exit;
  end;

  FClassificationField := FDataset.FindField(FClassificationFieldName);
  FDescriptionField := FDataset.FindField(FDescriptionFieldName);
  FIDField := FDataset.FindField(FIDFieldName);
  FImageField := FDataset.FindField(FImageFieldName);
  FImageNameField := FDataset.FindField(FImageNameFieldName);

  bm := FDataset.GetBookmark;
  FDataset.DisableControls;
  try
    FDataset.First;
    while not FDataset.EoF do
    begin
      Add(FIDField.AsInteger);
      FDataset.Next;
    end;
    LayoutThumbnails;
  finally
    FDataset.EnableControls;
    FDataset.GotoBookmark(bm);
    FDataset.FreeBookmark(bm);
    Invalidate;
  end;
end;

procedure TDBThumbnailViewer.SetDataset(AValue: TDataset);
begin
  if AValue = FDataset then
    exit;
  FDataset := AValue;
  if FDataset = nil then
    Clear;
  Invalidate;
end;

procedure TDBThumbnailViewer.SetClassificationFieldName(AValue: String);
begin
  if AValue = FClassificationFieldName then
    exit;
  FClassificationFieldName := AValue;
end;

procedure TDBThumbnailViewer.SetDescriptionFieldName(AValue: String);
begin
  if AValue = FDescriptionFieldName then
    exit;
  FDescriptionFieldName := AValue;
end;

procedure TDBThumbnailViewer.SetIDFieldName(AValue: String);
begin
  if AValue = FIDFieldName then
    exit;
  FIDFieldName := AValue;
end;

procedure TDBThumbnailViewer.SetImageFieldName(AValue: String);
begin
  if AValue = FImageFieldName then
    exit;
  FImageFieldName := AValue;
end;

procedure TDBThumbnailViewer.SetImageNameFieldName(AValue: String);
begin
  if AValue = FImageNameFieldName then
    exit;
  FImageNameFieldName := AValue;
end;

procedure TDBThumbnailViewer.SetSelectedIndex(AValue: Integer);
var
  thumb: TDBThumbnail;
  id: Integer;
begin
  inherited;
  if (SelectedIndex > -1) and (FDataset <> nil) then
  begin
    thumb:= Thumbnail[SelectedIndex] as TDBThumbnail;
    FDataset.Locate(FIDFieldName, thumb.ID, []);
  end;
end;

end.

