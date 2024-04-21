unit idbThumbnails;

{$mode ObjFPC}{$H+}

interface

uses
  LCLIntf, LCLType, LMessages,
  Classes, SysUtils, fgl, Types,
  Graphics, Controls, Forms;

type
  TBasicThumbnailViewer = class;

  TBasicThumbnail = class(TObject)
  private
    FViewer: TBasicThumbnailViewer;
    FLeft: Integer;
    FTop: Integer;
  public
    procedure DrawToCanvas(ACanvas: TCanvas; ARect: TRect); virtual;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Viewer: TBasicThumbnailViewer read FViewer write FViewer;
  end;

  TThumbnailList = specialize TFPGObjectList<TBasicThumbnail>;

  { TBasicThumbnailviewer }

  TBasicThumbnailviewer = class(TScrollingWinControl)
  private
    FThumbnailList: TThumbnailList;
    FThumbnailSpacing: Integer;
    FThumbnailHeight: Integer;
    FThumbnailWidth: Integer;
    FThumbnailColor: TColor;
    FThumbnailBorderColor: TColor;
    FSelectedColor: TColor;
    FSelectedIndex: Integer;
    FTotalHeight: Integer;
    FColCount, FRowCount: Integer;
    function GetThumbnailCount: Integer;
    procedure SetSelectedColor(AValue: TColor);
    procedure SetSelectedIndex(AValue: Integer);
    procedure SetThumbnailBorderColor(AValue: TColor);
    procedure SetThumbnailColor(AValue: TColor);
    procedure SetThumbnailHeight(AValue: Integer);
    procedure SetThumbnailSpacing(AValue: Integer);
    procedure SetThumbnailWidth(AValue: Integer);

  protected
    procedure DoOnResize; override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    function ThumbnailVisible(AThumbnail: TBasicThumbnail): Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(AThumbnail: TBasicThumbnail); virtual;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    class function GetControlClassDefaultSize: TSize; override;
    procedure GetThumbnailColRow(X, Y: Integer; out ACol, ARow: Integer);
    function GetThumbnailIndexAt(X, Y: Integer): Integer;
    function IndexOf(AThumbnail: TBasicThumbnail): Integer;
    procedure LayoutThumbnails; virtual;

    property ThumbnailCount: Integer read GetThumbnailCount;

    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clHighlight;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex default -1;
    property ThumbnailBorderColor: TColor read FThumbnailBorderColor write SetThumbnailBorderColor default clScrollbar;
    property ThumbnailColor: TColor read FThumbnailColor write SetThumbnailColor default clWindow;
    property ThumbnailHeight: Integer read FThumbnailHeight write SetThumbnailHeight;
    property ThumbnailSpacing: Integer read FThumbnailSpacing write SetThumbnailSpacing;
    property ThumbnailWidth: Integer read FThumbnailWidth write SetThumbnailWidth;
  end;


implementation

{ TBasicThumbnail }

// Background is already painted
procedure TBasicThumbnail.DrawToCanvas(ACanvas: TCanvas; ARect: TRect);
var
  s: String;
  ts: TTextStyle;
begin
  s := IntToStr(FViewer.IndexOf(self));
  ts := ACanvas.TextStyle;
  ts.Alignment := taCenter;
  ts.Layout := tlCenter;
  ACanvas.TextRect(ARect, 0, 0, s, ts);
end;


{ TBasicThumbnailViewer }

constructor TBasicThumbnailviewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  FThumbnailList := TThumbnailList.Create;
  FThumbnailSpacing := 8;
  FThumbnailWidth := 68;
  FThumbnailHeight := 68;
  FThumbnailBorderColor := clScrollbar;
  FThumbnailColor := clWindow;

  FSelectedColor := clHighlight;
  FSelectedIndex := -1;

  HorzScrollbar.Tracking := true;
  VertScrollbar.Tracking := true;
end;

destructor TBasicThumbnailviewer.Destroy;
begin
  FThumbnailList.Free;
  inherited;
end;

procedure TBasicThumbnailviewer.Add(AThumbnail: TBasicThumbnail);
begin
  FThumbnailList.Add(AThumbnail);
  AThumbnail.Viewer := Self;
end;

procedure TBasicThumbnailViewer.Clear;
begin
  FThumbnailList.Clear;
  FRowCount := 0;
  FColCount := 0;
  FTotalHeight := 0;
  FSelectedIndex := -1;
end;

procedure TBasicThumbnailViewer.Delete(AIndex: Integer);
begin
  FThumbnailList.Delete(AIndex);
  if FSelectedIndex > -1 then
  begin
    if (AIndex < FSelectedIndex) then
      dec(FSelectedIndex);
  end;
end;

procedure TBasicThumbnailviewer.DoOnResize;
begin
  inherited;
  LayoutThumbnails;
end;

class function TBasicThumbnailviewer.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 240;
  Result.CY := 240;
end;

procedure TBasicThumbnailViewer.GetThumbnailColRow(X, Y: Integer;
  out ACol, ARow: Integer);
var
  w, h: Integer;
  col, row: Integer;
begin
  X := X + HorzScrollbar.Position;
  Y := Y + VertScrollbar.Position;
  w := FThumbnailWidth + FThumbnailSpacing;
  h := FThumbnailHeight + FThumbnailSpacing;
  ACol := X div w;
  if X mod w < FThumbnailSpacing then  // X is in spacing part
    ACol := -1;
  ARow := Y div h;
  if Y mod h < FThumbnailSpacing then  // Y is in spacing part
    ARow := -1;
end;

function TBasicThumbnailviewer.GetThumbnailCount: Integer;
begin
  Result := FThumbnailList.Count;
end;

function TBasicThumbnailViewer.GetThumbnailIndexAt(X, Y: Integer): Integer;
var
  col, row: Integer;
begin
  GetThumbnailColRow(X, Y, col, row);
  if (col >= 0) and (row >= 0) then
    Result := row * FColCount + col
  else
    Result := -1;
end;

function TBasicThumbnailviewer.IndexOf(AThumbnail: TBasicThumbnail): Integer;
begin
  Result := FThumbnailList.IndexOf(AThumbnail);
end;

procedure TBasicThumbnailviewer.LayoutThumbnails;
var
  i: Integer;
  x, y: Integer;
  thumbnail: TBasicThumbnail;
begin
  x := FThumbnailSpacing;
  y := FThumbnailSpacing;
  FTotalHeight := 2 * FThumbnailSpacing;
  FColCount := 0;
  FRowCount := 1;
  for i := 0 to ThumbnailCount-1 do
  begin
    thumbnail := FThumbnailList[i];
    thumbnail.Left := x;
    thumbnail.Top := y;
    inc(x, FThumbnailWidth + FThumbnailSpacing);
    if x + FThumbnailWidth >= ClientWidth then
    begin
      if FColCount = 0 then
        FColCount := i + 1;
      inc(FRowCount);
      inc(y, FThumbnailHeight + FThumbnailSpacing);
      x := FThumbnailSpacing;
    end;
  end;
  if x = FThumbnailSpacing then dec(FRowCount);
  FTotalHeight := y + FThumbnailHeight + FThumbnailSpacing;

  VertScrollbar.Page := ClientHeight;
  VertScrollbar.Range := FTotalHeight;
end;

// FIX ME: Is not called...
procedure TBasicThumbnailViewer.KeyUp(var Key: Word; Shift: TShiftState);
var
  idx: Integer;
begin
  inherited;
  idx := FSelectedIndex;
  case Key of
    VK_DOWN:
      if FSelectedIndex = -1 then
        SelectedIndex := 0
      else
      begin
        idx := idx + FColCount;
        if idx < ThumbnailCount then
          SelectedIndex := idx;
      end;
    VK_UP:
      if FSelectedIndex = -1 then
        SelectedIndex := 0
      else
      begin
        idx := Idx - FColCount;
        if idx >= 0 then
          SelectedIndex := idx
      end;
    VK_NEXT:
      if FSelectedIndex = -1 then
        SelectedIndex := 0
      else
      begin
        idx := idx + 1;
        if idx < ThumbnailCount then
          SelectedIndex := idx;
      end;
    VK_PRIOR:
      if FSelectedIndex = -1 then
        SelectedIndex := 0
      else
      begin
        idx := idx - 1;
        if idx >= 0 then
          SelectedIndex := idx;
      end;
  end;
end;

procedure TBasicThumbnailViewer.MouseDown(Button: TMouseButton; Shift:TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    SelectedIndex := GetThumbnailIndexAt(X, Y);
    Invalidate;
  end;
end;

procedure TBasicThumbnailviewer.Paint;
var
  i: Integer;
  R: TRect;
  thumbnail: TBasicThumbnail;
begin
  for i := 0 to ThumbnailCount-1 do
  begin
    thumbnail := FThumbnailList[i];
    if ThumbnailVisible(thumbnail) then
    begin
      R := Rect(0, 0, FThumbnailWidth, FThumbnailHeight);
      OffsetRect(R, thumbnail.Left, thumbnail.Top);

      Canvas.Brush.Color := FThumbnailColor;
      if FThumbnailColor = clNone then
        Canvas.Brush.Style := bsClear
      else
        Canvas.Brush.Style := bsSolid;
      if i = FSelectedIndex then
      begin
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := FSelectedColor;
      end;

      if FThumbnailBorderColor = clNone then
        Canvas.Pen.Style := psClear
      else
      begin
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Color := FThumbnailBorderColor;
      end;

      Canvas.Rectangle(R);
      thumbnail.DrawToCanvas(Canvas, R);
    end;
  end;
end;

procedure TBasicThumbnailViewer.SetSelectedColor(AValue: TColor);
begin
  if AValue = FSelectedColor then
    exit;
  FSelectedColor := AValue;
  Invalidate;
end;

procedure TBasicThumbnailViewer.SetSelectedIndex(AValue: Integer);
begin
  if AValue = FSelectedIndex then
    exit;
  FSelectedIndex := AValue;
  Invalidate;
end;

procedure TBasicThumbnailviewer.SetThumbnailBorderColor(AValue: TColor);
begin
  if AValue = FThumbnailBorderColor then
    exit;
  FThumbnailBorderColor := AValue;
  Invalidate;
end;

procedure TBasicThumbnailviewer.SetThumbnailColor(AValue: TColor);
begin
  if AValue = FThumbnailColor then
    exit;
  FThumbnailColor := AValue;
  Invalidate;
end;

procedure TBasicThumbnailviewer.SetThumbnailHeight(AValue: Integer);
begin
  if AValue = FThumbnailHeight then
    exit;
  FThumbnailHeight := AValue;
  LayoutThumbnails;
end;

procedure TBasicThumbnailviewer.SetThumbnailSpacing(AValue: Integer);
begin
  if AValue = FThumbnailSpacing then
    exit;
  FThumbnailSpacing := AValue;
  LayoutThumbnails;
end;

procedure TBasicThumbnailviewer.SetThumbnailWidth(AValue: Integer);
begin
  if AValue = FThumbnailWidth then
    exit;
  FThumbnailWidth := AValue;
  LayoutThumbnails;
end;

function TBasicThumbnailviewer.ThumbnailVisible(AThumbnail: TBasicThumbnail): Boolean;
var
  x: Integer;
  y: Integer;
begin
  x := AThumbnail.Left - HorzScrollbar.Position;
  y := AThumbnail.Top - VertScrollbar.Position;
  Result :=
    (x + FThumbnailWidth >= 0) and (x < ClientWidth) and
    (y + FThumbnailHeight>= 0) and (y < ClientHeight);
end;


end.

