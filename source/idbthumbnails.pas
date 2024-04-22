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
    FSelected: Boolean;
    procedure SetSelected(AValue: Boolean);
  public
    procedure DrawToCanvas(ACanvas: TCanvas; ARect: TRect); virtual;
    property Left: Integer read FLeft write FLeft;
    property Selected: Boolean read FSelected write SetSelected;
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
    FThumbnailBorderColor: TColor;     // Border of "normal" thumbnail
    FThumbnailColor: TColor;           // Background of "normal" thumbnail
    FFocusedBorderColor: TColor;       // Border of SelectedIndex
    FFocusedColor: TColor;             // Background of SelectedIndex
    FSelectedBorderColor: TColor;      // Border of thumbnail.Selected
    FSelectedIndex: Integer;           // Background of thumbnail.Selected
    FTotalHeight: Integer;
    FColCount, FRowCount: Integer;
    FMultiSelect: Boolean;
    function GetThumbnail(AIndex: Integer): TBasicThumbnail;
    function GetThumbnailCount: Integer;
    procedure SetFocusedBorderColor(AValue: TColor);
    procedure SetFocusedColor(AValue: TColor);
    procedure SetMultiSelect(AValue: Boolean);
    procedure SetSelectedBorderColor(AValue: TColor);
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
    procedure SetSelectedIndex(AValue: Integer); virtual;
    procedure SingleSelect(AThumbnail: TBasicThumbnail);
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

    property Thumbnail[AIndex: Integer]: TBasicThumbnail read GetThumbnail;
    property ThumbnailCount: Integer read GetThumbnailCount;

    property FocusedBorderColor: TColor read FFocusedBorderColor write FFocusedBorderColor default clWindowText;
    property FocusedColor: TColor read FFocusedColor write SetFocusedColor default clBtnFace;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default false;
    property SelectedBorderColor: TColor read FSelectedBorderColor write FSelectedBorderColor default clWindowText;
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

procedure TBasicThumbnail.SetSelected(AValue: Boolean);
begin
  if AValue = FSelected then
    exit;
  FSelected := AValue;
  if FSelected and (not FViewer.MultiSelect) then
    FViewer.SingleSelect(Self)
  else
    FSelected := AValue;
end;


{ TBasicThumbnailViewer }

constructor TBasicThumbnailviewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  FThumbnailList := TThumbnailList.Create;
  FThumbnailSpacing := 8;
  FThumbnailWidth := 100;
  FThumbnailHeight := 100;
  FThumbnailBorderColor := clScrollbar;
  FThumbnailColor := clWindow;

  FFocusedBorderColor := clWindowText;
  FFocusedColor := clBtnFace;
  FSelectedBorderColor := clWindowText;
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

function TBasicThumbnailViewer.GetThumbnail(AIndex: Integer): TBasicThumbnail;
begin
  Result := FThumbnailList[AIndex];
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
  thumb: TBasicThumbnail;
begin
  x := FThumbnailSpacing;
  y := FThumbnailSpacing;
  FTotalHeight := 2 * FThumbnailSpacing;
  FColCount := 0;
  FRowCount := 1;
  for i := 0 to ThumbnailCount-1 do
  begin
    thumb := FThumbnailList[i];
    thumb.Left := x;
    thumb.Top := y;
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
  Rclip: TRect;
  thumb: TBasicThumbnail;
begin
  for i := 0 to ThumbnailCount-1 do
  begin
    thumb := FThumbnailList[i];
    if ThumbnailVisible(thumb) then
    begin
      R := Rect(0, 0, FThumbnailWidth, FThumbnailHeight);
      OffsetRect(R, thumb.Left, thumb.Top);
      Rclip := R;
      InflateRect(Rclip, -1, -1);

      // Define the brush
      Canvas.Brush.Color := FThumbnailColor;
      if FThumbnailColor = clNone then
        Canvas.Brush.Style := bsClear
      else
        Canvas.Brush.Style := bsSolid;
      if i = FSelectedIndex then
        Canvas.Brush.Color := FFocusedColor;

      // Define the pen
      if i = FSelectedIndex then
      begin
        Canvas.Pen.Width := 2;
        Canvas.Pen.Color := FFocusedBorderColor;
        dec(Rclip.Right);
        dec(Rclip.Bottom);
      end else
      if thumb.Selected then
      begin
        Canvas.Pen.Width := 1;
        Canvas.Pen.Color := FSelectedBorderColor;
      end else
      if FThumbnailBorderColor = clNone then
        Canvas.Pen.Style := psClear
      else
      begin
        Canvas.Pen.Width := 1;
        Canvas.Pen.Color := ColorToRGB(FThumbnailBorderColor);
      end;

      // Draw the thumbnail background
      Canvas.Rectangle(R);

      // Make the thumbnail draw itself
      Canvas.ClipRect := Rclip;
      Canvas.Clipping := true;
      thumb.DrawToCanvas(Canvas, R);
      Canvas.Clipping := false;
    end;
  end;
end;

procedure TBasicThumbnailViewer.SetFocusedBorderColor(AValue: TColor);
begin
  if AValue = FFocusedBorderColor then
    exit;
  FFocusedBorderColor := AValue;
  Invalidate;
end;

procedure TBasicThumbnailViewer.SetFocusedColor(AValue: TColor);
begin
  if AValue = FFocusedColor then
    exit;
  FFocusedColor := AValue;
  Invalidate;
end;

procedure TBasicThumbnailViewer.SetMultiSelect(AValue: Boolean);
begin
  if AValue = FMultiSelect then
    exit;
  FMultiSelect := AValue;
  if not FMultiSelect then
    SingleSelect(FThumbnailList[FSelectedIndex]);
end;

procedure TBasicThumbnailViewer.SetSelectedBorderColor(AValue: TColor);
begin
  if AValue = FSelectedBorderColor then
    exit;
  FSelectedBorderColor := AValue;
  Invalidate;
end;

procedure TBasicThumbnailViewer.SetSelectedIndex(AValue: Integer);
begin
  if AValue = FSelectedIndex then
    exit;
  FSelectedIndex := AValue;
  if FSelectedIndex > -1 then
    SingleSelect(FThumbnailList[AValue])
  else
    SingleSelect(nil);
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

procedure TBasicThumbnailViewer.SingleSelect(AThumbnail: TBasicThumbnail);
var
  i: Integer;
  thumb: TBasicThumbnail;
begin
  for i := 0 to FThumbnailList.Count-1 do
  begin
    thumb := FThumbnailList[i];
    thumb.Selected := (AThumbnail = thumb);
  end;
  Invalidate;
end;

end.

