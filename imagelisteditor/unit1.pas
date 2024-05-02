unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ImageList1: TImageList;
    Label1: TLabel;
    ScrollBox1: TScrollBox;
    procedure Button1Click(Sender: TObject);
    procedure ScrollBox1Paint(Sender: TObject);
    procedure ScrollBox1Resize(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  ImageListEditor;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  F: TImageListEditorDlg;
begin
  F := TImageListEditorDlg.Create(nil);
  try
    F.LoadFromImageList(ImageList1);
    if F.ShowModal = mrOK then
    begin
      F.SaveToImageList;
      Scrollbox1.Invalidate;
    end;
  finally
    F.Free;
  end;
end;

procedure TForm1.ScrollBox1Paint(Sender: TObject);
const
  DIST = 6;
var
  i: Integer;
  x, y: Integer;
  w, h: Integer;
begin
  x := 0;
  y := 0;
  w := ImageList1.Width;
  h := ImageList1.Height;
  for i:=0 to ImageList1.Count-1 do begin
    ImageList1.Draw(Scrollbox1.Canvas, x, y, i);
    inc(x, w+DIST);
    if x > Scrollbox1.ClientWidth - w then begin
      x := 0;
      inc(y, h+DIST);
    end;
  end;
end;

procedure TForm1.ScrollBox1Resize(Sender: TObject);
var
  cols, rows: Integer;
  w, h: Integer;
begin
  w := ImageList1.ResolutionForPPI[ImageList1.Width, PixelsPerInch, 1.0].Width;
  h := ImageList1.ResolutionForPPI[ImageList1.Width, PixelsPerInch, 1.0].Height;
  cols := Scrollbox1.ClientWidth div w;
  rows := ImageList1.Count div cols + 2;
  Scrollbox1.VertScrollbar.Range := rows * h - Scrollbox1.ClientHeight;
  Scrollbox1.VertScrollbar.Page := h;
end;

end.

