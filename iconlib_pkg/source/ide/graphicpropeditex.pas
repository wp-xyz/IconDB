unit GraphicPropEditEx;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  GraphicPropEdit, IconLibFrm;

type

  { TGraphicPropertyEditorFormEx }

  TGraphicPropertyEditorFormEx = class(TGraphicPropertyEditorForm)
    IconLibButton: TButton;
    procedure IconLibButtonClick(Sender: TObject);
  private
    FIconLibForm: TIconLibForm;
    procedure IconLibDblClick(Sender: TObject);
  end;

implementation

{$R *.lfm}

{ TGraphicPropertyEditorFormEx }

procedure TGraphicPropertyEditorFormEx.IconLibButtonClick(Sender: TObject);
var
  L, T: Integer;
  R: TRect;
begin
  if FIconLibForm = nil then
  begin
    FIconLibForm := TIconLibForm.Create(self);
    FIconLibForm.OnIconDblClick := @IconLibDblClick;
  end;

  R := Screen.DesktopRect;
  L := Left + Width;
  if L + FIconLibForm.Width > R.Right then
  begin
    L := Left - FIconLibForm.Width;
    if L < R.Left then
      L := Left + (Width - FIconLibForm.Width) div 2;
  end;
  T := Top;
  FIconLibForm.Left := L;
  FIconLibForm.Top := T;

  FIconLibForm.Show;
end;

procedure TGraphicPropertyEditorFormEx.IconLibDblClick(Sender: TObject);
begin
  FIconLibForm.LoadPictureFromIconLib(ImagePreview.Picture);
  FIconLibForm.Close;
end;

end.

