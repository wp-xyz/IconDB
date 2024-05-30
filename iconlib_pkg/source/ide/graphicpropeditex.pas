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
  public
    function ShowIconLib: Boolean;
  end;

implementation

{$R *.lfm}

{ TGraphicPropertyEditorFormEx }

procedure TGraphicPropertyEditorFormEx.IconLibButtonClick(Sender: TObject);
begin
  if ShowIconLib then
    FIconLibForm.LoadPictureFromIconLib(ImagePreview.Picture);
end;

procedure TGraphicPropertyEditorFormEx.IconLibDblClick(Sender: TObject);
begin
  FIconLibForm.ModalResult := mrOK;
end;

function TGraphicPropertyEditorFormEx.ShowIconLib: Boolean;
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

  Result := FIconLibForm.ShowModal = mrOK;
end;

end.

