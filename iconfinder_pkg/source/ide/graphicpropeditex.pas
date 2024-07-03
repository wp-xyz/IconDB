{
 *****************************************************************************
  This file is part of a Lazarus Package, IconLib.

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.
 *****************************************************************************

 Extended graphic property editor form allowing searching icons by keywords.
}

unit GraphicPropEditEx;
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  GraphicPropEdit, IconFinderStrConstsIDE, IconFinderFrm;

type

  { TGraphicPropertyEditorFormEx }

  TGraphicPropertyEditorFormEx = class(TGraphicPropertyEditorForm)
    IconFinderButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure IconFinderButtonClick(Sender: TObject);
  private
    FIconFinderForm: TIconFinderForm;
    procedure IconFinderDblClick(Sender: TObject);
  public
    function ShowIconFinder: Boolean;
  end;

implementation

{$R *.lfm}

{ TGraphicPropertyEditorFormEx }

procedure TGraphicPropertyEditorFormEx.IconFinderButtonClick(Sender: TObject);
begin
  if ShowIconFinder then
    FIconFinderForm.LoadPictureFromIconFinder(ImagePreview.Picture);
end;

procedure TGraphicPropertyEditorFormEx.FormCreate(Sender: TObject);
begin
  inherited;
  IconFinderButton.Caption := RSGraphPropEditor_IconFinder;
  LoadSaveBtnPanel.AutoSize := true;
end;

procedure TGraphicPropertyEditorFormEx.IconFinderDblClick(Sender: TObject);
begin
  FIconFinderForm.ModalResult := mrOK;
end;

function TGraphicPropertyEditorFormEx.ShowIconFinder: Boolean;
var
  L, T: Integer;
  R: TRect;
begin
  if FIconFinderForm = nil then
  begin
    FIconFinderForm := TIconFinderForm.Create(self);
    FIconFinderForm.OnIconDblClick := @IconFinderDblClick;
  end;

  R := Screen.DesktopRect;
  L := Left + Width;
  if L + FIconFinderForm.Width > R.Right then
  begin
    L := Left - FIconFinderForm.Width;
    if L < R.Left then
      L := Left + (Width - FIconFinderForm.Width) div 2;
  end;
  T := Top;
  FIconFinderForm.Left := L;
  FIconFinderForm.Top := T;

  FIconFinderForm.ReadSettings('GraphicPropertyEditor');

  Result := FIconFinderForm.ShowModal = mrOK;
end;

end.

