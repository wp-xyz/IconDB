unit IconLibMetaData;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, ExtCtrls, ExtDlgs,
  IconThumbnails;

type

  { TIconMetadataForm }

  TIconMetadataForm = class(TForm)
    ButtonPanel: TButtonPanel;
    cmbStyle: TComboBox;
    lblStyle: TLabel;
    mmoKeywords: TMemo;
    Image: TImage;
    lblKeywords: TLabel;
  public
    procedure ControlsToMetadata(AIcon: TIconItem);
    procedure MetadataToControls(AIcon: TIconItem);
  end;

var
  IconMetadataForm: TIconMetadataForm;

implementation

{$R *.lfm}

{ Extracts metadata from the form controls to the provided icon item. }
procedure TIconMetadataForm.ControlsToMetadata(AIcon: TIconItem);
begin
  AIcon.SetKeywordsFromStrings(mmoKeywords.Lines);
  if cmbStyle.ItemIndex = -1 then
    AIcon.StyleAsString := ''
  else
    AIcon.StyleAsString := cmbStyle.Items[cmbStyle.ItemIndex];
end;

{ Moves metadata from provided icon item to controls of the form. }
procedure TIconMetadataForm.MetadataToControls(AIcon: TIconItem);
begin
  if AIcon = nil then
    raise Exception.Create('[TMetadataForm.SetMetadata] The icon item cannot be nil.');

  AIcon.ExportKeywordsToStrings(mmoKeywords.Lines);
  mmoKeywords.SelStart := Length(mmoKeywords.Text);
  cmbStyle.ItemIndex := Integer(AIcon.Style) - 1;
  Image.Picture.Assign(AIcon.Picture);
end;

end.

