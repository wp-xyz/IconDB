unit IconLibReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Controls, Graphics, Buttons,
  // IDEIntf
  PropEdits, ComponentEditors, IDEOptEditorIntf,
  // IconLib
  ImageListEditorEx, GraphPropEditsEx;

procedure Register;

implementation

uses
  IconLibSettings;

procedure Register;
begin
  // Register new property editors for TGraphic, TImage, button glyphs
  RegisterPropertyEditor(ClassTypeInfo(TGraphic), nil, '', TGraphicPropertyEditorEx);
  RegisterPropertyEditor(ClassTypeInfo(TPicture), nil, '', TPicturePropertyEditorEx);
  RegisterPropertyEditor(ClassTypeInfo(TBitmap), TSpeedButton, 'Glyph', TButtonGlyphPropEditorEx);
  RegisterPropertyEditor(ClassTypeInfo(TBitmap), TBitBtn, 'Glyph', TButtonGlyphPropEditorEx);

  // Register new component editor for TImageList
  RegisterComponentEditor(TImageList, TImageListComponentEditorEx);

  // Register options page in IDE
  IconLibOptionsFrameID := RegisterIDEOptionsEditor(IconLibOptionsGroup, TIconLibSettingsFrame, 9999)^.Index;  // AIndex = what ???
end;


end.

