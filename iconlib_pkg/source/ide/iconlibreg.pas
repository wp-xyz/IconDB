unit IconLibReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Controls,
  // IDEIntf
  ComponentEditors, IDEOptEditorIntf,
  // IconLib
  ImageListEditorEx;

procedure Register;

implementation

uses
  IconLibSettings;

procedure Register;
begin
  //Register new component editor for TImageList
  RegisterComponentEditor(TImageList, TImageListComponentEditorEx);

  // Register options page in IDE
  IconLibOptionsFrameID := RegisterIDEOptionsEditor(IconLibOptionsGroup, TIconLibSettingsFrame, 9999)^.Index;  // AIndex = what ???
end;


end.

