{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit iconlib_dsgn_pkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  ImageListEditorEx, IconLibSettings, IconLibReg, IconLibFrm, 
  GraphPropEditsEx, GraphicPropEditEx, IconLibCommon, IconLibStrConstsIDE, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('IconLibReg', @IconLibReg.Register);
end;

initialization
  RegisterPackage('iconlib_dsgn_pkg', @Register);
end.
