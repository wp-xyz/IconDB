{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit iconlib_dsgn_pkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  imagelisteditorex, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('imagelisteditorex', @imagelisteditorex.Register);
end;

initialization
  RegisterPackage('iconlib_dsgn_pkg', @Register);
end.
