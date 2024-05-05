unit ilUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

function CreateIniFile: TCustomIniFile;

implementation

function CreateIniFile: TCustomIniFile;
var
  fn: String;
begin
  fn := ChangeFileExt(GetAppConfigFile(false), '.ini');
  Result := TIniFile.Create(fn);
end;

end.

