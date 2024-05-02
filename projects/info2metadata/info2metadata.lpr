program info2metadata;

uses
  SysUtils, Classes, FPImage, FPReadPNG, FPReadBMP, LazFileUtils;

procedure WriteHelp;
begin
  WriteLn('info2metadata - converts an info.txt file to a metadata.txt file.');
  WriteLn('Syntax:  info2metadata directory');
end;

const
  INFO_FILENAME = 'info.txt';
  METADATA_FILENAME = 'metadata.txt';

var
  folder: String;
  info: TStrings;
  metadata: TStrings;
  readerClass: TFPCustomImageReaderClass;
  width, height: Integer;
  parts: TStringArray;
  stream: TStream;
  i: Integer;

begin
  if ParamCount = 0 then
  begin
    WriteHelp;
    Halt;
  end;

  folder := AppendPathDelim(ParamStr(1));
  if not DirectoryExists(folder) then
  begin
    Write('Directory "', folder, '" not found.');
    Halt;
  end;

  if not FileExists(folder + INFO_FILENAME) then
  begin
    Write('info.txt not found.');
    Halt;
  end;

  info := TStringList.Create;
  metadata := TStringList.Create;
  try
    metadata.Add('#File structure: "filename|width|height|style|keyword1;keyword2;..."');
    metadata.Add('#Allowed styles: classic, flat, outline, outline 2-color');
    metadata.Add('#');

    info.LoadFromFile(folder + INFO_FILENAME);
    for i := 0 to info.Count - 1 do begin
      if (info[i] = '') or (info[i][1] = '#') then
        continue;
      parts := info[i].Split('|');
      if not FileExists(folder + parts[0]) then
      begin
        WriteLn('File "', parts[0], '" not found.');
        Continue;
      end;
      stream := TFileStream.Create(folder + parts[0], fmOpenRead + fmShareDenyNone);
      try
        readerClass := TFPCustomImage.FindReaderFromStream(stream);
        with readerClass.ImageSize(stream) do
        begin
          width := X;
          height := Y;
        end;
      finally
        stream.Free;
      end;
      metadata.Add('%s|%d|%d|%s|%s', [parts[0], width, height, parts[1], parts[2]]);
    end;

    if FileExists(folder + METADATA_FILENAME) then
      RenameFile(folder + METADATA_FILENAME, folder + METADATA_FILENAME + '.bak');

    metadata.SaveToFile(folder + METADATA_FILENAME);
  finally
    metadata.Free;
    info.Free;
  end;
end.

