program create_metadata;

uses
  Classes, SysUtils, FPImage, FPReadPNG, FPReadBMP, FileUtil, LazFileUtils;

procedure WriteHelp;
begin
  WriteLn('create_metadata - creates the metadata.txt files for the Lazarus icon library.');
  WriteLn('Syntax:   create_metadata directory');
end;

const
  METADATA_FILENAME = 'metadata.txt';
var
  files: TStrings;
  metadata: TStrings;
  folder: String;
  filename: String;
  style: String;
  keywords: String;
  width, height: Integer;
  readerClass: TFPCustomImageReaderClass;
  stream: TStream;
  i: Integer;
begin
  if ParamCount = 0 then
  begin
    WriteHelp;
    halt;
  end;

  folder := AppendPathDelim(ParamStr(1));
  if not DirectoryExists(folder) then
  begin
    WriteLn('Directory "' + folder + '" not found.');
    Halt;
  end;

  files := TStringList.Create;
  metadata := TStringList.Create;
  try
    metadata.Add('#File structure: "filename|width|height|style|keyword1;keyword2;..."');
    metadata.Add('#Allowed styles: classic, flat, outline, outline 2-color');
    metadata.Add('#');

    FindAllFiles(files, folder, '*.png;*.bmp', false);
    if files.Count = 0 then
    begin
      WriteLn('No image files found.');
      Halt;
    end;

    for i := 0 to files.Count - 1 do
    begin
      filename := ExtractFileName(files[i]);
      stream := TFileStream.Create(files[i], fmOpenRead or fmShareDenyNone);
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
      style := '';
      keywords := '';
      metadata.Add('%s|%d|%d|%s|%s', [fileName, width, height, style, keywords]);
    end;

    if FileExists(folder + METADATA_FILENAME) then
      RenameFile(folder + METADATA_FILENAME, folder + METADATA_FILENAME + '.bak');

    metadata.SaveToFile(folder + METADATA_FILENAME);
  finally
    metadata.Free;
    files.Free;
  end;
end.


