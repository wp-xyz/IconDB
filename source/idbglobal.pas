unit idbGlobal;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  KEYWORD_SEPARATOR = ';';
  ICON_FILE_MASK = '*.png';
  MAX_FILTER_HISTORY_COUNT = 24;

type
  TSettings = record
    RowHeight: Integer;
  end;

var
  Settings: TSettings = (
    RowHeight: 1
  );

implementation

end.

