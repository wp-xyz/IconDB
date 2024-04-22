unit idbGlobal;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Forms;

const
  APP_CAPTION = 'Icon Database';
  APP_CAPTION_COUNT = 'Icon Database (%d icons)';
  DBF_FILENAME = 'icon_db.dbf';
  KEYWORD_SEPARATOR = ';';
  ICON_FILE_MASK = '*.png';
  INFO_FILE_NAME = 'info.txt';      // Contains style and keywords for each icon
  MAX_FILTER_HISTORY_COUNT = 24;

  CLASSIC_STYLE = 0;
  FLAT_STYLE = 1;
  OUTLINE_STYLE = 2;
  OUTLINE2_STYLE = 3;
  LAST_STYLE = 3;

type
  TSettings = record
    DatabaseFolder: String;
    RowLines: Integer;
    FixedThumbnailSize: Boolean;
    ThumbnailWidth: Integer;
    ThumbnailHeight: Integer;
    ThumbnailBorder: Integer;
  end;

var
  Settings: TSettings = (
    DatabaseFolder: '';
    RowLines: 1;
    FixedThumbnailSize: false;
    ThumbnailWidth: 68;
    ThumbnailHeight: 68;
    ThumbnailBorder: 4
  );

function GetStyleName(AStyle: Integer): String;

function CreateIni: TCustomIniFile;

procedure ReadSettingsFromIni(AIniFile: TCustomIniFile; Section: String);
procedure WriteSettingsToIni(AIniFile: TCustomIniFile; Section: String);

procedure ReadFormFromIni(AIniFile: TCustomIniFile; AForm: TForm;
  ASection: String; APositionOnly: Boolean = false);
procedure WriteFormToIni(AIniFile: TCustomIniFile; AForm: TForm; ASection: string);

implementation

uses
  LCLType, LCLIntf, TypInfo;

function GetStyleName(AStyle: Integer): String;
begin
  case AStyle of
    CLASSIC_STYLE  : Result := 'classic';
    FLAT_STYLE     : Result := 'flat';
    OUTLINE_STYLE  : Result := 'outline';
    OUTLINE2_STYLE : Result := 'outline 2-color';
    else             Result := IntToStr(AStyle);
  end;
end;

function CreateIni: TCustomIniFile;
begin
  Result := TIniFile.Create(GetAppConfigFile(false));
end;

procedure ReadSettingsFromIni(AIniFile: TCustomIniFile; Section: String);
begin
  Settings.DatabaseFolder := AIniFile.ReadString(Section, 'DatabaseFolder', '');
  Settings.RowLines := AIniFile.ReadInteger(Section, 'RowLines', 1);
  Settings.FixedThumbnailSize := AIniFile.ReadBool(Section, 'FixedThumbnailSize', false);
  Settings.ThumbnailWidth := AIniFile.ReadInteger(Section, 'ThumbnailWidth', 80);
  Settings.ThumbnailHeight := AIniFile.ReadInteger(Section, 'ThumbnailHeight', 80);
end;

procedure WriteSettingsToIni(AIniFile: TCustomIniFile; Section: String);
begin
  AIniFile.EraseSection(Section);
  AIniFile.WriteString(Section, 'DatabaseFolder', Settings.DatabaseFolder);
  AIniFile.WriteInteger(Section, 'RowLines', Settings.RowLines);
  AIniFile.WriteBool(Section, 'FixedThumbnailSize', Settings.FixedThumbnailSize);
  AIniFile.WriteInteger(Section, 'ThumbnailWidth', Settings.ThumbnailWidth);
  AIniFile.WriteInteger(Section, 'ThumbnailHeight', Settings.ThumbnailHeight);
end;

procedure ReadFormFromIni(AIniFile: TCustomIniFile; AForm: TForm;
  ASection: String; APositionOnly: Boolean = false);
var
  s: String;
  ws: TWindowState;
  L, T, W, H: integer;
  R: TRect;
begin
  with AIniFile do
  begin
    s := ReadString(ASection, 'WindowState', '');
    if s = '' then
      ws := AForm.WindowState
    else
      ws := TWindowState(GetEnumValue(TypeInfo(TWindowState), s));
    AForm.WindowState := ws;
    if ws = wsNormal then
    begin
      L := ReadInteger(ASection, 'Left', AForm.Left);
      T := ReadInteger(ASection, 'Top',  AForm.Top);
      if APositionOnly then
      begin
        W := AForm.Width;
        H := AForm.Height;
      end else
      begin
        W := ReadInteger(ASection, 'Width',  AForm.Width);
        H := ReadInteger(ASection, 'Height', AForm.Height);
      end;
      R := Screen.WorkAreaRect;
      if W > R.Width then W := R.Width;
      if H > R.Height then H := R.Height;
      if L < R.Left then L := R.Left;
      if T < R.Top then T := R.Top;
      if L + W > R.Right then
        L := R.Right - W - GetSystemMetrics(SM_CXSIZEFRAME);
      if T + H > R.Bottom then
        T := R.Bottom - H - GetSystemMetrics(SM_CYCAPTION) - GetSystemMetrics(SM_CYSIZEFRAME);
      AForm.SetBounds(L, T, W, H);
      AForm.Position := poDesigned;
    end;
  end;
end;

procedure WriteFormToIni(AIniFile: TCustomIniFile; AForm: TForm; ASection: string);
begin
  with AIniFile do
  begin
    EraseSection(ASection);

    WriteString(ASection, 'WindowState', GetEnumName(TypeInfo(TWindowState), integer(AForm.WindowState)));
    if AForm.WindowState = wsNormal then
    begin
      WriteInteger(ASection, 'Left',   AForm.Left);
      WriteInteger(ASection, 'Top',    AForm.Top);
      WriteInteger(ASection, 'Width',  AForm.ClientWidth);
      WriteInteger(ASection, 'Height', AForm.ClientHeight);
    end;
  end;
end;
end.

