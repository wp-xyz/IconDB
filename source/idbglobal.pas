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
  MAX_FILTER_HISTORY_COUNT = 24;

type
  TSettings = record
    DatabaseFolder: String;
    RowLines: Integer;
  end;

var
  Settings: TSettings = (
    DatabaseFolder: '';
    RowLines: 1
  );

function CreateIni: TCustomIniFile;

procedure ReadSettingsFromIni(AIniFile: TCustomIniFile; Section: String);
procedure WriteSettingsToIni(AIniFile: TCustomIniFile; Section: String);

procedure ReadFormFromIni(AIniFile: TCustomIniFile; AForm: TForm;
  ASection: String; APositionOnly: Boolean = false);
procedure WriteFormToIni(AIniFile: TCustomIniFile; AForm: TForm; ASection: string);

implementation

uses
  LCLType, LCLIntf, TypInfo;

function CreateIni: TCustomIniFile;
begin
  Result := TIniFile.Create(GetAppConfigFile(false));
end;

procedure ReadSettingsFromIni(AIniFile: TCustomIniFile; Section: String);
begin
  Settings.DatabaseFolder := AIniFile.ReadString(Section, 'DatabaseFolder', '');
  Settings.RowLines := AIniFile.ReadInteger(Section, 'RowLines', 1);
end;

procedure WriteSettingsToIni(AIniFile: TCustomIniFile; Section: String);
begin
  AIniFile.EraseSection(Section);
  AIniFile.WriteString(Section, 'DatabaseFolder', Settings.DatabaseFolder);
  AIniFile.WriteInteger(Section, 'RowLines', Settings.RowLines);
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

