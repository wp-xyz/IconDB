unit idbMain;

{$mode objfpc}{$H+}
{$define APP_DEBUG}

interface

uses
  Classes, SysUtils, db, dbf, FileUtil, StrUtils, IniFiles,
  Forms, Controls, Graphics, Dialogs, DBGrids, DBCtrls,
  ExtCtrls, StdCtrls, Grids, Buttons, ComCtrls, ActnList, StdActns, Types,
  {$ifdef APP_DEBUG}
  LazLogger,
  {$endif}
  idbDatamodule, idbThumbnails, idbThumbnailsDB;

type

  { TMainForm }

  TMainForm = class(TForm)
    acAddIcons: TAction;
    acFilter: TAction;
    acClearFilter: TAction;
    acEditKeywords: TAction;
    acDeleteIcon: TAction;
    acSettings: TAction;
    ActionList: TActionList;
    cmbFilterByKeywords: TComboBox;
    cmbFilterByStyle: TComboBox;
    CoolBar: TCoolBar;
    DataSource1: TDataSource;
    Dbf1: TDbf;
    DBGrid: TDBGrid;
    acFileExit: TFileExit;
    ImageList1: TImageList;
    InfoIconName: TDBText;
    Image1: TImage;
    InfoIconSize: TDBText;
    InfoIconType: TDBText;
    InfoIconHash: TDBText;
    InfoKeywords: TLabel;
    lblIconName: TLabel;
    lblIconType: TLabel;
    lblIconSize: TLabel;
    lblIconHash: TLabel;
    lblKeywords: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    FilterPanel: TPanel;
    Panel2: TPanel;
    ScrollBox: TScrollBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    StatusbarTimer: TTimer;
    pgThumbnails: TTabSheet;
    pgGrid: TTabSheet;
    ToolBar: TToolBar;
    tbAddIcons: TToolButton;
    tbEditKeywords: TToolButton;
    tbDeleteIcon: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    tbFilter: TToolButton;
    tbClearFilter: TToolButton;
    ToolButton6: TToolButton;
    tbExit: TToolButton;
    procedure acAddIconsExecute(Sender: TObject);
    procedure acClearFilterExecute(Sender: TObject);
    procedure acDeleteIconExecute(Sender: TObject);
    procedure acEditKeywordsExecute(Sender: TObject);
    procedure acFilterExecute(Sender: TObject);
    procedure acSettingsExecute(Sender: TObject);
    procedure cmbFilterByKeywordsCloseUp(Sender: TObject);
    procedure cmbFilterByKeywordsEditingDone(Sender: TObject);
    procedure cmbFilterByStyleChange(Sender: TObject);
    procedure DBGridDblClick(Sender: TObject);
    procedure DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ScrollBoxResize(Sender: TObject);
    procedure StatusbarTimerTimer(Sender: TObject);
  private
    procedure DatasetAfterDelete(ADataset: TDataset);
    procedure DatasetAfterOpen(ADataset: TDataset);
    procedure DatasetAfterPost(ADataset: TDataset);
    procedure DatasetAfterScroll(ADataset: TDataset);
    procedure ProgressHandler(Sender: TObject; AMin, AValue, AMax: Integer);

    procedure SetupDBGrid;
    procedure UpdateCaption(ARecordCount: Integer);
    procedure UpdateDBGridRowHeight(ALineCount: Integer);
    procedure UpdateIconDetails;
    procedure UpdateImage;
    procedure UpdateKeywords;

  private
    FThumbnailViewer: TDBThumbnailViewer;
    procedure PopulateThumbnails;

  private
    procedure ReadIni;
    procedure WriteIni;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  idbGlobal, idbKeywords, idbSettings, idbDuplicates;


{ TMainForm }

procedure TMainForm.acAddIconsExecute(Sender: TObject);
var
  n: Integer;
  Duplicates: TStrings;
  ok: Boolean = false;
  F: TDuplicateIconsForm;
begin
  if SelectDirectoryDialog1.Execute then
  begin
    Screen.Cursor := crHourglass;
    MainDatamodule.OnProgress := @ProgressHandler;
    Duplicates := TStringList.Create;
    try
      n := MainDatamodule.AddIconsFromDirectory(SelectDirectoryDialog1.FileName, Duplicates);
      Statusbar.SimpleText := Format('%d icons loaded.', [n]);
      StatusbarTimer.Enabled := true;
      ok := true;
    finally
      MainDatamodule.OnProgress := nil;
      Screen.Cursor := crDefault;
      if ok and (Duplicates.Count > 0) then
      begin
        F := TDuplicateIconsForm.Create(nil);
        try
          F.DuplicateList.Items.Assign(Duplicates);
          F.ShowModal;
        finally
          F.Free;
        end;
      end;
      Duplicates.Free;
      UpdateCaption(MainDatamodule.Dataset.RecordCount);
    end;
  end;

  PopulateThumbnails;
end;

procedure TMainForm.acClearFilterExecute(Sender: TObject);
begin
  cmbFilterByKeywords.Clear;
  acFilter.Checked := false;
  acFilterExecute(nil);
end;

procedure TMainForm.acDeleteIconExecute(Sender: TObject);
begin
  if MessageDlg('Do you really want to delete the selected icon from the database?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    MainDatamodule.DeleteIcon;
end;

procedure TMainForm.acEditKeywordsExecute(Sender: TObject);
var
  F: TEditKeywordsForm;
begin
  F := TEditKeywordsForm.Create(nil);
  try
    F.SetDataSource(DataSource1);
    if F.ShowModal = mrOK then
    begin
      Screen.Cursor := crHourglass;
      try
        MainDatamodule.EditKeywordsAndStyle(F.GetKeywords, F.GetStyle);
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure TMainForm.acFilterExecute(Sender: TObject);
begin
  if acFilter.Checked then
    MainDatamodule.FilterByKeywords(cmbFilterByKeywords.Text)
  else
    MainDatamodule.FilterByKeywords('');
  UpdateIconDetails;
  PopulateThumbnails;
end;

procedure TMainForm.acSettingsExecute(Sender: TObject);
var
  F: TSettingsForm;
begin
  F := TSettingsForm.Create(nil);
  try
    F.SettingsToControls;
    if F.ShowModal = mrOK then
    begin
      F.ControlsToSettings;
      MainDatamodule.ChangeDatabase(Settings.DatabaseFolder);
      UpdateDBGridRowHeight(Settings.RowLines);
    end;
  finally
    F.Free;
  end;
end;

procedure TMainForm.cmbFilterByKeywordsCloseUp(Sender: TObject);
begin
  if cmbFilterByKeywords.ItemIndex > -1 then
  begin
    cmbFilterByKeywords.Text := cmbFilterByKeywords.Items[cmbFilterByKeywords.ItemIndex];
    cmbFilterByKeywords.Items.Move(cmbFilterByKeywords.ItemIndex, 0);
    acFilter.Checked := true;
    acFilterExecute(nil);
  end;
end;

procedure TMainForm.cmbFilterByKeywordsEditingDone(Sender: TObject);
var
  idx: Integer;
begin
  idx := cmbFilterByKeywords.Items.IndexOf(cmbFilterByKeywords.Text);
  if idx = -1 then
  begin
    cmbFilterByKeywords.Items.Insert(0, cmbFilterByKeywords.Text);
    while cmbFilterByKeywords.Items.Count > MAX_FILTER_HISTORY_COUNT do
      cmbFilterByKeywords.Items.Delete(cmbFilterByKeywords.Items.Count-1);
  end else
    cmbFilterByKeywords.Items.Move(idx, 0);

  acFilter.Checked := true;
  acFilterExecute(nil);
end;

procedure TMainForm.cmbFilterByStyleChange(Sender: TObject);
begin
  MainDatamodule.FilterByStyle(cmbFilterByStyle.ItemIndex-1);
  PopulateThumbnails;
end;

procedure TMainForm.DatasetAfterDelete(ADataset: TDataset);
begin
  UpdateCaption(ADataset.RecordCount);
end;

procedure TMainForm.DatasetAfterOpen(ADataset: TDataset);
begin
  UpdateCaption(ADataset.RecordCount);
end;

procedure TMainForm.DatasetAfterScroll(ADataSet: TDataSet);
begin
  UpdateIconDetails;
end;

procedure TMainForm.DatasetAfterPost(ADataSet: TDataSet);
begin
  UpdateIconDetails;
end;

procedure TMainForm.DBGridDblClick(Sender: TObject);
begin
  acEditKeywordsExecute(nil);
end;

procedure TMainForm.DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  col: TColumn;
  stream: TStream;
  field: TField;
  pic: TPicture;
  R: TRect;
  ts: TTextStyle;
  s: String;
begin
  col := DBGrid.Columns[DataCol];
  field := col.Field;
  if field = nil then
    exit;
  R := Rect;
  if field.FieldName = 'ICON' then
  begin
    {$ifdef APP_DEBUG}
    DebugLn(['[DBGrid1DrawColumnCell] ',
      'Drawing image cell for record IconID=', Maindatamodule.IconIDField.AsInteger, ', ',
      'Name="', MainDatamodule.NameField.AsString, '"']);
    {$endif}
    stream := MainDatamodule.Dataset.CreateBlobStream(field, bmRead);
    try
      if stream.Size > 0 then
      begin
        pic := TPicture.Create;
        try
          try
            pic.LoadFromStream(stream);
            if stream.Size > 0 then
              DBGrid.Canvas.Draw(Rect.left + varCellPadding, Rect.Top + varCellPadding, pic.Graphic);
          except
            on E:Exception do MessageDlg(E.Message, mtError, [mbOK], 0);
          end;
        finally
          pic.Free;
        end;
      end;
    finally
      stream.Free;
    end;
  end else
  begin
    ts := DBGrid.Canvas.TextStyle;
    ts.Layout := tlCenter;
    ts.Alignment := col.Alignment;
    ts.EndEllipsis := true;
//    ts.Wordbreak := true;
//    ts.SingleLine := false;
    InflateRect(R, -varCellPadding, -varCellPadding);
    case field.FieldName of
      'STYLE':
        if field.IsNull then
          s := ''
        else
          s := GetStyleName(field.AsInteger);
      'KEYWORDS':
        s := StringReplace(field.AsString, KEYWORD_SEPARATOR, KEYWORD_SEPARATOR + ' ', [rfReplaceAll]);
      else
        s := field.AsString;
    end;
    DBGrid.Canvas.TextRect(R, R.Left, R.Top, s, ts);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  ReadIni;

  if MainDatamodule = nil then
    MainDatamodule := TMainDatamodule.Create(Application);
  Datasource1.Dataset := MainDatamodule.Dataset;
  MainDatamodule.AfterDelete := @DatasetAfterDelete;
  MainDatamodule.AfterOpen := @DatasetAfterOpen;
  MainDatamodule.AfterPost := @DatasetAfterPost;
  MainDatamodule.AfterScroll := @DatasetAfterScroll;
  MainDatamodule.OpenDataset;

  SetupDBGrid;
  InfoIconName.Datafield := 'NAME';
  InfoIconType.DataField := 'ICONTYPE';
  InfoIconSize.DataField := 'SIZE';
  InfoIconHash.DataField := 'ICONHASH';

  FThumbnailViewer := TDBThumbnailViewer.Create(self);
  FThumbnailViewer.Parent := pgThumbnails;  //ScrollBox;
  FThumbnailViewer.Align := alClient;;
//  FThumbnailViewer.Height := Scrollbox.Height;
  FThumbnailViewer.Dataset := MainDatamodule.Dataset;
  FThumbnailViewer.IDFieldName := 'ICONID';
  FThumbnailViewer.ImageFieldName := 'ICON';
  FThumbnailViewer.ImageNameFieldName := 'NAME';
  FThumbnailViewer.DescriptionFieldName := 'KEYWORDS';
  FThumbnailViewer.ClassificationFieldName := 'STYLE';
  FThumbnailViewer.FocusedColor := clBlack;
  FThumbnailViewer.ThumbnailWidth := 68;
  FThumbnailViewer.ThumbnailHeight := 68;
//  FThumbnailViewer.BevelOuter := bvNone;
//  FThumbnailViewer.AutoSize := true;
//  FThumbnailViewer.Color := clBlack;

  PopulateThumbnails;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WriteIni;
  FThumbnailViewer.DisableAlign;
  FThumbnailViewer.Clear;
  FThumbnailViewer.EnableAlign;
end;

procedure TMainForm.ScrollBoxResize(Sender: TObject);
begin
  FThumbnailViewer.InvalidatePreferredSize;
  FThumbnailViewer.AdjustSize;
end;

procedure TMainForm.StatusbarTimerTimer(Sender: TObject);
begin
  StatusbarTimer.Enabled := false;
  Statusbar.SimpleText := '';
end;

procedure TMainForm.PopulateThumbnails;
begin
  if FThumbnailViewer <> nil then
    FThumbnailViewer.Populate;
end;

procedure TMainForm.ProgressHandler(Sender: TObject; AMin, AValue, AMax: Integer);
begin
  if AValue mod 10 = 0 then
  begin
    Statusbar.SimpleText := Format('Loading icon %d of %d...', [AValue, AMax - AMin + 1]);
    Statusbar.Repaint;
    //Statusbar.Invalidate;
    //Application.ProcessMessages;
  end;
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
begin
  ini := CreateIni;
  try
    ReadFormFromIni(ini, Self, 'MainForm');
    ReadSettingsFromIni(ini, 'Settings');
  finally
    ini.Free;
  end;
end;

procedure TMainForm.SetupDBGrid;
begin
  DBGrid.Columns.Clear;
  DBGrid.Datasource := Datasource1;

  with DBGrid.Columns.Add do
  begin
    FieldName := 'NAME';
    Title.Caption := 'Name';
    Title.Alignment := taCenter;
    Width := 160;
  end;
  with DBGrid.Columns.Add do
  begin
    FieldName := 'NAMEBASE';
    Title.Caption := 'Name base';
    Title.Alignment := taCenter;
    Width := 140;
  end;
  with DBGrid.Columns.Add do
  begin
    FieldName := 'SIZE';
    Title.Caption := 'Size';
    Title.Alignment := taCenter;
    Alignment := taCenter;
    Width := 50;
  end;
  with DBGrid.Columns.Add do
  begin
    FieldName := 'STYLE';
    Title.Caption := 'Style';
    Title.Alignment := taCenter;
    Alignment := taLeftJustify;
    Width := 80;
  end;
  with DBGrid.Columns.Add do
  begin
    FieldName := 'KEYWORDS';
    Title.Caption := 'Keywords';
    Title.Alignment := taCenter;
    Width := 160;
  end;
  with DBGrid.Columns.Add do
  begin
    FieldName := 'ICON';
    Title.Caption := 'Image';
    Title.Alignment := taCenter;
    Width := 80;
  end;

  UpdateDBGridRowHeight(Settings.RowLines);
end;

procedure TMainForm.UpdateCaption(ARecordCount: Integer);
begin
  if ARecordCount > 0 then
    Caption := Format(APP_CAPTION_COUNT, [ARecordCount])
  else
    Caption := APP_CAPTION;
end;

procedure TMainForm.UpdateDBGridRowHeight(ALineCount: Integer);
var
  h: Integer;
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.SetSize(1, 1);
    bmp.Canvas.Font.Assign(DBGrid.Font);
    h := bmp.Canvas.TextHeight('Tg');
  finally
    bmp.Free;
  end;

  DBGrid.DefaultRowHeight := h * ALineCount + 2*varCellPadding;
end;

procedure TMainForm.UpdateIconDetails;
begin
  UpdateImage;
  UpdateKeywords;
end;

procedure TMainForm.UpdateImage;
begin
  MainDatamodule.LoadPicture(Image1.Picture);
end;

procedure TMainForm.UpdateKeywords;
var
  field: TField;
begin
  field := MainDatamodule.KeywordsField;
  if field.IsNull then
    infoKeywords.Caption := ''
  else
    infoKeywords.Caption := StringReplace(field.AsString, ';', '; ', [rfReplaceAll]);
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;
begin
  ini := CreateIni;
  try
    WriteFormToIni(ini, self, 'MainForm');
    WriteSettingsToIni(ini, 'Settings');
  finally
    ini.Free;
  end;
end;

end.

