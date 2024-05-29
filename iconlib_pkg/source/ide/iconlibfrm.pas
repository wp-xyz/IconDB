unit IconLibFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazFileUtils, LazConfigStorage, LazLoggerBase,
  // LCL
  Forms, Controls, Graphics, Dialogs,
  // IDEIntf
  BaseIDEIntf, IDEOptionsIntf, IDEImagesIntf,
  // Icon lib
  IconThumbnails, IconViewer;

const
  ICONLIB_CONFIG_FILENAME = 'iconlibcfg.xml';

type

  { TIconLibForm }

  TIconLibForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FViewer: TIconViewerFrame;
    FOnIconDblClick: TNotifyEvent;
  protected
    procedure AddDefaultIconFolder;
    procedure AddIconFolders;
    procedure IconViewerDblClick(Sender: TObject);
    procedure IconViewerFilter(Sender: TObject);
  public
    procedure LoadPictureFromIconLib(APicture: TPicture);
    property OnIconDblClick: TNotifyEvent read FOnIconDblClick write FOnIconDblClick;
  end;

implementation

{$R *.lfm}

{ TIconLibForm }

procedure TIconLibForm.FormCreate(Sender: TObject);
begin
  FViewer := TIconViewerFrame.Create(self);
  FViewer.Align := alClient;
  FViewer.Parent := self;
  FViewer.IconViewer.FocusedColor := clWindowText;
  FViewer.IconViewer.ThumbnailColor := clWindow;
  FViewer.ImageList := IDEImages.Images_16;
  FViewer.ImageIndex_ExecuteFilter := IDEImages.GetImageIndex('item_filter', 16);
  FViewer.ImageIndex_ClearFilter := IDEImages.GetImageIndex('menu_clean', 16);
  FViewer.BorderSpacing.Top := 6;
  FViewer.OnIconDblClick := @IconViewerDblClick;
  FViewer.OnFilter := @IconViewerFilter;
  AddIconFolders;
end;

procedure TIconLibForm.AddDefaultIconFolder;
var
  LazDir: String;
begin
  LazDir := AppendPathDelim(IDEEnvironmentOptions.GetParsedLazarusDirectory);
  FViewer.AddIconFolder(LazDir + 'images/general_purpose/');
end;

procedure TIconLibForm.AddIconFolders;
var
  Config: TConfigStorage;
  folder: String;
  isHidden: Boolean;
  n, i: Integer;
begin
  try
    Config := GetIDEConfigStorage(ICONLIB_CONFIG_FILENAME, true);
    try
      n := Config.GetValue('IconLib/Folders/Count', 0);
      if n = 0 then
        AddDefaultIconFolder
      else
        for i := 0 to n-1 do
        begin
          folder := Config.GetValue('IconLib/Folders/Item' + IntToStr(i) + '/Value', '');
          isHidden := Config.GetValue('IconLib/Folders/Item' + IntToStr(i) + '/Hidden', false);
          if (folder <> '') and DirectoryExists(folder) then
            FViewer.AddIconFolder(folder, isHidden);
        end;
    finally
      Config.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('TIconLibSettingsFrame.ReadSettings Loading ' +  ICONLIB_CONFIG_FILENAME + ' failed: ' + E.Message);
    end;
  end;
end;

procedure TIconLibForm.IconViewerDblClick(Sender: TObject);
begin
  if Assigned(FOnIconDblClick) then
    FOnIconDblClick(Sender);
end;

procedure TIconLibForm.IconViewerFilter(Sender: TObject);
begin
  Caption := Format('Icon Library (%d out of %d icons)', [FViewer.FilteredCount, FViewer.TotalCount]);
end;

procedure TIconLibForm.LoadPictureFromIconLib(APicture: TPicture);
begin
  if FViewer.SelectedIcon <> nil then
    APicture.Assign(FViewer.SelectedIcon.Picture);
end;

end.

