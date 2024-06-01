{
 *****************************************************************************
  This file is part of a Lazarus Package, IconLib.

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.
 *****************************************************************************

 Form with icon viewer needed by the extended graphic property and imagelist
 component editors which allow searching icons by keywords.
}

unit IconLibFrm;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazFileUtils, LazConfigStorage, LazLoggerBase,
  // LCL
  Forms, Controls, Graphics, Dialogs, ButtonPanel,
  // IDEIntf
  BaseIDEIntf, IDEImagesIntf,
  // BuildIntf
  IDEOptionsIntf,
  // Icon lib
  IconLibStrConstsIDE, IconLibCommon, IconThumbnails, IconViewer, IconLibSettings;

type
  { TIconLibForm }

  TIconLibForm = class(TForm)
    ButtonPanel: TButtonPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FViewer: TIconViewerFrame;
    FOnIconSelectClick: TNotifyEvent;
    FOnIconDblClick: TNotifyEvent;
    FSettingsNodeName: String;
  protected
    procedure AddDefaultIconFolder;
    procedure IconViewerDblClickHandler(Sender: TObject);
    procedure IconViewerFilterHandler(Sender: TObject);
    procedure IconSelectHandler(Sender: TObject);
  public
    procedure LoadPictureFromIconLib(APicture: TPicture);
    procedure LoadPictureSizesFromIconLib(ASizes: Array of TPoint; APictures: Array of TPicture);
    procedure ReadSettings(ANodeName: String);
    procedure WriteSettings;
    property OnIconDblClick: TNotifyEvent read FOnIconDblClick write FOnIconDblClick;
    property OnIconSelectClick: TNotifyEvent read FOnIconSelectClick write FOnIconSelectClick;
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
  FViewer.OnIconDblClick := @IconViewerDblClickHandler;
  FViewer.OnFilter := @IconViewerFilterHandler;
  ButtonPanel.OKButton.Caption := RSIconLibIDE_Select;
end;

procedure TIconLibForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CanClose then
    WriteSettings;
end;

procedure TIconLibForm.AddDefaultIconFolder;
var
  LazDir: String;
begin
  LazDir := AppendPathDelim(IDEEnvironmentOptions.GetParsedLazarusDirectory);
  FViewer.AddIconFolder(LazDir + DEFAULT_IMAGE_FOLDER);  // images/general_purpose/
end;

procedure TIconLibForm.IconSelectHandler(Sender: TObject);
begin
  if Assigned(FOnIconSelectClick) then
    FOnIconSelectClick(Sender);
end;

procedure TIconLibForm.IconViewerDblClickHandler(Sender: TObject);
begin
  if Assigned(FOnIconDblClick) then
    FOnIconDblClick(Sender);
end;

procedure TIconLibForm.IconViewerFilterHandler(Sender: TObject);
begin
  Caption := Format(RSIconLibIDE_Caption, [FViewer.FilteredCount, FViewer.TotalCount]);
end;

procedure TIconLibForm.LoadPictureFromIconLib(APicture: TPicture);
begin
  if FViewer.SelectedIcon <> nil then
    APicture.Assign(FViewer.SelectedIcon.Picture);
end;

procedure TIconLibForm.LoadPictureSizesFromIconLib(ASizes: Array of TPoint;
  APictures: Array of TPicture);
var
  i: Integer;
  item, largestItem: TIconItem;
  w, h: Integer;
begin
  DebugLn([Length(ASizes), ' ', Length(APictures)]);

  if FViewer.SelectedIcon <> nil then
  begin
    // Assuming ASizes are ordered by size...
    w := ASizes[High(ASizes)].X;
    h := ASizes[High(ASizes)].Y;
    largestItem := FViewer.IconViewer.FindIconSize(FViewer.SelectedIcon, w, h);
    if largestItem = nil then                   // this should not happen...
      largestItem := FViewer.IconViewer.FindLargestIcon(FViewer.SelectedIcon);

    for i := 0 to High(ASizes) do
    begin
      w := ASizes[i].X;
      h := ASizes[i].Y;
      item := FViewer.IconViewer.FindIconSize(FViewer.SelectedIcon, w, h);
      if item = nil then
        item := largestItem;
      APictures[i].Assign(item.Picture);
    end;
  end;
end;

procedure TIconLibForm.OKButtonClick(Sender: TObject);
begin
  IconSelectHandler(Sender);
end;

procedure TIconLibForm.ReadSettings(ANodeName: String);
var
  Config: TConfigStorage;
begin
  FSettingsNodeName := ANodeName;
  try
    Config := GetIDEConfigStorage(ICONLIB_CONFIG_FILENAME, true);
    try
      GlobalReadSettings(Config, FViewer, ANodeName);
    finally
      Config.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('TIconLibSettingsFrame.ReadSettings Loading ' +  ICONLIB_CONFIG_FILENAME + ' failed: ' + E.Message);
    end;
  end;
  if FViewer.IconViewer.IconFolders.Count = 0 then
    AddDefaultIconFolder;
end;

procedure TIconLibForm.WriteSettings;
var
  Config: TConfigStorage;
begin
  try
    Config := GetIDEConfigStorage(ICONLIB_CONFIG_FILENAME, true); //, false);
    try
      GlobalWriteSettings(Config, FViewer, FSettingsNodeName);
      Config.WriteToDisk;
    finally
      Config.Free;
    end;
  except
     on E: Exception do begin
       DebugLn('TIconLibSettingsFrame.ReadSettings Saving ' + ICONLIB_CONFIG_FILENAME + ' failed: ' + E.Message);
     end;
  end;
end;

end.

