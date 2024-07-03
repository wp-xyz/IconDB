{
 *****************************************************************************
  This file is part of a Lazarus Package, IconFinder.

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.
 *****************************************************************************

 Form with icon viewer needed by the extended graphic property and imagelist
 component editors which allow searching icons by keywords.
}

unit IconFinderFrm;
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
  // Icon Finder
  IconFinderStrConstsIDE, IconFinderCommon, IconThumbnails, IconViewer, IconFinderSettings;

type
  { TIconFinderForm }

  TIconFinderForm = class(TForm)
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
    procedure LoadPictureFromIconFinder(APicture: TPicture);
    procedure LoadPictureSizesFromIconFinder(ASizes: Array of TPoint; APictures: Array of TPicture);
    procedure ReadSettings(ANodeName: String);
    procedure WriteSettings;
    property OnIconDblClick: TNotifyEvent read FOnIconDblClick write FOnIconDblClick;
    property OnIconSelectClick: TNotifyEvent read FOnIconSelectClick write FOnIconSelectClick;
  end;

implementation

{$R *.lfm}

{ TIconFinderForm }

procedure TIconFinderForm.FormCreate(Sender: TObject);
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
  ButtonPanel.OKButton.Caption := RSIconFinderIDE_Select;
end;

procedure TIconFinderForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CanClose then
    WriteSettings;
end;

procedure TIconFinderForm.AddDefaultIconFolder;
var
  LazDir: String;
begin
  LazDir := AppendPathDelim(IDEEnvironmentOptions.GetParsedLazarusDirectory);
  FViewer.AddIconFolder(LazDir + DEFAULT_IMAGE_FOLDER);  // images/general_purpose/
end;

procedure TIconFinderForm.IconSelectHandler(Sender: TObject);
begin
  if Assigned(FOnIconSelectClick) then
    FOnIconSelectClick(Sender);
end;

procedure TIconFinderForm.IconViewerDblClickHandler(Sender: TObject);
begin
  if Assigned(FOnIconDblClick) then
    FOnIconDblClick(Sender);
end;

procedure TIconFinderForm.IconViewerFilterHandler(Sender: TObject);
begin
  Caption := Format(RSIconFinderIDE_Caption, [FViewer.FilteredCount, FViewer.TotalCount]);
end;

procedure TIconFinderForm.LoadPictureFromIconFinder(APicture: TPicture);
begin
  if FViewer.SelectedIcon <> nil then
    APicture.Assign(FViewer.SelectedIcon.Picture);
end;

procedure TIconFinderForm.LoadPictureSizesFromIconFinder(ASizes: Array of TPoint;
  APictures: Array of TPicture);
var
  i: Integer;
  item, largestItem: TIconItem;
  w, h: Integer;
begin
  //DebugLn([Length(ASizes), ' ', Length(APictures)]);

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

procedure TIconFinderForm.OKButtonClick(Sender: TObject);
begin
  IconSelectHandler(Sender);
end;

procedure TIconFinderForm.ReadSettings(ANodeName: String);
var
  Config: TConfigStorage;
begin
  FSettingsNodeName := ANodeName;
  try
    Config := GetIDEConfigStorage(ICONFINDER_CONFIG_FILENAME, true);
    try
      GlobalReadSettings(Config, FViewer, ANodeName);
    finally
      Config.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('TIconFinderSettingsFrame.ReadSettings Loading ' +  ICONFINDER_CONFIG_FILENAME + ' failed: ' + E.Message);
    end;
  end;
  if FViewer.IconViewer.IconFolders.Count = 0 then
    AddDefaultIconFolder;
end;

procedure TIconFinderForm.WriteSettings;
var
  Config: TConfigStorage;
begin
  try
    Config := GetIDEConfigStorage(ICONFINDER_CONFIG_FILENAME, true); //, false);
    try
      GlobalWriteSettings(Config, FViewer, FSettingsNodeName);
      Config.WriteToDisk;
    finally
      Config.Free;
    end;
  except
     on E: Exception do begin
       DebugLn('TIconFinderSettingsFrame.ReadSettings Saving ' + ICONFINDER_CONFIG_FILENAME + ' failed: ' + E.Message);
     end;
  end;
end;

end.
