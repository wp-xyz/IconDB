{
 **********************************************************************
  This file is part of a Lazarus Package, IconLib.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 **********************************************************************

 Settings for the Icon Lib addon to the IDE:
 - add/remove folders with icons to be included
 - rearrange folders
 - define keywords
 - save keywords to "metadata.xml" file in the icon folder.

 The settings are stored in the file "iconlib.xml" in the Lazarus user profile
 (primary config directory).
}

unit IconLibSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazConfigStorage, LazFileUtils, LazLoggerBase,
  // LCL
  Controls, Graphics, StdCtrls, EditBtn, Dialogs, ComCtrls, Forms,
  // BuildIntf
  IDEOptionsIntf, baseIDEIntf,
  // IdeIntf
  LazIDEIntf, IDEOptEditorIntf, IDEImagesIntf,
  // Icon Lib
  IconLibStrConstsIDE, IconLibCommon, IconThumbnails, IconViewer,
  IconLibFolders, IconLibMetadata;


{ TIconLibSettings
  The Options Group ID, and, perhaps, a place in the Tree View }

type
  TIconLibSettings = class(TAbstractIDEEnvironmentOptions) // needed by options group.
  private

  public
    constructor Create(const {%H-}pbReadRegFile: boolean);
    destructor Destroy; override;
    class function GetGroupCaption: String; override;
    class function GetInstance: TAbstractIDEOptions; override;
    procedure DoAfterWrite({%H-}Restore: boolean); override;
  end;


{ TIconLibSettingsFrame
  This is the frame displayed when the user clicks the Tree View node }

type
  TIconLibSettingsFrame = class(TAbstractIDEOptionsEditor)
    ToolBar: TToolBar;
    tbFolders: TToolButton;
    tbEditMetadata: TToolButton;
    tbSaveMetadata: TToolButton;
    procedure tbFoldersClick(Sender: TObject);
    procedure tbEditMetadataClick(Sender: TObject);
    procedure tbSaveMetadataClick(Sender: TObject);
  private
    FViewer: TIconViewerFrame;
    procedure AddDefaultFolder;
    procedure CenterForm(AForm: TCustomForm);
    procedure EditFolders;
    procedure EditIconMetaData(AIcon: TIconItem);
    procedure IconViewerDblClick(Sender: TObject);
    procedure IconViewerFilter(Sender: TObject);
    procedure SaveMetadataFiles;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure RestoreSettings({%H-}AOptions: TAbstractIDEOptions); override;
  end;

procedure GlobalReadSettings(AConfig: TConfigStorage; AViewer: TIconViewerFrame; ANodeName: String);
procedure GlobalWriteSettings(AConfig: TConfigStorage; AViewer: TIconViewerFrame; ANodeName: String);

var
  IconLibOptionsGroup: integer;
  IconLibOptionsFrameID: integer;

implementation

{$R *.lfm}

{ TIconLibSettings }

constructor TIconLibSettings.Create(const pbReadRegFile: boolean);
begin
  // inherited Create;
end;

destructor TIconLibSettings.Destroy;
begin
  inherited Destroy;
end;

class function TIconLibSettings.GetGroupCaption: String;
begin
  Result := RSIconLibIDE_IconLibrary;
end;

class function TIconLibSettings.GetInstance: TAbstractIDEOptions;
begin
  //result := TAbstractIDEOptions(self);    // Nope, it does not like that !
  result := nil;
end;

procedure TIconLibSettings.DoAfterWrite(Restore: boolean);
begin
  inherited DoAfterWrite(Restore);
end;


{ TIconLibSettingsFrame }

constructor TIconLibSettingsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FViewer := TIconViewerFrame.Create(self);
  FViewer.Align := alClient;
  FViewer.IconViewer.FocusedColor := clWindowText;
  FViewer.IconViewer.ThumbnailColor := clWindow;
  FViewer.ImageList := IDEImages.Images_16;
  FViewer.ImageIndex_ExecuteFilter := IDEImages.GetImageIndex('item_filter', 16);
  FViewer.ImageIndex_ClearFilter := IDEImages.GetImageIndex('menu_clean', 16);
  FViewer.OnIconDblClick := @IconViewerDblClick;
  FViewer.OnFilter := @IconViewerFilter;
  FViewer.Parent := self; //FIconLibGroupBox;
  FViewer.UpdateLanguage;

  Toolbar.Images := IDEImages.Images_16;

  tbFolders.Caption := RSIconLibIDE_Folders;
  tbFolders.Hint := RSIconLibIDE_FolderHint;
  tbFolders.ImageIndex := IDEImages.GetImageIndex('laz_open');
  tbEditMetadata.Caption := RSIconLibIDE_Metadata;
  tbEditMetadata.Hint := RSIconLibIDE_MetadataHint;
  tbEditMetadata.ImageIndex := IDEImages.GetImageIndex('laz_edit');
  tbSaveMetadata.Caption := RSIconLibIDE_SaveMetadata;
  tbSaveMetadata.Hint := RSIconLibIDE_SaveMetadataHint;
  tbSaveMetadata.ImageIndex := IDEImages.GetImageIndex('laz_save');
end;

destructor TIconLibSettingsFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TIconLibSettingsFrame.AddDefaultFolder;
var
  lazDir: String;
begin
  lazDir := AppendPathDelim(IDEEnvironmentOptions.GetParsedLazarusDirectory);
  FViewer.AddIconFolder(LazDir + 'images/general_purpose/');
end;

procedure TIconLibSettingsFrame.CenterForm(AForm: TCustomForm);
var
  P: TPoint;
begin
  P := ClientToScreen(Point(Left + (Width - AForm.Width) div 2, Top + (Height - AForm.Height) div 2));
  AForm.SetBounds(P.X, P.Y, AForm.Width, AForm.Height);
end;

procedure TIconLibSettingsFrame.EditFolders;
var
  F: TIconFolderForm;
  folders: TStrings;
  P: TPoint;
begin
  F := TIconFolderForm.Create(nil);
  folders := TStringList.Create;
  try
    FViewer.IconViewer.WriteIconFolders(folders);
    CenterForm(F);
    F.SetIconFolders(folders);
    if F.ShowModal = mrOK then
    begin
      Screen.BeginWaitCursor;
      try
        F.GetIconFolders(folders);
        FViewer.ReadIconFolders(folders);
      finally
        Screen.EndWaitCursor;
      end;
    end;
  finally
    folders.Free;
    F.Free;
  end;
end;

{ Opens the metadata editor for specifying the keywords and the style of the
  currently selected icon. The data are copied to all icons sharing the same
  name base. }
procedure TIconlibSettingsFrame.EditIconMetaData(AIcon: TIconItem);
var
  F: TIconMetadataForm;
begin
  if AIcon = nil then
    exit;

  F := TIconMetadataForm.Create(nil);
  try
    CenterForm(F);
    F.MetaDataToControls(AIcon);
    if F.ShowModal = mrOK then
    begin
      F.ControlsToMetaData(AIcon);
      FViewer.CopyMetadataToNameBase(AIcon);
    end;
  finally
    F.Free;
  end;
end;

function TIconLibSettingsFrame.GetTitle: String;
begin
  Result := RSIconLibIDE_General;
end;

procedure TIconLibSettingsFrame.IconViewerDblClick(Sender: TObject);
begin
  EditIconMetadata(FViewer.IconViewer.SelectedIcon);
end;

procedure TIconLibSettingsFrame.IconViewerFilter(Sender: TObject);
begin
  //FIconLibGroupbox.Caption := Format('Icon Library (%d out of %d icons)', [FViewer.FilteredCount, FViewer.TotalCount]);
end;

procedure TIconLibSettingsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  Config: TConfigStorage;
begin
  try
    Config := GetIDEConfigStorage(ICONLIB_CONFIG_FILENAME, true);
    try
      GlobalReadSettings(Config, FViewer, 'IDEOptions');
      if FViewer.IconViewer.IconFolders.Count = 0 then
        AddDefaultFolder;
    finally
      Config.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('TIconLibSettingsFrame.ReadSettings Loading ' +  ICONLIB_CONFIG_FILENAME + ' failed: ' + E.Message);
    end;
  end;
end;

// Maybe the initial settings before we have a config file ?  Labels and Captions.
procedure TIconLibSettingsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  if FViewer.IconViewer.IconFolders.Count = 0 then
    AddDefaultFolder;
end;

class function TIconLibSettingsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := nil;
end;

procedure TIconLibSettingsFrame.tbFoldersClick(Sender: TObject);
begin
  EditFolders;
end;

procedure TIconLibSettingsFrame.tbEditMetadataClick(Sender: TObject);
begin
  EditIconMetaData(FViewer.IconViewer.SelectedIcon);
end;

procedure TIconLibSettingsFrame.tbSaveMetadataClick(Sender: TObject);
begin
  SaveMetadataFiles;
end;

procedure TIconLibSettingsFrame.SaveMetadataFiles;
begin
  FViewer.IconViewer.WriteMetadataFiles;
end;

// Gets called whenever user opens Options tree.
procedure TIconLibSettingsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Config: TConfigStorage;
begin
  try
     Config := GetIDEConfigStorage(ICONLIB_CONFIG_FILENAME, true); //, false);
     try
       GlobalWriteSettings(Config, FViewer, 'IDEOptions');
     finally
       Config.Free;
     end;
   except
     on E: Exception do begin
       DebugLn('TIconLibSettingsFrame.ReadSettings Saving ' + ICONLIB_CONFIG_FILENAME + ' failed: ' + E.Message);
     end;
   end;
end;

procedure TIconLibSettingsFrame.RestoreSettings(AOptions: TAbstractIDEOptions);
begin
  inherited RestoreSettings(AOptions);
end;

{------------------------------------------------------------------------------}

procedure GlobalReadSettings(AConfig: TConfigStorage; AViewer: TIconViewerFrame;
  ANodeName: String);
var
  folder: String;
  isHidden: Boolean;
  n, i: Integer;
  s: String;
  list: TStrings;
begin
  // Icon folder list
  n := AConfig.GetValue('IconLib/Folders/Count', 0);
  for i := 0 to n-1 do
  begin
    folder := AConfig.GetValue(Format('IconLib/Folders/Item%d/Value', [i]), '');
    isHidden := AConfig.GetValue(Format('IconLib/Folders/Item%d/Hidden', [i]), false);
    if (folder <> '') and DirectoryExists(folder) then
      AViewer.AddIconFolder(folder, isHidden);
  end;

  // Keyword filter history list
  list := TStringList.Create;
  try
    n := AConfig.GetValue('IconLib/FilterHistory/Count', 0);
    for i := 0 to n-1 do
    begin
      s := AConfig.GetValue(Format('IconLib/FilterHistory/Item%d/Value', [i]), '');
      if s <> '' then list.Add(s);
    end;
    AViewer.SetKeywordsHistory(list);
  finally
    list.Free;
  end;

  // Read the icon size and style filter settings
  if (ANodeName <> '') and (ANodename[Length(ANodeName)] = '/') then Delete(ANodeName, Length(ANodeName), 1);
  if (ANodeName <> '') and (ANodeName[1] = '/') then Delete(ANodeName, 1, 1);
  if ANodeName <> '' then
  begin
    AViewer.SizeFilter := AConfig.GetValue(Format('IconLib/%s/SizeFilter/Value', [ANodeName]), '');
    AViewer.StyleFilter := AConfig.GetValue(Format('IconLib/%s/StyleFilter/Value', [ANodeName]), '');
  end;
end;

procedure GlobalWriteSettings(AConfig: TConfigStorage; AViewer: TIconViewerFrame;
  ANodeName: String);
var
  i: Integer;
  list: TStrings;
  s: String;
begin
  list := TStringList.Create;
  try
    // Icon folder list
    AViewer.IconViewer.WriteIconFolders(list);
    AConfig.SetValue('IconLib/Folders/Count', list.Count);
    for i := 0 to list.Count-1 do
    begin
      AConfig.SetValue(Format('IconLib/Folders/Item%d/Value', [i]), list[i]);
      if list.Objects[i] <> nil then
        AConfig.SetValue(Format('IconLib/Folders/Item%d/Hidden', [i]), true);
    end;

    // Keyword filter history list
    list.Clear;
    AViewer.GetKeywordsHistory(list);
    AConfig.SetValue('IconLib/FilterHistory/Count', list.Count);
    for i := 0 to list.Count-1 do
      AConfig.SetValue(Format('IconLib/FilterHistory/Item%d/Value', [i]), list[i]);
  finally
    list.Free;
  end;

  // Write the icon size and style filter settings
  if (ANodeName <> '') and (ANodename[Length(ANodeName)] = '/') then Delete(ANodeName, Length(ANodeName), 1);
  if (ANodeName <> '') and (ANodeName[1] = '/') then Delete(ANodeName, 1, 1);
  if ANodeName <> '' then
  begin
    s := AViewer.SizeFilter;
    if s <> '' then
      AConfig.SetValue(Format('IconLib/%s/SizeFilter/Value', [ANodeName]), s);
    s := AViewer.StyleFilter;
    if s <> '' then
      AConfig.SetValue(Format('IconLib/%s/StyleFilter/Value', [ANodeName]), s);
  end;
end;


initialization
  IconLibOptionsGroup := GetFreeIDEOptionsGroupIndex(GroupEditor);
  RegisterIDEOptionsGroup(IconLibOptionsGroup, TIconLibSettings, False);   // F cos I get Index from above line. I think.

end.
