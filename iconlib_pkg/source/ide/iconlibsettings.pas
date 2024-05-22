{
 **********************************************************************
  This file is part of a Lazarus Package, IconLib.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 **********************************************************************

... decription ....

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
  IconThumbnails, IconViewer, IconLibFolders, IconLibMetadata;

const
  ICONLIB_CONFIG_FILENAME = 'iconlibcfg.xml';


{ TIconLibSettings }

// -------- The Options Group ID, and, perhaps, a place in the Tree View -------

type
  TIconLibSettings = class(TAbstractIDEEnvironmentOptions)          // needed by options group.
  private

  public
    constructor Create(const {%H-}pbReadRegFile: boolean);
    destructor Destroy; override;
    class function GetGroupCaption: String; override;
    class function GetInstance: TAbstractIDEOptions; override;
    procedure DoAfterWrite({%H-}Restore: boolean); override;
  end;


// ------ This is the Frame displayed when user clicks the Tree View node ------
type
  { TIconLibSettingsFrame }
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
  Result := 'Icon Library';
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
  FViewer.Parent := self; //FIconLibGroupBox;
  FViewer.IconViewer.FocusedColor := clWindowText;
  FViewer.IconViewer.ThumbnailColor := clWindow;
  FViewer.OnIconDblClick := @IconViewerDblClick;
  FViewer.OnFilter := @IconViewerFilter;

  Toolbar.Images := IDEImages.Images_16;

  tbFolders.Caption := 'Folders...';
  tbFolders.Hint := 'Icon folder management';
  tbFolders.ImageIndex := IDEImages.GetImageIndex('laz_open');
  tbEditMetadata.Caption := 'Metadata...';
  tbEditMetadata.Hint := 'Edit icon metadata';
  tbEditMetadata.ImageIndex := IDEImages.GetImageIndex('laz_edit');
  tbSaveMetadata.Caption := 'Save metadata';
  tbSaveMetadata.Hint := 'Save icon metadata files';
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
  Result := 'General';  //rsGeneral;
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
  folder: String;
  n, i: Integer;
begin
  try
    Config := GetIDEConfigStorage(ICONLIB_CONFIG_FILENAME, true);
    try
      n := Config.GetValue('IconLib/Folders/Count', 0);
      if n = 0 then
        AddDefaultFolder
      else
        for i := 0 to n-1 do
        begin
          folder := Config.GetValue('IconLib/Folders/Item' + IntToStr(i) + '/Value', '');

          DebugLn(['[ReadSettings] i = ', i, ', folder = ', folder]);

          if (folder <> '') and DirectoryExists(folder) then
            FViewer.AddIconFolder(folder);
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
  i: Integer;
  list: TStrings;
begin
  try
     Config := GetIDEConfigStorage(ICONLIB_CONFIG_FILENAME, false);
     try
       list := TStringList.Create;
       try
         FViewer.IconViewer.WriteIconFolders(list);
         Config.SetValue('IconLib/Folders/Count', list.Count);
         for i := 0 to list.Count-1 do
         begin

           DebugLn(['[WriteSettings] i = ', i, ', list[i] = ', list[i]]);

           Config.SetValue('IconLib/Folders/Item' + IntToStr(i) + '/Value', list[i]);
         end;
       finally
         list.Free;
       end;
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


initialization
  IconLibOptionsGroup := GetFreeIDEOptionsGroupIndex(GroupEditor);
  RegisterIDEOptionsGroup(IconLibOptionsGroup, TIconLibSettings, False);   // F cos I get Index from above line. I think.

end.
