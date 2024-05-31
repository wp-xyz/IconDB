{
 *****************************************************************************
  This file is part of a Lazarus Package, IconLib.

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.
 *****************************************************************************

 Extended image list component editor for searching icons by keywords.

 Changes required in Laz 3.99, unit ImageListEditor (IDEIntf):
 - make "InternalAddImageToList" protected
 - make "UpdatePreviewImage" protected
 - new public property "Modified: boolean read FModified"

 Each icon folder contained in the icon lib must have a file "metadata.xml" with
 a metadata block for each image.
}

unit ImageListEditorEx;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazFileUtils, LazLoggerBase, LazConfigStorage,
  // LCL
  Forms, Controls, Graphics, Dialogs, ImgList, StdCtrls,
  // BuildIntf
  IDEOptionsIntf,
  // IDEIntf
  PropEdits, ComponentEditors, ImageListEditor, BaseIDEIntf, ObjInspStrConsts, IDEImagesIntf,
  // Thumbnails
  IconLibCommon, IconThumbnails, IconViewer, IconLibFrm;

type

  { TImageListEditorDlgEx }

  TImageListEditorDlgEx = class(TImageListEditorDlg)
    BtnReplaceFromIconLib: TButton;
    BtnAddFromIconLib: TButton;
    procedure BtnAddFromIconLibClick(Sender: TObject);
    procedure BtnReplaceFromIconLibClick(Sender: TObject);
  private
    FIconLibForm: TIconLibForm;
    FReplace: Boolean;
    procedure AddReplaceFromIconLib(AReplace: Boolean);
    procedure IconLibDblClick(Sender: TObject);
  public
    function ShowIconLib: Boolean;
  end;


  { TImageListComponentEditorEx }

  TImageListComponentEditorEx = class(TComponentEditor)
  protected
    procedure DoShowEditor;
  public
    procedure ExecuteVerb({%H-}Index: Integer); override;
    function GetVerb({%H-}Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;


implementation

{$R *.lfm}

function EditImageList(AImageList: TImageList): Boolean;
var
  ImageListEditorDlg: TImageListEditorDlgEx;
begin
  ImageListEditorDlg := TImageListEditorDlgEx.Create(Application);
  try
    ImageListEditorDlg.LoadFromImageList(AImageList);

    if ImageListEditorDlg.ShowModal = mrOk then
      ImageListEditorDlg.SaveToImageList;

    Result := ImageListEditorDlg.Modified;
  finally
    ImageListEditorDlg.Free;
  end;
end;


{ TImageListEditorDlgEx }

procedure TImageListEditorDlgEx.AddReplaceFromIconLib(AReplace: Boolean);
var
  res: TCustomImageListResolution;
  sizes: array of TPoint = nil;
  pictures: array of TPicture = nil;
  pic: TPicture;
  i: Integer;
begin
  if ImageList.ResolutionCount = 0 then
  begin
    SetLength(sizes, 1);
    sizes[0] := Point(ImageList.Width, ImageList.Height);
  end else
  begin
    SetLength(sizes, ImageList.ResolutionCount);
    for i := 0 to High(sizes) do
    begin
      res := ImageList.ResolutionByIndex[i];  // they are ordered by size
      sizes[i] := Point(res.Width, res.Height);
    end;
  end;

  try
    SetLength(pictures, Length(sizes));
    for i := 0 to High(pictures) do
      pictures[i] := TPicture.Create;

    // Get pictures form icon lib
    FIconLibForm.LoadPictureSizesFromIconLib(sizes, pictures);

    // First, add the largest image to the imagelist
    if AReplace then
      InternalAddImageToList(pictures[High(pictures)], atReplaceAllResolutions)
    else
      InternalAddImageToList(pictures[High(pictures)], atAdd);

    // Then iterate over all other sizes requested and add them to the imagelist
    for i := Length(pictures)-2 downto 0 do
      InternalAddImageToList(pictures[i], atReplace);
  finally
    for i := 0 to High(pictures) do
      pictures[i].Free;
  end;

  ImageListbox.Invalidate;
  UpdatePreviewImage;
end;

procedure TImageListEditorDlgEx.BtnAddFromIconLibClick(Sender: TObject);
begin
  if ShowIconLib then
    AddReplaceFromIconLib(false);
end;

procedure TImageListEditorDlgEx.BtnReplaceFromIconLibClick(Sender: TObject);
begin
  if ShowIconLib then
    AddReplaceFromIconLib(true);
end;

procedure TImageListEditorDlgEx.IconLibDblClick(Sender: TObject);
begin
  FIconLibForm.ModalResult := mrOK;
end;

function TImageListEditorDlgEx.ShowIconLib: Boolean;
var
  L, T: Integer;
  R: TRect;
begin
  if FIconLibForm = nil then
  begin
    FIconLibForm := TIconLibForm.Create(self);
//    FIconLibForm.OnIconSelectClick := @IconLibSelectClick;
    FIconLibForm.OnIconDblClick := @IconLibDblClick;
  end;

  R := Screen.DesktopRect;
  L := Left + Width;
  if L + FIconLibForm.Width > R.Right then
  begin
    L := Left - FIconLibForm.Width;
    if L < R.Left then
      L := Left + (Width - FIconLibForm.Width) div 2;
  end;
  T := Top;
  FIconLibForm.Left := L;
  FIconLibForm.Top := T;

  Result := FIconLibForm.ShowModal = mrOK;
end;


{ TImageListEditorDlgEx }
(*
procedure TImageListEditorDlgEx.FormCreate(Sender: TObject);

  procedure AddButton(ACaption: String; AOnClick: TNotifyEvent; ABefore, AAfter: TButton);
  var
    btn: TButton;
  begin
    btn := TButton.Create(FImageListEditor);
    btn.Parent := FImageListEditor.GroupBoxL;
    btn.Caption := ACaption;
    btn.AutoSize := true;
    btn.OnClick := AOnClick;

    // Put new button between ABefore and AAfter
    btn.BorderSpacing.Top := ABefore.BorderSpacing.Top;
    btn.Anchors := btn.Anchors + [akRight];
    btn.AnchorSideRight.Control := ABefore;
    btn.AnchorSideRight.Side := asrRight;
    btn.AnchorSideLeft.Control := ABefore;
    btn.AnchorSideLeft.Side := asrLeft;
    btn.AnchorSideTop.Control := ABefore;
    btn.AnchorSideTop.Side := asrBottom;
    AAfter.AnchorSideTop.Control := btn;
  end;

begin
  FImageListEditor := TImageListEditorDlg.Create(self);
  FImageListEditor.Parent := Self;
  FImageListEditor.Align := alLeft;
  FImageListEditor.BorderStyle := bsNone;
  FImageListEditor.Show;

  AddButton('Add from lib', @BtnLoadFromLibClick, FImageListEditor.BtnAddSliced, FImageListEditor.BtnReplace);
  AddButton('Replace from lib', @BtnReplaceFromLibClick, FImageListEditor.BtnReplace, FImageListEditor.BtnReplaceAll);

  FIconLibGroupbox := TGroupBox.Create(Self);
  FIconLibGroupbox.Align := alClient;
  FIconLibGroupbox.BorderSpacing.Top := 6;
  FIconLibGroupbox.BorderSpacing.Right := 6;
  FIconLibGroupbox.BorderSpacing.Bottom := 6;
  FIconLibGroupbox.Caption := 'Icon Library';
  FIconLibGroupbox.Parent := self;

  FViewer := TIconViewerFrame.Create(self);
  FViewer.Align := alClient;
  FViewer.Parent := FIconLibGroupBox;
  FViewer.IconViewer.FocusedColor := clWindowText;
  FViewer.IconViewer.ThumbnailColor := clWindow;
  FViewer.ImageList := IDEImages.Images_16;
  FViewer.ImageIndex_ExecuteFilter := IDEImages.GetImageIndex('item_filter', 16);
  FViewer.ImageIndex_ClearFilter := IDEImages.GetImageIndex('menu_clean', 16);
  FViewer.OnIconDblClick := @IconViewerDblClick;
  FViewer.OnFilter := @IconViewerFilter;
  AddIconFolders;

  Caption := sccsILEdtCaption;
end;

procedure TImageListEditorDlgEx.FormShow(Sender: TObject);
begin
  Constraints.MinHeight := FImageListEditor.Constraints.MinHeight;
  if Height < Constraints.MinHeight then Height := 0;
end;

procedure TImageListEditorDlgEx.BtnLoadFromLibClick(Sender: TObject);
begin
  LoadFromIconLib(false);
end;

procedure TImageListEditorDlgEx.BtnReplaceFromLibClick(Sender: TObject);
begin
  LoadFromIconLib(true);
end;

procedure TImageListEditorDlgEx.AddDefaultIconFolder;
var
  LazDir: String;
begin
  LazDir := AppendPathDelim(IDEEnvironmentOptions.GetParsedLazarusDirectory);
  FViewer.AddIconFolder(LazDir + 'images/general_purpose/');
end;

procedure TImageListEditorDlgEx.AddIconFolders;
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

function TImageListEditorDlgEx.GetModified: Boolean;
begin
  Result := FImageListEditor.Modified;
end;

procedure TImageListEditorDlgEx.IconViewerDblClick(Sender: TObject);
begin
  LoadFromIconLib(false);
end;

procedure TImageListEditorDlgEx.IconViewerFilter(Sender: TObject);
begin
  FIconLibGroupbox.Caption := Format('Icon Library (%d out of %d icons)', [FViewer.FilteredCount, FViewer.TotalCount]);
end;

function TImageListEditorDlgEx.ImageList: TImageList;
begin
  Result := FImageListEditor.ImageList;
end;

procedure TImageListEditorDlgEx.LoadFromIconLib(Replace: Boolean);
var
  i, w, h: Integer;
  //pic: TPicture;
  item, largestItem: TIconItem;
  res: TCustomImageListResolution;
begin
  if FViewer.SelectedIcon <> nil then
  begin
    // Find largest icon
    if FImageList.ResolutionCount = 0 then
    begin
      w := FImageList.Width;
      h := FImageList.Height;
    end else
    begin
      res := ImageList.ResolutionByIndex[ImageList.ResolutionCount-1];  // they are ordered by size
      w := res.Width;
      h := res.Height;
    end;
    largestItem := FViewer.IconViewer.FindIconSize(FViewer.SelectedIcon, w, h);
    if largestItem = nil then                   // this should not happen...
      largestItem := FViewer.IconViewer.FindLargestIcon(FViewer.SelectedIcon);

    if Replace then
      FImageListEditor.InternalAddImageToList(largestItem.Picture, atReplaceAllResolutions)
    else
      FImageListEditor.InternalAddImageToList(largestItem.Picture, atAdd);

    // Iterate over all sizes registered in the imagelist.
    for i := 0 to ImageList.ResolutionCount-1 do
    begin
      res := ImageList.ResolutionByIndex[i];
      w := res.Width;
      h := res.Height;
      item := FViewer.IconViewer.FindIconSize(FViewer.SelectedIcon, w, h);
      if item = nil then
        item := largestItem;
      FImageListEditor.InternalAddImageToList(item.Picture, atReplace);
    end;
    FImageListEditor.ImageListbox.Invalidate;
    FImageListEditor.UpdatePreviewImage;
  end;
end;

procedure TImageListEditorDlgEx.LoadFromImageList(AImageList: TImageList);
begin
  FImageListEditor.LoadFromImageList(AImageList);
  FImageList := AImageList;
end;

procedure TImageListEditorDlgEx.SaveToImageList;
begin
  FImageListEditor.SaveToImageList;
end;
*)

{ TImageListComponentEditorEx }

procedure TImageListComponentEditorEx.DoShowEditor;
var
  Hook: TPropertyEditorHook;
  AImg: TImageList;
begin
  if GetComponent is TImageList then
  begin
    AImg := TImageList(GetComponent);
    GetHook(Hook);

    if EditImageList(AImg) then
      if Assigned(Hook) then Hook.Modified(Self);
  end;
end;

procedure TImageListComponentEditorEx.ExecuteVerb(Index: Integer);
begin
  DoShowEditor;
end;

function TImageListComponentEditorEx.GetVerb(Index: Integer): String;
begin
  Result := oisImageListComponentEditor;
end;

function TImageListComponentEditorEx.GetVerbCount: Integer;
begin
  Result := 1;
end;


end.

