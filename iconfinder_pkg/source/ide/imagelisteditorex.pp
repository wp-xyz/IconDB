{
 *****************************************************************************
  This file is part of a Lazarus Package, IconFinder.

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
  Forms, Controls, Graphics, Dialogs, ImgList, StdCtrls, ComCtrls, Menus, ActnList,
  // BuildIntf
  IDEOptionsIntf,
  // IDEIntf
  PropEdits, ComponentEditors, ImageListEditor, BaseIDEIntf, ObjInspStrConsts, IDEImagesIntf,
  // Thumbnails
  IconFinderStrConstsIDE, IconFinderCommon, IconThumbnails, IconViewer, IconFinderFrm;

type

  { TImageListEditorDlgEx }

  TImageListEditorDlgEx = class(TImageListEditorDlg)
    procedure FormCreate(Sender: TObject);
  private
    FIconFinderForm: TIconFinderForm;
    procedure AddImgFromIconFinder(Sender: TObject);
    procedure AddReplaceFromIconFinder(AReplace: Boolean);
    procedure IconFinderDblClick(Sender: TObject);
    procedure ReplaceImgByIconFinder(Sender: TObject);
  public
    function ShowIconFinder: Boolean;
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

procedure TImageListEditorDlgEx.AddImgFromIconFinder(Sender: TObject);
begin
  if ShowIconFinder then
    AddReplaceFromIconFinder(false);
end;

procedure TImageListEditorDlgEx.AddReplaceFromIconFinder(AReplace: Boolean);
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

    // Get pictures form icon finder
    FIconFinderForm.LoadPictureSizesFromIconFinder(sizes, pictures);

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

procedure TImageListEditorDlgEx.FormCreate(Sender: TObject);
var
  acAdd: TAction;
  acReplace: TAction;
  mAdd: TMenuItem;
  mReplace: TMenuItem;
begin
  DebugLn('[TImageListEditorDlgEx.FormCreate] ENTER');

  inherited;

  acAdd := TAction.Create(ActionList);
  acAdd.Caption := RSImgListEditor_AddFromIconFinder;
  acAdd.OnExecute := @AddImgfromIconFinder;

  acReplace := TAction.Create(ActionList);
  acReplace.Caption := RSImgListEditor_ReplaceFromIconFinder;
  acReplace.OnExecute := @ReplaceImgByIconFinder;

  mAdd := TMenuItem.Create(AddPopupMenu);
  mAdd.Action := acAdd;
  AddPopupMenu.Items.Insert(3, mAdd);

  mReplace := TMenuItem.Create(ReplacePopupMenu);
  mReplace.Action := acReplace;
  ReplacePopupMenu.Items.Add(mReplace);

  DebugLn('[TImageListEditorDlgEx.FormCreate] EXIT');
end;

procedure TImageListEditorDlgEx.IconFinderDblClick(Sender: TObject);
begin
  FIconFinderForm.ModalResult := mrOK;
end;

function TImageListEditorDlgEx.ShowIconFinder: Boolean;
var
  L, T: Integer;
  R: TRect;
begin
  if FIconFinderForm = nil then
  begin
    FIconFinderForm := TIconFinderForm.Create(self);
    FIconFinderForm.OnIconDblClick := @IconFinderDblClick;
  end;

  R := Screen.DesktopRect;
  L := Left + Width;
  if L + FIconFinderForm.Width > R.Right then
  begin
    L := Left - FIconFinderForm.Width;
    if L < R.Left then
      L := Left + (Width - FIconFinderForm.Width) div 2;
  end;
  T := Top;
  FIconFinderForm.Left := L;
  FIconFinderForm.Top := T;

  FIconFinderForm.ReadSettings('ImageListComponentEditor');

  Result := FIconFinderForm.ShowModal = mrOK;
end;

procedure TImageListEditorDlgEx.ReplaceImgByIconFinder(Sender: TObject);
begin
  if ShowIconFinder then
    AddReplaceFromIconFinder(true);
end;


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

