{
 *****************************************************************************
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
  Classes, SysUtils, LazFileUtils,
  Forms, Controls, Graphics, Dialogs, ImgList, StdCtrls,
  ObjInspStrConsts,
  // BuildIntf
  IDEOptionsIntf,
  // IDEIntf
  PropEdits, ComponentEditors, ImageListEditor,
  // Thumbnails
  IconThumbnails, IconViewer;

type

  { TImageListEditorDlgEx }

  TImageListEditorDlgEx = class(TForm)
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FImageListEditor: TImageListEditorDlg;
    FImageList: TImageList;
    FViewer: TIconViewerFrame;
    FIconLibGroupBox: TGroupBox;
    function GetModified: Boolean;
    procedure BtnLoadFromLibClick(Sender: TObject);
    procedure BtnReplaceFromLibClick(Sender: TObject);
    procedure IconViewerDblClick(Sender: TObject);
    procedure IconViewerFilter(Sender: TObject);
  protected
    procedure AddDefaultIconFolders;
    procedure LoadFromIconLib(Replace: Boolean);
    function ImageList: TImageList;

  public
    procedure LoadFromImageList(AImageList: TImageList);
    procedure SaveToImageList;
    property Modified: Boolean read GetModified;

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

procedure Register;


implementation

{$R *.lfm}

procedure Register;
begin
  //Register new component editor for TImageList
  RegisterComponentEditor(TImageList, TImageListComponentEditorEx);
end;

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

type
  // Helper class for accessing protected methods
  TImageListEditorDlgHelper = class helper for TImageListEditorDlg
  public
    procedure InternalAddImageToList(const Picture: TPicture; AddType: TAddType);
    procedure UpdatePreviewImage;
  end;

procedure TImageListEditorDlgHelper.InternalAddImageToList(const Picture: TPicture; AddType: TAddType);
begin
  inherited InternalAddImageToList(Picture, AddType);
end;

procedure TImageListEditorDlgHelper.UpdatePreviewImage;
begin
  inherited UpdatePreviewImage;
end;


{ TImageListEditorDlgEx }

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
  FViewer.OnIconDblClick := @IconViewerDblClick;
  FViewer.OnFilter := @IconViewerFilter;
  AddDefaultIconFolders;

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

procedure TImageListEditorDlgEx.AddDefaultIconFolders;
var
  LazDir: String;
begin
  LazDir := AppendPathDelim(IDEEnvironmentOptions.GetParsedLazarusDirectory);
  FViewer.AddIconFolder(LazDir + 'images/general_purpose/');
  {
  FViewer.AddIconFolder(LazDir + 'images/components/');
  FViewer.AddIconFolder(LazDir + 'components/chmhelp/lhelp/images/');
  FViewer.AddIconFolder(LazDir + 'components/lazcontrols/images/');
  }
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


//initialization
  //Register new component editor for TImageList
//  RegisterComponentEditor(TImageList, TImageListComponentEditorEx);

end.

