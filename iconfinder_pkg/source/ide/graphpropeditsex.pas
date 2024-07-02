{
 *****************************************************************************
  This file is part of a Lazarus Package, IconLib.

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.
 *****************************************************************************

 Extended graphic property editors for searching icons by keywords.

 Changes required in Laz 3.99 unit GraphicPropEdit

 - After the form class declaration, add

     TGraphicPropertyEditorFormClass = class of TGraphicPropertyEditorForm;

 Changes required in Laz 3.99, unit GraphPropEdits (IDEIntf):

 - Add protected virtual class function GetEditorFormClass to TGraphicPropertyEditor,
   TPicturePropertyEditor and TButtonGlyphPropEditor:

     class function GetEditorFormClass: TGraphicPropertyEditorFormClass; virtual;

     class function TGraphicPropertyEditor.GetEditorFormClass: TGraphicPropertyEditorFormClass;
     begin
       Result := TGraphicPropertyEditorForm;
     end;

 - In the Edit method of these property editors, find the line

     TheDialog := TGraphicPropertyEditorForm.Create(nil);

   and replace the word "TGraphicPropertyEditorForm" by "GetEditorFormClass"
}

unit GraphPropEditsEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  GraphPropEdits, GraphicPropEdit, GraphicPropEditEx;

type
  TGraphicPropertyEditorEx = class(TGraphicPropertyEditor)
  protected
    class function GetEditorFormClass: TGraphicPropertyEditorFormClass; override;
  end;

  TPicturePropertyEditorEx = class(TPicturePropertyEditor)
  protected
    class function GetEditorFormClass: TGraphicPropertyEditorFormClass; override;
  end;

  TButtonGlyphPropEditorEx = class(TButtonGlyphPropEditor)
  protected
    class function GetEditorFormClass: TGraphicPropertyEditorFormClass; override;
  end;

implementation

class function TGraphicPropertyEditorEx.GetEditorFormClass: TGraphicPropertyEditorFormClass;
begin
  Result := TGraphicPropertyEditorFormEx;
end;

class function TPicturePropertyEditorEx.GetEditorFormClass: TGraphicPropertyEditorFormClass;
begin
  Result := TGraphicPropertyEditorFormEx;
end;

class function TButtonGlyphPropEditorEx.GetEditorFormClass: TGraphicPropertyEditorFormClass;
begin
  Result := TGraphicPropertyEditorFormEx;
end;


end.

