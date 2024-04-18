unit idbKeywords;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, DBCtrls, ExtCtrls, ExtDlgs;

type

  { TEditKeywordsForm }

  TEditKeywordsForm = class(TForm)
    ButtonPanel: TButtonPanel;
    mmoKeywords: TMemo;
    Image: TImage;
    Label5: TLabel;
  private
    FDataSource: TDataSource;
    procedure LoadIconFromDataset(APicture: TPicture);
    function SeparateKeywords: String;

  public
    function GetKeywords: String;
    procedure SetDataSource(ADataSource: TDataSource);

  end;

var
  EditKeywordsForm: TEditKeywordsForm;

implementation

{$R *.lfm}

uses
  idbGlobal, idbDatamodule;

procedure TEditKeywordsForm.LoadIconFromDataset(APicture: TPicture);
begin
  Maindatamodule.LoadPicture(APicture);
end;

function TEditKeywordsForm.GetKeywords: String;
var
  i: Integer;
  s: String;
  L: TStringList;
begin
  Result := '';
  L := TStringList.Create;
  try
    L.Assign(mmoKeywords.Lines);
    for i := L.Count-1 downto 0 do
    begin
      L[i] := Trim(L[i]);
      if L[i] = '' then L.Delete(i);
      if L.IndexOf(L[i]) <> i then L.Delete(i);
    end;
    L.Sorted := True;
    L.Delimiter := KEYWORD_SEPARATOR;
    L.StrictDelimiter := true;
    Result := L.DelimitedText;
  finally
    L.Free;
  end;
end;

function TEditKeywordsForm.SeparateKeywords: String;
var
  L: TStrings;
  field: TField;
  s: String;
begin
  field := FDatasource.Dataset.FieldByName('KEYWORDS');
  if field.IsNull then
    Result := ''
  else
  begin
    L := TStringList.Create;
    try
      L.Delimiter := KEYWORD_SEPARATOR;
      L.StrictDelimiter := true;
      L.DelimitedText := field.AsString;
      Result := L.Text;
    finally
      L.Free;
    end;
  end;
end;

procedure TEditKeywordsForm.SetDataSource(ADataSource: TDataSource);
begin
  FDataSource := ADataSource;
  LoadIconFromDataset(Image.Picture);
  mmoKeywords.Lines.Text := SeparateKeywords;
end;

end.

