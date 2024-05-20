unit IconKeywordFilterEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ButtonPanel, Spin;

type

  { TKeywordFilterEditorForm }

  TKeywordFilterEditorForm = class(TForm)
    Bevel1: TBevel;
    btnClear: TBitBtn;
    btnAND: TBitBtn;
    btnNOT: TBitBtn;
    btnOR: TBitBtn;
    BitBtn3: TBitBtn;
    btnNew: TBitBtn;
    btnEdit: TBitBtn;
    btnAdd: TBitBtn;
    ButtonPanel: TButtonPanel;
    edFilter: TEdit;
    FilterPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblColumns: TLabel;
    lbKeywords: TListBox;
    KeywordPanel: TPanel;
    CenterPanel: TPanel;
    RightPanel: TPanel;
    seColumns: TSpinEdit;
    procedure btnClearClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnANDClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnNOTClick(Sender: TObject);
    procedure btnORClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure lbKeywordsDblClick(Sender: TObject);
    procedure seColumnsChange(Sender: TObject);
  private
    function GetFilter: String;
    procedure SetFilter(AValue: String);
    procedure SetKeywords(AValue: TStrings);

  public
    property Filter: String read GetFilter write SetFilter;
    property Keywords: TStrings write SetKeywords;

  end;

var
  KeywordFilterEditorForm: TKeywordFilterEditorForm;

implementation

{$R *.lfm}

function EndSpace(s: String): String;
begin
  if (s <> '') and (s[Length(s)] <> ' ') then
    Result := s + ' '
  else
    Result := s;
end;

{ TKeywordFilterEditorForm }

procedure TKeywordFilterEditorForm.btnAddClick(Sender: TObject);
var
  keyword: String;
begin
  if lbKeywords.ItemIndex >-1 then
  begin
    keyword := lbKeywords.Items[lbKeywords.ItemIndex];
    edFilter.Text := EndSpace(edFilter.Text) + keyword;
  end;
end;

procedure TKeywordFilterEditorForm.btnClearClick(Sender: TObject);
begin
  edFilter.Text := '';
end;

procedure TKeywordFilterEditorForm.btnANDClick(Sender: TObject);
begin
  edFilter.Text := EndSpace(edFilter.Text) + 'AND';
end;

procedure TKeywordFilterEditorForm.btnNewClick(Sender: TObject);
var
  idx: Integer;
  keyword: String;
  list: TStringList;
begin
  keyword := '';
  if InputQuery('New keyword', 'Keyword', keyword) then
  begin
    idx := lbKeywords.Items.IndexOf(keyword);
    if idx > -1 then
    begin
      MessageDlg('Keyword already defined.', mtInformation, [mbOK], 0);
      lbKeywords.ItemIndex := idx;
    end else
    begin
      list := TStringList.Create;
      try
        list.Assign(lbKeywords.Items);
        list.Sorted := true;
        idx := list.Add(keyword);
        lbKeywords.Items.Assign(list);
      finally
        list.Free;
      end;
    end;
    lbKeywords.ItemIndex := idx;
  end;
end;

procedure TKeywordFilterEditorForm.btnNOTClick(Sender: TObject);
begin
  edFilter.Text := EndSpace(edFilter.Text) + 'NOT';
end;

procedure TKeywordFilterEditorForm.btnORClick(Sender: TObject);
begin
  edFilter.Text := EndSpace(edFilter.Text) + 'OR';
end;

procedure TKeywordFilterEditorForm.FormActivate(Sender: TObject);
var
  w: Integer;
begin
  w := btnNew.Width;
  if btnEdit.Width > w then w := btnEdit.Width;
  if lblColumns.Width > w then w := lblColumns.Width;
  btnNew.Constraints.MinWidth := w;
  btnEdit.Constraints.MinWidth := w;

  Constraints.MinWidth :=
    btnAdd.Width + btnAND.Width + btnOR.Width + btnNOT.Width + btnClear.Width +
    btnAdd.BorderSpacing.Right * 4 + KeywordPanel.BorderSpacing.Around * 2;

  Constraints.MinHeight :=
    RightPanel.Top + RightPanel.Height - KeywordPanel.BorderSpacing.Around +
    ButtonPanel.Height + 2*ButtonPanel.BorderSpacing.Around
end;

function TKeywordFilterEditorForm.GetFilter: String;
begin
  Result := edFilter.Text;
end;

procedure TKeywordFilterEditorForm.lbKeywordsDblClick(Sender: TObject);
begin
  btnAddClick(nil);
end;

procedure TKeywordFilterEditorForm.seColumnsChange(Sender: TObject);
begin
  lbKeywords.Columns := seColumns.Value;
end;

procedure TKeywordFilterEditorForm.SetFilter(AValue: String);
begin
  edFilter.Text := trim(AValue);
end;

procedure TKeywordFilterEditorForm.SetKeywords(AValue: TStrings);
begin
  lbKeywords.Items.Assign(AValue);
end;

end.

