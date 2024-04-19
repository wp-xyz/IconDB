unit idbDuplicates;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls;

type

  { TDuplicateIconsForm }

  TDuplicateIconsForm = class(TForm)
    ButtonPanel: TButtonPanel;
    Label1: TLabel;
    DuplicateList: TListBox;
  private

  public

  end;

var
  DuplicateIconsForm: TDuplicateIconsForm;

implementation

{$R *.lfm}

end.

