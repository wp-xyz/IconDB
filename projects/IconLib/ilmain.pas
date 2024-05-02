unit ilMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ActnList;

type

  { TMainForm }

  TMainForm = class(TForm)
    acAddIcons: TAction;
    acEditMetadata: TAction;
    acExit: TAction;
    ActionList: TActionList;
    CoolBar1: TCoolBar;
    Images: TImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure acExitExecute(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.acExitExecute(Sender: TObject);
begin
  Close;
end;

end.

