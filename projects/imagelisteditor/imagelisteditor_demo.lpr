program ImageListEditor_Demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ile_Main, ImageListEditor,
  BasicThumbnails, IconThumbNails, IconViewer;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='ImageListEditor_Demo';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

