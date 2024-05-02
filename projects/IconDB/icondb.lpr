program icondb;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, idbMain, dbflaz, idbKeywords, idbDatamodule, idbGlobal, idbSettings,
  idbDuplicates, idbThumbnailsDB, BasicThumbnails;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainDatamodule, MainDatamodule);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

