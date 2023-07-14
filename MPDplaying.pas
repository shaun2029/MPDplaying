program MPDplaying;

{$mode Delphi}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, main, Settings, webcontrol, PlayList, mpc,
  VolumeControl
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.CreateForm(TfrmSearch, frmSearch);
  Application.Run;
end.

