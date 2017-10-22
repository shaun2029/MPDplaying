unit main;

{$mode Delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, Menus, IniPropStorage, Unix, Pipes, Process, Settings;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnPrev: TBitBtn;
    btnNext: TBitBtn;
    mnuSettings: TMenuItem;
    mMenu: TMainMenu;
    mmPlaying: TMemo;
    tmrPlaying: TTimer;
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuSettingsClick(Sender: TObject);
    procedure tmrPlayingTimer(Sender: TObject);
  private
    function GetMPDHostParams: string;
    procedure Update;
    { private declarations }
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

function TfrmMain.GetMPDHostParams: string;
begin
  Result := '';

  if (Trim(frmSettings.edtHost.Text) <> '') then
  begin
    Result := 'MPD_HOST=' + Trim(frmSettings.edtHost.Text);
  end;

  if (Trim(frmSettings.edtPort.Text) <> '') then
  begin
    Result := Result + ' MPD_Port=' + Trim(frmSettings.edtPort.Text);
  end;
end;

procedure TfrmMain.tmrPlayingTimer(Sender: TObject);
begin
  Update;
end;

procedure TfrmMain.btnNextClick(Sender: TObject);
var
  Output: string;
  CommandLine: string;
begin
  CommandLine := 'bash -c "' + GetMPDHostParams + ' mpc next"';
  RunCommand(CommandLine, Output);
  Update;
end;

procedure TfrmMain.btnPrevClick(Sender: TObject);
var
  Output: string;
  CommandLine: string;
begin
  CommandLine := 'bash -c "' + GetMPDHostParams + ' mpc prev"';
  RunCommand(CommandLine, Output);
  Update;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  Update;
end;

procedure TfrmMain.mnuSettingsClick(Sender: TObject);
begin
  tmrPlaying.Enabled := False;
  frmSettings.ShowModal;
  tmrPlaying.Enabled := True;
end;

procedure TfrmMain.Update;
var
  Output: string;
  CommandLine: string;
  Outputs:TStringlist;
begin
  CommandLine := 'bash -c "' + GetMPDHostParams + ' mpc"';
  if (RunCommand(CommandLine, Output)) and (Output<>'') then
  begin
    Outputs := TStringlist.Create;
    Outputs.Text := Output;
    Output := Outputs.Strings[0];
    FreeAndNil(Outputs);

    if (mmPlaying.Lines.IndexOf(Output) <> 0) then
    begin
      Self.Caption := 'Playing: ' + Copy(Output, 1, 60);
      mmPlaying.Lines.Insert(0, Output);
      if mmPlaying.Lines.Count > 12 then
        mmPlaying.Lines.Delete(mmPlaying.Lines.Count-1);
{
      CommandLine := 'notify-send -t 10000 "Playing: " "' + Output + '"';
      RunCommand(CommandLine, Output);
}
    end;
  end;
end;

end.

