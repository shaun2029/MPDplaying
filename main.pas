unit main;

{$mode Delphi}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Menus, IniPropStorage, ComCtrls, Unix, Pipes,
  Process, Settings, IniFiles;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnPrev: TBitBtn;
    btnNext: TBitBtn;
    grpMood: TCheckGroup;
    GroupBox1: TGroupBox;
    mnuSettings: TMenuItem;
    mMenu: TMainMenu;
    mmPlaying: TMemo;
    tmrPlaying: TTimer;
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure grpMoodItemClick(Sender: TObject; Index: integer);
    procedure mnuSettingsClick(Sender: TObject);
    procedure tmrPlayingTimer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure pbLevelSoftContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure rgpMoodSelectionChanged(Sender: TObject);
  private
    function GetMPDHostParams: string;
    procedure SaveSettings;
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

procedure TfrmMain.FormActivate(Sender: TObject);
var
   Cfg: TIniFile;
   i, Min, Max: integer;
begin
  try
    Cfg := TIniFile.Create(GetUserDir + '.music-skip.conf');
    Min := Cfg.ReadInteger('Settings', 'Min', 1);
    Max := Cfg.ReadInteger('Settings', 'Max', 3);

    Cfg.Free;
  except
    on E: Exception do
    begin
      ShowMessage('Exception: ' + E.Message);
    end;
  end;

  for i := 0 to grpMood.Items.Count - 1 do
  begin
    if (i >= Min) and (i <= Max) then grpMood.Checked[i] := True;
  end;
end;

procedure TfrmMain.SaveSettings;
var
  Cfg: TIniFile;
  F : TextFile;
  Min, Max, Soft, Hard, i: integer;
begin

  for i := 0 to grpMood.Items.Count - 1 do
  begin
    if grpMood.Checked[i] then
    begin
      Min := i;
      Break;
    end;
  end;

  for i := grpMood.Items.Count - 1 downto 0 do
  begin
    if grpMood.Checked[i] then
    begin
      Max := i;
      Break;
    end;
  end;

  for i := 0 to grpMood.Items.Count - 1 do
  begin
    if (i >= Min) and (i <= Max) then grpMood.Checked[i] := True;
  end;

  Soft := Min * 3;
  Hard := (Max + 1) * 3;

  // Ensure that all music can be played.
  if Soft <= 0 then
     Soft := -20;

  AssignFile(f,GetUserDir + '.music-skip');
  Rewrite(f);
  WriteLn(f, FloatToStr(Soft));
  WriteLn(f, FloatToStr(Hard));
  CloseFile(f);

  try
    Cfg := TIniFile.Create(GetUserDir + '.music-skip.conf');
    Cfg.WriteInteger('Settings', 'Min', Min);
    Cfg.WriteInteger('Settings', 'Max', Max);
    Cfg.Free;
  except
    on E: Exception do
    begin
      ShowMessage('Exception: ' + E.Message);
    end;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveSettings;
end;

procedure TfrmMain.grpMoodItemClick(Sender: TObject; Index: integer);
begin
  SaveSettings;
end;

procedure TfrmMain.pbLevelSoftContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

procedure TfrmMain.rgpMoodSelectionChanged(Sender: TObject);
begin
  SaveSettings;
end;

end.

