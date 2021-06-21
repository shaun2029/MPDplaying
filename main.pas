unit main;

{$mode Delphi}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Menus, ComCtrls, Unix,
  Process, Settings, IniFiles, webcontrol;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnPlayFile: TBitBtn;
    btnPrev: TBitBtn;
    btnNext: TBitBtn;
    grpMood: TCheckGroup;
    GroupBox1: TGroupBox;
    mmPlaying: TMemo;
    mmQueued: TMemo;
    mnuSettings: TMenuItem;
    mMenu: TMainMenu;
    mmPlayedQueue: TMemo;
    dlgOpenMusicFile: TOpenDialog;
    tmrWebControl: TTimer;
    tmrPlaying: TTimer;
    procedure btnNextClick(Sender: TObject);
    procedure btnPlayFileClick(Sender: TObject);
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
    procedure tmrWebControlTimer(Sender: TObject);
  private
    FPlaylist: TStringList;
    FPlaylistPos: integer;
    FWebControl: TSimpleWebControl;
    function AddFileToPlaylist(Filename: string): boolean;
    function GetMPDHostParams: string;
    function GetPlaylist: string;
    function GetPlaylistPos: integer;
    procedure SaveSettings;
    procedure Update;
    procedure UpdatePlaylist;
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

procedure TfrmMain.btnPlayFileClick(Sender: TObject);
begin
  if dlgOpenMusicFile.Execute then
  begin
    if AddFileToPlaylist(dlgOpenMusicFile.FileName) then
    begin
      UpdatePlaylist;
      btnNext.Click;
    end;
  end;
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
  FPlaylist := TStringList.Create;
  FWebControl := TSimpleWebControl.Create(8080);
  tmrWebControl.Enabled := True;
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

function TfrmMain.AddFileToPlaylist(Filename: string): boolean;
var
  Output: string;
  CommandLine: string;
  Outputs:TStringlist;
begin
  Result := False;

  CommandLine := 'bash -c ''' + GetMPDHostParams + ' mpc insert "file://' + Filename + '"''';
  if (RunCommand(CommandLine, Output)) and (Output = '') then
  begin
    Result := True;
{
    Outputs := TStringlist.Create;
    Outputs.Text := Output;
    Output := Outputs.Strings[0];
    FreeAndNil(Outputs);

    if (mmPlayedQueue.Lines.IndexOf(Output) <> 0) then
    begin
      Self.Caption := 'Playing: ' + Copy(Output, 1, 60);
      mmPlayedQueue.Lines.Insert(0, Output);
      if mmPlayedQueue.Lines.Count > 12 then
        mmPlayedQueue.Lines.Delete(mmPlayedQueue.Lines.Count-1);

      FWebControl.Playing := Self.Caption;
}
{
      CommandLine := 'notify-send -t 10000 "Playing: " "' + Output + '"';
      RunCommand(CommandLine, Output);
}
  end;
end;

function TfrmMain.GetPlaylistPos: integer;
var
  Output: string;
  CommandLine: string;
  Outputs:TStringlist;
begin
  CommandLine := 'bash -c "' + GetMPDHostParams + ' mpc | grep playing | awk ''{print $2}'' | sed ''s/\/.*//g'' | sed ''s/#//g'' "';
  if (RunCommand(CommandLine, Output)) and (Output<>'') then
  begin
    Outputs := TStringlist.Create;
    Outputs.Text := Output;
    Output := Outputs.Strings[0];
    FreeAndNil(Outputs);

    Result := StrToIntDef(Output, -1);
  end;
end;

function TfrmMain.GetPlaylist: string;
var
  Output: string;
  CommandLine: string;
  Outputs:TStringlist;
begin
  CommandLine := 'bash -c "' + GetMPDHostParams + ' mpc playlist ' + '"';
  if (RunCommand(CommandLine, Output)) and (Output<>'') then
  begin
    Result := Output;
  end;
end;

procedure TfrmMain.UpdatePlaylist;
var
  Output: string;
  CommandLine: string;
  Outputs:TStringlist;
  i: integer;
begin
  FPlaylist.Clear;
  Update;
end;

procedure TfrmMain.Update;
var
  Output: string;
  CommandLine: string;
  Outputs:TStringlist;
  i: integer;
begin
  if FPlaylist.Count = 0 then
  begin
    FPlaylist.Text := GetPlaylist;
  end;

  FPlaylistPos := GetPlaylistPos - 1;
  mmPlayedQueue.Clear;
  mmQueued.Clear;

  if (FPlaylistPos >= 0) and (FPlaylistPos < FPlaylist.Count) then
  begin
    for i := -10 to -1 do
    begin
      if (FPlaylistPos + 1 >= 0) and (FPlaylistPos + 1 < FPlaylist.Count) then
        mmPlayedQueue.Lines.Append(FPlaylist.Strings[FPlaylistPos + i]);
    end;

    for i := 1 to 10 do
    begin
      if (FPlaylistPos + 1 >= 0) and (FPlaylistPos + 1 < FPlaylist.Count) then
        mmQueued.Lines.Append(FPlaylist.Strings[FPlaylistPos + i]);
    end;

    Output := FPlaylist.Strings[FPlaylistPos];
    Self.Caption := 'Playing: ' + Copy(Output, 1, 60);
    mmPlaying.Text := Output;
  end
  else
  begin
    FPlaylist.Text := GetPlaylist;
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
  Min, Max, i: integer;
  Soft, Hard: double;
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

  Soft := Min * 3.5;
  Hard := (Max + 1) * 3.5;

  // Ensure that all music can be played.
  if Soft < 0 then
     Soft := 0;

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
  FWebControl.Free;
  SaveSettings;
  FPlaylist.Free;
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

procedure TfrmMain.tmrWebControlTimer(Sender: TObject);
var
  MoodIndex, i: integer;
  Command: TRemoteCommand;
begin
  tmrWebControl.Enabled := False;
  FWebControl.ProccessConnections;

  Command := FWebControl.Command;

  if Command <> rcomNone then
  begin
    case Command of
      rcomNext: btnNextClick(Self);
      rcomPrevious: btnPrevClick(Self);
    end;

    MoodIndex := Ord(Command) - Ord(rcomMoodLight);

    if (MoodIndex >= 0) and (MoodIndex < 6) then
    begin
      grpMood.Checked[MoodIndex] := not grpMood.Checked[MoodIndex];
      SaveSettings;
    end;
  end;

  for i := 0 to grpMood.Items.Count - 1 do
  begin
    FWebControl.Mood[i] := grpMood.Checked[i];
  end;

  tmrWebControl.Enabled := True;
end;

end.

