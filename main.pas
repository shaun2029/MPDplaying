unit main;

{$mode Delphi}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Menus, ComCtrls, Unix,
  Process, Settings, IniFiles, webcontrol, Mpc, Types, PlayList, LCLType;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnSearch: TBitBtn;
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
    procedure btnSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
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
    function GetPlaylist: string;
    function GetPlaylistPos: integer;
    function GetHost: string;
    function GetPort: string;
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

function TfrmMain.GetHost: string;
begin
  Result := '';

  if (Trim(frmSettings.edtHost.Text) <> '') then
  begin
    Result := Trim(frmSettings.edtHost.Text);
  end;
end;

function TfrmMain.GetPort: string;
begin
  Result := '';

  if (Trim(frmSettings.edtPort.Text) <> '') then
  begin
    Result := Trim(frmSettings.edtPort.Text);
  end;
end;

procedure TfrmMain.tmrPlayingTimer(Sender: TObject);
begin
  Update;
end;

procedure TfrmMain.btnNextClick(Sender: TObject);
begin
  MpcNext(GetHost, GetPort);
  Update;
end;

procedure TfrmMain.btnPlayFileClick(Sender: TObject);
begin
  if dlgOpenMusicFile.Execute then
  begin
    if MpcAddFileToPlaylist(GetHost, GetPort, dlgOpenMusicFile.FileName) then
    begin
      UpdatePlaylist;
      btnNext.Click;
    end;
  end;
end;

procedure TfrmMain.btnPrevClick(Sender: TObject);
begin
  MpcPrev(GetHost, GetPort);
  Update;
end;

procedure TfrmMain.btnSearchClick(Sender: TObject);
var
  frmSearch: TfrmSearch;
begin
  frmSearch := TfrmSearch.Create(Self);
  frmSearch.Host := GetHost;
  frmSearch.Port := GetPort;
  frmSearch.ShowModal;
  UpdatePlaylist;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FPlaylist := TStringList.Create;
  FWebControl := TSimpleWebControl.Create(8080);
  tmrWebControl.Enabled := True;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_LEFT) or (Key = VK_UP) then
    btnPrevClick(Self)
  else if (Key = VK_RIGHT) or (Key = VK_DOWN) then
    btnNextClick(Self);
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Lowercase(Key) = 'p' then
  begin
    btnSearchClick(Self);
    Key := #0;
  end
  else if Lowercase(Key) = 'n' then
  begin
    btnNextClick(Self);
    Key := #0;
  end;
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
begin
  Result := MpcAddFileToPlaylist(GetHost, GetPort, Filename);
end;

function TfrmMain.GetPlaylistPos: integer;
begin
  Result := MpcGetPlaylistPos(GetHost, GetPort);
end;

function TfrmMain.GetPlaylist: string;
begin
  Result := MpcGetPlaylist(GetHost, GetPort);
end;

procedure TfrmMain.UpdatePlaylist;
begin
  FPlaylist.Clear;
  Update;
end;

procedure TfrmMain.Update;
var
  Output: string;
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
      if (FPlaylistPos + i >= 0) and (FPlaylistPos + i < FPlaylist.Count) then
        mmPlayedQueue.Lines.Append(FPlaylist.Strings[FPlaylistPos + i]);
    end;

    for i := 1 to 10 do
    begin
      if (FPlaylistPos + i >= 0) and (FPlaylistPos + i < FPlaylist.Count) then
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

