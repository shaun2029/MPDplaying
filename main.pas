unit main;

{$mode Delphi}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Menus, ComCtrls, Unix,
  Process, Settings, IniFiles, webcontrol, Mpc, Types, PlayList, LCLType,
  VolumeControl, MusicSkip;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnSearch: TBitBtn;
    btnPlayFile: TBitBtn;
    btnPrev: TBitBtn;
    btnNext: TBitBtn;
    GroupBox1: TGroupBox;
    grpMoodRange: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    lblVersion: TLabel;
    lblMood: TLabel;
    mmPlaying: TMemo;
    mmQueued: TMemo;
    mnuSettings: TMenuItem;
    mMenu: TMainMenu;
    mmPlayedQueue: TMemo;
    dlgOpenMusicFile: TOpenDialog;
    shpMood: TShape;
    tmrWebControl: TTimer;
    tmrPlaying: TTimer;
    procedure btnNextClick(Sender: TObject);
    procedure btnPlayFileClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure grpMoodItemClick(Sender: TObject; Index: integer);
    procedure grpMoodRangeClick(Sender: TObject);
    procedure mnuSettingsClick(Sender: TObject);
    procedure shpMoodDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure shpMoodMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure shpMoodMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure shpMoodMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure shpMoodPaint(Sender: TObject);
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
    frmSearch: TfrmSearch;
    FMoodWidth, FMoodMid: double;

    function AddFileToPlaylist(Filename: string): boolean;
    function GetPlaylist: string;
    function GetPlaylistPos: integer;
    function GetHost: string;
    function GetPort: string;
    procedure SaveSettings;
    procedure ShadeTShapeArea(Shape: TShape; Min, Max: Double);
    procedure ToggleMute;
    procedure UnMute;
    procedure UpdatePlaying;
    procedure UpdatePlaylist;
    procedure SetVolume(Up: boolean);
    { private declarations }
  public
    { public declarations }
  end;

const
  VERSION='v2.0.0';

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
  if Assigned(frmSearch) then
  begin
    if frmSearch.IsQueueUpdated then
      UpdatePlaylist
    else
      UpdatePlaying;
  end
  else  UpdatePlaying;
end;

procedure TfrmMain.UnMute;
var
  Volume: TVolumeControl;
begin
  Volume := TVolumeControl.Create('Master', vcPulse);
  Volume.UnMute;
  Volume.Free;
end;

procedure TfrmMain.ToggleMute;
var
  Volume: TVolumeControl;
begin
  Volume := TVolumeControl.Create('Master', vcPulse);
  Volume.ToggleMute;
  Volume.Free;
end;

procedure TfrmMain.btnNextClick(Sender: TObject);
begin
  MpcNext(GetHost, GetPort);
  UnMute;
  UpdatePlaying;
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
  UnMute;
  UpdatePlaying;
end;

procedure TfrmMain.btnSearchClick(Sender: TObject);
begin
  if not Assigned(frmSearch) then
  begin
    frmSearch := TfrmSearch.Create(Self);
    frmSearch.Host := GetHost;
    frmSearch.Port := GetPort;
  end;

  frmSearch.Show;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FPlaylist := TStringList.Create;
  FWebControl := TSimpleWebControl.Create(8088);
  tmrWebControl.Enabled := True;
  lblVersion.Caption := VERSION;
  frmSearch := nil;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(frmSearch) then
  begin
    frmSearch.Close;
    frmSearch.Free;
  end;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  F: TextFile;
begin
  if (Key = VK_LEFT) then
  begin
    btnPrevClick(Self);
    Key := 0;
  end
  else if (Key = VK_RIGHT) then
  begin
    btnNextClick(Self);
    Key := 0;
  end
  else if (Key = VK_UP) then
  begin
    SetVolume(True);
    Key := 0;
  end
  else if (Key = VK_DOWN) then
  begin
    SetVolume(False);
    Key := 0;
  end
  else if (Key = VK_SPACE) then
  begin
    try
      AssignFile(f, '/tmp/music.pause');
      Rewrite(f);
      CloseFile(f);
    except
      on E: Exception do;
    end;
    Key := 0;
  end
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
  UpdatePlaying;
end;

procedure TfrmMain.mnuSettingsClick(Sender: TObject);
begin
  tmrPlaying.Enabled := False;
  frmSettings.ShowModal;
  tmrPlaying.Enabled := True;
end;

procedure TfrmMain.shpMoodDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
end;

procedure TfrmMain.shpMoodMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMoodMid := X / shpMood.Width;
  FMoodMid := Round(FMoodMid * 20) / 20;
  if FMoodMid > 1 then FMoodMid := 1;
  if FMoodMid < 0 then FMoodMid := 0;

  SaveSettings;
end;

procedure TfrmMain.shpMoodMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
end;

procedure TfrmMain.shpMoodMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TfrmMain.shpMoodPaint(Sender: TObject);
var
  Min, Max: Double;
begin
  Min := FMoodMid - FMoodWidth / 2;
  Max := Min + FMoodWidth;

  ShadeTShapeArea(shpMood, Min, Max);
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
  UpdatePlaying;
end;

procedure TfrmMain.SetVolume(Up: boolean);
var
  Volume: TVolumeControl;
begin
  Volume := TVolumeControl.Create('Master', vcPulse);
  Volume.UnMute;
  if Up then
    Volume.VolumeUp
  else
    Volume.VolumeDown;

  { Reset update playing timer. }
  tmrPlaying.Enabled := False;
  tmrPlaying.Enabled := True;

  mmPlaying.Text := Format('Vol: %d %%', [Volume.GetVolume]);
  Volume.Free;
end;

procedure TfrmMain.UpdatePlaying;
var
  Output, CurrentTrackName: string;
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
    for i := -8 to -1 do
    begin
      if (FPlaylistPos + i >= 0) and (FPlaylistPos + i < FPlaylist.Count) then
        mmPlayedQueue.Lines.Append(FPlaylist.Strings[FPlaylistPos + i]);
    end;

    for i := 1 to 8 do
    begin
      if (FPlaylistPos + i >= 0) and (FPlaylistPos + i < FPlaylist.Count) then
        mmQueued.Lines.Append(FPlaylist.Strings[FPlaylistPos + i]);
    end;

    Output := FPlaylist.Strings[FPlaylistPos];
    Self.Caption := 'Playing: ' + Copy(Output, 1, 60);
    mmPlaying.Text := Output;

    CurrentTrackName := MpcGetPlayingTrackName(GetHost, GetPort);

    { Check that the playing song is correct. }
    if Pos(Output, CurrentTrackName) <> 1 then
    begin
      { Clear playlist so next update will refresh the playlist. }
      FPlaylist.Clear;
    end;
  end
  else
  begin
    { Clear playlist so next update will refresh the playlist. }
    FPlaylist.Clear;
  end;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
var
  Cfg: TIniFile;
  MusicSkip: TMusicSkip;
  Range, Min, Max: Double;
  URL: String;
begin
  URL := Format('http://%s:5000', [GetHost]);
  MusicSkip := TMusicSkip.Create(URL);

  if MusicSkip.GetSkipValues(Min, Max) then
  begin
    lblMood.Caption := Format('%.1f - %.1f', [Min*10, Max*10]);
    WriteLn('Skip values were successfully queried.')
  end
  else
  begin
    Min := 0;
    Max := 10;
    lblMood.Caption := 'error';
    WriteLn('Failed to get skip values.');
  end;

  MusicSkip.Free;

  Range := Abs(Max - Min);
  FMoodMid := Round((Min + Range / 2) * 20) / 20;
  if Range < 0.25 then grpMoodRange.ItemIndex := 0
  else if Range < 0.35 then grpMoodRange.ItemIndex := 1
  else if Range < 0.45 then grpMoodRange.ItemIndex := 2
  else if Range < 0.55 then grpMoodRange.ItemIndex := 3
  else if Range < 0.65 then grpMoodRange.ItemIndex := 4
  else if Range < 0.75 then grpMoodRange.ItemIndex := 5
  else grpMoodRange.ItemIndex := 6;
end;

procedure TfrmMain.SaveSettings;
var
  Cfg: TIniFile;
  F : TextFile;
  Range: integer;
  MusicSkip: TMusicSkip;
  URL: String;
  Min, Max: Double;
begin
  Range := grpMoodRange.ItemIndex;

  case Range of
    0: FMoodWidth := 0.2;
    1: FMoodWidth := 0.3;
    2: FMoodWidth := 0.4;
    3: FMoodWidth := 0.5;
    4: FMoodWidth := 0.6;
    5: FMoodWidth := 0.7;
    else FMoodWidth := 1;
  end;

  AssignFile(f,GetUserDir + '.music-skip');
  Rewrite(f);
  WriteLn(f, FloatToStr(FMoodWidth));
  WriteLn(f, FloatToStr(FMoodMid));
  CloseFile(f);

  shpMood.Update;

  Min := FMoodMid - FMoodWidth / 2;
  Max := Min + FMoodWidth;

  if Min < 0 then Min := 0;
  if Max > 1 then Max := 1;

  if Abs(Min - Max) < 0.19 then
  begin
    Max := Min + 0.2;
    if Min < 0 then
    begin
      Min := 0; Max := 0.2
    end;
    if Max > 1 then
    begin
      Min := 0.8;
      Max := 1.0;
    end;
  end;

  URL := Format('http://%s:5000', [GetHost]);
  MusicSkip := TMusicSkip.Create(URL);

  if MusicSkip.SetSkipValues(Min, Max) then
  begin
    lblMood.Caption := Format('%.1f - %.1f', [Min*10, Max*10]);
    WriteLn('Skip values were set successfully.')
  end
  else
  begin
    lblMood.Caption := 'error';
    WriteLn('Failed to set skip values.');
  end;

  MusicSkip.Free;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FWebControl.Free;
  FPlaylist.Free;
end;

procedure TfrmMain.grpMoodItemClick(Sender: TObject; Index: integer);
begin
end;

procedure TfrmMain.grpMoodRangeClick(Sender: TObject);
begin

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
  MoodIndex: integer;
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

    if (MoodIndex >= 0) and (MoodIndex <= 6) then
    begin
{
      case MoodIndex of
        0: begin grpMood.ItemIndex := 0; grpMoodRange.ItemIndex := 2; end;
        1: begin grpMood.ItemIndex := 1; grpMoodRange.ItemIndex := 2; end;
        2: begin grpMood.ItemIndex := 2; grpMoodRange.ItemIndex := 2; end;
        3: begin grpMood.ItemIndex := 3; grpMoodRange.ItemIndex := 2; end;
        4: begin grpMood.ItemIndex := 4; grpMoodRange.ItemIndex := 2; end;
        else begin grpMood.ItemIndex := 4; grpMoodRange.ItemIndex := 3; end;
      end;
      SaveSettings;
}
    end;
  end;

  tmrWebControl.Enabled := True;
end;

procedure TfrmMain.ShadeTShapeArea(Shape: TShape; Min, Max: Double);
var
  ShadedRect: TRect;
  MinX, MaxX: Integer;
begin
  // Ensure min and max are within the range [0, 1]
  if Min < 0 then Min := 0;
  if Max > 1 then Max := 1;

  // Calculate the pixel positions based on min and max
  MinX := Round(Min * Shape.Width);
  MaxX := Round(Max * Shape.Width);

  // Clear the shape before shading
  Shape.Canvas.Brush.Color := $00343436; // or any background color
  Shape.Canvas.FillRect(Rect(0, 0, Shape.Width, Shape.Height));

  // Set the brush color for shading
  Shape.Canvas.Brush.Color := clGray; // or any shading color

  // Define the shaded rectangle
  ShadedRect := Rect(MinX, 0, MaxX, Shape.Height);

  // Draw the shaded area
  Shape.Canvas.FillRect(ShadedRect);
end;

end.

