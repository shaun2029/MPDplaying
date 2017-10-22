unit main;

{$mode Delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, Menus, IniPropStorage, ComCtrls, Unix, Pipes, Process,
  Settings, IniFiles;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnPrev: TBitBtn;
    btnNext: TBitBtn;
    GroupBox1: TGroupBox;
    mnuSettings: TMenuItem;
    mMenu: TMainMenu;
    mmPlaying: TMemo;
    pbLevelHard: TProgressBar;
    pbLevelSoft: TProgressBar;
    rgpHard: TRadioGroup;
    rgpSoft: TRadioGroup;
    tmrPlaying: TTimer;
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure mnuSettingsClick(Sender: TObject);
    procedure tmrPlayingTimer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure pbLevelSoftContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure rgpSoftSelectionChanged(Sender: TObject);
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
   Hardness, Softness: integer;
begin
  try
    Cfg := TIniFile.Create(GetUserDir + '.music-skip.conf');
    Softness := Cfg.ReadInteger('Settings', 'Softness', 0);
    Hardness := Cfg.ReadInteger('Settings', 'Hardness', 4);

    Cfg.Free;
  except
    on E: Exception do
    begin
      ShowMessage('Exception: ' + E.Message);
    end;
  end;

  case Softness of
       0: rgpSoft.ItemIndex := 0;
       1: rgpSoft.ItemIndex := 1;
       2: rgpSoft.ItemIndex := 2;
       3: rgpSoft.ItemIndex := 3;
       else rgpSoft.ItemIndex := 4;
  end;

  case Hardness of
       0: rgpHard.ItemIndex := 0;
       1: rgpHard.ItemIndex := 1;
       2: rgpHard.ItemIndex := 2;
       3: rgpHard.ItemIndex := 3;
       else rgpHard.ItemIndex := 4;
  end;
end;

procedure TfrmMain.SaveSettings;
var
  F : TextFile;
   Soft, Hard: integer;
begin

  case rgpSoft.ItemIndex of
       0: Soft := 0;
       1: Soft := 4;
       2: Soft := 8;
       3: Soft := 10;
       else Soft := 12;
  end;

  case rgpHard.ItemIndex of
       0: Hard := 8;
       1: Hard := 11;
       2: Hard := 13;
       3: Hard := 15;
       else Hard := 20;
  end;

  if Hard < Soft + 2 then
     Hard := Soft + 2;

  if Hard >= 16 then
     Hard := 20;

  pbLevelSoft.Position := Soft;
  pbLevelHard.Position := Hard;

  // Ensure that all music can be played.
  if Soft <= 0 then
     Soft := -20;

  AssignFile(f,GetUserDir + '.music-skip');
  Rewrite(f);
  WriteLn(f, FloatToStr(Soft));
  WriteLn(f, FloatToStr(Hard));
  CloseFile(f);
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
   Cfg: TIniFile;
begin
  try
    Cfg := TIniFile.Create(GetUserDir + '.music-skip.conf');
    Cfg.WriteInteger('Settings', 'Softness', rgpSoft.ItemIndex);
    Cfg.WriteInteger('Settings', 'Hardness', rgpHard.ItemIndex);
    Cfg.Free;
  except
    on E: Exception do
    begin
      ShowMessage('Exception: ' + E.Message);
    end;
  end;

  SaveSettings;
end;

procedure TfrmMain.pbLevelSoftContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

procedure TfrmMain.rgpSoftSelectionChanged(Sender: TObject);
begin
  SaveSettings;
end;

end.

