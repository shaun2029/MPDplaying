//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit VolumeControl;

{$mode objfpc}{$H+}

interface

// Code to support older Lazarus/FPC
//{$define NOGUI}
//{$define LOGGING}

uses
  Process,
  {$ifndef NOGUI}
  LCLProc,
  {$else}
  process_legacy,
  {$endif}
  Classes, SysUtils, unix;

type
  {$ifdef NOGUI}
  TMplayerEQ = array of integer;
  {$endif}

  TVolumeControlType = (vcPulse, vcAlsa);

  { TMusicPlayer }

  { TVolumeControl }

  TVolumeControl  = class
  private
    FControlType: TVolumeControlType;
    FVolAttenuation: integer;
    FMixerControl: string;
    FVolume: integer;

    FMuteLevel: integer;
    procedure SetVolume(Volume: integer);
  public
    constructor Create(MixerControl : string; ControlType: TVolumeControlType);
    destructor Destroy; override;

    procedure VolumeUp;
    procedure VolumeDown;
    function GetVolume: integer;
    procedure Mute;
    procedure ToggleMute;
    procedure UnMute;
  published
  end;

implementation

constructor TVolumeControl.Create(MixerControl : string; ControlType: TVolumeControlType);
begin
  FMixerControl := MixerControl;
  FControlType := ControlType;
  FVolAttenuation := 0;

  FMuteLevel := 0;

  // Force getvolume to read the volume.
  FVolume := -1;
  FVolume := GetVolume;
end;

destructor TVolumeControl.Destroy;
begin
  inherited Destroy;
end;

procedure TVolumeControl.VolumeUp;
var
  Value: Integer;
begin
  case FVolume of
    0..1: Inc(FVolume); // for better low volume control
    2..4: FVolume := 5;
    5..100:
      begin
        Value := 5 - (FVolume mod 5);
        if Value = 0 then Value := 5;

        FVolume := FVolume + Value;
        if FVolume > 100 then FVolume := 100;
      end;
  end;

  SetVolume(FVolume);
end;

procedure TVolumeControl.VolumeDown;
var
  Value: Integer;
begin
  case FVolume of
    1..2: Dec(FVolume); // for better low volume control
    3..5: FVolume := 2;
    6..100:
      begin
        Value := FVolume mod 5;
        if Value = 0 then Value := 5;

        FVolume := FVolume - Value;
        if FVolume < 0 then FVolume := 0;
      end;
  end;

  SetVolume(FVolume);
end;

procedure TVolumeControl.SetVolume(Volume: integer);
var
  Output: string;
  CommandLine: string;
begin
  if Volume > 100 then Volume := 100
  else if Volume < 0 then Volume := 0;

  if FControlType = vcPulse then
    CommandLine := 'amixer -D pulse sset ''' + FMixerControl + ''' '
  else
    CommandLine := 'amixer -- sset ''' + FMixerControl + ''' playback ';

  CommandLine := CommandLine + IntToStr(Volume) + '%';

  RunCommand(CommandLine, Output);
end;

function TVolumeControl.GetVolume: integer;
var
  Output: string;
  CommandLine: string;
  PStart, PEnd: Integer;
begin
  Result := 100;

  if FControlType = vcPulse then
    CommandLine := 'amixer -D pulse sget ''' + FMixerControl
  else
    CommandLine := 'amixer -- sget ''' + FMixerControl + ''' playback';

  if RunCommand(CommandLine, Output) then
  begin
    PStart := Pos('[', Output) + 1;
    PEnd := Pos('%', Output);
    if (PEnd > PStart) and (PStart > 1) then
    begin
      Output := Copy(Output, PStart, PEnd - PStart);
      Result := StrToIntDef(Output, 50);
    end;
  end;
end;

procedure TVolumeControl.ToggleMute;
var
  Output: string;
  CommandLine: string;
begin
  if FControlType = vcPulse then
    CommandLine := 'amixer -D pulse set ''' + FMixerControl + ''' 1+ toggle'
  else
    CommandLine := 'amixer set ''' + FMixerControl + ''' 1+ toggle';

  RunCommand(CommandLine, Output);
end;

procedure TVolumeControl.Mute;
var
  Output: string;
  CommandLine: string;
begin
  if FControlType = vcPulse then
    CommandLine := 'amixer -D pulse set ''' + FMixerControl + ''' 1+ off'
  else
    CommandLine := 'amixer set ''' + FMixerControl + ''' 1+ off';

  RunCommand(CommandLine, Output);
end;

procedure TVolumeControl.UnMute;
var
  Output: string;
  CommandLine: string;
begin
  if FControlType = vcPulse then
    CommandLine := 'amixer -D pulse set ''' + FMixerControl + ''' 1+ on'
  else
    CommandLine := 'amixer set ''' + FMixerControl + ''' 1+ on';

  RunCommand(CommandLine, Output);
end;

end.

