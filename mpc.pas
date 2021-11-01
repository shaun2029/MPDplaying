unit mpc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Dialogs;

procedure MpcNext(Host, Port: string);
procedure MpcPrev(Host, Port: string);
function MpcAddFileToPlaylist(Host, Port, Filename: string): boolean;
function MpcAddTrackToPlaylist(Host, Port, Name: string): boolean;
function MpcGetPlaylistPos(Host, Port: string): integer;
function MpcGetPlaylist(Host, Port: string): string;
function MpcSearch(Host, Port, Artist, Title: string): string;
function MpcGetPlayingTrackName(Host, Port: string): string;

implementation

function MpcGetMPDHostParams(Host, Port: string): string;
begin
  Result := '';

  if (Host <> '') then
  begin
    Result := 'MPD_HOST=' + Host;
  end;

  if (Port <> '') then
  begin
    Result := Result + ' MPD_Port=' + Port;
  end;
end;

procedure MpcNext(Host, Port: string);
var
  Output: string;
  CommandLine: string;
begin
  CommandLine := 'bash -c "' + MpcGetMPDHostParams(Host, Port) + ' mpc next"';
  RunCommand(CommandLine, Output);
end;

procedure MpcPrev(Host, Port: string);
var
  Output: string;
  CommandLine: string;
begin
  CommandLine := 'bash -c "' + MpcGetMPDHostParams(Host, Port) + ' mpc prev"';
  RunCommand(CommandLine, Output);
end;

function MpcGetPlaylistPos(Host, Port: string): integer;
var
  Output: string;
  CommandLine: string;
  Outputs:TStringlist;
begin
  Result := -1;

  CommandLine := 'bash -c "' + MpcGetMPDHostParams(Host, Port) + ' mpc | grep playing | awk ''{print $2}'' | sed ''s/\/.*//g'' | sed ''s/#//g'' "';
  if (RunCommand(CommandLine, Output)) and (Output<>'') then
  begin
    Outputs := TStringlist.Create;
    Outputs.Text := Output;
    Output := Outputs.Strings[0];
    FreeAndNil(Outputs);

    Result := StrToIntDef(Output, -1);
  end;
end;

function MpcGetPlayingTrackName(Host, Port: string): string;
var
  Output: string;
  CommandLine: string;
  Outputs:TStringlist;
begin
  Result := '';

  CommandLine := 'bash -c "' + MpcGetMPDHostParams(Host, Port) + ' mpc | head -n 1 "';
  if (RunCommand(CommandLine, Output)) and (Output<>'') then
  begin
    Outputs := TStringlist.Create;
    Outputs.Text := Output;
    Output := Outputs.Strings[0];
    FreeAndNil(Outputs);

    Result := Output;
  end;
end;

function MpcGetPlaylist(Host, Port: string): string;
var
  Output: string;
  CommandLine: string;
  Outputs:TStringlist;
begin
  Result := '';

  CommandLine := 'bash -c "' + MpcGetMPDHostParams(Host, Port) + ' mpc playlist ' + '"';
  if (RunCommand(CommandLine, Output)) and (Output<>'') then
  begin
    Result := Output;
  end;
end;

function MpcSearch(Host, Port, Artist, Title: string): string;
var
  Output: string;
  CommandLine: string;
  Outputs:TStringlist;
begin
  Result := '';

  CommandLine := 'bash -c "' + MpcGetMPDHostParams(Host, Port) + ' mpc search artist ''' + artist + ''' title ''' + Title + '''"';
  if (RunCommand(CommandLine, Output)) and (Output<>'') then
  begin
    Result := Output;
  end;
end;

function MpcAddFileToPlaylist(Host, Port, Filename: string): boolean;
var
  AProcess     : TProcess;
  i : longint;
  Output: String;
  F : File Of byte;
  Size: int64;
begin
  Result := False;
  // Set up the process; as an example a recursive directory search is used
  // because that will usually result in a lot of data.
  AProcess := TProcess.Create(nil);
  AProcess.Executable := '/usr/bin/mpc';

  if Host <> '' then
  begin
    AProcess.Parameters.Add('-h');
    AProcess.Parameters.Add(Host);
  end;

  if Port <> '' then
  begin
    AProcess.Parameters.Add('-p');
    AProcess.Parameters.Add(Port);
  end;

  AProcess.Parameters.Add('insert');
  AProcess.Parameters.Add('file://' + Filename);

  // Process option poUsePipes has to be used so the output can be captured.
  // Process option poWaitOnExit can not be used because that would block
  // this program, preventing it from reading the output data of the process.
  AProcess.Options := [poUsePipes];
  AProcess.ShowWindow := swoHide;

  // Start the process (run the dir/ls command)
  AProcess.Execute;

  Output := '';

  // All generated output from AProcess is read in a loop until no more data is available
  repeat
    if AProcess.StdErr.NumBytesAvailable > 0 then
    begin
      // Get the new data from the process to a maximum of the buffer size that was allocated.
      // Note that all read(...) calls will block except for the last one, which returns 0 (zero).
      Output := Output + Char(AProcess.StdErr.ReadByte);
    end
    else Sleep(40);
  until (not AProcess.Running) and (AProcess.StdErr.NumBytesAvailable < 1);

  if (AProcess.ExitStatus <> 0) then
  begin
    MessageDlg('Failed to add file', Output, mtError, [mbOk], 0);
  end
  else Result := True;
end;

function MpcAddTrackToPlaylist(Host, Port, Name: string): boolean;
var
  AProcess     : TProcess;
  Progress, i : longint;
  ProgressStrings: TStringList;
  FileID, FileName, Output: String;
  F : File Of byte;
  Size: int64;
begin
  Result := False;
  // Set up the process; as an example a recursive directory search is used
  // because that will usually result in a lot of data.
  AProcess := TProcess.Create(nil);
  AProcess.Executable := '/usr/bin/mpc';

  if Host <> '' then
  begin
    AProcess.Parameters.Add('-h');
    AProcess.Parameters.Add(Host);
  end;

  if Port <> '' then
  begin
    AProcess.Parameters.Add('-p');
    AProcess.Parameters.Add(Port);
  end;

  AProcess.Parameters.Add('insert');
  AProcess.Parameters.Add(Name);

  // Process option poUsePipes has to be used so the output can be captured.
  // Process option poWaitOnExit can not be used because that would block
  // this program, preventing it from reading the output data of the process.
  AProcess.Options := [poUsePipes];
  AProcess.ShowWindow := swoHide;

  // Start the process (run the dir/ls command)
  AProcess.Execute;

  Output := '';

  // All generated output from AProcess is read in a loop until no more data is available
  repeat
    if AProcess.StdErr.NumBytesAvailable > 0 then
    begin
      // Get the new data from the process to a maximum of the buffer size that was allocated.
      // Note that all read(...) calls will block except for the last one, which returns 0 (zero).
      Output := Output + Char(AProcess.StdErr.ReadByte);
    end
    else Sleep(40);
  until (not AProcess.Running) and (AProcess.StdErr.NumBytesAvailable < 1);

  if (AProcess.ExitStatus <> 0) then
  begin
    MessageDlg('Failed to add track', Output, mtError, [mbOk], 0);
  end
  else Result := True;
end;

end.

