unit webcontrol;

{$mode delphi}

interface

uses
  LCLProc,
  process_legacy,
  Classes, blcksock, sockets, Synautil, SysUtils, SyncObjs;

type
  { TSimpleWebControl }
  TRemoteCommand = (rcomNone, rcomNext, rcomPrevious, rcomMoodLight,
    rcomMoodMellow, rcomMoodRock, rcomMoodRocking, rcomMoodMoshing, rcomMoodDeath);

  TSimpleWebControl = class
    private
      FCommand: TRemoteCommand;
      FCritical: TCriticalSection;
      ListenerSocket, ConnectionSocket: TTCPBlockSocket;
      FPlaying: string;

      procedure AttendConnection(ASocket: TTCPBlockSocket);
      procedure CreateFile(Filename: string);
      function GetCommand: TRemoteCommand;
      function GetGateway: string;
      procedure SetPlaying(const AValue: string);
      function UppercaseFirstChar(s: String): String;
    public
      Mood: array [0..5] of boolean;

      constructor Create(Port: integer); overload;
      destructor Destroy; override;

      procedure ProccessConnections;

      procedure Lock();
      procedure Unlock();
    published
      property Command: TRemoteCommand read GetCommand;
      property Playing: string write SetPlaying;
  end;

implementation

constructor TSimpleWebControl.Create(Port: integer);
var
  i: integer;
begin
  inherited Create;

  FPlaying := '';
  for i := 0 to High(Mood) do
  begin
    Mood[i] := False;
  end;

  FCritical := TCriticalSection.Create;

  ListenerSocket := TTCPBlockSocket.Create;
  ConnectionSocket := TTCPBlockSocket.Create;

  ListenerSocket.CreateSocket;
  ListenerSocket.SetLinger(True, 10);
  ListenerSocket.Bind('0.0.0.0', IntToStr(Port));

  if (ListenerSocket.LastError) <> 0 then
  begin
      WriteLn('Error: Failed to start HTTP control on ports 80 and 8080');
  end
  else
  begin
     WriteLn('Started HTTP control on port ' + IntToStr(Port));
  end;

  ListenerSocket.listen;
end;

destructor TSimpleWebControl.Destroy;
begin
  ListenerSocket.Free;
  ConnectionSocket.Free;

  inherited Destroy;
end;

procedure TSimpleWebControl.ProccessConnections;
begin
  if ListenerSocket.canread(0) then
  begin
    ConnectionSocket.Socket := ListenerSocket.accept;
    //WriteLn('Attending Connection. Error code (0=Success): ', ConnectionSocket.lasterror);
    AttendConnection(ConnectionSocket);
    ConnectionSocket.CloseSocket;
  end;
end;

procedure TSimpleWebControl.Lock;
begin
  FCritical.Enter;
end;

procedure TSimpleWebControl.Unlock;
begin
  FCritical.Leave;
end;

function TSimpleWebControl.UppercaseFirstChar(s: String): String;
var
  ch, rest: String;
begin
  ch := Copy(s, 1, 1);
  rest := Copy(s, Length(ch)+1, MaxInt);
  Result := Uppercase(ch) + rest
end;

function TSimpleWebControl.GetGateway: string;
begin
  Result := '';
  RunCommand('bash -c "route -n | grep 0.0.0.0 | awk  ''{ print $2 }'' | grep -v "0.0.0.0" | head -n 1"', Result);
end;

{@@
  Attends a connection. Reads the headers and gives an
  appropriate response
}
procedure TSimpleWebControl.AttendConnection(ASocket: TTCPBlockSocket);
var
  timeout: integer;
  s: string;
  method, uri, protocol: string;
  OutputDataString, HostName: string;
  i: Integer;
  Command: TRemoteCommand;
  LocalConnection: boolean;
  ConnectionIP: string;
begin
  timeout := 12000;

//  WriteLn('Received headers+document from browser:');

  //read request line
  s := ASocket.RecvString(timeout);

  ConnectionIP :=  ASocket.GetRemoteSinIP;
  //LocalConnection := not (ConnectionIP = GetGateway());
  LocalConnection := False;

//  WriteLn(s);
  method := fetch(s, ' ');
  uri := fetch(s, ' ');
  protocol := fetch(s, ' ');
{
  //read request headers
  repeat
    s := ASocket.RecvString(Timeout);
    WriteLn(s);
  until s = '';
}
  // Now write the document to the output stream

  Command := rcomNone;

  OutputDataString := '';

  if (uri = '/playing?') or (uri = '/playing') then
  begin
    OutputDataString :=
        FPlaying;
    // Write the document back to the browser
    ASocket.SendString(OutputDataString);
  end
  else if (uri = '/next?') or (uri = '/next') then
  begin
    Command := rcomNext;
  end
  else if (uri = '/previous?') or (uri = '/previous') then
  begin
    Command := rcomPrevious;
  end
  else if (uri = '/light?') or (uri = '/light') then
  begin
    Command := rcomMoodLight;
  end
  else if (uri = '/mellow?') or (uri = '/mellow') then
  begin
    Command := rcomMoodMellow;
  end
  else if (uri = '/rock?') or (uri = '/rock') then
  begin
    Command := rcomMoodRock;
  end
  else if (uri = '/rocking?') or (uri = '/rocking') then
  begin
    Command := rcomMoodRocking;
  end
  else if (uri = '/moshing?') or (uri = '/moshing') then
  begin
    Command := rcomMoodMoshing;
  end
  else if (uri = '/death?') or (uri = '/death') then
  begin
    Command := rcomMoodDeath;
  end;

  if (Command <> rcomNone) then
  begin
    Lock();
    FCommand := Command;
    Unlock();

    // Write the document back to the browser
    ASocket.SendString('OK' + CRLF);
  end
  else if (OutputDataString = '') then
  begin
    // Write the output document to the stream
    OutputDataString :=
      '<!DOCTYPE HTML>' + CRLF
      + '<html>' + CRLF
      + '<head>' + CRLF
      + '    <title>RadioGaga ' + UppercaseFirstChar(HostName) + '</title>' + CRLF
      + '    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />' + CRLF
      + '    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0" />' + CRLF
      + '</head>' + CRLF
      + '<body>' + CRLF
      + '<font size="6">' + CRLF
      + '<h3>RadioGaga ' + UppercaseFirstChar(HostName) +'</h3>' + CRLF
      + '<p id="display" >Updating...</p>' + CRLF
      + '</font>' + CRLF;

      if (LocalConnection) then
      begin
        OutputDataString := OutputDataString
      + '<audio controls src="http://192.168.0.2:8000/mpd"></audio>' + CRLF
      end
      else
      begin
        OutputDataString := OutputDataString
        + '<audio controls src="http://www.immutablefix.co.uk:5588/mpd"></audio>' + CRLF
      end;

      if (uri = '/control?') or (uri = '/control') then
      begin
        OutputDataString := OutputDataString
        + '<br>' + CRLF
        + '<button id="previous" style="padding: 15px 40px;"><font size="6">< |</font></button>' + CRLF
        + '<button id="next" style="padding: 15px 40px;"><font size="6">| ></font></button>' + CRLF
        + '<br>' + CRLF
        + '<h2>Mood Selection</h2>' + CRLF
        + '<input type="checkbox" name="light" id="light"><font size="5">Light</font>' + CRLF
        + '<input type="checkbox" name="mellow" id="mellow"><font size="5">Mellow</font>' + CRLF
        + '<input type="checkbox" name="rock" id="rock"><font size="5">Rock</font>' + CRLF
        + '<input type="checkbox" name="rocking" id="rocking"><font size="5">Rocking</font>' + CRLF
        + '<input type="checkbox" name="moshing" id="moshing"><font size="5">Moshing</font>' + CRLF
        + '<input type="checkbox" name="death" id="death"><font size="5">Death</font>' + CRLF;
      end;

        OutputDataString := OutputDataString
        + '<script>' + CRLF
        + 'var HttpClient = function() {' + CRLF
        + '    this.get = function(aUrl, aCallback) {' + CRLF
        + '        var anHttpRequest = new XMLHttpRequest();' + CRLF
        + '        anHttpRequest.onreadystatechange = function() {' + CRLF
        + '            if (anHttpRequest.readyState == 4 && anHttpRequest.status == 200)' + CRLF
        + '                aCallback(anHttpRequest.responseText);' + CRLF
        + '        }' + CRLF
        + '' + CRLF
        + '        anHttpRequest.open( "GET", aUrl, true );' + CRLF
        + '        anHttpRequest.send( null );' + CRLF
        + '    }' + CRLF
        + '}' + CRLF
        + '' + CRLF
        + 'function updatePlaying() {' + CRLF
        + '    var client = new HttpClient();' + CRLF
        + '    client.get(''/playing'', function(response) {' + CRLF
        + '        document.getElementById("display").innerHTML = response;' + CRLF
        + '    });' + CRLF
        + '' + CRLF
        + '    setTimeout(updatePlaying, 2000);' + CRLF
        + '}' + CRLF
        + '' + CRLF
        + 'setTimeout(updatePlaying,2000);' + CRLF
        + '' + CRLF;

      if (uri = '/control?') or (uri = '/control') then
      begin
        Lock();
        if (Mood[0]) then
           OutputDataString := OutputDataString + 'document.getElementById("light").checked = true;' + CRLF
        else
            OutputDataString := OutputDataString + 'document.getElementById("light").checked = false;' + CRLF;

        if (Mood[1]) then
           OutputDataString := OutputDataString + 'document.getElementById("mellow").checked = true;' + CRLF
        else
            OutputDataString := OutputDataString + 'document.getElementById("light").checked = false;' + CRLF;

        if (Mood[2]) then
           OutputDataString := OutputDataString + 'document.getElementById("rock").checked = true;' + CRLF
        else
            OutputDataString := OutputDataString + 'document.getElementById("rock").checked = false;' + CRLF;

        if (Mood[3]) then
           OutputDataString := OutputDataString + 'document.getElementById("rocking").checked = true;' + CRLF
        else
            OutputDataString := OutputDataString + 'document.getElementById("rocking").checked = false;' + CRLF;

        if (Mood[4]) then
           OutputDataString := OutputDataString + 'document.getElementById("moshing").checked = true;' + CRLF
        else
            OutputDataString := OutputDataString + 'document.getElementById("moshing").checked = false;' + CRLF;

        if (Mood[5]) then
           OutputDataString := OutputDataString + 'document.getElementById("death").checked = true;' + CRLF
        else
            OutputDataString := OutputDataString + 'document.getElementById("death").checked = false;' + CRLF;
        Unlock();

        OutputDataString := OutputDataString
        + '' + CRLF
        + 'var checkboxs = document.getElementsByTagName("input");' + CRLF
        + 'var checkboxsCount = checkboxs.length;' + CRLF
        + 'for (var i = 0; i < checkboxsCount; i += 1) {' + CRLF
        + '    checkboxs[i].onclick = function(e) {' + CRLF
        + '        var client = new HttpClient();' + CRLF
        + '        client.get(''/'' + this.id, function(response) {' + CRLF
        + '        });' + CRLF
        + '    };' + CRLF
        + '}' + CRLF
        + '' + CRLF
        + 'var buttons = document.getElementsByTagName("button");' + CRLF
        + 'var buttonsCount = buttons.length;' + CRLF
        + 'for (var i = 0; i < buttonsCount; i += 1) {' + CRLF
        + '    buttons[i].onclick = function(e) {' + CRLF
        + '        var client = new HttpClient();' + CRLF
        + '        client.get(''/'' + this.id, function(response) {' + CRLF
        + '        });' + CRLF
        + '    };' + CRLF
        + '}' + CRLF;
      end;

      OutputDataString := OutputDataString
      + '</script>' + CRLF
      + '</body>' + CRLF
      + '</html>' + CRLF;

    // Write the headers back to the client
    ASocket.SendString('HTTP/1.0 200' + CRLF);
    ASocket.SendString('Content-type: Text/Html' + CRLF);
    ASocket.SendString('Content-length: ' + IntTostr(Length(OutputDataString)) + CRLF);
    ASocket.SendString('Connection: close' + CRLF);
    ASocket.SendString('Date: ' + Rfc822DateTime(now) + CRLF);
    ASocket.SendString('Server: RadioGaga' + CRLF);
    ASocket.SendString('' + CRLF);

    // Write the document back to the browser
    ASocket.SendString(OutputDataString);
  end;
end;

procedure TSimpleWebControl.CreateFile(Filename: string);
var
  FS: TFileStream = nil;
  Flags: Word;
begin
  try
    if not FileExists(Filename) then
    begin
      Flags := fmOpenReadWrite or fmCreate;
      FS := TFileStream.Create(Filename, Flags);
    end;
  finally
    if Assigned(FS) then
      FS.Free;
  end;
end;

function TSimpleWebControl.GetCommand: TRemoteCommand;
begin
  Lock();
  Result := FCommand;
  FCommand := rcomNone;
  Unlock();
end;

procedure TSimpleWebControl.SetPlaying(const AValue: string);
begin
  Lock();
  FPlaying := AValue;
  Unlock();
end;


end.

