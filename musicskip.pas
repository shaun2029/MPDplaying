unit MusicSkip;

{$mode Delphi}

interface

uses
  httpsend,         // Synapse component for HTTP requests
  ssl_openssl,      // For HTTPS support
  fpjson,           // For parsing JSON
  jsonparser,       // For the JSON parser
  sysutils,         // For exception handling
  classes;          // For TStringStream

type
  TMusicSkip = class
  private
    FServerURL: string;
  public
    constructor Create(const ServerURL: string);
    // Method to set the skip values using the API
    function SetSkipValues(skipMin, skipMax: Double): Boolean;
    // Method to get the skip values from the API
    function GetSkipValues(out skipMin, skipMax: Double): Boolean;
    // Helper method to make an HTTP POST request
    function PostRequest(const URL: string; const Data: string): Boolean;
  end;

implementation

{ TMusicSkip }

constructor TMusicSkip.Create(const ServerURL: string);
begin
  FServerURL := ServerURL;
end;

// Method to set the skip values using the API
function TMusicSkip.SetSkipValues(skipMin, skipMax: Double): Boolean;
var
  JSONData: TJSONObject;
  PostData: string;
begin
  Result := False;
  // Create a JSON object with skipMin and skipMax
  JSONData := TJSONObject.Create;
  try
    JSONData.Add('skipMin', skipMin);
    JSONData.Add('skipMax', skipMax);
    PostData := JSONData.AsJSON;

    // Send the POST request with the JSON data
    if PostRequest(FServerURL + '/set_skip_values', PostData) then
    begin
      WriteLn('Skip values set successfully.');
      Result := True;
    end
    else
      WriteLn('Error setting skip values.');
  finally
    JSONData.Free;
  end;
end;

// Method to get the skip values from the API
function TMusicSkip.GetSkipValues(out skipMin, skipMax: Double): Boolean;
var
  HTTPSender: THTTPSend;
  ResponseStream: TStringStream;
  JSONData: TJSONData;
  JSONObject: TJSONObject;
begin
  Result := False;  // Default to false in case of failure
  skipMin := 0.0;
  skipMax := 1.0;

  // Create the HTTPSender instance and response stream to capture the API output
  HTTPSender := THTTPSend.Create;
  ResponseStream := TStringStream.Create('');
  try
    // Send the GET request to the API
    if HTTPSender.HTTPMethod('GET', FServerURL + '/get_skip_values') then
    begin
      // Load the response into the response stream
      ResponseStream.LoadFromStream(HTTPSender.Document);
      WriteLn('Response: ', ResponseStream.DataString);

      // Parse the JSON response
      try
        JSONData := GetJSON(ResponseStream.DataString);
        if JSONData.JSONType = jtObject then
        begin
          JSONObject := TJSONObject(JSONData);
          // Extract skipMin and skipMax values
          skipMin := JSONObject.Get('skipMin', 0.0);
          skipMax := JSONObject.Get('skipMax', 1.0);
          Result := True;
        end;
      except
        on E: Exception do
        begin
          WriteLn('Error parsing JSON: ', E.Message);
        end;
      end;
    end
    else
    begin
      WriteLn('Failed to connect to the server.');
    end;
  finally
    HTTPSender.Free;
    ResponseStream.Free;
    if Assigned(JSONData) then
      JSONData.Free;
  end;
end;

// Helper method to make an HTTP POST request
function TMusicSkip.PostRequest(const URL: string; const Data: string): Boolean;
var
  HTTPSender: THTTPSend;
  DataStream: TStringStream;
begin
  Result := False;
  HTTPSender := THTTPSend.Create;
  DataStream := TStringStream.Create(Data, TEncoding.UTF8);
  try
    HTTPSender.Document.LoadFromStream(DataStream);
    HTTPSender.MimeType := 'application/json';

    if HTTPSender.HTTPMethod('POST', URL) then
    begin
      Result := True;
    end
    else
    begin
      WriteLn('Error posting data to ', URL);
    end;
  finally
    HTTPSender.Free;
    DataStream.Free;
  end;
end;

end.
