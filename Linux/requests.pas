unit Requests;

{$mode objfpc}{$H+}

interface

uses
  httpsend,
  SysUtils,
  Classes,
  StrUtils,
  ssl_openssl,
  synautil;

type
  TRequests = class
  public
    function Get(const url: String): String;
    function Post(const url: String; params : String='') : String;
end;
function Request(const url, method : String; params : String='') : String;

implementation
uses
  BotApi;
function Request(const url, method : String; params : String='') : String;
var
  Lines : TStringList;
  Client : THTTPSend;
  IsOK : Boolean;
begin
Client := THTTPSend.Create;
if AnsiStartsText('https', url) then
  begin
    Log('Sending HTTPS ' + method + ' request to ' + url);
    try
      Client.Sock.CreateWithSSL(TSSLOpenSSL);
      Log('SSL Created');
      if params <> '' then
      begin
        WriteStrToStream(Client.Document, params);
        Client.MimeType := 'application/x-www-form-urlencoded';
        Log('Params Sent: [' + params + ']');
      end;
      IsOK := Client.HTTPMethod(method, url);
      if IsOK then
        begin
        Log('Connected');
        Lines := TStringList.Create;
        Lines.LoadFromStream(Client.Document)
        end
      else
        Raise Exception.Create('No Internet Connection.');
    finally
      Client.Free;
    end;
  end
else if AnsiStartsText('http', url) then
  begin
    Log('Sending HTTP ' + method + ' request to ' + url);
    try
      if params <> '' then
      begin
        WriteStrToStream(Client.Document, params);
        Client.MimeType := 'application/x-www-form-urlencoded';
        Log('Params Sent: [' + params + ']');
      end;
      IsOK := Client.HTTPMethod(method, url);
      if IsOK then
        begin
          Log('Connected');
          Lines := TStringList.Create;
          Lines.LoadFromStream(Client.Document);
        end
      else
        Raise Exception.Create('No Internet Connection.');
    finally
      Client.Free;
    end;
  end
else
  Result := 'Missing or Unsupported protocol.';
if Assigned(Lines) then
  begin
   //Log('Response = ' + Lines.Text);
   Result := Lines.Text;
  end
else
   Result := 'Empty Response';
end;

function TRequests.Get(const url : string): string;
begin
Result := Request(url , 'GET');
end;

function TRequests.Post(const url: String; params: String=''): String;
begin
Result := Request(url, 'POST', params);
end;

end.
