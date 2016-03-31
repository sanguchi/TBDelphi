unit Requests;
  
interface
uses
  HTTPSend,
  Classes,
  StrUtils,
  ssl_openssl,
  synautil;
type
  TRequests = class
  published
    function Get(const url: String): String;
    function Post(const url: String; params : String='') : String;
end;
function Request(const url, method : String; params : String='') : String;

implementation

function Request(const url, method : String; params : String='') : String;
var
  Lines : TStringList;
  Client : THTTPSend;
  IsOK : Boolean;
begin
Lines := TStringList.Create;
Client := THTTPSend.Create;
Result := 'Failed to connect.';
if AnsiStartsText('https', url) then
  begin
    //WriteLn('Sending HTTPS ' + method + ' request to ' + url);
    try
      Client.Sock.CreateWithSSL(TSSLOpenSSL);
      if params <> '' then
      begin
        WriteStrToStream(Client.Document, params);
        Client.MimeType := 'application/x-www-form-urlencoded';
      end;
      IsOK := Client.HTTPMethod(method, url);
      if IsOK then
        Lines.LoadFromStream(Client.Document);
    finally
      Client.Free;
    end;
  end
else if AnsiStartsText('http', url) then
  begin
    //WriteLn('Sending HTTP ' + method + ' request to ' + url);
    try
      if params <> '' then
      begin
        WriteStrToStream(Client.Document, params);
        Client.MimeType := 'application/x-www-form-urlencoded';
      end;
      IsOK := Client.HTTPMethod(method, url);
      if IsOK then
        Lines.LoadFromStream(Client.Document);
    finally
      Client.Free;
    end;
  end
else
  Result := 'Missing or Unsupported protocol.';
Result := Lines.Text;
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

