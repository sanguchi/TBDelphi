unit BotApi;

interface    //declaracion de las variables, procedures y functions.
uses
  SysUtils,
  Requests,
  Classes,
  TelegramTypes;
const RootUrl = 'https://api.telegram.org/bot';
type
  TBotApi = class
  private
    Token : String;
    ApiUrl : String;
    Client : TRequests;
  published
    Constructor Create(TokenString : String);
    function SendMessage(ChatID : Integer; Text : String): TelegramMessage;
    function GetUpdates(offset: Integer=0; limit: Integer=10; timeout: Integer=0): UpdateArray;
    function GetMe : TelegramUser;
  end;

implementation

{ TBot }
constructor TBotApi.Create(TokenString: string);
begin
  Token := TokenString;
  Client := TRequests.Create;
  ApiUrl := RootUrl + Token + '/';
end;

function TBotApi.GetMe: TelegramUser;
var
  MethodUrl : String;
  Response : String;
  TGram : TelegramResponse;
begin
  MethodUrl := ApiUrl + 'getMe';
  Response := Client.Get(MethodUrl);
  TGram := TelegramResponse.Create(Response);
  Result := TelegramUser.Create(TGram.GetResult);
  TGram.Free;
end;


function TBotApi.GetUpdates(offset: Integer=0; limit: Integer=10; timeout: Integer=0): UpdateArray;
var
  MethodUrl : String;
  Response : String;
  Headers : String;
  TGram : TelegramResponse;
  Parser : UpdateArrayParser;
begin
  MethodUrl := ApiUrl + 'getUpdates';
  Headers := Format('offset=%u&limit=%u&timeout=%u', [offset, limit, timeout]);
  Response := Client.Post(MethodUrl, Headers);
  TGram := TelegramResponse.Create(Response);
  //WriteLn('Tgram created');
  Parser := UpdateArrayParser.Create(TGram.GetResult);
  Result := Parser.GetUpdatesArray;
end;

function TBotApi.SendMessage(ChatID: Integer; Text: String): TelegramMessage;
var
  MethodUrl : String;
  Response : String;
  Headers : String;
  TGram : TelegramResponse;
  MSG : TelegramMessage;
begin
  MethodUrl := ApiUrl + 'sendMessage';
  Headers := Format('chat_id=%s&text=%s', [IntToStr(ChatID), Text]);
  WriteLn('Sending Message to chat ' + IntToStr(ChatID) + '. MSG : ' + Text);
  Response := Client.Post(MethodUrl, Headers);
  TGram := TelegramResponse.Create(Response);
  MSG := TelegramMessage.Create(TGram.GetResult);
  Result := MSG;
end;

end.
