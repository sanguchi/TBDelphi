unit BotApi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Requests, TelegramTypes;

const RootUrl = 'https://api.telegram.org/bot';
var
  DoLog : Boolean = False;
type
  TBotApi = class
  private
    Token : String;
    ApiUrl : String;
    Client : TRequests;
  public
    Constructor Create(TokenString : String);
    function SendMessage(ChatID : Integer; Text : String): TelegramMessage;
    function GetUpdates(offset: Integer=0; limit: Integer=10; timeout: Integer=0): UpdateArray;
    function GetMe : TelegramUser;
  end;

procedure Log(text : String);

implementation
procedure Log(text : String);
begin
  if DoLog then
    WriteLn(text);
end;

{ TBot }
constructor TBotApi.Create(TokenString: string);
begin
  Token := TokenString;
  Client := TRequests.Create;
  ApiUrl := RootUrl + Token + '/';
  Log('Bot created, ApiUrl: [' + ApiUrl + ']');
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
  Log(Format('BotApi: GetUpdates[Offset(%u)|Limit(%u)|Timeout(%u)]', [offset, limit, timeout]));
  MethodUrl := ApiUrl + 'getUpdates';
  Log('  MethodUrl = ' + MethodUrl);
  Headers := Format('offset=%u&limit=%u&timeout=%u', [offset, limit, timeout]);
  Log('  Headers = ' + Headers);
  Response := Client.Post(MethodUrl, Headers);
  Log('  Response received.');
  TGram := TelegramResponse.Create(Response);
  Log('  Tgram Object Created.');
  Parser := UpdateArrayParser.Create(TGram.GetResult);
  Log('  UpdateArrayParser Created');
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
  Log('Sending Message to chat ' + IntToStr(ChatID) + '. MSG : ' + Text);
  Response := Client.Post(MethodUrl, Headers);
  TGram := TelegramResponse.Create(Response);
  MSG := TelegramMessage.Create(TGram.GetResult);
  Result := MSG;
end;


end.

