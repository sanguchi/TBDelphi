program EchoBot;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, BotApi, TelegramTypes, Commands, StrUtils;

type

  { BotApplication }

  BotApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  HandlerArray = array of Handler;

{ BotApplication }

procedure BotApplication.DoRun;
var
  Handlers : HandlerArray;
  ErrorMsg: String;
  Response : string;
  Bot : TBotApi;
  Token : string;
  UBot : TelegramUser;
  TokenFile : TextFile;
  Updates : UpdateArray;
  Update : TelegramUpdate;
  Running : Boolean;
  LastID : Integer;
  Index : Integer;
  Jndex : Integer;
begin

  // quick check parameters
  ErrorMsg:=CheckOptions('h,v', 'help,log');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  if HasOption('v' , 'log') then
    BotApi.DoLog := True;

  { add your program here }
  //Client := TRequests.Create;
  //WriteLn('test');
  //Page := Client.Get('https://www.example.com');
  //WriteLn(Page);
  //ReadLn;

  { TODO : Check if token file exists. }
  if BotApi.DoLog then
    WriteLn('LogMode ON')
  else
    WriteLn('LogMode OFF');
  WriteLn('Make sure there is a file called [token.txt] with the token and press enter.');
  ReadLn;
  AssignFile(TokenFile, 'token.txt');
  Reset(TokenFile);
  ReadLn(TokenFile, Token);
  Bot := TBotApi.Create(Token);
  {Init Handlers}
  SetLength(Handlers, 6);
  Handlers[0] := Echo.Create(Bot);
  Handlers[1] := Info.Create(Bot);
  Handlers[2] := Help.Create(Bot);
  Handlers[3] := Start.Create(Bot);
  Handlers[4] := Stop.Create(Bot);
  Handlers[5] := About.Create(Bot);
  Response := Format('Your token is: %s', [Token]);
  WriteLn(Response);
  WriteLn('Press Enter to make a call to GetMe.');
  ReadLn;
  try
    WriteLn('-------');
    UBot := Bot.GetMe;
    Response := Format('BotID: %u', [UBot.GetUserID]);
    WriteLn(Response);
    Response := 'Bot FirstName: ' + UBot.GetFirstName;
    WriteLn(Response);
    Response := 'Bot LastName: ' + UBot.GetLastName;
    WriteLn(Response);
    Response := 'Bot Username: ' + UBot.GetUsername;
    WriteLn(Response);
    WriteLn('-------');
    WriteLn('If this information is correct press enter, otherwise press CONTROL+C');
    ReadLn;
  except
    WriteLn('Bot.Free;');
  end;
  WriteLn('[Querying]...');
  Running := True;
  LastID := 0;
  while Running do
  begin
    Updates := Bot.GetUpdates(LastID, 10, 0);
    while Length(Updates) = 0 do
    begin
      Updates := Bot.GetUpdates(LastID, 10, 0);
    end;
    for Index := 0 to Length(Updates) -1 do
    begin
      Update := Updates[Index];
      if LastID < Update.GetUpdateID then
        LastID := Update.GetUpdateID
      else
        Continue;
      if (Update.GetMessage.GetChat.GetTitle <> '') and (Update.GetMessage.GetText <> '') then
        WriteLn('[' + Update.GetMessage.GetChat.GetTitle + ']: [' + Update.GetMessage.GetText + ']');
      if Update.GetMessage.GetText <> '' then
        for Jndex := 0 to Length(Handlers) -1 do
        begin
          if AnsiStartsStr('/' + Handlers[Jndex].TriggerWord, Update.GetMessage.GetText) then
            begin
              WriteLn('Handler ' + Handlers[Jndex].TriggerWord + ' triggered');
              Handlers[Jndex].FireEvent(Update.GetMessage);
            end;
        end;
    end;
  end;
  WriteLn('Finished, press Enter to exit.');
  ReadLn;
  // stop program loop
  //Client.Free;
  Terminate;
end;

constructor BotApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor BotApplication.Destroy;
begin
  inherited Destroy;
end;

procedure BotApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: BotApplication;
begin
  Application:=BotApplication.Create(nil);
  Application.Title:='Bot';
  Application.Run;
  Application.Free;
end.

