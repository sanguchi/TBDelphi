program Project22;

{$MODE Delphi}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  BotApi in 'BotApi.pas',
  StrUtils,
  TelegramTypes in 'TelegramTypes.pas';

function Echo(bot_instance : TBotApi; Msg : TelegramMessage): Boolean;
var
  echoed_text : String;
  L : Integer;
begin
  Result := True;
  L := Length(Msg.GetText);
  //WriteLn('Procesing ' + Msg.GetText);
  if L >= 6 then
  begin
    if Copy(Msg.GetText, 1, 5) = '/echo' then
    begin
      echoed_text := Copy(Msg.GetText, 7, L);
      WriteLn('Echoing : ' + '[' + echoed_text + ']');
      bot_instance.SendMessage(Msg.GetChat.GetID, echoed_text);
    end;
  end;
  if Msg.GetText = '/stop' then
  begin
    Result := False;
    WriteLn('STOP command received, Bot is going to shutdown now!');
  end;
end;
var
  Response : String;
  Name : String;
  Bot : TBotApi;
  Token : String;
  UBot : TelegramUser;
  F : TextFile;
  U : UpdateArray;
  L : Integer;
  I : Integer;
  O : Integer;
  TU : TelegramUpdate;
  M : TelegramMessage;
  Running : Boolean;
begin
  { TODO -oUser -cConsole Main : Insert code here }
  { TODO : Check if token file exists. }
  Running := True;
  O := 0;
  WriteLn('Make sure there is a file called [token.txt] with the token and press enter.');
  ReadLn;
  AssignFile(F, 'token.txt');
  Reset(F);
  ReadLn(F, Token);
  Bot := TBotApi.Create(Token);
  Response := Format('Your token is: %s', [Token]);
  WriteLn(Response);
  WriteLn('Press Enter to make a call to GetMe.');
  ReadLn;
  try
    UBot := Bot.GetMe;
    Response := Format('BotID: %u', [UBot.GetUserID]);
    WriteLn(Response);
    Response := 'Bot FirstName: ' + UBot.GetFirstName;
    WriteLn(Response);
    Response := 'Bot LastName: ' + UBot.GetLastName;
    WriteLn(Response);
    Response := 'Bot Username: ' + UBot.GetUsername;
    WriteLn(Response);
    WriteLn('Press Enter to Start Querying TelegramAPI.');
    ReadLn;
    WriteLn('[Querying] commands supported: [/echo, /stop].');
    U := Bot.GetUpdates(O, 10, 0);
    L := Length(U);
    TU := U[0];
    O := TU.GetUpdateID;
    while Running do
    begin
      L := Length(U);
      for I:=0 to L - 1 do
      begin
        TU := U[I];
        if O = TU.GetUpdateID then
          Continue
        else
        O := TU.GetUpdateID;
        M := TU.GetMessage;
        Response := M.GetText;
        Name := M.GetChat.GetTitle;
        if (Response <> '') and (Name <> '') then
        begin
          WriteLn('[' + Name + ']: [' + Response + ']');
          if not Echo(Bot, M) then
             Running := False;
        end;
      end;
      Sleep(3000);
      U := Bot.GetUpdates(O, 10, 0);
    end;
    {for I:=0 to L - 1 do
    begin
      //WriteLn('Press Enter to iterate over item ' + IntToStr(I) + '.');
      //ReadLn;
      TU := U[I];
      //WriteLn('Item ' + IntToStr(I) + ' Assignated, Press Enter.');
      if not Assigned(TU) then
      begin
        WriteLn('TU is not assigned');
        ReadLn;
        Break;
      end;
      M := TU.GetMessage;
      Response := M.GetText;
      WriteLn('Text of Message ' + IntToStr(I) + ': [' + Response + ']');
      WriteLn('---------------Press Enter----------------');
      ReadLn;
    end;
    WriteLn('Press Enter to call sendMessage with ChatID 59802458 and text "RADtest"');
    ReadLn;
    M := Bot.SendMessage(59802458, 'RADtest');
    if M.GetText = 'RADtest' then
      WriteLn('Sent Succesfully.')
    else
      WriteLn('Errors while sending message.');
    //Response := Bot.GetUpdates(0, 1, 0);
    //WriteLn(Response);
    }
  finally
    Bot.Free;
  end;
  WriteLn('Finished, press Enter to exit.');
  ReadLn;
end.
