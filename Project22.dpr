program Project22;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  BotApi in 'BotApi.pas',
  StrUtils,
  TelegramTypes in 'TelegramTypes.pas';

procedure Echo(bot_instance : TBotApi; Msg : TelegramMessage);
var
  echoed_text : String;
  L : Integer;
begin
  L := Length(Msg.GetText);
  WriteLn('Procesing ' + Msg.GetText);
  if L >= 6 then
    if Copy(Msg.GetText, 1, 5) = '/echo' then
    begin
      echoed_text := Copy(Msg.GetText, 7, L);
      WriteLn('Echoing : ' + '[' + echoed_text + ']');
      bot_instance.SendMessage(Msg.GetChat.GetID, echoed_text);
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
  WriteLn('Write Bot token and press Enter.');
  ReadLn(Token);
  AssignFile(F, 'test.txt');
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
    while Running do
    begin
      U := Bot.GetUpdates(O, 10, 0);
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
          Echo(Bot, M);
        end;
      end;
      Sleep(3000);
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
