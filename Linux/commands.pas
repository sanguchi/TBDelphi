unit Commands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, TelegramTypes, BotApi;
type

  { Handler }

  Handler = class
  protected
    bot : TBotApi;
    trigger : String;
  public
    constructor Create(bot_instance : TBotApi);
    procedure FireEvent(Msg : TelegramMessage); virtual; abstract;
    property TriggerWord : String read trigger;
  end;

  Echo = class(Handler)
    procedure FireEvent(Msg : TelegramMessage); override;
  end;

  Info = class(Handler)
    procedure FireEvent(Msg : TelegramMessage); override;
  end;

  Help = class(Handler)
    procedure FireEvent(Msg : TelegramMessage); override;
  end;

  Start = class(Handler)
    procedure FireEvent(Msg : TelegramMessage); override;
  end;

  Stop = class(Handler)
    procedure FireEvent(Msg : TelegramMessage); override;
  end;

  About = class(Handler)
    procedure FireEvent(Msg : TelegramMessage); override;
  end;

implementation
procedure Echo.FireEvent(Msg : TelegramMessage);
begin
  //WriteLn('Procesing ' + Msg.GetText);
  if Msg.GetText = '/' + trigger then
    bot.SendMessage(Msg.GetChat.GetID, '/echo <text>' + sLineBreak + 'Repeat text.');
  if Length(Msg.GetText) >= 6 then
    bot.SendMessage(Msg.GetChat.GetID, Copy(Msg.GetText, 7, Length(Msg.GetText)));
end;

procedure Info.FireEvent(Msg : TelegramMessage);
var
  InfoString : String;
begin
  InfoString := 'Your ID: [' + IntToStr(Msg.GetFrom.GetUserID) + ']' + sLineBreak + 'First Name[' + Msg.GetFrom.GetFirstName + ']' + sLineBreak + 'Last Name[' + Msg.GetFrom.GetLastName + ']' + sLineBreak + 'Username[@' + Msg.GetFrom.GetUsername + ']';
  if Msg.GetText = '/' + trigger then
    bot.SendMessage(Msg.GetChat.GetID, '/info' + sLineBreak+ 'Returns information about user.')
  else
    bot.SendMessage(Msg.GetChat.GetID, InfoString);
end;

procedure Help.FireEvent(Msg : TelegramMessage);
begin
  bot.SendMessage(Msg.GetChat.GetID, 'Available Commands:' + sLineBreak + '/echo' + sLineBreak + '/info' + sLineBreak + '/help' + sLineBreak + '/start' + sLineBreak + '/stop' + sLineBreak + '/about');
end;

procedure Start.FireEvent(Msg : TelegramMessage);
begin
  bot.SendMessage(Msg.GetChat.GetID, 'Hi! ' + Msg.GetFrom.GetFirstName + ', Nice to meet you!, im a bot written in pascal :D' + sLineBreak + 'You can check my commands typing /help');
end;

procedure Stop.FireEvent(Msg : TelegramMessage);
begin
  bot.SendMessage(Msg.GetChat.GetID, 'Ohhh... okay, if you want me to stay quiet... :(');
end;

procedure About.FireEvent(Msg : TelegramMessage);
begin
  bot.SendMessage(Msg.GetChat.GetID, 'var' + sLineBreak + '  text : String;' + sLineBreak + '  creator : String;' + sLineBreak + '  language : String;' + sLineBreak + 'begin' + sLineBreak + '  creator := ''@Sanguchi'';' + sLineBreak + '  language := ''PASCAL!''; ' + sLineBreak + '  text := ''If you have any issues contact me.'';' + sLineBreak + 'end;');
end;

{ Handler }

constructor Handler.Create(bot_instance: TBotApi);
begin
  bot := bot_instance;
  trigger := AnsiLowerCase(Self.ClassName);
  Log('Handler [' + trigger + '] loaded.');
end;


end.

