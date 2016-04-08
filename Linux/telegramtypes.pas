unit TelegramTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type
  {Custom type made to handle errors from telegram servers}
  TelegramError = class
  private
    error_code : Integer;
    description : String;
    ok : Boolean;
  public
    constructor Create(JSON : String);
    property GetDescription : String read description;
    property GetErrorCode : Integer read error_code;
    property GetOK : Boolean read ok;
  end;

  TelegramResponse = class
  private
    ok : Boolean;
    description : String;
    result_field : String;
  public
    constructor Create(JSON : String);
    property GetOK : Boolean read ok;
    property GetDescription : String read description;
    property GetResult : String read result_field;
  end;

  TelegramUser = class
  private
    id : Integer;
    first_name : String;
    last_name : String;
    username : String;
  public
    constructor Create(JSON : String);
    property GetUserID : Integer read id;
    property GetFirstName : String read first_name;
    property GetLastName : String read last_name;
    property GetUsername : String read username;
  end;

  TelegramChat = class
  private
    id : Int64;
    chat_type : String;
    title : String;
    username : String;
    first_name : String;
    last_name : String;
  public
    constructor Create(JSON : String);
    property GetID : Int64 read id;
    property GetType : String read chat_type;
    property GetTitle : String read title;
    property GetUsername : String read username;
    property GetFirstName : String read first_name;
    property GetLastName : String read last_name;
  end;

  TelegramPhotoSize = class
  private
    file_id : String;
    width : Integer;
    height : Integer;
    file_size : Integer;
  public
    constructor Create(JSON : String);
    property GetFileID : String read file_id;
    property GetWidth : Integer read width;
    property GetHeight : Integer read height;
    property GetFileSize : Integer read file_size;
  end;

  TelegramAudio = class
  private
    file_id : String;
    duration : Integer;
    performer : String;
    title : String;
    mime_type : String;
    file_size : Integer;
  public
    constructor Create(JSON : String);
    property GetFileID : String read file_id;
    property GetDuration : Integer read duration;
    property GetPerformer : String read performer;
    property GetTitle : String read title;
    property GetMimeType : String read mime_type;
    property GetFileSize : Integer read file_size;
  end;

  TelegramDocument = class
  private
    file_id : String;
    thumb : TelegramPhotoSize;
    file_name : String;
    mime_type : String;
    file_size : Integer;
  public
    constructor Create(JSON : String);
    property GetFileID : String read file_id;
    property GetThumb : TelegramPhotoSize read thumb;
    property GetFileName : String read file_name;
    property GetMimeType : String read mime_type;
    property GetFileSize : Integer read file_size;
  end;

  TelegramSticker = class
  private
    file_id : String;
    width : Integer;
    height : Integer;
    thumb : TelegramPhotoSize;
    file_size : Integer;
  public
    constructor Create(JSON : String);
    property GetFileID : String read file_id;
    property GetWidth : Integer read width;
    property GetHeight : Integer read height;
    property GetThumb : TelegramPhotoSize read thumb;
    property GetFileSize : Integer read file_size;
  end;

  TelegramVideo = class
  private
    file_id : String;
    width : Integer;
    height : Integer;
    duration : Integer;
    thumb : TelegramPhotoSize;
    mime_type : String;
    file_size : Integer;
  public
    constructor Create(JSON : String);
    property GetFileID : String read file_id;
    property GetWidth : Integer read width;
    property GetHeight : Integer read height;
    property GetDuration : Integer read duration;
    property GetThumb : TelegramPhotoSize read thumb;
    property GetMimeType : String read mime_type;
    property GetFileSize : Integer read file_size;
  end;

  TelegramVoice = class
  private
    file_id : String;
    duration : Integer;
    mime_type : String;
    file_size : Integer;
  public
    constructor Create(JSON : String);
    property GetFileID : String read file_id;
    property GetDuration : Integer read duration;
    property GetMimeType : String read mime_type;
    property GetFileSize : Integer read file_size;
  end;

  TelegramContact = class
  private
    phone_number : String;
    first_name : String;
    last_name : String;
    user_id : Integer;
  public
    constructor Create(JSON : String);
    property GetPhoneNumber : String read phone_number;
    property GetFirstName : String read first_name;
    property GetLastName : String read last_name;
    property GetUserID : Integer read user_id;
  end;

  TelegramLocation = class
  private
    longitude : Single;
    latitude : Single;
  public
    constructor Create(JSON : String);
    property GetLongitude : Single read longitude;
    property GetLatitude : Single read latitude;
  end;

  PhotoSizeArray = array of TelegramPhotoSize;

  TelegramMessage = class
  private
    message_id : Integer;
    from : TelegramUser;
    date : Integer;
    chat : TelegramChat;
    forward_from : TelegramUser;
    forward_date : Integer;
    reply_to_message : TelegramMessage;
    text : String;
    audio : TelegramAudio;
    document : TelegramDocument;
    photo : PhotoSizeArray;
    sticker : TelegramSticker;
    video : TelegramVideo;
    voice : TelegramVoice;
    caption : String;
    contact : TelegramContact;
    location : TelegramLocation;
    new_chat_participant : TelegramUser;
    left_chat_participant : TelegramUser;
    new_chat_title : String;
    new_chat_photo : PhotoSizeArray;
    delete_chat_photo : Boolean;
    group_chat_created : Boolean;
    supergroup_chat_created : Boolean;
    channel_chat_created : Boolean;
    migrate_to_chat_id : Integer;
    migrate_from_chat_id : Integer;
  public
    constructor Create(JSON : String);
    property GetMessageID : Integer read message_id;
    property GetFrom : TelegramUser read from;
    property GetDate : Integer read date;
    property GetChat : TelegramChat read chat;
    property GetForwardFrom : TelegramUser read forward_from;
    property GetForwardDate : Integer read forward_date;
    property GetReplyToMessage : TelegramMessage read reply_to_message;
    property GetText : String read text;
    property GetAudio : TelegramAudio read audio;
    property GetDocument : TelegramDocument read document;
    property GetPhoto : PhotoSizeArray read photo;
    property GetSticker : TelegramSticker read sticker;
    property GetVideo : TelegramVideo read video;
    property GetVoice : TelegramVoice read voice;
    property GetCaption : String read caption;
    property GetContact : TelegramContact read contact;
    property GetLocation : TelegramLocation read location;
    property GetNewChatParticipant : TelegramUser read new_chat_participant;
    property GetLeftChatParticipant : TelegramUser read left_chat_participant;
    property GetNewChatTitle : String read new_chat_title;
    property GetNewChatPhoto : PhotoSizeArray read new_chat_photo;
    property GetDeleteChatPhoto : Boolean read delete_chat_photo;
    property GetGroupChatCreated : Boolean read group_chat_created;
    property GetSupergroupChatCreated : Boolean read supergroup_chat_created;
    property GetChannelChatCreated : Boolean read channel_chat_created;
    property GetMigrateToChatID : Integer read migrate_to_chat_id;
    property GetMigrateFromChatID : Integer read migrate_from_chat_id;
  end;

  TelegramInlineQuery = class
  private
    id : String;
    from : TelegramUser;
    query : String;
    offset : String;
  public
    constructor Create(JSON : String);
    property GetID : String read id;
    property GetFrom : TelegramUser read from;
    property GetQuery : String read query;
    property GetOffset : String read offset;
  end;

  TelegramChosenInlineResult = class
  private
    result_id : String;
    from : TelegramUser;
    query : String;
  public
    constructor Create(JSON : String);
    property GetResultID : String read result_id;
    property GetFrom : TelegramUser read from;
    property GetQuery : String read query;
  end;

  TelegramUpdate = class
  private
    update_id : Integer;
    msg : TelegramMessage;
    inline_query : TelegramInlineQuery;
    chosen_inline_result : TelegramChosenInlineResult;
  public
    constructor Create(JSON : String);
    property GetUpdateID : Integer read update_id;
    property GetMessage : TelegramMessage read msg;
    property GetInlineQuery : TelegramInlineQuery read inline_query;
    property GetChosenInlineResult : TelegramChosenInlineResult read chosen_inline_result;
  end;

  PhotoSizeMatrix = array of PhotoSizeArray;

  TelegramUserProfilePhotos = class
  private
    total_count : Integer;
    photos : PhotoSizeMatrix;
  public
    constructor Create(JSON : String);
    property GetTotalCount : Integer read total_count;
    property GetPhotos : PhotoSizeMatrix read photos;
  end;

  TelegramFile = class
  private
    file_id : String;
    file_size : Integer;
    file_path : String;
  public
    constructor Create(JSON : String);
    property GetFileID : String read file_id;
    property GetFileSize : Integer read file_size;
    property GetFilePath : String read file_path;
  end;

  TelegramReplyKeyboardMarkup = class
  private
    keyboard : array of array of String;
    resize_keyboard : Boolean;
    one_time_keyboard : Boolean;
    selective : Boolean;
  public
    constructor Create();
    procedure AddRow(Buttons : array of String);
    property Resize : Boolean read resize_keyboard write resize_keyboard;
    property OneTime : Boolean read one_time_keyboard write one_time_keyboard;
    property IsSelective : Boolean read selective write selective;
    function Serialize : String;
  end;

  TelegramReplyKeyboardHide = class
  private
    hide_keyboard : Boolean;
    selective : Boolean;
  public
    constructor Create(IsSelective : Boolean=False);
    function Serialize : String;
  end;

  TelegramForceReply = class
  private
    force_reply : Boolean;
    selective : Boolean;
  public
    constructor Create(IsSelective : Boolean=False);
    function Serialize : String;
  end;

  TelegramInputFile = class
  private
    content : file;
    file_id : String;
  public
    constructor Create(FileURL : String);
  end;

  TelegramInlineQueryResultArticle = class
  private
    result_type : String;
    id : String;
  public
    constructor Create(query_id : Integer);
  public
    title : String;
    message_text : String;
    parse_mode : String;
    disable_web_page_preview : String;
    url : String;
    hide_url : Boolean;
    description : String;
    thumb_url : String;
    thumb_width : Integer;
    thumb_height : Integer;
  end;

  UpdateArray = array of TelegramUpdate;

  UpdateArrayParser = class
  private
    updates_array : UpdateArray;
  public
    constructor Create(JSON : String);
    property GetUpdatesArray : UpdateArray read updates_array;
  end;

  PhotoSizeArrayParser = class
  private
    photosize_array : PhotoSizeArray;
  public
    constructor Create(JSON : String);
    property GetPhotoSizeArray : PhotoSizeArray read photosize_array;
  end;

implementation
uses
  BotApi;
{ TelegramResponse }
constructor TelegramResponse.Create(JSON: String);
var
  JObject : TJSONObject;
begin
  JObject := TJSONObject(GetJSON(JSON));
  ok := JObject.Get('ok', False);
  if ok then
  begin
    result_field := JObject.Extract('result').AsJSON;
    description := JObject.Get('description', TJSONStringType('No Description'));
    Log('TelegramResponse: Succesfull Parsed.');
    //Log('Full JSON:');
    //Log(JObject.FormatJSON);
    //Log('result field:');
    //Log(result_field);
  end
  else
    TelegramError.Create(JSON);
  JObject.Free;
end;

{ TelegramError }

constructor TelegramError.Create(JSON: String);
var
  JResult : TJSONObject;
  ErrorMessage : String;
begin
  JResult := TJSONObject(GetJSON(JSON));
  ok := JResult.Get('ok', False);
  description := JResult.Get('description', TJSONStringType('No Description'));
  error_code := JResult.Get('error_code', 0);
  ErrorMessage := Format('Error %u: %s', [error_code, description]);
  Log('Telegram Error : ' + ErrorMessage);
  raise Exception.Create(ErrorMessage);
  JResult.Free;
end;

{ UpdateArrayParser }

constructor UpdateArrayParser.Create(JSON: String);
var
  list : TJSONArray;
  I : Integer;
begin
  //Log('UpdateArrayParser: JSON = ' + JSON);
  list := GetJSON(JSON) as TJSONArray;
  Log('UpdateArrayParser: list length : ' + IntToStr(list.Count));
  SetLength(updates_array, list.Count);
  for I:=0 to list.Count -1 do
    begin
      Log('UpdateArrayParser: Parsing message number ' + IntToStr(I));
      updates_array[I] := TelegramUpdate.Create(list[I].AsJSON);
    end;
end;

{ TelegramUpdate }

constructor TelegramUpdate.Create(JSON: String);
var
  JResult : TJSONObject;
  JData : TJSONData;
begin
  Log('TelegramUpdate: Parser Started.');
  JResult := TJSONObject(GetJSON(JSON));
  update_id := JResult.Get('update_id', 0);
  Log('TelegramUpdate: UpdateID parsed');
  //LAST CRASH IS HERE
  jData := JResult.Extract('message');
  if Assigned(JData) then
    msg := TelegramMessage.Create(JData.AsJSON)
  else
    msg := nil;
  {
  if Assigned(JResult.Extract('message')) then
    msg := TelegramMessage.Create(JResult.Extract('message').AsJSON)
  else
    msg := nil;
  }
  Log('TelegramUpdate: Message Parsed.');
  { TODO : Parse InlineQuery and ChosenInlineResult. }
  JResult.Free;
end;

{ TelegramUser }

constructor TelegramUser.Create(JSON: String);
var
  JResult : TJSONObject;
begin
  JResult := TJSONObject(GetJSON(JSON));
  id := JResult.Get('id', 0);
  first_name := JResult.Get('first_name', TJSONStringType(''));
  last_name := JResult.Get('last_name', TJSONStringType(''));
  username := JResult.Get('username', TJSONStringType(''));
  Log('TelegramUser: Successfully Parsed.');
  JResult.Free;
end;

{ TelegramChat }

constructor TelegramChat.Create(JSON: String);
var
  JResult : TJSONObject;
begin
  JResult := TJSONObject(GetJSON(JSON));
  id := JResult.Get('id', 0);
  chat_type := JResult.Get('type', TJSONStringType(''));
  title := JResult.Get('title', TJSONStringType(''));
  first_name := JResult.Get('first_name', TJSONStringType(''));
  last_name := JResult.Get('last_name', TJSONStringType(''));
  username := JResult.Get('username', TJSONStringType(''));
  Log('TelegramChat: Successfully Parsed.');
  JResult.Free;
end;

{ TelegramPhotoSize }

constructor TelegramPhotoSize.Create(JSON: String);
begin

end;

{ TelegramAudio }

constructor TelegramAudio.Create(JSON: String);
begin

end;

{ TelegramDocument }

constructor TelegramDocument.Create(JSON: String);
begin

end;

{ TelegramSticker }

constructor TelegramSticker.Create(JSON: String);
begin

end;

{ TelegramVideo }

constructor TelegramVideo.Create(JSON: String);
begin

end;

{ TelegramVoice }

constructor TelegramVoice.Create(JSON: String);
begin

end;

{ TelegramContact }

constructor TelegramContact.Create(JSON: String);
begin

end;

{ TelegramLocation }

constructor TelegramLocation.Create(JSON: String);
begin

end;

{ TelegramMessage }

constructor TelegramMessage.Create(JSON: String);
var
  JResult : TJSONObject;
  jData : TJSONData;
begin
  Log('Message: Parser start');
  JResult := TJSONObject(GetJSON(JSON));

  message_id := JResult.Get('message_id', 0);
  Log('Message: message_id Parsed.');

  jData := JResult.Extract('from');
  if Assigned(jData) then
    from := TelegramUser.Create(jData.AsJSON)
  else
    from := nil;
  Log('Message: from Parsed.');

  date := JResult.Get('date', 0);
  Log('Message: date parsed');

  jData := JResult.Extract('chat');
  if Assigned(jData) then
    chat := TelegramChat.Create(jData.AsJSON)
  else
    chat := nil;
  Log('Message: chat parsed');

  jData := JResult.Extract('forward_from');
  if Assigned(jData) then
    forward_from := TelegramUser.Create(jData.AsJSON)
  else
    forward_from := nil;
  Log('Message: forward_from parsed');

  forward_date := JResult.Get('forward_date', 0);
  Log('Message: forward_date parsed');

  jData := JResult.Extract('reply_to_message');
  if Assigned(jData) then
    reply_to_message := TelegramMessage.Create(jData.AsJSON)
  else
    reply_to_message := nil;
  Log('Message: reply_to_message parsed');

  text := JResult.Get('text', TJSONStringType(''));
  Log('Message: text parsed');

  jData := JResult.Extract('audio');
  if Assigned(jData) then
    audio := TelegramAudio.Create(jData.AsJSON)
  else
    audio := nil;
  Log('Message: audio parsed');

  jData := JResult.Extract('document');
  if Assigned(jData) then
    document := TelegramDocument.Create(jData.AsJSON)
  else
    document := nil;
  Log('Message: document parsed');

  //  PhotoParser := PhotoSizeArrayParser.Create(JResult.Get('photo', GetJSON('{"result" : null}').AsJSON));
  //  photo := PhotoParser.GetPhotoSizeArray;

  jData := JResult.Extract('photo');
  if Assigned(jData) then
    photo := PhotoSizeArrayParser.Create(jData.AsJSON).GetPhotoSizeArray
  else
    photo := nil;
  Log('Message: photo parsed');

  jData := JResult.Extract('sticker');
  if Assigned(jData) then
    sticker := TelegramSticker.Create(jData.AsJSON)
  else
    sticker := nil;
  Log('Message: sticker parsed');

  jData := JResult.Extract('video');
  if Assigned(jData) then
    video := TelegramVideo.Create(jData.AsJSON)
  else
    video := nil;
  Log('Message: video parsed');

  jData := JResult.Extract('voice');
  if Assigned(jData) then
    voice := TelegramVoice.Create(jData.AsJSON)
  else
    voice := nil;
  Log('Message: voice parsed');

  caption := JResult.Get('caption', TJSONStringType(''));
  Log('Message: caption parsed');

  jData := JResult.Extract('contact');
  if Assigned(jData) then
    contact := TelegramContact.Create(jData.AsJSON)
  else
    contact := nil;
  Log('Message: contact parsed');

  jData := JResult.Extract('location');
  if Assigned(jData) then
    location := TelegramLocation.Create(jData.AsJSON)
  else
    location := nil;
  Log('Message: location parsed');

  jData := JResult.Extract('new_chat_participant');
  if Assigned(jData) then
    new_chat_participant := TelegramUser.Create(jData.AsJSON)
  else
    new_chat_participant := nil;
  Log('Message: new_chat_participant parsed');

  jData := JResult.Extract('left_chat_participant');
  if Assigned(jData) then
    left_chat_participant := TelegramUser.Create(jData.AsJSON)
  else
    left_chat_participant := nil;
  Log('Message: left_chat_participant parsed');

  new_chat_title := JResult.Get('new_chat_title', TJSONStringType(''));
  Log('Message: new_chat_title parsed');

  {I'll keep this code, idk if i'll need it someday.}
  //  PhotoParser := PhotoSizeArrayParser.Create(JResult.Get('new_chat_photo').AsJSON);
  //  new_chat_photo := PhotoParser.GetPhotoSizeArray;

  jData := JResult.Extract('new_chat_photo');
  if Assigned(jData) then
    new_chat_photo := PhotoSizeArrayParser.Create(jData.AsJSON).GetPhotoSizeArray
  else
    new_chat_photo := nil;
  Log('Message: new_chat_photo parsed');

  delete_chat_photo := JResult.Get('delete_chat_photo', False);
  Log('Message: delete_chat_photo parsed');

  group_chat_created := JResult.Get('group_chat_created', False);
  Log('Message: group_chat_created parsed');

  supergroup_chat_created := JResult.Get('supergroup_chat_created', False);
  Log('Message: supergroup_chat_created parsed');

  channel_chat_created := JResult.Get('channel_chat_created', False);
  Log('Message: channel_chat_created parsed');

  migrate_to_chat_id := JResult.Get('migrate_to_chat_id', 0);
  Log('Message: migrate_to_chat_id parsed');

  migrate_from_chat_id := JResult.Get('migrate_from_chat_id', 0);
  Log('Message: migrate_from_chat_id parsed');

  Log('Message: Deleting Jresult');
  JResult.Free;
  Log('Message: Deleting jData');
  jData.Free;
  Log('Message: Parser Finalized.');
end;

{ TelegramInlineQuery }

constructor TelegramInlineQuery.Create(JSON: String);
begin

end;

{ TelegramChosenInlineResult }

constructor TelegramChosenInlineResult.Create(JSON: String);
begin

end;

{ TelegramUserProfilePhotos }

constructor TelegramUserProfilePhotos.Create(JSON: String);
begin

end;

{ TelegramFile }

constructor TelegramFile.Create(JSON: String);
begin

end;

{ TelegramReplyKeyboardMarkup }

procedure TelegramReplyKeyboardMarkup.AddRow(Buttons: array of String);
begin

end;

constructor TelegramReplyKeyboardMarkup.Create;
begin

end;

function TelegramReplyKeyboardMarkup.Serialize: String;
begin

end;

{ TelegramReplyKeyboardHide }

constructor TelegramReplyKeyboardHide.Create(IsSelective: Boolean);
begin

end;

function TelegramReplyKeyboardHide.Serialize: String;
begin

end;

{ TelegramForceReply }

constructor TelegramForceReply.Create(IsSelective: Boolean);
begin

end;

function TelegramForceReply.Serialize: String;
begin

end;

{ TelegramInputFile }

constructor TelegramInputFile.Create(FileURL: String);
begin

end;

{ TelegramInlineQueryResultArticle }

constructor TelegramInlineQueryResultArticle.Create(query_id: Integer);
begin

end;

{ PhotoSizeArrayParser }

constructor PhotoSizeArrayParser.Create(JSON: String);
var
  list : TJSONArray;
  I : Integer;
begin
  Log('PhotoSizeArrayParser: Starting Parser.');
  list := GetJSON(JSON) as TJSONArray;
  SetLength(photosize_array, list.Count);
  for I:=0 to list.Count -1 do
    begin
      Log('PhotoSizeArrayParser: Parsing TelegramPhotoSize at index ' + IntToStr(I));
      photosize_array[I] := TelegramPhotoSize.Create(list[I].AsJSON);
    end;
  Log('PhotoSizeArrayParser: Parser terminated.');
end;

end.

