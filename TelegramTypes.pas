unit TelegramTypes;

interface
uses
  uLkJSON,
  SysUtils;
type
  {Custom type made to handle errors from telegram servers}
  TelegramError = class
  private
    error_code : Integer;
    description : String;
    ok : Boolean;
  published
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
  published
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
  published
    constructor Create(JSON : String);
    property GetUserID : Integer read id;
    property GetFirstName : String read first_name;
    property GetLastName : String read last_name;
    property GetUsername : String read username;
  end;

  TelegramChat = class
  private
    id : Integer;
    chat_type : String;
    title : String;
    username : String;
    first_name : String;
    last_name : String;
  published
    constructor Create(JSON : String);
    property GetID : Integer read id;
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
  published
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
  published
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
  published
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
  published
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
  published
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
  published
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
  published
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
  published
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
  published
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
  published
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
  published
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
  published
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
  published
    constructor Create(JSON : String);
    property GetTotalCount : Integer read total_count;
    property GetPhotos : PhotoSizeMatrix read photos;
  end;

  TelegramFile = class
  private
    file_id : String;
    file_size : Integer;
    file_path : String;
  published
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
  published
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
  published
    constructor Create(IsSelective : Boolean=False);
    function Serialize : String;
  end;

  TelegramForceReply = class
  private
    force_reply : Boolean;
    selective : Boolean;
  published
    constructor Create(IsSelective : Boolean=False);
    function Serialize : String;
  end;

  TelegramInputFile = class
  private
    content : file;
    file_id : String;
  published
    constructor Create(FileURL : String);
  end;

  TelegramInlineQueryResultArticle = class
  private
    result_type : String;
    id : String;
  published
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
    procedure ParseSingleUpdate(ElName: string; Elem: TlkJSONbase; data: pointer; var Continue: Boolean);
  published
    constructor Create(JSON : String);
    property GetUpdatesArray : UpdateArray read updates_array;
  end;

  PhotoSizeArrayParser = class
  private
    photosize_array : PhotoSizeArray;
    procedure ParseSinglePhotoSize(ElName: string; Elem: TlkJSONbase; data: pointer; var Continue: Boolean);
  published
    constructor Create(JSON : String);
    property GetPhotoSizeArray : PhotoSizeArray read photosize_array;
  end;

implementation

{ TelegramResponse }
constructor TelegramResponse.Create(JSON: String);
var
  JObject : TlkJSONobject;
  JResult : TlkJSONobject;
  JList : TlkJSONlist;
  FInt : Integer;
begin
  JObject := TlkJSON.ParseText(JSON) as TlkJSONobject;
  ok := JObject.Field['ok'].Value;
  if ok then
  begin
    //WriteLn(JObject.Field['result'].SelfTypeName);
    result_field := TlkJSON.GenerateText(JObject.Field['result']);
    FInt := JObject.IndexOfName('description');
    if FInt = -1 then
      description := 'No description.'
    else
      description := JObject.Field['description'].Value;
    //WriteLn('Response Succesfull Parsed.');
  end
  else
    TelegramError.Create(JSON);
end;

{ TelegramError }

constructor TelegramError.Create(JSON: String);
var
  JResult : TlkJSONobject;
  FInt : Integer;
  ErrorMessage : String;
begin
  JResult := TlkJSON.ParseText(JSON) as TlkJSONobject;
  ok := JResult.Field['ok'].Value;
  FInt := JResult.IndexOfName('description');
  if FInt = -1 then
    description := 'No description.'
  else
    description := JResult.Field['description'].Value;
  FInt := JResult.IndexOfName('error_code');
  if FInt = -1 then
    error_code := 0
  else
    error_code := JResult.Field['error_code'].Value;
  ErrorMessage := Format('Error %u: %s', [error_code, description]);
  JResult.Free;
  WriteLn('Telegram Error : ' + ErrorMessage);
  raise Exception.Create(ErrorMessage);
end;

{ UpdateArrayParser }

constructor UpdateArrayParser.Create(JSON: String);
var
  list : TlkJSONlist;
  proc : TlkJSONFuncEnum;
begin
  proc := ParseSingleUpdate;
  list := TlkJSON.ParseText(JSON) as TlkJSONlist;
  //WriteLn('list length : ' + IntToStr(list.Count));
  SetLength(updates_array, 0);
  list.ForEach(proc, nil);
end;
procedure UpdateArrayParser.ParseSingleUpdate;
var
  JSON : String;
  Updt : TelegramUpdate;
  Ind : Integer;
begin
  JSON := TlkJSON.GenerateText(Elem);
  //WriteLn('Elem : ' + JSON);
  Updt := TelegramUpdate.Create(JSON);
  Ind := Length(updates_array);
  //WriteLn('Adding Update to updates_array at index ' + IntToStr(Ind));
  SetLength(updates_array, Ind + 1);
  updates_array[High(updates_array)] := Updt;
end;
{ TelegramUpdate }

constructor TelegramUpdate.Create(JSON: String);
var
  JResult : TlkJSONobject;
  FInt : Integer;
  msgJSON : String;
begin
  JResult := TlkJSON.ParseText(JSON) as TlkJSONobject;
  FInt := JResult.IndexOfName('update_id');
  if FInt = -1 then
    update_id := 0
  else
    update_id := JResult.Field['update_id'].Value;
  FInt := JResult.IndexOfName('message');
  if FInt = -1 then
    msg := nil
  else
    msg := TelegramMessage.Create(TlkJSON.GenerateText(JResult.Field['message']));
  { TODO : Parse InlineQuery and ChosenInlineResult. }
  JResult.Free;
end;

{ TelegramUser }

constructor TelegramUser.Create(JSON: String);
var
  JResult : TlkJSONobject;
  FInt : Integer;
begin
  JResult := TlkJSON.ParseText(JSON) as TlkJSONobject;
  FInt := JResult.IndexOfName('id');
  if FInt = -1 then
    id := 0
  else
    id := JResult.Field['id'].Value;
  FInt := JResult.IndexOfName('first_name');
  if FInt = -1 then
    first_name := ''
  else
    first_name := JResult.Field['first_name'].Value;
  FInt := JResult.IndexOfName('last_name');
  if FInt = -1 then
    last_name := ''
  else
    last_name := JResult.Field['first_name'].Value;
  FInt := JResult.IndexOfName('username');
  if FInt = -1 then
    username := ''
  else
    username := JResult.Field['username'].Value;
  JResult.Free;
end;

{ TelegramChat }

constructor TelegramChat.Create(JSON: String);
var
  JResult : TlkJSONobject;
  FInt : Integer;
begin
  JResult := TlkJSON.ParseText(JSON) as TlkJSONobject;
  FInt := JResult.IndexOfName('id');
  if FInt = -1 then
    id := 0
  else
    id := JResult.Field['id'].Value;
  FInt := JResult.IndexOfName('type');
  if FInt = -1 then
    chat_type := ''
  else
    chat_type := JResult.Field['type'].Value;
  FInt := JResult.IndexOfName('title');
  if FInt = -1 then
    title := ''
  else
    title := JResult.Field['title'].Value;
  FInt := JResult.IndexOfName('first_name');
  if FInt = -1 then
    first_name := ''
  else
    first_name := JResult.Field['first_name'].Value;
  FInt := JResult.IndexOfName('last_name');
  if FInt = -1 then
    last_name := ''
  else
    last_name := JResult.Field['first_name'].Value;
  FInt := JResult.IndexOfName('username');
  if FInt = -1 then
    username := ''
  else
    username := JResult.Field['username'].Value;
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
  JResult : TlkJSONobject;
  FInt : Integer;
  PhotoParser : PhotoSizeArrayParser;
begin
  //WriteLn('Message: Begin.');
  JResult := TlkJSON.ParseText(JSON) as TlkJSONobject;
  //WriteLn('Message Parsed.');
  FInt := JResult.IndexOfName('message_id');
  if FInt = -1 then
    message_id := 0
  else
    message_id := JResult.Field['message_id'].Value;
  //WriteLn('MessageID Parsed.');
  FInt := JResult.IndexOfName('from');
  if FInt = -1 then
    from := nil
  else
    from := TelegramUser.Create(TlkJSON.GenerateText(JResult.Field['from']));
  //WriteLn('From user Parsed.');
  FInt := JResult.IndexOfName('date');
  if FInt = -1 then
    date := 0
  else
    date := JResult.Field['date'].Value;
  FInt := JResult.IndexOfName('chat');
  if FInt = -1 then
    chat := nil
  else
    chat := TelegramChat.Create(TlkJSON.GenerateText(JResult.Field['chat']));
  FInt := JResult.IndexOfName('forward_from');
  if FInt = -1 then
    forward_from := nil
  else
    forward_from := TelegramUser.Create(TlkJSON.GenerateText(JResult.Field['forward_from']));
  FInt := JResult.IndexOfName('forward_date');
  if FInt = -1 then
    forward_date := 0
  else
    forward_date := JResult.Field['forward_date'].Value;
  FInt := JResult.IndexOfName('reply_to_message');
  if FInt = -1 then
    reply_to_message := nil
  else
    reply_to_message := TelegramMessage.Create(TlkJSON.GenerateText(JResult.Field['reply_to_message']));
  FInt := JResult.IndexOfName('text');
  if FInt = -1 then
    text := ''
  else
    text := JResult.Field['text'].Value;
  FInt := JResult.IndexOfName('audio');
  if FInt = -1 then
    audio := nil
  else
    audio := TelegramAudio.Create(TlkJSON.GenerateText(JResult.Field['audio']));
  FInt := JResult.IndexOfName('document');
  if FInt = -1 then
    document := nil
  else
    document := TelegramDocument.Create(TlkJSON.GenerateText(JResult.Field['document']));
  FInt := JResult.IndexOfName('photo');
  if FInt = -1 then
    photo := nil
  else
  begin
    PhotoParser := PhotoSizeArrayParser.Create(TlkJSON.GenerateText(JResult.Field['photo']));
    photo := PhotoParser.GetPhotoSizeArray;
  end;
  FInt := JResult.IndexOfName('sticker');
  if FInt = -1 then
    photo := nil
  else
    sticker := TelegramSticker.Create(TlkJSON.GenerateText(JResult.Field['sticker']));
  FInt := JResult.IndexOfName('video');
  if FInt = -1 then
    video := nil
  else
    video := TelegramVideo.Create(TlkJSON.GenerateText(JResult.Field['video']));
  FInt := JResult.IndexOfName('voice');
  if FInt = -1 then
    voice := nil
  else
    voice := TelegramVoice.Create(TlkJSON.GenerateText(JResult.Field['voice']));
  FInt := JResult.IndexOfName('caption');
  if FInt = -1 then
    caption := ''
  else
    caption := JResult.Field['caption'].Value;
  FInt := JResult.IndexOfName('contact');
  if FInt = -1 then
    contact := nil
  else
    contact := TelegramContact.Create(TlkJSON.GenerateText(JResult.Field['contact']));
  FInt := JResult.IndexOfName('location');
  if FInt = -1 then
    location := nil
  else
    location := TelegramLocation.Create(TlkJSON.GenerateText(JResult.Field['location']));
  FInt := JResult.IndexOfName('new_chat_participant');
  if FInt = -1 then
    new_chat_participant := nil
  else
    new_chat_participant := TelegramUser.Create(TlkJSON.GenerateText(JResult.Field['new_chat_participant']));
  FInt := JResult.IndexOfName('left_chat_participant');
  if FInt = -1 then
    left_chat_participant := nil
  else
    left_chat_participant := TelegramUser.Create(TlkJSON.GenerateText(JResult.Field['left_chat_participant']));
  FInt := JResult.IndexOfName('new_chat_title');
  if FInt = -1 then
    new_chat_title := ''
  else
    new_chat_title := JResult.Field['new_chat_title'].Value;
  FInt := JResult.IndexOfName('new_chat_photo');
  if FInt = -1 then
    new_chat_photo := nil
  else
  begin
    PhotoParser := PhotoSizeArrayParser.Create(TlkJSON.GenerateText(JResult.Field['new_chat_photo']));
    new_chat_photo := PhotoParser.GetPhotoSizeArray;
  end;
  FInt := JResult.IndexOfName('delete_chat_photo');
  if FInt = -1 then
    delete_chat_photo := False
  else
    delete_chat_photo := True;
  FInt := JResult.IndexOfName('group_chat_created');
  if FInt = -1 then
    group_chat_created := False
  else
    group_chat_created := True;
  FInt := JResult.IndexOfName('supergroup_chat_created');
  if FInt = -1 then
    supergroup_chat_created := False
  else
    supergroup_chat_created := True;
  FInt := JResult.IndexOfName('channel_chat_created');
  if FInt = -1 then
    channel_chat_created := False
  else
    channel_chat_created := True;
  FInt := JResult.IndexOfName('migrate_to_chat_id');
  if FInt = -1 then
    migrate_to_chat_id := 0
  else
    migrate_to_chat_id := JResult.Field['migrate_to_chat_id'].Value;
  FInt := JResult.IndexOfName('migrate_from_chat_id');
  if FInt = -1 then
    migrate_from_chat_id := 0
  else
    migrate_from_chat_id := JResult.Field['migrate_from_chat_id'].Value;
  if assigned(PhotoParser) then
    PhotoParser.Free;
  JResult.Free;

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
  list : TlkJSONlist;
  proc : TlkJSONFuncEnum;
begin
  proc := ParseSinglePhotoSize;
  list := TlkJSON.ParseText(JSON) as TlkJSONlist;
  SetLength(photosize_array, 0);
  list.ForEach(proc, nil);
end;

procedure PhotoSizeArrayParser.ParseSinglePhotoSize;
var
  JSON : String;
  Photo : TelegramPhotoSize;
  Ind : Integer;
begin
  JSON := TlkJSON.GenerateText(Elem);
  //WriteLn('Elem : ' + JSON);
  Ind := Length(photosize_array);
  Photo := TelegramPhotoSize.Create(JSON);
  SetLength(photosize_array, Ind + 1);
  photosize_array[High(photosize_array)] := Photo;
end;

end.


