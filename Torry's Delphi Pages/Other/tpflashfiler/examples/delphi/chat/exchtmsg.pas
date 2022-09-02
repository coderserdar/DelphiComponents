{*********************************************************}
{* FlashFiler: Chat message & command handler definitions*}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
 
unit ExChtMsg;

interface

uses
  Classes,
  StdCtrls,
  SysUtils,
  Windows,
  FFLLBase,
  FFLLComm,
  FFLLReq,
  FFNetMsg;

const
  ffnmChatText = ffnmUser + 100;
  ffnmChatUsers = ffnmUser + 101;

  ffc_Private = 'Private: ';


type
  { We create one instance of this record per chat user. }
  PffChatUser = ^TffChatUser;
  TffChatUser = packed record
    ClientID : TffClientID;
    UserName : TffName;
  end;

  { Message text sent from client to server or vice versa. }
  PffnmChatText = ^TffnmChatText;
  TffnmChatText = packed record
    IsPrivate : boolean;
    UserName : TffName;
      { If private and going to server then this is the name of the
        intended recipient.  If private and coming from server then this
        is the name of the sender.  If public and going to server then this
        is blank.  If public and coming form server then this is the name of
        the sender. }
    Text : string[255];
  end;

  PffnmChatUsers = ^TffnmChatUsers;
  TffnmChatUsers = packed record
    UserList : TffVarMsgField;
  end;

  { The following class handles commands on the chat server.
    Text messages are routed to all chat users or to a specific chat user
      if the message is marked private.
    When a new client connects, all clients are notified and an updated user
      list is distributed to each client.
    When a client disconnects, all clients are notified and an updated user
      list is distributed to each client. }
  TffChatSrvHandler = class(TffBaseCommandHandler)
  protected
    FUsers : TffPointerList;
    FMemo : TMemo;
    procedure scInitialize; override;
    procedure scPrepareForShutdown; override;
    procedure scShutdown; override;
    procedure scStartup; override;
    procedure SendUserList;
    procedure nmCheckSecureComms(var Msg : TffDataMessage);
              message ffnmCheckSecureComms;
    procedure nmChatText(var Msg : TffDataMessage);
              message ffnmChatText;
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    procedure OnAddClient(Sender : TffBaseTransport;
                    const userID : TffName;
                    const timeout : longInt;
                    const clientVersion : longInt;
                      var passwordHash : TffWord32;
                      var aClientID : TffClientID;
                      var errorCode : TffResult;
                      var isSecure : boolean;
                      var aVersion : longInt);
    procedure OnRemoveClient(Sender : TffBaseTransport;
                       const aClientID : TffClientID;
                         var errorCode : TffResult);
    procedure Process(Msg : PffDataMessage); override;
    property Memo : TMemo read FMemo write FMemo;
  end;

  { The following class handles commands on the chat client.
    Received text messages are displayed in the memo.
    Received user lists are used to populate a listbox. }
  TffChatClntHandler = class(TffBaseCommandHandler)
  protected
    FOutput   : TListBox;
    FUserList : TListBox;
    procedure scInitialize; override;
    procedure scPrepareForShutdown; override;
    procedure scShutdown; override;
    procedure scStartup; override;
    procedure nmChatText(var Msg : TffDataMessage);
              message ffnmChatText;
    procedure nmChatUsers(var Msg : TffDataMessage);
              message ffnmChatUsers;
  public
    procedure Process(Msg : PffDataMessage); override;
    property Output : TListBox read FOutput write FOutput;
    property UserList : TListBox read FUserList write FUserList;
  end;

implementation

{===TffChatSrvHandler===================================================}
constructor TffChatSrvHandler.Create(aOwner : TComponent);
begin
  inherited;
  FUsers := TffPointerList.Create;
end;
{--------}
destructor TffChatSrvHandler.Destroy;
var
  Index : longInt;
  PUserRec : PffChatUser;
begin
  { Free any user records still in our list. }
  for Index := pred(FUsers.Count) downto 0 do begin
   PUserRec := FUsers.Pointers[Index];
   Dispose(PUserRec);
  end;
  FUsers.Free;
  inherited Destroy;
end;
{--------}
procedure TffChatSrvHandler.scInitialize;
begin
  { Do nothing }
end;
{--------}
procedure TffChatSrvHandler.scPrepareForShutdown;
begin
  { Do nothing }
end;
{--------}
procedure TffChatSrvHandler.scShutdown;
begin
  { Do nothing }
end;
{--------}
procedure TffChatSrvHandler.scStartup;
begin
  { Do nothing }
end;
{--------}
procedure TffChatSrvHandler.SendUserList;
var
  aTransport : TffBaseTransport;
  Index : longInt;
  UserList : TStringList;
  UserStream : TMemoryStream;
begin

  { Get the current transport. }
  aTransport := TffBaseTransport.CurrentTransport;

  UserList := TStringList.Create;
  try
    for Index := 0 to pred(FUsers.Count) do
      UserList.Add(PffChatUser(FUsers.Pointers[Index])^.UserName);
    UserList.Sort;

    { Create a stream to hold the users. }
    UserStream := TMemoryStream.Create;
    try
      UserList.SaveToStream(UserStream);

      { Send the user list to everyone. }
      for Index := 0 to pred(aTransport.ConnectionCount) do
        aTransport.Post(0, aTransport.ConnectionIDs[Index], ffnmChatUsers,
                        UserStream.Memory, UserStream.Size, 1000,
                        ffrmNoReplyExpected);

    finally
      UserStream.Free;
    end;
  finally
    UserList.Free;
  end;
end;
{--------}
procedure TffChatSrvHandler.OnAddClient
                               (Sender : TffBaseTransport;
                          const userID : TffName;
                          const timeout : longInt;
                          const clientVersion : longInt;
                            var passwordHash : TffWord32;
                            var aClientID : TffClientID;
                            var errorCode : TffResult;
                            var isSecure : boolean;
                            var aVersion : longInt);
var
  aName : TffName;
  Index : longInt;
  PUserRec : PffChatUser;

  { We use this function to determine if another chat user has already
    registered this user's name. }
  function NameUsed(const newName : TffName) : boolean;
  var
    Index : longInt;
  begin
    Result := False;
    for Index := 0 to pred(FUsers.Count) do begin
      Result := (FFAnsiCompareText(PffChatUser(FUsers.Pointers[Index])^.UserName, {!!.07}
                 newName) = 0);
      if Result then
        break;
    end;
  end;

begin
  passwordHash := 0;
  errorCode := 0;
  isSecure := False;
  aVersion := FFVersionNumber;

  { Does the user have a conflicting name?  If so then add a numeric
    suffix. }
  aName := UserID;
  Index := 0;
  while NameUsed(aName) do begin
    inc(Index);
    aName := Copy(UserID, 1, 26) + '(' + IntToStr(Index) + ')';
  end;

  { Generate a clientID and add a new user info record. }
  aClientID := GetTickCount;
  New(PUserRec);
  PUserRec^.UserName := aName;
  PUserRec^.ClientID := aClientID;
  FUsers.Append(PUserRec);

  { Note that we tell everyone about the new user after we receive the
    nmCheckSecureComms message.  It is only at that point that we really
    know the new chat user has completed all steps to establish the
    connection. }

end;
{--------}
procedure TffChatSrvHandler.OnRemoveClient
                                    (Sender : TffBaseTransport;
                               const aClientID : TffClientID;
                                 var errorCode : TffResult);
var
  aTransport : TffBaseTransport;
  Index : longInt;
  PUserRec : PffChatUser;
  TextRequest : TffnmChatText;
begin
  errorCode := 0;

  { Find the user's info record. }
  PUserRec := nil;
  for Index := 0 to pred(FUsers.Count) do
    if PffChatUser(FUsers.Pointers[Index])^.ClientID = aClientID then begin
      PUserRec := PffChatUser(FUsers.Pointers[Index]);
      break;
    end;

  { Did we find a user record? }
  if assigned(PUserRec) then begin
    { Yes.  Free the user information. }
    with TextRequest do begin
      IsPrivate := False;
      UserName := '';
      Text := PUserRec^.UserName + ' is no more.';
    end;
    FUsers.RemoveAt(Index);
    Dispose(PUserRec);

    { Let everyone know about the disconnect. }
    aTransport := TffBaseTransport.CurrentTransport;
    for Index := 0 to pred(aTransport.ConnectionCount) do
      aTransport.Post(0, aTransport.ConnectionIDs[Index], ffnmChatText,
                      @TextRequest, sizeOf(TextRequest), 1000,
                      ffrmNoReplyExpected);

    { Log the disconnection. }
    FMemo.Lines.Add(TextRequest.Text);

    { Send the updated list of users to everyone. }
    SendUserList;
  end;

end;
{--------}
procedure TffChatSrvHandler.Process(Msg : PffDataMessage);
begin
  Dispatch(Msg^);
  bchFreeMsg(Msg);
end;
{--------}
procedure TffChatSrvHandler.nmCheckSecureComms(var Msg : TffDataMessage);
var
  aName : TffName;
  aTransport : TffBaseTransport;
  Error : TffResult;
  Index : longInt;
  PUserRec : PffChatUser;
  TextRequest : TffnmChatText;
begin
  with Msg do begin
    {Note: If we get this message the client's password must have been
           OK; the transport will hangup if the clientID is unknown.}
    Error := 0;
    TffBaseTransport.Reply(ffnmCheckSecureComms, Error, nil, 0);
  end;

  { Find the user's info record. }
  PUserRec := nil;
  for Index := 0 to pred(FUsers.Count) do
    if PffChatUser(FUsers.Pointers[Index])^.ClientID = Msg.dmClientID then begin
      PUserRec := PffChatUser(FUsers.Pointers[Index]);
      break;
    end;

  if assigned(PUserRec) then
    aName := PUserRec^.UserName
  else
    aName := IntToStr(Msg.dmClientID);

  { Let everybody know about the new user. }
  with TextRequest do begin
    IsPrivate := False;
    UserName := '';
    Text := aName + ' has joined the discussion.';
  end;

  aTransport := TffBaseTransport.CurrentTransport;
  for Index := 0 to pred(aTransport.ConnectionCount) do
    aTransport.Post(0, aTransport.ConnectionIDs[Index], ffnmChatText,
                    @TextRequest, sizeOf(TextRequest), 1000,
                    ffrmNoReplyExpected);

  { Send the updated list of users to everyone. We do this here instead of
    OnAdd client because it is only at this point that we know the client
    was really added. }
  SendUserList;

  { Log the connection. }
  FMemo.Lines.Add(TextRequest.Text);

end;
{--------}
procedure TffChatSrvHandler.nmChatText(var Msg : TffDataMessage);
var
  aTransport : TffBaseTransport;
  Index : longInt;
  InRequest : PffnmChatText;
  PRecipient : PffChatUser;
  PUserRec : PffChatUser;
begin

  aTransport := TffBaseTransport.CurrentTransport;

  { Get a handle on the incoming message's content. }
  InRequest := PffnmChatText(Msg.dmData);

  { Find the chat user. }
  PUserRec := nil;
  for Index := 0 to pred(FUsers.Count) do
    if PffChatUser(FUsers.Pointers[Index])^.ClientID = Msg.dmClientID then begin
      PUserRec := PffChatUser(FUsers.Pointers[Index]);
      break;
    end;

  { Did we find the user's record? }
  if assigned(PUserRec) then begin
    { Yes.  Log the message. }
    FMemo.Lines.Add(PUserRec^.UserName + ': ' + InRequest^.Text);

    { Is this a private message? }
    if InRequest^.IsPrivate then begin
      { Find the recipient's user record. }
      PRecipient := nil;
      for Index := 0 to pred(FUsers.Count) do
        if PffChatUser(FUsers.Pointers[Index])^.UserName =
             InRequest.UserName then begin
          PRecipient := PffChatUser(FUsers.Pointers[Index]);
          break;
        end;
      if assigned(PRecipient) then begin
        { Re-use the inbound request for the outbound message. }
        InRequest.UserName := PUserRec^.UserName;
        aTransport.Post(0, PRecipient^.ClientID, ffnmChatText, InRequest,
                        sizeOf(TffnmChatText), 1000, ffrmNoReplyExpected);
      end;
    end
    else begin
      { No.  Route it to everyone. Re-use the inbound request for the
        outbound message. }
      InRequest.UserName := PUserRec^.UserName;
      for Index := 0 to pred(aTransport.ConnectionCount) do
        aTransport.Post(0, aTransport.ConnectionIDs[Index], ffnmChatText,
                        InRequest, sizeOf(TffnmChatText), 1000,
                        ffrmNoReplyExpected);
    end;
  end;
end;
{====================================================================}

{===TffChatClntHandler===============================================}
procedure TffChatClntHandler.scInitialize;
begin
  { Do nothing }
end;
{--------}
procedure TffChatClntHandler.scPrepareForShutdown;
begin
  { Do nothing }
end;
{--------}
procedure TffChatClntHandler.scShutdown;
begin
  { Do nothing }
end;
{--------}
procedure TffChatClntHandler.scStartup;
begin
  { Do nothing }
end;
{--------}
procedure TffChatClntHandler.Process(Msg : PffDataMessage);
begin
  Dispatch(Msg^);
  bchFreeMsg(Msg);
end;
{--------}
procedure TffChatClntHandler.nmChatText(var Msg : TffDataMessage);
var
  aMessage : string;
  PRequest : PffnmChatText;
begin
  { Get a handle on the message content. }
  PRequest := PffnmChatText(Msg.dmData);
  if PRequest^.IsPrivate then
    aMessage := ffc_Private
  else
    aMessage := '';

  aMessage := aMessage + PRequest^.UserName + ' :' + PRequest^.Text;
  FOutput.Items.Add(aMessage);
  FOutput.ItemIndex := pred(FOutput.Items.Count);
  FOutput.ItemIndex := -1;
end;
{--------}
procedure TffChatClntHandler.nmChatUsers(var Msg : TffDataMessage);
var
  aList : TStringList;
  aStream : TMemoryStream;
  Index : longInt;
  PRequest : PffnmChatUsers;
begin
  { Get a handle on the message content. }
  PRequest := PffnmChatUsers(Msg.dmData);

  { Read the content into a stream. }
  aStream := TMemoryStream.Create;
  try
    aStream.Write(PRequest^, Msg.dmDataLen);
    aList := TStringList.Create;
    try
      aStream.Position := 0;
      aList.LoadFromStream(aStream);

      { Populate the listbox. }
      FUserList.Items.Clear;
      for Index := 0 to pred(aList.Count) do
        FUserList.Items.Add(aList.Strings[Index]);
    finally
      aList.Free;
    end;
  finally
    aStream.Free;
  end;
end;
{====================================================================}

end.
