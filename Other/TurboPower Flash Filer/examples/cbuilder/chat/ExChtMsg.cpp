//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "ExChtMsg.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)

__fastcall TffChatSrvHandler::TffChatSrvHandler(TComponent* aOwner) :
  TffBaseCommandHandler(aOwner)
{
  FUsers = new TffPointerList;
}

__fastcall TffChatSrvHandler::~TffChatSrvHandler()
{
  // Free any user records still in our list.
  int i;
  for (i = FUsers->Count - 1;i >= 0; i--) {
   TffChatUser* PUserRec = (TffChatUser*)FUsers->Pointers[i];
   delete PUserRec;
  }
  delete FUsers;
}

void __fastcall TffChatSrvHandler::scInitialize()
{
  // Do nothing
}

void __fastcall TffChatSrvHandler::scPrepareForShutdown()
{
  // Do nothing
}

void __fastcall TffChatSrvHandler::scShutdown()
{
  // Do nothing
}

void __fastcall TffChatSrvHandler::scStartup()
{
  // Do nothing
}

void __fastcall TffChatSrvHandler::SendUserList()
{

  // Get the current transport.
  TffBaseTransport* aTransport =
    TffBaseTransport::CurrentTransport(__classid(TffBaseTransport));

  TStringList* UserList = new TStringList;
  try {
    for (int i = 0;i < FUsers->Count;i++)
      UserList->Add(((TffChatUser*)FUsers->Pointers[i])->UserName);
    UserList->Sort();

    // Create a stream to hold the users.
    TMemoryStream* UserStream = new TMemoryStream;
    try {
      UserList->SaveToStream(UserStream);

      // Send the user list to everyone.
      for (int i = 0; i < aTransport->ConnectionCount();i++)
        aTransport->Post(0, aTransport->ConnectionIDs[i], ffnmChatUsers,
                        UserStream->Memory, UserStream->Size, 1000,
                        ffrmNoReplyExpected);

    }
    __finally {
      delete UserStream;
    }
  }
  __finally {
    delete UserList;
  }

}

  // We use this function to determine if (another chat user has already
  // registered this user's name.
bool TffChatSrvHandler::NameUsed(const TffName newName)
{
  bool Result = False;
  for (int i = 0; i < FUsers->Count; i++) {
    Result = (AnsiCompareText(((TffChatUser*)FUsers->Pointers[i])->UserName,
               newName) == 0);
    if (Result)
      break;
  }
  return Result;
}


void __fastcall TffChatSrvHandler::OnAddClient(TffBaseTransport* Sender,
  const TffName& userID, const int timeout, const int clientVersion,
  TffWord32& passwordHash, TffClientID& aClientID, TffResult& errorCode,
  bool& isSecure, int& aVersion)
{
  passwordHash = 0;
  errorCode = 0;
  isSecure = false;
  aVersion = ffVersionNumber;

  // Does the user have a conflicting name?  if (so then add a numeric
  // suffix.
  String aName = userID;
  int i = 0;
  while (NameUsed(aName)) {
    i++;
    aName = aName.SubString(1, 26) + "(" + String(i) + ")";
  }

  // Generate a clientID and add a new user info record.
  aClientID = GetTickCount();
  TffChatUser* PUserRec = new TffChatUser;
  PUserRec->UserName = aName;
  PUserRec->ClientID = aClientID;
  FUsers->Append(PUserRec);

  // Note that we tell everyone about the new user after we receive the
  // nmCheckSecureComms message.  It is only at that point that we really
  // know the new chat user has completed all steps to establish the
  // connection.
}

void __fastcall TffChatSrvHandler::OnRemoveClient(
  TffBaseTransport* Sender, const TffClientID aClientID, TffResult& errorCode)
{
  errorCode = 0;

  // Find the user's info record.
  TffChatUser* PUserRec;
  PUserRec = 0;
  int i;
  for (i = 0; i < FUsers->Count; i++)
    if (((TffChatUser*)FUsers->Pointers[i])->ClientID == aClientID) {
      PUserRec = (TffChatUser*)FUsers->Pointers[i];
      break;
    }

  // Did we find a user record?
  TffnmChatText TextRequest;
  if (PUserRec) {
    // Yes.  Free the user information.
    TextRequest.IsPrivate = false;
    TextRequest.UserName = "";
    String S = String(PUserRec->UserName) + " is no more.";
    strcpy(TextRequest.Text, S.c_str());
    FUsers->RemoveAt(i);
    delete PUserRec;

    // Let everyone know about the disconnect.
    TffBaseTransport* aTransport =
      TffBaseTransport::CurrentTransport(__classid(TffBaseTransport));
    for (int i = 0; i < aTransport->ConnectionCount(); i++)
      aTransport->Post(0, aTransport->ConnectionIDs[i], ffnmChatText,
                      &TextRequest, sizeof(TextRequest), 1000,
                      ffrmNoReplyExpected);

    // Log the disconnection.
    FMemo->Lines->Add(TextRequest.Text);

    // Send the updated list of users to everyone.
    SendUserList();
  }

}

void __fastcall TffChatSrvHandler::Process(TffDataMessage* Msg)
{
  Dispatch(Msg);
  bchFreeMsg(Msg);
}

void __fastcall TffChatSrvHandler::nmCheckSecureComms(TffDataMessage& Msg)
{
  // Note: if (we get this message the client's password must have been
  // OK; the transport will hangup if (the clientID is unknown.}
  TffResult Error = 0;
  TffBaseTransport* aTransport =
    TffBaseTransport::CurrentTransport(__classid(TffBaseTransport));
  aTransport->Reply(ffnmCheckSecureComms, Error, 0, 0);

  // Find the user's info record.
  TffChatUser* PUserRec = 0;
  for (int i=0;i<FUsers->Count;i++)
    if (((TffChatUser*)FUsers->Pointers[i])->ClientID == Msg.dmClientID) {
      PUserRec = (TffChatUser*)FUsers->Pointers[i];
      break;
    }

  String aName;
  if (PUserRec)
    aName = PUserRec->UserName;
  else
    aName = IntToStr(Msg.dmClientID);

  // Let everybody know about the new user.

  TffnmChatText TextRequest;
  TextRequest.IsPrivate = false;
  TextRequest.UserName = "";
  strcpy(TextRequest.Text, String(aName + " has joined the discussion.").c_str());

  aTransport =
    TffBaseTransport::CurrentTransport(__classid(TffBaseTransport));
  for (int i = 0; i < aTransport->ConnectionCount(); i++)
    aTransport->Post(0, aTransport->ConnectionIDs[i], ffnmChatText,
                    &TextRequest, sizeof(TextRequest), 1000,
                    ffrmNoReplyExpected);

  // Send the updated list of users to everyone.
  SendUserList();

  // Log the connection.
  FMemo->Lines->Add(TextRequest.Text);

}

void __fastcall TffChatSrvHandler::nmChatText(TffDataMessage& Msg)
{

  TffBaseTransport* aTransport =
    TffBaseTransport::CurrentTransport(__classid(TffBaseTransport));

  // Get a handle on the incoming message's content.
  TffnmChatText* InRequest = (TffnmChatText*)Msg.dmData;

  // Find the chat user.
  TffChatUser* PUserRec = 0;
  for (int i = 0; i < FUsers->Count; i++)
    if (((TffChatUser*)FUsers->Pointers[i])->ClientID == Msg.dmClientID) {
      PUserRec = (TffChatUser*)FUsers->Pointers[i];
      break;
    }

  // Did we find the user's record?
  if (PUserRec) {
    // Yes.  Log the message.
    FMemo->Lines->Add(String(PUserRec->UserName) + ": " + InRequest->Text);

    // Is this a private message?
    if (InRequest->IsPrivate) {
      // Find the recipient's user record.
      TffChatUser* PRecipient = 0;
      for (int i = 0; i < FUsers->Count; i++)
        if (String(((TffChatUser*)FUsers->Pointers[i])->UserName) ==
             InRequest->UserName) {
          PRecipient = (TffChatUser*)FUsers->Pointers[i];
          break;
        }
      if (PRecipient) {
        // Re-use the inbound request for the outbound message.
        InRequest->UserName = PUserRec->UserName;
        aTransport->Post(0, PRecipient->ClientID, ffnmChatText, InRequest,
                        sizeof(TffnmChatText), 1000, ffrmNoReplyExpected);
      }
    }
    else {
      // No.  Route it to everyone. Re-use the inbound request for the
      // outbound message.
      InRequest->UserName = PUserRec->UserName;
      for (int i = 0; i < aTransport->ConnectionCount(); i++)
        aTransport->Post(0, aTransport->ConnectionIDs[i], ffnmChatText,
                        InRequest, sizeof(TffnmChatText), 1000,
                        ffrmNoReplyExpected);
    }
  }
}

//===TffChatClntHandler===============================================
void __fastcall TffChatClntHandler::scInitialize()
{
  // Do nothing
}

void __fastcall TffChatClntHandler::scPrepareForShutdown()
{
  // Do nothing
}

void __fastcall TffChatClntHandler::scShutdown()
{
  // Do nothing
}

void __fastcall TffChatClntHandler::scStartup()
{
  // Do nothing
}

void __fastcall TffChatClntHandler::Process(TffDataMessage* Msg)
{
  Dispatch(Msg);
  bchFreeMsg(Msg);
}

void __fastcall TffChatClntHandler::nmChatText(TffDataMessage& Msg)
{
  // Get a handle on the message content.
  TffnmChatText* PRequest = (TffnmChatText*)Msg.dmData;
  String aMessage;
  if (PRequest->IsPrivate)
    aMessage = ffc_Private;
  else
    aMessage = "";

  aMessage = aMessage + PRequest->UserName + " :" + PRequest->Text;
  FOutput->Items->Add(aMessage);
  FOutput->ItemIndex = FOutput->Items->Count - 1;
  FOutput->ItemIndex = -1;
}

void __fastcall TffChatClntHandler::nmChatUsers(TffDataMessage& Msg)
{
  // Get a handle on the message content.
  TffnmChatUsers* PRequest = (TffnmChatUsers*)Msg.dmData;

  // Read the content into a stream.
  TMemoryStream* aStream = new TMemoryStream;
  try {
    aStream->Write(PRequest, Msg.dmDataLen);
    TStringList* aList = new TStringList;
    try {
      aStream->Position = 0;
      aList->LoadFromStream(aStream);

      // Populate the listbox.
      FUserList->Items->Clear();
      for (int i = 0; i < aList->Count; i++)
        FUserList->Items->Add(aList->Strings[i]);
    }
    __finally {
      delete aList;
    }
  }
  __finally {
    delete aStream;
  }
}
