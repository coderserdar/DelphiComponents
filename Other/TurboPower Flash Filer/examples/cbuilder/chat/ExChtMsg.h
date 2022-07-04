//---------------------------------------------------------------------------
#ifndef ExChtMsgH

#include "FFLLBase.hpp"
#include "FFLLComm.hpp"
#include "FFLLReq.hpp"
#include "FFNetMsg.hpp"

const int ffnmChatText = ffnmUser + 100;
const int ffnmChatUsers = ffnmUser + 101;
const String ffc_Private = "Private: ";


  // We create one instance of this record per chat user.
  #pragma pack(push, 1)
  struct TffChatUser {
    TffClientID ClientID;
    TffName UserName;
  };

  // Message text sent from client to server or vice versa.
  struct TffnmChatText {
    bool IsPrivate;
    TffName UserName;
      // If private and going to server then this is the name of the
      //  intended recipient.  If private and coming from server then this
      //  is the name of the sender.  If public and going to server then this
      //  is blank.  If public and coming form server then this is the name of
      //  the sender.
    char Text[255];
  };

  struct TffnmChatUsers {
    TffVarMsgField UserList;
  };
  #pragma pack(pop)
  // The following class handles commands on the chat server.
  //  Text messages are routed to all chat users or to a specific chat user
  //    if the message is marked private.
  //  When a new client connects, all clients are notified and an updated user
  //    list is distributed to each client.
  //  When a client disconnects, all clients are notified and an updated user
  //    list is distributed to each client.
  class TffChatSrvHandler : public TffBaseCommandHandler {
  protected :
    TffPointerList* FUsers;
    TMemo* FMemo;
    void __fastcall scInitialize();
    void __fastcall scPrepareForShutdown();
    void __fastcall scShutdown();
    void __fastcall scStartup();
    void __fastcall SendUserList();
    void __fastcall nmCheckSecureComms(TffDataMessage& Msg);
    void __fastcall nmChatText(TffDataMessage& Msg);
  public :
    __fastcall TffChatSrvHandler(TComponent* aOwner);
    __fastcall ~TffChatSrvHandler();
    bool NameUsed(const TffName newName);
    void __fastcall OnAddClient(TffBaseTransport* Sender, const TffName& userID,
      const int timeout, const int clientVersion, TffWord32& passwordHash,
      TffClientID& aClientID, TffResult& errorCode, bool& isSecure,
      int& aVersion);
    void __fastcall OnRemoveClient(TffBaseTransport* Sender,
      const TffClientID aClientID, TffResult& errorCode);
    void __fastcall Process(TffDataMessage* Msg);
    __property TMemo* Memo = {read = FMemo, write = FMemo};
    BEGIN_MESSAGE_MAP
      MESSAGE_HANDLER(ffnmCheckSecureComms, TffDataMessage, nmCheckSecureComms)
      MESSAGE_HANDLER(ffnmChatText, TffDataMessage, nmChatText)
    END_MESSAGE_MAP(TffBaseCommandHandler)
  };

  // The following class handles commands on the chat client.
  //  Received text messages are displayed in the memo.
  //  Received user lists are used to populate a listbox.
  class TffChatClntHandler : public TffBaseCommandHandler {
  protected :
    TListBox* FOutput;
    TListBox* FUserList;
    void __fastcall scInitialize();
    void __fastcall scPrepareForShutdown();
    void __fastcall scShutdown();
    void __fastcall scStartup();
    void __fastcall nmChatText(TffDataMessage& Msg);
    void __fastcall nmChatUsers(TffDataMessage& Msg);
  public :
    __fastcall TffChatClntHandler(TComponent* AOwner) :
      TffBaseCommandHandler(AOwner) {};
    void __fastcall Process(TffDataMessage* Msg);
    __property TListBox* UserList = {read = FUserList, write = FUserList};
    __property TListBox* Output = {read = FOutput, write = FOutput};
    BEGIN_MESSAGE_MAP
      MESSAGE_HANDLER(ffnmChatText, TffDataMessage, nmChatText)
      MESSAGE_HANDLER(ffnmChatUsers, TffDataMessage, nmChatUsers)
    END_MESSAGE_MAP(TffBaseCommandHandler)
  };


#define ExChtMsgH
//---------------------------------------------------------------------------
#endif
