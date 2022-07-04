unit ICQClient {v. 1.21};
{************************************************
    For updates checkout: http://www.cobans.net
      (c) Alex Demchenko(alex@ritlabs.com)
          Gene Reeves(notgiven2k@lycos.com)

    TICQClient v1.20/v1.20f/v1.21 by
      Dalibor Drzik (eraser@senior.cz)
      Yegor Derevenet (yegor@box.vsi.ru)
*************************************************}
{Includes patches from Saif.N for Support Web Pager }

{$R-}                   //Remove range checking
{$DEFINE USE_FORMS}     //If you don't use forms unit remove this line

//Some needed defines, do not remove them!
{$IFDEF VER90}
  {$DEFINE OLD_DELPHI}
{$ENDIF}
{$IFDEF VER100}                                
  {$DEFINE OLD_DELPHI}
{$ENDIF}
{$IFDEF VER120}
  {$DEFINE OLD_DELPHI}
{$ENDIF}
{$IFDEF VER130}
  {$DEFINE OLD_DELPHI}
{$ENDIF}

//{$DEFINE HTTP_DEBUG}

interface
uses
  Windows, Messages, Classes, WinSock, ICQWorks, ICQSock, ICQDirect2, ICQLang,
  SysUtils;

{$IFNDEF USE_FORMS}
  {$DEFINE OLD_DELPHI}
{$ENDIF}

type
  //UIN Entry used in direct connections
  PUINEntry = ^TUINEntry;
  TUINEntry = record
    UIN: LongWord;
    Nick: ShortString;
    CType: Word;
    CTag: Word;
    CGroupID: Word;
    Cellular: ShortString;
    Authorized: Boolean; // *yegor
  end;

  TMyTimer = class;

  //Callback function types
  TOnPktParseAdv = procedure(Sender: TObject; Buffer: Pointer; BufLen: LongWord; Incoming: Boolean) of object;
  TOnHandlePkt = procedure(Flap: TFlapHdr; Buffer: Pointer) of object;
  TOnMsgProc = procedure(Sender: TObject; Msg, UIN: String) of object;
  TOnWPagerProc = procedure(Sender: TObject; PSender,PEmail,PSenderIP, Msg:String) of object;     // Added by Saif.N
  TOnOffMsgProc = procedure(Sender: TObject; DateTime: TDateTime; Msg, UIN: String) of object;
  TOnURLProc = procedure(Sender: TObject; Description, URL, UIN: String) of object;
  TOnOffURLProc = procedure(Sender: TObject; DateTime: TDateTime; Description, URL, UIN: String) of object;
  TOnStatusChange = procedure(Sender: TObject; UIN: String; Status: LongWord) of object;
  TOnOnlineInfo = procedure(Sender: TObject; UIN: String; Port: Word; InternalIP, ExternalIP: String; OnlineTime: TDateTime; Idle: Word; ICQVersion, MirandaVersion: LongWord; ProtoVer, UserCaps: Byte) of object;
  TOnUserEvent = procedure(Sender: TObject; UIN: String) of object;
  TOnUserGeneralInfo = procedure(Sender: TObject; UIN, NickName, FirstName, LastName, Email, City, State, Phone, Fax, Street, Cellular, Zip, Country: String; TimeZone: Byte; PublishEmail: Boolean) of object;
  TOnUserWorkInfo = procedure (Sender: TObject; UIN, WCity, WState, WPhone, WFax, WAddress, WZip, WCountry, WCompany, WDepartment, WPosition, WOccupation, WHomePage: String) of object;
  TOnUserInfoMore = procedure (Sender: TObject; UIN: String; Age: Word; Gender: Byte; HomePage: String; BirthYear: Word; BirthMonth: Word; BirthDay: Word; Lang1, Lang2, Lang3: String) of object;
  TOnUserInfoAbout = procedure(Sender: TObject; UIN, About: String) of object;
  TOnUserInfoInterests = procedure(Sender: TObject; UIN: String; Interests: TStringList) of object;
  TOnUserInfoMoreEmails = procedure(Sender: TObject; UIN: String; Emails: TStringList) of object;
  TOnUserInfoBackground = procedure(Sender: TObject; UIN: String; Pasts, Affiliations: TStringList) of object;
  TOnUserFound = procedure(Sender: TObject; UIN, Nick, FirstName, LastName, Email: String; Status: Word; Gender, Age: Byte; SearchComplete: Boolean; Authorize: Boolean) of object;
  TOnServerListRecv = procedure(Sender: TObject; SrvContactList: TList) of object;
  TOnAdvMsgAck = procedure(Sender: TObject; UIN: String; ID: Word; AcceptType: Byte; AcceptMsg: String) of object;
  TOnMsgAck = procedure(Sender: TObject; UIN: String; ID: Word) of object;
  TOnAutoMsgResponse = procedure(Sender: TObject; UIN: String; ID: Word; RespStatus: Byte; Msg: String) of object;
  TOnContactListRecv = procedure(Sender: TObject; UIN: String; ContactList: TStringList) of object;
  TOnContactListReq = procedure(Sender: TObject; UIN, Reason: String) of object;
  TOnDirectPktAck = procedure(Sender: TObject; ID: Word) of object;
  TOnSMSAck = procedure(Sender: TObject; Source, Network, MsgId: String; Deliverable: Boolean) of object;
  TOnSMSReply = procedure(Sender: TObject; Source, SmsSender, Time, Text: String) of object;
  TOnInfoChanged = procedure(Sender: TObject; InfoType: TInfoType; ChangedOk: Boolean) of object;
  TOnAuthResponse = procedure(Sender: TObject; UIN: String; Granted: Boolean; Reason: String) of object;
  TOnChangeResponse = procedure(Sender: TObject; ErrorCode: Word) of object;
  TOnFTRequest = procedure(Sender: TObject; RequestRec: TFTRequestRec) of object;
  TOnUserInfoShort = procedure(Sender: TObject; UIN, NickName, FirstName, LastName, Email: String; UserFound, AuthRequired: Boolean) of object;
  TOnAuthRequest = Procedure(Sender:TObject; UIN:String; Reason:String) of Object;

  {TICQNet -- Object implementing sending/receiving packets between Client and ICQ Server.}
  TICQNet = class(TObject)
  private
    FSrcBuf: array[0..MAX_DATA_LEN - 1] of Byte;        //.              .
    FSrcLen: Word;                                      //.PACKET READING.
    FNewFlap: TFlapHdr;                                 //.     DATA     .
    FFlapSet: Boolean;                                  //.              .

    FSocket: TEventSocket;
    FSendSocket: TEventSocket;
    FCanSend: Boolean;                                  //Can send data through HTTP proxy

    FHost: String;
    FPort: Word;

    FProxyType: TProxyType;
    FProxyHost: String;
    FProxyPort: Word;
    FProxyUser: String;
    FProxyPass: String;
    FProxyAuth: Boolean;
    FProxyResolve: Boolean;

    FOnError: TOnError;
    FOnHandlePkt: TOnHandlePkt;
    FOnDisconnect: TNotifyEvent;
    FOnConnectError: TNotifyEvent;
    FOnPktParse: TOnPktParseAdv;

    FHTTPBuffer: TNetBuffer;
    FHTTPConnected: Boolean;
    FHTTPSid: String;
    FHTTPIP: String;
    FHTTPPort: Word;
    FHTTPSeq: Word;
    FHTTPFirstResend: Boolean;

    FErrLang: TICQLangType;


    procedure _OnConnect(Sender: TObject);
    procedure _OnError(Sender: TObject; ErrorType: TErrorType; ErrorMsg: String);
    procedure _OnConnectError(Sender: TObject);
    procedure _OnDisconnect(Sender: TObject);
    procedure _OnReceive(Sender: TObject; Buffer: Pointer; BufLen: LongWord);
    procedure _OnDataSent(Sender: TObject);
    procedure SendHTTPData;
    procedure HTTPReconnect;
    procedure HTTPResend;
    procedure HandleHTTPDataPak(Buffer: Pointer; BufLen: LongWord);
    procedure HandleHTTPData(Buffer: Pointer; BufLen: LongWord);
    procedure HandleFlapData(Buffer: Pointer; BufLen: LongWord);
    procedure FreeSocket(var Socket: TEventSocket);
  public
    constructor Create;
    destructor Destroy; override;

    property Host: String read FHost write FHost;
    property Port: Word read FPort write FPort;

    property ProxyType: TProxyType read FProxyType write FProxyType;
    property ProxyHost: String read FProxyHost write FProxyHost;
    property ProxyPort: Word read FProxyPort write FProxyPort;
    property ProxyUser: String read FProxyUser write FProxyUser;
    property ProxyPass: String read FProxyPass write FProxyPass;
    property ProxyAuth: Boolean read FProxyAuth write FProxyAuth default False;
    property ProxyResolve: Boolean read FProxyResolve write FProxyResolve default False;

    procedure Connect;
    procedure SendData(var Data; DataLen: LongWord);
    procedure FreeSockets;
    procedure Disconnect;

    property ErrorLanguage: TICQLangType write FErrLang default LANG_EN;
  published
    property OnError: TOnError read FOnError write FOnError;
    property OnHandlePkt: TOnHandlePkt read FOnHandlePkt write FOnHandlePkt;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnConnectError: TNotifyEvent read FOnConnectError write FOnConnectError;
    property OnPktParse: TOnPktParseAdv read FOnPktParse write FOnPktParse;
  end;

  {TICQClient -- ICQ Component}
  TICQClient = class(TComponent)
  private
    FSock: TICQNet;                                     //Client's socket
    FLUIN: LongWord;                                    //Client's UIN
    FLPass: String;                                     //Client's password
    FFirstConnect: Boolean;                             //Flag, used in login sequence
    FSeq: Word;                                         //Main Flap Seq
    FSeq2: Word;                                        //TO_ICQSRV Seq
    FDSeq: Word;                                        //Direct connection Seq
    FCookie: String;                                    //Temporary cookie, used in login sequence, we can use String type, becouse ICQ server doesn't send 0x00 chars in it's Cookie part
    FIp: String;                                        //Ip to connect to
    FPort: Word;                                        //Port to connect to
    FDConnCookie: LongWord;                             //Direct connection cookie
    FDirect: TDirectControl;                            //Direct control
    //-- Proxy settings
    FProxyType: TProxyType;                             //.
    FProxyHost: String;                                 //.
    FProxyPort: Word;                                   //. Proxy Configaration
    FProxyAuth: Boolean;                                //.        Data
    FProxyPass: String;                                 //.
    FProxyUser: String;                                 //.
    FProxyResolve: Boolean;
    //-- Events & other stuff --
    FContactLst: TStrings;
    FVisibleLst: TStrings;
    FInvisibleLst: TStrings;
    FOnMsg: TOnMsgProc;
    FonWPager: TOnWPagerProc;
    FOnURL: TOnURLProc;
    FOnOffMsg: TOnOffMsgProc;
    FOnOffURL: TOnOffURLProc;
    FOnLogin: TNotifyEvent;
    FOnLogOff: TNotifyEvent;
    FOnPktParse: TOnAdvPktParse;
    FOnDPktParse: TOnParseDirectPkt;
    FOnConnectionFailed: TNotifyEvent;
    FOnStatusChange: TOnStatusChange;
    FOnUserOffline: TOnUserEvent;
    FOnAddedYou: TOnUserEvent;
    FOnUserGeneralInfo: TOnUserGeneralInfo;
    FOnUserWorkInfo: TOnUserWorkInfo;
    FOnUserInfoMore: TOnUserInfoMore;
    FOnUserInfoAbout: TOnUserInfoAbout;
    FOnUserInfoInterests: TOnUserInfoInterests;
    FOnUserInfoMoreEmails: TOnUserInfoMoreEmails;
    FOnUserInfoBackground: TOnUserInfoBackground;
    FStatus: LongWord;
    FDoPlain: Boolean;
    FInfoChain: TStringList;
    FSInfoChain: TStringList;
    FLastInfoUin: String;
    FLastSInfoUin: String;
    FLoggedIn: Boolean;
    FRegisteringUIN: Boolean;
    FRegPassword: String;
    FOnUserFound: TOnUserFound;
    FOnUserNotFound: TNotifyEvent;
    FOnServerListRecv: TOnServerListRecv;
    FOnAdvMsgAck: TOnAdvMsgAck;
    FOnMsgAck: TOnMsgAck;
    FOnNewUINRegistered: TOnUserEvent;
    FOnNewUINRefused: TNotifyEvent;
    FOnAutoMsgResponse: TOnAutoMsgResponse;
    FAutoAwayMsg: String;
    FOnUnregisterOk: TNotifyEvent;
    FOnUnregBadPass: TNotifyEvent;
    FOnContactListRecv: TOnContactListRecv;
    FOnContactListReq: TOnContactListReq;
    FOnDirectPktAck: TOnDirectPktAck;
    FOnSmsRefused: TNotifyEvent;
    FOnSMSAck: TOnSMSAck;
    FOnOnlineInfo: TOnOnlineInfo;
    FUseDirect: Boolean;
    FOnError: TOnError;
    FTimer: TMyTimer;
    FTimeout: Byte;
    FOnSMSReply: TOnSMSReply;
    FOnInfoChanged: TOnInfoChanged;
    FOnAuthSet: TNotifyEvent;
    FOnAuthResponse: TOnAuthResponse;
    FOnChangeResponse: TOnChangeResponse;
    FOnFTRequest: TOnFTRequest;
    FOnFTInit: TOnFTInit;
    FOnFTStart: TOnFTStart;
    FOnFTFileData: TOnFTFileData;
    FOnSendFileStart: TOnSendFileStart;
    FOnSendFileData : TOnSendFileData;
    FOnSendFileFinish: TOnSendFileFinish;
    FLastError: String;
    FOnUserInfoShort: TOnUserInfoShort;
    FOnAuthReq:TOnAuthRequest;
    FErrLang: TICQLangType;

    procedure InitNetICQ;
    procedure OnIntError(Sender: TObject; ErrorType: TErrorType; ErrorMsg: String);
    procedure HandlePacket(Flap: TFlapHdr; Data: Pointer);
    procedure SetStatus(NewStatus: LongWord);
    //-- Handling Snac packet procedures
    procedure HSnac0407(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
    procedure HSnac1503(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
    procedure HSnac030B(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
    procedure HSnac131C(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
    procedure HSnac1319(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
    procedure HSnac1306(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
    procedure HSnac040b(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
    procedure HSnac040C(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
    procedure HSnac1705(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
    procedure HSnac131b(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
    procedure HSnac130e(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
    procedure HDirectMsg(Sender: TObject; UIN: LongWord; Pak: PRawPkt; Len: LongWord);
    procedure FTOnConnectError(Sender: TObject);
    procedure FTOnDisconnect(Sender: TObject);
    procedure FTOnDirectParse(Sender: TObject; Buffer: Pointer; BufLen: LongWord; Incoming: Boolean; UIN:Cardinal);
    procedure FTOnPktParse(Sender: TObject; Buffer: Pointer; BufLen: LongWord; Incoming: Boolean);
    procedure OnFTInitProc(Sender: TObject; UIN: LongWord; FileCount, TotalBytes, Speed: LongWord; NickName: String);
    procedure OnFTStartProc(Sender: TObject; StartRec: TFTStartRec; FileName: String; FileSize, Speed: LongWord);
    procedure OnFTFileDataProc(Sender: TObject; UIN: LongWord; Data: Pointer; DataLen: LongWord; LastPacket: Boolean);
    procedure OnSendFileStartProc(Sender:TObject; UIN: LongWord; SendFileRec:TSendFileRec);
    Procedure OnSendFileDataProc(Sender:TObject; UIN:LongWord; Data:Pointer;Var DataLen:LongWord; Var IsLastPacket:Boolean);
    {$HINTS OFF}
    Procedure OnSendFileFinishProc(Sender:TObject; UIN:LongWord; SendFileRec:TSendFileRec; Aborted:Boolean);
    {$HINTS ON}
    procedure SetContactList(Value: TStrings);
    procedure SetVisibleList(Value: TStrings);
    procedure SetInvisibleList(Value: TStrings);
    procedure OnTimeout(Sender: TObject);
    procedure SetErrorLang(NewLang: TICQLangType);
    procedure SetPortLast(aPort:Word);
    procedure SetPortFirst(aPort:word);
    function GetPortLast:Word;
    function GetPortFirst:Word;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Login(Status: LongWord = S_ONLINE; BirthDay: Boolean = False);
    procedure RequestOfflineMessages; // *yegor
    procedure LogOff;
    procedure RegisterNewUIN(const Password: String);
    procedure Disconnect;
    procedure SendMessage(UIN: LongWord; const Msg: String);
    procedure SendURL(UIN: LongWord; const URL, Description: String);
    function AddContact(UIN: LongWord): Boolean;
    function AddContactMulti(UINList: Array of LongWord):Boolean;
    Function AddContactVisible(UIN: LongWord):Boolean;
    Function AddContactInvisible(UIN: LongWord):Boolean;
    procedure RemoveContact(UIN: LongWord);
    procedure RemoveContactVisible(UIN: LongWord);
    procedure RemoveContactInvisible(UIN: LongWord);
    procedure RequestInfo(UIN: LongWord);
    procedure RequestInfoShort(UIN: LongWord);
    procedure SearchByMail(const Email: String);
    procedure SearchByUIN(UIN: LongWord);
    procedure SearchByName(const FirstName, LastName, NickName, Email: String);
    procedure SearchRandom(Group: Word);
    procedure SearchWhitePages(const FirstName, LastName, NickName, Email: String; MinAge, MaxAge: Word; Gender: Byte; const Language, City, Country, Company, Department, Position, Occupation, Organization, OrganKeyWords, PastAffiliation, AffiKeyWords, KeyWord: String; Online: Boolean);
    procedure SetSelfInfoGeneral(NickName, FirstName, LastName, Email, City, State, Phone, Fax, Street, Cellular, Zip, Country: String; TimeZone: Byte; PublishEmail: Boolean);
    procedure SetSelfInfoMore(Age: Word; Gender: Byte; const HomePage: String; BirthYear: Word; BirthMonth, BirthDay: Byte; Language1, Language2, Language3: String);
    procedure SetSelfInfoAbout(const About: String);
    procedure RequestContactList;
    procedure DestroyUINList(var List: TList);
    procedure SendSMS(const Destination, Text: String);
    procedure SendMessageAdvanced(UIN: LongWord; const Msg: String; ID: Word; RTFFormat: Boolean);
    procedure SendYouWereAdded(UIN: LongWord);
    function SendMessageDirect(UIN: LongWord; const Msg: String; RTFFormat: Boolean): Word;
    Function SendFileDC(UIN:LongWord; FileName, Description:String): Word;
    Procedure SendFileCancel(UIN: LongWord);
    procedure RequestAwayMsg(UIN: LongWord; ID: Word; ReqStatus: Byte);
    procedure UnregisterUIN(const Password: String);
    procedure ChangePassword(const NewPassword: String);
    procedure ChangePasswordPtr(Buffer: Pointer; BufLen: Word);
    function DirectConnectionEstabilished(UIN: LongWord): Boolean;
    function SendContacts(UIN: LongWord; Contacts: TStringList; ID: Word): Boolean;
    function RequestContacts(UIN: LongWord; const Reason: String; ID: Word): Boolean;
    function SendContactsDC(UIN: LongWord; Contacts: TStringList): Word;
    function RequestContactsDC(UIN: LongWord; const Reason: String): Word;
    procedure SendKeepAlive;
    procedure SetAuthorization(AuthorizationRequired, WebAware: Boolean);
    procedure SendAuthRequest(UIN: LongWord; Msg: String);
    procedure SendAuthResponse (UIN: LongWord; Authorize: Boolean; Reason: string);
    procedure SSLChangeStart(FirstUpload: Boolean);
    procedure SSLChangeEnd;
    procedure SSLAddGroup(GroupName: String; GroupID: Word);
    procedure SSLAddUser(GroupID, UserID: Word; UIN, Name, SMSNumber: String; Authorize, UpdateUser: Boolean);
    procedure SSLDelUser(GroupID, UserID: Word; UIN, Name, SMSNumber: String; Authorize: Boolean);
    procedure SSLDelGroup(GroupName: String; GroupID: Word);
    procedure SSLUpdateGroup(GroupName: String; GroupID: Word; UserIDs: TStringList);
    procedure SSLAddUserIntoList(UserID: Word; UIN: String; BuddyType: Word);
    procedure SSLDelUserFromList(UserID: Word; UIN: String; BuddyType: Word);
    function FTResponse(ResponseRec: TFTRequestRec; Accept: Boolean; Reason: String): Boolean;
    procedure FTCancel(UIN: LongWord);
    function FTStartResponse(StartRec: TFTStartRec): Boolean;
    property LastError: String read FLastError;
    property Status: LongWord read FStatus write SetStatus;
    property LoggedIn: Boolean read FLoggedIn;
  published
    property DisableDirectConnections: Boolean read FUseDirect write FUseDirect default False;
    property ProxyType: TProxyType read FProxyType write FProxyType default P_NONE;
    property ProxyHost: String read FProxyHost write FProxyHost;
    property ProxyPort: Word read FProxyPort write FProxyPort;
    property ProxyUserID: String read FProxyUser write FProxyUser;
    property ProxyResolve: Boolean read FProxyResolve write FProxyResolve default False;
    property ProxyAuth: Boolean read FProxyAuth write FProxyAuth default False;
    property ProxyPass: String read FProxyPass write FProxyPass;
    property UIN: LongWord read FLUIN write FLUIN;
    property Password: String read FLPass write FLPass;
    property ErrorLanguage: TICQLangType read FErrLang write SetErrorLang default LANG_EN;
    property ICQServer: String read FIp write FIp;
    property ICQPort: Word read FPort write FPort;
    property PortRangeFirst: Word read GetPortFirst write SetPortFirst;
    property PortRangeLast: Word read GetPortLast write SetPortLast;
    property ConvertToPlaintext: Boolean read FDoPlain write FDoPlain;
    property ContactList: TStrings read FContactLst write SetContactList;
    property VisibleList: TStrings read FVisibleLst write SetVisibleList;
    property InvisibleList: TStrings read FInvisibleLst write SetInvisibleList;
    property AutoAwayMessage: String read FAutoAwayMsg write FAutoAwayMsg;
    property OnLogin: TNotifyEvent read FOnLogin write FOnLogin;
    property OnLogOff: TNotifyEvent read FOnLogOff write FOnLogOff;
    property OnMessageRecv: TOnMsgProc read FOnMsg write FOnMsg;
    property OnWPagerRecv: TOnWPagerProc read FOnWPager write FOnWPager;    // Edited by Saif.N
    property OnURLRecv: TOnURLProc read FOnURL write FOnURL;
    property OnOfflineMsgRecv: TOnOffMsgProc read FOnOffMsg write FOnOffMsg;
    property OnOfflineURLRecv: TOnOffURLProc read FOnOffURL write FOnOffURL;
    property OnPktParse: TOnAdvPktParse read FOnPktParse write FOnPktParse;
    property OnPktDirectParse: TOnParseDirectPkt read FOnDPktParse write FOnDPktParse;
    property OnConnectionFailed: TNotifyEvent read FOnConnectionFailed write FOnConnectionFailed;
    property OnStatusChange: TOnStatusChange read FOnStatusChange write FOnStatusChange;
    property OnUserOffline: TOnUserEvent read FOnUserOffline write FOnUserOffline;
    property OnAddedYou: TOnUserEvent read FOnAddedYou write FOnAddedYou;
    property OnUserGeneralInfo: TOnUserGeneralInfo read FOnUserGeneralInfo write FOnUserGeneralInfo;
    property OnUserWorkInfo: TOnUserWorkInfo read FOnUserWorkInfo write FOnUserWorkInfo;
    property OnUserInfoMore: TOnUserInfoMore read FOnUserInfoMore write FOnUserInfoMore;
    property OnUserInfoAbout: TOnUserInfoAbout read FOnUserInfoAbout write FOnUserInfoAbout;
    property OnUserInfoInterests: TOnUserInfoInterests read FOnUserInfoInterests write FOnUserInfoInterests;
    property OnUserInfoMoreEmails: TOnUserInfoMoreEmails read FOnUserInfoMoreEmails write FOnUserInfoMoreEmails;
    property OnUserInfoBackground: TOnUserInfoBackground read FOnUserInfoBackground write FOnUserInfoBackground;
    property OnUserFound: TOnUserFound read FOnUserFound write FOnUserFound;
    property OnUserNotFound: TNotifyEvent read FOnUserNotFound write FOnUserNotFound;
    property OnServerListRecv: TOnServerListRecv read FOnServerListRecv write FOnServerListRecv;
    property OnAdvancedMsgAck: TOnAdvMsgAck read FOnAdvMsgAck write FOnAdvMsgAck;
    property OnMsgAck: TOnMsgAck read FOnMsgAck write FOnMsgAck;
    property OnNewUINRegistered: TOnUserEvent read FOnNewUINRegistered write FOnNewUINRegistered;
    property OnNewUINRefused: TNotifyEvent read FOnNewUINRefused write FOnNewUINRefused;
    property OnAutoMsgResponse: TOnAutoMsgResponse read FOnAutoMsgResponse write FOnAutoMsgResponse;
    property OnUnregisterOk: TNotifyEvent read FOnUnregisterOk write FOnUnregisterOk;
    property OnUnregisterBadPassword: TNotifyEvent read FOnUnregBadPass write FOnUnregBadPass;
    property OnContactListRecv: TOnContactListRecv read FOnContactListRecv write FOnContactListRecv;
    property OnContactListRequest: TOnContactListReq read FOnContactListReq write FOnContactListReq;
    property OnDirectPacketAck: TOnDirectPktAck read FOnDirectPktAck write FOnDirectPktAck;
    property OnSMSRefused: TNotifyEvent read FOnSmsRefused write FOnSmsRefused;
    property OnSMSAck: TOnSMSAck read FOnSMSAck write FOnSMSAck;
    property OnOnlineInfo: TOnOnlineInfo read FOnOnlineInfo write FOnOnlineInfo;
    property OnError: TOnError read FOnError write FOnError;
    property ConnectionTimeout: Byte read FTimeout write FTimeout;
    property OnSMSReply: TOnSMSReply read FOnSMSReply write FOnSMSReply;
    property OnInfoChanged: TOnInfoChanged read FOnInfoChanged write FOnInfoChanged;
    property OnAuthorizationChangedOk: TNotifyEvent read FOnAuthSet write FOnAuthSet;
    property OnAuthResponse: TOnAuthResponse read FOnAuthResponse write FOnAuthResponse;
    property OnAuthRequest: TOnAuthRequest read FOnAuthReq write FOnAuthReq;
    property OnSSLChangeResponse: TOnChangeResponse read FOnChangeResponse write FOnChangeResponse;
    property OnFTRequest: TOnFTRequest read FOnFTRequest write FOnFTRequest;
    property OnFTInit: TOnFTInit read FOnFTInit write FOnFTInit;
    property OnFTStart: TOnFTStart read FOnFTStart write FOnFTStart;
    property OnFTFileData: TOnFTFileData read FOnFTFileData write FOnFTFileData;
    property OnSendFileStart: TOnSendFileStart read FOnSendFileStart write FOnSendFileStart;
    property OnSendFileData:  TOnSendFileData  read FOnSendFileData  write FOnSendFileData;
    property OnSendFileFinish: TOnSendFileFinish read FOnSendFileFinish write FOnSendFileFinish;
    property OnUserInfoShort: TOnUserInfoShort read FOnUserInfoShort write FOnUserInfoShort;
  end;

  TMyTimer = class(TObject)
  private
    FInterval: LongWord;
    FWindowHandle: THandle;
    FOnTimer: TNotifyEvent;
    FEnabled: Boolean;
    FTag: Integer;
    procedure UpdateTimer;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: LongWord);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure WndProc(var Msg: TMessage);
  protected
    procedure Timer; dynamic;
  public
    constructor Create;
    destructor Destroy; override;
    property Tag: Integer read FTag write FTag;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: LongWord read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

procedure Register;

implementation
{-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-}

{*** CONSTRUCTOR ***}
constructor TICQNet.Create;
begin
  inherited Create;
  FSocket := nil; FSendSocket := nil;
  FHTTPBuffer := TNetBuffer.Create;
end;

{*** DESTRUCTOR ***}
destructor TICQNet.Destroy;
begin
  FreeSocket(FSocket);
  FreeSocket(FSendSocket);
  FHTTPBuffer.Free;
  inherited Destroy;
end;

procedure TICQNet._OnConnect(Sender: TObject);
begin
  if FProxyType = P_HTTP then begin
    if Sender = FSocket then begin
      if not FHTTPConnected then begin
        FHTTPConnected := True;
        FSocket.SendStr(CreateHTTP_INIT(ProxyAuth, ProxyUser, ProxyPass))
      end else
        FSocket.SendStr(CreateHTTP_RECV(FHTTPIP, FHTTPSid, ProxyAuth, ProxyUser, ProxyPass));
    end else begin
      FCanSend := True;
      SendHTTPData;
    end;
  end;
end;

procedure TICQNet._OnError(Sender: TObject; ErrorType: TErrorType; ErrorMsg: String);
begin
  if Sender = FSocket then FreeSocket(FSendSocket) else FreeSocket(FSocket);

  if Assigned(OnError) then
    FOnError(Self, ErrorType, ErrorMsg);

  if Assigned(OnConnectError) then
    FOnConnectError(Self);
end;

procedure TICQNet._OnConnectError(Sender: TObject);
begin
  if Sender = FSocket then FreeSocket(FSendSocket) else FreeSocket(FSocket);

  if Assigned(OnConnectError) then
    FOnConnectError(Self);
end;

procedure TICQNet._OnDisconnect(Sender: TObject);
begin
  if Sender = FSocket then FreeSocket(FSendSocket) else FreeSocket(FSocket);

  if Assigned(OnDisconnect) then
    FOnDisconnect(Self);
end;

procedure TICQNet._OnReceive(Sender: TObject; Buffer: Pointer; BufLen: LongWord);
begin
  if Sender = FSocket then begin
    if ProxyType <> P_HTTP then
      HandleFlapData(Buffer, BufLen)
    else begin
      {$IFDEF HTTP_DEBUG}
      LogText('http.txt', 'Received (proxy): '#13#10 + DumpPacket(Buffer, BufLen) + #13#10#13#10);
      {$ENDIF}
      HandleHTTPData(Buffer, BufLen);
      HTTPReconnect;
    end;
  end else begin
    {$IFDEF HTTP_DEBUG}
    LogText('http.txt', 'Received (send-proxy): '#13#10 + PChar(Buffer));
    {$ENDIF}
    FCanSend := False;
    HTTPResend;
  end;
end;

procedure TICQNet._OnDataSent(Sender: TObject);
begin
end;

procedure TICQNet.SendHTTPData;
var
  Len: LongWord;
  Buffer: array[0..$FFFF-1] of Byte;
  Pkt: TRawPkt;
begin
  if (FCanSend) then begin
    if FHTTPFirstResend then begin
      CreateHTTP_LOGIN(@pkt, Host, Port);

      Inc(FHTTPSeq);
      FSendSocket.SendStr(CreateHTTP_Header('POST', 'http://' + FHTTPIP + '/data?sid=' + FHTTPSid + '&seq=' + IntToStr(FHTTPSeq), FHTTPIP, pkt.Len + 14, ProxyAuth, ProxyUser, ProxyPass));

      CreateHTTP_DATA_HDR(@pkt, 3, pkt.Len);
      FSendSocket.SendData(@pkt, pkt.Len);

      CreateHTTP_LOGIN(@pkt, Host, Port);

      FSendSocket.SendData(@pkt, pkt.Len);
      FHTTPFirstResend := False;
      FHTTPBuffer.Enter;
      FHTTPBuffer.Clear;
      FHTTPBuffer.Leave;
    end;
    FHTTPBuffer.Enter;
    Len := FHTTPBuffer.GetLength;
    if Len > 0 then begin
      Inc(FHTTPSeq);
      FSendSocket.SendStr(CreateHTTP_Header('POST', 'http://' + FHTTPIP + '/data?sid=' + FHTTPSid + '&seq=' + IntToStr(FHTTPSeq), FHTTPIP, Len + 14, ProxyAuth, ProxyUser, ProxyPass));

      if FHTTPFirstResend then begin
      end else
        CreateHTTP_DATA_HDR(@pkt, 5, Len);

      {$IFDEF HTTP_DEBUG}
      LogText('http.txt', 'SENDING: '#13#10 + DumpPacket(@pkt, pkt.Len) + #13#10#13#10);
      {$ENDIF}
      FSendSocket.SendData(@pkt, pkt.Len);

      Len := FHTTPBuffer.GetPacket(@Buffer);
      while Len > 0 do begin
        {$IFDEF HTTP_DEBUG}
        LogText('http.txt', 'SENDING: '#13#10 + DumpPacket(@Buffer, Len) + #13#10#13#10);
        {$ENDIF}
        FHTTPBuffer.DelPacket;
        FSendSocket.SendData(@Buffer, Len);
        Len := FHTTPBuffer.GetPacket(@Buffer);
      end;
      FCanSend := False;
    end;
    FHTTPBuffer.Leave;
  end;
end;

procedure TICQNet.HTTPReconnect;
begin
  FreeSocket(FSocket);

  FSocket := THTTPSocket.Create;

  FSocket.ProxyHost := FProxyHost;
  FSocket.ProxyPort := FProxyPort;
  FSocket.Host := FHTTPIP;
  FSocket.Port := FHTTPPort;
  FSocket.ProxyReady := False;

  FSocket.ProxyUser := FProxyUser;
  FSocket.ProxyPass := FProxyPass;
  FSocket.ProxyAuth := FProxyAuth;
  FSocket.ProxyResolve := FProxyResolve;
  FSocket._OnConnect := _OnConnect;
  FSocket._OnError := _OnError;
  FSocket._OnConnectError := _OnConnectError;
  FSocket._OnDisconnect := _OnDisconnect;
  FSocket._OnReceive := _OnReceive;
  FSocket.ErrorLanguage := FErrLang;
  FSocket.Connect;

  {$IFDEF HTTP_DEBUG}
  LogText('http.txt', 'Initializing RECV thread...' + #13#10#13#10);
  {$ENDIF}
end;

procedure TICQNet.HTTPResend;
begin
  FreeSocket(FSendSocket);

  FSendSocket := THTTPSocket.Create;

  FSendSocket.ProxyHost := FProxyHost;
  FSendSocket.ProxyPort := FProxyPort;
  FSendSocket.Host := FHTTPIP;
  FSendSocket.Port := FHTTPPort;
  FSendSocket.ProxyReady := False;

  FSendSocket.ProxyUser := FProxyUser;
  FSendSocket.ProxyPass := FProxyPass;
  FSendSocket.ProxyAuth := FProxyAuth;
  FSendSocket.ProxyResolve := FProxyResolve;
  FSendSocket._OnConnect := _OnConnect;
  FSendSocket._OnError := _OnError;
  FSendSocket._OnConnectError := _OnConnectError;
  FSendSocket._OnDisconnect := _OnDisconnect;
  FSendSocket._OnReceive := _OnReceive;
  FSendSocket._OnDataSent := _OnDataSent;
  FSendSocket.ErrorLanguage := FErrLang;
  FSendSocket.Connect;

  {$IFDEF HTTP_DEBUG}
  LogText('http.txt', 'Initializing SEND thread...' + #13#10#13#10);
  {$ENDIF}
end;

procedure TICQNet.HandleHTTPDataPak(Buffer: Pointer; BufLen: LongWord);
var
  pkt: TRawPkt;
  ptype: Word;
  sw: LongWord;
begin
  Move(Buffer^, pkt.Data, BufLen);
  pkt.Len := 0;
  GetInt(@pkt, 2); //Version
  ptype := GetInt(@pkt, 2);
  Inc(pkt.Len, 6);
  case ptype of
    2 {HELLO REPLY}:
    begin
      FHTTPSeq := 0;
      sw := GetInt(@pkt, 4); FHTTPSid := IntToHex(sw, 8);
      sw := GetInt(@pkt, 4); FHTTPSid := FHTTPSid + IntToHex(sw, 8);
      sw := GetInt(@pkt, 4); FHTTPSid := FHTTPSid + IntToHex(sw, 8);
      sw := GetInt(@pkt, 4); FHTTPSid := FHTTPSid + IntToHex(sw, 8);
      FHTTPSid := LowerCase(FHTTPSid);
      FHTTPIP := GetWStr(@pkt);
      FHTTPPort := GetLInt(@pkt, 2);
      {FHTTPBuffer.Enter;
      //FHTTPBuffer.Clear;
      FHTTPBuffer.AddPacket(@pkt, pkt.Len);
      FHTTPBuffer.Leave;}
      FHTTPFirstResend := True;
      HTTPResend; {Initialize send connection}
    end;
    5 {FLAP PACKETS}: begin
      HandleFlapData(Ptr(LongWord(@pkt.Data) + pkt.Len + 2), BufLen - pkt.Len - 2);
    end;
    7: begin
      FreeSockets;
      if Assigned(OnError) then
        FOnError(Self, ERR_PROXY, ICQLanguages[FErrLang].Translate(IMSG_EHTTP_INIT));
      if Assigned(OnConnectError)
         then FOnConnectError(Self);
    end;
  end;
end;

procedure TICQNet.HandleHTTPData(Buffer: Pointer; BufLen: LongWord);
var
  Len: Word;
  Buf: TRawPkt;
  l: LongWord;
begin
  l := 0;
  while True do
  begin
    if l = BufLen then Break;
    Len := Swap16(PWord(Buffer)^);
    if (Len > 8192) or (Len < 12) then Break;
    Move(Ptr(LongWord(Buffer) + 2)^, Buf, Len);
    Inc(l, Len + 2);
    Buffer := Ptr(LongWord(Buffer) + Len + 2);
    {Handle ICQ Pak packet}
    HandleHTTPDataPak(@Buf, Len);
  end;
end;

procedure TICQNet.HandleFlapData(Buffer: Pointer; BufLen: LongWord);
var
  i, len: LongWord;
  flap: TFlapHdr;
begin
  inherited;
  for i := 0 to BufLen - 1 do
  begin
    FSrcBuf[FSrcLen] := PByte(LongWord(Buffer) + i)^;
    Inc(FSrcLen);
    //Searching for the Flap header
    if (FSrcLen >= TFLAPSZ) and (not FFlapSet) then
    begin
      FFlapSet := True;
      FNewFlap := PFlapHdr(@FSrcBuf)^;
      FNewFlap.DataLen := Swap16(FNewFlap.DataLen);
      FNewFlap.Seq := Swap16(FNewFlap.Seq);
      if FNewFlap.DataLen > 8192 then
      begin
        if Assigned(OnError) then
          OnError(Self, ERR_PROTOCOL, ICQLanguages[FErrLang].Translate(IMSG_EPROTO_LEN));
        FreeSocket(FSocket);
        FreeSocket(FSendSocket);
        Exit;
      end;
    end;
    //Whole packet was received
    if FSrcLen = FNewFlap.DataLen + TFLAPSZ then
    begin
      if FNewFlap.Ident <> $2a then
      begin
        if Assigned(OnError) then
          OnError(Self, ERR_PROTOCOL, ICQLanguages[FErrLang].Translate(IMSG_EMALFORMED_PKT));
        FreeSocket(FSocket);
        FreeSocket(FSendSocket);
        Exit;
      end;
      Move(FNewFlap, flap, SizeOf(FNewFlap));
      //Preparing structures for receiving the next packet
      FNewFlap.DataLen := 0;
      len := FSrcLen; FSrcLen := 0;
      FFlapSet := False;
      //Dump packet (if needed)
      if Assigned(OnPktParse) then
        OnPktParse(Self, @FSrcBuf, len, True);
      //Handling packet
      if Assigned(OnHandlePkt) then
        FOnHandlePkt(flap, Ptr(LongWord(@FSrcBuf) + TFLAPSZ));
    end;
  end;
end;

procedure TICQNet.Connect;
begin
  FSrcLen := 0;
  FFlapSet := False;
  FreeSocket(FSocket);
  FreeSocket(FSendSocket);
  FHTTPConnected := False;
  FCanSend := False;
  case FProxyType of
    P_NONE:     FSocket := TEventSocket.Create;         {do not use proxy}
    P_SOCKS4:   FSocket := TSOCKS4Socket.Create;        {use socks4/4a proxy}
    P_SOCKS5:   FSocket := TSOCKS5Socket.Create;        {use socks5 proxy}
    P_HTTP:     FSocket := THTTPSocket.Create;          {use http proxy}
    P_HTTPS:    FSocket := THTTPSSocket.Create;         {use https proxy}
  else
    Exit;
  end;
  if FProxyType = P_NONE then begin
    FSocket.ProxyReady := True;
    FSocket.ProxyHost := FHost;
    FSocket.ProxyPort := FPort;
  end else
  if FProxyType = P_HTTP then begin
    FSocket.ProxyHost := FProxyHost;
    FSocket.ProxyPort := FProxyPort;
    FSocket.Host := 'http.proxy.icq.com';
    FSocket.Port := 80;
    FSocket.ProxyReady := False;
  end else begin
    FSocket.Host := FHost;
    FSocket.Port := FPort;
    FSocket.ProxyHost := FProxyHost;
    FSocket.ProxyPort := FProxyPort;
    FSocket.ProxyReady := False;
  end;

  if FProxyType = P_HTTP then begin
    FHTTPBuffer.Enter;
    FHTTPBuffer.Clear;
    FHTTPBuffer.Leave;
  end;

  FSocket.ProxyUser := FProxyUser;
  FSocket.ProxyPass := FProxyPass;
  FSocket.ProxyAuth := FProxyAuth;
  FSocket.ProxyResolve := FProxyResolve;
  FSocket._OnConnect := _OnConnect;
  FSocket._OnError := _OnError;
  FSocket._OnConnectError := _OnConnectError;
  FSocket._OnDisconnect := _OnDisconnect;
  FSocket._OnReceive := _OnReceive;
  FSocket.Connect;
end;

procedure TICQNet.SendData(var Data; DataLen: LongWord);
begin
  if FProxyType <> P_HTTP then begin
    if FSocket <> nil then begin
      if Assigned(OnPktParse) then
        OnPktParse(Self, @Data, DataLen, False);
      FSocket.SendData(@Data, DataLen);
    end
  end else begin
    if Assigned(OnPktParse) then
      OnPktParse(Self, @Data, DataLen, False);
    FHTTPBuffer.Enter;
    FHTTPBuffer.AddPacket(@Data, DataLen);
    FHTTPBuffer.Leave;
    SendHTTPData;
  end;
end;

procedure TICQNet.FreeSocket(var Socket: TEventSocket);
begin
  {Free socket safe}
  if (Socket <> nil) then begin
    if Socket.Working then begin {working means not-terminated}
      Socket.FreeOnTerminate := True;
      Socket.Terminate;
    end else
      Socket.Free;
    Socket := nil;
  end;
end;

procedure TICQNet.FreeSockets;
begin
  FreeSocket(FSocket);
  FreeSocket(FSendSocket);
end;

procedure TICQNet.Disconnect;
begin
  FreeSockets;
  if Assigned(OnDisconnect) then
    FOnDisconnect(Self);
end;
{-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-}

{*** CONSTRUCTOR ***}
constructor TICQClient.Create(AOwner: TComponent);
begin
  inherited;
  FLastError := '';                     //Last error

  gPortRange.First := 3000;             // Kludge for port range support
  gPortRange.Last  := 50000;

  FContactLst := TStringList.Create;    //Contact list
  FVisibleLst := TStringList.Create;    //Visible list
  FInvisibleLst := TStringList.Create;  //Invisible list

  FInfoChain := TStringList.Create;     //Info request chain
  FSInfoChain := TStringList.Create;    //Short info request chain

  //Socket for working with TCP
  FSock := TICQNet.Create;
  //Assign events  
  FSock.OnError := OnIntError;
  FSock.OnHandlePkt := HandlePacket;
  FSock.OnDisconnect := FTOnDisconnect;
  FSock.OnConnectError := FTOnConnectError;
  FSock.OnPktParse := FTOnPktParse;

  //Set default port and server
  if ICQPort = 0 then ICQPort := 5190;
  if ICQServer = '' then ICQServer := 'login.icq.com';


  FTimer := TMyTimer.Create;              //Timeout timer
  FTimer.OnTimer := OnTimeout;          //Set timeout event
  FTimer.Enabled := False;              //Disable timer by default

  Randomize;                            //Initialize random generator
  FSeq := Random($AAAA);                //Choose random seq, which is used in Flap header

  FDirect := nil;                       //Do not initialize direct control until we connect
end;

{*** DESTRUCTOR ***}
destructor TICQClient.Destroy;
begin
  if FDirect <> nil then
    FDirect.Free;

  FSock.OnConnectError := nil;
  //FSock.OnConnect := nil;
  FSock.OnDisconnect := nil;
  FSock.OnError := nil;
  //FSock.OnSockReceive := nil;
  FSock.Free;

  FTimer.OnTimer := nil;
  FTimer.Free;

  //Free TStringList objects
  FContactLst.Free;
  FVisibleLst.Free;
  FInvisibleLst.Free;
  FInfoChain.Free;
  FSInfoChain.Free;

  inherited;
end;

{Set NetICQ's properties}
procedure TICQClient.InitNetICQ;
begin
  //Assign properties
  FSock.Host := FIp;
  FSock.Port := FPort;
  FSock.ProxyType := FProxyType;
  FSock.ProxyHost := FProxyHost;
  FSock.ProxyPort := FProxyPort;
  FSock.ProxyUser := FProxyUser;
  FSock.ProxyAuth := FProxyAuth;
  FSock.ProxyPass := FProxyPass;
  FSock.ProxyResolve := FProxyResolve;
  FSock.ErrorLanguage := FErrLang;

//  //Assign events
//  FSock.OnHandlePkt := HandlePacket;
//  FSock.OnDisconnect := FTOnDisconnect;
//  FSock.OnConnectError := FTOnConnectError;
//  FSock.OnPktParseA := FTOnPktParse;
end;

{Called when error happened.}
procedure TICQClient.OnIntError(Sender: TObject; ErrorType: TErrorType; ErrorMsg: String);
begin
  FLastError := ErrorMsg;
  if Assigned(OnError) then
    FOnError(Self, ErrorType, ErrorMsg);
end;

{Logoff of server.}
Procedure TICQClient.LogOff;
var
  pkt: TRawPkt;
Begin
  CreateCLI_GOODBYE(@pkt, fSeq);
  fSock.SendData(Pkt, Pkt.Len);
  Sleep(10);
  Disconnect;
//  FSock.Disconnect;
  If fDirect <> nil then Begin
    fDirect.free;
    fDirect := nil;
  End;
  FTimer.Enabled := False;
  if assigned(OnLogOff) Then
    fOnLogOff(Self);
End;

{Logins to server.}
procedure TICQClient.Login(Status: LongWord = S_ONLINE; BirthDay: Boolean = False);
begin
  if FDirect <> nil then
  begin
    FDirect.OnError := nil;
    FDirect.OnHandle := nil;
    FDirect.OnPktDump := nil;
    FDirect.Free;
  end;

  if not DisableDirectConnections then
  begin
    FDirect := TDirectControl.Create(FLUIN);
    FDirect.OnPktDump := FTOnDirectParse;
    FDirect.OnHandle := HDirectMsg;
    FDirect.OnError := OnIntError;
    FDirect.OnFTInit := OnFTInitProc;
    FDirect.OnFTStart := OnFTStartProc;
    FDirect.OnFTFileData := OnFTFileDataProc;
    FDirect.OnSendFileStart := OnSendFileStartProc;
    FDirect.OnSendFileData  := OnSendFileDataProc;

    //Assign proxy settings
    FDirect.ProxyType := ProxyType;
    FDirect.ProxyHost := ProxyHost;
    FDirect.ProxyPort := ProxyPort;
    FDirect.ProxyUserID := ProxyUserID;
    FDirect.ProxyAuth := ProxyAuth;
    FDirect.ProxyPass := ProxyPass;
    FDirect.UseProxyResolve := ProxyResolve;
  end;


  FDSeq := Random(High(Word));
  FSeq2 := 2;
  FCookie := '';
  FFirstConnect := True;
  FStatus := Status;
  If BirthDay then FStatus := FStatus or SF_BIRTH;
  FLoggedIn := False;
  FRegisteringUIN := False;

  InitNetICQ;
  FTimer.Interval := FTimeout * 1000;
  FTimer.Enabled := False;
  if FTimeout <> 0 then
    FTimer.Enabled := True;

  FSock.Connect;
end;

procedure TICQClient.RequestOfflineMessages; // *yegor
var
   pkt: TRawPkt;
begin
     if not LoggedIn
        then Exit;
     CreateCLI_TOICQSRV(@pkt, FLUIN, CMD_REQOFFMSG, nil, 0, FSeq, FSeq2);{SNAC(x15/x02)}
     FSock.SendData(pkt, pkt.Len);
end; // **yegor

{Registers a new UIN.}
procedure TICQClient.RegisterNewUIN(const Password: String);
begin
  FRegisteringUIN := True;
  FRegPassword := Password;
  FLoggedIn := False;
  InitNetICQ;
  FTimer.Interval := FTimeout * 1000;
  FTimer.Enabled := True;
  FSock.Connect;
end;

{Disconnect user from server.}
procedure TICQClient.Disconnect;
begin
  FTimer.Enabled := False;
  FSock.Disconnect;
  if Assigned(OnConnectionFailed) then
    FOnConnectionFailed(Self);
end;

{Send a message to UIN.}
procedure TICQClient.SendMessage(UIN: LongWord; const Msg: String);
var
  pkt: TRawPkt;
begin
  if not LoggedIn then Exit;
  CreateCLI_SENDMSG(@pkt, 0, Random($FFFFAA), UIN, Msg, FSeq);
  FSock.SendData(pkt, pkt.Len);
end;

{Send an URL message to UIN.}
procedure TICQClient.SendURL(UIN: LongWord; const URL, Description: String);
var
  pkt: TRawPkt;
begin
  if not LoggedIn then Exit;
  CreateCLI_SENDURL(@pkt, 0, Random($FFFFAA), FLUIN, UIN, URL, Description, FSeq);
  FSock.SendData(pkt, pkt.Len);
end;

{Adds UIN to contact list after logon(when you are online), UIN automaticly
added to ContactList TStrings. After adding the UIN you will receive status
notifications. Returns True when UIN is added to the list(it wasn't there before).}
function TICQClient.AddContact(UIN: LongWord): Boolean;
var
  pkt: TRawPkt;
begin
  Result := False;
  if FContactLst.IndexOf(IntToStr(UIN)) < 0 then
  begin
    FContactLst.Add(IntToStr(UIN));
    Result := True;
  end else
    Exit;
  if not LoggedIn then Exit;
  CreateCLI_ADDCONTACT(@pkt, IntToStr(UIN), FSeq);           {SNAC(x03/x04)}
  FSock.SendData(pkt, pkt.Len);
end;

{Adds list of UINs to contact list after logon(when you are online), UIN automaticly
added to ContactList TStrings. After adding the UINs you will receive status
notifications. Returns True when UINs are added to the list(it wasn't there before).}
function TICQClient.AddContactMulti(UINlist: array of LongWord): Boolean;
var
pkt: TRawPkt;
temp : integer;
begin
  for temp := Low(UINList) to High(UINList) do
    FContactlst.Add(inttostr(UINlist[temp]));
  CreateCLI_ADDCONTACT_multi(@pkt, UINList, FSeq); {SNAC(x03/x04)}
  FSock.SendData(pkt, pkt.Len);
  Result := True;
end;

{Addes UIN to the visible list. Use while you are online.
Returns True when UIN is added to the list(it wasn't there before).}
Function TICQClient.AddContactVisible(UIN: LongWord):Boolean;
var
  idx: Integer;
  pkt: TRawPkt;
  SL:TSTringList;
begin
  Result := False;
  idx := FVisibleLst.IndexOf(IntToStr(UIN));
  if idx > -1 then
    Exit;  // Already in list
  FVisibleLst.Add(IntToStr(UIN));
  Result := True;
  if not LoggedIn then Exit;
  if StatusToInt (FStatus)=S_INVISIBLE // *yegor
     then begin
          SL := TStringList.Create;
          Try
            SL.Add(IntToStr(UIN));
            CreateCLI_ADDVISIBLE(@pkt, SL, FSeq);
          Finally
            SL.Free;
          End;
          FSock.SendData(pkt, pkt.Len);
     end; // **yegor
end;

{Addes UIN to the invisible list. Use while you are online.
Returns True when UIN is added to the list(it wasn't there before).}
Function TICQClient.AddContactInvisible(UIN: LongWord):Boolean;
var
  idx: Integer;
  pkt: TRawPkt;
  SL:TSTringList;
begin
  Result := False;
  idx := FInVisibleLst.IndexOf(IntToStr(UIN));
  if idx > -1 then
    Exit;  // Already in list
  FInVisibleLst.Add(IntToStr(UIN));
  Result := True;
  if not LoggedIn then Exit;
  if StatusToInt (FStatus)<>S_INVISIBLE // *yegor
     then begin
          SL := TStringList.Create;
          Try
            SL.Add(IntToStr(UIN));
            CreateCLI_ADDINVISIBLE(@pkt, SL, FSeq);
          Finally
            SL.Free;
          End;
          FSock.SendData(pkt, pkt.Len);
     end; // **yegor
end;

{Removes UIN from contact list. Use while you are online.}
procedure TICQClient.RemoveContact(UIN: LongWord);
var
  idx: Integer;
  pkt: TRawPkt;
begin
  idx := FContactLst.IndexOf(IntToStr(UIN));
  if idx > -1 then
    FContactLst.Delete(idx);
  if not LoggedIn then Exit;
  CreateCLI_REMOVECONTACT(@pkt, UIN, FSeq);
  FSock.SendData(pkt, pkt.Len);
end;

{Removes UIN from the visible list. Use while you are online.}
procedure TICQClient.RemoveContactVisible(UIN: LongWord);
var
  idx: Integer;
  pkt: TRawPkt;
begin
  idx := FVisibleLst.IndexOf(IntToStr(UIN));
  if idx > -1 then
    FVisibleLst.Delete(idx);
  if not LoggedIn then Exit;
  if StatusToInt (FStatus)=S_INVISIBLE // *yegor
     then begin
          CreateCLI_REMVISIBLE(@pkt, UIN, FSeq);
          FSock.SendData(pkt, pkt.Len);
     end; // **yegor
end;

{Removes UIN from the invisible list. Use while you are online.}
procedure TICQClient.RemoveContactInvisible(UIN: LongWord);
var
  idx: Integer;
  pkt: TRawPkt;
begin
  idx := FInvisibleLst.IndexOf(IntToStr(UIN));
  if idx > -1 then
    FInvisibleLst.Delete(idx);
  if not LoggedIn then Exit;
  if StatusToInt (FStatus)<>S_INVISIBLE // *yegor
     then begin
          CreateCLI_REMINVISIBLE(@pkt, UIN, FSeq);
          FSock.SendData(pkt, pkt.Len);
     end; // **yegor
end;

{Query info about UIN. As answer you will recieve theese events: OnUserWorkInfo,
OnUserInfoMore, OnUserInfoAbout, OnUserInfoInterests, OnUserInfoMoreEmails,
OnUserFound.}
procedure TICQClient.RequestInfo(UIN: LongWord);
var
  pkt: TRawPkt;
begin
  if not LoggedIn then Exit;
  FInfoChain.Values[IntToStr(FSeq2)] := IntToStr(UIN);
  CreateCLI_METAREQINFO(@pkt, FLUIN, UIN, FSeq, FSeq2);
  FSock.SendData(pkt, pkt.Len);
end;

{Request short info(nick, first, last, email) of UIN.}
procedure TICQClient.RequestInfoShort(UIN: LongWord);
var
  pkt: TRawPkt;
begin
  if not LoggedIn then Exit;
  FSInfoChain.Values[IntToStr(FSeq2)] := IntToStr(UIN);
  CreateCLI_METAREQINFO_SHORT(@pkt, FLUIN, UIN, FSeq, FSeq2);
  FSock.SendData(pkt, pkt.Len);
end;

{Searches user by Mail}
procedure TICQClient.SearchByMail(const Email: String);
var
  pkt: TRawPkt;
begin
  if not LoggedIn then Exit;
  CreateCLI_SEARCHBYMAIL(@pkt, FLUIN, Email, FSeq, FSeq2);
  FSock.SendData(pkt, pkt.Len);
end;

{Searches user by UIN}
procedure TICQClient.SearchByUIN(UIN: LongWord);
var
  pkt: TRawPkt;
begin
  if not LoggedIn then Exit;
  CreateCLI_SEARCHBYUIN(@pkt, FLUIN, UIN, FSeq, FSeq2);
  FSock.SendData(pkt, pkt.Len);
end;

{Searches user by Name and other data}
procedure TICQClient.SearchByName(const FirstName, LastName, NickName, Email: String);
var
  pkt: TRawPkt;
begin
  if not LoggedIn then Exit;
  CreateCLI_SEARCHBYNAME(@pkt, FLUIN, FirstName, LastName, NickName, Email, FSeq, FSeq2);
  FSock.SendData(pkt, pkt.Len);
end;

{Searches random user from Group, where Group id could be found in RandGroups:
array[1..11]...(ICQWorks.pas) constant. As answer you will receive OnUserFound
notification, only one user will be found.}
procedure TICQClient.SearchRandom(Group: Word);
var
  pkt: TRawPkt;
begin
  if not LoggedIn then Exit;
  CreateCLI_SEARCHRANDOM(@pkt, FLUIN, Group, FSeq, FSeq2);
  FSock.SendData(pkt, pkt.Len);
end;

{Searches user in 'White Pages'. As answer you will receive OnUserFound notification
when at least one user found or OnUserNotFound if such user does not exist.}
procedure TICQClient.SearchWhitePages(const FirstName, LastName, NickName, Email: String; MinAge, MaxAge: Word;
  Gender: Byte; const Language, City, Country, Company, Department, Position, Occupation,
  Organization, OrganKeyWords, PastAffiliation, AffiKeyWords, KeyWord: String; Online: Boolean);
var
  pkt: TRawPkt;
begin
  if not LoggedIn then Exit;
  CreateCLI_SEARCHWP(@pkt, FLUIN, FirstName, LastName, NickName, Email,
    MinAge, MaxAge,
    Gender,
    StrToLanguageI(Language),
    City, StrToCountryI(Country),
    Company,
    Department,
    Position,
    StrToOccupationI(Occupation),
    StrToOrganizationI(Organization),
    OrganKeyWords,
    StrToPastI(PastAffiliation),
    AffiKeyWords,
    KeyWord,
    Ord(Online),
    FSeq,
    FSeq2);
  FSock.SendData(pkt, pkt.Len);
end;

{Set general info about yourself. You can skip some parameters (eg. use '' -
empty strings) to unspecify some info. }
procedure TICQClient.SetSelfInfoGeneral(NickName, FirstName, LastName, Email, City, State, Phone, Fax, Street, Cellular, Zip, Country: String; TimeZone: Byte; PublishEmail: Boolean);
var
  pkt: TRawPkt;
begin
  if not LoggedIn then Exit;
  //Truncate state if more then 3 chars
  if Length(State) > 3 then
    State := Copy(State, 0, 3);
  CreateCLI_METASETGENERAL(@pkt, FLUIN, NickName, FirstName, LastName, Email, City, State, Phone, Fax, Street, Cellular, Zip, StrToCountryI(Country), TimeZone, PublishEmail, FSeq, FSeq2);
  FSock.SendData(pkt, pkt.Len);
end;

{Set more info about yourself.}
procedure TICQClient.SetSelfInfoMore(Age: Word; Gender: Byte; const HomePage: String; BirthYear: Word; BirthMonth, BirthDay: Byte; Language1, Language2, Language3: String);
var
  pkt: TRawPkt;
begin
  if not LoggedIn then Exit;
  CreateCLI_METASETMORE(@pkt, FLUIN, Age, Gender, HomePage, BirthYear, BirthMonth, BirthDay, StrToLanguageI(Language1), StrToLanguageI(Language2), StrToLanguageI(Language3), FSeq, FSeq2);
  FSock.SendData(pkt, pkt.Len);
end;

{Set info about yourself.}
procedure TICQClient.SetSelfInfoAbout(const About: String);
var
  pkt: TRawPkt;
begin
  if not LoggedIn then Exit;
  CreateCLI_METASETABOUT(@pkt, FLUIN, About, FSeq, FSeq2);
  FSock.SendData(pkt, pkt.Len);
end;

{Requests server side contact list. For more info look at OnServerListRecv event.}
procedure TICQClient.RequestContactList;
var
  pkt: TRawPkt;
begin
  if not LoggedIn then Exit;
  CreateCLI_REQROSTER(@pkt, FSeq);
  FSock.SendData(pkt, pkt.Len);
end;

{Releases memory used while parsing the server side contact list.}
procedure TICQClient.DestroyUINList(var List: TList);
var
  i: Word;
begin
  if List = nil then Exit;
  if List.Count > 0 then
    for i := 0 to List.Count - 1 do
      FreeMem(List.Items[i], SizeOf(TUINEntry)); //Free allocated memory for TUINEntry
  List.Free;
  List := nil;
end;

{Sends sms message to Destination with Text.}
procedure TICQClient.SendSMS(const Destination, Text: String);
var
  pkt: TRawPkt;
begin
  if (Length(Text) = 0) or (not LoggedIn) then Exit;
  CreateCLI_SENDSMS(@pkt, FLUIN, Destination, Text, GetACP, GetSMSTime, FSeq, FSeq2);
  FSock.SendData(pkt, pkt.Len);
end;

{Sends Msg to UIN with advanced options, after UIN has got your message you will
receive confirmation. ID - randomly generated value, may be used for packet acknowledgements
(see OnAdvancedMsgAck event). If your Msg is in the RTF(RichText Format), then RTFFormat
parameter should be True, otherwise - False. Beware of using the RTF Format, some clients
(old versions of ICQ, linux & windows clones) don't support it.}
procedure TICQClient.SendMessageAdvanced(UIN: LongWord; const Msg: String; ID: Word; RTFFormat: Boolean);
var
  pkt: TRawPkt;
begin
  if (Length(Msg) = 0) or (not LoggedIn) then Exit;
  CreateCLI_SENDMSG_ADVANCED(@pkt, FStatus, 0, ID, UIN, Msg, RTFFormat, FSeq);
  FSock.SendData(pkt, pkt.Len);
end;

{Send YouWereAdded notification to user}
procedure TICQClient.SendYouWereAdded(UIN: LongWord);
var
  lpkt: TRawPkt;
begin
  CreateCLI_SEND_YOU_WERE_ADDED(@lpkt, 0, Random($FFFFAA), UIN, FLUIN, FSeq);
  FSock.SendData(lpkt, lpkt.Len);
end;

{Send message to client dirrectly when it's possible}
function TICQClient.SendMessageDirect(UIN: LongWord; const Msg: String; RTFFormat: Boolean): Word;
var
  lpkt: TRawPkt;
begin
  Result := 0;
  if FDirect = nil then Exit;
  if (FDSeq = 0) then Inc(FSeq);
  Result := CreatePEER_MSG(@lpkt, Msg, RTFFormat, FDSeq);
  if not FDirect.SendData(UIN, @lpkt) then
    Result := 0;
end;

{Request an away messages, set when user changes status.}
procedure TICQClient.RequestAwayMsg(UIN: LongWord; ID: Word; ReqStatus: Byte);
var
  pkt: TRawPkt;
begin
  if (not LoggedIn) then Exit;
  CreateCLI_REQAWAYMSG(@pkt, FStatus, 0, ID, UIN, ReqStatus, FSeq);
  FSock.SendData(pkt, pkt.Len);
end;

{Unregister an UIN number.}
procedure TICQClient.UnregisterUIN(const Password: String);
var
  pkt: TRawPkt;
begin
  if (not LoggedIn) then Exit;
  CreateCLI_UNREGUIN(@pkt, FLUIN, Password, FSeq, FSeq2);
  FSock.SendData(pkt, pkt.Len);
end;

{Change current password to NewPassword.}
procedure TICQClient.ChangePassword(const NewPassword: String);
var
  pkt: TRawPkt;
begin
  if (not LoggedIn) then Exit;
  CreateCLI_METASETPASS(@pkt, FLUIN, NewPassword, nil, 0, FSeq, FSeq2);
  FSock.SendData(pkt, pkt.Len);
end;

{Change current password to Buffer's value.}
procedure TICQClient.ChangePasswordPtr(Buffer: Pointer; BufLen: Word);
var
  pkt: TRawPkt;
begin
  if (not LoggedIn) then Exit;
  CreateCLI_METASETPASS(@pkt, FLUIN, '', Buffer, BufLen, FSeq, FSeq2);
  FSock.SendData(pkt, pkt.Len);
end;

{Returns True if direct connection with UIN has been estabilished.}
function TICQClient.DirectConnectionEstabilished(UIN: LongWord): Boolean;
begin
  Result := False;
  if FDirect = nil then Exit;
  Result := FDirect.ConnectionEstabilished(UIN);
end;

{Send contacts to UIN through server.}
function TICQClient.SendContacts(UIN: LongWord; Contacts: TStringList; ID: Word): Boolean;
var
  pkt: TRawPkt;
begin
  Result := False;
  if not LoggedIn then Exit;
  CreateCLI_SENDCONTACTS(@pkt, FStatus, 0, ID, UIN, Contacts, FSeq);
  FSock.SendData(pkt, pkt.Len);
  Result := True;
end;

{Request contacts from UIN through server.}
function TICQClient.RequestContacts(UIN: LongWord; const Reason: String; ID: Word): Boolean;
var
  pkt: TRawPkt;
begin
  Result := False;
  if not LoggedIn then Exit;
  CreateCLI_SENDCONTACTS_REQ(@pkt, FStatus, 0, ID, UIN, Reason, FSeq);
  FSock.SendData(pkt, pkt.Len);
  Result := True;  
end;

{Sends contacts to UIN directly. Returns ID of the packet or 0 if failed.}
function TICQClient.SendContactsDC(UIN: LongWord; Contacts: TStringList): Word;
var
  pkt: TRawPkt;
begin
  Result := 0;
  if FDirect = nil then Exit;
  if (FDSeq = 0) then Inc(FSeq);
  Result := CreatePEER_CONTACTS(@pkt, Contacts, FDSeq);
  if not FDirect.SendData(UIN, @pkt) then
    Result := 0;
end;

{Sends PEER_MSG with info to start a File Send. Returns ID or 0 if Fialed.}
Function TICQClient.SendFileDC(UIN:LongWord; FileName, Description:String): Word;
Var
  Pkt:TRawPkt;
  FileSend:TSendFileRec;
  I:Integer;
Begin
  Result := 0;

  If fDirect = nil then exit;
  If (FDSeq = 0) then inc(FSeq);
//  If not FileExists(FileName) then Exit;

  FileSend.Files := TStringList.Create;
  FileSend.FilesCurrent := 0;
  FileSend.Files.Text := FileName;
  If FileSend.Files.Count = 0 then Begin
    FileSend.Files.Free;
    FileSend.Files := nil;
    Exit;
  End;

  FileSend.UIN := UIN;
  FileSend.FilesCount := FileSend.Files.Count;
  FileSend.FilePath := ExtractFilePath(FileSend.Files[FileSend.FilesCurrent]);
  FileSend.FileName := ExtractFileName(FileSend.Files[FileSend.FilesCurrent]);
  FileSend.FileDescription := Description;
  FileSend.TotalSize := 0;
  for i := 0 to FileSend.Files.Count -1 do
    Inc(FileSend.TotalSize, FileSizeFromStr(FileSend.Files[i]));
  FileSend.FileSize  := FileSizeFromStr(FileSend.Files[FileSend.FilesCurrent]);
  FileSend.Port := FindBindPort;
  FileSend.Speed:= 100;
  fDirect.SetFileRecord(UIN, FileSend);
  Result := CreatePEER_MSG_FILE(@Pkt, FileSend, FDSeq);
  If Not FDirect.SendData(UIN, @Pkt) then
    Result := 0;
End;

Procedure TICQClient.SendFileCancel(UIN: LongWord);
Begin
  If FDirect <> nil then
    fDirect.StopFileSending(UIN);
End;

{Request contacts from UIN directly. Returns ID of the packet or 0 if failed.}
function TICQClient.RequestContactsDC(UIN: LongWord; const Reason: String): Word;
var
  lpkt: TRawPkt;
begin
  Result := 0;
  if FDirect = nil then Exit;
  if (FDSeq = 0) then Inc(FSeq);
  Result := CreatePEER_CONTACTREQ(@lpkt, Reason, FDSeq);
  if not FDirect.SendData(UIN, @lpkt) then
    Result := 0;
end;

{Send keep alive packet.}
procedure TICQClient.SendKeepAlive;
var
  lpkt: TRawPkt;
begin
  if (not LoggedIn) then Exit;
  CreateCLI_KEEPALIVE(@lpkt, FSeq);
  FSock.SendData(lpkt, lpkt.Len);
end;

{Set AuthorizationRequired and WebAware options.}
procedure TICQClient.SetAuthorization(AuthorizationRequired, WebAware: Boolean);
var
  lpkt: TRawPkt;
begin
  if (not LoggedIn) then Exit;
  CreateCLI_METASETPERMISSIONS(@lpkt, FLUIN, AuthorizationRequired, WebAware, FSeq, FSeq2);
  FSock.SendData(lpkt, lpkt.Len);
end;

{Request authorization.}
procedure TICQClient.SendAuthRequest(UIN: LongWord; Msg: String);
var
  lpkt: TRawPkt;
begin
  if (not LoggedIn) then Exit;
  CreateCLI_REQAUTH(@lpkt, UIN, Msg, FSeq);
  FSock.SendData(lpkt, lpkt.Len);
end;

procedure TICQCLient.SendAuthResponse (UIN: LongWord; Authorize: Boolean; Reason: string);
var
  opkt: TRawPkt;
begin
  if (not LoggedIn) then Exit;
  CreateCLI_AUTHORIZE(@opkt, UIN, Integer(Authorize), Reason, FSeq);
  FSock.SendData(opkt, opkt.Len);
end;

{Start changes of SSL.}
procedure TICQClient.SSLChangeStart(FirstUpload: Boolean);
var
  lpkt: TRawPkt;
begin
  if (not LoggedIn) then Exit;
  CreateCLI_ADDSTART(@lpkt, FirstUpload, FSeq);
  FSock.SendData(lpkt, lpkt.Len);
end;

{End changes of SSL.}
procedure TICQClient.SSLChangeEnd;
var
  lpkt: TRawPkt;
begin
  if (not LoggedIn) then Exit;
  CreateCLI_ADDEND(@lpkt, FSeq);
  FSock.SendData(lpkt, lpkt.Len);
end;

{Add group to SSL.}
procedure TICQClient.SSLAddGroup(GroupName: String; GroupID: Word);
var
  lpkt: TRawPkt;
begin
  if (not LoggedIn) then Exit;
  CreateCLI_ADDBUDDY(@lpkt, GroupName, '', '', GroupID, 0, BUDDY_GROUP, False, FSeq);
  FSock.SendData(lpkt, lpkt.Len);
end;

{Add user to SSL.}
procedure TICQClient.SSLAddUser(GroupID, UserID: Word; UIN, Name, SMSNumber: String; Authorize, UpdateUser: Boolean);
var
  lpkt: TRawPkt;
begin
  if (not LoggedIn) then Exit;
  if not UpdateUser then
    CreateCLI_ADDBUDDY(@lpkt, UIN, Name, SMSNumber, GroupID, UserID, BUDDY_NORMAL, Authorize, FSeq)
  else
    CreateCLI_UPDATEBUDDY(@lpkt, UIN, Name, SMSNumber, GroupID, UserID, BUDDY_NORMAL, Authorize, FSeq);
  FSock.SendData(lpkt, lpkt.Len);
end;

{Remove user from SSL.}
procedure TICQClient.SSLDelUser(GroupID, UserID: Word; UIN, Name, SMSNumber: String; Authorize: Boolean);
var
  lpkt: TRawPkt;
begin
  if (not LoggedIn) then Exit;
  CreateCLI_DELETEBUDDY(@lpkt, UIN, Name, SMSNumber, GroupID, UserID, BUDDY_NORMAL, Authorize, True, FSeq);
  FSock.SendData(lpkt, lpkt.Len);
end;

{Remove group from SSL.}
procedure TICQClient.SSLDelGroup(GroupName: String; GroupID: Word);
var
  lpkt: TRawPkt;
begin
  if (not LoggedIn) then Exit;
  CreateCLI_DELETEBUDDY(@lpkt, GroupName, '', '', GroupID, 0, BUDDY_GROUP, False, True, FSeq);
  FSock.SendData(lpkt, lpkt.Len);
end;

{Update group's ids.}
procedure TICQClient.SSLUpdateGroup(GroupName: String; GroupID: Word; UserIDs: TStringList);
var
  lpkt: TRawPkt;
begin
  if (not LoggedIn) then Exit;
  CreateCLI_UPDATEGROUP(@lpkt, GroupName, GroupID, UserIDs, FSeq);
  FSock.SendData(lpkt, lpkt.Len);
end;

{Add user to the specified SSL's list.}
procedure TICQClient.SSLAddUserIntoList(UserID: Word; UIN: String; BuddyType: Word);
var
  lpkt: TRawPkt;
begin
  if (not LoggedIn) then Exit;
  CreateCLI_ADDBUDDY(@lpkt, UIN, '', '', $0000, UserID, BuddyType, False, FSeq);
  FSock.SendData(lpkt, lpkt.Len);
end;

{Remove user from the specified SSL's list.}
procedure TICQClient.SSLDelUserFromList(UserID: Word; UIN: String; BuddyType: Word);
var
  lpkt: TRawPkt;
begin
  if (not LoggedIn) then Exit;
  CreateCLI_DELETEBUDDY(@lpkt, UIN, '', '', $0000, UserID, BuddyType, False, True, FSeq);
  FSock.SendData(lpkt, lpkt.Len);
end;

{Send response on a file request.}
function TICQClient.FTResponse(ResponseRec: TFTRequestRec; Accept: Boolean; Reason: String): Boolean;
var
  lpkt: TRawPkt;
begin
  if DisableDirectConnections then
  begin
    Result := False;
    Exit;
  end;
  if not Accept then
  begin
    if ResponseRec.ReqType <> 0 then
    begin
      {Never tested because didn't see when ICQ request ft through server, miranda-icq doest this but doesn't handle file declines even from real ICQ}
      CreateCLI_SENDMSG_FILEDECLINE(@lpkt, ResponseRec.Seq, ResponseRec.ITime, ResponseRec.IRandomID,
        ResponseRec.UIN, ResponseRec.FileSize,
        ResponseRec.Description, ResponseRec.FileName, Reason, 0, FSeq);
      FSock.SendData(lpkt, lpkt.Len);
      Result := True;
      Exit;
    end;
  end;
  if FDirect <> nil then
  if FDirect.AddFileUser(ResponseRec.UIN, ResponseRec.Port, @ResponseRec) then
    begin
      {Send response through estabilished direct connection}
      if ResponseRec.ReqType = 0 then
      begin
        CreatePEER_FILEINIT(@lpkt, True, ResponseRec.Description, ResponseRec.FileName, ResponseRec.Port,
          ResponseRec.FileSize, ResponseRec.Seq, Reason, Accept);
        Result := FDirect.SendData(ResponseRec.UIN, @lpkt);
      end else
      {Send response through server}
      begin
        CreateCLI_SENDMSG_FILEACK(@lpkt, FStatus, ResponseRec.Seq, ResponseRec.ITime, ResponseRec.IRandomID,
          ResponseRec.UIN, ResponseRec.FileSize, ResponseRec.Description, ResponseRec.FileName,
          ResponseRec.Port, FSeq);
        FSock.SendData(lpkt, lpkt.Len);
        Result := True;
      end;
      Exit;
    end else
      OnIntError(Self, ERR_WARNING, ICQLanguages[FErrLang].Translate(IMSG_WADD_USER));
  Result := False;
end;

procedure TICQClient.FTCancel(UIN: LongWord);
begin
  if FDirect <> nil then
    FDirect.StopFileReceiving(UIN);
end;

function TICQClient.FTStartResponse(StartRec: TFTStartRec): Boolean;
var
  lpkt: TRawPkt;
begin
  Result := False;
  if FDirect = nil then Exit;
  CreatePEER_FILE_INIT2(@lpkt, StartRec.Current, {StartPos} $00000000, StartRec.Speed);
  Result := FDirect.SendDataFile(StartRec.UIN, @lpkt);
end;

{@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@}
{Handling of all incoming packets}
procedure TICQClient.HandlePacket(Flap: TFlapHdr; Data: Pointer);
var
  FUIN: String;
  FData: String;
  pkt: TRawPkt;
  T: Word;
  Snac: TSnacHdr;
  i: Word;
  UINlist: array of LongWord;
begin
  case Flap.ChID of
    1: //Channel 1
    begin
      {SRV_HELLO}
      if Flap.DataLen = 4 then
      begin
        if FRegisteringUIN then
        begin
          //Send CLI_HELLO
          CreateCLI_HELLO(@pkt, FSeq);
          FSock.SendData(pkt, pkt.Len);
          //Register a new UIN.
          CreateCLI_REGISTERUSER(@pkt, FRegPassword, FSeq);
          FSock.SendData(pkt, pkt.Len);
          Exit;
        end;
        if FFirstConnect then
        begin
          //Send login packet
          CreateCLI_IDENT(@pkt, FLUIN, FLPass, FSeq);
          FSock.SendData(pkt, pkt.len);
        end else
        begin
          //Sending the cookie(second stage of login sequence)
          CreateCLI_COOKIE(@pkt, FCookie, FSeq);
          FSock.SendData(pkt, pkt.Len);
        end;
      end;
      FFirstConnect := False;
    end;
    2: //Channel 2
    begin
      Move(Data^, pkt.Data, Flap.DataLen);
      pkt.Len := 0;
      GetSnac(@pkt, Snac);

      if Snac.Flags and $8000 = $8000 then begin
      end;

      case Snac.Family of
        $01: //Family x01
          case Snac.SubType of
            $03: {SRV_FAMILIES}
            begin
              CreateCLI_FAMILIES(@pkt, FSeq);           {SNAC(x01/x17)}
              FSock.SendData(pkt, pkt.Len);
            end;
            $07: {SRV_RATES}
            begin
              CreateCLI_ACKRATES(@pkt, FSeq);           {SNAC(x01/x08)}
              FSock.SendData(pkt, pkt.Len);
              CreateCLI_SETICBM(@pkt, FSeq);            {SNAC(x04/x02)}
              FSock.SendData(pkt, pkt.Len);
              CreateCLI_REQINFO(@pkt, FSeq);            {SNAC(x01/x0E)}
              FSock.SendData(pkt, pkt.Len);
              CreateCLI_REQLOCATION(@pkt, FSeq);        {SNAC(x02/x02)}
              FSock.SendData(pkt, pkt.Len);
              CreateCLI_REQBUDDY(@pkt, FSeq);           {SNAC(x03/x02)}
              FSock.SendData(pkt, pkt.Len);
              CreateCLI_REQICBM(@pkt, FSeq);            {SNAC(x04/x04)}
              FSock.SendData(pkt, pkt.Len);
              CreateCLI_REQBOS(@pkt, FSeq);             {SNAC(x09/x02)}
              FSock.SendData(pkt, pkt.Len);
            end;
            $13: {SRV_MOTD}
            begin
              CreateCLI_RATESREQUEST(@pkt, FSeq);       {SNAC(x01/x06)}
              FSock.SendData(pkt, pkt.Len);
            end;
          end;
        $03: //Family x03
        begin
          case Snac.SubType of
            $0B: {SRV_USERONLINE}
              HSnac030B(Flap, Snac, @pkt);
            $0C: {SRV_USEROFFLINE}
            begin
              FData := GetStr(@pkt, GetInt(@pkt, 1));
              if Assigned(OnUserOffline) then
                FOnUserOffline(Self, FData);
            end;
          end;
        end;
        $04: //Family x04
          if Snac.SubType = $07 then {SRV_MSG}
            HSnac0407(Flap, Snac, @pkt)
          else if Snac.SubType = $0b then {SRV_MSGACK}
            HSnac040b(Flap, Snac, @pkt)
          Else if Snac.SubType = $0c Then {SRV_MSGACK}
            HSnac040c(Flap, Snac, @Pkt);
        $09: //Family x09
        begin
          if Snac.SubType = $03 then
          begin
            CreateCLI_SETUSERINFO(@pkt, FSeq);                                  {SNAC(x02/x04)}
            FSock.SendData(pkt, pkt.Len);
            // eraser 13.05.2004
            if FContactLst.Count > 0 then
            begin
              SetLength(UINList, FContactLst.Count);
              for i := 0 to FContactLst.Count - 1 do
                UINList[i] := StrToInt(FContactLst.Strings[i]);
              CreateCLI_ADDCONTACT_multi(@pkt, UINList, FSeq); {SNAC(x03/x04)}
              FSock.SendData(pkt, pkt.Len);
            end;
            //
            if StatusToInt(FStatus) <> S_INVISIBLE then
            begin
              CreateCLI_ADDINVISIBLE(@pkt, FInvisibleLst, FSeq);                {SNAC(x09/x07)}
              FSock.SendData(pkt, pkt.Len);
            end else
            begin
              CreateCLI_ADDVISIBLE(@pkt, FVisibleLst, FSeq);                    {SNAC(x09/x05)}
              FSock.SendData(pkt, pkt.Len);
            end;
            // eraser 15.05.2004
            if (StatusToInt(FStatus) = S_AWAY) or (StatusToInt(FStatus) = S_NA) then
            begin
              CreateCLI_SETIDLETIME(@pkt, True, FSeq);
              FSock.SendData(pkt, pkt.Len);
            end else
            begin
              CreateCLI_SETIDLETIME(@pkt, False, FSeq);
              FSock.SendData(pkt, pkt.Len);
            end;
            //
            FDConnCookie := Random(High(Integer));
            if FDirect <> nil then
            begin
              if ProxyType = P_NONE then
                i := FDirect.BindPort
              else
                i := 0;
              CreateCLI_SETSTATUS(@pkt, FStatus, GetLocalIP, i, FDConnCookie, FProxyType, FSeq)  {SNAC(x01/x1E)}
            end else
              CreateCLI_SETSTATUS(@pkt, FStatus, 0, 0, 0, FProxyType, FSeq);    {SNAC(x01/x1E)}
            FSock.SendData(pkt, pkt.Len);
            CreateCLI_READY(@pkt, FSeq);                                        {SNAC(x01/x02)}
            FSock.SendData(pkt, pkt.Len);
            {OnLogin Event}
            FLoggedIn := True;
            FTimer.Enabled := False;
            FInfoChain.Clear;
            FSInfoChain.Clear;
            if Assigned(OnLogin) then
              FOnLogin(Self);
          end;
        end;
        $13: //Family x13
        begin
          if Snac.SubType = $0e then
            HSnac130e(Flap, Snac, @pkt) {SRV_UPDATE_ACK}
          else if Snac.SubType = $1B then
            HSnac131b(Flap, Snac, @pkt) {SRV_AUTH}
          else if Snac.SubType = $1C then {SRV_ADDEDYOU}
            HSnac131C(Flap, Snac, @pkt)
          else if Snac.SubType = $19 then {SRV_AUTH_REQ}
            HSnac1319(Flap, Snac, @pkt)
          else if Snac.SubType = $06 then {SRV_REPLYROSTER}
            HSnac1306(Flap, Snac, @pkt);
        end;
        $15: //Family x15
        begin
          if Snac.SubType = $03 then {SRV_FROMICQSRV}
            HSnac1503(Flap, Snac, @pkt);
        end;
        $17:
        begin
          if Snac.SubType = $01 then {SRV_REGREFUSED}
          begin
            if Assigned(OnNewUINRefused) then
              FOnNewUINRefused(Self);
          end else
          if Snac.SubType = $05 then
            HSnac1705(Flap, Snac, @pkt);
        end;
      end;
    end;
    4: //Channel 4
    begin
      if FLoggedIn or FRegisteringUIN then
      begin
        FTOnConnectError(Self);
        FSock.Disconnect;
        Exit;
      end;
      Move(Data^, pkt.Data, Flap.DataLen); pkt.Len := 0;
      //SRV_COOKIE
      FUIN  := GetTLVStr(@pkt, T);                //Client's UIN in ASCII format

      //Start modification  
       if T = $008e then // TLV(142 [0x008E])
        begin
         Inc(pkt.Len, GetInt(@pkt, 0));
         FUIN := GetTLVStr(@pkt, T);
        end
       else
        if T = $0001 then // TLV(1 [0x0001])
          FUIN := GetWStr(@pkt);
      //end modification

      if T <> 1 then
      begin
        OnIntError(nil, ERR_PROTOCOL, ICQLanguages[FErrLang].Translate(IMSG_EMALFORMED_LOGIN_PKT));
        FTOnConnectError(Self);
        FSock.Disconnect;
        Exit;
      end;
      FData := GetTLVStr(@pkt, T);                //IP, Port to connect to
      if T = 4 then
      begin
        OnIntError(nil, ERR_LOGIN, ICQLanguages[FErrLang].Translate(IMSG_EBAD_PASS));
        FTOnConnectError(Self);
        Exit;
      end else
      if T = 8 then
      begin
        OnIntError(nil, ERR_LOGIN, ICQLanguages[FErrLang].Translate(IMSG_EOFTEN_LOGINS));
        FTOnConnectError(Self);
        Exit;
      end else
      if T <> 5 then
      begin
        OnIntError(nil, ERR_PROTOCOL, ICQLanguages[FErrLang].Translate(IMSG_EMALFORMED_LOGIN_PKT));
        FTOnConnectError(Self);
        FSock.Disconnect;
        Exit;
      end;
      FCookie := GetTLVStr(@pkt, T);              //Cookie used in second stage of login
      if T <> 6 then
      begin
        OnIntError(nil, ERR_PROTOCOL, ICQLanguages[FErrLang].Translate(IMSG_EMALFORMED_LOGIN_PKT));
        FTOnConnectError(Self);
        FSock.Disconnect;
        Exit;
      end;
      //Sending CLI_GOODBYE
      PktInit(@pkt, 4, FSeq);
      PktFinal(@pkt);
      FSock.SendData(pkt, pkt.Len);
      FSock.Disconnect;
      //Assigning new IP and Port to connect to in second attemp
      InitNetICQ;
      FSock.Host := Copy(FData, 0, Pos(':', FData) - 1);
      FSock.Port := StrToInt(Copy(FData, Pos(':', FData) + 1, Length(FData) - Pos(':', FData)));
      if (FSock.Port = 0) then
      begin
        OnIntError(nil, ERR_PROTOCOL, ICQLanguages[FErrLang].Translate(IMSG_EMALFORMED_LOGIN_PKT));
        FTOnConnectError(Self);
        Exit;
      end;
      FSock.Connect;
    end;
  end;
end;

{////////////////////////////////////////////////////////////////////////////////////////////////////}
procedure TICQClient.SetStatus(NewStatus: LongWord);
var
  pkt: TRawPkt;
begin
  if not LoggedIn then Exit;
  if StatusToInt(FStatus) = StatusToInt(NewStatus) then Exit;  // eraser 15.05.2004
  if (StatusToInt(FStatus) = S_INVISIBLE) and (StatusToInt(NewStatus) <> S_INVISIBLE) then
  begin
    CreateCLI_ADDINVISIBLE(@pkt, FInvisibleLst, FSeq);
    FSock.SendData(pkt, pkt.Len);
  end else
  if (StatusToInt(NewStatus) = S_INVISIBLE) and (StatusToInt(FStatus) <> S_INVISIBLE) then
  begin
    CreateCLI_ADDVISIBLE(@pkt, FVisibleLst, FSeq);
    FSock.SendData(pkt, pkt.Len);
  end;
  // eraser 15.05.2004
  if (StatusToInt(NewStatus) = S_AWAY) or (StatusToInt(NewStatus) = S_NA) then
  begin
    CreateCLI_SETIDLETIME(@pkt, True, FSeq);
    FSock.SendData(pkt, pkt.Len);
  end else
  if (StatusToInt(FStatus) = S_AWAY) or (StatusToInt(FStatus) = S_NA) then
  begin
    CreateCLI_SETIDLETIME(@pkt, False, FSeq);
    FSock.SendData(pkt, pkt.Len);
  end;
  //
  CreateCLI_SETSTATUS_SHORT(@pkt, NewStatus, FSeq);
  FSock.SendData(pkt, pkt.Len);
  FStatus := NewStatus;
end;

{$HINTS OFF}
{Handling packet with messages}
{SNAC(04,07) N E E D  C O M P L E T L Y  T O  R E B U I L D !!!}
procedure TICQClient.HSnac0407(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
var
  ITime, IRandomID: LongWord;
  ULen: Word;
  c, i: Word;
  TlvType:Word;
  ft_pkt, ack_pkt: TRawPkt;
  chunks: array[0..49] of Byte;
  CharsetNumber, CharsetSubset: Word;
  Msg, UIN: String;
  Status: LongWord;
  MsgType: Word;
  MsgFlag: Byte;
  Answer: LongWord;
  aPort: Word;
  aIP: DWord;
  extIP: DWORD;
  wAck: Word;
  Request: Boolean;
//  dwOnlineSince: DWORD;
  Desc, URL: String;
  v: Byte;
  atype: String;
  XML: String;
  XMLTime, XMLSource, XMLSender, XMLText: String;
  FName, FDesc: String;
  FSize: LongWord;
  FFSeq, FFSeq2: Word;
  Rec: TFTRequestRec;
  TCMD: String;
  List: TStringList;
  PSender, PEmail, PSenderIP: string;
begin
  Request := False;
  ITime := GetInt(Pkt, 4);                      //Time(system up since):  time(NULL) - ITime div 1000
                                                //'System up since: ' + DateTimeToStr(DateTimeToUnix(Now) - ITime div 1000);
                                                //Seen by ICQ2003x

  IRandomID := GetInt(Pkt, 2);                  //RandomID
  Inc(Pkt^.Len, 2);                             //Unknown: empty
  Msg := '';
  {Subtypes}
  case GetInt(Pkt, 2) of
    1:                                          //Simply(old-type) message
    begin
      UIN := GetStr(Pkt, GetInt(Pkt, 1));
      Inc(Pkt^.Len, 2);
      c := GetInt(Pkt, 2);                      //A count of the number of following TLVs.
      for i := 0 to c - 1 do                    //Skip all TLVs
      begin
        Inc(Pkt^.Len, 2);
        Inc(Pkt^.Len, GetInt(Pkt, 2));
      end;
      if GetInt(Pkt, 2) = 2 then                //TLV with message remain
      begin
        Inc(Pkt^.Len, 4);                       //TLV length + Unknown const
        Inc(Pkt^.Len, GetInt(Pkt, 2));          //Counts of following bytes + following bytes
        Inc(Pkt^.Len, 2);                       //x0101, Unknown, constant
        ULen := GetInt(Pkt, 2) - 4;             //Length of the message + 4

        // Support for other charsets by Yegor
        CharsetNumber:=GetInt (Pkt, 2);         //The encoding used for the message.
                                                //0x0000: US-ASCII
                                                //0x0002: UCS-2BE (or UTF-16?)
                                                //0x0003: local 8bit encoding, eg iso-8859-1, cp-1257, cp-1251.
                                                //Beware that UCS-2BE will contain zero-bytes for characters in the US-ASCII range.
        CharsetSubset:=GetInt (Pkt, 2);         //Unknown; seen: 0x0000 = 0, 0xffff = -1.

//        Inc(Pkt^.Len, 4);                       //Unknown seems to be constant

        Msg := GetStr(Pkt, ULen);               //The actual message text. There will be no ending NULL.
        if not ((CharsetNumber=0) or (CharsetNumber=3))
           then Msg:=UTF8ToStrSmart (Msg);
        if FDoPlain
           then Msg:=RTF2Plain (Msg);
        if (Length(Msg) > 0) and Assigned(OnMessageRecv) then
          FOnMsg(Self, Msg, UIN);
      end;
    end;
    2:                                          //Advanced(new-type)
    begin
      UIN := GetStr(Pkt, GetInt(Pkt, 1));
      Inc(Pkt.Len, 4);
      for c := 0 to 5 do
      begin
        if GetInt(Pkt, 2) = 5 then
        begin
          Inc(Pkt^.Len, 2);
          if GetInt(Pkt, 2) <> 0 then           //ACKTYPE: 0x0000 - This is a normal message
            Exit;
          Inc(Pkt^.Len, 16);                    //File signature
          Inc(Pkt^.Len, 8);                     //TIME + RANDOM
          for i := 0 to 5 do
          begin
            TlvType := GetInt(Pkt, 2);

            if TlvType = $2711 then      //Searching for TLV(2711) (with sources)
            begin
              // * eraser 21.04.2004
              // todo: AdvancedOnlineInfo event: aIP, aPort, ITime, Online Since
              Inc(Pkt^.Len, 2);                 //TLV Length
              Move(Ptr(LongWord(Pkt) + Pkt^.Len)^, chunks, 47);
              if GetInt(Pkt, 1) <> $1B then     //If this value is not present, this is not a message packet. Also, ICQ2001b does not send an ACK, SNAC(4,B), if this is not 0x1B.
                Exit;
              Inc(Pkt^.Len, 26);
              FFSeq := GetInt(Pkt, 2);
              Inc(Pkt^.Len, 16);
              // * eraser 21.04.2004
              MsgType := GetInt(Pkt, 1);        //message type (e.g. ea = AWAY)
              MsgFlag := GetInt(Pkt, 1);        //message flags (e.g. 03 = MFLAG_AUTO)
              Answer := GetInt(Pkt, 4);         //Read away-msg: seen 00 00 00 00
                                                //Send away-msg: seen zero and non zero

              //Away-Msgs respond from ICQ Lite
              if ((MsgType and $E0) = $E0) and (MsgFlag = MFLAG_AUTO) then
              begin
                if Request = False then begin
                  Msg := GetLNTS(Pkt);          //message string (null-terminated)
                  if Assigned(OnAutoMsgResponse) then
                    FOnAutoMsgResponse(Self, UIN, IRandomID, MsgType, Msg);
                 // Exit;
                end;
                (*
                  else (todo) who is reading my away-msg event
                *)
              end else
              //
              if MsgType = M_FILE then          //File request
              begin
                FDesc := GetLNTS(Pkt);          //File description
                Inc(Pkt^.Len, 4);               //Unknown: 00 00 00 00
                FName := GetLNTS(Pkt);          //File name
                FSize := GetLInt(Pkt, 4);       //File size

                {Set the records' items}
                Rec.ITime := ITime;
                Rec.IRandomID := IRandomID;
                Rec.UIN := StrToInt64(UIN);
                Rec.FileSize := FSize;
                Rec.Description := FDesc;
                Rec.FileName := FName;
                Rec.Seq := FFSeq;
                Rec.ReqType := $01;
                if Assigned(OnFTRequest) then
                  FOnFTRequest(Self, Rec);
                Exit;
              end else
              if MsgType = M_ADVANCED then      //Advanced message container
              begin
                GetLNTS(Pkt);                   //Empty message (contains only a null terminator)
                Inc(Pkt^.Len, 2);               //Following length
                Inc(Pkt^.Len, 16);              //Signature
                Inc(Pkt^.Len, 2);               //Unknown: empty
                TCMD := GetDWStr(Pkt);          //Text command
                If (Pos('File', TCmd) > 0) or (Pos(#$D4#$E0#$E9#$EB, TCmd) > 0) Then  {Fix for russian users from Yegor}          //Someone is sending us a file
                begin
                  Inc(Pkt^.Len, 19);
                  fDesc := GetDWStr(Pkt);
                  aPort := GetInt(Pkt, 2);
                  FFSeq2:= GetInt(Pkt, 2);
                  fName := GetWStr(Pkt);
                  fSize := GetInt(Pkt, 4);
                  Rec.Port := aPort;
                  Rec.ITime := ITime;
                  Rec.IRandomID := IRandomID;
                  Rec.UIN := StrToInt64(UIN);
                  Rec.FileSize := FSize;
                  Rec.Description := FDesc;
                  Rec.FileName := FName;
                  Rec.Seq := FFSeq;
                  // Send Ack through Server.
                  Rec.ReqType := $01; //
                  //NOTE From NighTrader:  This is not working correctly,
                  //I was doing this wrong, if you have any knowladge of
                  //how this works please ICQ me @ 30391169.
                  if Assigned(OnFTRequest) then
                    FOnFTRequest(Self, Rec);
                  Exit;
                end else
                if (Pos('Request For Contacts', TCMD) > 0) or (Pos(#$C7#$E0#$EF#$F0#$EE#$F1#$20#$CA#$EE#$ED#$F2#$E0#$EA#$F2#$EE#$E2, TCmd) > 0) then
                begin
                  Inc(Pkt^.Len, 15);            //15 unknown bytes
                  Inc(Pkt^.Len, 4);             //Following length
                  Msg := GetDWStr(Pkt);         //Message containing a reason
                  if Assigned(OnContactListRequest) then
                    FOnContactListReq(Self, UIN, Msg);
                end else
                if (Pos('Contacts',TCMD) > 0 ) or (Pos(#$CA#$EE#$ED#$F2#$E0#$EA#$F2#$FB, TCMD) > 0)  then
                begin
                  Inc(Pkt^.Len, 4);             //Following length
                  Msg := GetDWStr(Pkt);         //Message containing a list with contacts
                  List := TStringList.Create;   //Create temporary list
                  ParseContacts(Msg, List);     //Parse message with contacts
                  if Assigned(OnContactListRecv) then
                    FOnContactListRecv(Self, UIN, List);
                end else
                if (Pos('Web Page Address (URL)',TCMD) > 0) or (Pos(#$CF#$EE#$F1#$EB#$E0#$F2#$FC#$20#$D1#$F1#$FB#$EB#$EA#$F3#$20#$ED#$E0#$20#$E2#$E5#$E1#$2D#$F1#$F2#$F0#$E0#$ED#$E8#$F6#$F3,TCmd) > 0) then
                begin
                  // Handle URL Msg
                  Inc(Pkt^.Len, 19);
                  Msg :=  GetDWStr(Pkt);
                  if Pos(#$FE, Msg) <> -1 then begin  // Break apart strings
                    Desc := Copy(Msg, 1, Pos(#$FE, Msg) - 1);
                    URL  := Copy(Msg, Length(Desc) + 2, 255);
                    if Assigned(OnURLRecv) then
                      FOnURL(Self, Desc, URL, UIN);
                  end;
                end;
              end
              else
                Msg := GetLNTS(Pkt);            //The actual message text. There will be ending NULL.

              {Sending ACK of the message}
              PktInit(@ack_pkt, 2, FSeq);               //Channel 2
              PktSnac(@ack_pkt, $04, $0B, 0, 0);        //SNAC(x04/x0B)
              Move(Ptr(LongWord(Pkt) + TSNACSZ)^, Ptr(LongWord(@ack_pkt) + ack_pkt.Len)^, 10); //First 10 bytes of TLV(2711)
              Inc(ack_pkt.Len, 10);                     //Skip first 10 bytes copied from TLV(2711) which were added before
              PktLStr(@ack_pkt, UIN);                   //User's UIN
              PktInt(@ack_pkt, $0003, 2);               //00 03
              PktAddArrBuf(@ack_pkt, @chunks, 47);      //First 47 bytes of source packet (with message)
              PktInt(@ack_pkt, $00000000, 4);           //00 00 00 00
              //If it's an auto-away message request
              if (MsgType and $E0 = $E0) and Request then
                PktLNTS(@ack_pkt, FAutoAwayMsg)         //Auto-away message
              else begin
                PktInt(@ack_pkt, 1, 1);                 //01
                PktInt(@ack_pkt, 0, 4);                 //00 00 00 00
                PktInt(@ack_pkt, 0, 2);                 //00 00
                PktInt(@ack_pkt, $FFFFFF00, 4);         //FF FF FF 00
              end;
              PktFinal(@ack_pkt);
              FSock.SendData(ack_pkt, ack_pkt.Len);

              if (Length(Msg) > 0) then
              begin
                if MsgType = M_PLAIN then
                begin
                  if FDoPlain then Msg := Rtf2Plain(Msg);   //Convert message from RTF to plaintext when needed
                  if Assigned(OnMessageRecv) then
                    FOnMsg(Self, Msg, UIN);
                end else
                if MsgType = M_URL then
                begin
                  Desc := Copy(Msg, 0, Pos(#$fe, Msg) - 1);
                  URL := Copy(Msg, Pos(#$fe, Msg) + 1, Length(Msg) - Pos(#$fe, Msg));
                  if Assigned(OnURLRecv) then
                    FOnURL(Self, Desc, URL, UIN);
                end;
              end;
              Exit;
            end else
            if TlvType = 5 then         // TVL(5) Get a Port
            begin
              Inc(pkt^.Len, 2);         // Skip Length
              aPort := GetInt(Pkt, 2);  // Get Port Number
            end else
            // *eraser 21.04.2004
            {
            if TlvType = 6 then        // TLV(6) Status
            begin
              Inc(pkt^.Len, 4);
              Status := GetInt(Pkt, 4);
            end else
            }
            if TlvType = $0004 then     // TLV(4) Get an ExtIP
            begin
              Inc(pkt^.Len, 4);         // skip length
              extIP := GetInt(Pkt, 4);  // Get IP Number
            end else
            if TlvType = $000F then     // TLV(F)
            begin
              Inc(pkt^.Len, 2);         // skip length
              Request := True;          // possibly it is a request
            end else
            if TlvType = $000A then     // TLV(A)
            begin
              Inc(pkt^.Len, 2);         // 0x0002 skip length
              wAck := GetInt(Pkt, 2);   // seen 0001 - req (unk 0x000F 0x0000)
                                        // seen 0002 - reply (IP, Port)
            end else
            //
            if TlvType = 3 then         // TLV(3) Get an IntIP
            begin
              Inc(pkt^.Len,2);          // Skip Length
              aIP := GetInt(Pkt,4);     // Get IP Number
            end else
              Inc(Pkt^.Len, GetInt(Pkt, 2));
          end;
        end else
          Inc(Pkt^.Len, GetInt(Pkt, 2));
      end;
      // todo special user online event
    end;
    4:                                                  //Another message type
    begin
      UIN := GetLStr(Pkt);
      for i := 0 to 4 do
      begin
        v := GetInt(Pkt, 1);
        if (v = 5) or ((GetInt(Pkt, 1) = 5) and (v = 0)) then    //TLV(5) was found
        begin
          if v = 5 then                                 //Some modifications for MAC clients
            Inc(Pkt^.Len, 40)
          else
            Inc(Pkt^.Len, 2);
          GetLInt(Pkt, 4);                              //UIN
          MsgType := GetLInt(Pkt, 2);                   //Message-type
          Msg := GetLNTS(Pkt);                          //Message
          if MsgType = $1a then                         //Probably advanced msg format
          begin
            Inc(Pkt^.Len, 20);                          //20 unknown bytes
            atype := GetDWStr(Pkt);                     //Advanced msg sub-type
            if Pos ('ICQSMS', aType)<>0 then            //Corresponds to received SMS message in XML formatted message
            begin
              Inc(Pkt^.Len, 3);                         //00 00 00
              Inc(Pkt^.Len, 4);                         //4-byte little endian length of the following data
              XML := GetStr(Pkt, GetLInt(Pkt, 4));      //XML entry of SMS response
              XMLSource := GetXMLEntry('source', XML);  //Source, usually: 'ICQ'
              XMLSender := GetXMLEntry('sender', XML);  //Source cellular number
              XMLText := GetXMLEntry('text', XML);      //Text of reply
              XMLTime := GetXMLEntry('time', XML);      //Time of sending reply
              if Assigned(OnSMSReply) then
                FOnSMSReply(Self, XMLSource, XMLSender, XMLTime, UTF8ToStrSmart(XMLText));
            end;
            Exit;
          end;

          if (Length(Msg) > 0) then
          begin
            if MsgType = M_PLAIN then
            begin
              if FDoPlain then Msg := Rtf2Plain(Msg);     //Convert message from RTF to plaintext when needed
              if Assigned(OnMessageRecv) then
                FOnMsg(Self, Msg, UIN);
            end
            else if MsgType = M_URL then
            begin
              Desc := Copy(Msg, 0, Pos(#$fe, Msg) - 1);
              URL := Copy(Msg, Pos(#$fe, Msg) + 1, Length(Msg) - Pos(#$fe, Msg));
              if Assigned(OnURLRecv) then
                FOnURL(Self, Desc, URL, UIN);
            end else
            if MsgType = M_WEB_PAGE then         // Updated by Saif.N * To Support Web Page Message
            begin
              PSender :=  Copy(Msg, 1, Pos(#$fe, Msg) - 1);
              Msg := Copy(Msg,Length(PSender)+4, Length(Msg));
              PEmail :=   Copy(Msg, 1 , Pos(#$fe, Msg)-1);
              Msg := Copy(Msg,Length(PEmail)+1, Length(Msg));
              PSenderIP:= Copy(Msg, Pos('IP:', Msg)+4, Pos(#$D, Msg)-Pos('IP:', Msg)-4);
              Msg := Copy(Msg, Pos(#$A,Msg) + 1, Length(Msg));
              if Assigned(OnWPagerRecv) then
                 FonWPager(Self, PSender, PEmail, PSenderIP, Msg);
            end;
          end;
          Exit;
        end else
          Inc(Pkt^.Len, GetInt(Pkt, 2));
      end;
    end;
  end;
end;
{$HINTS ON}

{Handling old type packets ICQ_FROMSRV}
procedure TICQClient.HSnac1503(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
var
  FMsgType: Word;
  lpkt: TRawPkt;
  FNick, FFirst, FLast, FEmail, FCity,
  FState, FPhone, FFax, FStreet, FCellular,
  FZip, FCountry, FCompany, FDepartment,
  FPosition, FOccupation, FHomePage,
  FLang1, FLang2, FLang3, FAbout: String;
  FTimeZone: Byte;
  FPublishEmail: Boolean;
  FAge, FYear: Word;
  FGender, FMonth, FDay: Byte;
  Msg, UIN, URL, Desc: String;
  List, List2: TStringList;
  C, i: Byte;
  WW: Word;
  FStatus: Word;
  cmd: Word;
  seq: Word;
  FSmsSource, FSmsDeliverable, FSmsNetwork, FMsgId: String;
  FAuthorize: Byte;
  Year, Month, Day, Hour, Minute: Integer;
  PSender, PEmail, PSenderIP: string;
begin
  if GetInt(Pkt, 2) = 1 then                      //TLV(1)
  begin
    Inc(Pkt^.Len, 8);
    case GetInt(Pkt, 2) of
      $4100:                                      //SRV_OFFLINEMSG
      begin
        Inc(Pkt^.Len, 2);                         //The sequence number this packet is a response to.
        UIN:= IntToStr (GetLInt(Pkt, 4));         //Source UIN
        Year:=GetLInt (Pkt, 2);
        Month:=GetLInt (Pkt, 1);
        Day:=GetLInt (Pkt, 1);
        Hour:=GetLInt (Pkt, 1);
        Minute:=GetLInt (Pkt, 1);
        FMsgType := GetLInt(Pkt, 2);              //The type of message sent, like URL message or the like.
        Msg := GetLNTS(Pkt);
        if FDoPlain then Msg := Rtf2Plain(Msg);     //Convert message from RTF to plaintext when needed
        if FMsgType = M_PLAIN then
        begin
          if Assigned(OnOfflineMsgRecv) then FOnOffMsg(Self, EncodeTime (Hour, Minute, 0, 0)+EncodeDate (Year, Month, Day), Msg, UIN);
        end else
        if FMsgType = M_URL then
        begin
          Desc := Copy(Msg, 0, Pos(#$fe, Msg) - 1);
          URL := Copy(Msg, Pos(#$fe, Msg) + 1, Length(Msg) - Pos(#$fe, Msg));
          if Assigned(OnOfflineURLRecv) then
            FOnOffURL(Self, EncodeTime (Hour, Minute, 0, 0)+EncodeDate (Year, Month, Day), Desc, URL, UIN);
        end
        else if FMsgType = M_WEB_PAGE then         // Updated by Saif.N * To Support Web Page Message
            begin
              PSender :=  Copy(Msg, 1, Pos(#$fe, Msg) - 1);
              Msg := Copy(Msg,Length(PSender)+4, Length(Msg));
              PEmail :=   Copy(Msg, 1 , Pos(#$fe, Msg)-1);
              Msg := Copy(Msg,Length(PEmail)+1, Length(Msg));
              PSenderIP:= Copy(Msg, Pos('IP:', Msg)+4, Pos(#$D, Msg)-Pos('IP:', Msg)-4);
              Msg := Copy(Msg, Pos(#$A,Msg) + 1, Length(Msg));
              if Assigned(OnWPagerRecv) then
                 FonWPager(Self, PSender, PEmail, PSenderIP, Msg);
            end;
      end;
      $4200: //All offline messages were sent, so we ACKING them
      begin
        FSeq2 := 2;
        CreateCLI_ACKOFFLINEMSGS(@lpkt, FLUIN, FSeq, FSeq2);
        FSock.SendData(lpkt, lpkt.Len);
      end;
      $da07: //SRV_META
      begin
        seq := GetLInt(Pkt, 2);
        cmd := GetInt(Pkt, 2);
        case cmd of
          $0100: //SRV_SMSREFUSED
          begin
            if Assigned(OnSMSRefused) then
              FOnSMSRefused(Self);
          end;
          $9600: //SRV_SMSACK
          begin
            if GetInt(Pkt, 1) <> $0a then Exit;
            Inc(Pkt^.Len, 12);
            Msg := GetStr(Pkt, GetLInt(Pkt, 2));
            FSmsSource := GetXMLEntry('source', Msg);
            FSmsDeliverable := GetXMLEntry('deliverable', Msg);
            FSmsNetwork := GetXMLEntry('network', Msg);
            FMsgId := GetXMLEntry('message_id', Msg);
            if Assigned(OnSMSAck) then
              FOnSMSAck(Self, FSmsSource, FSmsNetwork, FMsgId, FSmsDeliverable = 'Yes');
          end;
          $b400: //SRV_METAUNREG_BADPASS Channel: 2, Snac(0x15, 0x03) 2010/180
          begin
            case GetInt(Pkt, 1) of
              $0a:
              begin
                if Assigned(OnUnregisterOk) then
                  FOnUnregisterOk(Self);
                CreateCLI_GOODBYE(@lpkt, FSeq);     //Send CLI_GOODBYE, it forces server to disconnect us
                FSock.SendData(lpkt, lpkt.Len);
              end;
              $14: if Assigned(OnUnregisterBadPassword) then
                FOnUnregBadPass(Self);
            end;
          end;
          $c800: //SRV_METAGENERAL Channel: 2, SNAC(0x15,0x03) 2010/200
          begin
            FLastInfoUin := FInfoChain.Values[IntToStr(seq)];          
            if GetInt(Pkt, 1) <> $0a then Exit;
            FNick := GetLNTS(Pkt);
            FFirst := GetLNTS(Pkt);
            FLast := GetLNTS(Pkt);
            FEmail := GetLNTS(Pkt);
            FCity := GetLNTS(Pkt);
            FState := GetLNTS(Pkt);
            FPhone := GetLNTS(Pkt);
            FFax := GetLNTS(Pkt);
            FStreet := GetLNTS(Pkt);
            FCellular := GetLNTS(Pkt);
            FZip := GetLNTS(Pkt);
            FCountry := CountryToStr(GetLInt(Pkt, 2));
            FTimeZone := GetInt(Pkt, 1);
            if GetInt(Pkt, 1) = 1 then
              FPublishEmail := True
            else
              FPublishEmail := False;
            if Assigned(OnUserGeneralInfo) then
              FOnUserGeneralInfo(Self, FLastInfoUin, FNick, FFirst,
                FLast, FEmail, FCity, FState, FPhone,
                FFax, FStreet, FCellular, FZip, FCountry,
                FTimeZone, FPublishEmail
              );
          end;
          $d200: //SRV_METAWORK Channel: 2, SNAC(0x15,0x3) 2010/210
          begin
            FLastInfoUin := FInfoChain.Values[IntToStr(seq)];
            if GetInt(Pkt, 1) <> $0a then Exit;
            FCity := GetLNTS(Pkt);
            FState := GetLNTS(Pkt);
            FPhone := GetLNTS(Pkt);
            FFax := GetLNTS(Pkt);
            FStreet := GetLNTS(Pkt);
            FZip := GetLNTS(Pkt);
            FCountry := CountryToStr(GetLInt(Pkt, 2));
            FCompany := GetLNTS(Pkt);
            FDepartment := GetLNTS(Pkt);
            FPosition := GetLNTS(Pkt);
            FOccupation := OccupationToStr(GetLInt(Pkt, 2));
            FHomePage := GetLNTS(Pkt);
            if Assigned(OnUserWorkInfo) then
              FOnUserWorkInfo(Self, FLastInfoUin, FCity, FState, FPhone,
                FFax, FStreet, FZip, FCountry, FCompany, FDepartment, FPosition,
                FOccupation, FHomePage
              );
          end;
          $dc00: //SRV_METAMORE Channel: 2, SNAC(0x15,0x3) 2010/220
          begin
            FLastInfoUin := FInfoChain.Values[IntToStr(seq)];
            if GetInt(Pkt, 1) <> $0a then Exit;
            FAge := GetLInt(Pkt, 2);
            if Integer(FAge) < 0 then
              FAge := 0;
            FGender := GetInt(Pkt, 1);
            FHomePage := GetLNTS(Pkt);
            FYear := GetLInt(Pkt, 2);
            FMonth := GetInt(Pkt, 1);
            FDay := GetInt(Pkt, 1);
            FLang1 := LanguageToStr(GetInt(Pkt, 1));
            FLang2 := LanguageToStr(GetInt(Pkt, 1));
            FLang3 := LanguageToStr(GetInt(Pkt, 1));
            if Assigned(OnUserInfoMore) then
              FOnUserInfoMore(Self, FLastInfoUin, FAge, FGender, FHomePage,
                FYear, FMonth, FDay, FLang1, FLang2, FLang3
              );
          end;
          $e600: //Channel: 2, SNAC(0x15,0x3) 2010/230
          begin
            FLastInfoUin := FInfoChain.Values[IntToStr(seq)];
            if GetInt(Pkt, 1) <> $0a then Exit;
            FAbout := GetLNTS(Pkt);
            if Assigned(OnUserInfoAbout) then
              FOnUserInfoAbout(Self, FLastInfoUin, FAbout);
          end;
          $eb00: //Channel: 2, SNAC(21,3) 2010/235
          begin
            FLastInfoUin := FInfoChain.Values[IntToStr(seq)];
            if GetInt(Pkt, 1) <> $0a then Exit;
            c := GetInt(Pkt, 1);        //The number of email addresses to follow. May be zero. Each consist of the following parameters:
            List := TStringList.Create;
            if c > 0 then
              for i := 0 to c - 1 do
              begin
                GetInt(Pkt, 1); //Publish email address? 1 = yes, 0 = no.
                List.Add(GetLNTS(Pkt)); //The email address.
              end;
            if Assigned(OnUserInfoMoreEmails) then
              FOnUserInfoMoreEmails(Self, FLastInfoUin, List)
            else
              List.Free;
          end;
          $f000: //Channel: 2, SNAC(21,3) 2010/240
          begin
            FLastInfoUin := FInfoChain.Values[IntToStr(seq)];
            if GetInt(Pkt, 1) <> $0a then Exit;
            c := GetInt(Pkt, 1);
            List := TStringList.Create;
            if c > 0 then
              for i := 0 to c - 1 do
              begin
                WW := GetLInt(Pkt, 2);
                List.Add(InterestToStr(WW) + '=' + GetLNTS(Pkt))
              end;
            if Assigned(OnUserInfoInterests) then
              FOnUserInfoInterests(Self, FLastInfoUin, List)
            else
              List.Free;
          end;
          $a401, $ae01: //SRV_METAFOUND Channel: 2, SNAC(21,3) 2010/420 or Channel: 2, SNAC(21,3) 2010/430
          begin
            if GetInt(Pkt, 1) <> $0a then
            begin
              if Assigned(OnUserNotFound) then
                FOnUserNotFound(Self);
              Exit;
            end;
            Inc(Pkt^.Len, 2);                   //Length of the following data.
            UIN := IntToStr(GetLInt(Pkt, 4));   //The user's UIN.
            FNick := GetLNTS(Pkt);              //The user's nick name.
            FFirst := GetLNTS(Pkt);             //The user's first name.
            FLast := GetLNTS(Pkt);              //The user's last name.
            FEmail := GetLNTS(Pkt);             //The user's email address.
            FAuthorize := GetInt(Pkt, 1);       //Authorize: 1 = no, 0 = yes.
            FStatus := GetLInt(Pkt, 2);         //0 = Offline, 1 = Online, 2 = not Webaware.
            FGender := GetInt(Pkt, 1);          //The user's gender. 1 = female, 2 = male, 0 = not specified.
            FAge := GetInt(Pkt, 1);             //The user's age.
            if Assigned(OnUserFound) then
              FOnUserFound(Self, UIN, FNick, FFirst, FLast, FEmail, FStatus, FGender, FAge, cmd = $ae01, FAuthorize = $00);
          end;
          $6603:
          begin
            if GetInt(Pkt, 1) <> $0a then
            begin
              if Assigned(OnUserNotFound) then
                FOnUserNotFound(Self);
              Exit;
            end;
            UIN := IntToStr(GetLInt(Pkt, 4));   //The user's UIN.
            if Assigned(OnUserFound) then
              FOnUserFound(Self, UIN, '', '', '', '', 0, 0, 0, True, False);
          end;
          $fa00:
          begin
            FLastInfoUin := FInfoChain.Values[IntToStr(seq)];          
            if GetInt(Pkt, 1) <> $0a then Exit;
            List := TStringList.Create;
            List2 := TStringList.Create;
            c := GetInt(Pkt, 1);                             //The number of background items to follow. May be zero. Each background item consists of the following two parameters
            if c > 0 then
              for i := 0 to c - 1 do
              begin
                WW := GetLInt(Pkt, 2);                       //The group this background is in, according to a table.
                if WW >= 8191 then Exit;
                List.Add(PastToStr(WW) + '=' + GetLNTS(Pkt)) //A longer description of this background item.
              end;
            c := GetInt(Pkt, 1);                             //The number of affiliations to follow. May be zero. Each affiliation consists of the following parameters:
            if c > 0 then
              for i := 0 to c - 1 do
              begin
                WW := GetLInt(Pkt, 2);                       //The group this affiliation is in, according to a table.
                if WW >= 8191 then Exit;
                List2.Add(AffiliationToStr(WW) + '=' + GetLNTS(Pkt)) //A longer description of the affiliation.
              end;
            if Assigned(OnUserInfoBackground) then
              FOnUserInfoBackground(Self, FLastInfoUin, List, List2)
            else begin
              List.Free;
              List2.Free;
            end;
          end;
          $0401: //SRV_METAINFO Channel: 2, SNAC(21,3) 2010/260
          begin
            FLastSInfoUin := FSInfoChain.Values[IntToStr(seq)];
            if FSInfoChain.IndexOfName(IntToStr(seq)) >= 0 then
              FSInfoChain.Delete(FSInfoChain.IndexOfName(IntToStr(seq)));
            if GetInt(Pkt, 1) <> $0a then
            begin
              if Assigned(OnUserInfoShort) then
                FOnUserInfoShort(Self, FLastSInfoUIN, '', '', '', '', False, False);
              Exit;
            end else
            begin
              FNick := GetLNTS(Pkt);            //Nickname
              FFirst := GetLNTS(Pkt);           //Firstname
              FLast := GetLNTS(Pkt);            //Lastname
              FEmail := GetLNTS(Pkt);           //Email
              if Assigned(OnUserInfoShort) then
                FOnUserInfoShort(Self, FLastSInfoUIN, FNick, FFirst, FLast, FEmail, True, GetInt(Pkt, 1) <> $01);
            end;
          end;
          $aa00:
            if Assigned(OnInfoChanged) then
              FOnInfoChanged(Self, INFO_PASSWORD, GetInt(Pkt, 1) = $0a);
          $6400: //SRV_METAGENERALDONE Channel: 2, SNAC(21,3) 2010/100
            if Assigned(OnInfoChanged) then
              FOnInfoChanged(Self, INFO_GENERAL, GetInt(Pkt, 1) = $0a);
          $7800: //SRV_METAMOREDONE Channel: 2, SNAC(21,3) 2010/120
            if Assigned(OnInfoChanged) then
              FOnInfoChanged(Self, INFO_MORE, GetInt(Pkt, 1) = $0a);
          $8200: //SRV_METAABOUTDONE Channel: 2, SNAC(21,3) 2010/130
            if Assigned(OnInfoChanged) then
              FOnInfoChanged(Self, INFO_ABOUT, GetInt(Pkt, 1) = $0a);
          $a000: //SRV_AUTHSET Channel: 2, SNAC(21, 3) 2010/160
            if Assigned(OnAuthorizationChangedOk) then
              FOnAuthSet(Self);
        end;
      end;
    end;
  end;
end;

{Handling packet with status changes}
{$WARNINGS OFF}
procedure TICQClient.HSnac030B(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
  function NumToIp(Addr: LongWord): String;
  var
    inaddr: in_addr;
  begin
    inaddr.S_addr := Addr;
    Result := inet_ntoa(inaddr);
  end;
var
  c, i: Word;
  s, UIN: String;
  Status: LongWord;
  FIntIP, FExtIP: LongWord;
  FIntPort: Word;
  FConnFlag: Byte;
  FDconCookie: LongWord;
  ICQVersion, MirandaVersion: DWORD;
  TimeOnlineSince: TDateTime;
  wIdleTime: Word;
  FProtoVer: Word;
  UserCaps:Byte;

const
  //  Capibilities
  AIM_CAPS_ICQ            = #$09#$46#$13#$44#$4C#$7F#$11#$D1 +
                            #$82#$22#$44#$45#$53#$54#$00#$00;
  AIM_CAPS_ICQRTF         = #$97#$B1#$27#$51#$24#$3C#$43#$34 +
                            #$AD#$22#$D6#$AB#$F7#$3F#$14#$92;
  AIM_CAPS_ICQSERVERRELAY = #$09#$46#$13#$49#$4C#$7F#$11#$D1 +
                            #$82#$22#$44#$45#$53#$54#$00#$00;
  AIM_CAPS_2001           = #$2E#$7A#$64#$75#$FA#$DF#$4D#$C8 +
                            #$88#$6F#$EA#$35#$95#$FD#$B6#$DF;
  AIM_CAPS_2001a          = #$09#$46#$14#$49#$4c#$7f#$11#$D1 +
                            #$82#$22#$44#$55#$53#$54#$00#$00;
begin
  UserCaps := 0;
  wIdleTime := 0;
  UIN := GetStr(Pkt, GetInt(Pkt, 1));
  Inc(Pkt^.Len, 2);
  c := GetInt(Pkt, 2);
  if c < 1 then Exit;
  for i := 0 to c - 1 do
  begin
    case GetInt(Pkt, 2) of
    $0c:
    begin
      Inc(Pkt^.Len, 2);                 //TLV's Length
      FIntIP := GetLInt(Pkt, 4);        //Internal IP
      FIntPort := GetInt(Pkt, 4);       //Internal port
      FConnFlag := GetInt(Pkt, 1);      //Connection flag
      FProtoVer := GetInt(Pkt, 2);      //Protocol version
      FDconCookie := GetLInt(Pkt, 4);   //Direct connection cookie
      //*eraser 10.03.04
      Inc(Pkt^.Len, 8);                 //Skip unknown data
      ICQVersion := GetInt(Pkt, 4);     //time(NULL), ff ff ff ff - Miranda
      MirandaVersion := GetInt(Pkt, 4); //time(NULL), version information
      Inc(Pkt^.Len, 6);                 //Skip remaining data
    end;
    $0a:
    begin
      Inc(Pkt^.Len, 2);                 //TLV's Length
      FExtIP := GetLInt(Pkt, 4);        //External IP
      if (FConnFlag = $04) or (FConnFlag = $02) then
      begin
        if FDirect <> nil then
          FDirect.AddUser(StrToInt64(UIN), FDConCookie, FExtIP, FIntIP, FIntPort);
      end else if FDirect <> nil then
        OnIntError(nil, ERR_WARNING, ICQLanguages[FErrLang].Translate(IMSG_WDC_BAD_PROXY));
    end;
    $06:
    begin
      Inc(Pkt^.Len, 2);                 //TLV's Length
      Status := GetInt(Pkt, 4);         //Online status
    end;

    $03: {online since}                                   //* eraser 9.03.04
    begin
      Inc(Pkt^.Len, 2);                                   //TLV's Length
      TimeOnlineSince := UnixToDateTime(GetInt(Pkt, 4));  //TimeStamp
    end;

    $04: {idle time}                    //* eraser 14.05.04
    begin
      Inc(Pkt^.Len, 2);                 //TLV's Length
      wIdleTime := GetInt(Pkt, 2);      //Idle in minutes
    end;

    $0D: {User Capabilities}
    begin
      s := GetWStr(Pkt);
      If Pos(AIM_CAPS_ICQ, s) > 0 then
        UserCaps := UserCaps + CAPS_ICQ;
      If Pos(AIM_CAPS_ICQRTF, s) > 0 then
        UserCaps := UserCaps + CAPS_ICQRTF;
      If Pos(AIM_CAPS_ICQSERVERRELAY, s) > 0 then
        UserCaps := UserCaps + CAPS_ICQSERVERRELAY;
      If Pos(AIM_CAPS_2001, s) > 0 then
        UserCaps := UserCaps + CAPS_2001;
      If Pos(AIM_CAPS_2001a, s) > 0 then
        UserCaps := UserCaps + CAPS_2001a;
    end else
      Inc(Pkt^.Len, GetInt(Pkt, 2));
    end;
  end;
  if Assigned(OnStatusChange) then
    FOnStatusChange(Self, UIN, Status);
  if Assigned(OnOnlineInfo) then
    FOnOnlineInfo(Self, UIN, FIntPort, NumToIp(FIntIP), NumToIp(FExtIP),
      TimeOnlineSince, wIdleTime, ICQVersion, MirandaVersion, FProtoVer,
      UserCaps);
end;
{$WARNINGS ON}

{Handling AddedYou packet}
procedure TICQClient.HSnac131C(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
var
  T: Word;
  UIN: String;
begin
  Inc(Pkt^.Len, 2);
  GetTLVInt(Pkt, T);
  if T <> 1 then Exit;
  UIN := GetLStr(Pkt);
  if Assigned(OnAddedYou) then
    FOnAddedYou(Self, UIN);
end;

{Authorization request}
procedure TICQClient.HSnac1319(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
var
  FUin: String;
  FReason: String;
begin
  Inc(Pkt^.Len, 8);
  FUin := GetLStr(Pkt);
  FReason := GetStr(Pkt, GetInt(Pkt, 2));
  If Assigned(OnAuthRequest) Then Begin
    fOnAuthReq(Self, FUIN, UTF8ToStrSmart (FReason));
  End;
end;

{This packet contains your complete server side contact list.}
procedure TICQClient.HSnac1306(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
var
  UINList: TList;
  procedure ReadChunk;
  var
    Len: Word;
    FGroup: ShortString;
    CTag, CId, CType: Word;
    TLen: Word;
    TType: Word;
    FNick, FCellular: ShortString;
    lpEntry: PUINEntry;
    FAuthorized: Boolean;
  begin
    FGroup := GetWStr(Pkt);             //The name of the group.
    CTag := GetInt(Pkt, 2);             //This field seems to be a tag or marker associating different groups together into a larger group such as the Ignore List or 'General' contact list group, etc.
    CId := GetInt(Pkt, 2);              //This is a random number generated when the user is added to the contact list, or when the user is ignored.
    CType := GetInt(Pkt, 2);            //This field seems to indicate what type of group this is.
    Len := GetInt(Pkt, 2);              //The length in bytes of the following TLVs.
    FNick := '';
    FCellular:='';
    FAuthorized:=true;
    while Integer(Len) > 0 do
    begin
      TType := GetInt(Pkt, 2);          //TLV Type
      TLen := GetInt(Pkt, 2);           //TLV Len
      if TType = $0131 then
        FNick := UTF8ToStrSmart(GetStr(Pkt, TLen))
      else if TType=314 then
        FCellular:=UTF8ToStrSmart(GetStr(Pkt, TLen))
      else begin // Support for authorization by yegor
           if TType=102
              then FAuthorized:=false;
           Inc(Pkt^.Len, TLen);            //Skip this TLV
      end;
      Dec(len, TLen + 4);               //TLV length + 2 bytes type + 2 bytes length
    end;

    //Group header

   //UIN entry
    if (CType=BUDDY_NORMAL) or (CType=BUDDY_GROUP) or (CType=BUDDY_IGNORE) or (CType=BUDDY_INVISIBLE) or (CType=BUDDY_VISIBLE) then
    begin
      GetMem(lpEntry, SizeOf(lpEntry^));
      if CType=BUDDY_GROUP
         then begin
              lpEntry^.UIN:=CTag;
              lpEntry^.Nick:=UTF8ToStrSmart (FGroup);
         end
         else begin
              lpEntry^.UIN:=StrToIntDef (FGroup, 0); // Fixed by yegor thx to Jeka
              lpEntry^.Nick:=FNick;
         end;
      lpEntry^.CType:=CType;
      lpEntry^.CTag:=CId;
      lpEntry^.CGroupID:=CTag;
      lpEntry^.Cellular:=FCellular;
      lpEntry^.Authorized:=FAuthorized;
      UINList.Add(lpEntry);
    end;
  end;
var
  count, T: Word;
  i: Word;
begin
  GetTLVInt(Pkt, T); if T <> 6 then Exit;
  Inc(Pkt^.Len, 4);                     //02 00 02 00 - UNKNOWNs
  count := GetInt(Pkt, 2);              //Total count of following groups. This is the size of the server side contact list and should be saved and sent with CLI_CHECKROSTER.
  if count < 1 then Exit;
  UINList := TList.Create;
  for i := 0 to count - 1 do
    ReadChunk;

  if Assigned(OnServerListRecv) then
    FOnServerListRecv(Self, UINList)
  else
    DestroyUINList(UINList);
end;

{This packet contains ack to message you've sent.}
procedure TICQClient.HSnac040b(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
var
  RetCode: Word;
  RetAcc: Byte;
  RetMsg: String;
  MsgType: Byte;
  FUIN: String;
begin
  Inc(Pkt^.Len, 4);                     //msg-id cookie
  RetCode := GetInt(Pkt, 2);            //Random ID
  Inc(Pkt^.Len, 2);                     //Other data :)
  Inc(Pkt^.Len, 2);                     //message type (channel): 00 02

  FUIN := GetLStr(Pkt);                 //User's UIN
  Inc(Pkt^.Len, 2);                     //reason code: 00 03
  Inc(Pkt^.Len, 45);                    //Skip 45 bytes of packet
  MsgType := GetInt(Pkt, 1);            //Msg-type
  Inc(Pkt^.Len, 1);                     //Msg-flags
  RetAcc := GetInt(Pkt, 1);             //Accept type
  Inc(Pkt^.Len, 3);                     //Unknown

  if (RetAcc <> ACC_NORMAL) and (RetAcc <> ACC_NO_OCCUPIED) and
     (RetAcc <> ACC_NO_DND) and (RetAcc <> ACC_AWAY) and
     (RetAcc <> ACC_NA) and (RetAcc <> ACC_CONTACTLST) then
    Exit;

//  0xE8   MTYPE_AUTOAWAY     Auto away message
//  0xE9   MTYPE_AUTOBUSY     Auto occupied message
//  0xEA   MTYPE_AUTONA     Auto not available message
//  0xEB   MTYPE_AUTODND     Auto do not disturb message
//  0xEC   MTYPE_AUTOFFC     Auto free for chat message

  if MsgType and $E0 = $E0 then   // is any type of 0xE.
  begin
    RetMsg := GetLNTS(Pkt);
    if Assigned(OnAutoMsgResponse) then
      FOnAutoMsgResponse(Self, FUIN, RetCode, MsgType, RetMsg);
    Exit;
  end;

  if RetAcc <> ACC_NORMAL then
  begin
    RetMsg := GetLNTS(Pkt);
  end else
    RetMsg := '';

  if Assigned(OnAdvancedMsgAck) then
    FOnAdvMsgAck(Self, FUIN, RetCode, RetAcc, RetMsg);
end;

{This packet contains Another type of ack to message you've sent.}
procedure TICQClient.HSnac040C(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
var
  RetCode: Word;
  FUIN: String;
begin
  Inc(Pkt^.Len, 4);                     //Time
  RetCode := GetInt(Pkt, 4);            //Random ID
  Inc(Pkt^.Len, 2);                     //Msg-Type  = 2
  FUIN := GetLStr(Pkt);                 //User's UIN
  If Assigned(OnMsgAck) Then
    fOnMsgAck(Self, FUIN, RetCode);
end;

{This packet contains response with new UIN created.}
procedure TICQClient.HSnac1705(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
var
  UIN: String;
begin
  if GetInt(Pkt, 2) <> $01 then Exit;   //TLV(01)
  Inc(Pkt^.Len, 2);                     //TLV's length
  Inc(Pkt^.Len, 2);                     //The length of the following data in bytes.
  Inc(Pkt^.Len, 4);                     //Unknown: empty.
  Inc(Pkt^.Len, 4);                     //Unknown: 0x2d000300 = 754975488.
  Inc(Pkt^.Len, 4);                     //Your port number as the server sees it.
  Inc(Pkt^.Len, 4);                     //Your IP address as the server sees it.
  Inc(Pkt^.Len, 4);                     //Unknown: 0x4 = 4.
  Inc(Pkt^.Len, 4);                     //The same UNKNOWN2 as sent in CLI_REGISTERUSER.
  Inc(Pkt^.Len, 16);                    //16 empty bytes
  UIN := IntToStr(GetLInt(Pkt, 4));     //New UIN
  if Assigned(OnNewUINRegistered) then  //Call associated event
    FOnNewUINRegistered(Self, UIN);
end;

{This packet contains reponse to CLI_REQAUTH.}
procedure TICQClient.HSnac131b(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
var
  T: Word;
  UIN, Reason: String;
  Granted: Boolean;
begin
  Inc(Pkt^.Len, 2);                     //Unknown: 00 06
  GetTLVInt(Pkt, T);                    //Unknown: 00 01 00 02 00 02
  UIN := GetLStr(Pkt);                  //The UIN that granted authorization.
  Granted := GetInt(Pkt, 1) = $01;      //00 - Rejected, 01 - Granted.
  Reason := GetWStr(Pkt);               //Reason, can be null.
  if Assigned(OnAuthResponse) then
    FOnAuthResponse(Self, UIN, Granted, Reason);
end;

{This command is sent as what is perhaps an acknowledgement reply to at least CLI_ADDBUDDY and CLI_UPDATEGROUP.}
procedure TICQClient.HSnac130e(Flap: TFlapHdr; Snac: TSnacHdr; Pkt: PRawPkt);
var
  T: Word;
  ErrCode: Word;
begin
  Inc(Pkt^.Len, 2);                     //Unknown: 6. Guess: The length in bytes of the following data.
  GetTLVInt(Pkt, T);                    //Unknown.
  ErrCode := GetInt(Pkt, 2);            //ErrorCode
  if (ErrCode <> ERRSSL_AUTH) and (ErrCode <> ERRSSL_NOERROR) and (ErrCode <> ERRSSL_NOTFOUND) and
     (ErrCode <> ERRSSL_EXISTS) then
    ErrCode := ERRSSL_OTHER;
  if Assigned(OnSSLChangeResponse) then
    FOnChangeResponse(Self, ErrCode);
end;

{Handle packet with message sent directly}
procedure TICQClient.HDirectMsg(Sender: TObject; UIN: LongWord; Pak: PRawPkt; Len: LongWord);
var
  Msg: String;
  lpkt: TRawPkt;
  LSeq: Word;
  cmd, scmd: Word;
  S, Desc, URL: String;
  List: TStringList;
  FText: String;
  FileLen: LongWord;
  Port: Word;
  rec: TFTRequestRec;
begin
  if not DecryptPak(Ptr(LongWord(Pak) + 2), Pak^.Len - 2, 8) then Exit;
  Pak.Len := 2;
  if GetInt(Pak, 1) <> $02 then Exit;   //02 - PEER_MSG
  Inc(Pak^.Len, 4);                     //Packet checksum
  cmd := GetLInt(Pak, 2);               //Command
  Inc(Pak^.Len, 2);                     //Unknown: 0xe = 14.
  LSeq := GetLInt(Pak, 2);              //Sequence number.
  Inc(Pak^.Len, 12);                    //Unknown: 12 empty bytes
  scmd := GetLInt(Pak, 2);              //Sub command
  case cmd of
    $07ee:                              //2030 - normal message.
    begin
      if scmd = $0001 then              //Simple message
      begin
        Inc(Pak^.Len, 2);               //Unknown: empty.
        Inc(Pak^.Len, 2);               //Our status.
        Msg := GetLNTS(Pak);            //Finally the message.
        if FDoPlain
           then Msg:=RTF2Plain (Msg);
        if Assigned(OnMessageRecv) then
          FOnMsg(Self, Msg, IntToStr(UIN));
      end else
      if scmd and $03e0 = $03e0 then    //Read auto-away message
      begin
        CreatePEER_AUTOMSG_ACK(@lpkt, FAutoAwayMsg, scmd, LSeq);        //Send ACK with auto msg reponse
        FDirect.SendData(UIN, @lpkt);
        Exit;                           //Do not send another ACK
      end else
      if scmd = $001a then              //Advanced message format
      begin
        Inc(Pak^.Len, 27);              //Skip 27 bytes of mostly unknown data
        S := GetStr(Pak, GetLInt(Pak, 4));
        if S = 'Contacts' then          //Receive contacts
        begin
          Inc(Pak^.Len, 19);            //Skip another 19 bytes of empty data + some lengths
          S := GetStr(Pak, GetLInt(Pak, 4));
          List := TStringList.Create;
          ParseContacts(S, List);
          if Assigned(OnContactListRecv) then
            FOnContactListRecv(Self, IntToStr(UIN), List)
          else
            List.Free;
        end else
        if Pos('Web Page Address', s) > 0 then
        begin
          Inc(Pak^.Len, 19);            //Skip another 19 bytes of empty data + some lengths
          S := GetStr(Pak, GetLInt(Pak, 4));
          if Assigned(OnURLRecv) then
          begin
            Desc := Copy(S, 0, Pos(#$fe, S) - 1);
            URL := Copy(S, Pos(#$fe, S) + 1, Length(S) - Pos(#$fe, S));
            if Assigned(OnURLRecv) then
              FOnURL(Self, Desc, URL, IntToStr(Uin));
          end;
        end else
        if S = 'Request For Contacts' then
        begin
          Inc(Pak^.Len, 19);            //Skip another 19 bytes of empty data + some lengths
          S := GetStr(Pak, GetLInt(Pak, 4));
          if Assigned(OnContactListRequest) then
            FOnContactListReq(Self, IntToStr(UIN), S)
        end else
        if S = 'File' then
        begin
          Inc(Pak^.Len, 19);                    //Skip another 19 bytes of empty data + some lengths
          Desc := GetStr(Pak, GetLInt(Pak, 4)); //Description
          Port := GetInt(Pak, 2);               //Port
          Inc(Pak^.Len, 2);                     //Seq
          FileLen := GetLInt(Pak, 2);
          if FileLen > 0 then
          begin
            FText := GetStr(Pak, FileLen - 1);  //Filename
            Inc(Pak^.Len, 1);                   //Null terminator
          end else FText := '';
          FileLen := GetLInt(Pak, 4);           //Filelength
          rec.ReqType := 0;
          rec.UIN := UIN;
          rec.Description := Desc;
          rec.FileName := FText;
          rec.FileSize := FileLen;
          rec.Seq := LSeq;
          rec.Port := Port;
          if Assigned(OnFTRequest) then
            FOnFTRequest(Self, rec);
          Exit;
        end;
      end;
    end;
    $07da:                              //Packet acks
    begin
      // Check Len of Packet to see if reply to PEER_MSG_FILE
      If (Len > $3a) and ((Pak.Data[$3a]=$46) and (Pak.Data[$3b]=$69)) then Begin
        Pak.Len := $0a;
        LSeq := GetLint(Pak, 2);
        Pak.Len := $51;
        Desc := GetDWStr(Pak);
        Port := GetInt(Pak, 2);
        fDirect.AddSendFileUser(UIN, Port, LSeq);
      End;
      if Assigned(OnDirectPacketAck) then
        FOnDirectPktAck(Self, LSeq);
    end;
  end;
  //ACK received packet, if this packet isn't a "cancel given message" or "acknowledge message"
  if (cmd <> $07da) and (cmd <> $07d0) then
  begin
    if FDirect <> nil then
      begin
        CreatePEER_MSGACK(@lpkt, LSeq);
        FDirect.SendData(UIN, @lpkt);
      end;
  end;
end;

procedure TICQClient.FTOnConnectError(Sender: TObject);
begin
  FLoggedIn := False;
  FTimer.Enabled := False;
  if Assigned(OnConnectionFailed) then
    FOnConnectionFailed(Self);
end;

procedure TICQClient.FTOnDisconnect(Sender: TObject);
begin
  FTimer.Enabled := False;
  if FLoggedIn then begin
    FLoggedIn := False;
    if Assigned(OnConnectionFailed) then FOnConnectionFailed(Self);
  end;
end;

procedure TICQClient.FTOnDirectParse(Sender: TObject; Buffer: Pointer; BufLen: LongWord; Incoming: Boolean; UIN:Cardinal);
begin
  if Assigned(OnPktDirectParse) then
    FOnDPktParse(Self, Buffer, BufLen, Incoming, UIN);
end;

procedure TICQClient.FTOnPktParse(Sender: TObject; Buffer: Pointer; BufLen: LongWord; Incoming: Boolean);
begin
  if Assigned(OnPktParse) then
    FOnPktParse(Self, Buffer, BufLen, Incoming);
end;

procedure TICQClient.OnFTInitProc(Sender: TObject; UIN: LongWord; FileCount, TotalBytes, Speed: LongWord; NickName: String);
begin
  if Assigned(OnFTInit) then
    FOnFTInit(Self, UIN, FileCount, TotalBytes, Speed, NickName);
end;

procedure TICQClient.OnFTStartProc(Sender: TObject; StartRec: TFTStartRec; FileName: String; FileSize, Speed: LongWord);
begin
  if Assigned(OnFTStart) then
    FOnFTStart(Self, StartRec, FileName, FileSize, Speed);
end;

procedure TICQClient.OnFTFileDataProc(Sender: TObject; UIN: LongWord; Data: Pointer; DataLen: LongWord; LastPacket: Boolean);
begin
  if Assigned(OnFTFileData) then
    FOnFTFileData(Self, UIN, Data, DataLen, LastPacket);
end;

procedure TICQClient.OnSendFileStartProc(Sender:TObject; UIN: LongWord; SendFileRec:TSendFileRec);
Begin
  If Assigned(OnSendFileStart) Then
    fOnSendFileStart(Sender, UIN, SendFileRec);
End;

Procedure TICQClient.OnSendFileDataProc(Sender:TObject; UIN:LongWord; Data:Pointer;Var DataLen:LongWord; Var IsLastPacket:boolean);
Begin
  If Assigned(OnSendFileData) Then
    fOnSendFileData(Sender, Uin, Data, DataLen, IsLastPacket);
End;

Procedure TICQClient.OnSendFileFinishProc(Sender:TObject; UIN:LongWord; SendFileRec:TSendFileRec; Aborted:Boolean);
Begin

  If Assigned(OnSendFileFinish) then
    fOnSendFileFinish(Sender, UIN, SendFileRec, Aborted);
End;

procedure TICQClient.SetContactList(Value: TStrings);
begin
  FContactLst.Assign(Value);
end;

procedure TICQClient.SetVisibleList(Value: TStrings);
begin
  FVisibleLst.Assign(Value);
end;

procedure TICQClient.SetInvisibleList(Value: TStrings);
begin
  FInvisibleLst.Assign(Value);
end;

procedure TICQClient.OnTimeout;
begin
  FTimer.Enabled := False;
  FSock.Disconnect;
  OnIntError(Self, ERR_CONNTIMEOUT, ICQLanguages[FErrLang].Translate(IMSG_ECON_TIMEDOUT));
  if Assigned(OnConnectionFailed) then
    FOnConnectionFailed(Self);
end;

procedure TICQClient.SetErrorLang(NewLang: TICQLangType);
begin
  FErrLang := NewLang;
  FSock.ErrorLanguage := NewLang;
end;

procedure TICQClient.SetPortLast(aPort:Word);
Begin
  gPortRange.Last := aPort;
End;

procedure TICQClient.SetPortFirst(aPort:word);
Begin
  gPortRange.First := aPort;
End;

function TICQClient.GetPortLast:Word;
Begin
  Result := gPortRange.Last;
End;

function TICQClient.GetPortFirst:Word;
Begin
  Result := gPortRange.First;
End;

{**************************************************************************}
constructor TMyTimer.Create;
begin
  inherited Create;
  FEnabled := True;
  FInterval := 1000;
  {$IFDEF OLD_DELPHI}
  FWindowHandle := AllocateHWnd(WndProc);
  {$ELSE}
  FWindowHandle := Classes.AllocateHWnd(WndProc); {Remove 'depricated' warning}
  {$ENDIF}
end;

destructor TMyTimer.Destroy;
begin
  SetEnabled(False);
  {$IFDEF OLD_DELPHI}
  DeallocateHWnd(FWindowHandle);
  {$ELSE}
  Classes.DeallocateHWnd(FWindowHandle); {Remove 'depricated' warning}
  {$ENDIF}
  inherited;
end;

procedure TMyTimer.WndProc(var Msg: TMessage);
begin
  with Msg do
    if Msg = WM_TIMER then
      try
        Timer;
      except
      end
    else
      Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
end;

procedure TMyTimer.UpdateTimer;
begin
  KillTimer(FWindowHandle, 1);
  if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
    SetTimer(FWindowHandle, 1, FInterval, nil);
end;

procedure TMyTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

procedure TMyTimer.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

procedure TMyTimer.SetOnTimer(Value: TNotifyEvent);
begin
  FOnTimer := Value;
  UpdateTimer;
end;

procedure TMyTimer.Timer;
begin
  if Assigned(FOnTimer) then FOnTimer(Self);
end;

{*********************************************************************}

procedure Register;
begin
  RegisterComponents('Samples', [TICQClient]);
end;



end.



