unit ICQWorks {TICQClient, TICQDb constants & procedures};
{(C) Alex Demchenko(alex@ritlabs.com)}

{$R-}                           //Remove range checking
{$DEFINE USE_ASM}               //Remove this line to use pascal code instead of assembler (where it's possible)

interface
uses
  Windows, Classes;

const
  MAX_DATA_LEN = 8192;          //Maximum packet size


  //Online statuses
  S_ONLINE      = $00000000;    //Online
  S_INVISIBLE   = $00000100;    //Invisible
  S_AWAY        = $00000001;    //Away
  S_NA          = $00000005;    //N/A
  L_S_NA        = $00000004;    // added by eraser 8.7.03
  S_OCCUPIED    = $00000011;    //Occupied
  L_S_OCCUPIED  = $00000010;    // added by eraser 8.7.03
  S_DND         = $00000013;    //Do Not Disturb
  L_S_DND       = $00000012;    // added by eraser 8.7.03
  S_FFC         = $00000020;    //Free For Chat
  S_OFFLINE     = $FFFFFFFF;    //The user is offline. / Set status to offline

  SF_BIRTH      = $00080000;    //Birtday

  //Flags used with statuses
  S_SHOWIP      = $00020000;    //show ip (for older clients), IP edit removed in ICQ2000a+ :)
  S_WEBAWARE    = $00030000;    //do not show status from the www
  S_ALLOWDCONN  = $00000000;    //allow direct connection with everyone
  S_ALLOWDAUTH  = $10000000;    //allow direct connection upon authorization
  S_ALLOWDLIST  = $20000000;    //allow direct connection with users in contact list

  //Message types
  M_PLAIN         = $01;        //Plain message
  M_CHAT          = $02;        //Chat request
  M_FILE          = $03;        //File request
  M_URL           = $04;        //URL
  M_AUTH_REQ      = $06;        //Auth request
  M_AUTH_DENIED   = $07;        //Deny auth
  M_AUTH_GIVEN    = $08;        //Authorize
  M_WEB_PAGE      = $0D;
  M_EMAIL_EXPRESS = $0E;
  M_CONTACTS      = $13;
  M_ADVANCED      = $1A;        //Seems to be an advanced-msg type (contacts, file requests, etc)

  //Genders
  GEN_FEMALE    = 1;            //Gender: Female
  GEN_MALE      = 2;            //Gender: Male

  //CLI_TOICQSRV commands
  CMD_ACKOFFMSG = $3E;
  CMD_REQOFFMSG = $3C;
  CMD_REQINFO   = $7D0;

  TFLAPSZ: Word = 6;            //Size of FLAP header
  TSNACSZ: Word = 10;           //Size of SNAC header

  //SRV UIN Flags
  U_NORMAL         = $0000;     //Normal list entry
  U_VISIBLE_LIST   = $0002;     //User in visible list
  U_INVISIBLE_LIST = $0003;     //User in invisible list
  U_IGNORE_LIST    = $000e;     //User in ignore list


  ACC_NORMAL      = $0;         //Normally accepted
  ACC_NO_OCCUPIED = $9;         //Not accepted, occupied
  ACC_NO_DND      = $A;         //Not accepted, dnd
  ACC_AWAY        = $4;         //Accepted but away
  ACC_NA          = $E;         //Accepted but NA
  ACC_CONTACTLST  = $C;         //Accepted to contact list (no blink in tray)

  //Auto message requests
  GET_AWAY        = $E8;        //Get away message
  GET_OCCUPIED    = $E9;        //Get occupied message
  GET_NA          = $EA;        //Get N/A message
  GET_DND         = $EB;        //Get DND message
  GET_FFC         = $EC;        //Get FFC(Free For Chat) message

  //Message flags
  MFLAG_NORMAL    = $01;        //Normal message
  MFLAG_AUTO      = $03;        //Auto-message flag
  MFLG_MULTI      = $80;        //This is multiple recipients message

  //Buddy types
  BUDDY_NORMAL    = $0000;      //A normal contact list entry
  BUDDY_GROUP     = $0001;      //A larger group header
  BUDDY_IGNORE    = $000e;      //A contact on the ignore list
  BUDDY_INVISIBLE = $0003;      //A contact on the invisible list
  BUDDY_VISIBLE   = $0002;      //A contact on the visible list

  //SSL errors
  ERRSSL_NOTFOUND = $0002;      //User not found
  ERRSSL_EXISTS   = $0003;      //Added buddy already exists
  ERRSSL_AUTH     = $000e;      //User not authorized
  ERRSSL_OTHER    = $000a;      //Other SSL error
  ERRSSL_NOERROR  = $0000;      //No error (changed successfully)

  // Client Capabilities
  CAPS_ICQ            = $01;
  CAPS_ICQRTF         = $02;
  CAPS_ICQSERVERRELAY = $04;
  CAPS_2001           = $08;
  CAPS_2001a          = $10;

  ICQClientVer_Major = 1;       //Major version of component
  ICQClientVer_Minor = 21;      //Minor version of component
  ICQClientVer_Build = '';

  // Internal Constants
  ICQ_PROTOCOL_VER = $0008;
type
  //File request record
  TFTRequestRec = record
    ReqType: Byte;
    ITime, IRandomID: LongWord;
    UIN: LongWord;
    Description: String;
    FileName: String;
    FileSize: LongWord;
    Seq: Word;
    Port: Word;
  end;

  //File start record
  TFTStartRec = record
    UIN: LongWord;
    FilesCount: LongWord;
    Current: Integer;
    Speed: LongWord;
  end;
  // File Send DC Record
  TSendFileRec = Record
    UIN:LongWord;
    Nick:String;
    Seq:Word;
    Files:TStrings;
    FilesCurrent:Integer;
    FilesCount:LongWord;
    FilePath:String;
    FileName:String;
    FileDescription:String;
    FileSize:LongWord;
    TotalSize:LongWord;
    Port:Word;
    Speed:Byte;
  End;
  pSendFileRec = ^TSendFileRec;

  // Port Range to support firewalls
  TPortRange = Record
    First:Word;
    Last: Word;
  End;

//Error types
  TErrorType = (ERR_SOCKET, ERR_INTERNAL, ERR_WARNING, ERR_PROXY, ERR_PROTOCOL, ERR_CONNTIMEOUT, ERR_LOGIN);

  //Proxy types
  TProxyType = (P_NONE, P_SOCKS4, P_SOCKS5, P_HTTPS, P_HTTP);

  //Info types
  TInfoType = (INFO_GENERAL, INFO_MORE, INFO_ABOUT, INFO_PASSWORD);

  //Database types, used in ICQDb.pas
  TDbType = (DB_ICQ, DB_MIRANDA);

  //Flap header
  PFlapHdr = ^TFlapHdr;
  TFlapHdr = record
    Ident: Byte;
    ChID: Byte;
    Seq: Word;
    DataLen: Word;
  end;

  //Snac header
  PSnacHdr = ^TSnacHdr;
  TSnacHdr = record
    Family: Word;
    SubType: Word;
    Flags: Word;
    ReqID: LongWord;
  end;

  //Raw packet
  PRawPkt = ^TRawPkt;
  TRawPkt = record
    Data: array[0..MAX_DATA_LEN - 1] of Byte;
    Len: Word;
  end;

  TThreadTimerTrigger = class(TThread)
  protected
    fInterval:LongInt;
    fOnTrigger:TNotifyEvent;
    Procedure Trigger;
    procedure Execute; override;
  Public
    Owner:TObject;

    Property Interval:LongInt read fInterval write fInterval;
    Property OnTrigger:TNotifyEvent read fOnTrigger write fOnTrigger;
  end;

  TTHreadTimer = Class(TObject)
  private
    fActive:Boolean;
    fInterval:LongInt;
    fOnTimer:TNotifyEvent;
    fTrigger:TThreadTimerTrigger;

    Procedure SetActive(Const IsActive:Boolean);
    Procedure SetInterval(Const aInterval:LongInt);
    Procedure SetOnTimer(Const aOnTimer:TNotifyEvent);
  public
    constructor Create;
    destructor Destroy; override;

    Property Enabled:Boolean read fActive write SetActive;
    Property Interval:LongInt read fInterval write SetInterval;
    Property OnTimer:TNotifyEvent read fOnTimer write SetOnTimer;
  end;

  //Filetransfer init call back function
  TOnFTInit = procedure(Sender: TObject; UIN: LongWord; FileCount, TotalBytes, Speed: LongWord; NickName: String) of object;
  TOnFTStart = procedure(Sender: TObject; StartRec: TFTStartRec; FileName: String; FileSize, Speed: LongWord) of object;
  TOnFTFileData = procedure(Sender: TObject; UIN: LongWord; Data: Pointer; DataLen: LongWord; LastPacket: Boolean) of object;

  TOnSendFileStart = Procedure(Sender: TObject; UIN: LongWord; SendFileRec: TSendFileRec) of Object;
  TOnSendFileData  = Procedure (Sender: TObject; UIN:LongWord; Data: Pointer;Var DataLen: LongWord; Var IsLastPacket: Boolean) of Object;
  TOnSendFileFinish= Procedure (Sender: TObject; UIN: LongWord; SendFileRec: TSendFileRec; Aborted:Boolean) of Object;

function IntToStr(Value: Int64): String;
function StrToInt(const Value: String): LongWord;
function IntToHex(Int: Int64; IntSize: Byte): String;
function HexToInt(Value: String): LongWord;
procedure ICQEncryptPass(SrcBuf: Pointer; BufLen: LongWord);
procedure ICQEncryptPassStr(var Pass: String);
function Swap16(Value: Word): Word;
function Swap32(Value: LongWord): LongWord;

{Low-level functions}
{-- Adding data --}
procedure PktAddData(Pkt: PRawPkt; Data: Pointer; DataLen: LongWord);
procedure PktAddArrBuf(Pkt: PRawPkt; Data: Pointer; DataLen: LongWord);
procedure PktInit(Pkt: PRawPkt; Channel: Byte; var Seq: Word);
procedure PktInitRaw(Pkt: PRawPkt);
procedure PktFinal(Pkt: PRawPkt);
procedure PktSnac(Pkt: PRawPkt; Family, SubType: Word; ID: LongWord; Flags: Word);
procedure PktInt(Pkt: PRawPkt; Value: LongWord; IntSize: Byte);
procedure PktLInt(Pkt: PRawPkt; Value: LongWord; IntSize: Byte);
procedure PktStr(Pkt: PRawPkt; const S: String);
procedure PktLStr(Pkt: PRawPkt; const S: String); overload;
procedure PktLStr(Pkt: PRawPkt; S: LongWord); overload;
procedure PktWStr(Pkt: PRawPkt; const S: String);
procedure PktDWStr(Pkt: PRawPkt; const S: String);
procedure PktLNTS(Pkt: PRawPkt; const S: String);
procedure PktLLNTS(Pkt: PRawPkt; const S: String);
procedure PktTLV(Pkt: PRawPkt; T, L: Word; V: LongWord); overload;
procedure PktTLV(Pkt: PRawPkt; T: Word; const V: String); overload;
procedure PktTLV(Pkt: PRawPkt; T, L: Word; V: Pointer); overload;

{-- Extracting data --}
function GetInt(Pkt: PRawPkt; IntSize: Byte): LongWord;
function GetLInt(Pkt: PRawPkt; IntSize: Byte): LongWord;
function GetStr(Pkt: PRawPkt; StrLen: Integer): String;
function GetTLVStr(Pkt: PRawPkt; var T: Word): String;
function GetTLVInt(Pkt: PRawPkt; var T: Word): LongWord;
procedure GetSnac(Pkt: PRawPkt; var Snac: TSnacHdr);
function GetLStr(Pkt: PRawPkt): String;
function GetWStr(Pkt: PRawPkt): String;
function GetDWStr(Pkt: PRawPkt): String;
function GetLNTS(Pkt: PRawPkt): String;


{High-level functions}
function StrToLanguageI(const Value: String): Word;
function StrToCountryI(const Value: String): Word;
function StrToInterestI(const Value: String): Word;
function StrToOccupationI(const Value: String): Word;
function StrToPastI(const Value: String): Word;
function StrToOrganizationI(const Value: String): Word;
procedure ParseContacts(Value: String; var List: TStringList);
function MakeContactsStr(Contacts: TStringList): String;

{Packet creation functions}
procedure CreateCLI_IDENT(Pkt: PRawPkt; UIN: LongWord; Password: String; var Seq: Word);
procedure CreateCLI_COOKIE(Pkt: PRawPkt; const Cookie: String; var Seq: Word);
procedure CreateCLI_FAMILIES(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_RATESREQUEST(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_ACKRATES(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_REQINFO(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_REQUNKNOWN(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_REQROSTER(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_CHECKROSTER(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_REQLOCATION(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_REQBUDDY(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_REQICBM(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_SETSTATUS(Pkt: PRawPkt; Status: LongWord; IP: LongInt; Port: Word; Cookie: LongWord; ProxyType: TProxyType; var Seq: Word);
procedure CreateCLI_SETSTATUS_SHORT(Pkt: PRawPkt; Status: LongWord; var Seq: Word);
procedure CreateCLI_REQBOS(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_SETUSERINFO(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_SETICBM(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_SETIDLETIME(Pkt: PRawPkt; const IsIdle: Boolean; var Seq: Word);
procedure CreateCLI_READY(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_TOICQSRV(Pkt: PRawPkt; UIN: LongWord; Command: Word; Data: Pointer; DataLen: LongWord; var Seq, Seq2: Word);
procedure CreateCLI_ADDCONTACT(Pkt: PRawPkt; UIN: String; var Seq: Word);
procedure CreateCLI_ADDCONTACT_MULTI(Pkt: PRawPkt; UINs: array of LongWord; var Seq: Word);
procedure CreateCLI_REMOVECONTACT(Pkt: PRawPkt; UIN: LongWord; var Seq: Word);
procedure CreateCLI_ADDVISIBLE(Pkt: PRawPkt; UINs: TStrings; var Seq: Word);
procedure CreateCLI_REMVISIBLE(Pkt: PRawPkt; UIN: LongWord; var Seq: Word);
procedure CreateCLI_ADDINVISIBLE(Pkt: PRawPkt; UINs: TStrings; var Seq: Word);
procedure CreateCLI_REMINVISIBLE(Pkt: PRawPkt; UIN: LongWord; var Seq: Word);
procedure CreateCLI_ACKOFFLINEMSGS(Pkt: PRawPkt; UIN: LongWord; var Seq, Seq2: Word);
procedure CreateCLI_SENDMSG(Pkt: PRawPkt; ITime, IRandom, UIN: LongWord; const Msg: String; var Seq: Word);
procedure CreateCLI_SENDURL(Pkt: PRawPkt; ITime, IRandom, MyUIN, UIN: LongWord; const URL, Description: String; var Seq: Word);
procedure CreateCLI_AUTHORIZE(Pkt: PRawPkt; UIN: LongWord; Auth: Byte; Reason: String; var Seq: Word);
procedure CreateCLI_METAREQINFO(Pkt: PRawPkt; UIN, dUIN: LongWord; var Seq, Seq2: Word);
procedure CreateCLI_SEARCHBYMAIL(Pkt: PRawPkt; UIN: LongWord; const Email: String; var Seq, Seq2: Word);
procedure CreateCLI_SEARCHBYUIN(Pkt: PRawPkt; UIN: LongWord; DUIN: LongWord; var Seq, Seq2: Word);
procedure CreateCLI_SEARCHBYNAME(Pkt: PRawPkt; UIN: LongWord; const FirstName, LastName, NickName, Email: String; var Seq, Seq2: Word);
procedure CreateCLI_SEARCHRANDOM(Pkt: PRawPkt; UIN: LongWord; Group: Word; var Seq, Seq2: Word);
procedure CreateCLI_SEARCHWP(Pkt: PRawPkt; UIN: LongWord; First, Last, Nick, Email: String; MinAge, MaxAge: Word; Gender: Byte; Language: Byte; City: String; Country: Word; Company, Department, Position: String; Occupation: Byte; Organization: Word; OrganKeyWords: String; Affiliation: Word; AffiKeyWords, KeyWord: String; Online: Byte; var Seq, Seq2: Word);
procedure CreateCLI_METASETMORE(Pkt: PRawPkt; UIN: LongWord; Age: Word; Gender: Byte; HomePage: String; BirthYear: Word; BirthMonth, BirthDay, Lang1, Lang2, Lang3: Byte; var Seq, Seq2: Word);
procedure CreateCLI_METASETGENERAL(Pkt: PRawPkt; UIN: LongWord; const NickName, FirstName, LastName, Email, City, State, Phone, Fax, Street, Cellular, Zip: String; Country: Word; TimeZone: Byte; PublishEmail: Boolean; var Seq, Seq2: Word);
procedure CreateCLI_METASETABOUT(Pkt: PRawPkt; UIN: LongWord; const About: String; var Seq, Seq2: Word);
procedure CreateCLI_SENDSMS(Pkt: PRawPkt; UIN: LongWord; const Destination, Text: String; CodePage: Word; const Time: String; var Seq, Seq2: Word);
procedure CreateCLI_SENDMSG_ADVANCED(Pkt: PRawPkt; Status: LongWord; ITime, IRandom, UIN: LongWord; const Msg: String; RTFFormat: Boolean; var Seq: Word);
procedure CreateCLI_SENDMSG_FILEACK(Pkt: PRawPkt; Status: LongWord; FFSeq: Word; ITime, IRandom, UIN, FileSize: LongWord; const FileDesc, FileName: String; Port: Word; var Seq: Word);
procedure CreateCLI_SENDMSG_FILEDECLINE(Pkt: PRawPkt; FFSeq: Word; ITime, IRandom, UIN, FileSize: LongWord; const FileDesc, FileName, Reason: String; Port: Word; var Seq: Word);
procedure CreateCLI_HELLO(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_GOODBYE(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_REGISTERUSER(Pkt: PRawPkt; const Password: String; var Seq: Word);
procedure CreateCLI_REQAWAYMSG(Pkt: PRawPkt; FStatus: LongWord; ITime, IRandom, UIN: LongWord; Status: Byte; var Seq: Word);
procedure CreateCLI_SENDCONTACTS(Pkt: PRawPkt; Status: LongWord; ITime, IRandom, UIN: LongWord; Contacts: TStringList; var Seq: Word);
procedure CreateCLI_SENDCONTACTS_REQ(Pkt: PRawPkt; Status: LongWord; ITime, IRandom, UIN: LongWord; Reason: String; var Seq: Word);
procedure CreateCLI_UNREGUIN(Pkt: PRawPkt; UIN: LongWord; const Password: String; var Seq, Seq2: Word);
procedure CreateCLI_METASETPASS(Pkt: PRawPkt; UIN: LongWord; const Password: String; Buffer: Pointer; BufLen: Word; var Seq, Seq2: Word);
procedure CreateCLI_METASETPERMISSIONS(Pkt: PRawPkt; UIN: LongWord; AuthorizationRequired, WebAware: Boolean; var Seq, Seq2: Word);
procedure CreateCLI_METAREQINFO_SHORT(Pkt: PRawPkt; UIN, DestUIN: LongWord; var Seq, Seq2: Word);
procedure CreateCLI_REQAUTH(Pkt: PRawPkt; UIN: LongWord; Msg: String; var Seq: Word);
procedure CreateCLI_KEEPALIVE(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_ADDSTART(Pkt: PRawPkt; FirstUpload: Boolean; var Seq: Word);
procedure CreateCLI_ADDEND(Pkt: PRawPkt; var Seq: Word);
procedure CreateCLI_UPDATEGROUP(Pkt: PRawPkt; Name: String; Tag: Word; IDs: TStringList; var Seq: Word);
procedure CreateCLI_UPDATEBUDDY(Pkt: PRawPkt; UIN, Name, SMSNumber: String; Tag, ID: Word; BuddyType: Word; NotAuthorized: Boolean; var Seq: Word);
procedure CreateCLI_ADDBUDDY(Pkt: PRawPkt; UIN, Name, SMSNumber: String; Tag, ID: Word; BuddyType: Word; NotAuthorized: Boolean; var Seq: Word);
procedure CreateCLI_DELETEBUDDY(Pkt: PRawPkt; UIN, Name, SMSNumber: String; Tag, ID: Word; BuddyType: Word; NotAuthorized, IsGroup: Boolean; var Seq: Word);
procedure CreateCLI_SEND_YOU_WERE_ADDED(Pkt: PRawPkt; ITime, IRandom, UIN, FromUIN: LongWord; var Seq: Word);


{Misc functions}
function SnacToStr(Family, SubType: Word): String;
function SrvMetaToStr(V1, V2: Word): String;
function PeerCmdToStr(Cmd: Byte): String;
function DumpPacket(Buffer: Pointer; BufLen: Word): String;
function RTF2Plain (const aSource: string): string;
function StatusToInt(Value: LongWord): LongWord;  // added by eraser 30.6.03
function StatusToStr(Value: LongWord): String;  // changed by eraser 30.6.03
function CountryToStr(Value: Word): String;
function LanguageToStr(Value: Byte): String;
function OccupationToStr(Value: Word): String;
function InterestToStr(Value: Word): String;
function PastToStr(Value: Word): String;
function AffiliationToStr(Value: Word): String;
function LoadPacketRaw(Pkt: PRawPkt; const FName: String): Boolean;
function LoadPacket(Pkt: PRawPkt; const FName: String; var Flap: TFlapHdr; var Snac: TSnacHdr): Boolean;
function FileExists(const FileName: String): Boolean;
function FileSizeFromStr(const FName: String): LongWord;
procedure LogText(const FName, Text: String);
procedure ShowMessage(const Value: String); overload;
procedure ShowMessage(Value: LongInt); overload;
function ExtractName(const Value: String): String;
function ExtractValue(const Value: String): String;
function UTF8ToStr(Value: String): String;
function UTF8ToStrSmart(Value: String): String;
function GetXMLEntry(const Tag, Msg: String): String;

{SMS functions}
function StrToUTF8(Value: String): String;
function STime: String;
function GetSMSTime: String;

function DecryptPak(Pak: Pointer; Size: LongWord; Ver: Byte): Boolean;
procedure EncryptPak(Pak: Pointer; Size: LongWord; Ver: Byte);

{Peer packet functions}
procedure CreatePEER_INIT(Pkt: PRawPkt; Cookie, DestUIN, SrcUIN, SrcPort, SrcIPExt, SrcIPInt: LongWord; ProxyType: TProxyType);
procedure CreatePEER_INIT2(Pkt: PRawPkt; Ack: Boolean);
procedure CreatePEER_ACK(Pkt: PRawPkt);
function CreatePEER_MSG(Pkt: PRawPkt; const Msg: String; RTFFormat: Boolean; var Seq: Word): Word;
procedure CreatePEER_MSGACK(Pkt: PRawPkt; Seq: Word);
function CreatePEER_MSG_FILE(Pkt: PRawPkt; FileSendRec:TSendFileRec; var Seq: Word): Word;
procedure CreatePEER_AUTOMSG_ACK(Pkt: PRawPkt; Answer: String; Status, Seq: Word);
function CreatePEER_CONTACTS(Pkt: PRawPkt; Contacts: TStringList; var Seq: Word): Word;
function CreatePEER_CONTACTREQ(Pkt: PRawPkt; const Reason: String; var Seq: Word): Word;
function CreatePEER_FILEINIT(Pkt: PRawPkt; Response: Boolean; FileDescription, FileName: String; Port: Word; FileLength: LongWord; var Seq: Word; Reason: String; Accept: Boolean): Word;
procedure CreatePEER_FILEINITACK(Pkt: PRawPkt; Speed: LongWord; Nick: String);
procedure CreatePEER_FILE_INIT(Pkt: PRawPkt; Count, Bytes, Speed: LongWord; Nick:String);
procedure CreatePEER_FILE_INIT2(Pkt: PRawPkt; Count, Bytes, Speed: LongWord);
procedure CreatePEER_FILE_START(Pkt: PRawPkt; FileName:String; FileSize, Speed:LongWord);
Procedure CreatePEER_FILE_DATA(Pkt: PRawPkt; Buffer:Pointer; BufLen:Integer);


function EncodeBase64(Value: String): String;
function DecodeBase64(Value: String): String;
function CreateHTTP_Header(const Method, URL, Host: String; DataLen: LongWord; Auth: Boolean; User, Password: String): String;
procedure CreateHTTP_DATA_HDR(Pkt: PRawPkt; PType: Word; DataLen: LongWord);
procedure CreateHTTP_DATA(Pkt: PRawPkt; PType: Word; Data: Pointer; DataLen: LongWord);
function CreateHTTP_INIT(Auth: Boolean; User, Password: String): String;
function CreateHTTP_RECV(Host, SID: String; Auth: Boolean; User, Password: String): String;
procedure CreateHTTP_LOGIN(Pkt: PRawPkt; Host: String; Port: Word);


function Decrypt99bPassword(UIN, CryptIV: LongWord; const HexPass: String): String;
function DecryptMirandaPassword(const Value: String): String;

function UnixToDateTime(const AValue: Int64): TDateTime;
function DateTimeToUnix(const AValue: TDateTime): Int64;

//Text constants
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//------------------------------------------------------------------------------------------------------------\
const
  Countries: array[0..242] of record Ident: Word; Value: String end =
    ((Ident: 1; Value: 'USA'),
    (Ident: 7; Value: 'Russia'),
    (Ident: 20; Value: 'Egypt'),
    (Ident: 27; Value: 'South Africa'),
    (Ident: 30; Value: 'Greece'),
    (Ident: 31; Value: 'Netherlands'),
    (Ident: 32; Value: 'Belgium'),
    (Ident: 33; Value: 'France'),
    (Ident: 34; Value: 'Spain'),
    (Ident: 36; Value: 'Hungary'),
    (Ident: 39; Value: 'Italy'),
    (Ident: 40; Value: 'Romania'),
    (Ident: 41; Value: 'Switzerland'),
    (Ident: 42; Value: 'Czech Republic'),
    (Ident: 43; Value: 'Austria'),
    (Ident: 44; Value: 'United Kingdom'),
    (Ident: 45; Value: 'Denmark'),
    (Ident: 46; Value: 'Sweden'),
    (Ident: 47; Value: 'Norway'),
    (Ident: 48; Value: 'Poland'),
    (Ident: 49; Value: 'Germany'),
    (Ident: 51; Value: 'Peru'),
    (Ident: 52; Value: 'Mexico'),
    (Ident: 53; Value: 'Cuba'),
    (Ident: 54; Value: 'Argentina'),
    (Ident: 55; Value: 'Brazil'),
    (Ident: 56; Value: 'Chile'),
    (Ident: 57; Value: 'Colombia'),
    (Ident: 58; Value: 'Venezuela'),
    (Ident: 60; Value: 'Malaysia'),
    (Ident: 61; Value: 'Australia'),
    (Ident: 62; Value: 'Indonesia'),
    (Ident: 63; Value: 'Philippines'),
    (Ident: 64; Value: 'New Zealand'),
    (Ident: 65; Value: 'Singapore'),
    (Ident: 66; Value: 'Thailand'),
    (Ident: 81; Value: 'Japan'),
    (Ident: 82; Value: 'Korea (Republic of)'),
    (Ident: 84; Value: 'Vietnam'),
    (Ident: 86; Value: 'China'),
    (Ident: 90; Value: 'Turkey'),
    (Ident: 91; Value: 'India'),
    (Ident: 92; Value: 'Pakistan'),
    (Ident: 93; Value: 'Afghanistan'),
    (Ident: 94; Value: 'Sri Lanka'),
    (Ident: 95; Value: 'Myanmar'),
    (Ident: 98; Value: 'Iran'),
    (Ident: 101; Value: 'Anguilla'),
    (Ident: 102; Value: 'Antigua'),
    (Ident: 103; Value: 'Bahamas'),
    (Ident: 104; Value: 'Barbados'),
    (Ident: 105; Value: 'Bermuda'),
    (Ident: 106; Value: 'British Virgin Islands'),
    (Ident: 107; Value: 'Canada'),
    (Ident: 108; Value: 'Cayman Islands'),
    (Ident: 109; Value: 'Dominica'),
    (Ident: 110; Value: 'Dominican Republic'),
    (Ident: 111; Value: 'Grenada'),
    (Ident: 112; Value: 'Jamaica'),
    (Ident: 113; Value: 'Montserrat'),
    (Ident: 114; Value: 'Nevis'),
    (Ident: 115; Value: 'St. Kitts'),
    (Ident: 116; Value: 'St. Vincent and the Grenadines'),
    (Ident: 117; Value: 'Trinidad and Tobago'),
    (Ident: 118; Value: 'Turks and Caicos Islands'),
    (Ident: 120; Value: 'Barbuda'),
    (Ident: 121; Value: 'Puerto Rico'),
    (Ident: 122; Value: 'Saint Lucia'),
    (Ident: 123; Value: 'United States Virgin Islands'),
    (Ident: 212; Value: 'Morocco'),
    (Ident: 213; Value: 'Algeria'),
    (Ident: 216; Value: 'Tunisia'),
    (Ident: 218; Value: 'Libya'),
    (Ident: 220; Value: 'Gambia'),
    (Ident: 221; Value: 'Senegal Republic'),
    (Ident: 222; Value: 'Mauritania'),
    (Ident: 223; Value: 'Mali'),
    (Ident: 224; Value: 'Guinea'),
    (Ident: 225; Value: 'Ivory Coast'),
    (Ident: 226; Value: 'Burkina Faso'),
    (Ident: 227; Value: 'Niger'),
    (Ident: 228; Value: 'Togo'),
    (Ident: 229; Value: 'Benin'),
    (Ident: 230; Value: 'Mauritius'),
    (Ident: 231; Value: 'Liberia'),
    (Ident: 232; Value: 'Sierra Leone'),
    (Ident: 233; Value: 'Ghana'),
    (Ident: 234; Value: 'Nigeria'),
    (Ident: 235; Value: 'Chad'),
    (Ident: 236; Value: 'Central African Republic'),
    (Ident: 237; Value: 'Cameroon'),
    (Ident: 238; Value: 'Cape Verde Islands'),
    (Ident: 239; Value: 'Sao Tome and Principe'),
    (Ident: 240; Value: 'Equatorial Guinea'),
    (Ident: 241; Value: 'Gabon'),
    (Ident: 242; Value: 'Congo'),
    (Ident: 243; Value: 'Dem. Rep. of the Congo'),
    (Ident: 244; Value: 'Angola'),
    (Ident: 245; Value: 'Guinea-Bissau'),
    (Ident: 246; Value: 'Diego Garcia'),
    (Ident: 247; Value: 'Ascension Island'),
    (Ident: 248; Value: 'Seychelle Islands'),
    (Ident: 249; Value: 'Sudan'),
    (Ident: 250; Value: 'Rwanda'),
    (Ident: 251; Value: 'Ethiopia'),
    (Ident: 252; Value: 'Somalia'),
    (Ident: 253; Value: 'Djibouti'),
    (Ident: 254; Value: 'Kenya'),
    (Ident: 255; Value: 'Tanzania'),
    (Ident: 256; Value: 'Uganda'),
    (Ident: 257; Value: 'Burundi'),
    (Ident: 258; Value: 'Mozambique'),
    (Ident: 260; Value: 'Zambia'),
    (Ident: 261; Value: 'Madagascar'),
    (Ident: 262; Value: 'Reunion Island'),
    (Ident: 263; Value: 'Zimbabwe'),
    (Ident: 264; Value: 'Namibia'),
    (Ident: 265; Value: 'Malawi'),
    (Ident: 266; Value: 'Lesotho'),
    (Ident: 267; Value: 'Botswana'),
    (Ident: 268; Value: 'Swaziland'),
    (Ident: 269; Value: 'Mayotte Island'),
    (Ident: 290; Value: 'St. Helena'),
    (Ident: 291; Value: 'Eritrea'),
    (Ident: 297; Value: 'Aruba'),
    (Ident: 298; Value: 'Faeroe Islands'),
    (Ident: 299; Value: 'Greenland'),
    (Ident: 350; Value: 'Gibraltar'),
    (Ident: 351; Value: 'Portugal'),
    (Ident: 352; Value: 'Luxembourg'),
    (Ident: 353; Value: 'Ireland'),
    (Ident: 354; Value: 'Iceland'),
    (Ident: 355; Value: 'Albania'),
    (Ident: 356; Value: 'Malta'),
    (Ident: 357; Value: 'Cyprus'),
    (Ident: 358; Value: 'Finland'),
    (Ident: 359; Value: 'Bulgaria'),
    (Ident: 370; Value: 'Lithuania'),
    (Ident: 371; Value: 'Latvia'),
    (Ident: 372; Value: 'Estonia'),
    (Ident: 373; Value: 'Moldova'),
    (Ident: 374; Value: 'Armenia'),
    (Ident: 375; Value: 'Belarus'),
    (Ident: 376; Value: 'Andorra'),
    (Ident: 377; Value: 'Monaco'),
    (Ident: 378; Value: 'San Marino'),
    (Ident: 379; Value: 'Vatican City'),
    (Ident: 380; Value: 'Ukraine'),
    (Ident: 381; Value: 'Yugoslavia'),
    (Ident: 385; Value: 'Croatia'),
    (Ident: 386; Value: 'Slovenia'),
    (Ident: 387; Value: 'Bosnia and Herzegovina'),
    (Ident: 389; Value: 'F.Y.R.O.M. (Former Yugoslav Republic of Macedonia)'),
    (Ident: 500; Value: 'Falkland Islands'),
    (Ident: 501; Value: 'Belize'),
    (Ident: 502; Value: 'Guatemala'),
    (Ident: 503; Value: 'El Salvador'),
    (Ident: 504; Value: 'Honduras'),
    (Ident: 505; Value: 'Nicaragua'),
    (Ident: 506; Value: 'Costa Rica'),
    (Ident: 507; Value: 'Panama'),
    (Ident: 508; Value: 'St. Pierre and Miquelon'),
    (Ident: 509; Value: 'Haiti'),
    (Ident: 590; Value: 'Guadeloupe'),
    (Ident: 591; Value: 'Bolivia'),
    (Ident: 592; Value: 'Guyana'),
    (Ident: 593; Value: 'Ecuador'),
    (Ident: 594; Value: 'French Guiana'),
    (Ident: 595; Value: 'Paraguay'),
    (Ident: 596; Value: 'Martinique'),
    (Ident: 597; Value: 'Suriname'),
    (Ident: 598; Value: 'Uruguay'),
    (Ident: 599; Value: 'Netherlands Antilles'),
    (Ident: 670; Value: 'Saipan Island'),
    (Ident: 671; Value: 'Guam'),
    (Ident: 672; Value: 'Christmas Island'),
    (Ident: 673; Value: 'Brunei'),
    (Ident: 674; Value: 'Nauru'),
    (Ident: 675; Value: 'Papua New Guinea'),
    (Ident: 676; Value: 'Tonga'),
    (Ident: 677; Value: 'Solomon Islands'),
    (Ident: 678; Value: 'Vanuatu'),
    (Ident: 679; Value: 'Fiji Islands'),
    (Ident: 680; Value: 'Palau'),
    (Ident: 681; Value: 'Wallis and Futuna Islands'),
    (Ident: 682; Value: 'Cook Islands'),
    (Ident: 683; Value: 'Niue'),
    (Ident: 684; Value: 'American Samoa'),
    (Ident: 685; Value: 'Western Samoa'),
    (Ident: 686; Value: 'Kiribati Republic'),
    (Ident: 687; Value: 'New Caledonia'),
    (Ident: 688; Value: 'Tuvalu'),
    (Ident: 689; Value: 'French Polynesia'),
    (Ident: 690; Value: 'Tokelau'),
    (Ident: 691; Value: 'Micronesia, Federated States of'),
    (Ident: 692; Value: 'Marshall Islands'),
    (Ident: 705; Value: 'Kazakhstan'),
    (Ident: 706; Value: 'Kyrgyz Republic'),
    (Ident: 708; Value: 'Tajikistan'),
    (Ident: 709; Value: 'Turkmenistan'),
    (Ident: 711; Value: 'Uzbekistan'),
    (Ident: 800; Value: 'International Freephone Service'),
    (Ident: 850; Value: 'Korea (North)'),
    (Ident: 852; Value: 'Hong Kong'),
    (Ident: 853; Value: 'Macau'),
    (Ident: 855; Value: 'Cambodia'),
    (Ident: 856; Value: 'Laos'),
    (Ident: 870; Value: 'INMARSAT'),
    (Ident: 871; Value: 'INMARSAT (Atlantic-East)'),
    (Ident: 872; Value: 'INMARSAT (Pacific)'),
    (Ident: 873; Value: 'INMARSAT (Indian)'),
    (Ident: 874; Value: 'INMARSAT (Atlantic-West)'),
    (Ident: 880; Value: 'Bangladesh'),
    (Ident: 886; Value: 'Taiwan, Republic of China'),
    (Ident: 960; Value: 'Maldives'),
    (Ident: 961; Value: 'Lebanon'),
    (Ident: 962; Value: 'Jordan'),
    (Ident: 963; Value: 'Syria'),
    (Ident: 964; Value: 'Iraq'),
    (Ident: 965; Value: 'Kuwait'),
    (Ident: 966; Value: 'Saudi Arabia'),
    (Ident: 967; Value: 'Yemen'),
    (Ident: 968; Value: 'Oman'),
    (Ident: 971; Value: 'United Arab Emirates'),
    (Ident: 972; Value: 'Israel'),
    (Ident: 973; Value: 'Bahrain'),
    (Ident: 974; Value: 'Qatar'),
    (Ident: 975; Value: 'Bhutan'),
    (Ident: 976; Value: 'Mongolia'),
    (Ident: 977; Value: 'Nepal'),
    (Ident: 994; Value: 'Azerbaijan'),
    (Ident: 995; Value: 'Georgia'),
    (Ident: 2691; Value: 'Comoros'),
    (Ident: 4101; Value: 'Liechtenstein'),
    (Ident: 4201; Value: 'Slovak Republic'),
    (Ident: 5399; Value: 'Guantanamo Bay'),
    (Ident: 5901; Value: 'French Antilles'),
    (Ident: 6101; Value: 'Cocos-Keeling Islands'),
    (Ident: 6701; Value: 'Rota Island'),
    (Ident: 6702; Value: 'Tinian Island'),
    (Ident: 6721; Value: 'Australian Antarctic Territory'),
    (Ident: 6722; Value: 'Norfolk Island'),
    (Ident: 9999; Value: 'Unknown'));

  Languages: array[0..72] of record Ident: Byte; Value: String end =
    ((Ident: 1; Value: 'Arabic'),
    (Ident: 2; Value: 'Bhojpuri'),
    (Ident: 3; Value: 'Bulgarian'),
    (Ident: 4; Value: 'Burmese'),
    (Ident: 5; Value: 'Cantonese'),
    (Ident: 6; Value: 'Catalan'),
    (Ident: 7; Value: 'Chinese'),
    (Ident: 8; Value: 'Croatian'),
    (Ident: 9; Value: 'Czech'),
    (Ident: 10; Value: 'Danish'),
    (Ident: 11; Value: 'Dutch'),
    (Ident: 12; Value: 'English'),
    (Ident: 13; Value: 'Esperanto'),
    (Ident: 14; Value: 'Estonian'),
    (Ident: 15; Value: 'Farci'),
    (Ident: 16; Value: 'Finnish'),
    (Ident: 17; Value: 'French'),
    (Ident: 18; Value: 'Gaelic'),
    (Ident: 19; Value: 'German'),
    (Ident: 20; Value: 'Greek'),
    (Ident: 21; Value: 'Hebrew'),
    (Ident: 22; Value: 'Hindi'),
    (Ident: 23; Value: 'Hungarian'),
    (Ident: 24; Value: 'Icelandic'),
    (Ident: 25; Value: 'Indonesian'),
    (Ident: 26; Value: 'Italian'),
    (Ident: 27; Value: 'Japanese'),
    (Ident: 28; Value: 'Khmer'),
    (Ident: 29; Value: 'Korean'),
    (Ident: 30; Value: 'Lao'),
    (Ident: 31; Value: 'Latvian'),
    (Ident: 32; Value: 'Lithuanian'),
    (Ident: 33; Value: 'Malay'),
    (Ident: 34; Value: 'Norwegian'),
    (Ident: 35; Value: 'Polish'),
    (Ident: 36; Value: 'Portuguese'),
    (Ident: 37; Value: 'Romanian'),
    (Ident: 38; Value: 'Russian'),
    (Ident: 39; Value: 'Serbo-Croatian'),
    (Ident: 40; Value: 'Slovak'),
    (Ident: 41; Value: 'Slovenian'),
    (Ident: 42; Value: 'Somali'),
    (Ident: 43; Value: 'Spanish'),
    (Ident: 44; Value: 'Swahili'),
    (Ident: 45; Value: 'Swedish'),
    (Ident: 46; Value: 'Tagalog'),
    (Ident: 47; Value: 'Tatar'),
    (Ident: 48; Value: 'Thai'),
    (Ident: 49; Value: 'Turkish'),
    (Ident: 50; Value: 'Ukrainian'),
    (Ident: 51; Value: 'Urdu'),
    (Ident: 52; Value: 'Vietnamese'),
    (Ident: 53; Value: 'Yiddish'),
    (Ident: 54; Value: 'Yoruba'),
    (Ident: 55; Value: 'Afrikaans'),
    (Ident: 56; Value: 'Bosnian'),
    (Ident: 57; Value: 'Persian'),
    (Ident: 58; Value: 'Albanian'),
    (Ident: 59; Value: 'Armenian'),
    (Ident: 60; Value: 'Punjabi'),
    (Ident: 61; Value: 'Chamorro'),
    (Ident: 62; Value: 'Mongolian'),
    (Ident: 63; Value: 'Mandarin'),
    (Ident: 64; Value: 'Taiwanese'),
    (Ident: 65; Value: 'Macedonian'),
    (Ident: 66; Value: 'Sindhi'),
    (Ident: 67; Value: 'Welsh'),
    (Ident: 68; Value: 'Azerbaijani'),
    (Ident: 69; Value: 'Kurdish'),
    (Ident: 70; Value: 'Gujarati'),
    (Ident: 71; Value: 'Tamil'),
    (Ident: 72; Value: 'Belorussian'),
    (Ident: 255; Value: 'Unknown'));

  Occupations: array[1..17] of record Ident: Byte; Value: String end =
    ((Ident: 1; Value: 'Academic'),
    (Ident: 2; Value: 'Administrative'),
    (Ident: 3; Value: 'Art/Entertainment'),
    (Ident: 4; Value: 'College Student'),
    (Ident: 5; Value: 'Computers'),
    (Ident: 6; Value: 'Community & Social'),
    (Ident: 7; Value: 'Education'),
    (Ident: 8; Value: 'Engineering'),
    (Ident: 9; Value: 'Financial Services'),
    (Ident: 10; Value: 'Government'),
    (Ident: 11; Value: 'High School Student'),
    (Ident: 12; Value: 'Home'),
    (Ident: 13; Value: 'ICQ - Providing Help'),
    (Ident: 14; Value: 'Law'),
    (Ident: 15; Value: 'Managerial'),
    (Ident: 16; Value: 'Manufacturing'),
    (Ident: 17; Value: 'Medical/Health'));

  Interests: array[100..150] of record Ident: Byte; Value: String end =
    ((Ident: 100; Value: 'Art'),
    (Ident: 101; Value: 'Cars'),
    (Ident: 102; Value: 'Celebrity Fans'),
    (Ident: 103; Value: 'Collections'),
    (Ident: 104; Value: 'Computers'),
    (Ident: 105; Value: 'Culture & Literature'),
    (Ident: 106; Value: 'Fitness'),
    (Ident: 107; Value: 'Games'),
    (Ident: 108; Value: 'Hobbies'),
    (Ident: 109; Value: 'ICQ - Providing Help'),
    (Ident: 110; Value: 'Internet'),
    (Ident: 111; Value: 'Lifestyle'),
    (Ident: 112; Value: 'Movies/TV'),
    (Ident: 113; Value: 'Music'),
    (Ident: 114; Value: 'Outdoor Activities'),
    (Ident: 115; Value: 'Parenting'),
    (Ident: 116; Value: 'Pets/Animals'),
    (Ident: 117; Value: 'Religion'),
    (Ident: 118; Value: 'Science/Technology'),
    (Ident: 119; Value: 'Skills'),
    (Ident: 120; Value: 'Sports'),
    (Ident: 121; Value: 'Web Design'),
    (Ident: 122; Value: 'Nature and Environment'),
    (Ident: 123; Value: 'News & Media'),
    (Ident: 124; Value: 'Government'),
    (Ident: 125; Value: 'Business & Economy'),
    (Ident: 126; Value: 'Mystics'),
    (Ident: 127; Value: 'Travel'),
    (Ident: 128; Value: 'Astronomy'),
    (Ident: 129; Value: 'Space'),
    (Ident: 130; Value: 'Clothing'),
    (Ident: 131; Value: 'Parties'),
    (Ident: 132; Value: 'Women'),
    (Ident: 133; Value: 'Social science'),
    (Ident: 134; Value: '60''s'),
    (Ident: 135; Value: '70''s'),
    (Ident: 136; Value: '80''s'),
    (Ident: 137; Value: '50''s'),
    (Ident: 138; Value: 'Finance and corporate'),
    (Ident: 139; Value: 'Entertainment'),
    (Ident: 140; Value: 'Consumer electronics'),
    (Ident: 141; Value: 'Retail stores'),
    (Ident: 142; Value: 'Health and beauty'),
    (Ident: 143; Value: 'Media'),
    (Ident: 144; Value: 'Household products'),
    (Ident: 145; Value: 'Mail order catalog'),
    (Ident: 146; Value: 'Business services'),
    (Ident: 147; Value: 'Audio and visual'),
    (Ident: 148; Value: 'Sporting and athletic'),
    (Ident: 149; Value: 'Publishing'),
    (Ident: 150; Value: 'Home automation'));

  RandGroups: array[1..11] of record Ident: Byte; Value: String end =
    ((Ident: 1; Value: 'General'),
    (Ident: 2; Value: 'Romance'),
    (Ident: 3; Value: 'Games'),
    (Ident: 4; Value: 'Students'),
    (Ident: 5; Value: '20 something'),
    (Ident: 6; Value: '30 something'),
    (Ident: 7; Value: '40 something'),
    (Ident: 8; Value: '50+'),
    (Ident: 9; Value: 'Romance'),
    (Ident: 10; Value: 'Man requesting woman'),
    (Ident: 11; Value: 'Woman requesting man'));

  Organizations: array[0..19] of record Ident: Word; Value: String end =
    ((Ident: 200; Value: 'Alumni Org.'),
    (Ident: 201; Value: 'Charity Org.'),
    (Ident: 202; Value: 'Club/Social Org.'),
    (Ident: 203; Value: 'Community Org.'),
    (Ident: 204; Value: 'Cultural Org.'),
    (Ident: 205; Value: 'Fan Clubs'),
    (Ident: 206; Value: 'Fraternity/Sorority'),
    (Ident: 207; Value: 'Hobbyists Org.'),
    (Ident: 208; Value: 'International Org.'),
    (Ident: 209; Value: 'Nature and Environment Org.'),
    (Ident: 210; Value: 'Professional Org.'),
    (Ident: 211; Value: 'Scientific/Technical Org.'),
    (Ident: 212; Value: 'Self Improvement Group'),
    (Ident: 213; Value: 'Spiritual/Religious Org.'),
    (Ident: 214; Value: 'Sports Org.'),
    (Ident: 215; Value: 'Support Org.'),
    (Ident: 216; Value: 'Trade and Business Org.'),
    (Ident: 217; Value: 'Union'),
    (Ident: 218; Value: 'Volunteer Org.'),
    (Ident: 299; Value: 'Other'));

  Pasts: array[0..7] of record Ident: Word; Value: String end =
    ((Ident: 300; Value: 'Elementary School'),
    (Ident: 301; Value: 'High School'),
    (Ident: 302; Value: 'College'),
    (Ident: 303; Value: 'University'),
    (Ident: 304; Value: 'Military'),
    (Ident: 305; Value: 'Past Work Place'),
    (Ident: 306; Value: 'Past Organization'),
    (Ident: 399; Value: 'Other'));



//------------------------------------------------------------------------------------------------------------\
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Var
  gPortRange:TPortRange;

implementation
const
  FileSignature: array[0..15] of Byte = ($f0, $2d, $12, $d9, $30, $91, $d3, $11,
                                         $8d, $d7, $00, $10, $4b, $06, $46, $2e);

  ContactsSignature: array[0..15] of Byte = ($2a, $0e, $7d, $46, $76, $76, $d4, $11,
                                             $bc, $e6, $00, $04, $ac, $96, $1e, $a6);

function IntToStr(Value: Int64): String;
begin
  Str(Value, Result);
end;

function StrToInt(const Value: String): LongWord;
var
  nCode: Integer;
begin
  Val(Value, Result, nCode);
end;

const
  HexChars: array[0..15] of Char = ('0', '1', '2', '3', '4', '5',
                                    '6', '7', '8', '9', 'a', 'b',
                                    'c', 'd', 'e', 'f');

function IntToHex(Int: Int64; IntSize: Byte): String;
var
  n: Byte;
begin
  Result := '';
  for n := 0 to IntSize - 1 do
  begin
    Result := HexChars[Int and $F] + Result;
    Int := Int shr $4;
  end;
end;

function HexToInt(Value: String): LongWord;
const
  HexStr: String = '0123456789abcdef';
var
  i: Word;
begin
  Result := 0;
  if Value = '' then Exit;
  for i := 1 to Length(Value) do
    Inc(Result, (Pos(Value[i], HexStr) - 1) shl ((Length(Value) - i) shl 2));
end;

const
  TXorData: array[0..15] of Byte = (
    $F3, $26, $81, $C4, $39, $86, $DB, $92,
    $71, $A3, $B9, $E6, $53, $7A, $95, $7C
  );

procedure ICQEncryptPass(SrcBuf: Pointer; BufLen: LongWord); assembler;
asm
  or    edx,edx
  jz    @@end
@@loop:
  mov   cl,byte ptr[eax + edx - 1]
  xor   cl,byte ptr[TXorData + edx - 1]
  mov   byte ptr[eax + edx - 1],cl
  dec   edx
  jnz   @@loop
@@end:
end;

procedure ICQEncryptPassStr(var Pass: String);
var
  i: Word;
begin
  for i := 1 to Length(Pass) do
    Pass[i] := Chr(Ord(Pass[i]) xor TXorData[i - 1]);
end;

function Swap16(Value: Word): Word; assembler;
asm
  rol   ax,8
end;

function Swap32(Value: LongWord): LongWord; assembler;
asm
  bswap eax
end;

{----------------------------------------------}
//Adding data in reverse order
procedure PktAddData(Pkt: PRawPkt; Data: Pointer; DataLen: LongWord);
var
  i: Word;
begin
  if DataLen = 0 then Exit;
  if Pkt^.Len + DataLen >= 8192 then
  begin
    DataLen := MAX_DATA_LEN - Pkt^.Len;
    if DataLen = 0 then Exit;
  end;
  for i := 0 to DataLen - 1 do
    PByte(LongWord(Pkt) + Pkt^.Len + i)^ := PByte(LongWord(Data) + DataLen - i - 1)^;
  Inc(Pkt^.Len, DataLen);
end;

//Adding data in direct order(local arrays, merging 2 or more packets)
procedure PktAddArrBuf(Pkt: PRawPkt; Data: Pointer; DataLen: LongWord);
begin
  if (DataLen = 0) or (Pkt^.Len >= MAX_DATA_LEN) then
  begin
    Exit;
  end;
  if Pkt^.Len + DataLen >= 8192 then
  begin
    DataLen := MAX_DATA_LEN - Pkt^.Len;
    if DataLen = 0 then Exit;
  end;
  Move(Data^, Ptr(LongWord(Pkt) + Pkt^.Len)^, DataLen);
  Inc(Pkt^.Len, DataLen);
end;

procedure PktInt(Pkt: PRawPkt; Value: LongWord; IntSize: Byte);
begin
  PktAddData(Pkt, @Value, IntSize);
end;

procedure PktLInt(Pkt: PRawPkt; Value: LongWord; IntSize: Byte);
begin
  PktAddArrBuf(Pkt, @Value, IntSize);
end;

procedure PktStr(Pkt: PRawPkt; const S: String);
begin
  PktAddArrBuf(Pkt, @S[1], Length(S));
end;

procedure PktLStr(Pkt: PRawPkt; const S: String); overload;
begin
  PktInt(Pkt, Length(S), 1);
  PktStr(Pkt, S);
end;

procedure PktLStr(Pkt: PRawPkt; S: LongWord); overload;
begin
  PktLStr(Pkt, IntToStr(S));
end;

procedure PktWStr(Pkt: PRawPkt; const S: String);
begin
  if Length(S) = 0 then
  begin
    PktInt(Pkt, 0, 2);
    Exit;
  end;
  PktInt(Pkt, Length(S), 2);
  PktStr(Pkt, S);
end;

procedure PktDWStr(Pkt: PRawPkt; const S: String);
begin
  PktLInt(Pkt, Length(S), 4);
  PktStr(Pkt, S);
end;

procedure PktLNTS(Pkt: PRawPkt; const S: String);
begin
  if Length(S) = 0 then
  begin
    PktInt(Pkt, 0, 2);
    Exit;
  end;
  PktLInt(Pkt, Length(S) + 1, 2);
  PktStr(Pkt, S);
  PktInt(Pkt, 0, 1);
end;

procedure PktLLNTS(Pkt: PRawPkt; const S: String);
begin
  if Length(S) = 0 then
  begin
    PktInt(Pkt, 0, 2);
    Exit;
  end;
  PktLInt(Pkt, Length(S) + 3, 2);
  PktLNTS(Pkt, S);
end;



{--}
function GetInt(Pkt: PRawPkt; IntSize: Byte): LongWord;
var
  i: Word;
begin
  Result := 0;
  if IntSize = 0 then Exit;
  if Pkt^.Len > 8100 then Exit;
  for i := Pkt^.Len to Pkt^.Len + IntSize - 1 do
    Inc(Result, PByte(LongWord(Pkt) + i)^ shl ((Pkt^.Len + IntSize - 1 - i) * 8));
  Inc(Pkt^.Len, IntSize);
end;

function GetLInt(Pkt: PRawPkt; IntSize: Byte): LongWord;
var
  i, c: Word;
begin
  Result := 0; c := 0;
  if IntSize = 0 then Exit;
  if Pkt^.Len > 8100 then Exit;
  for i := Pkt^.Len to Pkt^.Len + IntSize - 1 do
  begin
    Inc(Result, PByte(LongWord(Pkt) + Pkt^.Len + IntSize - c - 1)^ shl ((Pkt^.Len + IntSize - 1 - i) * 8));
    Inc(c);
  end;
  Inc(Pkt^.Len, IntSize);
end;

function GetStr(Pkt: PRawPkt; StrLen: Integer): String;
begin
  Result := '';
  while StrLen > 0 do
  begin
    Result := Result + PChar(LongWord(Pkt) + Pkt^.Len)^;
    Inc(Pkt^.Len);
    Dec(StrLen);
    if Pkt^.Len > 8100 then Exit;
  end;
end;

function GetTLVStr(Pkt: PRawPkt; var T: Word): String;
var
  ISize: Word;
begin
  T := GetInt(Pkt, 2);          //Get type
  ISize := GetInt(Pkt, 2);      //Get data length
  Result := GetStr(Pkt, ISize); //Get data
end;

function GetTLVInt(Pkt: PRawPkt; var T: Word): LongWord;
var
  ISize: Word;
begin
  T := GetInt(Pkt, 2);          //Get type
  ISize := GetInt(Pkt, 2);      //Get data length
  Result := GetInt(Pkt, ISize); //Get data
end;

procedure GetSnac(Pkt: PRawPkt; var Snac: TSnacHdr);
begin
  Snac := PSnacHdr(LongWord(Pkt) + Pkt^.Len)^;
  Snac.Family := Swap16(Snac.Family);
  Snac.SubType := Swap16(Snac.SubType);
  Snac.ReqID := Swap32(Snac.ReqID);
  Snac.Flags := Swap16(Snac.Flags);
  Inc(Pkt^.Len, TSNACSZ);
end;

function GetLStr(Pkt: PRawPkt): String;
begin
  Result := GetStr(Pkt, GetInt(Pkt, 1));
end;

function GetWStr(Pkt: PRawPkt): String;
begin
  Result := GetStr(Pkt, GetInt(Pkt, 2));
end;

function GetDWStr(Pkt: PRawPkt): String;
begin
  Result := GetStr(Pkt, GetLInt(Pkt, 4));
end;

function GetLNTS(Pkt: PRawPkt): String;
begin
  Result := GetStr(Pkt, LongInt(GetLInt(Pkt, 2)) - 1);
  Inc(Pkt^.Len, 1);
end;


{--------}
procedure PktTLV(Pkt: PRawPkt; T, L: Word; V: LongWord); overload;
begin
  PktInt(Pkt, T, 2);  //Add type
  PktInt(Pkt, L, 2);  //Add length
  PktInt(Pkt, V, L);  //Add value
end;

procedure PktTLV(Pkt: PRawPkt; T: Word; const V: String); overload;
begin
  PktInt(Pkt, T, 2);            //Add type
  PktInt(Pkt, Length(V), 2);    //Add length
  PktStr(Pkt, V);               //Add value
end;

procedure PktTLV(Pkt: PRawPkt; T, L: Word; V: Pointer); overload; //for arrays
begin
  PktInt(Pkt, T, 2);            //Add type
  PktInt(Pkt, L, 2);            //Add length
  PktAddArrBuf(Pkt, V, L);      //Add value
end;

procedure PktInit(Pkt: PRawPkt; Channel: Byte; var Seq: Word);
begin
  Pkt^.Len := 0;                //Starting size of packet to 0
  PktInt(Pkt, $2A, 1);          //Ident, always $2A
  PktInt(Pkt, Channel, 1);      //Channel
  PktInt(Pkt, SEQ, 2); Inc(SEQ);//Seq
  PktInt(Pkt, 0, 2);            //Reserved for size
end;


{$IFDEF USE_ASM}
procedure PktInitRaw(Pkt: PRawPkt); assembler;
asm
  mov   word ptr[eax + MAX_DATA_LEN],0          //Default size of the packet
end;
{$ELSE}
procedure PktInitRaw(Pkt: PRawPkt);
begin
  Pkt^.Len := 0;                                //Default size of the packet
end;
{$ENDIF}


//Used with PktInit only
{$IFDEF USE_ASM}
procedure PktFinal(Pkt: PRawPkt); assembler;
asm
  mov   cx,word ptr[eax + MAX_DATA_LEN]
  sub   cx,TFLAPSZ
  rol   cx,8
  mov   word ptr[eax + 4],cx   //Store the packet size (without flap header size)
end;
{$ELSE}
procedure PktFinal(Pkt: PRawPkt);
begin
  PWord(LongWord(Pkt) + 4)^ := Swap16(Pkt.Len - TFLAPSZ); //Store the packet size (without flap header size)
end;
{$ENDIF}

procedure PktSnac(Pkt: PRawPkt; Family, SubType: Word; ID: LongWord; Flags: Word);
begin
  PktInt(Pkt, Family, 2);       //Snac family
  PktInt(Pkt, SubType, 2);      //Snac subtype
  PktInt(Pkt, Flags, 2);        //Snac flags
  PktInt(Pkt, ID, 4);           //Snac reference
end;

{@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@}
function StrToLanguageI(const Value: String): Word;
var
  i: Word;
begin
  for i := Low(Languages) to High(Languages) do
    if Languages[i].Value = Value then
    begin
      Result := Languages[i].Ident;
      Exit;
    end;
  Result := 0;
end;

function StrToCountryI(const Value: String): Word;
var
  i: Word;
begin
  for i := Low(Countries) to High(Countries) do
    if Countries[i].Value = Value then
    begin
      Result := Countries[i].Ident;
      Exit;
    end;
  Result := 0;
end;

function StrToInterestI(const Value: String): Word;
var
  i: Word;
begin
  for i := Low(Interests) to High(Interests) do
    if Interests[i].Value = Value then
    begin
      Result := Interests[i].Ident;
      Exit;
    end;
  Result := 0;
end;

function StrToOccupationI(const Value: String): Word;
var
  i: Word;
begin
  for i := Low(Occupations) to High(Occupations) do
    if Occupations[i].Value = Value then
    begin
      Result := Occupations[i].Ident;
      Exit;
    end;
  Result := 0;
end;

function StrToPastI(const Value: String): Word;
var
  i: Word;
begin
  for i := Low(Pasts) to High(Pasts) do
    if Pasts[i].Value = Value then
    begin
      Result := Pasts[i].Ident;
      Exit;
    end;
  Result := 0;
end;

function StrToOrganizationI(const Value: String): Word;
var
  i: Word;
begin
  for i := Low(Organizations) to High(Organizations) do
    if Organizations[i].Value = Value then
    begin
      Result := Organizations[i].Ident;
      Exit;
    end;
  Result := 0;
end;

{Parse contacts responses.}
procedure ParseContacts(Value: String; var List: TStringList);
var
  i, l: Word;
  FName, FUIN: String;
begin
  l := 0; FName := ''; FUIN := '';
  if Length(Value) > Pos(#$fe, Value) + 1 then
    for i := Pos(#$fe, Value) + 1 to Length(Value) do
    begin
      if Value[i] = #$fe then
        Inc(l)
      else
      begin
        if l mod 2 = 0 then
          FName := FName + Value[i]
        else
          FUIN := FUIN + Value[i];
      end;
      if l = 2 then
      begin
        if (FName <> '') and (FUIN <> '') then
          List.Add(FName + '=' + FUIN);
        FName := '';
        FUIN := '';
        l := 0;
      end;
    end;
end;

{Create string representation of list with contacts.}
function MakeContactsStr(Contacts: TStringList): String;
var
  i, count: Word;
  S: String;
begin
  count := 0;
  S := '';
  if Contacts.Count > 0 then
    for i := 0 to Contacts.Count - 1 do
    begin
      if ExtractName(Contacts.Strings[i]) <> '' then
      begin
        S := S + ExtractName(Contacts.Strings[i]) + #$fe;
        if ExtractValue(Contacts.Strings[i]) = '' then
          S := S + ExtractName(Contacts.Strings[i]) + #$fe
        else
          S := S + ExtractValue(Contacts.Strings[i]) + #$fe;
        Inc(count);
      end;
    end;
  Result := IntToStr(count) + #$fe + S;
end;

{-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{The first packet sent by the client after connecting and receiving
the SRV_HELLO packet from the server. The packet basiclly identifies
what kind and version of client is connecting along with the user's
UIN and password.}
procedure CreateCLI_IDENT(Pkt: PRawPkt; UIN: LongWord; Password: String; var Seq: Word);
begin
  PktInit(Pkt, 1, Seq);                         //Channel 1
  PktInt(Pkt, 1, 4);                            //00 00 00 01
  PktTLV(Pkt, 1, IntToStr(UIN));                //Adding user's UIN

  ICQEncryptPassStr(Password);                  //Encrypt password
  PktTLV(Pkt, 2, Password);                     //Adding encrypted password

  // Pack client identification details. We identify ourselves as ICQ 2003b.
  // eraser 13.05.2004
  PktTLV(Pkt, 3, 'ICQ Inc. - Product of ICQ (TM).2003b.5.56.1.3916.85');   //Cookie
  //Uknowns
  PktInt(Pkt, $00160002, 4); PktInt(Pkt, $010a, 2);
  PktInt(Pkt, $00170002, 4); PktInt(Pkt, $0005, 2);
  PktInt(Pkt, $00180002, 4); PktInt(Pkt, $0025, 2);
  PktInt(Pkt, $00190002, 4); PktInt(Pkt, $0001, 2);
  PktInt(Pkt, $001a0002, 4); PktInt(Pkt, $0e90, 2);
  PktInt(Pkt, $00140004, 4); PktInt(Pkt, $00000055, 4);
  PktTLV(Pkt, $000f, 'en');
  PktTLV(Pkt, $000e, 'us');
  PktFinal(Pkt);                                //Finalize packet
end;

{Sent as the first packet after the client has logged in
to the second server and received the SRV_HELLO packet.}
procedure CreateCLI_COOKIE(Pkt: PRawPkt; const Cookie: String; var Seq: Word);
begin
  PktInit(Pkt, 1, Seq);                         //Channel 1
  PktInt(Pkt, 1, 4);                            //00 00 00 01
  PktTLV(Pkt, 6, Cookie);                       //TLV(06) Cookie
  PktFinal(Pkt);                                //Finalize packet
end;

{This packet is a response to SNAC(1,3), SRV_FAMILIES. This tells
the server which SNAC families and their corresponding versions
which the client understands. This also seems to identify the client
as an ICQ vice AIM client to the server.}
procedure CreateCLI_FAMILIES(Pkt: PRawPkt; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, 1, $17, 0, 0);                   //Snac: Type x01/x17, ID x0000, Flags 0
  PktInt(Pkt, $00010003, 4);                    //Family x01 is Version x03
  PktInt(Pkt, $00130002, 4);                    //Family x13 at Version x02
  PktInt(Pkt, $00020001, 4);                    //Family x02 at Version x01
  PktInt(Pkt, $00030001, 4);                    //Family x03 at Version x01
  PktInt(Pkt, $00150001, 4);                    //Family x15 at Version x01
  PktInt(Pkt, $00040001, 4);                    //Family x04 at Version x01
  PktInt(Pkt, $00060001, 4);                    //Family x06 at Version x01
  PktInt(Pkt, $00090001, 4);                    //Family x09 at Version x01
  PktInt(Pkt, $000A0001, 4);                    //Family x0A at Version x01
  PktInt(Pkt, $000B0001, 4);                    //Family x0B at Version x01
  PktFinal(Pkt);                                //Finalize packet
end;

{This packet requests from the server several bits of information most
likely regarding how fast certain packets can be sent to the server and
possibly a maximum packet size as well.}
procedure CreateCLI_RATESREQUEST(Pkt: PRawPkt; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $01, $06, 0, 0);                 //Snac: Type x01/x06, ID x0000, Flags 0
  PktFinal(Pkt);                                //Finalize packet
end;

{This packet is sent in response to the SRV_RATES SNAC(1,7). This
packet contains the same group numbers as was in the SRV_RATES
packet and is an acknowledgement of their receipt.}
procedure CreateCLI_ACKRATES(Pkt: PRawPkt; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $01, $08, 0, 0);                 //Type x01/x08, ID x0000, Flags 0
  PktInt(Pkt, $0001, 2);                        //Group1 - 0x0001
  PktInt(Pkt, $0002, 2);                        //Group2 - 0x0002
  PktInt(Pkt, $0003, 2);                        //Group3 - 0x0003
  PktInt(Pkt, $0004, 2);                        //Group4 - 0x0004
  PktInt(Pkt, $0005, 2);                        //Group5 - 0x0005
  PktFinal(Pkt);                                //Finalize packet
end;

{This command requests from the server certain information
about the client that is stored on the server}
procedure CreateCLI_REQINFO(Pkt: PRawPkt; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $01, $0E, 0, 0);                 //Snac: Type x01/x0E, ID x0000, Flags 0
  PktFinal(Pkt);                                //Finalize packet
end;

{Unknown}
procedure CreateCLI_REQUNKNOWN(Pkt: PRawPkt; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $13, $02, 0, 0);                 //Snac: Type x13/x02, ID x0000, Flags 0
  PktFinal(Pkt);                                //Finalize packet
end;

{This command, like CLI_CHECKROSTER, requests the server side contact list.
The difference between CLI_REQROSTER and CLI_CHECKROSTER is that CLI_REQROSTER
has no parameters, and always causes SRV_REPLYROSTER (rather than
SRV_REPLYROSTEROK). My guess is that CLI_REQROSTER is sent instead of
CLI_CHECKROSTER when the client does not have a cached copy of the contact
list; ie, the first time a user logs in with a particular client.}
procedure CreateCLI_REQROSTER(Pkt: PRawPkt; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $13, $04, $00010004, 0);         //Snac: Type x13/x04, ID x00010004, Flags 0
  PktFinal(Pkt);                                //Finalize packet
end;

{Synchronizes the server side contact list with the client's.
If the passed values match those on the server, SNAC(13,F)
SRV_REPLYROSTEROK will be returned. If the values are older
than what is on the server then SNAC(13,6) SRV_REPLYROSTER will
be returned.}
procedure CreateCLI_CHECKROSTER(Pkt: PRawPkt; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $13, $05, $00010005, 0);         //Snac: Type x13/x05, ID x00010005, Flags 0
  PktInt(Pkt, $3C36D709, 4);                    //time(NULL), The last modification time of the server side contact list.
  PktInt(Pkt, $0000, 2);                        //Size of server side contact list.
  PktFinal(Pkt);                                //Finalize packet
end;

{Request rights information for location service. This is from
the OSCAR document.}
procedure CreateCLI_REQLOCATION(Pkt: PRawPkt; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $02, $02, 0, 0);                 //Snac: Type x02/x02, ID x0000, Flags 0
  PktFinal(Pkt);                                //Finalize packet
end;

{Request rights information for buddy service. This from the OSCAR document.}
procedure CreateCLI_REQBUDDY(Pkt: PRawPkt; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $03, $02, 0, 0);                 //Snac: Type x03/x02, ID x0000, Flags 0
  PktFinal(Pkt);                                //Finalize packet
end;

{Request rights information for ICBM (instant messages) operations. This
from the OSCAR document.}
procedure CreateCLI_REQICBM(Pkt: PRawPkt; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $04, $04, 0, 0);                 //Snac: Type x04/x04, ID x0000, Flags 0
  PktFinal(Pkt);                                //Finalize packet
end;

{Request BOS rights. This from the OSCAR document.}
procedure CreateCLI_REQBOS(Pkt: PRawPkt; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $09, $02, 0, 0);                 //Snac: Type x09/x02, ID x0000, Flags 0
  PktFinal(Pkt);                                //Finalize packet
end;

{This packet sends the client's capabilities information to the server.}
procedure CreateCLI_SETUSERINFO(Pkt: PRawPkt; var Seq: Word);
const
  caps: array[0..$40 - 1] of Byte = (
    $09, $46, $13, $49, $4C, $7F, $11, $D1, $82, $22, $44, $45, $53, $54, $00, $00,
    $97, $B1, $27, $51, $24, $3C, $43, $34, $AD, $22, $D6, $AB, $F7, $3F, $14, $92,
    $2E, $7A, $64, $75, $FA, $DF, $4D, $C8, $88, $6F, $EA, $35, $95, $FD, $B6, $DF,
    $09, $46, $13, $44, $4C, $7F, $11, $D1, $82, $22, $44, $45, $53, $54, $00, $00
  );
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $02, $04, 0, 0);                 //Snac: Type x02/x04, ID x0000, Flags 0
  PktTLV(Pkt, 5, Length(caps), @caps);          //Client's capabilities
  PktFinal(Pkt);                                //Finalize packet
end;

{This packet seems to change some of the values passed from the server
in SRV_REPLYICBM SNAC(4,5).}
procedure CreateCLI_SETICBM(Pkt: PRawPkt; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $04, $02, 0, 0);                 //Snac: Type x04/x02, ID x0000, Flags 0
  PktInt(Pkt, 0, 4);                            //0, Unknown; Numbers similar to x04/x05
  PktInt(Pkt, $0003, 2);                        //3, Unknown
  PktInt(Pkt, $1F40, 2);                        //8000, Unknown
  PktInt(Pkt, $03E7, 2);                        //999, Unknown
  PktInt(Pkt, $03E7, 2);                        //999, Unknown
  PktInt(Pkt, 0, 4);                            //0, Unknown
  PktFinal(Pkt);                                //Finalize packet
end;

{This sets the clients online status code and some other direct client
to client information as well. Used in login sequence.}
procedure CreateCLI_SETSTATUS(Pkt: PRawPkt; Status: LongWord; IP: LongInt; Port: Word; Cookie: LongWord; ProxyType: TProxyType; var Seq: Word);
var
  lpkt: TRawPkt;
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $01, $1E, 0, 0);                 //Snac: Type x01/x1E, ID x0000, Flags 0
  PktTLV(Pkt, $06, 4, Status);                  //TLV(06) Status
  PktTLV(Pkt, $08, 2, 0);                       //TLV(08) Error code
  PktInitRaw(@lpkt);
  //{$R-}
  PktInt(@lpkt, Swap32(IP), 4);                 //The client computer's local IP address.(internal)
  //{$R+}
  PktInt(@lpkt, Port, 4);                       //This is the port to connect with when making client to client connections.
  if ProxyType = P_NONE then
    PktInt(@lpkt, $04, 1)                       //01 = Firewall (or HTTPS proxy); 02 = SOCKS4/5 proxy; 04 = 'normal' connection
  else if (ProxyType = P_SOCKS4) or (ProxyType = P_SOCKS5) then
    PktInt(@lpkt, $02, 1);
  PktInt(@lpkt, ICQ_PROTOCOL_VER, 2);                      //The highest client to client protocol version this client uses.
  PktInt(@lpkt, Cookie, 4);                     //Probably a direct client to client connection cookie.
  PktInt(@lpkt, $0000, 2);                      //0, Unknown
  PktInt(@lpkt, $0050, 2);                      //80, Unknown
  PktInt(@lpkt, $0000, 2);                      //0, Unknown
  PktInt(@lpkt, $0003, 2);                      //Count: 3

  //Theese are used in miranda-icq              // eraser
  //PktInt(@lpkt, $FFFFFFFF, 4);                //time(NULL): Miranda
  //PktInt(@lpkt, $00010201, 4);                //time(NULL): version 0.1.2.1
  //PktInt(@lpkt, $3B7248ED, 4);                //time(NULL): Thu Nov 08 22:49:54 2001

  PktInt(@lpkt, $00000000, 4);                  //time(NULL)
  PktInt(@lpkt, $00000000, 4);                  //time(NULL)
  PktInt(@lpkt, $00000000, 4);                  //time(NULL)
  PktInt(@lpkt, $0000, 2);                      //0, Unknown
  PktTLV(Pkt, $0C, lpkt.len, @lpkt.Data);       //TLV(0C)
  PktFinal(Pkt);                                //Finalize packet
end;

{Set client's online status after login.}
procedure CreateCLI_SETSTATUS_SHORT(Pkt: PRawPkt; Status: LongWord; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $01, $1E, 0, 0);                 //Snac: Type x01/x1E, ID x0000, Flags 0
  PktTLV(Pkt, $06, 4, Status);                  //TLV(06) Status
  PktFinal(Pkt);                                //Finalize packet
end;

{Set idle information. If idle_secs field is 0 then the user
isn't idle at all and the server informs all watching clients
to remove idletime-string in their tooltip of my uin-item.
If idle_secs field is greater then 0 then the user has already
been idle for idle_secs number of seconds. The server will
automatically keep incrementing this number, so do not repeatedly
call with new idle times.}
procedure CreateCLI_SETIDLETIME(Pkt: PRawPkt; const IsIdle: Boolean; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);             //Channel 2
  PktSnac(Pkt, $01, $11, 0, 0);     //Snac: Type x01/x11, ID x0000, Flags 0
  if IsIdle then
    PktInt(Pkt, $0000003C, 4)       //idle_secs field is 60 seconds - Show idle time = YES
  else
    PktInt(Pkt, $00000000, 4);      //idle_secs field is 0 - Show idle time = NO
  PktFinal(Pkt);                    //Finalize packet
end;

{This packet seems to pass the SNAC Families and their versions
along with some unknown other information back to the server.}
procedure CreateCLI_READY(Pkt: PRawPkt; var Seq: Word);
const
  buf: array[0..79] of Byte = (
    $00, $01, $00, $03, $01, $10, $04, $7B,
    $00, $13, $00, $02, $01, $10, $04, $7B,
    $00, $02, $00, $01, $01, $01, $04, $7B,
    $00, $03, $00, $01, $01, $10, $04, $7B,
    $00, $15, $00, $01, $01, $10, $04, $7B,
    $00, $04, $00, $01, $01, $10, $04, $7B,
    $00, $06, $00, $01, $01, $10, $04, $7B,
    $00, $09, $00, $01, $01, $10, $04, $7B,
    $00, $0A, $00, $01, $01, $10, $04, $7B,
    $00, $0B, $00, $01, $01, $10, $04, $7B
  );
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $01, $02, 0, 0);                 //Snac: Type x01/x02, ID x0000, Flags 0
  PktAddArrBuf(Pkt, @buf, SizeOf(buf));         //Number sequence matches SNAC(x01/x17)
  PktFinal(Pkt);                                //Finalize packet
end;

{This packet seems to act as an interface between the AIM OSCAR-based server
and the old original ICQ server database.}
procedure CreateCLI_TOICQSRV(Pkt: PRawPkt; UIN: LongWord; Command: Word; Data: Pointer; DataLen: LongWord; var Seq, Seq2: Word);
var
  lpkt: TRawPkt;
  len: Word;
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  if Seq2 = 2 then
    PktSnac(Pkt, $15, $02, $00010002, 0)        //Snac: Type x15/x02, ID x00010002, Flags 0
  else
    PktSnac(Pkt, $15, $02, $00000000, 0);       //Snac: Type x15/x02, ID x00000000, Flags 0
  PktInitRaw(@lpkt);
  Inc(lpkt.Len, 2);
  PktInt(@lpkt, Swap32(UIN), 4);
  PktInt(@lpkt, Swap16(Command), 2);
  PktInt(@lpkt, Swap16(Seq2), 2);
  PktAddArrBuf(@lpkt, Data, DataLen);
  //Store remaining size
  len := lpkt.Len;
  lpkt.Len := 0;
  PktLInt(@lpkt, len - 2, 2);
  lpkt.Len := len;
  //--
  PktTLV(Pkt, 1, lpkt.Len, @lpkt);
  PktFinal(Pkt);
  Inc(Seq2);
end;

{This is sent at login and when you add a new user to your
contact list. It contains a list of all the uin's in you're
contact list. ****May be repeated multiple times****}
procedure CreateCLI_ADDCONTACT(Pkt: PRawPkt; UIN: String; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $03, $04, 0, 0);                 //Snac: Type x03/x04, ID x0000, Flags 0
  PktLStr(Pkt, UIN);                            //UIN
  PktFinal(Pkt);                                //Finalize packet
end;

procedure CreateCLI_ADDCONTACT_multi(Pkt: PRawPkt; UINs: array of LongWord; var Seq: Word);
var
  temp : integer;
begin
  PktInit(Pkt, 2, Seq); //Channel 2
  PktSnac(Pkt, $03, $04, 0, 0); //Snac: Type x03/x04, ID x0000, Flags 0
  for temp := Low(UINs) to High(Uins) do
    PktLStr(Pkt, inttostr(UINs[temp])); //UIN
  PktFinal(Pkt); //Finalize packet
end;


{Sent to remove contacts from contact list.}
procedure CreateCLI_REMOVECONTACT(Pkt: PRawPkt; UIN: LongWord; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $03, $05, 0, 0);                 //Snac: Type x03/x05, ID x0000, Flags 0
  PktLStr(Pkt, IntToStr(UIN));                  //List of UINs to remove from contact list.
  PktFinal(Pkt);                                //Finalize packet
end;

{Add UINs to your visible list.}
procedure CreateCLI_ADDVISIBLE(Pkt: PRawPkt; UINs: TStrings; var Seq: Word);
var
  i: Word;
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $09, $05, 0, 0);                 //Snac: Type x09/x05, ID x0000, Flags 0
  if UINs.Count > 0 then
    for i := 0 to UINs.Count - 1 do
      PktLStr(Pkt, UINs.Strings[i]);
  PktFinal(Pkt);                                //Finalize packet
end;

{Remove UINs from your visible list.}
procedure CreateCLI_REMVISIBLE(Pkt: PRawPkt; UIN: LongWord; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $09, $06, 0, 0);                 //Snac: Type x09/x05, ID x0000, Flags 0
  PktLStr(Pkt, IntToStr(UIN));                  //The UINs to remove from your invisible list.
  PktFinal(Pkt);                                //Finalize packet
end;

{List of UINs to add to invisible list. ****May be repeated multiple times****}
procedure CreateCLI_ADDINVISIBLE(Pkt: PRawPkt; UINs: TStrings; var Seq: Word);
var
  i: Word;
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $09, $07, 0, 0);                 //Snac: Type x09/x05, ID x0000, Flags 0
  if UINs.Count > 0 then
    for i := 0 to UINs.Count - 1 do
      PktLStr(Pkt, UINs.Strings[i]);
  PktFinal(Pkt);                                //Finalize packet
end;

{Remove UINs from your invisible list...}
procedure CreateCLI_REMINVISIBLE(Pkt: PRawPkt; UIN: LongWord; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $09, $08, 0, 0);                 //Snac: Type x09/x05, ID x0000, Flags 0
  PktLStr(Pkt, IntToStr(UIN));                  //The UINs to remove from your invisible list.
  PktFinal(Pkt);                                //Finalize packet
end;

{Acknowledge the receipt of all offline messages.}
procedure CreateCLI_ACKOFFLINEMSGS(Pkt: PRawPkt; UIN: LongWord; var Seq, Seq2: Word);
begin
  CreateCLI_TOICQSRV(Pkt, UIN, CMD_ACKOFFMSG, nil, 0, Seq, Seq2);
end;

{Send a message.}
procedure CreateCLI_SENDMSG(Pkt: PRawPkt; ITime, IRandom, UIN: LongWord; const Msg: String; var Seq: Word);
var
  lpkt: TRawPkt;
  pmsg: TRawPkt;
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $04, $06, 0, 0);                 //Snac: Type x04/x06, ID x0000, Flags 0
  PktInt(Pkt, ITime, 4);                        //MID
  PktInt(Pkt, IRandom, 4);                      //MID
  PktInt(Pkt, 1, 2);                            //type, 1 - text messages
  PktLStr(Pkt, UIN);                            //The UIN to send the message to.
  PktInitRaw(@lpkt);                            //Allocate packet for incapsulated TLV(02)
  PktTLV(@lpkt, 1281, 1, 1);                    //Unknown: 0x1 = 1.
  PktInitRaw(@pmsg);                            //Allocate packet for incapsulated TLV(257)
  PktInt(@pmsg, 0, 4);                          //Unknown: empty. vICQ uses 00 00 ff ff.
  PktStr(@pmsg, Msg);                           //Finally, the message to send.
  PktTLV(@lpkt, 257, pmsg.Len, @pmsg);          //Add TLV(257)
  PktTLV(Pkt, 2, lpkt.Len, @lpkt);              //Add TLV(2)
  PktTLV(Pkt, 6, 0, 0);                         //Always present empty TLV.
  PktFinal(Pkt);                                //Finalize packet
end;

{Send an URL.}
procedure CreateCLI_SENDURL(Pkt: PRawPkt; ITime, IRandom, MyUIN, UIN: LongWord; const URL, Description: String; var Seq: Word);
var
  lpkt: TRawPkt;
  S: String;
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $04, $06, 0, 0);                 //Snac: Type x04/x06, ID x0000, Flags 0
  PktInt(Pkt, ITime, 4);                        //MID
  PktInt(Pkt, IRandom, 4);                      //MID
  PktInt(Pkt, 4, 2);                            //type, 1 - url etc messages
  PktLStr(Pkt, UIN);                            //The UIN to sent the message to.
  PktInitRaw(@lpkt);                            //Allocate packet for incapsulated TLV(5)
  PktLInt(@lpkt, MyUIN, 4);                     //My UIN.
  PktInt(@lpkt, 4, 1);                          //The message type as in the old protocol.
  PktInt(@lpkt, 0, 1);                          //Unknown flags; possibly the message flags.
  S := Description + #$fe + URL;                //Concatinate: Decription + 0xFE + URL
  PktLNTS(@lpkt, S);                            //Finally the URL to send.
  PktTLV(Pkt, 5, lpkt.Len, @lpkt);              //Add TLV(5)
  PktTLV(Pkt, 6, 0, 0);                         //Always present empty TLV.
  PktFinal(Pkt);                                //Finalize packet
end;

{Grant another user's request for authorization (in response to SRV_AUTH_REQ).}
procedure CreateCLI_AUTHORIZE(Pkt: PRawPkt; UIN: LongWord; Auth: Byte; Reason: String; var Seq: Word);
begin
  if Auth = 1 then Reason := '';
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $13, $1A, $1A, 0);               //Snac: Type x13/x1A, ID x0000, Flags 0
  PktLStr(Pkt, UIN);                            //UIN of the user to authorize.
  PktInt(Pkt, Auth, 1);                         //Decline or authorize the contact add request: 00 - decline, 01 - authorize
  PktInt(Pkt, Length(Reason), 2);               //Length of the following reason; always 0 for authorize.
  PktStr(Pkt, Reason);                          //Reason for declining authorization as ASCII string.
  PktInt(Pkt, 0, 2);                            //Unknown: empty.
  PktFinal(Pkt);                                //Finalize packet
end;

{Request information about another user.}
procedure CreateCLI_METAREQINFO(Pkt: PRawPkt; UIN, dUIN: LongWord; var Seq, Seq2: Word);
var
  lpkt: TRawPkt;
begin
  PktInitRaw(@lpkt);
  PktLInt(@lpkt, $04b2, 2); //CLI_METAREQINFO 0x04d0, ICQ 2002a sends 0xb204
  PktLInt(@lpkt, dUIN, 4);
  CreateCLI_TOICQSRV(Pkt, UIN, $07D0, @lpkt, lpkt.Len, Seq, Seq2);
end;

{Searches user by email.}
procedure CreateCLI_SEARCHBYMAIL(Pkt: PRawPkt; UIN: LongWord; const Email: String; var Seq, Seq2: Word);
var
  lpkt: TRawPkt;
begin
  PktInitRaw(@lpkt);
  PktInt(@lpkt, $7305, 2); //CLI_SEARCHBYMAIL
  PktInt(@lpkt, $5e01, 2); //The key to search for: 0x15e = 350 = email address
  PktLLNTS(@lpkt, Email);  //The email address to search for.
  CreateCLI_TOICQSRV(Pkt, UIN, $07D0, @lpkt, lpkt.Len, Seq, Seq2); //Incapsulate in CLI_TOICQSRV
end;

{Searches user by UIN.}
procedure CreateCLI_SEARCHBYUIN(Pkt: PRawPkt; UIN: LongWord; DUIN: LongWord; var Seq, Seq2: Word);
var
  lpkt: TRawPkt;
begin
  PktInitRaw(@lpkt);
  PktInt(@lpkt, $6905, 2); //CLI_SEARCHBYUIN
  PktInt(@lpkt, $3601, 2); //The key to search for: 0x15e = 350 = email address
  PktInt(@lpkt, $0400, 2); //Length of the following data
  PktInt(@lpkt, Swap32(DUIN), 4); //UIN
  CreateCLI_TOICQSRV(Pkt, UIN, $07D0, @lpkt, lpkt.Len, Seq, Seq2); //Incapsulate in CLI_TOICQSRV
end;

{Search for a user by most common options.}
procedure CreateCLI_SEARCHBYNAME(Pkt: PRawPkt; UIN: LongWord; const FirstName, LastName, NickName, Email: String; var Seq, Seq2: Word);
var
  lpkt: TRawPkt;
begin
  PktInitRaw(@lpkt);
  PktInt(@lpkt, $5F05, 2);      //CLI_SEARCHBYNAME
  if Length(FirstName) <> 0 then
  begin
    PktInt(@lpkt, $4001, 2);    //The key to search for: 0x140 = 320 = first name.
    PktLLNTS(@lpkt, FirstName); //The first name to search for.
  end;
  if Length(LastName) <> 0 then
  begin
    PktInt(@lpkt, $4a01, 2);    //The key to search for: 0x14a = 330 = last name.
    PktLLNTS(@lpkt, LastName);  //The last name to search for.
  end;
  if Length(NickName) <> 0 then
  begin
    PktInt(@lpkt, $5401, 2);    //The key to search for: 0x154 = 340 = nick.
    PktLLNTS(@lpkt, NickName);  //The nick name to search for.
  end;
  if Length(Email) <> 0 then
  begin
    PktInt(@lpkt, $5e01, 2);    //The key to search for: 0x15e = 350 = email address.
    PktLLNTS(@lpkt, Email);     //The email address to search for.
  end;
  CreateCLI_TOICQSRV(Pkt, UIN, $07D0, @lpkt, lpkt.Len, Seq, Seq2); //Incapsulate in CLI_TOICQSRV
end;

{Ask for a random UIN from a user in given chat group.}
procedure CreateCLI_SEARCHRANDOM(Pkt: PRawPkt; UIN: LongWord; Group: Word; var Seq, Seq2: Word);
var
  lpkt: TRawPkt;
begin
  PktInitRaw(@lpkt);
  PktInt(@lpkt, $4E07, 2); //CLI_SEARCHRANDOM Channel: 2, SNAC(21,2) 2000/1870
  PktInt(@lpkt, Swap16(Group), 2); //The random chat group to request a UIN from.
  CreateCLI_TOICQSRV(Pkt, UIN, $07D0, @lpkt, lpkt.Len, Seq, Seq2); //Incapsulate in CLI_TOICQSRV
end;

{Do an extensive search for a user.}
procedure CreateCLI_SEARCHWP(Pkt: PRawPkt; UIN: LongWord; First, Last, Nick, Email: String; MinAge, MaxAge: Word;
  Gender: Byte; Language: Byte; City: String; Country: Word; Company, Department, Position: String;
  Occupation: Byte; Organization: Word; OrganKeyWords: String; Affiliation: Word; AffiKeyWords,
  KeyWord: String; Online: Byte; var Seq, Seq2: Word);
var
  lpkt: TRawPkt;
begin
  if (Gender <> GEN_MALE) and (Gender <> GEN_FEMALE) then
    Gender := 0;                        //Don't care about gender
  PktInitRaw(@lpkt);                    //Initialize temporary packet
  PktInt(@lpkt, $5F05, 2);              //CLI_SEARCHBYPERSINF, Channel: 2, SNAC(21,2) 2000/1375
  if First <> '' then begin PktInt(@lpkt, $4001, 2); PktLLNTS(@lpkt, First); end;                                       //Fist Name
  if Last <> '' then begin PktInt(@lpkt, $4a01, 2); PktLLNTS(@lpkt, Last); end;                                         //Last Name
  if Nick <> '' then begin PktInt(@lpkt, $5401, 2); PktLLNTS(@lpkt, Nick); end;                                         //Nick Name
  if Email <> '' then begin PktInt(@lpkt, $5e01, 2); PktLLNTS(@lpkt, Email); end;                                       //Email
  if Gender <> 0 then begin PktInt(@lpkt, $7c01, 2); PktInt(@lpkt, $0100, 2); PktInt(@lpkt, Gender, 1);  end;           //Gender
  if (MinAge <> 0) and (MaxAge <> 0) then begin PktInt(@lpkt, $6801, 2); PktInt(@lpkt, $0400, 2); PktLInt(@lpkt, MinAge, 2); PktLInt(@lpkt, MaxAge, 2); end;      //Age
  if City <> '' then begin PktInt(@lpkt, $9001, 2); PktLLNTS(@lpkt, City); end;                                         //City/State
  if Country <> 0 then begin PktInt(@lpkt, $a401, 2); PktInt(@lpkt, $0200, 2); PktLInt(@lpkt, Country, 2); end;         //Country
  if Language <> 0 then begin PktInt(@lpkt, $8601, 2); PktInt(@lpkt, $0200, 2); PktLInt(@lpkt, Language, 2); end;       //Language
  if Company <> '' then begin PktInt(@lpkt, $ae01, 2); PktLLNTS(@lpkt, Company); end;                                   //Company
  if Department <> '' then begin PktInt(@lpkt, $b801, 2); PktLLNTS(@lpkt, Department); end;                             //Department
  if Occupation <> 0 then begin PktInt(@lpkt, $cc01, 2); PktInt(@lpkt, $0200, 2); PktLInt(@lpkt, Occupation, 2); end;   //Occupation
  if Position <> '' then begin PktInt(@lpkt, $c201, 2); PktLLNTS(@lpkt, Position); end;
  if Affiliation <> 0 then begin PktInt(@lpkt, $d601, 2); PktInt(@lpkt, 3 + Length(AffiKeyWords), 2); PktLInt(@lpkt, Affiliation, 2); PktLNTS(@lpkt, AffiKeyWords); end;     //Affiliation
  if Organization <> 0 then begin PktInt(@lpkt, $fe01, 2); PktInt(@lpkt, 3 + Length(OrganKeyWords), 2); PktLInt(@lpkt, Organization, 2); PktLNTS(@lpkt, OrganKeyWords); end; //Organization
  if KeyWord <> '' then begin PktInt(@lpkt, $2602, 2); PktLLNTS(@lpkt, KeyWord); end;                                   //Search by KeyWord
  if Online = 1 then begin PktInt(@lpkt, $3002, 2); PktInt(@lpkt, $0100, 2); PktInt(@lpkt, $01, 1); end;                //Search only online users
  CreateCLI_TOICQSRV(Pkt, UIN, $07D0, @lpkt, lpkt.Len, Seq, Seq2); //Incapsulate in CLI_TOICQSRV
end;

{Set more information about yourself.}
procedure CreateCLI_METASETMORE(Pkt: PRawPkt; UIN: LongWord; Age: Word; Gender: Byte; HomePage: String; BirthYear: Word; BirthMonth, BirthDay, Lang1, Lang2, Lang3: Byte; var Seq, Seq2: Word);
var
  lpkt: TRawPkt;
begin
  if (Gender <> GEN_MALE) and (Gender <> GEN_FEMALE) then
    Gender := 0;                //Gender not specified.
  PktInitRaw(@lpkt);            //Initialize packet
  PktInt(@lpkt, $FD03, 2);      //CLI_METASETMORE Channel: 2, SNAC(21,2) 2000/1021
  PktLInt(@lpkt, Age, 2);       //Your age.
  PktInt(@lpkt, Gender, 1);     //Your gender.
  PktLNTS(@lpkt, HomePage);     //Your personal home page.
  PktLInt(@lpkt, BirthYear, 2); //Your year of birth.
  PktInt(@lpkt, BirthMonth, 1); //Your month of birth.
  PktInt(@lpkt, BirthDay, 1);   //Your day of birth.
  PktInt(@lpkt, Lang1, 1);      //Your first language. Numbers according to a table.
  PktInt(@lpkt, Lang2, 1);      //Your second language. Numbers according to a table.
  PktInt(@lpkt, Lang3, 1);      //Your third language. Numbers according to a table.
  CreateCLI_TOICQSRV(Pkt, UIN, $07D0, @lpkt, lpkt.Len, Seq, Seq2); //Incapsulate in CLI_TOICQSRV
end;

{Set general information about yourself.}
procedure CreateCLI_METASETGENERAL(Pkt: PRawPkt; UIN: LongWord; const NickName, FirstName, LastName, Email, City, State, Phone, Fax, Street, Cellular, Zip: String; Country: Word; TimeZone: Byte; PublishEmail: Boolean; var Seq, Seq2: Word);
var
  lpkt: TRawPkt;
begin
  PktInitRaw(@lpkt);
  PktInt(@lpkt, $EA03, 2);      //CLI_METASETGENERAL Channel: 2, SNAC(21,2) 2000/1002
  PktLNTS(@lpkt, NickName);     //The nick of the user.
  PktLNTS(@lpkt, FirstName);    //The first name of the user.
  PktLNTS(@lpkt, LastName);     //The last name of the user.
  PktLNTS(@lpkt, Email);        //The email address of the user.
  PktLNTS(@lpkt, City);         //The city the user lives in.
  PktLNTS(@lpkt, State);        //The state the user lives in.
  PktLNTS(@lpkt, Phone);        //The phone number of the user.
  PktLNTS(@lpkt, Fax);          //The fax number of the user.
  PktLNTS(@lpkt, Street);       //The street the user lives in.
  PktLNTS(@lpkt, Cellular);     //The cell phone number of the user.
  PktLNTS(@lpkt, Zip);          //The zip code of the user.
  PktLInt(@lpkt, Country, 2);   //The country the user lives in according to a table.
  PktInt(@lpkt, TimeZone, 1);   //The timezone the user lives in, as multiples of 30minutes relative to UTC.
  PktInt(@lpkt, Ord(PublishEmail), 1); //Publush email: 1 = yes, 0 = no. 
  CreateCLI_TOICQSRV(Pkt, UIN, $07D0, @lpkt, lpkt.Len, Seq, Seq2); //Incapsulate in CLI_TOICQSRV
end;

{Set the about string.}
procedure CreateCLI_METASETABOUT(Pkt: PRawPkt; UIN: LongWord; const About: String; var Seq, Seq2: Word);
var
  lpkt: TRawPkt;
begin
  PktInitRaw(@lpkt);
  PktInt(@lpkt, $0604, 2);      //CLI_METASETABOUT Channel: 2, SNAC(21,2) 2000/1030
  PktLNTS(@lpkt, About);        //The about information string to set.
  CreateCLI_TOICQSRV(Pkt, UIN, $07D0, @lpkt, lpkt.Len, Seq, Seq2); //Incapsulate in CLI_TOICQSRV
end;

{Send SMS message.}
procedure CreateCLI_SENDSMS(Pkt: PRawPkt; UIN: LongWord; const Destination, Text: String; CodePage: Word; const Time: String; var Seq, Seq2: Word);
var
  lpkt: TRawPkt;
  S: String;
begin
  PktInitRaw(@lpkt);
  PktInt(@lpkt, $8214, 2);      //CLI_SENDSMS Channel: 2, SNAC(21,2) 2000/5250
  PktInt(@lpkt, $0001, 2);      //Unknown: 0x1 = 1.
  PktInt(@lpkt, $0016, 2);      //Unknown: 0x16 = 22.
  PktInt(@lpkt, 0, 4);
  PktInt(@lpkt, 0, 4);
  PktInt(@lpkt, 0, 4);
  PktInt(@lpkt, 0, 4);
  //Format message
  S := '<icq_sms_message>' +
       '<destination>' + Destination + '</destination>' +
       '<text>' + StrToUTF8(Text) + '</text>' +
       '<codepage>' + IntToStr(CodePage) + '</codepage>' +
       '<senders_UIN>' + IntToStr(UIN) + '</senders_UIN>' +
       '<senders_name>TICQClient</senders_name>' +
       '<delivery_receipt>Yes</delivery_receipt>' +
       '<time>' + Time + '</time>' +
       '</icq_sms_message>' + #0;             // *eraser 7.04.2004
  PktTLV(@lpkt, 0, S);          //The message as a XML entity
  CreateCLI_TOICQSRV(Pkt, UIN, $07D0, @lpkt, lpkt.Len, Seq, Seq2); //Incapsulate in CLI_TOICQSRV
end;

{Sends a messaged/request with advanced options}
procedure CreateCLI_SENDADVMSG_CUSTOM(Pkt: PRawPkt; Status: LongWord; FFSeq: Word;
  ITime, IRandom, UIN: LongWord; CMD, MSGTYPE: Byte; ACMD: Word;
  const Msg: String; Data: Pointer; DataLen: LongWord; RTFFormat: Boolean;
  var Seq: Word; Accept: Boolean = True);
var
  lpTLV05: TRawPkt;
  lpTLV2711: TRawPkt;
const
  StrGuid: String = '{97B12751-243C-4334-AD22-D6ABF73F1492}';
  Capabilities: array[0..15] of Byte = ($09, $46, $13, $49, $4C, $7F, $11, $D1,
                                        $82, $22, $44, $45, $53, $54, $00, $00);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $04, $06, 0, 0);                 //Snac: Type x04/x06, ID x0000, Flags 0

  // testing * eraser 20.04.2003
  ITime := GetTickCount;
  //

  PktLInt(Pkt, ITime, 4);                       //Seems to be a time stamp in 1/1500 sec since 8am of that Sunday.
  PktInt(Pkt, IRandom, 2);                      //A seemingly random ID generated for each message.
  PktInt(Pkt, $00000002, 4);                    //The message type used
  PktLStr(Pkt, UIN);                            //Destination UIN.

  PktInitRaw(@lpTLV2711);                       //TLV(2711)
  PktInt(@lpTLV2711, $1b, 1);                   //If this value is not present, this is not a message packet.
  PktInt(@lpTLV2711, ICQ_PROTOCOL_VER, 2);                 //This is the version of the TCP protocol that the sending client uses.
  PktInt(@lpTLV2711, $00, 1);                   //Unknown
  PktInt(@lpTLV2711, $00000000, 4);             //Caps, empty
  PktInt(@lpTLV2711, $00000000, 4);             //Caps, empty
  PktInt(@lpTLV2711, $00000000, 4);             //Caps, empty
  PktInt(@lpTLV2711, $00000000, 4);             //Caps, empty
  PktInt(@lpTLV2711, $0000, 2);                 //Unknown
  PktInt(@lpTLV2711, $03, 1);                   //Unknown
  PktInt(@lpTLV2711, $00000000, 4);             //0 = normal message, 4 = file ok or file request.
  PktInt(@lpTLV2711, FFSeq, 2);                 //SEQ1
  PktInt(@lpTLV2711, $0e00, 2);                 //Unknown, seen: 0x1200 and 0x0e00.
  PktInt(@lpTLV2711, FFSeq, 2);                 //SEQ1
  PktInt(@lpTLV2711, $00000000, 4);             //Unknown, always zero.
  PktInt(@lpTLV2711, $00000000, 4);             //Unknown, always zero.
  PktInt(@lpTLV2711, $00000000, 4);             //Unknown, always zero.
  PktInt(@lpTLV2711, CMD, 1);                   //0x01 - normal message, 0x1a - filerequest/response, contacts
  PktInt(@lpTLV2711, MSGTYPE, 1);               //00 - normal message 80 - multiple recipients 03 - auto reply message request.
  // * eraser 16.05.2004
  PktLInt(@lpTLV2711, WORD(StatusToInt(Status)), 2); //word (LE) status code

  if MSGTYPE = $03 then
    PktInt(@lpTLV2711, $0100, 2)                //seen 0x0100 in information request messages.
  else
    PktInt(@lpTLV2711, $0000, 2);               //Usually 0, seen 0x0002 in information request messages.
  //
  if Length(Msg) = 0 then
  begin
    PktLInt(@lpTLV2711, $0001, 2);              //Length of the message
    PktInt(@lpTLV2711, $00, 1);                 //The message should always have null terminator, event if it's empty
  end else
    PktLNTS(@lpTLV2711, Msg);                   //The message!

  if CMD = $01 then
  begin
    PktInt(@lpTLV2711, $00000000, 4);           //Only present in actual real messages, this will be the background color of the text box in RGB0 format.
    PktInt(@lpTLV2711, $FFFFFF00, 4);           //Only present in actual real messages, this will be the text color of the message in RGB0 format.
  end else
    PktAddArrBuf(@lpTLV2711, Data, DataLen);    //Add packet specific data

  if RTFFormat then                             //If we are using RTF format, then add GUID
  begin
    PktLInt(@lpTLV2711, Length(StrGuid), 4);    //This is a little-endian string length of the following GUID. This is only present in real messages sent by the latest 2001b client build 3659.
    PktStr(@lpTLV2711, StrGuid);                //This GUID seems to indicate that the client is capable of handling Multibyte Wide Character Strings as messages. Only present in real messages sent by build 3659 2001b clients.
  end;

  PktInitRaw(@lpTLV05);                         //TLV(05)
  PktInt(@lpTLV05, $0000, 2);                   //0x0000 - normal message
  PktInt(@lpTLV05, ITime, 4);                   //Seems to be a time stamp in 1/1500 sec since 8am of that Sunday.
  PktInt(@lpTLV05, IRandom, 2);                 //A seemingly random ID generated for each message.
  PktInt(@lpTLV05, $0000, 2);                   //Unknown: 0.
  PktAddArrBuf(@lpTLV05, @Capabilities, 16);    //One of the capabilities sent in CLI_SETUSERINFO
  PktTLV(@lpTLV05, $000a, 2, ACMD);             //0x0001 - normal message 0x0002 - file ack or file ok
  PktTLV(@lpTLV05, $000f, 0, $00);              //Unknown, empty.
  //PktTLV(@lpTLV05, $0003, $0004, $c0a86516);    //Timestamp
  //PktTLV(@lpTLV05, $0005, $0002, $5d3c);        //Unknown

  PktTLV(@lpTLV05, $2711, lpTLV2711.Len, @lpTLV2711); //Incapsulate TLV2711 into TLV05

  PktTLV(Pkt, $05, lpTLV05.Len, @lpTLV05);      //Incapsulate TLV05 into Pkt

  PktTLV(Pkt, $0003, 0, 0);                     //Unknown, empty TLV(03)
  PktFinal(Pkt);                                //Finalize packet
end;

{Sends a messaged with advanced options}
procedure CreateCLI_SENDMSG_ADVANCED(Pkt: PRawPkt; Status: LongWord; ITime, IRandom, UIN: LongWord; const Msg: String; RTFFormat: Boolean; var Seq: Word);
begin
  CreateCLI_SENDADVMSG_CUSTOM(Pkt, Status, $FFFF, ITime, IRandom, UIN, $01, $00, $0001, Msg, nil, 0, RTFFormat, Seq, True);
end;

{Sends an ack  with advanced options}
procedure CreateCLI_SENDMSG_FILEACK(Pkt: PRawPkt; Status: LongWord; FFSeq: Word; ITime, IRandom, UIN, FileSize: LongWord; const FileDesc, FileName: String; Port: Word; var Seq: Word);
var
  Data: TRawPkt;
  Pkt1: TRawPkt;
const
  TextCMD = 'File Transfer';
begin
  PktInitRaw(@Data);                            //Additional data packet
  PktInitRaw(@Pkt1);                            //First temporary packet
  PktAddArrBuf(@Pkt1, @FileSignature, 16);      //File signature
  PktInt(@Pkt1, $0000, 2);                      //Unknown: empty
  PktDWStr(@Pkt1, TextCMD);                     //Text command
  PktInt(@Pkt1, $00000101, 4);                  //Unknown
  PktInt(@Pkt1, $00000000, 4);                  //Unknown
  PktInt(@Pkt1, $00000000, 4);                  //Unknown
  PktInt(@Pkt1, $0000, 2);                      //Unknown
  PktInt(@Pkt1, $00, 1);                        //Unknown
  PktLInt(@Data, Pkt1.Len, 2);                  //Length of the first temp packet
  PktAddArrBuf(@Data, @Pkt1, Pkt1.Len);         //Implode packets
  PktInitRaw(@Pkt1);                            //Second additional packet
  PktDWStr(@Pkt1, FileDesc);                    //File description
  PktInt(@Pkt1, Port, 2);                       //Listeners port
  PktInt(@Pkt1, $0000, 2);                      //Unknown
  PktLNTS(@Pkt1, FileName);                     //Filename
  PktLInt(@Pkt1, FileSize, 4);                  //Filesize
  PktLInt(@Pkt1, Port, 4);                      //Listeners port again
  PktLInt(@Data, Pkt1.Len, 4);                  //Length of the second temp packet
  PktAddArrBuf(@Data, @Pkt1, Pkt1.Len);         //Implode packets
  CreateCLI_SENDADVMSG_CUSTOM(Pkt, Status, FFSeq, ITime, IRandom, UIN, $1a, $00, $0002, '', @Data, Data.Len, False, Seq);
end;

{Request an auto-away message.}
procedure CreateCLI_REQAWAYMSG(Pkt: PRawPkt; FStatus: LongWord; ITime, IRandom, UIN: LongWord; Status: Byte; var Seq: Word);
begin
  CreateCLI_SENDADVMSG_CUSTOM(Pkt, FStatus, $FFFF, ITime, IRandom, UIN, Status, $03, $0001, '', nil, 0, False, Seq);
end;

{Send contacts through server.}
procedure CreateCLI_SENDCONTACTS(Pkt: PRawPkt; Status: LongWord; ITime, IRandom, UIN: LongWord; Contacts: TStringList; var Seq: Word);
var
  lpData: TRawPkt;
  Pkt1: TRawPkt;
  S: String;
const
  TextCMD = 'Contacts';
begin
  PktInitRaw(@lpData);                          //Init data packet
  PktInitRaw(@Pkt1);                            //Init first temporary packet
  PktAddArrBuf(@Pkt1, @ContactsSignature, 16);  //Contacs signature
  PktInt(@Pkt1, $0000, 2);                      //0x0000 - Send contacts
  PktDWStr(@Pkt1, TextCMD);                     //Text command
  PktInt(@Pkt1, $00000000, 4);                  //Unknown
  PktInt(@Pkt1, $00010000, 4);                  //Unknown
  PktInt(@Pkt1, $00000000, 4);                  //Unknown
  PktInt(@Pkt1, $0000, 2);                      //Unknown
  PktInt(@Pkt1, $00, 1);                        //Unknown
  PktLInt(@lpData, Pkt1.Len, 2);                //Length of the following packet (Pkt1)
  PktAddArrBuf(@lpData, @Pkt1, Pkt1.Len);       //Implode packets
  S := MakeContactsStr(Contacts);               //Create text list from string list
  PktLInt(@lpData, Length(S) + 4, 4);           //Length of the following data
  PktDWStr(@lpData, S);                         //Length of the following string
  CreateCLI_SENDADVMSG_CUSTOM(Pkt, Status, $FFFF, ITime, IRandom, UIN, $1a, $00, $0001, '', @lpData, lpData.Len, False, Seq);
end;

{Send contacts request through server.}
procedure CreateCLI_SENDCONTACTS_REQ(Pkt: PRawPkt; Status: LongWord; ITime, IRandom, UIN: LongWord; Reason: String; var Seq: Word);
var
  lpData: TRawPkt;
  Pkt1: TRawPkt;
const
  TextCMD = 'Request For Contacts';
begin
  PktInitRaw(@lpData);                          //Init data packet
  PktInitRaw(@Pkt1);                            //Init first temporary packet
  PktAddArrBuf(@Pkt1, @ContactsSignature, 16);  //Contacs signature
  PktInt(@Pkt1, $0200, 2);                      //0x0200 - Request for contacts
  PktDWStr(@Pkt1, TextCMD);                     //Text command
  PktInt(@Pkt1, $00000000, 4);                  //Unknown
  PktInt(@Pkt1, $00010000, 4);                  //Unknown
  PktInt(@Pkt1, $00000000, 4);                  //Unknown
  PktInt(@Pkt1, $0000, 2);                      //Unknown
  PktInt(@Pkt1, $00, 1);                        //Unknown
  PktLInt(@lpData, Pkt1.Len, 2);                //Length of the following packet (Pkt1)
  PktAddArrBuf(@lpData, @Pkt1, Pkt1.Len);       //Implode packets
  PktLInt(@lpData, Length(Reason) + 4, 4);      //Length of the following data
  PktDWStr(@lpData, Reason);                    //Length of the following string
  CreateCLI_SENDADVMSG_CUSTOM(Pkt, Status, $FFFF, ITime, IRandom, UIN, $1a, $00, $0001, '', @lpData, lpData.Len, False, Seq);
end;

{Create a FILE_ACK showing that user declined file.}
procedure CreateCLI_SENDMSG_FILEDECLINE(Pkt: PRawPkt; FFSeq: Word; ITime, IRandom, UIN, FileSize: LongWord; const FileDesc, FileName, Reason: String; Port: Word; var Seq: Word);
var
  Pkt1: TRawPkt;
const
  TextCMD = 'File Transfer';
begin
  PktInit(Pkt, 2, Seq);
  PktSnac(Pkt, $04, $0b, $00000000, $0000);
  PktInt(Pkt, ITime, 4);                        //Time
  PktInt(Pkt, IRandom, 2);                      //RandomID
  PktInt(Pkt, $0000, 2);                        //Unknown
  PktInt(Pkt, $0002, 2);                        //Message type
  PktLStr(Pkt, UIN);                            //Destination UIN
  PktInt(Pkt, $0003, 2);                        //Unknown
  PktInt(Pkt, $1b00, 2);                        //If this value is not present, this is not a message packet.
  PktInt(Pkt, $0800, 2);                        //TCP version
  PktInt(Pkt, $00000000, 4);                    //Unknown: empty
  PktInt(Pkt, $00000000, 4);                    //Unknown: empty
  PktInt(Pkt, $00000000, 4);                    //Unknown: empty
  PktInt(Pkt, $00000000, 4);                    //Unknown: empty
  PktInt(Pkt, $0000, 2);                        //Unknown: empty
  PktInt(Pkt, $03, 1);                          //Unknown: 0x03
  PktInt(Pkt, $00000000, 4);                    //Unknown: empty
  PktInt(Pkt, FFSeq, 2);                        //SEQ1
  PktInt(Pkt, $0e00, 2);                        //Unknown, seen: 0x1200 and 0x0e00.
  PktInt(Pkt, FFSeq, 2);                        //SEQ1
  PktInt(Pkt, $00000000, 4);                    //Capability: empty
  PktInt(Pkt, $00000000, 4);                    //Capability: empty
  PktInt(Pkt, $00000000, 4);                    //Capability: empty
  PktInt(Pkt, $1a00, 2);                        //SUBCMD
  PktInt(Pkt, $01000000, 4);                    //Unknown
  if Length(Reason) = 0 then                    //Use null terminator as a message, even when reason is empty
  begin
    PktInt(Pkt, $0100, 2);
    PktInt(Pkt, $00, 1);
  end else
    PktLNTS(Pkt, Reason);                       //Reason
  PktInitRaw(@Pkt1);                            //Initialize raw packet
  PktAddArrBuf(@Pkt1, @FileSignature, 16);      //File signature
  PktInt(@Pkt1, $0000, 2);                      //Unknown: empty
  PktDWStr(@Pkt1, TextCMD);                     //Text command
  PktInt(@Pkt1, $00000101, 4);                  //Unknown
  PktInt(@Pkt1, $00000000, 4);                  //Unknown
  PktInt(@Pkt1, $00000000, 4);                  //Unknown
  PktInt(@Pkt1, $0000, 2);                      //Unknown
  PktInt(@Pkt1, $00, 1);                        //Unknown
  PktLInt(Pkt, Pkt1.Len, 2);                    //Length of the first temp packet
  PktAddArrBuf(Pkt, @Pkt1, Pkt1.Len);           //Implode packets
  PktInitRaw(@Pkt1);                            //Second additional packet
  PktDWStr(@Pkt1, FileDesc);                    //File description
  PktInt(@Pkt1, Port, 2);                       //Listeners port
  PktInt(@Pkt1, $0000, 2);                      //Unknown
  PktLNTS(@Pkt1, FileName);                     //Filename
  PktLInt(@Pkt1, FileSize, 4);                  //Filesize
  PktLInt(@Pkt1, Port, 4);                      //Listeners port again
  PktLInt(Pkt, Pkt1.Len, 4);                    //Length of the second temp packet
  PktAddArrBuf(Pkt, @Pkt1, Pkt1.Len);           //Implode packets
  PktFinal(Pkt);                                //Finalize packet
end;

{Sends CLI_HELLO, used in registering the new UIN}
procedure CreateCLI_HELLO(Pkt: PRawPkt; var Seq: Word);
begin
  PktInit(Pkt, 1, Seq);                         //Channel 2
  PktInt(Pkt, $00000001, 4);                    //Always sent as the first parameter of a Channel 1 packet.
  PktFinal(Pkt);                                //Finalize packet
end;

{Sends CLI_HELLO, used in unregistering the existing UIN}
procedure CreateCLI_GOODBYE(Pkt: PRawPkt; var Seq: Word);
begin
  PktInit(Pkt, 1, Seq);                         //Channel 2
  PktFinal(Pkt);                                //Finalize packet
end;

{Register a new UIN.}
procedure CreateCLI_REGISTERUSER(Pkt: PRawPkt; const Password: String; var Seq: Word);
var
  lpTLV01: TRawPkt;
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $17, $04, 0, 0);                 //Snac: Type x17/x04, ID x0000, Flags 0

  PktInitRaw(@lpTLV01);                         //TLV(01), - this TLV contains all information needed to request a new UIN.
  PktInt(@lpTLV01, $00000000, 4);               //Unknown: empty.
  PktInt(@lpTLV01, $28000300, 4);               //Unknown.
  PktInt(@lpTLV01, $00000000, 4);               //Unknown: empty.
  PktInt(@lpTLV01, $00000000, 4);               //Unknown: empty.
  PktInt(@lpTLV01, $9E270000, 4);               //Unknown. Seen: 03 46 00 00, B4 25 00 00, 9E 27 00 00.
  PktInt(@lpTLV01, $9E270000, 4);               //Same UNKNOWN2 as above.
  PktInt(@lpTLV01, $00000000, 4);               //Unknown: empty.
  PktInt(@lpTLV01, $00000000, 4);               //Unknown: empty.
  PktInt(@lpTLV01, $00000000, 4);               //Unknown: empty.
  PktInt(@lpTLV01, $00000000, 4);               //Unknown: empty.
  PktLNTS(@lpTLV01, Password);                  //The password to use with your new UIN.
  PktInt(@lpTLV01, $9E270000, 4);               //The same UNKNOWN2 again.
  PktInt(@lpTLV01, $0000, 2);                   //Unknown: empty.
  PktInt(@lpTLV01, $0302, 2);                   //Unknown. Seen: CF 01, 03 02.

  PktTLV(Pkt, $01, lpTLV01.Len, @lpTLV01);      //Incapsulate TLV01 into Pkt
  PktFinal(Pkt);                                //Finalize packet
end;

{Unregister an UIN number.}
procedure CreateCLI_UNREGUIN(Pkt: PRawPkt; UIN: LongWord; const Password: String; var Seq, Seq2: Word);
var
  lpkt: TRawPkt;
begin
  PktInitRaw(@lpkt);
  PktInt(@lpkt, $c404, 2);                      //CLI_METAUNREG Channel: 2, SNAC(21,2) 2000/1220
  PktLInt(@lpkt, UIN, 4);                       //User's UIN
  PktLNTS(@lpkt, Password);                     //User's Password
  CreateCLI_TOICQSRV(Pkt, UIN, $07D0, @lpkt, lpkt.Len, Seq, Seq2); //Incapsulate in CLI_TOICQSRV
end;

{Change user's password.}
procedure CreateCLI_METASETPASS(Pkt: PRawPkt; UIN: LongWord; const Password: String; Buffer: Pointer; BufLen: Word; var Seq, Seq2: Word);
var
  lpkt: TRawPkt;
begin
  PktInitRaw(@lpkt);
  PktInt(@lpkt, $2e04, 2);                      //CLI_METASETPASS Channel: 2, SNAC(21,2) 2000/1070
  if Buffer <> nil then
  begin
    if BufLen > 0 then
    begin
      PktLInt(@lpkt, BufLen + 1, 2);
      PktAddArrBuf(@lpkt, Buffer, BufLen);
      PktInt(@lpkt, $00, 1);
    end else
      PktLInt(@lpkt, $0000, 2);
  end else
    PktLNTS(@lpkt, Password);                   //User's Password
  CreateCLI_TOICQSRV(Pkt, UIN, $07D0, @lpkt, lpkt.Len, Seq, Seq2); //Incapsulate in CLI_TOICQSRV
end;

{Set permissions.}
procedure CreateCLI_METASETPERMISSIONS(Pkt: PRawPkt; UIN: LongWord; AuthorizationRequired, WebAware: Boolean; var Seq, Seq2: Word);
var
  lpkt: TRawPkt;
begin
  PktInitRaw(@lpkt);
  PktInt(@lpkt, $2404, 2);                      //CLI_METASETPERMISSION Channel: 2, SNAC(21,2) 2000/1060
  PktInt(@lpkt, Ord(not AuthorizationRequired), 1); //Authorization required?
  PktInt(@lpkt, Ord(WebAware), 1);              //Webaware?
  PktInt(@lpkt, $0100, 2);                      //Unknown: 01 00
  CreateCLI_TOICQSRV(Pkt, UIN, $07D0, @lpkt, lpkt.Len, Seq, Seq2); //Incapsulate in CLI_TOICQSRV
end;

procedure CreateCLI_METAREQINFO_SHORT(Pkt: PRawPkt; UIN, DestUIN: LongWord; var Seq, Seq2: Word);
var
  lpkt: TRawPkt;
begin
  PktInitRaw(@lpkt);
  PktInt(@lpkt, $BA04, 2);                      //CLI_METAREQINFO_SHORT Channel: 2, SNAC(21,2) 2000/1210
  PktLInt(@lpkt, DestUIN, 4);
  CreateCLI_TOICQSRV(Pkt, UIN, $07D0, @lpkt, lpkt.Len, Seq, Seq2); //Incapsulate in CLI_TOICQSRV
end;

{Request authorization from another user so we can add them to our contact list.}
procedure CreateCLI_REQAUTH(Pkt: PRawPkt; UIN: LongWord; Msg: String; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $13, $18, $18, 0);               //SNAC: 0x13/0x18, Ref 0x00000018, Flags 0
  PktLStr(Pkt, UIN);                            //The UIN of the user authorization is requested from.
  PktWStr(Pkt, StrToUtf8(Msg));                 //Message sent to user in the authorization request.
  PktInt(Pkt, $0000, 2);                        //Unknown: empty.
  PktFinal(Pkt);                                //Finalize packet.
end;

{Keep alive packet.}
procedure CreateCLI_KEEPALIVE(Pkt: PRawPkt; var Seq: Word);
begin
  PktInit(Pkt, 5, Seq);                         //Channel 5
  PktFinal(Pkt);                                //Finalize packet
end;

{This SNAC is sent just before CLI_ADDBUDDY when adding a new contact to the
contact list. This SNAC is NOT sent when adding a UIN to the Ignore list. A
CLI_ADDEND when finished modifying the server side contact list.}
procedure CreateCLI_ADDSTART(Pkt: PRawPkt; FirstUpload: Boolean; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $13, $11, $00000011, 0);         //SNAC: 0x13/0x18, Ref 0x00000011, Flags 0
  if FirstUpload then
    PktInt(Pkt, $00010000, 4);                  //Add 0x00010000 value when uploading w/o authorization
  PktFinal(Pkt);                                //Finalize packet
end;

{This SNAC is sent to tell the server that modifications to the server side contact
list are finished.}
procedure CreateCLI_ADDEND(Pkt: PRawPkt; var Seq: Word);
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $13, $12, $00000012, 0);         //SNAC: 0x13/0x18, Ref 0x00000012, Flags 0
  PktFinal(Pkt);                                //Finalize packet
end;

{This SNAC contains a single header group as described in SRV_REPLYROSTER. Sent
when a user is added to the contact list and updates the server side contact list.}
procedure CreateCLI_UPDATEGROUP(Pkt: PRawPkt; Name: String; Tag: Word; IDs: TStringList; var Seq: Word);
var
  TLVC8: TRawPkt;
  i: Word;
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $13, $09, 0, 0);                 //SNAC: 0x13/0x09, Ref 0x00000000, Flags 0
  {Create temporary array with group values}
  PktInitRaw(@TLVC8);
  if IDs.Count > 0 then
    for i := 0 to IDs.Count - 1 do
      PktInt(@TLVC8, StrToInt(IDs.Strings[i]), 2);
  PktWStr(Pkt, StrToUTF8(Name));                //The name of this group.
  PktInt(Pkt, Tag, 2);                          //The tag ID of this group. All members of this group have the same ID.
  PktInt(Pkt, $0000, 2);                        //The individual ID assigned to a contact. 0 for group headers.
  PktInt(Pkt, $0001, 2);                        //The type of the group. 0x0001 - Larger grouping header.
  PktInt(Pkt, TLVC8.Len + 4, 2);                //The number of bytes in the following TLVs. May be zero.
  PktTLV(Pkt, $00c8, TLVC8.Len, @TLVC8);        //Sent only with group header, a list of all IDs in this group.
  PktFinal(Pkt);                                //Finalize packet
end;

{Same as CreateCLI_UPDATEGROUP modified to use only with buddies. Prototype. Can be used for UPDATEBUDDY and ADDBUDDY}
procedure __CreateCLI_UPDATEBUDDY(Pkt: PRawPkt; A: Byte; UIN, Name, SMSNumber: String; Tag, ID: Word; BuddyType: Word; NotAuthorized, IsGroup: Boolean; var Seq: Word);
var
  TLVs: TRawPkt;
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $13, A, 0, 0);                   //SNAC: 0x13/0x08|0x09, Ref 0x00000000, Flags 0
  {Create temporary array with addition TLVs}
  PktInitRaw(@TLVs);
  if Name <> '' then
    PktTLV(@TLVs, $0131, StrToUTF8(Name));
  if NotAuthorized then
    PktTLV(@TLVs, $0066, 0, 0);
  if SMSNumber <> '' then
    PktTLV(@TLVs, $013A, StrToUTF8(SMSNumber));
  PktWStr(Pkt, StrToUTF8(UIN));                            //The name of this group/buddy's UIN
  PktInt(Pkt, Tag, 2);                          //The tag ID of this group. All members of this group have the same ID.
  PktInt(Pkt, ID, 2);                           //The individual ID assigned to a contact. 0 for group headers.
  PktInt(Pkt, BuddyType, 2);                    //The type of the buddy.
  if IsGroup or ((A = $0A) and (TLVs.Len <> 0)) or (A <> $0A) then
    PktInt(Pkt, TLVs.Len, 2);                   //The number of bytes in the following TLVs. May be zero.
  PktAddArrBuf(Pkt, @TLVs, TLVs.Len);           //Sent only with group header, a list of all IDs in this group.
  PktFinal(Pkt);                                //Finalize packet
end;

{Update SSL buddy.}
procedure CreateCLI_UPDATEBUDDY(Pkt: PRawPkt; UIN, Name, SMSNumber: String; Tag, ID: Word; BuddyType: Word; NotAuthorized: Boolean; var Seq: Word);
begin
  __CreateCLI_UPDATEBUDDY(Pkt, $09, UIN, Name, SMSNumber, Tag, ID, BuddyType, NotAuthorized, False, Seq);
end;

{Add SSL buddy.}
procedure CreateCLI_ADDBUDDY(Pkt: PRawPkt; UIN, Name, SMSNumber: String; Tag, ID: Word; BuddyType: Word; NotAuthorized: Boolean; var Seq: Word);
begin
  __CreateCLI_UPDATEBUDDY(Pkt, $08, UIN, Name, SMSNumber, Tag, ID, BuddyType, NotAuthorized, False, Seq);
end;

{Delete buddy from SSL.}
procedure CreateCLI_DELETEBUDDY(Pkt: PRawPkt; UIN, Name, SMSNumber: String; Tag, ID: Word; BuddyType: Word; NotAuthorized, IsGroup: Boolean; var Seq: Word);
begin
  __CreateCLI_UPDATEBUDDY(Pkt, $0A, UIN, Name, SMSNumber, Tag, ID, BuddyType, NotAuthorized, IsGroup, Seq);
end;

{Send you where added message.}
procedure CreateCLI_SEND_YOU_WERE_ADDED(Pkt: PRawPkt; ITime, IRandom, UIN, FromUIN: LongWord; var Seq: Word);
var
  lpkt: TRawPkt;
begin
  PktInit(Pkt, 2, Seq);                         //Channel 2
  PktSnac(Pkt, $04, $06, 0, 0);                 //Snac: Type x04/x06, ID x0000, Flags 0
  PktInt(Pkt, ITime, 4);                        //MID
  PktInt(Pkt, IRandom, 4);                      //MID
  PktInt(Pkt, 4, 2);                            //type, 4
  PktLStr(Pkt, UIN);                            //The UIN to send the message to.

  PktInitRaw(@lpkt);                            //Allocate packet for incapsulated TLV(02)

  PktLInt(@lpkt, FromUIN, 4);                   //Sender's UIN
  PktLInt(@lpkt, $000c, 4);                     //Message type: MSG_ADDED
  PktLNTS(@lpkt, '');                           //Empty message body


  PktTLV(Pkt, 5, lpkt.Len, @lpkt);              //Add TLV(5)
  PktTLV(Pkt, 6, 0, 0);                         //Always present empty TLV.
  PktFinal(Pkt);                                //Finalize packet
end;

{Conver SNAC's numberic representation to string name}
function SnacToStr(Family, SubType: Word): String;
begin
  Result := 'unknown';
  {CLI}
  if (Family = 1) and (SubType = 2) then
    Result := 'CLI_READY'
  else if (Family = 1) and (SubType = 6) then
    Result := 'CLI_RATESREQUEST'
  else if (Family = 1) and (SubType = 8) then
    Result := 'CLI_ACKRATES'
  else if (Family = 1) and (SubType = $11) then
    Result := 'CLI_SETIDLETIME'
  else if (Family = 1) and (SubType = $E) then
    Result := 'CLI_REQINFO'
  else if (Family = 1) and (SubType = $17) then
    Result := 'CLI_FAMILIES'
  else if (Family = 1) and (SubType = $1E) then
    Result := 'CLI_SETSTATUS'
  else if (Family = 2) and (SubType = $2) then
    Result := 'CLI_REQLOCATION'
  else if (Family = 2) and (SubType = $4) then
    Result := 'CLI_SETUSERINFO'
  else if (Family = 3) and (SubType = $2) then
    Result := 'CLI_REQBUDDY'
  else if (Family = 3) and (SubType = $4) then
    Result := 'CLI_ADDCONTACT'
  else if (Family = 3) and (SubType = $5) then
    Result := 'CLI_REMOVECONTACT'
  else if (Family = 4) and (SubType = $2) then
    Result := 'CLI_SETICBM'
  else if (Family = 4) and (SubType = $4) then
    Result := 'CLI_REQICBM'
  else if (Family = 4) and (SubType = $6) then
    Result := 'CLI_SENDMSG'
  else if (Family = 4) and (SubType = $B) then
    Result := 'CLI_ACKMSG'
  else if (Family = 9) and (SubType = $2) then
    Result := 'CLI_REQBOS'
  else if (Family = 9) and (SubType = $5) then
    Result := 'CLI_ADDVISIBLE'
  else if (Family = 9) and (SubType = $6) then
    Result := 'CLI_REMVISIBLE'
  else if (Family = 9) and (SubType = $7) then
    Result := 'CLI_ADDINVISIBLE'
  else if (Family = 9) and (SubType = $8) then
    Result := 'CLI_REMINVISIBLE'
  else if (Family = $13) and (SubType = $2) then
    Result := 'CLI_REQUNKNOWN'
  else if (Family = $13) and (SubType = $4) then
    Result := 'CLI_REQROSTER2'
  else if (Family = $13) and (SubType = $5) then
    Result := 'CLI_REQROSTER'
  else if (Family = $13) and (SubType = $7) then
    Result := 'CLI_UNKNOWN1'
  else if (Family = $13) and (SubType = $8) then
    Result := 'CLI_ADDBUDDY'
  else if (Family = $13) and (SubType = $9) then
    Result := 'CLI_UPDATEGROUP'
  else if (Family = $13) and (SubType = $A) then
    Result := 'CLI_DELETEBUDDY'
  else if (Family = $13) and (SubType = $11) then
    Result := 'CLI_ADDSTART'
  else if (Family = $13) and (SubType = $12) then
    Result := 'CLI_ADDEND'
  else if (Family = $13) and (SubType = $18) then
    Result := 'CLI_REQAUTH'
  else if (Family = $13) and (SubType = $1A) then
    Result := 'CLI_AUTHORIZE'
  else if (Family = $15) and (SubType = $2) then
    Result := 'CLI_TOICQSRV'
  else if (Family = $17) and (SubType = $4) then
    Result := 'CLI_REGISTERUSER'
  {SRV}
  else if (Family = $1) and (SubType = $3) then
    Result := 'SRV_FAMILIES'
  else if (Family = $1) and (SubType = $7) then
    Result := 'SRV_RATES'
  else if (Family = $1) and (SubType = $F) then
    Result := 'SRV_REPLYINFO'
  else if (Family = $1) and (SubType = $13) then
    Result := 'SRV_MOTD'
  else if (Family = $1) and (SubType = $18) then
    Result := 'SRV_FAMILIES2'
  else if (Family = $2) and (SubType = $3) then
    Result := 'SRV_REPLYLOCATION'
  else if (Family = $3) and (SubType = $3) then
    Result := 'SRV_REPLYBUDDY'
  else if (Family = $3) and (SubType = $B) then
    Result := 'SRV_USERONLINE'
  else if (Family = $3) and (SubType = $C) then
    Result := 'SRV_USEROFFLINE'
  else if (Family = $4) and (SubType = $5) then
    Result := 'SRV_REPLYICBM'
  else if (Family = $4) and (SubType = $7) then
    Result := 'SRV_RECVMSG'
  else if (Family = $4) and (SubType = $c) then
    Result := 'SRV_MSGACK_ADVANCED'
  else if (Family = $9) and (SubType = $3) then
    Result := 'SRV_REPLYBOS'
  else if (Family = $0B) and (SubType = $02) then
    Result := 'SRV_SETINTERVAL'
  else if (Family = $13) and (SubType = $3) then
    Result := 'SRV_REPLYUNKNOWN'
  else if (Family = $13) and (SubType = $6) then
    Result := 'SRV_REPLYROSTER'
  else if (Family = $13) and (SubType = $E) then
    Result := 'SRV_UPDATEACK'
  else if (Family = $13) and (SubType = $F) then
    Result := 'SRV_REPLYROSTEROK'
  else if (Family = $13) and (SubType = $19) then
    Result := 'SRV_AUTHORIZATION_REQUEST'
  else if (Family = $13) and (SubType = $1C) then
    Result := 'SRV_ADDEDYOU'
  else if (Family = $15) and (SubType = $3) then
    Result := 'SRV_FROMICQSRV'
  else if (Family = $17) and (SubType = $1) then
    Result := 'SRV_REGREFUSED'
  else if (Family = $17) and (SubType = $5) then
    Result := 'SRV_NEWUIN';
end;

{Convert meta command to string representation.}
function SrvMetaToStr(V1, V2: Word): String;
begin
  Result := '';
  if V1 = 2000 then
    case V2 of
      1002: Result := 'CLI_METASETGENERAL';
      1021: Result := 'CLI_METASETMORE';
      1030: Result := 'CLI_METASETABOUT';
      1060: Result := 'CLI_SETAUTH';
      1070: Result := 'CLI_METASETPASS';
      1210: Result := 'CLI_METAREQINFO_SHORT';
      1220: Result := 'CLI_METAUNREG';      
      1232: Result := 'CLI_METAREQINFO';
      1331: Result := 'CLI_SEARCHWP';
      1375: Result := 'CLI_SEARCHBYPERSINF';
      1385: Result := 'CLI_SEARCHBYUIN';
      1395: Result := 'CLI_SEARCHBYMAIL';
      1870: Result := 'CLI_SEARCHRANDOM';
      1880: Result := 'CLI_METASETRANDOM';
      2200: Result := 'CLI_REQXML';
      5250: Result := 'CLI_SENDSMS';
    end
  else if V1 = 2010 then
    case V2 of
      1:   Result := 'SRV_SMSREFUSED';
      100: Result := 'SRV_METAGENERALDONE';
      120: Result := 'SRV_METAMOREDONE';
      130: Result := 'SRV_METAABOUTDONE';
      150: Result := 'SRV_SMSACK';
      160: Result := 'SRV_AUTHDONE';
      170: Result := 'SRV_METAPASSDONE';
      180: Result := 'SRV_METAUNREG';
      200: Result := 'SRV_METAGENERAL';
      210: Result := 'SRV_METAWORK';
      220: Result := 'SRV_METAMORE';
      230: Result := 'SRV_METAABOUT';
      235: Result := 'SRV_METAMOREEMAIL';
      240: Result := 'SRV_METAINTEREST';
      250: Result := 'SRV_METABACKGROUND';
      260: Result := 'SRV_METAINFO';
      270: Result := 'SRV_META270';
      420: Result := 'SRV_METAFOUND';
      430: Result := 'SRV_METALAST';
      870: Result := 'SRV_METARANDOM';
      880: Result := 'SRV_METARANDOMDONE';
    end
  else if V1 = 60 then
    Result := 'CLI_REQOFFLINEMSGS'
  else if V1 = 62 then
    Result := 'CLI_ACKOFFLINEMSGS'
  else if V1 = 65 then
    Result := 'SRV_OFFLINEMSG'
  else if V1 = 66 then
    Result := 'SRV_DONEOFFLINEMSGS';
  if Result = '' then
    Result := IntToStr(V1) + '/' + IntToStr(V2);
end;

{Convert peer command to string representation.}
function PeerCmdToStr(Cmd: Byte): String;
begin
  case Cmd of
    $00: Result := 'PEER_FILE_INIT';
    $01: Result := 'PEER_INIT_ACK';
    $02: Result := 'PEER_MSG';
    $03: Result := 'PEER_INIT2';
    $06: Result := 'PEER_FILEDATA';
    $ff: Result := 'PEER_INIT';
  else
    Result := '';
  end;
end;

{Return Buffer in a string hex dump.}
function DumpPacket(Buffer: Pointer; BufLen: Word): String;
var
  S: String;
  i, n: Word;
begin
  for i := 1 to BufLen do
  begin
    S := S + IntToHex(PByte(LongWord(Buffer) + i - 1)^, 2) + ' ';
    if i mod 16 = 0 then
    begin
      S := S + '  ';
      for n := i - 15 to i do
      begin
        if (PByte(LongWord(Buffer) + n - 1)^ < $20) or (PByte(LongWord(Buffer) + n - 1)^ > $7F) then
          S := S + '.'
        else
          S := S + PChar(Buffer)[n - 1];
      end;
      S := S + #13#10;
    end;
  end;
  if BufLen mod 16 <> 0 then
  begin
    for i := 0 to 15 - (BufLen mod 16) do
      S := S + '   ';
    S := S + '  ';
    for i := BufLen mod 16 downto 1 do
    begin
      if (PByte(LongWord(Buffer) + BufLen - i)^ < $20) or (PByte(LongWord(Buffer) + BufLen - i)^ > $7F) then
        S := S + '.'
      else
        S := S + PChar(Buffer)[BufLen - i];
    end;
  end;
  Result := S;
end;

{Convert RTF enabled text to plain.}
function RTF2Plain (const aSource: string): string;
var
   Source: string;
   NChar: Integer;

function ProcessGroupRecursevly: string;

procedure SkipStar;
var
   BracesOpened: Integer;
   Escaped: Boolean;
begin
     BracesOpened:=1;
     Escaped:=false;
     while BracesOpened>0
           do begin
              Inc (NChar);
              case Source [NChar] of
                   '{': if Escaped
                           then Escaped:=false
                           else Inc (BracesOpened);
                   '}': if Escaped
                           then Escaped:=false
                           else Dec (BracesOpened);
                   '\': Escaped:=not Escaped;
                   else Escaped:=false;
              end;
           end;
end;

function UnicodeCharCode2ANSIChar (aCode: LongInt): Char;
type
    TUnicode2ANSITable=array [$0410..$044f] of Char;
const
     Unicode2ANSITable: TUnicode2AnsiTable=('À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Ç', 'È', 'É', 'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï', 'Ð', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', '×', 'Ø', 'Ù', 'Ú', 'Û', 'Ü', 'Ý', 'Þ', 'ß',
                                             'à', 'á', 'â', 'ã', 'ä', 'å', 'æ', 'ç', 'è', 'é', 'ê', 'ë', 'ì', 'í', 'î', 'ï', 'ð', 'ñ', 'ò', 'ó', 'ô', 'õ', 'ö', '÷', 'ø', 'ù', 'ú', 'û', 'ü', 'ý', 'þ', 'ÿ');
begin
     if (Low (Unicode2ANSITable)<=aCode) and (aCode<=High (Unicode2ANSITable))
        then UnicodeCharCode2ANSIChar:=Unicode2ANSITable [aCode]
        else UnicodeCharCode2ANSIChar:='?';
end;

var
   Control, NumericValue, TextValue: string;
begin
     Result:='';
     Inc (NChar);
     while NChar<=Length (Source)
           do case Source [NChar] of
                   '{': Result:=Result+ProcessGroupRecursevly;
                   '}': begin
                             Inc (NChar);
                             Break;
                        end;
                   '\': begin
                             Inc (NChar);
                             case Source [NChar] of
                                  '''': begin
                                             Result:=Result+Chr (HexToInt (Copy (Source, NChar+1, 2)));
                                             Inc (NChar, 3);
                                        end;
                                  '~': Result:=Result+#$20;
                                  '*': SkipStar;
                                  'a'..'z': begin
                                                 Control:='';
                                                 while Source [NChar] in ['a'..'z']
                                                       do begin
                                                          Control:=Control+Source [NChar];
                                                          Inc (NChar);
                                                       end;
                                                 if Source [NChar]='-'
                                                    then begin
                                                         NumericValue:=Source [NChar];
                                                         Inc (NChar);
                                                    end
                                                    else NumericValue:='';
                                                  while Source [NChar] in ['0'..'9']
                                                        do begin
                                                           NumericValue:=NumericValue+Source [NChar];
                                                           Inc (NChar);
                                                        end;
                                                  if Source [NChar]='{'
                                                     then ProcessGroupRecursevly;
                                                  TextValue:='';
                                                  if not (Source [NChar] in ['a'..'z', '{', '}', '\'])
                                                     then begin
                                                          Inc (NChar);
                                                          while not (Source [NChar] in ['{', '}', '\'])
                                                                do begin
                                                                   TextValue:=TextValue+Source [NChar];
                                                                   Inc (NChar);
                                                                end;
                                                     end;
                                                  if (Control='line') or (Control='par')
                                                     then Result:=Result+#$0D#$0A
                                                     else if Control='tab'
                                                             then Result:=Result+#$09
                                                             else if Control='u'
                                                                     then Result:=Result+UnicodeCharCode2ANSIChar (StrToInt (NumericValue))
                                                                     else if Control='colortbl'
                                                                             then TextValue:='';
                                                 if Length (TextValue)>0
                                                    then if (not ((TextValue [Length (TextValue)]=';') and (Source [NChar]='}')))
                                                            then begin
                                                                 Result:=Result+TextValue;
                                                                 TextValue:='';
                                                            end;
                                            end;
                                  else begin
                                       Result:=Result+Source [NChar];
                                       Inc (NChar);
                                  end;
                             end;
                   end;
                   else begin
                        Result:=Result+Source [NChar];
                        Inc (NChar);
                   end;
           end;
end;

function InitSource: Boolean;
var
   BracesCount: Integer;
   Escaped: Boolean;
begin
     if Copy (aSource, 1, 5)<>'{\rtf'
        then InitSource:=false
        else begin
             Source:='';
             BracesCount:=0;
             Escaped:=false;
             NChar:=1;
             while (NChar<=Length (aSource)) and (BracesCount>=0)
                   do begin
                      if not (aSource [NChar] in [#$0D, #$0A])
                         then begin
                              Source:=Source+aSource [NChar];
                              case aSource [NChar] of
                                   '{': if not Escaped
                                           then Inc (BracesCount)
                                           else Escaped:=false;
                                   '}': if not Escaped
                                           then Dec (BracesCount)
                                           else Escaped:=false;
                                   '\': Escaped:=true;
                                   else Escaped:=false;
                              end;
                         end;
                      Inc (NChar);
                   end;
             InitSource:=BracesCount=0;
        end;
end;

begin
     if InitSource
        then begin
             NChar:=1;
             Result:=ProcessGroupRecursevly;
        end
        else Result:=aSource;
end;

function StatusToInt(Value: LongWord): LongWord;
(*       StatusToInt() by eraser         *)
begin
  (*
                *** WARNING ***
    Do not change if-then statement order
  *)

  if (Value and S_DND) = S_DND then
    Result := S_DND
  else if (Value and S_OCCUPIED) = S_OCCUPIED then
    Result := S_OCCUPIED
  else if (Value and S_NA) = S_NA then
    Result := S_NA
  else if (Value and L_S_OCCUPIED) = L_S_OCCUPIED then  // updated by eraser 8.7.03
    Result := S_OCCUPIED
  else if (Value and L_S_DND) = L_S_DND then  // updated by eraser 8.7.03
    Result := S_DND
  else if (Value and S_AWAY) = S_AWAY then
    Result := S_AWAY
  else if (Value and S_INVISIBLE) = S_INVISIBLE then
    Result := S_INVISIBLE
  else if (Value and S_FFC) = S_FFC then
    Result := S_FFC
  else if (Value and L_S_NA) = L_S_NA then  // updated by eraser 8.7.03
    Result := S_NA
  else Result := S_ONLINE;

  if Value = S_OFFLINE then Result := S_OFFLINE;
end;

function StatusToStr(Value: LongWord): String;
(*       StatusToStr() by eraser          *)
begin
  (*
                *** WARNING ***
    Do not change if-then statement order
  *)

  if (Value and S_DND) = S_DND then
    Result := 'DND'
  else if (Value and S_OCCUPIED) = S_OCCUPIED then
    Result := 'Occupied'
  else if (Value and S_NA) = S_NA then
    Result := 'N/A'
  else if (Value and S_AWAY) = S_AWAY then
    Result := 'Away'
  else if (Value and S_INVISIBLE) = S_INVISIBLE then
    Result := 'Invisible'
  else if (Value and S_FFC) = S_FFC then
    Result := 'FFC'
  else Result := 'Online';
end;

(*
function StatusToStr(Value: LongWord): String;
begin
  {Remove any used flags.}
  Value := Value and not S_SHOWIP and not S_WEBAWARE and not S_ALLOWDCONN
                 and not S_ALLOWDAUTH and not S_ALLOWDLIST;

  if Value = S_INVISIBLE then
    Result := 'Invisible'
  else if Value = S_AWAY then
    Result := 'Away'
  else if Value = S_NA then
    Result := 'N/A'
  else if Value = S_OCCUPIED then
    Result := 'Occupied'
  else if Value = S_DND then
    Result := 'DND'
  else if Value = S_FFC then
    Result := 'FFC'
  else
    Result := 'Online';
end;
*)

function CountryToStr(Value: Word): String;
var
  i: Word;
begin
  Result := '';
  for i := Low(Countries) to High(Countries) do
    if Countries[i].Ident = Value then
    begin
      Result := Countries[i].Value;
      Exit;
    end;
end;

function LanguageToStr(Value: Byte): String;
var
  i: Byte;
begin
  for i := Low(Languages) to High(Languages) do
    if Languages[i].Ident = Value then
    begin
      Result := Languages[i].Value;
      Exit;
    end;
  Result := '';
end;

function OccupationToStr(Value: Word): String;
begin
  if (Value >= Low(Occupations)) and (Value <= High(Occupations)) then
    Result := Occupations[Value].Value
  else
    Result := '';
end;

function InterestToStr(Value: Word): String;
begin
  if (Value >= Low(Interests)) and (Value <= High(Interests)) then
    Result := Interests[Value].Value
  else
    Result := '';
end;

function PastToStr(Value: Word): String;
var
  i: Word;
begin
  for i := Low(Pasts) to High(Pasts) do
    if Pasts[i].Ident = Value then
    begin
      Result := Pasts[i].Value;
      Exit;
    end;
  Result := '';
end;

function AffiliationToStr(Value: Word): String;
var
  i: Word;
begin
  for i := Low(Organizations) to High(Organizations) do
    if Organizations[i].Ident = Value then
    begin
      Result := Organizations[i].Value;
      Exit;
    end;
  Result := '';
end;

{Local raw packet from file.}
function LoadPacketRaw(Pkt: PRawPkt; const FName: String): Boolean;
function TestDigit(Digit: Char): Boolean;
begin
  Result := False;
  case Digit of
    '0'..'9': Result := True;
    'A', 'B', 'C', 'D', 'E', 'F',
    'a', 'b', 'c', 'd', 'e', 'f': Result := True;
  end;
end;
function Convert(Digit: Char): Byte;
begin
  Result := 0;
  case Digit of
    '0'..'9': Result := StrToInt(Digit);
    'A', 'a': Result := $A;
    'B', 'b': Result := $B;
    'C', 'c': Result := $C;
    'D', 'd': Result := $D;
    'E', 'e': Result := $E;
    'F', 'f': Result := $F;
  end;
end;
var
  F: TextFile;
  c, c1: Char;
  i: Integer;
begin
  PktInitRaw(Pkt);
  System.Assign(F, FName); Reset(F);
  while not Eof(F) do
  begin
    for i := 0 to 15 do
    begin
      Read(F, c);
      Read(F, c1);
      if TestDigit(c) and TestDigit(c1) then
        PktInt(Pkt, Convert(c) shl 4 + Convert(c1), 1);
      Read(F, c);
      if c = '' then Break;
    end;
    Readln(F);
  end;
  System.Close(F);
  Result := True;
end;

{Load low packet from file & extract snac header.}
function LoadPacket(Pkt: PRawPkt; const FName: String; var Flap: TFlapHdr; var Snac: TSnacHdr): Boolean;
begin
  Result := LoadPacketRaw(Pkt, FName);
  pkt^.Len := TFLAPSZ;
  GetSnac(Pkt, Snac);
end;

{Checks if the FileName is exists.}
function FileExists(const FileName: String): Boolean;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Handle := FindFirstFile(PChar(FileName), FindData);
  Result := (Handle <> INVALID_HANDLE_VALUE) and (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0);
  Windows.FindClose(Handle);
end;

{Get size of a file.}
function FileSizeFromStr(const FName: String): LongWord;
var
  FileHandle: THandle;
begin
  Result := INVALID_FILE_SIZE;
  FileHandle := CreateFile(PChar(FName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_ALWAYS, 0, 0);
  if FileHandle = INVALID_HANDLE_VALUE then Exit;
  Result := GetFileSize(FileHandle, nil);
  CloseHandle(FileHandle);
end;

{Add some Text to FName file.}
procedure LogText(const FName, Text: String);
var
  F: TextFile;
begin
  if not FileExists(FName) then
  begin
    Assign(F, FName);
    {$I-}
    ReWrite(F);
    if IOResult <> 0 then
      Exit;
    {$I+}
    CloseFile(F);
  end;
  Assign(F, FName);
  {$I-}
  Append(F);
  if IOResult <> 0 then
    Exit;
  Writeln(F, Text);
  {$I+}
  CloseFile(F);
end;

{Overloaded procedure equal to ShowMessage() from 'Dialogs' unit}
procedure ShowMessage(const Value: String); overload;
begin
  MessageBox(0, PChar(Value), 'Message', 0);
end;

procedure ShowMessage(Value: LongInt); overload;
begin
  MessageBox(0, PChar(IntToStr(Value)), 'Message', 0);
end;

//Extract the name from the following string: 'AA=BB', where AA is name
function ExtractName(const Value: String): String;
var
  i: Word;
begin
  Result := '';
  i := Pos('=', Value);
  if i = 0 then
    Exit;
  Result := Copy(Value, 0, i - 1);
end;

//Extract the value from the following string: 'AA=BB', where BB is value
function ExtractValue(const Value: String): String;
var
  i: Word;
begin
  Result := '';
  i := Pos('=', Value);
  if i = 0 then
    Exit;
  Result := Copy(Value, i + 1, Length(Value) - i);
end;

{Convert string from UTF-8 format into ASCII}
function UTF8ToStr(Value: String): String;
var
  buffer: Pointer;
  BufLen: LongWord;
begin
  BufLen := Length(Value) + 4;
  GetMem(buffer, BufLen);
  FillChar(buffer^, BufLen, 0);
  MultiByteToWideChar(CP_UTF8, 0, @Value[1], BufLen - 4, buffer, BufLen);
  Result := WideCharToString(buffer);
  FreeMem(buffer, BufLen);
end;

{Convert string from UTF-8 format mixed with standart ASCII symbols($00..$7f)}
function UTF8ToStrSmart(Value: String): String;
var
  Digit: String;
  i: Word;
  HByte: Byte;
  Len: Byte;
begin
  Result := '';
  Len := 0;
  if Value = '' then Exit;
  for i := 1 to Length(Value) do
  begin
    if Len > 0 then
    begin
      Digit := Digit + Value[i];
      Dec(Len);
      if Len = 0 then
        Result := Result + UTF8ToStr(Digit);
    end else
    begin
      HByte := Ord(Value[i]);
      if HByte in [$00..$7f] then       //Standart ASCII chars
        Result := Result + Value[i]
      else begin
        //Get length of UTF-8 char
        if HByte and $FC = $FC then
          Len := 6
        else if HByte and $F8 = $F8 then
          Len := 5
        else if HByte and $F0 = $F0 then
          Len := 4
        else if HByte and $E0 = $E0 then
          Len := 3
        else if HByte and $C0 = $C0 then
          Len := 2
        else begin
          Result := Result + Value[i];
          Continue;
        end;
        Dec(Len);
        Digit := Value[i];
      end;
    end;
  end;
end;

{Get an XML entry.}
function GetXMLEntry(const Tag, Msg: String): String;
var
  p1, p2: Word;
begin
  p1 := Pos('<' + Tag + '>', Msg);
  p2 := Pos('</' + Tag + '>', Msg);
  Result := Copy(Msg, p1 + Length(Tag) + 2, p2 - p1 - Length(Tag) - 2);
end;


{SMS functions}
{Convert string to UTF8 format}
function StrToUTF8(Value: String): String;
var
  buffer: Pointer;
  BufLen: LongWord;
  lpBuf: Pointer;
begin
  BufLen := Length(Value) * 2 + 4;
  GetMem(buffer, BufLen); FillChar(buffer^, BufLen, 0);
  GetMem(lpBuf, BufLen); FillChar(lpBuf^, BufLen, 0);
  StringToWideChar(Value, buffer, BufLen);
  WideCharToMultiByte(CP_UTF8, 0, buffer, -1, lpBuf, BufLen, nil, nil);
  FreeMem(buffer, BufLen);
  Result := PChar(lpBuf);
  FreeMem(lpBuf, BufLen);
end;

{Get current time in format like 'Mon, 19 Nov 2001 08:23:38 GMT'}
function STime: String;
var
  buf: array[0..15] of Char;
  recv_bytes: Integer;
  SysTime: TSystemTime;
begin
  GetSystemTime(SysTime);
  recv_bytes := GetTimeFormat(LANG_ENGLISH, TIME_FORCE24HOURFORMAT,
    @SysTime, PChar('HH:mm:ss'), @buf, SizeOf(buf));
  Result := Copy(buf, 0, recv_bytes);
end;


{Get current time in format like 'Mon, 19 Nov 2001 08:23:38 GMT'}
function GetSMSTime: String;
  function STime: String;
  var
    buf: array[0..15] of Char;
    recv_bytes: Integer;
    SysTime: TSystemTime;
  begin
    GetSystemTime(SysTime);
    recv_bytes := GetTimeFormat(LANG_ENGLISH, TIME_FORCE24HOURFORMAT,
      @SysTime, PChar('HH:mm:ss'), @buf, SizeOf(buf));
    Result := Copy(buf, 0, recv_bytes);
  end;
  function SDate: String;
  var
    buf: array[0..15] of Char;
    recv_bytes: Integer;
    SysTime: TSystemTime;
  begin
    GetSystemTime(SysTime);
    recv_bytes := GetDateFormat(LANG_ENGLISH, 0,
      @SysTime, 'dd MMM yyyy', @buf, SizeOf(buf));
    Result := Copy(buf, 0, recv_bytes);
  end;
begin
  Result := SDate + ' ' + STime + ' GMT';
end;

const
  client_check_data: PChar =
    'As part of this software beta version Mirabilis is ' +
    'granting a limited access to the ICQ network, ' +
    'servers, directories, listings, information and databases ("' +
    'ICQ Services and Information"). The ' +
    'ICQ Service and Information may databases ("' +
    'ICQ Services and Information"). The ' +
    'ICQ Service and Information may'#0;

{Decrypt peer packet.}
function DecryptPak(Pak: Pointer; Size: LongWord; Ver: Byte): Boolean;
var
  hex, key, B1, M1, check: LongWord;
  i: Word;
  X1, X2, X3: Byte;
begin
  if Ver > 6 then
  begin
    Pak := Ptr(LongWord(Pak) + 1);
    Dec(Size);
  end;
  { get checkcode }
  check := PLongWord(pak)^;
  { primary decryption }
  key := $67657268 * size + check;
  i := 4;
  while i < Integer((size + 3) shr 2) do
  begin
    hex := key + Ord(client_check_data[i and $FF]);
    PLongWord(LongWord(pak) + i)^ := PLongWord(LongWord(pak) + i)^ xor hex;
    Inc(i, 4);
  end;
  B1 := (PByte(LongWord(pak) + 4)^ shl 24) or (PByte(LongWord(pak) + 6)^ shl 16) or (PByte(LongWord(pak) + 4)^ shl 8) or (PByte(LongWord(pak) + 6)^ shl 0);
  { special decryption }
  B1 := B1 xor check;
  { validate packet }
  M1 := (B1 shr 24) and $FF;
  if (M1 < 10) or (M1 >= size) then
  begin
    Result := False;
    Exit;
  end;
  X1 := PByte(LongWord(pak) + M1)^ xor $FF;
  if (((B1 shr 16) and $FF) <> X1) then
  begin
    Result := False;
    Exit;
  end;
  X2 := ((B1 shr 8) and $FF);
  if (X2 < 220) then
  begin
    X3 := Ord(client_check_data[X2]) xor $FF;
    if (B1 and $FF) <> X3 then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

{Encrypt peer packet.}
procedure EncryptPak(Pak: Pointer; Size: LongWord; Ver: Byte);
var
  B1, M1, check, hex: LongWord;
  key: int64;
  i: Word;
  X1, X2, X3, at: Byte;
  p: PByte;
begin
  p := Pak;
  size := Size;

  if (Ver > 6) then
  begin
    Inc(p);
    Dec(Size);
  end;

  { calculate verification data }
  if size < 255 then
    M1 := (Random(High(Word)) mod (Integer(size - 10))) + 10
  else
    M1 := (Random(High(Word)) mod 245) + 10;
  X1 := PByte(LongWord(p) + M1)^ xor $FF;
  X2 := Random(High(Word)) mod 220;
  X3 := Ord(client_check_data[X2]) xor $FF;
  B1 := (PByte(LongWord(p) + 4)^ shl 24) or (PByte(LongWord(p) + 6)^ shl 16) or
        (PByte(LongWord(p) + 4)^ shl 8) or (PByte(LongWord(p) + 6)^);

  { calculate checkcode }
  check := (M1 shl 24) or (X1 shl 16) or (X2 shl 8) or X3;
  check := check xor B1;

  { main XOR key }
  key := Int64($67657268) * size + check;

  { XORing the actual data }
  i := 0;
  while i < ((size + 3) div 4) do
  begin
    hex := key + Ord(client_check_data[i and $FF]);
    PLongWord(LongWord(p) + i)^ := PLongWord(LongWord(p) + i)^ xor hex;
    Inc(i, 4);
  end;

  { storing the checkcode }
  if Ver > 6 then at := 1 else at := 0;
  PLongWord(LongWord(pak) + at)^ := check;
end;

{This packet is sent during direct connection initialization between two ICQ clients.
It is sent by the originator of the connection to start the handshake and by the
receiver directly after it has sent the PEER_ACK packet as a reply to the originator's
PEER_INIT.}
procedure CreatePEER_INIT(Pkt: PRawPkt; Cookie, DestUIN, SrcUIN, SrcPort, SrcIPExt, SrcIPInt: LongWord; ProxyType: TProxyType);
begin
  PktInitRaw(Pkt);
  PktInt(Pkt, $ff, 1);          //The command: connect.
  PktInt(Pkt, $0800, 2);        //The peer-to-peer version this packet uses.
  PktInt(Pkt, $2b00, 2);        //The length of the following data in bytes.
  PktLInt(Pkt, DestUIN, 4);     //The UIN this packet is sent to.
  PktInt(Pkt, $0000, 2);        //Unknown: empty.
  PktLInt(Pkt, SrcPort, 4);     //The port the sender listens on.
  PktLInt(Pkt, SrcUIN, 4);      //The UIN of the sender.
  PktLInt(Pkt, SrcIPExt, 4);    //The IP of the sender as the server sees it.
  PktLInt(Pkt, SrcIPInt, 4);    //The local IP of the sender.
  if ProxyType = P_NONE then
    PktInt(Pkt, $04, 1)         //TCP connection flags: dirrect connection
  else
    PktInt(Pkt, $02, 1);        //TCP connection flags: 02 - SOCKS4/5 proxy
  PktLInt(Pkt, SrcPort, 4);     //The sender's "other" port.
  PktInt(Pkt, Cookie, 4);       //The connection cookie the server gave for this pair of UINs
  PktInt(Pkt, $50000000, 4);    //Unknown: 0x50 = 80.
  PktInt(Pkt, $03000000, 4);    //Unknown: 0x3 = 3.
  PktInt(Pkt, $00000000, 4);    //Unknown: empty.
end;

{This is an additional packet in the peer-to-peer handshake. The purpose is still
unknown. It is sent by the originator of the connection after he has acknowledged
the peer's PEER_INIT and by the peer as a reply to the originator's PEER_INIT2.}
procedure CreatePEER_INIT2(Pkt: PRawPkt; Ack: Boolean);
begin
  PktInitRaw(Pkt);
  PktInt(Pkt, $03, 1);          //The command: the last connect package
  PktInt(Pkt, $0a000000, 4);    //Unknown: 0xa = 10.
  PktInt(Pkt, $01000000, 4);    //Unknown: 0x1 = 1.
  if Ack then                   //
    PktInt(Pkt, $01000000, 4)   //Unknown. Use 01 00 00 00 = 0x1 = 1 for incoming,
  else                          //
    PktInt(Pkt, $00000000, 4);  //0 for outgoing connections.
  PktInt(Pkt, $00000000, 4);    //Unknown: empty.
  PktInt(Pkt, $00000000, 4);    //Unknown: empty.
  if Ack then                   //
    PktInt(Pkt, $01000400, 4)   //Unknown. Use 01 00 04 00 = 0x40001 for incoming
  else                          //
    PktInt(Pkt, $00000000, 4);  //and 0 for outgoing connections.
  PktInt(Pkt, $00000000, 4);    //Unknown: empty.
  if Ack then                   //
    PktInt(Pkt, $00000000, 4)   //Unknown. Use 0 on incoming,
  else                          //
    PktInt(Pkt, $01000400, 4);  //but 01 00 04 00 = 0x4001 for outgoing connections.
end;

{Acknowledges the receipt of a PEER_INIT packet.}
procedure CreatePEER_ACK(Pkt: PRawPkt);
begin
  PktInitRaw(Pkt);
  PktInt(Pkt, $01000000, 4);    //The command: acknowlegde the PEER_INIT
end;

{Basic header of outgoing PEER packet.}
procedure CreatePEER_HDR(Pkt: PRawPkt; Cmd, SubCmd, Seq: Word; Accept: Boolean);
begin
  PktInitRaw(Pkt);              //Init
  PktInt(Pkt, $02, 1);          //The command: send a message.
  PktInt(Pkt, $00000000, 4);    //The checksum of this packet.
  PktInt(Pkt, Cmd, 2);          //Message common type
  PktInt(Pkt, $0e00, 2);        //Unknown: 0xe = 14.
  PktLInt(Pkt, Seq, 2);         //Our sequence number.
  PktInt(Pkt, $00000000, 4);    //Unknown: empty.
  PktInt(Pkt, $00000000, 4);    //Unknown: empty.
  PktInt(Pkt, $00000000, 4);    //Unknown: empty.
  PktInt(Pkt, SubCmd, 2);       //The message type: message
  if Accept then
    PktInt(Pkt, $0000, 2)       //0x0000 - accept
  else
    PktInt(Pkt, $0100, 2);      //0x0100 - decline
end;

{Send a message to peer.}
function CreatePEER_MSG(Pkt: PRawPkt; const Msg: String; RTFFormat: Boolean; var Seq: Word): Word;
const
  StrGuid: String = '{97B12751-243C-4334-AD22-D6ABF73F1492}';
begin
  CreatePEER_HDR(Pkt, $ee07, $0100, Seq, True);
  PktInt(Pkt, $0000, 2);        //Our status.
  PktLNTS(Pkt, Msg);            //Finally the message.
  PktInt(Pkt, $00000000, 4);    //The foreground the client is expected to use.
  PktInt(Pkt, $ffffff00, 4);    //The background color the client is expected to show the message with.
  if RTFFormat then
  begin
    PktLInt(Pkt, Length(StrGuid), 4);    //This is a little-endian string length of the following GUID. This is only present in real messages sent by the latest 2001b client build 3659.
    PktStr(Pkt, StrGuid);                //This GUID seems to indicate that the client is capable of handling Multibyte Wide Character Strings as messages. Only present in real messages sent by build 3659 2001b clients.}
  end;
  EncryptPak(Pkt, Pkt^.Len, 8); //Encrypt packet
  Result := Seq; Inc(Seq);      //Inc Seq
end;

{Ack}
procedure CreatePEER_MSGACK(Pkt: PRawPkt; Seq: Word);
begin
  CreatePEER_HDR(Pkt, $da07, $0100, Seq, True);
  PktInt(Pkt, $0000, 2);        //Our status
  PktInt(Pkt, $0100, 2);        //Msg len = 1, Value = 0
  PktInt(Pkt, $00, 1);          //Msg null terminator
  PktInt(Pkt, $00000000, 4);    //The foreground the client is expected to use.
  PktInt(Pkt, $ffffff00, 4);    //The background color the client is expected to show the message with.
  EncryptPak(Pkt, Pkt^.Len, 8); //Encrypt packet
end;

{Response on auto-away msg request.}
procedure CreatePEER_AUTOMSG_ACK(Pkt: PRawPkt; Answer: String; Status, Seq: Word);
begin
  CreatePEER_HDR(Pkt, $da07, Swap16(Status), Seq, True);
  PktInt(Pkt, $0000, 2);        //Our status
  PktLNTS(Pkt, Answer);
  EncryptPak(Pkt, Pkt^.Len, 8); //Encrypt packet
end;

{Sends contacts to user.}
function CreatePEER_CONTACTS(Pkt: PRawPkt; Contacts: TStringList; var Seq: Word): Word;
const
  StrCmd: String = 'Contacts';
var
  S: String;
begin
  CreatePEER_HDR(Pkt, $ee07, $1a00, Seq, True);
  PktInt(Pkt, $0000, 2);        //Our status.
  PktInt(Pkt, $0100, 2);        //Message length: 2
  PktInt(Pkt, $00, 1);          //Null terminator
  PktInt(Pkt, $2d00, 2);        //Following length
  PktAddArrBuf(Pkt, @ContactsSignature, 16);     //14 unknown bytes
  PktInt(Pkt, $0000, 2);                //Possible command: send contacts
  PktLInt(Pkt, Length(StrCmd), 4);      //Length of the text command
  PktStr(Pkt, StrCmd);                  //Text command
  PktInt(Pkt, $00000000, 4);    //Unknown: empty
  PktInt(Pkt, $0001, 2);        //Unknown: 0x01
  PktInt(Pkt, $00000000, 4);    //Unknown: empty
  PktInt(Pkt, $00000000, 4);    //Unknown: empty
  PktInt(Pkt, $00, 1);          //Unknown: empty
  S := MakeContactsStr(Contacts);       //Create text list from string list
  PktLInt(Pkt, Length(S) + 4, 4);       //Length of the following data
  PktDWStr(Pkt, S);              //Length of the following string
  EncryptPak(Pkt, Pkt^.Len, 8); //Encrypt packet
  Result := Seq; Inc(Seq);      //Inc Seq
end;

function CreatePEER_CONTACTREQ(Pkt: PRawPkt; const Reason: String; var Seq: Word): Word;
const
  StrCmd: String = 'Request For Contacts';
begin
  CreatePEER_HDR(Pkt, $ee07, $1a00, Seq, True);
  PktInt(Pkt, $0000, 2);        //Our status.
  PktInt(Pkt, $0100, 2);        //Message length: 2
  PktInt(Pkt, $00, 1);          //Null terminator
  PktInt(Pkt, $3900, 2);        //Following length
  PktAddArrBuf(Pkt, @ContactsSignature, 16);   //16 unknown bytes
  PktInt(Pkt, $0200, 2);        //Possible command requesting contacts
  PktLInt(Pkt, Length(StrCmd), 4);    //Length of the text command
  PktStr(Pkt, StrCmd);          //Text command
  PktInt(Pkt, $00000000, 4);    //Unknown: empty
  PktInt(Pkt, $0001, 2);        //Unknown: 0x01
  PktInt(Pkt, $00000000, 4);    //Unknown: empty
  PktInt(Pkt, $00000000, 4);    //Unknown: empty
  PktInt(Pkt, $00, 1);          //Unknown: empty
  PktLInt(Pkt, Length(Reason) + 4, 4);  //Length of the following data
  PktLInt(Pkt, Length(Reason), 4);      //Length of the following string
  PktStr(Pkt, Reason);          //Following string
  EncryptPak(Pkt, Pkt^.Len, 8); //Encrypt packet
  Result := Seq; Inc(Seq);      //Inc Seq
end;

function CreatePEER_MSG_FILE(Pkt: PRawPkt; FileSendRec:TSendFileRec; var Seq: Word): Word;
const
  StrCmd = 'File';
var
  lpkt: TRawPkt;
begin
  CreatePEER_HDR(Pkt, $ee07, $1a00, Seq, True);
  PktInt(Pkt, $0000, 2);        //Status
  PktInt(Pkt, $01, 1);          //Flags
  PktInt(Pkt, $0000, 2);        //Unknown
  PktInitRaw(@lpkt);                    //Init new packet for the feature concatination
  PktAddArrBuf(@lpkt, @FileSignature, 16);  //Signature
  PktInt(@lpkt, $0000, 2);              //Unknown
  PktLInt(@lpkt, Length(StrCmd), 4);    //Length of the text command
  PktStr(@lpkt, StrCmd);                //Text command
  PktInt(@lpkt, $00000100, 4);          //Unknown
  PktInt(@lpkt, $00010000, 4);          //Unknown
  PktInt(@lpkt, $00000000, 4);          //Unknown
  PktInt(@lpkt, $0000, 2);              //Unknown
  PktInt(@lpkt, $00, 1);                //Unknown
  PktLInt(Pkt, lpkt.Len, 2);            //Length of the message
  PktAddArrBuf(Pkt, @lpkt, lpkt.Len);   //Implode packets
  PktInitRaw(@lpkt);                    //Init new packet for the feature concatination
  PktLInt(@lpkt, Length(FileSendRec.FileDescription), 4);   //Length of files description
  PktStr(@lpkt, FileSendRec.FileDescription);       //Files description
  PktInt(@lpkt, FileSendRec.Port, 2);               //Port
  PktInt(@lpkt, $5401, 2);             //Seq
  PktLInt(@lpkt, Length(FileSendRec.FileName) + 2, 2);      //Length of files description
  PktStr(@lpkt, FileSendRec.FileName);              //Files description
  PktInt(@lpkt, $0000, 2);              //Null terminator
  PktLInt(@lpkt, FileSendRec.TotalSize, 4);        //Filelength
  PktLInt(@lpkt, FileSendRec.Port, 4);              //Port
  PktLInt(Pkt, lpkt.Len, 4);            //Length of the following data
  PktAddArrBuf(Pkt, @lpkt, lpkt.Len);   //Implode packets
  EncryptPak(Pkt, Pkt^.Len, 8);         //Encrypt packet
  Result := Seq;                        //Inc Seq
end;

function CreatePEER_FILEINIT(Pkt: PRawPkt; Response: Boolean; FileDescription, FileName: String; Port: Word; FileLength: LongWord; var Seq: Word; Reason: String; Accept: Boolean): Word;
const
  StrCmd = 'File';
var
  lpkt: TRawPkt;
begin
  CreatePEER_HDR(Pkt, $da07, $1a00, Seq, Accept);
  if Accept then Reason := '';
  PktInt(Pkt, $0000, 2);        //Status
  if Length(Reason) = 0 then
  begin
    PktInt(Pkt, $01, 1);          //Flags
    PktInt(Pkt, $0000, 2);        //Unknown
  end else
  begin
    PktLInt(Pkt, Length(Reason) + 1, 2);
    PktStr(Pkt, Reason);
    PktInt(Pkt, $00, 1);
  end;
  PktInitRaw(@lpkt);                    //Init new packet for the feature concatination
  PktAddArrBuf(@lpkt, @FileSignature, 16);  //Signature
  PktInt(@lpkt, $0000, 2);              //Unknown
  PktLInt(@lpkt, Length(StrCmd), 4);    //Length of the text command
  PktStr(@lpkt, StrCmd);                //Text command
  PktInt(@lpkt, $00000100, 4);          //Unknown
  PktInt(@lpkt, $00010000, 4);          //Unknown
  PktInt(@lpkt, $00000000, 4);          //Unknown
  PktInt(@lpkt, $0000, 2);              //Unknown
  PktInt(@lpkt, $00, 1);                //Unknown
  PktLInt(Pkt, lpkt.Len, 2);            //Length of the message
  PktAddArrBuf(Pkt, @lpkt, lpkt.Len);   //Implode packets
  PktInitRaw(@lpkt);                    //Init new packet for the feature concatination
  PktLInt(@lpkt, Length(FileDescription), 4);   //Length of files description
  PktStr(@lpkt, FileDescription);       //Files description
  PktInt(@lpkt, Port, 2);               //Port
  if not Response then
    PktInt(@lpkt, $5401, 2)             //Seq
  else
    PktInt(@lpkt, $0000, 2);            //Seq
  PktLInt(@lpkt, Length(FileName) + 2, 2);      //Length of files description
  PktStr(@lpkt, FileName);              //Files description
  PktInt(@lpkt, $0000, 2);              //Null terminator
  PktLInt(@lpkt, FileLength, 4);        //Filelength
  PktLInt(@lpkt, Port, 4);              //Port
  PktLInt(Pkt, lpkt.Len, 4);            //Length of the following data
  PktAddArrBuf(Pkt, @lpkt, lpkt.Len);   //Implode packets
  EncryptPak(Pkt, Pkt^.Len, 8);         //Encrypt packet
  Result := Seq;                        //Inc Seq
end;

{This packet is a response to the incoming file transfer. It is sent as
a response to the PEER_FILE_INIT command.}
procedure CreatePEER_FILEINITACK(Pkt: PRawPkt; Speed: LongWord; Nick: String);
begin
  PktInitRaw(Pkt);                      //Initialize packet
  PktInt(Pkt, $01, 1);                  //The command: PEER_FILE_INITACK
  PktLInt(Pkt, Speed, 4);               //The receiver's speed. See PEER_FILE_INIT.
  PktLNTS(Pkt, Nick);                   //The receiver's nick.
end;

{The init packet sent within a new connection to the receiver given port
to initiate the file transfer. Note: the new connection is started with a
PEER_INIT/PEER_INITACK/PEER_INITACK2 sequence. Note: This is v6 of the
protocol.}
procedure CreatePEER_FILE_INIT2(Pkt: PRawPkt; Count, Bytes, Speed: LongWord);
begin
  PktInitRaw(Pkt);                      //Initialize packet
  PktInt(Pkt, $03, 1);                  //The command: PEER_FILE_INITACK2
  PktInt(Pkt, $00000000, 4);            //Unknown: empty.
  PktLInt(Pkt, Bytes, 4);               //Total bytes of all files to sent.
  PktLInt(Pkt, Speed, 4);               //The sender's speed. See PEER_FILE_INIT.
  PktLInt(Pkt, Count, 4);               //Total number of files to be sent.
end;

{This packet is sent before sending each file.}
procedure CreatePEER_FILE_START(Pkt: PRawPkt; FileName:String; FileSize, Speed:LongWord);
Begin
  PktInitRaw(Pkt);                     // Initialize packet.
  PktLInt(Pkt, $02, 2);                 //The command: PEER_MSG.
  PktLNTS(Pkt, FileName);              //Name of file to send.
  PktLInt(Pkt, $000001, 3);           //Unknown.
  PktLInt(Pkt, FileSize, 4);            //Size of file in bytes.
  PktLInt(Pkt, $3d60acdf, 4);           //Unknown
  PktLInt(Pkt, Speed, 4);               //Speed.
End;

procedure CreatePEER_FILE_INIT(Pkt: PRawPkt; Count, Bytes, Speed: LongWord; Nick:String);
begin
  PktInitRaw(Pkt);                      //Initialize packet
  PktInt(Pkt, $00, 1);                  //The command: PEER_FILE_INITACK2
  PktInt(Pkt, $00000000, 4);            //Unknown: empty.
  PktLInt(Pkt, Count, 4);                //Total number of files to be sent.
  PktLInt(Pkt, Bytes, 4);               //Total bytes of all files to sent.
  PktLInt(Pkt, Speed, 4);               //The sender's speed. See PEER_FILE_INIT.
  PktLNTS(Pkt, Nick);                   //The Sender's Nick;
end;

// Create File Data Packet
Procedure CreatePEER_FILE_DATA(Pkt: PRawPkt; Buffer:Pointer; BufLen:Integer);
Begin
  PktInitRaw(Pkt);
  PktInt(Pkt, $06, 1);                // The command: PEER_FILE_DATA
  PktAddArrBuf(Pkt, Buffer, BufLen); // The data.
  //EncryptPak(Pkt, Pkt^.Len, 8);
End;

const
  b64alphabet: PChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

function EncodeBase64(Value: String): String;
const
  pad: PChar = '====';

  function EncodeChunk(const Chunk: String): String;
  var
    W: LongWord;
    i, n: Byte;
  begin
    n := Length(Chunk); W := 0;
    for i := 0 to n - 1 do
      W := W + Ord(Chunk[i + 1]) shl ((2 - i) * 8);
    Result := b64alphabet[(W shr 18) and $3f] +
              b64alphabet[(W shr 12) and $3f] +
              b64alphabet[(W shr 06) and $3f] +
              b64alphabet[(W shr 00) and $3f];
    if n <> 3 then
      Result := Copy(Result, 0, n + 1) + Copy(pad, 0, 3 - n);   //add padding when out len isn't 24 bits
  end;

begin
  Result := '';
  while Length(Value) > 0 do
  begin
    Result := Result + EncodeChunk(Copy(Value, 0, 3));
    Delete(Value, 1, 3);
  end;
end;

function DecodeBase64(Value: String): String;
  function DecodeChunk(const Chunk: String): String;
  var
    W: LongWord;
    i: Byte;
  begin
    W := 0; Result := '';
    for i := 1 to 4 do
      if Pos(Chunk[i], b64alphabet) <> 0 then
        W := W + Word((Pos(Chunk[i], b64alphabet) - 1)) shl ((4 - i) * 6);
    for i := 1 to 3 do
      Result := Result + Chr(W shr ((3 - i) * 8) and $ff);
  end;
begin
  Result := '';
  if Length(Value) mod 4 <> 0 then Exit;
  while Length(Value) > 0 do
  begin
    Result := Result + DecodeChunk(Copy(Value, 0, 4));
    Delete(Value, 1, 4);
  end;
end;

{Create HTTP header.}
function CreateHTTP_Header(const Method, URL, Host: String; DataLen: LongWord; Auth: Boolean; User, Password: String): String;
begin
  Result := Method + ' ' + URL + ' HTTP/1.0' + #13#10 +
            'user-agent: Mozilla/4.08 [en] (WinNT; U ;Nav)' + #13#10;
  if DataLen > 0 then
            Result := Result + 'content-length: ' + IntToStr(DataLen) + #13#10;
  if Auth then
            Result := Result + 'Proxy-Authorization: Basic ' + EncodeBase64(User + ':' + Password) + #13#10;
  Result := Result +
            'cache-control: no-store, no-cache' + #13#10 +
            'host: ' + Host + #13#10 +
            'connection: close' + #13#10 +
            'pragma: no-cache' + #13#10#13#10;
end;


procedure CreateHTTP_DATA_HDR(Pkt: PRawPkt; PType: Word; DataLen: LongWord);
begin
  PktInitRaw(pkt);
  PktInt(pkt, DataLen + 12, 2);       //Length of the following data
  PktInt(pkt, $0443, 2);              //Version
  PktInt(pkt, PType, 2);              //Packet type
  PktInt(pkt, $00000000, 4);          //Unknown
  PktInt(pkt, $00000001, 4);          //Unknown
end;

{Write data headers in http proto.}
procedure CreateHTTP_DATA(Pkt: PRawPkt; PType: Word; Data: Pointer; DataLen: LongWord);
var
  lpkt: TRawPkt;
begin
  PktInitRaw(pkt);
  PktInitRaw(@lpkt);
  PktInt(@lpkt, $0443, 2);              //Version
  PktInt(@lpkt, PType, 2);              //Packet type
  PktInt(@lpkt, $00000000, 4);          //Unknown
  PktInt(@lpkt, $00000001, 4);          //Unknown
  PktAddArrBuf(@lpkt, Data, DataLen);   //Packet specific data

  PktInt(pkt, lpkt.Len, 2);             //Length of the following data
  PktAddArrBuf(pkt, @lpkt, lpkt.Len);   //Implode packets
end;

{First packet sent to proxy.}
function CreateHTTP_INIT(Auth: Boolean; User, Password: String): String;
begin
  Result := CreateHTTP_Header('GET', 'http://http.proxy.icq.com/hello', 'http.proxy.icq.com', 0, Auth, User, Password);
end;

{Packet sent after packet was received from server.}
function CreateHTTP_RECV(Host, SID: String; Auth: Boolean; User, Password: String): String;
begin
  Result := CreateHTTP_Header('GET', 'http://' + Host + '/monitor?sid=' + SID, Host, 0, Auth, User, Password);
end;

{Packet send as a response on HTTP_HELLO, ptype = 2.}
procedure CreateHTTP_LOGIN(Pkt: PRawPkt; Host: String; Port: Word);
{var
  lpkt: TRawPkt;}
begin
  {PktInitRaw(@lpkt);
  PktWStr(@lpkt, Host);                 //ICQ server
  PktInt(@lpkt, Port, 2);               //Port
  CreateHTTP_DATA(Pkt, $0003, @lpkt, lpkt.Len);}
  PktInitRaw(Pkt);
  PktWStr(Pkt, Host);                 //ICQ server
  PktInt(Pkt, Port, 2);               //Port
end;




{Xorkeygen tabs}
const
  TAB0: array[0..63] of LongWord =
    ($00820200, $00020000, $80800000, $80820200,
     $00800000, $80020200, $80020000, $80800000,
     $80020200, $00820200, $00820000, $80000200,
     $80800200, $00800000, $00000000, $80020000,
     $00020000, $80000000, $00800200, $00020200,
     $80820200, $00820000, $80000200, $00800200,
     $80000000, $00000200, $00020200, $80820000,
     $00000200, $80800200, $80820000, $00000000,
     $00000000, $80820200, $00800200, $80020000,
     $00820200, $00020000, $80000200, $00800200,
     $80820000, $00000200, $00020200, $80800000,
     $80020200, $80000000, $80800000, $00820000,
     $80820200, $00020200, $00820000, $80800200,
     $00800000, $80000200, $80020000, $00000000,
     $00020000, $00800000, $80800200, $00820200,
     $80000000, $80820000, $00000200, $80020200);

  TAB1: array[0..63] of LongWord =
    ($10042004, $00000000, $00042000, $10040000,
     $10000004, $00002004, $10002000, $00042000,
     $00002000, $10040004, $00000004, $10002000,
     $00040004, $10042000, $10040000, $00000004,
     $00040000, $10002004, $10040004, $00002000,
     $00042004, $10000000, $00000000, $00040004,
     $10002004, $00042004, $10042000, $10000004,
     $10000000, $00040000, $00002004, $10042004,
     $00040004, $10042000, $10002000, $00042004,
     $10042004, $00040004, $10000004, $00000000,
     $10000000, $00002004, $00040000, $10040004,
     $00002000, $10000000, $00042004, $10002004,
     $10042000, $00002000, $00000000, $10000004,
     $00000004, $10042004, $00042000, $10040000,
     $10040004, $00040000, $00002004, $10002000,
     $10002004, $00000004, $10040000, $00042000);

  TAB2: array[0..63] of LongWord =
    ($41000000, $01010040, $00000040, $41000040,
     $40010000, $01000000, $41000040, $00010040,
     $01000040, $00010000, $01010000, $40000000,
     $41010040, $40000040, $40000000, $41010000,
     $00000000, $40010000, $01010040, $00000040,
     $40000040, $41010040, $00010000, $41000000,
     $41010000, $01000040, $40010040, $01010000,
     $00010040, $00000000, $01000000, $40010040,
     $01010040, $00000040, $40000000, $00010000,
     $40000040, $40010000, $01010000, $41000040,
     $00000000, $01010040, $00010040, $41010000,
     $40010000, $01000000, $41010040, $40000000,
     $40010040, $41000000, $01000000, $41010040,
     $00010000, $01000040, $41000040, $00010040,
     $01000040, $00000000, $41010000, $40000040,
     $41000000, $40010040, $00000040, $01010000);

  TAB3: array[0..63] of LongWord =
    ($00100402, $04000400, $00000002, $04100402,
     $00000000, $04100000, $04000402, $00100002,
     $04100400, $04000002, $04000000, $00000402,
     $04000002, $00100402, $00100000, $04000000,
     $04100002, $00100400, $00000400, $00000002,
     $00100400, $04000402, $04100000, $00000400,
     $00000402, $00000000, $00100002, $04100400,
     $04000400, $04100002, $04100402, $00100000,
     $04100002, $00000402, $00100000, $04000002,
     $00100400, $04000400, $00000002, $04100000,
     $04000402, $00000000, $00000400, $00100002,
     $00000000, $04100002, $04100400, $00000400,
     $04000000, $04100402, $00100402, $00100000,
     $04100402, $00000002, $04000400, $00100402,
     $00100002, $00100400, $04100000, $04000402,
     $00000402, $04000000, $04000002, $04100400);

  TAB4: array[0..63] of LongWord =
    ($02000000, $00004000, $00000100, $02004108,
     $02004008, $02000100, $00004108, $02004000,
     $00004000, $00000008, $02000008, $00004100,
     $02000108, $02004008, $02004100, $00000000,
     $00004100, $02000000, $00004008, $00000108,
     $02000100, $00004108, $00000000, $02000008,
     $00000008, $02000108, $02004108, $00004008,
     $02004000, $00000100, $00000108, $02004100,
     $02004100, $02000108, $00004008, $02004000,
     $00004000, $00000008, $02000008, $02000100,
     $02000000, $00004100, $02004108, $00000000,
     $00004108, $02000000, $00000100, $00004008,
     $02000108, $00000100, $00000000, $02004108,
     $02004008, $02004100, $00000108, $00004000,
     $00004100, $02004008, $02000100, $00000108,
     $00000008, $00004108, $02004000, $02000008);

  TAB5: array[0..63] of LongWord =
    ($20000010, $00080010, $00000000, $20080800,
     $00080010, $00000800, $20000810, $00080000,
     $00000810, $20080810, $00080800, $20000000,
     $20000800, $20000010, $20080000, $00080810,
     $00080000, $20000810, $20080010, $00000000,
     $00000800, $00000010, $20080800, $20080010,
     $20080810, $20080000, $20000000, $00000810,
     $00000010, $00080800, $00080810, $20000800,
     $00000810, $20000000, $20000800, $00080810,
     $20080800, $00080010, $00000000, $20000800,
     $20000000, $00000800, $20080010, $00080000,
     $00080010, $20080810, $00080800, $00000010,
     $20080810, $00080800, $00080000, $20000810,
     $20000010, $20080000, $00080810, $00000000,
     $00000800, $20000010, $20000810, $20080800,
     $20080000, $00000810, $00000010, $20080010);

  TAB6: array[0..63] of LongWord =
    ($00001000, $00000080, $00400080, $00400001,
     $00401081, $00001001, $00001080, $00000000,
     $00400000, $00400081, $00000081, $00401000,
     $00000001, $00401080, $00401000, $00000081,
     $00400081, $00001000, $00001001, $00401081,
     $00000000, $00400080, $00400001, $00001080,
     $00401001, $00001081, $00401080, $00000001,
     $00001081, $00401001, $00000080, $00400000,
     $00001081, $00401000, $00401001, $00000081,
     $00001000, $00000080, $00400000, $00401001,
     $00400081, $00001081, $00001080, $00000000,
     $00000080, $00400001, $00000001, $00400080,
     $00000000, $00400081, $00400080, $00001080,
     $00000081, $00001000, $00401081, $00400000,
     $00401080, $00000001, $00001001, $00401081,
     $00400001, $00401080, $00401000, $00001001);

  TAB7: array[0..63] of LongWord =
    ($08200020, $08208000, $00008020, $00000000,
     $08008000, $00200020, $08200000, $08208020,
     $00000020, $08000000, $00208000, $00008020,
     $00208020, $08008020, $08000020, $08200000,
     $00008000, $00208020, $00200020, $08008000,
     $08208020, $08000020, $00000000, $00208000,
     $08000000, $00200000, $08008020, $08200020,
     $00200000, $00008000, $08208000, $00000020,
     $00200000, $00008000, $08000020, $08208020,
     $00008020, $08000000, $00000000, $00208000,
     $08200020, $08008020, $08008000, $00200020,
     $08208000, $00000020, $00200020, $08008000,
     $08208020, $00200000, $08200000, $08000020,
     $00208000, $00008020, $08008020, $08200000,
     $00000020, $08208000, $00208020, $00000000,
     $08000000, $08200020, $00008000, $00208020);

  TAB8: array[0..63] of LongWord =
    ($00000000, $00000010, $20000000, $20000010,
     $00010000, $00010010, $20010000, $20010010,
     $00000800, $00000810, $20000800, $20000810,
     $00010800, $00010810, $20010800, $20010810,
     $00000020, $00000030, $20000020, $20000030,
     $00010020, $00010030, $20010020, $20010030,
     $00000820, $00000830, $20000820, $20000830,
     $00010820, $00010830, $20010820, $20010830,
     $00080000, $00080010, $20080000, $20080010,
     $00090000, $00090010, $20090000, $20090010,
     $00080800, $00080810, $20080800, $20080810,
     $00090800, $00090810, $20090800, $20090810,
     $00080020, $00080030, $20080020, $20080030,
     $00090020, $00090030, $20090020, $20090030,
     $00080820, $00080830, $20080820, $20080830,
     $00090820, $00090830, $20090820, $20090830);

  TAB9: array[0..63] of LongWord =
    ($00000000, $02000000, $00002000, $02002000,
     $00200000, $02200000, $00202000, $02202000,
     $00000004, $02000004, $00002004, $02002004,
     $00200004, $02200004, $00202004, $02202004,
     $00000400, $02000400, $00002400, $02002400,
     $00200400, $02200400, $00202400, $02202400,
     $00000404, $02000404, $00002404, $02002404,
     $00200404, $02200404, $00202404, $02202404,
     $10000000, $12000000, $10002000, $12002000,
     $10200000, $12200000, $10202000, $12202000,
     $10000004, $12000004, $10002004, $12002004,
     $10200004, $12200004, $10202004, $12202004,
     $10000400, $12000400, $10002400, $12002400,
     $10200400, $12200400, $10202400, $12202400,
     $10000404, $12000404, $10002404, $12002404,
     $10200404, $12200404, $10202404, $12202404);

  TABA: array[0..63] of LongWord =
    ($00000000, $00000001, $00040000, $00040001,
     $01000000, $01000001, $01040000, $01040001,
     $00000002, $00000003, $00040002, $00040003,
     $01000002, $01000003, $01040002, $01040003,
     $00000200, $00000201, $00040200, $00040201,
     $01000200, $01000201, $01040200, $01040201,
     $00000202, $00000203, $00040202, $00040203,
     $01000202, $01000203, $01040202, $01040203,
     $08000000, $08000001, $08040000, $08040001,
     $09000000, $09000001, $09040000, $09040001,
     $08000002, $08000003, $08040002, $08040003,
     $09000002, $09000003, $09040002, $09040003,
     $08000200, $08000201, $08040200, $08040201,
     $09000200, $09000201, $09040200, $09040201,
     $08000202, $08000203, $08040202, $08040203,
     $09000202, $09000203, $09040202, $09040203);

  TABB: array[0..63] of LongWord =
    ($00000000, $00100000, $00000100, $00100100,
     $00000008, $00100008, $00000108, $00100108,
     $00001000, $00101000, $00001100, $00101100,
     $00001008, $00101008, $00001108, $00101108,
     $04000000, $04100000, $04000100, $04100100,
     $04000008, $04100008, $04000108, $04100108,
     $04001000, $04101000, $04001100, $04101100,
     $04001008, $04101008, $04001108, $04101108,
     $00020000, $00120000, $00020100, $00120100,
     $00020008, $00120008, $00020108, $00120108,
     $00021000, $00121000, $00021100, $00121100,
     $00021008, $00121008, $00021108, $00121108,
     $04020000, $04120000, $04020100, $04120100,
     $04020008, $04120008, $04020108, $04120108,
     $04021000, $04121000, $04021100, $04121100,
     $04021008, $04121008, $04021108, $04121108);

  TABC: array[0..63] of LongWord =
    ($00000000, $10000000, $00010000, $10010000,
     $00000004, $10000004, $00010004, $10010004,
     $20000000, $30000000, $20010000, $30010000,
     $20000004, $30000004, $20010004, $30010004,
     $00100000, $10100000, $00110000, $10110000,
     $00100004, $10100004, $00110004, $10110004,
     $20100000, $30100000, $20110000, $30110000,
     $20100004, $30100004, $20110004, $30110004,
     $00001000, $10001000, $00011000, $10011000,
     $00001004, $10001004, $00011004, $10011004,
     $20001000, $30001000, $20011000, $30011000,
     $20001004, $30001004, $20011004, $30011004,
     $00101000, $10101000, $00111000, $10111000,
     $00101004, $10101004, $00111004, $10111004,
     $20101000, $30101000, $20111000, $30111000,
     $20101004, $30101004, $20111004, $30111004);

  TABD: array[0..63] of LongWord =
    ($00000000, $08000000, $00000008, $08000008,
     $00000400, $08000400, $00000408, $08000408,
     $00020000, $08020000, $00020008, $08020008,
     $00020400, $08020400, $00020408, $08020408,
     $00000001, $08000001, $00000009, $08000009,
     $00000401, $08000401, $00000409, $08000409,
     $00020001, $08020001, $00020009, $08020009,
     $00020401, $08020401, $00020409, $08020409,
     $02000000, $0A000000, $02000008, $0A000008,
     $02000400, $0A000400, $02000408, $0A000408,
     $02020000, $0A020000, $02020008, $0A020008,
     $02020400, $0A020400, $02020408, $0A020408,
     $02000001, $0A000001, $02000009, $0A000009,
     $02000401, $0A000401, $02000409, $0A000409,
     $02020001, $0A020001, $02020009, $0A020009,
     $02020401, $0A020401, $02020409, $0A020409);

  TABE: array[0..63] of LongWord =
    ($00000000, $00000100, $00080000, $00080100,
     $01000000, $01000100, $01080000, $01080100,
     $00000010, $00000110, $00080010, $00080110,
     $01000010, $01000110, $01080010, $01080110,
     $00200000, $00200100, $00280000, $00280100,
     $01200000, $01200100, $01280000, $01280100,
     $00200010, $00200110, $00280010, $00280110,
     $01200010, $01200110, $01280010, $01280110,
     $00000200, $00000300, $00080200, $00080300,
     $01000200, $01000300, $01080200, $01080300,
     $00000210, $00000310, $00080210, $00080310,
     $01000210, $01000310, $01080210, $01080310,
     $00200200, $00200300, $00280200, $00280300,
     $01200200, $01200300, $01280200, $01280300,
     $00200210, $00200310, $00280210, $00280310,
     $01200210, $01200310, $01280210, $01280310);

  TABF: array[0..63] of LongWord =
    ($00000000, $04000000, $00040000, $04040000,
     $00000002, $04000002, $00040002, $04040002,
     $00002000, $04002000, $00042000, $04042000,
     $00002002, $04002002, $00042002, $04042002,
     $00000020, $04000020, $00040020, $04040020,
     $00000022, $04000022, $00040022, $04040022,
     $00002020, $04002020, $00042020, $04042020,
     $00002022, $04002022, $00042022, $04042022,
     $00000800, $04000800, $00040800, $04040800,
     $00000802, $04000802, $00040802, $04040802,
     $00002800, $04002800, $00042800, $04042800,
     $00002802, $04002802, $00042802, $04042802,
     $00000820, $04000820, $00040820, $04040820,
     $00000822, $04000822, $00040822, $04040822,
     $00002820, $04002820, $00042820, $04042820,
     $00002822, $04002822, $00042822, $04042822);

  TABQ: array[0..15] of boolean =
    (FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
     FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE);


type
  RTTabArray = array[0..31] of LongInt;

procedure TableGen(var t: RTTabArray; UIN: LongInt);
var u:     array[0..7] of Byte;
    ul:    array[0..1] of LongWord absolute u;
    v:     Byte;
    x:     Byte;
    l,h:   LongWord;
    a,b,c: LongWord;
begin
     {---- create UIN "hash" ----}
     v := ((UIN+9) shr 6) and 1;
     u[0] := ( ((trunc(sqrt(UIN*3+2)) and 1) or
               (((UIN shr 17) and 1) shl 1))  shl 2 ) or v;
     u[1] := ( (((trunc(sin(UIN)) shr 14) and 1) or
                (((UIN shr 12) and 1) shl 1))  shl 2 ) or v;
     u[4] := ( (( (((UIN shr 7) and 1) or
                   (((UIN shr 12) and 1) shl 1))  shl 1 ) or
                ((UIN shr 12) and 1))  shl 1 ) or
             ((UIN shr (UIN and 1)) and 1);
     u[6] := ( (( (((trunc(cos(UIN)) shr 8) and 1) or
                  (((UIN shr 5) and 1) shl 1))  shl 1 ) or
                ((UIN shr 19) and 1))  shl 1 ) or
             ((UIN shr 18) and 1);
     u[3] := (( ((((UIN shr 9) and 1) shl 1) or
                 ((UIN shr 6) and 1))  shl 1 ) or
              (((UIN*5) shr 11) and 1))  shl 1;
     u[5] := ( (((trunc(sin(UIN)/cos(UIN){=tan(UIN)}) shr 4) and 1) or
                (((UIN shr 11) and 1) shl 1))  shl 2 ) or
             ((UIN shr 2) and 1);
     u[2] := ( (((trunc(sqrt(UIN*3+2)) shr 13) and 1) or
                (((UIN shr 10) and 1) shl 1))  shl 2 ) or v;
     u[7] := 0;

     {---- generate run-time encryption table ----}
     l := ul[0];
     h := ul[1];

     a := (l and $0F0F0F0F) xor ((h shr 4) and $0F0F0F0F);
     l := l xor a;
     h := h xor (a shl 4);

     a := (l and $CCCC0000) xor ((l and $FFFFF333) shl 18);
     l := l xor (a xor (a shr 18));
     a := (h and $CCCC0000) xor ((h and $FFFFF333) shl 18);
     h := h xor (a xor (a shr 18));

     a := (l and $55555555) xor ((h shr 1) and $55555555);
     l := l xor a;
     h := h xor (a shl 1);

     a := (h and $00FF00FF) xor ((l shr 8) and $00FF00FF);
     l := l xor (a shl 8);
     h := h xor a;

     a := (l and $55555555) xor ((h shr 1) and $55555555);
     l := l xor a;
     h := h xor (a shl 1);

     a := l and $0FFFFFFF;
     b := ( ((l and $F000000F) or ((h shr 12) and $00000FF0))  shr 4 ) or
          (h and $0000FF00) or ((h and $FF) shl 16);

     for x := 0 to 15 do
     begin
          if TABQ[x] then
          begin
               a := ((a and $3F) shl 26) or (a shr 2);
               b := ((b and $3F) shl 26) or (b shr 2);
          end
          else begin
                    a := ((a and $1F) shl 27) or (a shr 1);
                    b := ((b and $1F) shl 27) or (b shr 1);
               end;

          a := a and $0FFFFFFF;
          b := b and $0FFFFFFF;

          l := TABB[(( ((a and $00C00000) or
                       ((a shr 1) and $07000000))  shr 1 ) or
                     (a and $00100000))  shr 20] or
               TABA[((a and $0001E000) or
                     ((a shr 1) and $00060000))  shr 13] or
               TAB9[((a and $C0) or (l shr 1))  shr 6] or
               TAB8[a and 63];

          h := TABD[((b and $00000180) or
                     ((b shr 1) and $00001E00))  shr 7] or
               TABF[((b and $01E00000) or
                     ((b shr 1) and $06000000))  shr 21] or
               TABE[(b shr 15) and 63] or
               TABC[b and 63];

          c := (h and $FFFF0000) or (l shr 16);
          t[x*2+0] := (l and $0000FFFF) or (h shl 16);
          t[x*2+1] := (c shl 4) or (c shr 28);          { = ROL(c,4)}
     end;
end;

procedure XORKeyGen(var t: RTTabArray; var KeyLow, KeyHigh: LongInt);
var l, h,
    a, b: LongInt;
    x: Byte;
begin
  l := KeyLow;
  h := KeyHigh;

  a := (l and $0F0F0F0F) xor ((h shr 4) and $0F0F0F0F);
  l := l xor a;
  h := h xor (a shl 4);

  a := (h and $0000FFFF) xor (l shr 16);
  l := l xor (a shl 16);
  h := h xor a;

  a := (l and $33333333) xor ((h shr 2) and $33333333);
  l := l xor a;
  h := h xor (a shl 2);

  a := (h and $00FF00FF) xor ((l shr 8) and $00FF00FF);
  l := l xor (a shl 8);
  h := h xor a;

  a := (l and $55555555) xor ((h shr 1) and $55555555);
  l := l xor a;
  h := h xor (a shl 1);

  l := (l shl 1) or (l shr 31);                      {l = ROL(l,1)}
  h := (h shl 1) or (h shr 31);                      {h = ROL(h,1)}

  for x := 0 to 7 do
  begin
    a := t[x*4+0] xor l;
    b := t[x*4+1] xor l;
    b := (b shr 4) or (b shl 28);                 {b = ROR(b,4)}
    h := (h xor LongInt((TAB2[(a shr  8) and 63] or
    TAB3[(b shr  8) and 63] or
    TAB4[(a shr 16) and 63] or
    TAB5[(b shr 16) and 63] or
    TAB6[(a shr 24) and 63] or
    TAB7[(b shr 24) and 63] or
    TAB1[ b         and 63] or
    TAB0[ a         and 63])));

    a := t[x*4+2] xor h;
    b := t[x*4+3] xor h;
    b := (b shr 4) or (b shl 28);                 {b = ROR(b,4)}
    l := l xor LongInt((TAB2[(a shr  8) and 63] or
      TAB3[(b shr  8) and 63] or
      TAB4[(a shr 16) and 63] or
      TAB5[(b shr 16) and 63] or
      TAB6[(a shr 24) and 63] or
      TAB7[(b shr 24) and 63] or
      TAB1[ b         and 63] or
      TAB0[ a         and 63]));
  end;

  h := (h shr 1) or (h shl 31);                      {h = ROR(h,1)}
  l := (l shr 1) or (l shl 31);                      {l = ROR(l,1)}
  a := (h and $55555555) xor ((l shr 1) and $55555555);
  h := h xor a;
  l := l xor (a shl 1);
  a := (l and $00FF00FF) xor ((h shr 8) and $00FF00FF);
  h := h xor (a shl 8);
  l := l xor a;
  a := (h and $33333333) xor ((l shr 2) and $33333333);
  h := h xor a;
  l := l xor (a shl 2);
  a := (l and $0000FFFF) xor (h shr 16);
  l := l xor a;
  h := h xor (a shl 16);
  a := (h and $0F0F0F0F) xor ((l shr 4) and $0F0F0F0F);
  KeyLow  := h xor a;
  KeyHigh := l xor (a shl 4);
end;

{Xorkeygen by CoverD}
procedure GetXorKey(FUIN: LongWord; FCryptIV: LongWord; var XorKey: array of Byte);
var
  UIN:      LongInt;
  CryptIV:  LongInt;
  RTTab:    RTTabArray;
  l:        LongInt;
  h:        LongInt;
  key:      array[0..15] of Byte;
  keyl:     array[0..3] of LongInt absolute key;
  x:        byte;
begin
  UIN      := FUIN;
  CryptIV  := FCryptIV;

  TableGen(RTTab, UIN);      {create UIN-based run-time encryption table}

  l := CryptIV;
  h := 0;
  XORKeyGen(RTTab, l,h);     {generate first 8 bytes of XOR key}
  keyl[0] := l;
  keyl[1] := h;
  XORKeyGen(RTTab, l,h);     {generate next 8 bytes (first 3 are used)}
  keyl[2] := l;
  keyl[3] := h;

  for x := 0 to 10 do
    XorKey[x] := key[x];
end;

function Decrypt99bPassword(UIN, CryptIV: LongWord; const HexPass: String): String;
var
  XorKey,
  FBytePassw: array[0..15] of Byte;
  i, n: Word;
begin
  Result := '';
  if (UIN = 0) or (CryptIV = 0) or (Length(HexPass) = 0) then Exit;
  GetXorKey(UIN, CryptIV, XorKey);
  i := 0;
  for n := 1 to Length(HexPass) do
    if n mod 2 = 0 then
    begin
      FBytePassw[i] := HexToInt(Copy(HexPass, n - 1, 2));
      Inc(i);
    end;
  {First 2-bytes -- Length}
  for n := 2 to i - 2 do
    Result := Result + Chr(FBytePassw[n] xor XorKey[n]);
end;

function DecryptMirandaPassword(const Value: String): String;
var
  i: Word;
begin
  Result := '';
  if Length(Value) < 1 then Exit;
  for i := 1 to Length(Value) do
  begin
    Result := Result + Chr(Ord(Value[i]) - 5);
  end;
end;

{Converts a Unix/Linux style date-and-time value to a TDateTime value.

Description
Call UnixToDateTime to convert the Unix/Linux encoding of a date and time value into the corresponding TDateTime.
Unix date-and-time values are encoded as the number of seconds since midnight at the start of January 1, 1970.}
function UnixToDateTime(const AValue: Int64): TDateTime;
const
{ Units of time }
  //HoursPerDay(24) * MinsPerHour(60) * SecsPerMin(60);
  SecsPerDay    = 86400;
{ Days between TDateTime basis (12/31/1899) and Unix time_t basis (1/1/1970) }
  UnixDateDelta = 25569;
begin
  Result := AValue / SecsPerDay + UnixDateDelta;
end;

{Converts a TDateTime value into a Unix/Linux-style date-and-time value.

Description
Call DateTimeToUnix to convert a TDateTime value the corresponding Unix/Linux encoding of the same date and time.
Unix/Linux date-and-time values are encoded as the number of seconds since midnight at the start of January 1, 1970.}
function DateTimeToUnix(const AValue: TDateTime): Int64;
const
  SecsPerDay    = 86400;
  UnixDateDelta = 25569;
begin
  Result := Round((AValue - UnixDateDelta) * SecsPerDay);
end;

{ TThreadTimerTrigger }
Procedure TThreadTimerTrigger.Trigger;
Begin
  If Assigned(fOnTrigger) then
    fOnTrigger(Owner);
End;

procedure TThreadTimerTrigger.Execute;
Var
  cnt:integer;
Begin
  While Not Terminated Do Begin
    Cnt := 10;
    While Cnt > 0 do Begin
      Dec(Cnt);
      Sleep(fInterval div 10);
      If Terminated then Exit;
    End;
    Synchronize(Trigger);
  End;
End;

Procedure TThreadTimer.SetActive(Const IsActive:Boolean);
Begin
  If fActive = IsActive then Exit;
  fActive := IsActive;
  If fActive then
    fTrigger.Resume
  Else
    fTrigger.Suspend;
End;

Procedure TThreadTimer.SetInterval(Const aInterval:LongInt);
Begin
  if fInterval = aInterval then Exit;
  fInterval := aInterval;
  fTrigger.Interval := aInterval;
End;

Procedure TThreadTimer.SetOnTimer(Const aOnTimer:TNotifyEvent);
Begin
  fOnTimer := aOnTimer;
  fTrigger.OnTrigger := aOnTimer;
End;

constructor TThreadTimer.Create;
Begin
  inherited Create;
  fActive := False;
  fTrigger := TThreadTimerTrigger.Create(True);
  fTrigger.FreeOnTerminate := True;
  fTrigger.Owner := Self;
  Interval := 100;
  OnTimer := nil;
End;

destructor TThreadTimer.Destroy;
Begin
  fTrigger.Terminate;
  if not fTrigger.Suspended then
    fTrigger.WaitFor;
//  fTrigger.Free;
  inherited;
End;

end.
