unit magtapiapi ;
{$IFNDEF VER140}
  {$WARN UNSAFE_TYPE off}
  {$WARN UNSAFE_CAST off}
  {$WARN UNSAFE_CODE off}
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

// Magenta TAPI defintions, Magenta Systems Ltd
// 3rd September 2012 - Release 5.71

// Copyright by Angus Robertson, Magenta Systems Ltd, England
// delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/

// Changes in 1.2 ===
// changed all Longint to DWORD

// Changes in 1.3
// renamed from tapi_api.pas

// Changes in 1.5
// added TRasEntry 'szDeviceType' strings returned as devicetype
// added lineGetID

// Changes in 1.6
// added lineGetTranslateCaps, lineGetCountry and related stuff

// Changes in 4.4 - just version for compatibility with TMagRas

// Changes in 4.60
// Added new RAS literal for Windows XP

// Changes in 5.40
// Made compatible with Delphi 2009, but still using ANSI RAS functions, not Unicode

//Changes in 5.70

interface
    uses Windows, Messages, SysUtils ;

const
//TAPI_CURRENT_VERSION = $00020000;  // Windows NT4 SP3
//TAPI_CURRENT_VERSION = $00020001;  // Windows 98, NT4 SP4 (patch for 95)
//TAPI_CURRENT_VERSION = $00020002;  // Windows 2K
  TAPI_CURRENT_VERSION = $00010004;  // Windows 95
  TAPI_MIN_VERSION =     $00010000;
  TAPI_MAX_VERSION =     $10000000;
  TapiDLL = 'TAPI32.DLL';

// NDIS on NT4 uses version 10003, modems are version 10004

{ Type definitions of the data types used in tapi }

type
  LPCall = ^TCall;
  TCall = DWORD;
  LPHLine = ^TLine;
  TLine = DWORD;
  LPPhone = ^TPhone;
  TPhone = DWORD;
  LPLineApp = ^TLineApp;
  TLineApp = DWORD;
  LPPhoneApp = ^TPhoneApp;
  TPhoneApp = DWORD;
  LPIcon = ^HIcon;


TLineCallback = procedure (hDevice, dwMessage, dwInstance,
                            dwParam1, dwParam2, dwParam3: DWORD);  stdcall;

 { Messages for Lines }

const
  LINE_ADDRESSSTATE                       = 0;
  LINE_CALLINFO                           = 1;
  LINE_CALLSTATE                          = 2;
  LINE_CLOSE                              = 3;
  LINE_DEVSPECIFIC                        = 4;
  LINE_DEVSPECIFICFEATURE                 = 5;
  LINE_GATHERDIGITS                       = 6;
  LINE_GENERATE                           = 7;
  LINE_LINEDEVSTATE                       = 8;
  LINE_MONITORDIGITS                      = 9;
  LINE_MONITORMEDIA                       = 10;
  LINE_MONITORTONE                        = 11;
  LINE_REPLY                              = 12;
  LINE_REQUEST                            = 13;
  PHONE_BUTTON                            = 14;
  PHONE_CLOSE                             = 15;
  PHONE_DEVSPECIFIC                       = 16;
  PHONE_REPLY                             = 17;
  PHONE_STATE                             = 18;
  LINE_CREATE                             = 19;             { TAPI v1.4 }
  PHONE_CREATE                            = 20;             { TAPI v1.4 }

  INITIALIZE_NEGOTIATION                  = $FFFFFFFF;

  LINEADDRCAPFLAGS_FWDNUMRINGS            = $00000001;
  LINEADDRCAPFLAGS_PICKUPGROUPID          = $00000002;
  LINEADDRCAPFLAGS_SECURE                 = $00000004;
  LINEADDRCAPFLAGS_BLOCKIDDEFAULT         = $00000008;
  LINEADDRCAPFLAGS_BLOCKIDOVERRIDE        = $00000010;
  LINEADDRCAPFLAGS_DIALED                 = $00000020;
  LINEADDRCAPFLAGS_ORIGOFFHOOK            = $00000040;
  LINEADDRCAPFLAGS_DESTOFFHOOK            = $00000080;
  LINEADDRCAPFLAGS_FWDCONSULT             = $00000100;
  LINEADDRCAPFLAGS_SETUPCONFNULL          = $00000200;
  LINEADDRCAPFLAGS_AUTORECONNECT          = $00000400;
  LINEADDRCAPFLAGS_COMPLETIONID           = $00000800;
  LINEADDRCAPFLAGS_TRANSFERHELD           = $00001000;
  LINEADDRCAPFLAGS_TRANSFERMAKE           = $00002000;
  LINEADDRCAPFLAGS_CONFERENCEHELD         = $00004000;
  LINEADDRCAPFLAGS_CONFERENCEMAKE         = $00008000;
  LINEADDRCAPFLAGS_PARTIALDIAL            = $00010000;
  LINEADDRCAPFLAGS_FWDSTATUSVALID         = $00020000;
  LINEADDRCAPFLAGS_FWDINTEXTADDR          = $00040000;
  LINEADDRCAPFLAGS_FWDBUSYNAADDR          = $00080000;
  LINEADDRCAPFLAGS_ACCEPTTOALERT          = $00100000;
  LINEADDRCAPFLAGS_CONFDROP               = $00200000;
  LINEADDRCAPFLAGS_PICKUPCALLWAIT         = $00400000;

  LINEADDRESSMODE_ADDRESSID               = $00000001;
  LINEADDRESSMODE_DIALABLEADDR            = $00000002;

  LINEADDRESSSHARING_PRIVATE              = $00000001;
  LINEADDRESSSHARING_BRIDGEDEXCL          = $00000002;
  LINEADDRESSSHARING_BRIDGEDNEW           = $00000004;
  LINEADDRESSSHARING_BRIDGEDSHARED        = $00000008;
  LINEADDRESSSHARING_MONITORED            = $00000010;

  LINEADDRESSSTATE_OTHER                  = $00000001;
  LINEADDRESSSTATE_DEVSPECIFIC            = $00000002;
  LINEADDRESSSTATE_INUSEZERO              = $00000004;
  LINEADDRESSSTATE_INUSEONE               = $00000008;
  LINEADDRESSSTATE_INUSEMANY              = $00000010;
  LINEADDRESSSTATE_NUMCALLS               = $00000020;
  LINEADDRESSSTATE_FORWARD                = $00000040;
  LINEADDRESSSTATE_TERMINALS              = $00000080;
  LINEADDRESSSTATE_CAPSCHANGE             = $00000100;      { TAPI v1.4 }

  LINEADDRFEATURE_FORWARD                 = $00000001;
  LINEADDRFEATURE_MAKECALL                = $00000002;
  LINEADDRFEATURE_PICKUP                  = $00000004;
  LINEADDRFEATURE_SETMEDIACONTROL         = $00000008;
  LINEADDRFEATURE_SETTERMINAL             = $00000010;
  LINEADDRFEATURE_SETUPCONF               = $00000020;
  LINEADDRFEATURE_UNCOMPLETECALL          = $00000040;
  LINEADDRFEATURE_UNPARK                  = $00000080;

  LINEANSWERMODE_NONE                     = $00000001;
  LINEANSWERMODE_DROP                     = $00000002;
  LINEANSWERMODE_HOLD                     = $00000004;

  LINEBEARERMODE_VOICE                    = $00000001;
  LINEBEARERMODE_SPEECH                   = $00000002;
  LINEBEARERMODE_MULTIUSE                 = $00000004;
  LINEBEARERMODE_DATA                     = $00000008;
  LINEBEARERMODE_ALTSPEECHDATA            = $00000010;
  LINEBEARERMODE_NONCALLSIGNALING         = $00000020;
  LINEBEARERMODE_PASSTHROUGH              = $00000040;      { TAPI v1.4 }

  LINEBUSYMODE_STATION                    = $00000001;
  LINEBUSYMODE_TRUNK                      = $00000002;
  LINEBUSYMODE_UNKNOWN                    = $00000004;
  LINEBUSYMODE_UNAVAIL                    = $00000008;

  LINECALLCOMPLCOND_BUSY                  = $00000001;
  LINECALLCOMPLCOND_NOANSWER              = $00000002;

  LINECALLCOMPLMODE_CAMPON                = $00000001;
  LINECALLCOMPLMODE_CALLBACK              = $00000002;
  LINECALLCOMPLMODE_INTRUDE               = $00000004;
  LINECALLCOMPLMODE_MESSAGE               = $00000008;

  LINECALLFEATURE_ACCEPT                  = $00000001;
  LINECALLFEATURE_ADDTOCONF               = $00000002;
  LINECALLFEATURE_ANSWER                  = $00000004;
  LINECALLFEATURE_BLINDTRANSFER           = $00000008;
  LINECALLFEATURE_COMPLETECALL            = $00000010;
  LINECALLFEATURE_COMPLETETRANSF          = $00000020;
  LINECALLFEATURE_DIAL                    = $00000040;
  LINECALLFEATURE_DROP                    = $00000080;
  LINECALLFEATURE_GATHERDIGITS            = $00000100;
  LINECALLFEATURE_GENERATEDIGITS          = $00000200;
  LINECALLFEATURE_GENERATETONE            = $00000400;
  LINECALLFEATURE_HOLD                    = $00000800;
  LINECALLFEATURE_MONITORDIGITS           = $00001000;
  LINECALLFEATURE_MONITORMEDIA            = $00002000;
  LINECALLFEATURE_MONITORTONES            = $00004000;
  LINECALLFEATURE_PARK                    = $00008000;
  LINECALLFEATURE_PREPAREADDCONF          = $00010000;
  LINECALLFEATURE_REDIRECT                = $00020000;
  LINECALLFEATURE_REMOVEFROMCONF          = $00040000;
  LINECALLFEATURE_SECURECALL              = $00080000;
  LINECALLFEATURE_SENDUSERUSER            = $00100000;
  LINECALLFEATURE_SETCALLPARAMS           = $00200000;
  LINECALLFEATURE_SETMEDIACONTROL         = $00400000;
  LINECALLFEATURE_SETTERMINAL             = $00800000;
  LINECALLFEATURE_SETUPCONF               = $01000000;
  LINECALLFEATURE_SETUPTRANSFER           = $02000000;
  LINECALLFEATURE_SWAPHOLD                = $04000000;
  LINECALLFEATURE_UNHOLD                  = $08000000;
  LINECALLFEATURE_RELEASEUSERUSERINFO     = $10000000;      { TAPI v1.4 }

  LINECALLINFOSTATE_OTHER                 = $00000001;
  LINECALLINFOSTATE_DEVSPECIFIC           = $00000002;
  LINECALLINFOSTATE_BEARERMODE            = $00000004;
  LINECALLINFOSTATE_RATE                  = $00000008;
  LINECALLINFOSTATE_MEDIAMODE             = $00000010;
  LINECALLINFOSTATE_APPSPECIFIC           = $00000020;
  LINECALLINFOSTATE_CALLID                = $00000040;
  LINECALLINFOSTATE_RELATEDCALLID         = $00000080;
  LINECALLINFOSTATE_ORIGIN                = $00000100;
  LINECALLINFOSTATE_REASON                = $00000200;
  LINECALLINFOSTATE_COMPLETIONID          = $00000400;
  LINECALLINFOSTATE_NUMOWNERINCR          = $00000800;
  LINECALLINFOSTATE_NUMOWNERDECR          = $00001000;
  LINECALLINFOSTATE_NUMMONITORS           = $00002000;
  LINECALLINFOSTATE_TRUNK                 = $00004000;
  LINECALLINFOSTATE_CALLERID              = $00008000;
  LINECALLINFOSTATE_CALLEDID              = $00010000;
  LINECALLINFOSTATE_CONNECTEDID           = $00020000;
  LINECALLINFOSTATE_REDIRECTIONID         = $00040000;
  LINECALLINFOSTATE_REDIRECTINGID         = $00080000;
  LINECALLINFOSTATE_DISPLAY               = $00100000;
  LINECALLINFOSTATE_USERUSERINFO          = $00200000;
  LINECALLINFOSTATE_HIGHLEVELCOMP         = $00400000;
  LINECALLINFOSTATE_LOWLEVELCOMP          = $00800000;
  LINECALLINFOSTATE_CHARGINGINFO          = $01000000;
  LINECALLINFOSTATE_TERMINAL              = $02000000;
  LINECALLINFOSTATE_DIALPARAMS            = $04000000;
  LINECALLINFOSTATE_MONITORMODES          = $08000000;

  LINECALLORIGIN_OUTBOUND                 = $00000001;
  LINECALLORIGIN_INTERNAL                 = $00000002;
  LINECALLORIGIN_EXTERNAL                 = $00000004;
  LINECALLORIGIN_UNKNOWN                  = $00000010;
  LINECALLORIGIN_UNAVAIL                  = $00000020;
  LINECALLORIGIN_CONFERENCE               = $00000040;
  LINECALLORIGIN_INBOUND                  = $00000080;      { TAPI v1.4 }

  LINECALLPARAMFLAGS_SECURE               = $00000001;
  LINECALLPARAMFLAGS_IDLE                 = $00000002;
  LINECALLPARAMFLAGS_BLOCKID              = $00000004;
  LINECALLPARAMFLAGS_ORIGOFFHOOK          = $00000008;
  LINECALLPARAMFLAGS_DESTOFFHOOK          = $00000010;

  LINECALLPARTYID_BLOCKED                 = $00000001;
  LINECALLPARTYID_OUTOFAREA               = $00000002;
  LINECALLPARTYID_NAME                    = $00000004;
  LINECALLPARTYID_ADDRESS                 = $00000008;
  LINECALLPARTYID_PARTIAL                 = $00000010;
  LINECALLPARTYID_UNKNOWN                 = $00000020;
  LINECALLPARTYID_UNAVAIL                 = $00000040;

  LINECALLPRIVILEGE_NONE                  = $00000001;
  LINECALLPRIVILEGE_MONITOR               = $00000002;
  LINECALLPRIVILEGE_OWNER                 = $00000004;

  LINECALLREASON_DIRECT                   = $00000001;
  LINECALLREASON_FWDBUSY                  = $00000002;
  LINECALLREASON_FWDNOANSWER              = $00000004;
  LINECALLREASON_FWDUNCOND                = $00000008;
  LINECALLREASON_PICKUP                   = $00000010;
  LINECALLREASON_UNPARK                   = $00000020;
  LINECALLREASON_REDIRECT                 = $00000040;
  LINECALLREASON_CALLCOMPLETION           = $00000080;
  LINECALLREASON_TRANSFER                 = $00000100;
  LINECALLREASON_REMINDER                 = $00000200;
  LINECALLREASON_UNKNOWN                  = $00000400;
  LINECALLREASON_UNAVAIL                  = $00000800;
  LINECALLREASON_INTRUDE                  = $00001000;      { TAPI v1.4 }
  LINECALLREASON_PARKED                   = $00002000;      { TAPI v1.4 }

  LINECALLSELECT_LINE                     = $00000001;
  LINECALLSELECT_ADDRESS                  = $00000002;
  LINECALLSELECT_CALL                     = $00000004;

  LINECALLSTATE_IDLE                      = $00000001;
  LINECALLSTATE_OFFERING                  = $00000002;
  LINECALLSTATE_ACCEPTED                  = $00000004;
  LINECALLSTATE_DIALTONE                  = $00000008;
  LINECALLSTATE_DIALING                   = $00000010;
  LINECALLSTATE_RINGBACK                  = $00000020;
  LINECALLSTATE_BUSY                      = $00000040;
  LINECALLSTATE_SPECIALINFO               = $00000080;
  LINECALLSTATE_CONNECTED                 = $00000100;
  LINECALLSTATE_PROCEEDING                = $00000200;
  LINECALLSTATE_ONHOLD                    = $00000400;
  LINECALLSTATE_CONFERENCED               = $00000800;
  LINECALLSTATE_ONHOLDPENDCONF            = $00001000;
  LINECALLSTATE_ONHOLDPENDTRANSFER        = $00002000;
  LINECALLSTATE_DISCONNECTED              = $00004000;
  LINECALLSTATE_UNKNOWN                   = $00008000;


  LINECARDOPTION_PREDEFINED               = $00000001;      { TAPI v1.4 }
  LINECARDOPTION_HIDDEN                   = $00000002;      { TAPI v1.4 }

  LINECONNECTEDMODE_ACTIVE                = $00000001;      { TAPI v1.4 }
  LINECONNECTEDMODE_INACTIVE              = $00000002;      { TAPI v1.4 }

  LINEDEVCAPFLAGS_CROSSADDRCONF           = $00000001;
  LINEDEVCAPFLAGS_HIGHLEVCOMP             = $00000002;
  LINEDEVCAPFLAGS_LOWLEVCOMP              = $00000004;
  LINEDEVCAPFLAGS_MEDIACONTROL            = $00000008;
  LINEDEVCAPFLAGS_MULTIPLEADDR            = $00000010;
  LINEDEVCAPFLAGS_CLOSEDROP               = $00000020;
  LINEDEVCAPFLAGS_DIALBILLING             = $00000040;
  LINEDEVCAPFLAGS_DIALQUIET               = $00000080;
  LINEDEVCAPFLAGS_DIALDIALTONE            = $00000100;

  LINEDEVSTATE_OTHER                      = $00000001;
  LINEDEVSTATE_RINGING                    = $00000002;
  LINEDEVSTATE_CONNECTED                  = $00000004;
  LINEDEVSTATE_DISCONNECTED               = $00000008;
  LINEDEVSTATE_MSGWAITON                  = $00000010;
  LINEDEVSTATE_MSGWAITOFF                 = $00000020;
  LINEDEVSTATE_INSERVICE                  = $00000040;
  LINEDEVSTATE_OUTOFSERVICE               = $00000080;
  LINEDEVSTATE_MAINTENANCE                = $00000100;
  LINEDEVSTATE_OPEN                       = $00000200;
  LINEDEVSTATE_CLOSE                      = $00000400;
  LINEDEVSTATE_NUMCALLS                   = $00000800;
  LINEDEVSTATE_NUMCOMPLETIONS             = $00001000;
  LINEDEVSTATE_TERMINALS                  = $00002000;
  LINEDEVSTATE_ROAMMODE                   = $00004000;
  LINEDEVSTATE_BATTERY                    = $00008000;
  LINEDEVSTATE_SIGNAL                     = $00010000;
  LINEDEVSTATE_DEVSPECIFIC                = $00020000;
  LINEDEVSTATE_REINIT                     = $00040000;
  LINEDEVSTATE_LOCK                       = $00080000;
  LINEDEVSTATE_CAPSCHANGE                 = $00100000;      { TAPI v1.4 }
  LINEDEVSTATE_CONFIGCHANGE               = $00200000;      { TAPI v1.4 }
  LINEDEVSTATE_TRANSLATECHANGE            = $00400000;      { TAPI v1.4 }
  LINEDEVSTATE_COMPLCANCEL                = $00800000;      { TAPI v1.4 }
  LINEDEVSTATE_REMOVED                    = $01000000;      { TAPI v1.4 }

  LINEDEVSTATUSFLAGS_CONNECTED            = $00000001;
  LINEDEVSTATUSFLAGS_MSGWAIT              = $00000002;
  LINEDEVSTATUSFLAGS_INSERVICE            = $00000004;
  LINEDEVSTATUSFLAGS_LOCKED               = $00000008;

  LINEDIALTONEMODE_NORMAL                 = $00000001;
  LINEDIALTONEMODE_SPECIAL                = $00000002;
  LINEDIALTONEMODE_INTERNAL               = $00000004;
  LINEDIALTONEMODE_EXTERNAL               = $00000008;
  LINEDIALTONEMODE_UNKNOWN                = $00000010;
  LINEDIALTONEMODE_UNAVAIL                = $00000020;

  LINEDIGITMODE_PULSE                     = $00000001;
  LINEDIGITMODE_DTMF                      = $00000002;
  LINEDIGITMODE_DTMFEND                   = $00000004;

  LINEDISCONNECTMODE_NORMAL               = $00000001;
  LINEDISCONNECTMODE_UNKNOWN              = $00000002;
  LINEDISCONNECTMODE_REJECT               = $00000004;
  LINEDISCONNECTMODE_PICKUP               = $00000008;
  LINEDISCONNECTMODE_FORWARDED            = $00000010;
  LINEDISCONNECTMODE_BUSY                 = $00000020;
  LINEDISCONNECTMODE_NOANSWER             = $00000040;
  LINEDISCONNECTMODE_BADADDRESS           = $00000080;
  LINEDISCONNECTMODE_UNREACHABLE          = $00000100;
  LINEDISCONNECTMODE_CONGESTION           = $00000200;
  LINEDISCONNECTMODE_INCOMPATIBLE         = $00000400;
  LINEDISCONNECTMODE_UNAVAIL              = $00000800;
  LINEDISCONNECTMODE_NODIALTONE           = $00001000;      { TAPI v1.4 }

  LINEERR_ALLOCATED                       = $80000001;
  LINEERR_BADDEVICEID                     = $80000002;
  LINEERR_BEARERMODEUNAVAIL               = $80000003;
  LINEERR_CALLUNAVAIL                     = $80000005;
  LINEERR_COMPLETIONOVERRUN               = $80000006;
  LINEERR_CONFERENCEFULL                  = $80000007;
  LINEERR_DIALBILLING                     = $80000008;
  LINEERR_DIALDIALTONE                    = $80000009;
  LINEERR_DIALPROMPT                      = $8000000A;
  LINEERR_DIALQUIET                       = $8000000B;
  LINEERR_INCOMPATIBLEAPIVERSION          = $8000000C;
  LINEERR_INCOMPATIBLEEXTVERSION          = $8000000D;
  LINEERR_INIFILECORRUPT                  = $8000000E;
  LINEERR_INUSE                           = $8000000F;
  LINEERR_INVALADDRESS                    = $80000010;
  LINEERR_INVALADDRESSID                  = $80000011;
  LINEERR_INVALADDRESSMODE                = $80000012;
  LINEERR_INVALADDRESSSTATE               = $80000013;
  LINEERR_INVALAPPHANDLE                  = $80000014;
  LINEERR_INVALAPPNAME                    = $80000015;
  LINEERR_INVALBEARERMODE                 = $80000016;
  LINEERR_INVALCALLCOMPLMODE              = $80000017;
  LINEERR_INVALCALLHANDLE                 = $80000018;
  LINEERR_INVALCALLPARAMS                 = $80000019;
  LINEERR_INVALCALLPRIVILEGE              = $8000001A;
  LINEERR_INVALCALLSELECT                 = $8000001B;
  LINEERR_INVALCALLSTATE                  = $8000001C;
  LINEERR_INVALCALLSTATELIST              = $8000001D;
  LINEERR_INVALCARD                       = $8000001E;
  LINEERR_INVALCOMPLETIONID               = $8000001F;
  LINEERR_INVALCONFCALLHANDLE             = $80000020;
  LINEERR_INVALCONSULTCALLHANDLE          = $80000021;
  LINEERR_INVALCOUNTRYCODE                = $80000022;
  LINEERR_INVALDEVICECLASS                = $80000023;
  LINEERR_INVALDEVICEHANDLE               = $80000024;
  LINEERR_INVALDIALPARAMS                 = $80000025;
  LINEERR_INVALDIGITLIST                  = $80000026;
  LINEERR_INVALDIGITMODE                  = $80000027;
  LINEERR_INVALDIGITS                     = $80000028;
  LINEERR_INVALEXTVERSION                 = $80000029;
  LINEERR_INVALGROUPID                    = $8000002A;
  LINEERR_INVALLINEHANDLE                 = $8000002B;
  LINEERR_INVALLINESTATE                  = $8000002C;
  LINEERR_INVALLOCATION                   = $8000002D;
  LINEERR_INVALMEDIALIST                  = $8000002E;
  LINEERR_INVALMEDIAMODE                  = $8000002F;
  LINEERR_INVALMESSAGEID                  = $80000030;
  LINEERR_INVALPARAM                      = $80000032;
  LINEERR_INVALPARKID                     = $80000033;
  LINEERR_INVALPARKMODE                   = $80000034;
  LINEERR_INVALPOINTER                    = $80000035;
  LINEERR_INVALPRIVSELECT                 = $80000036;
  LINEERR_INVALRATE                       = $80000037;
  LINEERR_INVALREQUESTMODE                = $80000038;
  LINEERR_INVALTERMINALID                 = $80000039;
  LINEERR_INVALTERMINALMODE               = $8000003A;
  LINEERR_INVALTIMEOUT                    = $8000003B;
  LINEERR_INVALTONE                       = $8000003C;
  LINEERR_INVALTONELIST                   = $8000003D;
  LINEERR_INVALTONEMODE                   = $8000003E;
  LINEERR_INVALTRANSFERMODE               = $8000003F;
  LINEERR_LINEMAPPERFAILED                = $80000040;
  LINEERR_NOCONFERENCE                    = $80000041;
  LINEERR_NODEVICE                        = $80000042;
  LINEERR_NODRIVER                        = $80000043;
  LINEERR_NOMEM                           = $80000044;
  LINEERR_NOREQUEST                       = $80000045;
  LINEERR_NOTOWNER                        = $80000046;
  LINEERR_NOTREGISTERED                   = $80000047;
  LINEERR_OPERATIONFAILED                 = $80000048;
  LINEERR_OPERATIONUNAVAIL                = $80000049;
  LINEERR_RATEUNAVAIL                     = $8000004A;
  LINEERR_RESOURCEUNAVAIL                 = $8000004B;
  LINEERR_REQUESTOVERRUN                  = $8000004C;
  LINEERR_STRUCTURETOOSMALL               = $8000004D;
  LINEERR_TARGETNOTFOUND                  = $8000004E;
  LINEERR_TARGETSELF                      = $8000004F;
  LINEERR_UNINITIALIZED                   = $80000050;
  LINEERR_USERUSERINFOTOOBIG              = $80000051;
  LINEERR_REINIT                          = $80000052;
  LINEERR_ADDRESSBLOCKED                  = $80000053;
  LINEERR_BILLINGREJECTED                 = $80000054;
  LINEERR_INVALFEATURE                    = $80000055;
  LINEERR_NOMULTIPLEINSTANCE              = $80000056;

  LINEFEATURE_DEVSPECIFIC                 = $00000001;
  LINEFEATURE_DEVSPECIFICFEAT             = $00000002;
  LINEFEATURE_FORWARD                     = $00000004;
  LINEFEATURE_MAKECALL                    = $00000008;
  LINEFEATURE_SETMEDIACONTROL             = $00000010;
  LINEFEATURE_SETTERMINAL                 = $00000020;

  LINEFORWARDMODE_UNCOND                  = $00000001;
  LINEFORWARDMODE_UNCONDINTERNAL          = $00000002;
  LINEFORWARDMODE_UNCONDEXTERNAL          = $00000004;
  LINEFORWARDMODE_UNCONDSPECIFIC          = $00000008;
  LINEFORWARDMODE_BUSY                    = $00000010;
  LINEFORWARDMODE_BUSYINTERNAL            = $00000020;
  LINEFORWARDMODE_BUSYEXTERNAL            = $00000040;
  LINEFORWARDMODE_BUSYSPECIFIC            = $00000080;
  LINEFORWARDMODE_NOANSW                  = $00000100;
  LINEFORWARDMODE_NOANSWINTERNAL          = $00000200;
  LINEFORWARDMODE_NOANSWEXTERNAL          = $00000400;
  LINEFORWARDMODE_NOANSWSPECIFIC          = $00000800;
  LINEFORWARDMODE_BUSYNA                  = $00001000;
  LINEFORWARDMODE_BUSYNAINTERNAL          = $00002000;
  LINEFORWARDMODE_BUSYNAEXTERNAL          = $00004000;
  LINEFORWARDMODE_BUSYNASPECIFIC          = $00008000;
  LINEFORWARDMODE_UNKNOWN                 = $00010000;      { TAPI v1.4 }
  LINEFORWARDMODE_UNAVAIL                 = $00020000;      { TAPI v1.4 }

  LINEGATHERTERM_BUFFERFULL               = $00000001;
  LINEGATHERTERM_TERMDIGIT                = $00000002;
  LINEGATHERTERM_FIRSTTIMEOUT             = $00000004;
  LINEGATHERTERM_INTERTIMEOUT             = $00000008;
  LINEGATHERTERM_CANCEL                   = $00000010;

  LINEGENERATETERM_DONE                   = $00000001;
  LINEGENERATETERM_CANCEL                 = $00000002;

  LINELOCATIONOPTION_PULSEDIAL            = $00000001;     { TAPI v1.4 }

  LINEMAPPER                              = $FFFFFFFF;

  LINEMEDIACONTROL_NONE                   = $00000001;
  LINEMEDIACONTROL_START                  = $00000002;
  LINEMEDIACONTROL_RESET                  = $00000004;
  LINEMEDIACONTROL_PAUSE                  = $00000008;
  LINEMEDIACONTROL_RESUME                 = $00000010;
  LINEMEDIACONTROL_RATEUP                 = $00000020;
  LINEMEDIACONTROL_RATEDOWN               = $00000040;
  LINEMEDIACONTROL_RATENORMAL             = $00000080;
  LINEMEDIACONTROL_VOLUMEUP               = $00000100;
  LINEMEDIACONTROL_VOLUMEDOWN             = $00000200;
  LINEMEDIACONTROL_VOLUMENORMAL           = $00000400;

  LINEMEDIAMODE_UNKNOWN                   = $00000002;
  LINEMEDIAMODE_INTERACTIVEVOICE          = $00000004;
  LINEMEDIAMODE_AUTOMATEDVOICE            = $00000008;
  LINEMEDIAMODE_DATAMODEM                 = $00000010;
  LINEMEDIAMODE_G3FAX                     = $00000020;
  LINEMEDIAMODE_TDD                       = $00000040;
  LINEMEDIAMODE_G4FAX                     = $00000080;
  LINEMEDIAMODE_DIGITALDATA               = $00000100;
  LINEMEDIAMODE_TELETEX                   = $00000200;
  LINEMEDIAMODE_VIDEOTEX                  = $00000400;
  LINEMEDIAMODE_TELEX                     = $00000800;
  LINEMEDIAMODE_MIXED                     = $00001000;
  LINEMEDIAMODE_ADSI                      = $00002000;
  LINEMEDIAMODE_VOICEVIEW                 = $00004000;      { TAPI v1.4 }
  LAST_LINEMEDIAMODE                      = $00004000;

  LINEOFFERINGMODE_ACTIVE                 = $00000001;      { TAPI v1.4 }
  LINEOFFERINGMODE_INACTIVE               = $00000002;      { TAPI v1.4 }

  LINEREMOVEFROMCONF_NONE                 = $00000001;
  LINEREMOVEFROMCONF_LAST                 = $00000002;
  LINEREMOVEFROMCONF_ANY                  = $00000003;

  LINEREQUESTMODE_MAKECALL                = $00000001;
  LINEREQUESTMODE_MEDIACALL               = $00000002;
  LINEREQUESTMODE_DROP                    = $00000004;
  LAST_LINEREQUESTMODE                    = LINEREQUESTMODE_MEDIACALL;

  LINEROAMMODE_UNKNOWN                    = $00000001;
  LINEROAMMODE_UNAVAIL                    = $00000002;
  LINEROAMMODE_HOME                       = $00000004;
  LINEROAMMODE_ROAMA                      = $00000008;
  LINEROAMMODE_ROAMB                      = $00000010;

  LINESPECIALINFO_NOCIRCUIT               = $00000001;
  LINESPECIALINFO_CUSTIRREG               = $00000002;
  LINESPECIALINFO_REORDER                 = $00000004;
  LINESPECIALINFO_UNKNOWN                 = $00000008;
  LINESPECIALINFO_UNAVAIL                 = $00000010;

  LINETERMDEV_PHONE                       = $00000001;
  LINETERMDEV_HEADSET                     = $00000002;
  LINETERMDEV_SPEAKER                     = $00000004;

  LINETERMMODE_BUTTONS                    = $00000001;
  LINETERMMODE_LAMPS                      = $00000002;
  LINETERMMODE_DISPLAY                    = $00000004;
  LINETERMMODE_RINGER                     = $00000008;
  LINETERMMODE_HOOKSWITCH                 = $00000010;
  LINETERMMODE_MEDIATOLINE                = $00000020;
  LINETERMMODE_MEDIAFROMLINE              = $00000040;
  LINETERMMODE_MEDIABIDIRECT              = $00000080;

  LINETERMSHARING_PRIVATE                 = $00000001;
  LINETERMSHARING_SHAREDEXCL              = $00000002;
  LINETERMSHARING_SHAREDCONF              = $00000004;

  LINETOLLLISTOPTION_ADD                  = $00000001;
  LINETOLLLISTOPTION_REMOVE               = $00000002;

  LINETONEMODE_CUSTOM                     = $00000001;
  LINETONEMODE_RINGBACK                   = $00000002;
  LINETONEMODE_BUSY                       = $00000004;
  LINETONEMODE_BEEP                       = $00000008;
  LINETONEMODE_BILLING                    = $00000010;

  LINETRANSFERMODE_TRANSFER               = $00000001;
  LINETRANSFERMODE_CONFERENCE             = $00000002;

  LINETRANSLATEOPTION_CARDOVERRIDE        = $00000001;
  LINETRANSLATEOPTION_CANCELCALLWAITING   = $00000002;      { TAPI v1.4 }
  LINETRANSLATEOPTION_FORCELOCAL          = $00000004;      { TAPI v1.4 }
  LINETRANSLATEOPTION_FORCELD             = $00000008;      { TAPI v1.4 }

  LINETRANSLATERESULT_CANONICAL           = $00000001;
  LINETRANSLATERESULT_INTERNATIONAL       = $00000002;
  LINETRANSLATERESULT_LONGDISTANCE        = $00000004;
  LINETRANSLATERESULT_LOCAL               = $00000008;
  LINETRANSLATERESULT_INTOLLLIST          = $00000010;
  LINETRANSLATERESULT_NOTINTOLLLIST       = $00000020;
  LINETRANSLATERESULT_DIALBILLING         = $00000040;
  LINETRANSLATERESULT_DIALQUIET           = $00000080;
  LINETRANSLATERESULT_DIALDIALTONE        = $00000100;
  LINETRANSLATERESULT_DIALPROMPT          = $00000200;

// TRasEntry 'szDeviceType' strings

  RASDT_Modem       = 'modem';
  RASDT_Isdn        = 'isdn';
  RASDT_X25         = 'x25';
  RASDT_Vpn         = 'vpn';
  RASDT_Pad         = 'pad';
  RASDT_Generic     = 'generic';  // following are Windows 2000 only
  RASDT_Serial      = 'serial';
  RASDT_FrameRelay  = 'framerelay';
  RASDT_Atm         = 'atm';
  RASDT_Sonet       = 'sonet';
  RASDT_SW56        = 'sw56';
  RASDT_Irda        = 'irda';
  RASDT_Parallel    = 'parallel';
  RASDT_PPPoE     = 'pppoe' ;   // Windows XP Point-to-Point Protocol over Ethernet

// TAPI device classes
  ClassComm             = 'comm' ;
  ClassCommModem        = 'comm/datamodem' ;
  ClassCommModemPort    = 'comm/datamodem/portname' ;
  ClassNdis             = 'ndis' ;

// TAPI stuff not properly documented, from MSDN Q163236, used in RASENTRY
// Device setting information - Win9x only

// Device Configuration
type
  TDevCfgHdr = Record
    dwSize: DWORD ;
    dwVersion: DWORD ;     // should be $100003
    fwOptions: Word ;      // four flags, see below (terminal, modem lights, etc)
    wWaitBong: Word ;      // number of seconds (2 sec granulity) to replace the wait for credit card tone
  end ;
{  bits for DCB Flags 
    fBinary: 1;     /* Binary Mode (skip EOF check)    */
    fParity: 1;     /* Enable parity checking          */
    fOutxCtsFlow:1; /* CTS handshaking on output       */
    fOutxDsrFlow:1; /* DSR handshaking on output       */
    fDtrControl:2;  /* DTR Flow control                */
    fDsrSensitivity:1; /* DSR Sensitivity              */
    fTXContinueOnXoff: 1; /* Continue TX when Xoff sent */
    fOutX: 1;       /* Enable output X-ON/X-OFF        */
    fInX: 1;        /* Enable input X-ON/X-OFF         */
    fErrorChar: 1;  /* Enable Err Replacement          */
    fNull: 1;       /* Enable Null stripping           */
    fRtsControl:2;  /* Rts Flow control                */
    fAbortOnError:1; /* Abort all reads and writes on Error */
}

// Communications Configuration
// CommConfig may be followed by a Provider structure, typically TModemSettings
  TComm2Config = Record    // different name to avoid conflict with windows.pas
    dwSize: DWORD;
    wVersion: Word;          // probably 1
    wReserved: Word;
    dcb: TDCB;               // modem device control block, baud rate, bits, xon/off, flow control, etc
    dwProviderSubType: DWORD; // PST_Modem (6) or others (in windows.pas)
    dwProviderOffset: DWORD;  // but lets assume it follows immediately and is ModemSettings
    dwProviderSize: DWORD;    // how large
//    wcProviderData: array[0..0] of WCHAR;  // allow modemsettings to follow
  end;

// modem device capabilities
  TModemDevCaps = Record
    case Integer of
    0: (dwActualSize,
    dwRequiredSize,
    dwDevSpecificOffset,
    dwDevSpecificSize,
    // product and version identification
    dwModemProviderVersion,
    dwModemManufacturerOffset,
    dwModemManufacturerSize,
    dwModemModelOffset,
    dwModemModelSize,
    dwModemVersionOffset,
    dwModemVersionSize,
    // local option capabilities
    dwDialOptions,          // bitmap of supported values
    dwCallSetupFailTimer,   // maximum in seconds
    dwInactivityTimeout,    // maximum in seconds
    dwSpeakerVolume,        // bitmap of supported values
    dwSpeakerMode,          // bitmap of supported values
    dwModemOptions,         // bitmap of supported values
    dwMaxDTERate,           // maximum value in bit/s
    dwMaxDCERate: DWORD ;   // maximum value in bit/s
    // Variable portion for proprietary expansion
    abVariablePortion: array [0..0] of AnsiChar) ;      // variable-length data
    1: (Data: array[0..511] of AnsiChar);  // not really sure!!!
  end;

// Modem Configuration
  PTModemSettings = ^TModemSettings ;
  TModemSettings = Record
    dwActualSize,            // size of returned data
    dwRequiredSize,          // structure size
    dwDevSpecificOffset,     // offset of data
    dwDevSpecificSize,       // size of defined data
    // Static local options (read/write)
    dwCallSetupFailTimer,      // call setup timeout in seconds
    dwInactivityTimeout,       // timeout in seconds (dialog shows minutes)
    dwSpeakerVolume,           // speaker volume level
    dwSpeakerMode,             // speaker mode
    dwPreferredModemOptions,   // preferred options
    // Negotiated options (read) for current or last call
    dwNegotiatedModemOptions,  // options
    dwNegotiatedDCERate: DWORD;   // DCE rate
    // Variable portion for proprietary expansion
    abVariablePortion: array [0..0] of AnsiChar ;      // variable-length data
  end ;


// service provider device configuration info for Win9x modems only
// used by lineGet/Set/DevConfig, and RasGet/Set/EntryProperties (deviceinfo)
  PTDevCfg = ^TDevCfg ;
  TDevCfg = Record
      DfgHdr: TDevCfgHdr ;
      CommConfig: TComm2Config;
      ModemSettings: TModemSettings ;
  end ;

const
// dwVersion for TDevCfgHdr
    MDMCFG_VERSION = $10000003 ;    // TAPI 1.3 ??

// fwOption field of DEVCFGHDR
    TERMINAL_PRE    = $00000001 ;   // Displays the pre-terminal screen.
    TERMINAL_POST   = $00000002 ;   // Displays the post-terminal screen.
    MANUAL_DIAL     = $00000004 ;   // Allows manual Dialing, if possible.
    LAUNCH_LIGHTS   = $00000008 ;   // Displays the modem tray icon.

// Flags in DCB
    dcb_Binary              = $00000001;
    dcb_Parity              = $00000002;
    dcb_OutxCtsFlow         = $00000004;
    dcb_OutxDsrFlow         = $00000008;
    dcb_DtrControlEnable    = $00000010;
    dcb_DtrControlHandshake = $00000020;
    dcb_DsrSensitivity      = $00000040;
    dcb_TXContinueOnXoff    = $00000080;
    dcb_OutX                = $00000100;
    dcb_InX                 = $00000200;
    dcb_ErrorChar           = $00000400;
    dcb_Null                = $00000800;
    dcb_RtsFlowEnable       = $00001000;
    dcb_RtsFlowHandshake    = $00002000;
    dcb_RtsFlowToggle       = $00003000;

// DialOptions for MODEMDEVCAPS
    DIALOPTION_BILLING  = $00000040 ; // Supports wait for bong "$"
    DIALOPTION_QUIET    = $00000080 ; // Supports wait for quiet "@"
    DIALOPTION_DIALTONE = $00000100 ; // Supports wait for dial tone "W"

// SpeakerVolume for MODEMDEVCAPS
    MDMVOLFLAG_LOW      = $00000001 ;
    MDMVOLFLAG_MEDIUM   = $00000002 ;
    MDMVOLFLAG_HIGH     = $00000004 ;

// SpeakerVolume for MODEMSETTINGS
    MDMVOL_LOW          = $00000000 ;
    MDMVOL_MEDIUM       = $00000001 ;
    MDMVOL_HIGH         = $00000002 ;

// SpeakerMode for MODEMDEVCAPS
    MDMSPKRFLAG_OFF         = $00000001 ;
    MDMSPKRFLAG_DIAL        = $00000002 ;
    MDMSPKRFLAG_ON          = $00000004 ;
    MDMSPKRFLAG_CALLSETUP   = $00000008 ;

// SpeakerMode for MODEMSETTINGS
    MDMSPKR_OFF         = $00000000 ;
    MDMSPKR_DIAL        = $00000001 ;
    MDMSPKR_ON          = $00000002 ;
    MDMSPKR_CALLSETUP   = $00000003 ;

// ModemOptions for MODEMDEVCAPS and MODEMSETTINGS
    MDM_COMPRESSION      = $00000001 ;
    MDM_ERROR_CONTROL    = $00000002 ;
    MDM_FORCED_EC        = $00000004 ;
    MDM_CELLULAR         = $00000008 ;
    MDM_FLOWCONTROL_HARD = $00000010 ;
    MDM_FLOWCONTROL_SOFT = $00000020 ;
    MDM_CCITT_OVERRIDE   = $00000040 ;
    MDM_SPEED_ADJUST     = $00000080 ;
    MDM_TONE_DIAL        = $00000100 ;
    MDM_BLIND_DIAL       = $00000200 ;
    MDM_V23_OVERRIDE     = $00000400 ;

// default TAPI device configuration for a Win9x modem
  DevCfgDefault: array [0..107] of byte =
       ($6C,0,0,0,3,0,1,0,0,0,8,0,$60,0,0,0,1,0,0,0,
        $1C,0,0,0,0,$C2,1,0,$15,$20,0,0,0,0,$0A,0,$0A,0,8,0,
        0,$11,$13,0,0,0,0,0,6,0,0,0,$30,0,0,0,$30,0,0,0,
        $30,0,0,0,$30,0,0,0,0,0,0,0,0,0,0,0,$3C,0,0,0,
        0,0,0,0,0,0,0,0,1,0,0,0,$D3,1,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0) ;


type

  LPLineAddressCaps = ^TLineAddressCaps;
  TLineAddressCaps = record
    dwTotalSize,
    dwNeededSize,
    dwUsedSize,
    dwLineDeviceID,
    dwAddressSize,
    dwAddressOffset,
    dwDevSpecificSize,
    dwDevSpecificOffset,
    dwAddressSharing,
    dwAddressStates,
    dwCallInfoStates,
    dwCallerIDFlags,
    dwCalledIDFlags,
    dwConnectedIDFlags,
    dwRedirectionIDFlags,
    dwRedirectingIDFlags,
    dwCallStates,
    dwDialToneModes,
    dwBusyModes,
    dwSpecialInfo,
    dwDisconnectModes,
    dwMaxNumActiveCalls,
    dwMaxNumOnHoldCalls,
    dwMaxNumOnHoldPendingCalls,
    dwMaxNumConference,
    dwMaxNumTransConf,
    dwAddrCapFlags,
    dwCallFeatures,
    dwRemoveFromConfCaps,
    dwRemoveFromConfState,
    dwTransferModes,
    dwParkModes,
    dwForwardModes,
    dwMaxForwardEntries,
    dwMaxSpecificEntries,
    dwMinFwdNumRings,
    dwMaxFwdNumRings,
    dwMaxCallCompletions,
    dwCallCompletionConds,
    dwCallCompletionModes,
    dwNumCompletionMessages,
    dwCompletionMsgTextEntrySize,
    dwCompletionMsgTextSize,
    dwCompletionMsgTextOffset,
    dwAddressFeatures: DWORD;                     { TAPI v1.4 }
  end;

  LPLineDialParams = ^TLineDialParams;
  TLineDialParams = record
    dwDialPause,
    dwDialSpeed,
    dwDigitDuration,
    dwWaitForDialtone: DWORD;
  end;

  LPLineCallInfo = ^TLineCallInfo;
  TLineCallInfo = record
  case Integer of
  0: (dwTotalSize,
    dwNeededSize,
    dwUsedSize: DWORD;
    hLine: TLine;
    dwLineDeviceID,
    dwAddressID,
    dwBearerMode,
    dwRate,
    dwMediaMode,
    dwAppSpecific,
    dwCallID,
    dwRelatedCallID,
    dwCallParamFlags,
    dwCallStates,
    dwMonitorDigitModes,
    dwMonitorMediaModes: DWORD;
    DialParams: TLineDialParams;
    dwOrigin,
    dwReason,
    dwCompletionID,
    dwNumOwners,
    dwNumMonitors,
    dwCountryCode,
    dwTrunk,
    dwCallerIDFlags,
    dwCallerIDSize,
    dwCallerIDOffset,
    dwCallerIDNameSize,
    dwCallerIDNameOffset,
    dwCalledIDFlags,
    dwCalledIDSize,
    dwCalledIDOffset,
    dwCalledIDNameSize,
    dwCalledIDNameOffset,
    dwConnectedIDFlags,
    dwConnectedIDSize,
    dwConnectedIDOffset,
    dwConnectedIDNameSize,
    dwConnectedIDNameOffset,
    dwRedirectionIDFlags,
    dwRedirectionIDSize,
    dwRedirectionIDOffset,
    dwRedirectionIDNameSize,
    dwRedirectionIDNameOffset,
    dwRedirectingIDFlags,
    dwRedirectingIDSize,
    dwRedirectingIDOffset,
    dwRedirectingIDNameSize,
    dwRedirectingIDNameOffset,
    dwAppNameSize,
    dwAppNameOffset,
    dwDisplayableAddressSize,
    dwDisplayableAddressOffset,
    dwCalledPartySize,
    dwCalledPartyOffset,
    dwCommentSize,
    dwCommentOffset,
    dwDisplaySize,
    dwDisplayOffset,
    dwUserUserInfoSize,
    dwUserUserInfoOffset,
    dwHighLevelCompSize,
    dwHighLevelCompOffset,
    dwLowLevelCompSize,
    dwLowLevelCompOffset,
    dwChargingInfoSize,
    dwChargingInfoOffset,
    dwTerminalModesSize,
    dwTerminalModesOffset,
    dwDevSpecificSize,
    dwDevSpecificOffset: DWORD;
    EndMark: Integer);
  1: (Data: array[0..1023] of AnsiChar);
  end;

  LPLineCallList = ^TLineCallList;
  TLineCallList = record
  case Integer of
  0: (dwTotalSize,
    dwNeededSize,
    dwUsedSize,
    dwCallsNumEntries,
    dwCallsSize,
    dwCallsOffset: DWORD);
  1: (Data: array[0..49] of AnsiChar);   // array of HCall - only likely to get 1
  end;

  LPLineCallParams = ^TLineCallParams;
  TLineCallParams = record            { Defaults:        }
    dwTotalSize,                    { ---------        }
    dwBearerMode,                   { voice            }
    dwMinRate,                      { (3.1kHz)         }
    dwMaxRate,                      { (3.1kHz)         }
    dwMediaMode,                    { interactiveVoice }
    dwCallParamFlags,               { 0                }
    dwAddressMode,                  { addressID        }
    dwAddressID: DWORD;           { (any available)  }
    DialParams: TLineDialParams;    { (0, 0, 0, 0)     }
    dwOrigAddressSize,              { 0                }
    dwOrigAddressOffset,
    dwDisplayableAddressSize,
    dwDisplayableAddressOffset,
    dwCalledPartySize,              { 0                }
    dwCalledPartyOffset,
    dwCommentSize,                  { 0                }
    dwCommentOffset,
    dwUserUserInfoSize,             { 0                }
    dwUserUserInfoOffset,
    dwHighLevelCompSize,            { 0                }
    dwHighLevelCompOffset,
    dwLowLevelCompSize,             { 0                }
    dwLowLevelCompOffset,
    dwDevSpecificSize,              { 0                }
    dwDevSpecificOffset: DWORD;
  end;

  LPLineCallStatus = ^TLineCallStatus;
  TLineCallStatus = record
  case Integer of
  0: (dwTotalSize,
    dwNeededSize,
    dwUsedSize,
    dwCallState,
    dwCallStateMode,
    dwCallPrivilege,
    dwCallFeatures,
    dwDevSpecificSize,
    dwDevSpecificOffset: DWORD);
  1: (Data: array[0..95] of AnsiChar);
  end;

  LPLineCardEntry = ^TLineCardEntry;
  TLineCardEntry = record
    dwPermanentCardID,
    dwCardNameSize,
    dwCardNameOffset,
    dwCardNumberDigits,                             { TAPI v1.4 }
    dwSameAreaRuleSize,                             { TAPI v1.4 }
    dwSameAreaRuleOffset,                           { TAPI v1.4 }
    dwLongDistanceRuleSize,                         { TAPI v1.4 }
    dwLongDistanceRuleOffset,                       { TAPI v1.4 }
    dwInternationalRuleSize,                        { TAPI v1.4 }
    dwInternationalRuleOffset,                      { TAPI v1.4 }
    dwOptions: DWORD;                               { TAPI v1.4 }
  end;

  LPLineCountryEntry = ^TLineCountryEntry;
  TLineCountryEntry = record
    dwCountryID,                                    { TAPI v1.4 }
    dwCountryCode,                                  { TAPI v1.4 }
    dwNextCountryID,                                { TAPI v1.4 }
    dwCountryNameSize,                              { TAPI v1.4 }
    dwCountryNameOffset,                            { TAPI v1.4 }
    dwSameAreaRuleSize,                             { TAPI v1.4 }
    dwSameAreaRuleOffset,                           { TAPI v1.4 }
    dwLongDistanceRuleSize,                         { TAPI v1.4 }
    dwLongDistanceRuleOffset,                       { TAPI v1.4 }
    dwInternationalRuleSize,                        { TAPI v1.4 }
    dwInternationalRuleOffset: DWORD;
  end ;

  LPLineCountryList = ^TLineCountryList;
  TLineCountryList = record
  case Integer of
  0: (dwTotalSize,                                    { TAPI v1.4 }
    dwNeededSize,                                   { TAPI v1.4 }
    dwUsedSize,                                     { TAPI v1.4 }
    dwNumCountries,                                 { TAPI v1.4 }
    dwCountryListSize,                              { TAPI v1.4 }
    dwCountryListOffset: DWORD);
  1: (Data: array[0..511] of AnsiChar);     // !!! only sufficient space for
  end ;                                 // !!! a few of TLineCountryEntry

  LPLineDevCaps = ^TLineDevCaps;
  TLineDevCaps = record
  case Integer of
  0: (dwTotalSize,
    dwNeededSize,
    dwUsedSize,
    dwProviderInfoSize,
    dwProviderInfoOffset,
    dwSwitchInfoSize,
    dwSwitchInfoOffset,
    dwPermanenTLineID,
    dwLineNameSize,
    dwLineNameOffset,
    dwStringFormat,
    dwAddressModes,
    dwNumAddresses,
    dwBearerModes,
    dwMaxRate,
    dwMediaModes,
    dwGenerateToneModes,
    dwGenerateToneMaxNumFreq,
    dwGenerateDigitModes,
    dwMonitorToneMaxNumFreq,
    dwMonitorToneMaxNumEntries,
    dwMonitorDigitModes,
    dwGatherDigitsMinTimeout,
    dwGatherDigitsMaxTimeout,
    dwMedCtlDigitMaxListSize,
    dwMedCtlMediaMaxListSize,
    dwMedCtlToneMaxListSize,
    dwMedCtlCallStateMaxListSize,
    dwDevCapFlags,
    dwMaxNumActiveCalls,
    dwAnswerMode,
    dwRingModes,
    dwLineStates,
    dwUUIAcceptSize,
    dwUUIAnswerSize,
    dwUUIMakeCallSize,
    dwUUIDropSize,
    dwUUISendUserUserInfoSize,
    dwUUICallInfoSize: DWORD;
    MinDialParams,
    MaxDialParams,
    DefaultDialParams: TLineDialParams;
    dwNumTerminals,
    dwTerminalCapsSize,
    dwTerminalCapsOffset,
    dwTerminalTextEntrySize,
    dwTerminalTextSize,
    dwTerminalTextOffset,
    dwDevSpecificSize,
    dwDevSpecificOffset,
    dwLineFeatures: DWORD;                        { TAPI v1.4 }
    EndMark: Integer);
  1: (Data: array[0..2047] of AnsiChar);
  end;

  LPLineDevStatus = ^TLineDevStatus;
  TLineDevStatus = record
    dwTotalSize,
    dwNeededSize,
    dwUsedSize,
    dwNumOpens,
    dwOpenMediaModes,
    dwNumActiveCalls,
    dwNumOnHoldCalls,
    dwNumOnHoldPendCalls,
    dwLineFeatures,
    dwNumCallCompletions,
    dwRingMode,
    dwSignalLevel,
    dwBatteryLevel,
    dwRoamMode,
    dwDevStatusFlags,
    dwTerminalModesSize,
    dwTerminalModesOffset,
    dwDevSpecificSize,
    dwDevSpecificOffset: DWORD;
  end;

  LPLineExtensionID = ^TLineExtensionID;
  TLineExtensionID = record
    dwExtensionID0,
    dwExtensionID1,
    dwExtensionID2,
    dwExtensionID3: DWORD;
  end;

{  THandleUnion = record
    case Integer of
      0: (hEvent: THandle);
      1: (hCompletionPort: THandle);
  end;

  LPLineInitializeExParams = ^TLineInitializeExParams;
  TLineInitializeExParams = record
    dwTotalSize,
    dwNeededSize,
    dwUsedSize,
    dwOptions: DWORD;
    Handles: THandleUnion;
    dwCompletionKey: DWORD;
  end;  }

  LPLineLocationEntry = ^TLineLocationEntry;
  TLineLocationEntry = record
    dwPermanentLocationID,
    dwLocationNameSize,
    dwLocationNameOffset,
    dwCountryCode,
    dwCityCodeSize,
    dwCityCodeOffset,
    dwPreferredCardID,
    dwLocalAccessCodeSize,                          { TAPI v1.4 }
    dwLocalAccessCodeOffset,                        { TAPI v1.4 }
    dwLongDistanceAccessCodeSize,                   { TAPI v1.4 }
    dwLongDistanceAccessCodeOffset,                 { TAPI v1.4 }
    dwTollPrefixListSize,                           { TAPI v1.4 }
    dwTollPrefixListOffset,                         { TAPI v1.4 }
    dwCountryID,                                    { TAPI v1.4 }
    dwOptions,                                      { TAPI v1.4 }
    dwCancelCallWaitingSize,                        { TAPI v1.4 }
    dwCancelCallWaitingOffset: DWORD;               { TAPI v1.4 }
  end;

  LPLineTranslateCaps = ^TLineTranslateCaps;
  TLineTranslateCaps = record
  case Integer of
  0: (dwTotalSize,
    dwNeededSize,
    dwUsedSize,
    dwNumLocations,
    dwLocationListSize,
    dwLocationListOffset,
    dwCurrentLocationID,
    dwNumCards,
    dwCardListSize,
    dwCardListOffset,
    dwCurrentPreferredCardID: DWORD);
  1: (Data: array[0..3500] of AnsiChar);
  end;

  LPLineTranslateOutput = ^TLineTranslateOutput;
  TLineTranslateOutput = record
  case Integer of
  0: (dwTotalSize,
    dwNeededSize,
    dwUsedSize,
    dwDialableStringSize,
    dwDialableStringOffset,
    dwDisplayableStringSize,
    dwDisplayableStringOffset,
    dwCurrentCountry,
    dwDestCountry,
    dwTranslateResults: DWORD);
  1: (Data: array[0..255] of AnsiChar);
  end;

  LPVarString = ^TVarString;
  TVarString = record
     case integer of
      1: (dwTotalSize,
          dwNeededSize,
          dwUsedSize,
          dwStringFormat,
          dwStringSize,
          dwStringOffset: DWORD);
      2: (Data: array [0..1024] of AnsiChar);
  end;


var

lineClose: function (
  hLine: TLine): DWORD;
  stdcall;

lineConfigDialog: function (
  dwDeviceID: DWORD;
  hwndOwner: HWnd;
  lpszDeviceClass: PAnsiChar): DWORD;
  stdcall;

lineDeallocateCall: function (
    hCall: TCall): DWORD;
  stdcall;
{
lineGetAddressCaps: function (
  hLineApp: TLineApp;
  dwDeviceID: DWORD;
  dwAddressID: DWORD;
  dwAPIVersion: DWORD;
  dwExtVersion: DWORD;
  var lpAddressCaps: TLineAddressCaps): DWORD;
  stdcall;  }

lineGetCallInfo: function (
  hCall: TCall;
  var lpCallInfo: TLineCallInfo): DWORD;
  stdcall;

lineGetCallStatus: function (
  hCall: TCall;
  var lpCallStatus: TLineCallStatus): DWORD;
  stdcall;

lineGetDevCaps: function (
  hLineApp: TLineApp;
  dwDeviceID: DWORD;
  dwAPIVersion: DWORD;
  dwExtVersion: DWORD;
  var lpLineDevCaps: TLineDevCaps): DWORD;
  stdcall;

LineInitialize: function (
    var lphLineApp: TLineApp;
    hInstance: HInst;
    lpfnCallback: TLineCallback;
    lpszAppName: PAnsiChar;
    var lpdwNumDevs: DWORD): DWORD;
    stdcall;
{
LineInitializeEx: function (       // TAPI 2
    var lphLineApp: TLineApp;
    hInstance: HInst;
    lpfnCallback: TLineCallback;
    lpszAppName: PAnsiChar;
    var lpdwNumDevs: DWORD;
    var lpdwAPIVersion: LongInt;
    var lpLineInitializeExParams: TLineInitializeExParams): DWORD;
    stdcall;   }

lineNegotiateAPIVersion: function (
  hLineApp: TLineApp;
  dwDeviceID: DWORD;
  dwAPILowVersion: DWORD;
  dwAPIHighVersion: DWORD;
  var lpdwAPIVersion: DWORD;
  var lpExtensionID: TLineExtensionID): DWORD;
  stdcall;

lineOpen: function (
  hLineApp: TLineApp;
  dwDeviceID: DWORD;
  var lphLine: TLine;
  dwAPIVersion: DWORD;
  dwExtVersion: DWORD;
  dwCallbackInstance: DWORD;
  dwPrivileges: DWORD;
  dwMediaModes: DWORD;
  lpCallParams: LPLineCallParams): DWORD;
  stdcall;

lineShutdown: function (
  hLineApp: TLineApp): DWORD;
  stdcall;

lineTranslateAddress: function (
  hLineApp: TLineApp;
  dwDeviceID: DWORD;
  dwAPIVersion: DWORD;
  lpszAddressIn: PAnsiChar;
  dwCard: DWORD;
  dwTranslateOptions: DWORD;
  var lpTranslateOutput: TLineTranslateOutput): DWORD;
  stdcall;

lineTranslateDialog: function (                       { TAPI v1.4 }
  hLineApp: TLineApp;
  dwDeviceID: DWORD;
  dwAPIVersion: DWORD;
  hwndOwner: HWnd;
  lpszAddressIn: PAnsiChar): DWORD;
  stdcall;

lineGetNewCalls: function (
  hLine: TLine;
  dwAddressID: DWORD;
  dwSelect: DWORD;
  var lpCallList: TLineCallList): DWORD;
  stdcall;

lineGetID: function (               // 1.5
  hLine: TLine;
  dwAddressID: DWORD;
  hCall: TCall;
  dwSelect: DWORD;
  var lpDeviceID: TVarString ;
  lpszDeviceClass: PAnsiChar): DWORD ;
  stdcall;

lineGetTranslateCaps: function (
  hLine: TLine;
  dwAPIVersion: DWORD;
  lpTranslateCaps: PAnsiChar): DWORD;
  stdcall;

lineGetCountry: function (
  dwCountryID,
  dwAPIVersion: DWORD;
  lpLineCountryList: PAnsiChar):DWORD;
  stdcall;


var
    TapiModule: THandle;

    function LoadTAPI: Boolean;

implementation

function LoadTAPI: Boolean;
begin
    Result := True;
    if TapiModule <> 0 then Exit;

// open DLL
    TapiModule := LoadLibrary(TapiDLL);
    if TapiModule = 0 then
    begin
        Result := false;
        exit ;
    end ;
    lineClose := GetProcAddress(TapiModule, 'lineClose');
    lineConfigDialog := GetProcAddress(TapiModule, 'lineConfigDialog');
    lineDeallocateCall := GetProcAddress(TapiModule, 'lineDeallocateCall');
    lineGetCallInfo := GetProcAddress(TapiModule, 'lineGetCallInfo');
    lineGetDevCaps := GetProcAddress(TapiModule, 'lineGetDevCaps');
    lineInitialize := GetProcAddress(TapiModule, 'lineInitialize');
    lineNegotiateApiVersion :=
                    GetProcAddress(TapiModule, 'lineNegotiateAPIVersion');
    lineOpen := GetProcAddress(TapiModule, 'lineOpen');
    lineShutdown := GetProcAddress(TapiModule, 'lineShutdown');
    lineTranslateDialog := GetProcAddress(TapiModule, 'lineTranslateDialog');
    lineGetCallStatus := GetProcAddress(TapiModule, 'lineGetCallStatus');
    lineTranslateAddress := GetProcAddress(TapiModule, 'lineTranslateAddress');
    lineGetNewCalls := GetProcAddress(TapiModule, 'lineGetNewCalls');
    lineGetID := GetProcAddress(TapiModule, 'lineGetID');
    lineGetTranslateCaps := GetProcAddress(TapiModule, 'lineGetTranslateCaps');
    lineGetCountry := GetProcAddress(TapiModule, 'lineGetCountry');
//    lineDrop := GetProcAddress(TapiModule, 'lineDrop');
end;

initialization
    TapiModule := 0 ;
finalization
    if TapiModule <> 0 then
    begin
        FreeLibrary (TapiModule) ;
        TapiModule := 0 ;
    end ;

end.
