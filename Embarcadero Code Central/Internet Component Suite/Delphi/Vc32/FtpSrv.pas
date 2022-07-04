{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TFtpServer class encapsulate the FTP protocol (server side)
              See RFC-959 for a complete protocol description.
Creation:     April 21, 1998
Version:      1.13
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997-2000 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Apr 29, 1998  V0.90 released for beta testing.
May 01, 1998  V0.92 Adapted for Delphi 1.0
May 03, 1998  V0.93 Adapted for Delphi 2.0 and C++Builder
May 04, 1998  V0.94 Use '/' or '\' as path delimiter. Expose only '/' to the
              outside. Stripped any telnet options (IE send two !). Handled
              absolute path. Implemented SIZE and REST commands.
              Added support for UNC (not finished !)
May 06, 1998  V0.95 Corrected spurious 226 message on PASV mode STOR.
              Made GetInteger retunrs a LongInt.
              Use a LongInt for N in CommandPORT (needed for 16 bits)
              Added slash substitution in BuildFilePath command.
Jul 09, 1998  V1.00 Adapted for Delphi 4, removed beta status.
Jul 21, 1998  V1.01 Added OnValidateDele event
              Changed function to get file size (do not open the file)
Feb 14, 1999  V1.02 Replaced straight winsock call by indirect calls thru
              wsocket (this provide runtime link to winsock DLL).
Mar 06, 1999  V1.03 Added code from  Plegge, Steve <jsp@nciinc.com> to add
              APPE, XMKD, KRMD and STRU commands support.
Jul 24, 1999  V1.04 Replaced msgStorDisabled value from '500 Cannot STOR.' to
              '501 Permission Denied' because CuteFTP doesn't like error 500.
              Suggested by Cedric Veilleux <webmaster@smashweb.com>.
Aug 20, 1999  V1.05 Added compile time options. Revised for BCB4.
              Added Addr property to select interface in multihomed computers.
Oct 02, 1999  V1.06 Added OnValidateRnFr and OnValidateRnTo events.
              Initialized Allowed variable to TRUE before triggerValidateDele.
Nov 24, 1999  V1.07 Added MTDM support. Thanks to Bruce Christensen
              <bkc51831234@hotmail.com> for his code.
Jan 24, 2000  V1.08 Patch IE5 bug in file names. Thanks to <dsnake@infonie.fr>
Jun 08, 2000  V1.09 Added 'A N' type for type command for AIX systems.
Oct 25, 2000  V1.10 Exposed list of clients thru Client[] property.
Oct 29, 2000  V1.11 Added IsClient() method.
              Implemented OnValidateRmd event.
Nov 01, 2000  V1.12 Implemented proposals from Carl@Smotricz.com:
              (1) support for MODE command, but only the default do-nothing
              option S. (2) binding the data socket to the local host address
              and port 20 ('ftp-data'). (3) detection of failure to open the
              data connection for STOR or RETR.
              Added option wsoNoReceiveLoop to sockets. See comments in TWSocket
              about this option. Help in very fast LAN.
Nov 11, 2000  V1.13 Checked for DOS attack. Close connection when buffer
              overflow occured. Thanks to Lester <les@lester.co.uk> for finding
              this security hole.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit FtpSrv;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$IFNDEF VER80} { Not for Delphi 1                    }
    {$H+}       { Use long strings                    }
    {$J+}       { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF VER110} { C++ Builder V3.0                    }
    {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER125} { C++ Builder V4.0                    }
    {$ObjExportAll On}
{$ENDIF}

{$DEFINE BIND_FTP_DATA}

interface

uses
    WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
    Dialogs, Winsock, WSocket, FtpSrvC, FtpSrvT;

const
    FtpServerVersion         = 113;
    CopyRight : String       = ' TFtpServer (c) 1998-2000 F. Piette V1.13 ';
    WM_FTPSRV_CLOSE_REQUEST  = WM_USER + 1;
    WM_FTPSRV_CLIENT_CLOSED  = WM_USER + 2;
    WM_FTPSRV_ABORT_TRANSFER = WM_USER + 3;
    WM_FTPSRV_CLOSE_DATA     = WM_USER + 4;

type
    FtpServerException  = class(Exception);
{ Various Delphi and C++Builder version handle string parameter passed as var }
{ differently. To get application code compatible across all versions, we     }
{ need to define our own string type. We use the larger we can with the given }
{ compiler version. btw: the 255 limit is not a problem because it applies to }
{ the command lines sent to the server and 255 should be enough except if     }
{ you use incredibly long file names.                                         }
{$IFDEF VER100}                     { Delphi 3   }
    TFtpString = String;
{$ELSE}                             { All others }
    TFtpString = String[255];
{$ENDIF}
    TFtpCtrlSocketClass = class of TFtpCtrlSocket;
    TFtpSrvAuthenticateEvent  =  procedure (Sender   : TObject;
                                            Client   : TFtpCtrlSocket;
                                            UserName : TFtpString;
                                            Password : TFtpString;
                                            var Authenticated : Boolean) of object;
    TFtpSrvChangeDirectoryEvent =  procedure (Sender      : TObject;
                                              Client      : TFtpCtrlSocket;
                                              Directory   : TFtpString;
                                              var Allowed : Boolean) of object;
    TFtpSrvBuildDirectoryEvent =  procedure (Sender        : TObject;
                                             Client        : TFtpCtrlSocket;
                                             var Directory : TFtpString;
                                             Detailed      : Boolean) of object;
    TFtpSrvClientConnectEvent = procedure (Sender : TObject;
                                           Client : TFtpCtrlSocket;
                                           Error  : Word) of object;
    TFtpSrvDataSessionConnectedEvent = procedure (Sender : TObject;
                                                  Client : TFtpCtrlSocket;
                                                  Data   : TWSocket;
                                                  Error  : Word) of object;
    TFtpSrvClientCommandEvent = procedure (Sender        : TObject;
                                           Client        : TFtpCtrlSocket;
                                           var Keyword   : TFtpString;
                                           var Params    : TFtpString;
                                           var Answer    : TFtpString) of object;
    TFtpSrvAnswerToClientEvent = procedure (Sender        : TObject;
                                            Client        : TFtpCtrlSocket;
                                            var Answer    : TFtpString) of object;
    TFtpSrvValidateXferEvent  = procedure (Sender        : TObject;
                                           Client        : TFtpCtrlSocket;
                                           var FilePath  : TFtpString;
                                           var Allowed   : Boolean) of object;
    TFtpSrvDataAvailableEvent = procedure (Sender : TObject;
                                           Client : TFtpCtrlSocket;
                                           Data   : TWSocket;
                                           Buf    : PChar;
                                           Len    : LongInt;
                                           Error  : Word) of object;
    TFtpSrvRetrDataSentEvent  = procedure (Sender : TObject;
                                           Client : TFtpCtrlSocket;
                                           Data   : TWSocket;
                                           Error  : Word) of object;
    TFtpSrvCommandProc        = procedure (Client        : TFtpCtrlSocket;
                                           var Keyword   : TFtpString;
                                           var Params    : TFtpString;
                                           var Answer    : TFtpString) of object;
    TFtpSrvCommandTableItem   = record
                                    KeyWord : String;
                                    Proc    : TFtpSrvCommandProc;
                                end;
    TFtpServer = class(TComponent)
    protected
        FAddr                   : String;
        FPort                   : String;
        FBanner                 : String;
        FServSocket             : TWSocket;
        FWindowHandle           : HWND;
        FClientClass            : TFtpCtrlSocketClass;
        FClientList             : TList;
        FClientNum              : LongInt;
        FMaxClients             : LongInt;
        FCmdTable               : array [0..31] of TFtpSrvCommandTableItem;
        FLastCmd                : Integer;
        FUserData               : LongInt;      { Reserved for component user }
        FOnStart                : TNotifyEvent;
        FOnStop                 : TNotifyEvent;
        FOnAuthenticate         : TFtpSrvAuthenticateEvent;
        FOnClientConnect        : TFtpSrvClientConnectEvent;
        FOnClientDisconnect     : TFtpSrvClientConnectEvent;
        FOnClientCommand        : TFtpSrvClientCommandEvent;
        FOnAnswerToClient       : TFtpSrvAnswerToClientEvent;
        FOnChangeDirectory      : TFtpSrvChangeDirectoryEvent;
        FOnMakeDirectory        : TFtpSrvChangeDirectoryEvent;
        FOnBuildDirectory       : TFtpSrvBuildDirectoryEvent;
        FOnAlterDirectory       : TFtpSrvBuildDirectoryEvent;
        FOnValidatePut          : TFtpSrvValidateXferEvent;
        FOnValidateDele         : TFtpSrvValidateXferEvent;
        FOnValidateRmd          : TFtpSrvValidateXferEvent;
        FOnValidateRnFr         : TFtpSrvValidateXferEvent;
        FOnValidateRnTo         : TFtpSrvValidateXferEvent;
        FOnStorSessionConnected : TFtpSrvDataSessionConnectedEvent;
        FOnStorSessionClosed    : TFtpSrvDataSessionConnectedEvent;
        FOnStorDataAvailable    : TFtpSrvDataAvailableEvent;
        FOnValidateGet          : TFtpSrvValidateXferEvent;
        FOnRetrSessionConnected : TFtpSrvDataSessionConnectedEvent;
        FOnRetrSessionClosed    : TFtpSrvDataSessionConnectedEvent;
        FOnRetrDataSent         : TFtpSrvRetrDataSentEvent;
        procedure Notification(AComponent: TComponent; operation: TOperation); override;
        procedure ServSocketSessionAvailable(Sender : TObject; Error : Word);
        procedure ServSocketStateChange(Sender : TObject; OldState, NewState : TSocketState);
        procedure ClientSessionClosed(Sender : TObject; Error : Word);
        procedure ClientDataSent(Sender : TObject; Error : Word);
        procedure ClientCommand(Sender : TObject; CmdBuf : PChar; CmdLen : Integer);
        procedure ClientPassiveSessionAvailable(Sender : TObject; Error : Word);
        procedure ClientStorSessionConnected(Sender : TObject; Error : Word);
        procedure ClientStorSessionClosed(Sender : TObject; Error : Word);
        procedure ClientStorDataAvailable(Sender: TObject; Error : word);
        procedure ClientRetrSessionConnected(Sender : TObject; Error : Word);
        procedure ClientRetrSessionClosed(Sender : TObject; Error : Word);
        procedure ClientRetrDataSent(Sender : TObject; Error : Word);
        procedure SendAnswer(Client : TFtpCtrlSocket; Answer : TFtpString);
        procedure SendNextDataChunk(Client : TFtpCtrlSocket; Data : TWSocket);
        procedure StartSendData(Client : TFtpCtrlSocket);
        procedure BuildDirectory(Client : TFtpCtrlSocket; var Params : TFtpString; Stream : TStream; Detailed   : Boolean);
        procedure TriggerServerStart; virtual;
        procedure TriggerServerStop; virtual;
        procedure TriggerAuthenticate(Client            : TFtpCtrlSocket;
                                      UserName          : String;
                                      PassWord          : String;
                                      var Authenticated : Boolean); virtual;
        procedure TriggerChangeDirectory(Client         : TFtpCtrlSocket;
                                         Directory      : String;
                                         var Allowed    : Boolean); virtual;
        procedure TriggerMakeDirectory(Client         : TFtpCtrlSocket;
                                       Directory      : String;
                                       var Allowed    : Boolean); virtual;
        procedure TriggerBuildDirectory(Client        : TFtpCtrlSocket;
                                        var Params    : TFtpString;
                                        Detailed      : Boolean);
        procedure TriggerAlterDirectory(Client        : TFtpCtrlSocket;
                                        var Params    : TFtpString;
                                        Detailed      : Boolean);
        procedure TriggerSendAnswer(Client : TFtpCtrlSocket;
                                    var Answer : TFtpString); virtual;
        procedure TriggerClientConnect(Client : TFtpCtrlSocket; Error : Word); virtual;
        procedure TriggerClientDisconnect(Client : TFtpCtrlSocket; Error : Word); virtual;
        procedure TriggerClientCommand(Client      : TFtpCtrlSocket;
                                       var Keyword : TFtpString;
                                       var Params  : TFtpString;
                                       var Answer  : TFtpString); virtual;
        procedure TriggerStorSessionConnected(Client : TFtpCtrlSocket;
                                              Data   : TWSocket;
                                              Error  : Word); virtual;
        procedure TriggerStorSessionClosed(Client : TFtpCtrlSocket;
                                           Data   : TWSocket;
                                           Error  : Word); virtual;
        procedure TriggerValidatePut(Client        : TFtpCtrlSocket;
                                     var FilePath  : TFtpString;
                                     var Allowed   : Boolean); virtual;
        procedure TriggerValidateDele(Client        : TFtpCtrlSocket;
                                      var FilePath  : TFtpString;
                                      var Allowed   : Boolean); virtual;
        procedure TriggerValidateRmd(Client        : TFtpCtrlSocket;
                                     var FilePath  : TFtpString;
                                     var Allowed   : Boolean); virtual;
        procedure TriggerValidateRnFr(Client        : TFtpCtrlSocket;
                                      var FilePath  : TFtpString;
                                      var Allowed   : Boolean); virtual;
        procedure TriggerValidateRnTo(Client        : TFtpCtrlSocket;
                                      var FilePath  : TFtpString;
                                      var Allowed   : Boolean); virtual;
        procedure TriggerRetrSessionConnected(Client : TFtpCtrlSocket;
                                              Data   : TWSocket;
                                              Error  : Word); virtual;
        procedure TriggerRetrSessionClosed(Client : TFtpCtrlSocket;
                                           Data   : TWSocket;
                                           Error  : Word); virtual;
        procedure TriggerValidateGet(Client        : TFtpCtrlSocket;
                                     var FilePath  : TFtpString;
                                     var Allowed   : Boolean); virtual;
        procedure TriggerStorDataAvailable(Client : TFtpCtrlSocket;
                                       Data   : TWSocket;
                                       Buf    : PChar;
                                       Len    : LongInt;
                                       Error  : Word); virtual;
        procedure TriggerRetrDataSent(Client : TFtpCtrlSocket;
                                       Data   : TWSocket;
                                       Error  : Word); virtual;
        function  GetClientCount : Integer; virtual;
        function  GetClient(nIndex : Integer) : TFtpCtrlSocket; virtual;
        function  GetActive : Boolean;
        procedure SetActive(newValue : Boolean);
        procedure AddCommand(const Keyword : String;
                             const Proc : TFtpSrvCommandProc); virtual;
        procedure WMFtpSrvCloseRequest(var msg: TMessage);
                                       message WM_FTPSRV_CLOSE_REQUEST;
        procedure WMFtpSrvClientClosed(var msg: TMessage);
                                       message WM_FTPSRV_CLIENT_CLOSED;
        procedure WMFtpSrvAbortTransfer(var msg: TMessage);
                                       message WM_FTPSRV_ABORT_TRANSFER;
        procedure WMFtpSrvCloseData(var msg: TMessage);
                                       message WM_FTPSRV_CLOSE_DATA;
        procedure CommandDirectory(Client      : TFtpCtrlSocket;
                                   var Keyword : TFtpString;
                                   var Params  : TFtpString;
                                   var Answer  : TFtpString;
                                   Detailed    : Boolean);
        procedure CommandUSER(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandPASS(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandQUIT(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandNOOP(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandLIST(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandNLST(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandDELE(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSIZE(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandREST(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandRNFR(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandRNTO(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandPORT(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSTOR(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandRETR(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandTYPE(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandCWD (Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandChangeDir(Client : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandMKD (Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandRMD (Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandCDUP(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandXPWD(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandPWD (Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSYST(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandABOR(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandPASV(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandAPPE(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSTRU(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandMDTM(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandMODE(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandOverflow(Client  : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   Start;
        procedure   Stop;
        procedure   DisconnectAll;
        procedure WndProc(var MsgRec: TMessage);
        { Check  if a given object is one of our clients }
        function    IsClient(SomeThing : TObject) : Boolean;
        property  ServSocket    : TWSocket            read  FServSocket;
        property  Handle        : HWND                read  FWindowHandle;
        property  ClientCount   : Integer             read  GetClientCount;
        property  Active        : Boolean             read  GetActive
                                                      write SetActive;
        property  ClientClass            : TFtpCtrlSocketClass
                                                      read  FClientClass
                                                      write FClientClass;
        { Client[] give direct access to anyone of our clients }
        property  Client[nIndex : Integer] : TFtpCtrlSocket
                                                      read  GetClient;
    published
        property  Addr                   : String     read  FAddr
                                                      write FAddr;
        property  Port                   : String     read  FPort
                                                      write FPort;
        property  Banner                 : String     read  FBanner
                                                      write FBanner;
        property  UserData               : LongInt    read  FUserData
                                                      write FUserData;
        property  MaxClients             : LongInt    read  FMaxClients
                                                      write FMaxClients;
        property  OnStart                : TNotifyEvent
                                                      read  FOnStart
                                                      write FOnStart;
        property  OnStop                 : TNotifyEvent
                                                      read  FOnStop
                                                      write FOnStop;
        property  OnAuthenticate         : TFtpSrvAuthenticateEvent
                                                      read  FOnAuthenticate
                                                      write FOnAuthenticate;
        property  OnClientDisconnect     : TFtpSrvClientConnectEvent
                                                      read  FOnClientDisconnect
                                                      write FOnClientDisconnect;
        property  OnClientConnect        : TFtpSrvClientConnectEvent
                                                      read  FOnClientConnect
                                                      write FOnClientConnect;
        property  OnClientCommand        : TFtpSrvClientCommandEvent
                                                      read  FOnClientCommand
                                                      write FOnClientCommand;
        property  OnAnswerToClient       : TFtpSrvAnswerToClientEvent
                                                      read  FOnAnswerToClient
                                                      write FOnAnswerToClient;
        property  OnChangeDirectory      : TFtpSrvChangeDirectoryEvent
                                                      read  FOnChangeDirectory
                                                      write FOnChangeDirectory;
        property  OnMakeDirectory        : TFtpSrvChangeDirectoryEvent
                                                      read  FOnMakeDirectory
                                                      write FOnMakeDirectory;
        property  OnBuildDirectory       : TFtpSrvBuildDirectoryEvent
                                                      read  FOnBuildDirectory
                                                      write FOnBuildDirectory;
        property  OnAlterDirectory       : TFtpSrvBuildDirectoryEvent
                                                      read  FOnAlterDirectory
                                                      write FOnAlterDirectory;
        property  OnStorSessionConnected : TFtpSrvDataSessionConnectedEvent
                                                      read  FOnStorSessionConnected
                                                      write FOnStorSessionConnected;
        property  OnRetrSessionConnected : TFtpSrvDataSessionConnectedEvent
                                                      read  FOnRetrSessionConnected
                                                      write FOnRetrSessionConnected;
        property  OnStorSessionClosed    : TFtpSrvDataSessionConnectedEvent
                                                      read  FOnStorSessionClosed
                                                      write FOnStorSessionClosed;
        property  OnRetrSessionClosed    : TFtpSrvDataSessionConnectedEvent
                                                      read  FOnRetrSessionClosed
                                                      write FOnRetrSessionClosed;
        property  OnRetrDataSent         : TFtpSrvRetrDataSentEvent
                                                      read  FOnRetrDataSent
                                                      write FOnRetrDataSent;
        property  OnValidatePut          : TFtpSrvValidateXferEvent
                                                      read  FOnValidatePut
                                                      write FOnValidatePut;
        property  OnValidateDele         : TFtpSrvValidateXferEvent
                                                      read  FOnValidateDele
                                                      write FOnValidateDele;
        property  OnValidateRmd          : TFtpSrvValidateXferEvent
                                                      read  FOnValidateRmd
                                                      write FOnValidateRmd;
        property  OnValidateRnFr         : TFtpSrvValidateXferEvent
                                                      read  FOnValidateRnFr
                                                      write FOnValidateRnFr;
        property  OnValidateRnTo         : TFtpSrvValidateXferEvent
                                                      read  FOnValidateRnTo
                                                      write FOnValidateRnTo;
        property  OnValidateGet          : TFtpSrvValidateXferEvent
                                                      read  FOnValidateGet
                                                      write FOnValidateGet;
        property  OnStorDataAvailable    : TFtpSrvDataAvailableEvent
                                                      read  FOnStorDataAvailable
                                                      write FOnStorDataAvailable;
    end;

procedure Register;

implementation

const
    msgDftBanner      = '220 ICS FTP Server ready.';
    msgTooMuchClients = '421 Too many users connected.';
    msgCmdUnknown     = '500 ''%s'': command not understood.';
    msgLoginFailed    = '530 Login incorrect.';
    msgNotLogged      = '530 Please login with USER and PASS.';
    msgNoUser         = '503 Login with USER first.';
    msgLogged         = '230 User %s logged in.';
    msgPassRequired   = '331 Password required for %s.';
    msgCWDSuccess     = '250 CWD command successful. "%s" is current directory.';
    msgCWDFailed      = '501 CWD failed. %s';
    msgPWDSuccess     = '257 "%s" is current directory.';
    msgQuit           = '221 Goodbye.';
    msgPortSuccess    = '200 Port command successful.';
    msgPortFailed     = '501 Invalid PORT command.';
    msgStorDisabled   = '501 Permission Denied'; {'500 Cannot STOR.';}
    msgStorSuccess    = '150 Opening data connection for %s.';
    msgStorFailed     = '501 Cannot STOR. %s';
    msgStorAborted    = '426 Connection closed; %s.';
    msgStorOk         = '226 File received ok';
    msgStorError      = '426 Connection closed; transfer aborted. Error #%d';
    msgRetrDisabled   = '500 Cannot RETR.';
    msgRetrSuccess    = '150 Opening data connection for %s.';
    msgRetrFailed     = '501 Cannot RETR. %s';
    msgRetrAborted    = '426 Connection closed; %s.';
    msgRetrOk         = '226 File sent ok';
    msgRetrError      = '426 Connection closed; transfer aborted. Error #%d';
    msgSystem         = '215 UNIX Type: L8 Internet Component Suite';
    msgDirOpen        = '150 Opening data connection for directory list.';
    msgDirFailed      = '451 Failed: %s.';
    msgTypeOk         = '200 Type set to %s.';
    msgTypeFailed     = '500 ''TYPE %s'': command not understood.';
    msgDeleNotExists  = '550 ''%s'': no such file or directory.';
    msgDeleOk         = '250 File ''%s'' deleted.';
    msgDeleFailed     = '450 File ''%s'' can''t be deleted.';
    msgDeleSyntax     = '501 Syntax error in parameter.';
    msgDeleDisabled   = '550 Cannot delete.';
    msgRnfrNotExists  = '550 ''%s'': no such file or directory.';
    msgRnfrSyntax     = '501 Syntax error is parameter.';
    msgRnfrOk         = '350 File exists, ready for destination name.';
    msgRnFrDisabled   = '500 Cannot RNFR.';
    msgRntoNotExists  = '550 ''%s'': no such file or directory.';
    msgRntoAlready    = '553 ''%s'': file already exists.';
    msgRntoOk         = '250 File ''%s'' renamed to ''%s''.';
    msgRntoFailed     = '450 File ''%s'' can''t be renamed.';
    msgRntoSyntax     = '501 Syntax error in parameter.';
    msgRnToDisabled   = '500 Cannot RNTO.';
    msgMkdOk          = '257 ''%s'': directory created.';
    msgMkdAlready     = '550 ''%s'': file or directory already exists.';
    msgMkdFailed      = '550 ''%s'': can''t create directory.';
    msgMkdSyntax      = '501 Syntax error in parameter.';
    msgRmdOk          = '250 ''%s'': directory removed.';
    msgRmdNotExists   = '550 ''%s'': no such directory.';
    msgRmdFailed      = '550 ''%s'': can''t remove directory.';
    msgRmdDisabled    = '500 Cannot remove directory.';
    msgRmdSyntax      = '501 Syntax error in parameter.';
    msgNoopOk         = '200 Ok. Parameter was ''%s''.';
    msgAborOk         = '225 ABOR command successful.';
    msgPasvLocal      = '227 Entering Passive Mode (127,0,0,1,%d,%d).';
    msgPasvRemote     = '227 Entering Passive Mode (%d,%d,%d,%d,%d,%d).';
    msgPasvExcept     = '500 PASV exception: ''%s''.';
    msgSizeOk         = '213 %d';
    msgSizeFailed     = '550 Command failed: %s.';
    msgSizeSyntax     = '501 Syntax error in parameter.';
    msgRestOk         = '350 REST supported. Ready to resume at byte offset %d.';
    msgRestZero       = '501 Required byte offset parameter bad or missing.';
    msgRestFailed     = '501 Syntax error in parameter: %s.';
    msgAppeFailed     = '550 APPE failed.';
    msgAppeSuccess    = '150 Opening data connection for %s (append).';
    msgAppeDisabled   = '500 Cannot APPE.';
    msgAppeAborted    = '426 Connection closed; %s.';
    msgAppeOk         = '226 File received ok';
    msgAppeError      = '426 Connection closed; transfer aborted. Error #%d';
    msgAppeReady      = '150 APPE supported.  Ready to append file "%s" at offset %d.';
    msgStruOk         = '200 Ok. STRU parameter ''%s'' ignored.';
    msgMdtmOk         = '213 %s';
    msgMdtmFailed     = '550 %s';
    msgMdtmSyntax     = '501 Syntax error in MDTM parameter.';
    msgMdtmNotExists  = '550 ''%s'': no such file or directory.';
    msgModeOK         = '200 MODE Ok';
    msgModeSyntax     = '501 Missing argument for MODE';
    msgModeNotS       = '502 MODE other than S not supported';
    msgOverflow       = '500 Command too long';

function SlashesToBackSlashes(const S : String) : String; forward;
function BackSlashesToSlashes(const S : String) : String; forward;
function BuildFilePath(const Directory : String;
                       FileName        : String) : String; forward;

var
    ThisYear, ThisMonth, ThisDay : Word;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [TFtpServer]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
procedure SetLength(var S: string; NewLength: Integer);
begin
    S[0] := chr(NewLength);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimRight(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] = ' ') do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimLeft(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetFileSize(FileName : String) : LongInt;
var
    SR : TSearchRec;
begin
{$WARNINGS OFF}
    if FindFirst(FileName, faReadOnly or faHidden or
                 faSysFile or faArchive, SR) = 0 then
        Result := SR.Size
    else
        Result := -1;
    FindClose(SR);
{$WARNINGS ON}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TFtpServer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FWindowHandle    := AllocateHWnd(WndProc);
    FServSocket      := TWSocket.Create(Self);
    FServSocket.Name := 'ServerWSocket';
    FClientList      := TList.Create;
    FPort            := 'ftp';
    FAddr            := '0.0.0.0';
    FBanner          := msgDftBanner;
    FClientClass     := TFtpCtrlSocket;
    AddCommand('PORT', CommandPORT);
    AddCommand('STOR', CommandSTOR);
    AddCommand('RETR', CommandRETR);
    AddCommand('CWD',  CommandCWD);
    AddCommand('XPWD', CommandXPWD);
    AddCommand('PWD',  CommandPWD);
    AddCommand('USER', CommandUSER);
    AddCommand('PASS', CommandPASS);
    AddCommand('LIST', CommandLIST);
    AddCommand('NLST', CommandNLST);
    AddCommand('TYPE', CommandTYPE);
    AddCommand('SYST', CommandSYST);
    AddCommand('QUIT', CommandQUIT);
    AddCommand('DELE', CommandDELE);
    AddCommand('SIZE', CommandSIZE);
    AddCommand('REST', CommandREST);
    AddCommand('RNFR', CommandRNFR);
    AddCommand('RNTO', CommandRNTO);
    AddCommand('MKD',  CommandMKD);
    AddCommand('RMD',  CommandRMD);
    AddCommand('ABOR', CommandABOR);
    AddCommand('PASV', CommandPASV);
    AddCommand('NOOP', CommandNOOP);
    AddCommand('CDUP', CommandCDUP);
    AddCommand('APPE', CommandAPPE);
    AddCommand('STRU', CommandSTRU);
    AddCommand('XMKD', CommandMKD);
    AddCommand('XRMD', CommandRMD);
    AddCommand('MDTM', CommandMDTM);
    AddCommand('MODE', CommandMODE);
    AddCommand('OVER', CommandOverflow);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TFtpServer.Destroy;
begin
    if Assigned(FServSocket) then begin
        FServSocket.Destroy;
        FServSocket := nil;
    end;
    if Assigned(FClientList) then begin
        FClientList.Destroy;
        FClientList := nil;
    end;
    DeallocateHWnd(FWindowHandle);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.WndProc(var MsgRec: TMessage);
begin
    with MsgRec do begin
        case Msg of
        WM_FTPSRV_CLOSE_REQUEST  : WMFtpSrvCloseRequest(MsgRec);
        WM_FTPSRV_CLIENT_CLOSED  : WMFtpSrvClientClosed(MsgRec);
        WM_FTPSRV_ABORT_TRANSFER : WMFtpSrvAbortTransfer(MsgRec);
        WM_FTPSRV_CLOSE_DATA     : WMFtpSrvCloseData(MsgRec);
        else
            Result := DefWindowProc(Handle, Msg, wParam, lParam);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.WMFtpSrvCloseRequest(var msg: TMessage);
var
    Client : TFtpCtrlSocket;
begin
    Client := TFtpCtrlSocket(msg.LParam);
    if Client.AllSent then
        Client.Close
    else
        Client.CloseRequest := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.Notification(AComponent: TComponent; operation: TOperation);
begin
    inherited Notification(AComponent, operation);
    if operation = opRemove then begin
        if AComponent = FServSocket then
            FServSocket := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.AddCommand(
    const Keyword : String;
    const Proc    : TFtpSrvCommandProc);
begin
    if FLastCmd > High(FCmdTable) then
        raise FtpServerException.Create('Too many command');
    FCmdTable[FLastCmd].KeyWord := KeyWord;
    FCmdTable[FLastCmd].Proc    := Proc;
    Inc(FLastCmd);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.Start;
begin
    if FServSocket.State = wsListening then
        Exit;             { Server is already running }
    FServSocket.Port  := Port;
    FServSocket.Proto := 'tcp';
    FServSocket.Addr  := FAddr;
    FServSocket.OnSessionAvailable := ServSocketSessionAvailable;
    FServSocket.OnChangeState      := ServSocketStateChange;
    FServSocket.ComponentOptions   := [wsoNoReceiveLoop];
    FServSocket.Listen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.Stop;
begin
    FServSocket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.DisconnectAll;
var
    Client : TFtpCtrlSocket;
begin
    while FClientList.Count > 0 do begin
        Client := TFtpCtrlSocket(FClientList.Items[0]);
        Client.Close;
        FClientList.Remove(Client);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.GetActive : Boolean;
begin
    Result := (FServSocket.State = wsListening);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.SetActive(newValue : Boolean);
begin
    if newValue then
        Start
    else
        Stop;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ServSocketStateChange(Sender : TObject; OldState, NewState : TSocketState);
begin
    if csDestroying in ComponentState then
        Exit;
    if NewState = wsListening then
        TriggerServerStart
    else if NewState = wsClosed then
        TriggerServerStop;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ServSocketSessionAvailable(Sender : TObject; Error : Word);
var
    Client : TFtpCtrlSocket;
begin
    if Error <> 0 then
        raise FtpServerException.Create('Session available error #' + IntToStr(Error));
    Inc(FClientNum);
    Client                 := FClientClass.Create(Self);
    FClientList.Add(Client);
    Client.Name            := 'ClientWSocket' + IntToStr(FClientNum);
    Client.DataSocket.Name := 'DataWSocket' + IntToStr(FClientNum);
    Client.Banner          := FBanner;
    Client.HSocket         := ServSocket.Accept;
    Client.OnCommand       := ClientCommand;
    Client.OnSessionClosed := ClientSessionClosed;
    Client.OnDataSent      := ClientDataSent;
    TriggerClientConnect(Client, Error);
    { The event handler may have destroyed the client ! }
    if FClientList.IndexOf(Client) < 0 then
        Exit;
    { The event handler may have closed the connection }
    if Client.State <> wsConnected then
        Exit;
    { Ok, the client is still there, process with the connection }
    if (FMaxClients > 0) and (FMaxClients < ClientCount) then begin
        { Sorry, toomuch clients }
        Client.Banner := msgTooMuchClients;
        Client.StartConnection;
        Client.Close;
    end
    else
        Client.StartConnection;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.SendAnswer(Client : TFtpCtrlSocket; Answer : TFtpString);
begin
    try
        TriggerSendAnswer(Client, Answer);
        Client.SendAnswer(Answer);
    except
        { Just ignore any exception here }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientCommand(
    Sender : TObject;
    CmdBuf : PChar;
    CmdLen : Integer);
const
    TELNET_IAC       = #255;
    TELNET_IP        = #244;
    TELNET_DATA_MARK = #242;
var
    Client  : TFtpCtrlSocket;
    Answer  : TFtpString;
    Params  : TFtpString;
    KeyWord : TFtpString;
    I, J    : Integer;
begin
    Client := Sender as TFtpCtrlSocket;
    Answer := '';

    { Copy the command received, removing any telnet option }
    try
        Params := '';
        I      := 0;
        while I < CmdLen do begin
            if CmdBuf[I] <> TELNET_IAC then begin
                Params := Params + CmdBuf[I];
                Inc(I);
            end
            else begin
                Inc(I);
                if CmdBuf[I] = TELNET_IAC then
                    Params := Params + CmdBuf[I];
                Inc(I);
            end;
        end;

        { Extract keyword, ignoring leading spaces and tabs }
        I := 1;
        while (I <= Length(Params)) and (Params[I] in [' ', #9]) do
            Inc(I);
        J := I;
        while (J <= Length(Params)) and (Params[J] in ['A'..'Z', 'a'..'z', '0'..'9']) do
            Inc(J);
        KeyWord := UpperCase(Copy(Params, I, J - I));

        { Extract parameters, ignoring leading spaces and tabs }
        while (J <= Length(Params)) and (Params[J] in [' ', #9]) do
            Inc(J);
        Params := Copy(Params, J, Length(Params));

        { Pass the command to the component user to let him a chance to }
        { handle it. If it does, he must return the answer.             }
        TriggerClientCommand(Client, Keyword, Params, Answer);
        if Answer <> '' then begin
            { Event handler has processed the client command, send the answer }
            SendAnswer(Client, Answer);
            Exit;
        end;

        { The command has not been processed, we'll process it }
        if Keyword = '' then begin
            { Empty keyword (should never occurs) }
            SendAnswer(Client, Format(msgCmdUnknown, [Params]));
            Exit;
        end;

        { We need to process the client command, search our command table }
        I := 0;
        while I <= High(FCmdTable) do begin
            if FCmdTable[I].KeyWord = KeyWord then begin
                FCmdTable[I].Proc(Client, KeyWord, Params, Answer);
                SendAnswer(Client, Answer);
                Exit;
            end;
            Inc(I);
        end;
        SendAnswer(Client, Format(msgCmdUnknown, [KeyWord]));
    except
        on E:Exception do begin
            SendAnswer(Client, '501 ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientDataSent(Sender : TObject; Error : Word);
var
    Client  : TFtpCtrlSocket;
begin
    Client := Sender as TFtpCtrlSocket;
    if Client.CloseRequest then begin
        Client.CloseRequest := FALSE;
        PostMessage(FWindowHandle, WM_FTPSRV_CLOSE_REQUEST, 0, LongInt(Client));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientSessionClosed(Sender : TObject; Error : Word);
begin
    PostMessage(FWindowHandle, WM_FTPSRV_CLIENT_CLOSED, 0, LongInt(Sender));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.WMFtpSrvClientClosed(var msg: TMessage);
var
    Client : TFtpCtrlSocket;
begin
    Client := TFtpCtrlSocket(Msg.LParam);
    try
        FClientList.Remove(Client);
        TriggerClientDisconnect(Client, Error);
    finally
        Client.Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.WMFtpSrvAbortTransfer(var msg: TMessage);
var
    Data : TWSocket;
begin
    Data := TWSocket(Msg.LParam);
    Data.ShutDown(2);
    Data.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.WMFtpSrvCloseData(var msg: TMessage);
var
    Data : TWSocket;
begin
    if msg.WParam > 0 then begin
{$IFNDEF VER80}
        Sleep(0);  { Release time slice }
{$ENDIF}
        PostMessage(FWindowHandle, Msg.Msg, msg.WParam - 1, msg.LParam);
    end
    else begin
        Data := TWSocket(Msg.LParam);
        Data.Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.GetClient(nIndex : Integer) : TFtpCtrlSocket;
begin
    if not Assigned(FClientList) then begin
        Result := nil;
        Exit;
    end;
    if (nIndex < 0) or (nIndex >= FClientList.Count) then begin
        Result := nil;
        Exit;
    end;
    Result := TFtpCtrlSocket(FClientList.Items[nIndex]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Check  if a given object is one of our clients }
function TFtpServer.IsClient(SomeThing : TObject) : Boolean;
begin
    if not Assigned(FClientList) then
        Result := FALSE
    else
        Result := (FClientList.IndexOf(Pointer(SomeThing)) >= 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.GetClientCount : Integer;
begin
    if Assigned(FClientList) then
        Result := FClientList.Count
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerServerStart;
begin
    if Assigned(FOnStart) then
        FOnStart(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerServerStop;
begin
    if Assigned(FOnStop) then
        FOnStop(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerAuthenticate(
    Client            : TFtpCtrlSocket;
    UserName          : String;
    PassWord          : String;
    var Authenticated : Boolean);
begin
    if Assigned(FOnAuthenticate) then
        FOnAuthenticate(Self, Client, UserName, Password, Authenticated);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerChangeDirectory(
    Client         : TFtpCtrlSocket;
    Directory      : String;
    var Allowed    : Boolean);
begin
    if Assigned(FOnChangeDirectory) then
        FOnChangeDirectory(Self, Client, Directory, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerMakeDirectory(
    Client         : TFtpCtrlSocket;
    Directory      : String;
    var Allowed    : Boolean);
begin
    if Assigned(FOnMakeDirectory) then
        FOnMakeDirectory(Self, Client, Directory, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerBuildDirectory(
    Client        : TFtpCtrlSocket;
    var Params    : TFtpString;
    Detailed      : Boolean);
begin
    if Assigned(FOnBuildDirectory) then
        FOnBuildDirectory(Self, Client, Params, Detailed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerAlterDirectory(
    Client        : TFtpCtrlSocket;
    var Params    : TFtpString;
    Detailed      : Boolean);
begin
    if Assigned(FOnAlterDirectory) then
        FOnAlterDirectory(Self, Client, Params, Detailed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerSendAnswer(
    Client     : TFtpCtrlSocket;
    var Answer : TFtpString);
begin
    if Assigned(FOnAnswerToClient) then
        FOnAnswerToClient(Self, Client, Answer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerClientDisconnect(Client : TFtpCtrlSocket; Error : Word);
begin
    if Assigned(FOnClientDisconnect) then
        FOnClientDisconnect(Self, Client, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerClientConnect(Client : TFtpCtrlSocket; Error : Word);
begin
    if Assigned(FOnClientConnect) then
        FOnClientConnect(Self, Client, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerStorSessionConnected(
    Client : TFtpCtrlSocket; Data : TWSocket; Error : Word);
begin
    if Assigned(FOnStorSessionConnected) then
        FOnStorSessionConnected(Self, Client, Data, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerRetrSessionConnected(
    Client : TFtpCtrlSocket; Data : TWSocket; Error : Word);
begin
    if Assigned(FOnRetrSessionConnected) then
        FOnRetrSessionConnected(Self, Client, Data, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerStorSessionClosed(
    Client : TFtpCtrlSocket; Data : TWSocket; Error : Word);
begin
    if Assigned(FOnStorSessionClosed) then
        FOnStorSessionClosed(Self, Client, Data, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerRetrSessionClosed(
    Client : TFtpCtrlSocket; Data : TWSocket; Error : Word);
begin
    if Assigned(FOnRetrSessionClosed) then
        FOnRetrSessionClosed(Self, Client, Data, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerClientCommand(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Assigned(FOnClientCommand) then
        FOnClientCommand(Self, Client, KeyWord, Params, Answer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidatePut(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidatePut) then
        FOnValidatePut(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateDele(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidateDele) then
        FOnValidateDele(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateRmd(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidateRmd) then
        FOnValidateRmd(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateRnFr(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidateRnFr) then
        FOnValidateRnFr(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateRnTo(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidateRnTo) then
        FOnValidateRnTo(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateGet(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidateGet) then
        FOnValidateGet(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerStorDataAvailable(
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
    Buf    : PChar;
    Len    : LongInt;
    Error  : Word);
begin
    if Assigned(FOnStorDataAvailable) then
        FOnStorDataAvailable(Self, Client, Data, Buf, Len, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerRetrDataSent(
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
    Error  : Word);
begin
    if Assigned(FOnRetrDataSent) then
        FOnRetrDataSent(Self, Client, Data, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandUSER(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    Client.CurCmdType := ftpcUSER;
    Client.UserName   := Trim(Params);
    Client.FtpState   := ftpcWaitingPassword;
    Answer            := Format(msgPassRequired, [Client.UserName]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandPASS(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Authenticated : Boolean;
begin
    if Client.FtpState <> ftpcWaitingPassword then
        Answer := msgNoUser
    else begin
        Client.CurCmdType    := ftpcPASS;
        Client.PassWord      := Trim(Params);
        Authenticated        := TRUE;
        TriggerAuthenticate(Client, Client.UserName, Client.PassWord, Authenticated);
        if Authenticated then begin
            Client.FtpState  := ftpcReady;
            Client.Directory := Client.HomeDir;
            Answer           := Format(msgLogged, [Client.UserName])
        end
        else begin
            Client.FtpState  := ftpcWaitingUserCode;
            Answer           := msgLoginFailed;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandCDUP(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcCDUP;
    Params := '..';
    CommandChangeDir(Client, Keyword, Params, Answer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandCWD(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType    := ftpcCWD;
    CommandChangeDir(Client, Keyword, Params, Answer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SlashesToBackSlashes(const S : String) : String;
var
    I : Integer;
begin
    Result := S;
    for I := 1 to Length(Result) do begin
        if Result [I] = '/' then
            Result[I] := '\';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BackSlashesToSlashes(const S : String) : String;
var
    I : Integer;
begin
    Result := S;
    for I := 1 to Length(Result) do begin
        if Result [I] = '\' then
            Result[I] := '/';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandChangeDir(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Allowed : Boolean;
    OldDir  : String;
begin
    OldDir := Client.Directory;
    try
        Params               := SlashesToBackSlashes(Params);
        Client.Directory     := Trim(Params);
        Allowed              := TRUE;
        TriggerChangeDirectory(Client, Client.Directory, Allowed);
        if Allowed then
            Answer           := Format(msgCWDSuccess,
                                       [BackSlashesToSlashes(Client.Directory)])
        else begin
            Client.Directory := OldDir;
            Answer           := Format(msgCWDFailed, ['No permission']);
        end;
    except
        on E:Exception do begin
            Client.Directory := OldDir;
            Answer           := Format(msgCWDFailed, [E.Message]);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandXPWD(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcXPWD;
    Answer            := Format(msgPWDSuccess,
                                [BackSlashesToSlashes(Client.Directory)]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandPWD(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcPWD;
    Answer            := Format(msgPWDSuccess,
                                [BackSlashesToSlashes(Client.Directory)]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandQUIT(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    Client.CurCmdType := ftpcQUIT;
    Answer            := msgQuit;
    PostMessage(FWindowHandle, WM_FTPSRV_CLOSE_REQUEST, 0, LongInt(Client));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetInteger(var I : Integer; const Src : String) : LongInt;
begin
    { Skip leading white spaces }
    while (I <= Length(Src)) and (Src[I] in [' ' , #9]) do
        Inc(I);
    Result := 0;
    while (I <= Length(Src)) and (Src[I] in ['0'..'9']) do begin
        Result := Result * 10 + Ord(Src[I]) - Ord('0');
        Inc(I);
    end;
    { Skip trailing white spaces }
    while (I <= Length(Src)) and (Src[I] in [' ' , #9]) do
        Inc(I);
    { Check if end of string of comma. If not, error, returns -1 }
    if I <= Length(Src) then begin
        if Src[I] = ',' then
            Inc(I)        { skip comma           }
        else
            raise Exception.Create('unexpected char'); { error, must be comma }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandPORT(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    I : Integer;
    N : LongInt;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    try
        Client.CurCmdType := ftpcPORT;
        I                 := 1;
        Client.DataAddr   := IntToStr(GetInteger(I, Params));
        Client.DataAddr   := Client.DataAddr + '.' + IntToStr(GetInteger(I, Params));
        Client.DataAddr   := Client.DataAddr + '.' + IntToStr(GetInteger(I, Params));
        Client.DataAddr   := Client.DataAddr + '.' + IntToStr(GetInteger(I, Params));
        N := GetInteger(I, Params);
        N := (N shl 8) + GetInteger(I, Params);
        Client.DataPort := IntToStr(N);
        Answer := msgPortSuccess;
    except
        Answer := msgPortFailed;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSTOR(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Allowed  : Boolean;
    FilePath : TFtpString;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    try
        Client.CurCmdType       := ftpcSTOR;
        Client.FileName         := SlashesToBackSlashes(Params);
        Client.HasOpenedFile    := FALSE;
        Client.AbortingTransfer := FALSE;
        Client.TransferError    := 'Transfer Ok';
        Allowed                 := TRUE;
        FilePath                := BuildFilePath(Client.Directory, Client.FileName);
        TriggerValidatePut(Client, FilePath, Allowed);
        if not Allowed then begin
            Answer := msgStorDisabled;
            Exit;
        end;
        Client.FilePath := FilePath;
        if Client.PassiveMode then begin
            Client.DataSocket.OnSessionConnected  := ClientStorSessionConnected;
            Client.DataSocket.OnSessionClosed     := ClientStorSessionClosed;
            Client.DataSocket.OnDataAvailable     := ClientStorDataAvailable;
            Client.DataSocket.OnDataSent          := nil;
            if Client.PassiveConnected then
                Client.DataSocket.OnSessionConnected(Client.DataSocket, 0)
            else
                Client.PassiveStart := TRUE;
        end
        else begin
            Client.DataSocket.Proto               := 'tcp';
            Client.DataSocket.Addr                := Client.DataAddr;
            Client.DataSocket.Port                := Client.DataPort;
            Client.DataSocket.OnSessionConnected  := ClientStorSessionConnected;
            Client.DataSocket.OnSessionClosed     := ClientStorSessionClosed;
            Client.DataSocket.OnDataAvailable     := ClientStorDataAvailable;
            Client.DataSocket.OnDataSent          := nil;
            Client.DataSocket.LingerOnOff         := wsLingerOff;
            Client.DataSocket.LingerTimeout       := 0;
{$IFDEF BIND_FTP_DATA}
            Client.DataSocket.LocalAddr           := Client.GetXAddr;
            Client.DataSocket.LocalPort           := 'ftp-data'; {20}
{$ENDIF}
            Client.DataSocket.ComponentOptions    := [wsoNoReceiveLoop];
            Client.DataSocket.Connect;
        end;
        Answer := Format(msgStorSuccess, [Params]);
    except
        on E:Exception do begin
            Answer := Format(msgStorFailed, [E.Message]);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientStorSessionConnected(Sender : TObject; Error : Word);
var
    Client      : TFtpCtrlSocket;
    Data        : TWSocket;
begin
    Data                     := TWSocket(Sender);
    Client                   := TFtpCtrlSocket(Data.Owner);
    Client.DataSessionActive := TRUE;
    Client.PassiveMode       := FALSE;
    TriggerStorSessionConnected(Client, Data, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientStorSessionClosed(Sender : TObject; Error : Word);
var
    Client      : TFtpCtrlSocket;
    Data        : TWSocket;
begin
    Data                     := TWSocket(Sender);
    Client                   := TFtpCtrlSocket(Data.Owner);
    Client.DataSessionActive := FALSE;
    Client.PassiveStart      := FALSE;
    Client.PassiveConnected  := FALSE;
    Client.RestartPos        := 0;
    { Reset data port to standard value }
    Client.DataPort          := 'ftp-data';

    { If we had opened a data stream ourself, then close it }
    if Client.HasOpenedFile then begin
        if Assigned(Client.DataStream) then
            Client.DataStream.Destroy;
        Client.DataStream    := nil;
        Client.HasOpenedFile := FALSE;
    end;

    TriggerStorSessionClosed(Client, Data, Error);

    if Client.CurCmdType = ftpcSTOR then begin
        if Client.AbortingTransfer then
            SendAnswer(Client, Format(msgStorAborted, [Client.TransferError]))
        else if Error = 0 then
            SendAnswer(Client, msgStorOk)
        else
            SendAnswer(Client, Format(msgStorError, [Error]));
    end
    else if Client.CurCmdType = ftpcAPPE then begin
        if Client.AbortingTransfer then
            SendAnswer(Client, Format(msgAppeAborted, [Client.TransferError]))
        else if Error = 0 then
            SendAnswer(Client, msgAppeOk)
        else
            SendAnswer(Client, Format(msgAppeError, [Error]));
    end
    else { Should never comes here }
        raise Exception.Create('Program error in ClientStorSessionClosed');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientStorDataAvailable(Sender: TObject; Error : word);
var
    Len    : Integer;
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
begin
    Data   := TWSocket(Sender);
    Client := TFtpCtrlSocket(Data.Owner);
    Len    := Data.Receive(Client.RcvBuf, Client.RcvSize);
    if Len <= 0 then
        Exit;

    if Client.AbortingTransfer then
        Exit;

    try
        { Trigger the user event for the received data }
        TriggerStorDataAvailable(Client, Data, Client.RcvBuf, Len, Error);

        { We need to open a datastream if not already done and a FilePath }
        { exists (the component user can have nullified the FilePath      }
        if (not Client.HasOpenedFile) and
           (Length(Client.FilePath) > 0) and
           (not Assigned(Client.DataStream)) then begin
            { Use different file modes for APPE vs STOR }
            if (Client.CurCmdType = ftpcAPPE) and
               (GetFileSize(Client.FilePath) > -1) then
                Client.DataStream    := TFileStream.Create(Client.FilePath,
                                        fmOpenReadWrite or fmShareDenyWrite)
            else
                Client.DataStream    := TFileStream.Create(Client.FilePath,
                                        fmCreate);
            Client.DataStream.Seek(Client.RestartPos, soFrombeginning);
            Client.HasOpenedFile := TRUE;
        end;

        { If we have a DataStream, then we need to write the data }
        if Assigned(Client.DataStream) then
            Client.DataStream.WriteBuffer(Client.RcvBuf^, Len);
    except
        { An exception occured, so we abort the transfer }
        on E:Exception do begin
            Client.TransferError    := E.Message;
            Client.AbortingTransfer := TRUE;
            PostMessage(FWindowHandle, WM_FTPSRV_ABORT_TRANSFER, 0, LongInt(Data));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BuildFilePath(
    const Directory : String;
    FileName        : String) : String;
var
    Drive : String;
    Path  : String;
begin
    FileName := SlashesToBackSlashes(FileName);
    PatchIE5(FileName);
    if IsUNC(FileName) then
        Result := FileName
    else if IsUNC(Directory) then begin
        if (Length(FileName) > 0) and (FileName[1] = '\') then
            Result := ExtractFileDrive(Directory) + FileName
        else
            Result := Directory + FileName;
    end
    else begin
        if (Length(FileName) > 1) and (FileName[2] = ':') then begin
            Drive := UpperCase(Copy(FileName, 1, 2));
            Path  := Copy(FileName, 3, Length(FileName));
        end
        else begin
            Drive := Copy(Directory, 1, 2);
            Path  := FileName;
        end;

        if (Length(Path) > 0) and (Path[1] = '\') then
            Result := Drive + Path
        else begin
            if Drive <> Copy(Directory, 1, 2) then
                raise Exception.Create('No current dir for ''' + Drive + '''');
            Result := Directory + Path;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandRETR(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Allowed  : Boolean;
    FilePath : TFtpString;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    try
        Client.CurCmdType    := ftpcRETR;
        Client.HasOpenedFile := FALSE;
        Client.FileName      := SlashesToBackSlashes(Params);
        Allowed              := TRUE;
        FilePath             := BuildFilePath(Client.Directory, Client.FileName);
        TriggerValidateGet(Client, FilePath, Allowed);
        if not Allowed then begin
            Answer := msgRetrDisabled;
            Exit;
        end;
        Client.FilePath := FilePath;
        Answer          := Format(msgRetrSuccess, [Params]);
        StartSendData(Client);
    except
        on E:Exception do begin
            Answer := Format(msgRetrFailed, [E.Message]);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientPassiveSessionAvailable(Sender : TObject; Error : Word);
var
    HSocket : TSocket;
    Client  : TFtpCtrlSocket;
    Data    : TWSocket;
begin
    Data    := TWSocket(Sender);
    Client  := TFtpCtrlSocket(Data.Owner);
    HSocket := Data.Accept;
    Data.OnSessionClosed := nil;
    Data.Close;   { We don't need to listen any more }

    if Client.CurCmdType in [ftpcSTOR, ftpcAPPE] then begin
        Client.DataSocket.OnSessionConnected  := ClientStorSessionConnected;
        Client.DataSocket.OnSessionClosed     := ClientStorSessionClosed;
        Client.DataSocket.OnDataAvailable     := ClientStorDataAvailable;
        Client.DataSocket.OnDataSent          := nil;
    end
    else if Client.CurCmdType in [ftpcRETR, ftpcLIST, ftpcNLST] then begin
        Client.DataSocket.OnSessionConnected  := ClientRetrSessionConnected;
        Client.DataSocket.OnSessionClosed     := ClientRetrSessionClosed;
        Client.DataSocket.OnDataAvailable     := nil;
        Client.DataSocket.OnDataSent          := ClientRetrDataSent;
    end
    else begin
        Client.DataSocket.OnSessionConnected  := nil;
        Client.DataSocket.OnSessionClosed     := nil;
        Client.DataSocket.OnDataAvailable     := nil;
        Client.DataSocket.OnDataSent          := nil;
    end;
    Client.DataSocket.LingerOnOff             := wsLingerOff;
    Client.DataSocket.LingerTimeout           := 0;
    Client.DataSocket.HSocket                 := HSocket;
    Client.PassiveConnected                   := TRUE;
    if Client.PassiveStart then
        Client.DataSocket.OnSessionConnected(Client.DataSocket, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.StartSendData(Client : TFtpCtrlSocket);
begin
    Client.AbortingTransfer              := FALSE;
    Client.DataSent                      := FALSE;
    Client.TransferError                 := 'Transfer Ok';
    if Client.PassiveMode then begin
        Client.DataSocket.OnSessionConnected  := ClientRetrSessionConnected;
        Client.DataSocket.OnSessionClosed     := ClientRetrSessionClosed;
        Client.DataSocket.OnDataAvailable     := nil;
        Client.DataSocket.OnDataSent          := ClientRetrDataSent;
        if Client.PassiveConnected then
            Client.DataSocket.OnSessionConnected(Client.DataSocket, 0)
        else
            Client.PassiveStart := TRUE;
    end
    else begin
        Client.DataSocket.Close;
        Client.DataSocket.Proto              := 'tcp';
        Client.DataSocket.Addr               := Client.DataAddr;
        Client.DataSocket.Port               := Client.DataPort;
        Client.DataSocket.OnSessionConnected := ClientRetrSessionConnected;
        Client.DataSocket.OnSessionClosed    := ClientRetrSessionClosed;
        Client.DataSocket.OnDataAvailable    := nil;
        Client.DataSocket.OnDataSent         := ClientRetrDataSent;
        Client.DataSocket.LingerOnOff        := wsLingerOff;
        Client.DataSocket.LingerTimeout      := 0;
{$IFDEF BIND_FTP_DATA}
        Client.DataSocket.LocalAddr           := Client.GetXAddr;
        Client.DataSocket.LocalPort           := 'ftp-data'; {20}
{$ENDIF}
        Client.DataSocket.ComponentOptions    := [wsoNoReceiveLoop];
        Client.DataSocket.Connect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientRetrSessionConnected(Sender : TObject; Error : Word);
var
    Client      : TFtpCtrlSocket;
    Data        : TWSocket;
begin
    Data                     := TWSocket(Sender);
    Client                   := TFtpCtrlSocket(Data.Owner);
    Client.DataSessionActive := (Error = 0);
    Client.PassiveMode       := FALSE;

    try
        TriggerRetrSessionConnected(Client, Data, Error);
        if Error <> 0 then
            raise Exception.Create('Client data socket connection error: ' +
                                   IntToStr(Error));

        { We need to open a datastream if not already done and a FilePath }
        { exists the component user can have nullified the FilePath or    }
        { created his own data stream (virtual file feature)              }
        if (not Client.HasOpenedFile) and
           (Length(Client.FilePath) > 0) and
           (not Assigned(Client.DataStream)) then begin
            Client.DataStream    := TFileStream.Create(Client.FilePath,
                                                       fmOpenRead + fmShareDenyNone);
            Client.DataStream.Seek(Client.RestartPos, soFrombeginning);
            Client.HasOpenedFile := TRUE;
        end;
    except
        on E:Exception do begin
            Client.AbortingTransfer := TRUE;
            Client.TransferError    := E.Message;
            PostMessage(FWindowHandle, WM_FTPSRV_ABORT_TRANSFER,
                        0, LongInt(Data));
            Exit;
        end;
    end;

    SendNextDataChunk(Client, Data);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientRetrSessionClosed(Sender : TObject; Error : Word);
var
    Client      : TFtpCtrlSocket;
    Data        : TWSocket;
begin
    Data                     := TWSocket(Sender);
    Client                   := TFtpCtrlSocket(Data.Owner);
    Client.DataSessionActive := FALSE;
    Client.PassiveStart      := FALSE;
    Client.PassiveConnected  := FALSE;
    Client.RestartPos        := 0;
    { Reset data port to standard value }
    Client.DataPort          := 'ftp-data';

    { If we had opened a data stream ourself, then close it }
    if Client.HasOpenedFile then begin
        if Assigned(Client.DataStream) then begin
            Client.DataStream.Destroy;
        end;
        Client.DataStream    := nil;
        Client.HasOpenedFile := FALSE;
    end;

    if Client.AbortingTransfer then
        SendAnswer(Client, Format(msgRetrFailed, [Client.TransferError]))
    else if Error <> 0 then
        SendAnswer(Client, Format(msgRetrFailed, ['Error #' + IntToStr(Error)]))
    else
        SendAnswer(Client, msgRetrOk);

    TriggerRetrSessionClosed(Client, Data, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.SendNextDataChunk(
    Client : TFtpCtrlSocket;
    Data   : TWSocket);
var
    Count : LongInt;
begin
    try
        if Assigned(Client.DataStream) then
            Count := Client.DataStream.Read(Client.RcvBuf^, Client.RcvSize)
        else
            Count := 0;

        if Count > 0 then begin
            Client.ByteCount := Client.ByteCount + Count;
            Data.Send(Client.RcvBuf, Count);
        end
        else begin { EOF }
            if not Client.DataSent then begin
                Client.DataSent := TRUE;
                PostMessage(Handle, WM_FTPSRV_CLOSE_DATA, 0, LongInt(Data));
            end;
        end;
    except
        { An exception occured, so we abort the transfer }
        on E:Exception do begin
            Client.TransferError    := E.Message;
            Client.AbortingTransfer := TRUE;
            PostMessage(FWindowHandle, WM_FTPSRV_ABORT_TRANSFER, 0, LongInt(Data));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientRetrDataSent(Sender : TObject; Error : Word);
var
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
begin
    Data   := TWSocket(Sender);
    Client := TFtpCtrlSocket(Data.Owner);

    if Client.AbortingTransfer then
        Exit;

    try
        { Trigger the user event for the received data }
        TriggerRetrDataSent(Client, Data, Error);
        if Error <> 0 then
            raise Exception.Create('Send: error #' + IntToStr(Error));
        SendNextDataChunk(Client, Data);
    except
        { An exception occured, so we abort the transfer }
        on E:Exception do begin
            Client.TransferError    := E.Message;
            Client.AbortingTransfer := TRUE;
            SendAnswer(Client, Format(msgRetrAborted, [Client.TransferError]));
            PostMessage(FWindowHandle, WM_FTPSRV_ABORT_TRANSFER, 0, LongInt(Data));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSYST(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcSYST;
    Answer            := msgSystem;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandDirectory(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString;
    Detailed    : Boolean);
begin
    if Assigned(Client.DataStream) then begin
        Client.DataStream.Destroy;
        Client.DataStream := nil;
    end;

    try
        Params := SlashesToBackSlashes(Params);
        TriggerBuildDirectory(Client, Params, Detailed);
        if not Assigned(Client.DataStream) then begin
            Client.DataStream    := TMemoryStream.Create;
            Client.HasOpenedFile := TRUE;
            BuildDirectory(Client, Params, Client.DataStream, Detailed);
            TriggerAlterDirectory(Client, Params, Detailed);
            Client.DataStream.Seek(0, 0);
        end;
        Client.FilePath := '';
        Answer := msgDirOpen;
        StartSendData(Client);
    except
        on E:Exception do begin
            Answer := Format(msgDirFailed, [E.Message])
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandLIST(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcLIST;
    CommandDirectory(Client, KeyWord, Params, Answer, TRUE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandNLST(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcNLST;
    CommandDirectory(Client, KeyWord, Params, Answer, FALSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FormatUnixDirEntry(F : TSearchRec) : String;
var
    Attr : String;
    Ext  : String;
    Day, Month, Year : Integer;
    Hour, Min        : Integer;
const
    StrMonth : array [1..12] of String =
        ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
         'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
begin
{$WARNINGS OFF}
    if ((F.Attr and faVolumeID) <> 0) or
       ((F.Attr and faHidden)   <> 0) then begin
        { Ignore hidden files and volume ID entries }
        Result := '';
        Exit;
    end;

    Attr := '-rw-rw-rw-';
    if (F.Attr and faDirectory) <> 0 then
        Attr[1] := 'd';

    if (F.Attr and faReadOnly) <> 0 then begin
        Attr[3] := '-';
        Attr[6] := '-';
        Attr[9] := '-';
    end;
{$WARNINGS ON}

    Ext := UpperCase(ExtractFileExt(F.Name));
    if (Ext = '.EXE') or (Ext = '.COM') or (Ext = '.BAT') then begin
        Attr[4]  := 'x';
        Attr[7]  := 'x';
        Attr[10] := 'x';
    end;

    Day   := (HIWORD(F.Time) and $1F);
    Month := ((HIWORD(F.Time) shr 5) and $0F);
    Year  := ((HIWORD(F.Time) shr 9) and $3F) + 1980;
{   Sec   := ((F.Time and $1F) shl 1); }
    Min   := ((F.Time shr 5) and $3F);
    Hour  := ((F.Time shr 11) and $1F);

    Result := Attr + '   1 ftp      ftp  ' + Format('%11d ', [F.Size]);

    Result := Result + Format('%s %2.2d ', [StrMonth[Month], Day]);
    if Year = ThisYear then
        Result := Result + Format('%2.2d:%2.2d ', [Hour, Min])
    else
        Result := Result + Format('%5d ', [Year]);

    Result := Result + F.Name + #13#10;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.BuildDirectory(
    Client     : TFtpCtrlSocket;
    var Params : TFtpString;
    Stream     : TStream;
    Detailed   : Boolean);
var
    F      : TSearchRec;
    Path   : String;
    Status : Integer;
    Buf    : String;
begin
    DecodeDate(Now, ThisYear, ThisMonth, ThisDay);

    if Params = '' then
        Path := Client.Directory + '*.*'
    else
        Path := BuildFilePath(Client.Directory, Params);

    if Path[Length(Path)] = '\' then
        Path := Path + '*.*';

    Status := FindFirst(Path, faAnyFile, F);
    while Status = 0 do begin
        if Detailed then
            Buf := FormatUnixDirEntry(F)
        else
            Buf := F.Name + #13#10;
        if Length(Buf) > 0 then
            Stream.Write(Buf[1], Length(Buf));
        Status := FindNext(F);
    end;
    FindClose(F);

    if Stream.Size = 0 then begin
        Buf := Path + ' not found' + #13#10;
        Stream.Write(Buf[1], Length(Buf));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandTYPE(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Buf : String;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcTYPE;
    Buf := UpperCase(Trim(Params));
    if (Buf = 'A') or (Buf = 'A N') or (Buf = 'I') then begin
        Answer            := Format(msgTypeOk, [Params]);
        Client.BinaryMode := (Buf = 'I');
    end
    else
        Answer := Format(msgTypeFailed, [Params]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandDELE(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FileName : TFtpString;
    Allowed  : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcDELE;
    FileName          := BuildFilePath(Client.Directory, Params);
    Allowed           := TRUE;
    TriggerValidateDele(Client, FileName, Allowed);
    if not Allowed then begin
        Answer := msgDeleDisabled;
        Exit;
    end;
    if Params = '' then
        Answer := Format(msgDeleSyntax, [Params])
    else if FileExists(FileName) then begin
        if DeleteFile(FileName) then
            Answer := Format(msgDeleOk, [FileName])
        else
            Answer := Format(msgDeleFailed, [FileName]);
    end
    else
        Answer := Format(msgDeleNotExists, [FileName]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSIZE(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FileName : String;
    Size     : LongInt;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcSIZE;
    FileName          := BuildFilePath(Client.Directory, Params);
    if Params = '' then
        Answer := Format(msgSizeSyntax, [Params])
    else begin
        try
            Size := GetFileSize(FileName);
            if Size >= 0 then
                Answer := Format(msgSizeOk, [Size])
            else
                Answer := Format(msgSizeFailed, ['File not found'])
        except
            on E:Exception do begin
                Answer := Format(msgSizeFailed, [E.Message])
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandREST(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcREST;
    try
        Client.RestartPos := StrToInt(Params);
        if Client.RestartPos <= 0 then begin
            Answer            := msgRestZero;
            Client.RestartPos := 0;
        end
        else
            Answer := Format(msgRestOk, [Client.RestartPos]);
    except
        on E:Exception do begin
            Answer            := Format(msgRestFailed, [E.Message]);
            Client.RestartPos := 0;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandRNFR(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FileName : TFtpString;
    Allowed  : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcRNFR;
    FileName          := BuildFilePath(Client.Directory, Params);
    Allowed           := TRUE;
    TriggerValidateRnFr(Client, FileName, Allowed);
    if not Allowed then begin
        Answer := msgRnFrDisabled;
        Exit;
    end;
    if Params = '' then
        Answer := Format(msgRnfrSyntax, [Params])
    else if FileExists(FileName) then begin
        Client.FromFileName := FileName;
        Answer              := Format(msgRnfrOk, [FileName])
    end
    else
        Answer := Format(msgRnfrNotExists, [FileName]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandRNTO(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FileName : TFtpString;
    Allowed  : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcRNTO;
    FileName          := BuildFilePath(Client.Directory, Params);
    Allowed           := TRUE;
    TriggerValidateRnTo(Client, FileName, Allowed);
    if not Allowed then begin
        Answer := msgRnToDisabled;
        Exit;
    end;
    if Params = '' then
        Answer := Format(msgRntoSyntax, [Params])
    else if FileExists(FileName) then
        Answer := Format(msgRntoAlready, [FileName])
    else if not FileExists(Client.FromFileName) then
        Answer := Format(msgRntoNotExists, [Client.FromFileName])
    else begin
        Client.ToFileName := FileName;
        if RenameFile(Client.FromFileName, Client.ToFileName) then
            Answer := Format(msgRntoOk, [Client.FromFileName, Client.ToFileName])
        else
            Answer := Format(msgRntoFailed, [Client.FromFileName]);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandNOOP(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    Client.CurCmdType := ftpcNOOP;
    Answer            := Format(MsgNoopOk, [Params]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandMKD(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FileName : String;
    Allowed  : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    try
        Client.CurCmdType := ftpcMKD;
        FileName          := BuildFilePath(Client.Directory, Params);
        Allowed           := TRUE;
        TriggerMakeDirectory(Client, FileName, Allowed);
        if not Allowed then
            Answer := Format(msgMkdFailed, [FileName])
        else if Params = '' then
            Answer := Format(msgMkdSyntax, [Params])
        else if FileExists(FileName) then
            Answer := Format(msgMkdAlready, [FileName])
        else begin
            {$I-}
            MkDir(FileName);
            if IOResult = 0 then
                Answer := Format(msgMkdOk, [FileName])
            else
                Answer := Format(msgMkdFailed, [FileName]);
            {$I+}
        end;
    except
        on E:Exception do begin
            Answer := Format(msgMkdFailed, [E.Message])
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandAPPE(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Allowed  : Boolean;
    FilePath : TFtpString;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    try
        Client.CurCmdType       := ftpcAPPE;
        Client.FileName         := SlashesToBackSlashes(Params);
        Client.HasOpenedFile    := FALSE;
        Client.AbortingTransfer := FALSE;
        Client.TransferError    := 'Transfer Ok';
        Allowed                 := TRUE;
        FilePath                := BuildFilePath(Client.Directory, Client.FileName);
        TriggerValidatePut(Client, FilePath, Allowed);
        if not Allowed then begin
            Answer := msgAppeDisabled;
            Exit;
        end;
        Client.FilePath := FilePath;
        if Client.PassiveMode then begin
            Client.DataSocket.OnSessionConnected  := ClientStorSessionConnected;
            Client.DataSocket.OnSessionClosed     := ClientStorSessionClosed;
            Client.DataSocket.OnDataAvailable     := ClientStorDataAvailable;
            Client.DataSocket.OnDataSent          := nil;
            if Client.PassiveConnected then
                Client.DataSocket.OnSessionConnected(Client.DataSocket, 0)
            else
                Client.PassiveStart := TRUE;
        end
        else begin
            Client.DataSocket.Proto               := 'tcp';
            Client.DataSocket.Addr                := Client.DataAddr;
            Client.DataSocket.Port                := Client.DataPort;
            Client.DataSocket.OnSessionConnected  := ClientStorSessionConnected;
            Client.DataSocket.OnSessionClosed     := ClientStorSessionClosed;
            Client.DataSocket.OnDataAvailable     := ClientStorDataAvailable;
            Client.DataSocket.OnDataSent          := nil;
            Client.DataSocket.LingerOnOff         := wsLingerOff;
            Client.DataSocket.LingerTimeout       := 0;
            Client.DataSocket.ComponentOptions    := [wsoNoReceiveLoop];
            Client.DataSocket.Connect;
        end;
            Client.RestartPos := GetFileSize(Client.FilePath);
            if Client.RestartPos < 0 then
                Client.RestartPos := 0;
            Answer := Format(msgAppeReady, [Params,Client.RestartPos]);
    except
        on E:Exception do begin
            Answer := Format(msgAppeFailed, [E.Message]);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSTRU(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    Client.CurCmdType := ftpcSTRU;
    Answer            := Format(MsgStruOk, [Params]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DirExists(Dir : String) : Boolean;
var
    F : TSearchRec;
begin
    Result := (FindFirst(Dir, faDirectory, F) = 0);
    FindClose(F);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandRMD(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FileName : TFtpString;
    Allowed  : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcRMD;
    FileName          := BuildFilePath(Client.Directory, Params);
    Allowed           := TRUE;
    TriggerValidateRmd(Client, FileName, Allowed);
    if not Allowed then begin
        Answer := msgRmdDisabled;
        Exit;
    end;
    if Params = '' then
        Answer := Format(msgMkdSyntax, [Params])
    else if not DirExists(FileName) then
        Answer := Format(msgRmdNotExists, [FileName])
    else begin
        {$I-}
        RmDir(FileName);
        if IOResult = 0 then
            Answer := Format(msgRmdOk, [FileName])
        else
            Answer := Format(msgRmdFailed, [FileName]);
        {$I+}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandABOR(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.DataSocket.State = wsConnected then begin
        Client.TransferError    := 'ABORT requested by client';
        Client.AbortingTransfer := TRUE;
        Client.DataSocket.Close;
    end;
    Answer := msgAborOk;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandPASV(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    saddr    : TSockAddrIn;
    saddrlen : Integer;
    DataPort : Integer;
    IPAddr   : TInAddr;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    try
        { Get our IP address from our control socket }
        saddrlen := SizeOf(saddr);
        Client.GetSockName(saddr, saddrlen);
        IPAddr   := saddr.sin_addr;

        Client.DataSocket.Close;
        Client.DataSocket.Addr  := '0.0.0.0';   { Any addr }
        Client.DataSocket.Port  := '0';         { Any port }
        Client.DataSocket.Proto := 'tcp';
        Client.DataSocket.OnSessionAvailable := ClientPassiveSessionAvailable;
        Client.DataSocket.OnSessionConnected := nil;
        Client.DataSocket.OnSessionClosed    := nil;
        Client.DataSocket.OnDataAvailable    := nil;
        Client.DataSocket.ComponentOptions   := [wsoNoReceiveLoop];
        Client.DataSocket.Listen;
{        if Client.DataSocket.Listen <> 0 then
            raise Exception.Create('Listen failed'); 18/11/98 }

        { Get the port assigned by winsock }
        saddrlen := SizeOf(saddr);
        Client.DataSocket.GetSockName(saddr, saddrlen);
        DataPort := WSocket_ntohs(saddr.sin_port);

        if Client.sin.sin_addr.s_addr = WSocket_htonl($7F000001) then
            Answer := Format(msgPasvLocal,
                          [HiByte(DataPort),
                           LoByte(DataPort)])
        else
            Answer := Format(msgPasvRemote,
                          [ord(IPAddr.S_un_b.s_b1),
                           ord(IPAddr.S_un_b.s_b2),
                           ord(IPAddr.S_un_b.s_b3),
                           ord(IPAddr.S_un_b.s_b4),
                           HiByte(DataPort),
                           LoByte(DataPort)]);
        Client.PassiveMode      := TRUE;
        Client.PassiveStart     := FALSE;
        Client.PassiveConnected := FALSE;
    except
        on E:Exception do begin
            Answer := Format(msgPasvExcept, [E.Message]);
            try
                Client.DataSocket.Close;
            except
                { Ignore any exception here }
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandMDTM(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FileTime : String;
    FileName : String;
begin
    Client.CurCmdType := ftpcMDTM;
    FileName          := BuildFilePath(Client.Directory , Params);
    if Params = '' then
        Answer := msgMdtmSyntax
    else if not FileExists(FileName) then
        Answer := Format(msgMdtmNotExists, [FileName])
    else begin
        try
            FileTime := FileUtcStr(FileName);
            if Length(FileTime) <> 0 then
                Answer := Format(msgMdtmOk, [FileTime])
            else
                Answer := Format(msgMdtmFailed,
                                 ['UTC File time retrieval failed'])
        except
            on E:Exception do
              Answer := Format(msgSizeFailed , [E.Message]);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandMode(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    if (Params = '') then begin
        Answer := msgModeSyntax;
        Exit;
    end;
    if (Params <> 'S') and (Params <> 's') then begin
        Answer := msgModeNotS;
        Exit;
    end;
    Answer := msgModeOK;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandOverflow(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Buf : array [0..1023] of char;
begin
    { Disable receiving }
    Client.Shutdown(0);
    { Flush receive buffer }
    while (Client.Receive(@Buf, SizeOf(buf)) > 0) do ;
    { Answer to client }
    Answer := msgOverflow;
    { Will close connection }
    PostMessage(FWindowHandle, WM_FTPSRV_CLOSE_REQUEST, 0, LongInt(Client));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

