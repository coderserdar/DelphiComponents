{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE (original)
              Marco van de Voort (ugly hack to console)
Object:       Show how to use TPop3Prot (POP3 protocol, RFC-1225)
              Written with a service that checks a pop3box for
              commands/input in mind.
Credit:       Marco van de Voort <marco@freepascal.org> ported my GUI code
              (MailRcv demo) to console mode.
Creation:     Sep 2003 (From GUI version created 03 october 1997)
Version:      1.01
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2010 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

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

Updates:
Sep 08, 2004 V1.01 A little bit of cleaning. Still need more :-(
                   Updated for NOFORMS use.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
program OverbyteIcsConPop3;

{$I OVERBYTEICSDEFS.INC}
{$IFDEF VER80}
    Bomb('Sorry, Delphi 1 does not support console mode programs');
{$ENDIF}
{$IFNDEF NOFORMS}
    Bomb('Please add NOFORMS to your project defines to reduce exe size');
{$ENDIF}
{$APPTYPE CONSOLE}
{$DEFINE DOHEADERS}     // true: find subject with "RUN SCRIPT"
                        // false: list all subjects
{$DEFINE DEBUG}         // debug output.

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  IniFiles,
{$IF CompilerVersion >= 20}
  AnsiStrings,
{$IFEND}
  OverbyteIcsPop3Prot,
  OverbyteIcsConApp in '..\..\..\Source\Extras\OverbyteIcsConApp.pas';

const
    ConPop3Version = 800;
    CopyRight : String = ' ConPOP3 (c) 1997-2012 F. Piette V8.00 ';

type
  TPOP3ExercizerApp = class(TConApplication)
    Msg       : TStringList;
    Pop3Client: TSyncPop3Cli;
    procedure ReadDataFromIni(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Connectevent     (Sender: TObject);
    procedure Quittevent       (Sender: TObject);
    procedure Userevent        (Sender: TObject);
    procedure Passevent        (Sender: TObject);
    procedure Retrevent        (Sender: TObject);
    procedure Statevent        (Sender: TObject);
    procedure ListAllevent     (Sender: TObject);
    procedure Listevent        (Sender: TObject);
    procedure Deleteevent      (Sender: TObject);
    procedure Noopevent        (Sender: TObject);
    procedure Lastevent        (Sender: TObject);
    procedure Resetevent       (Sender: TObject);
    procedure Topevent         (Sender: TObject);
    procedure Rpopevent        (Sender: TObject);
    procedure Apopevent        (Sender: TObject);
    procedure Nextevent        (Sender: TObject);
    procedure GetAllevent      (Sender: TObject);

    procedure Pop3ClientDisplay(Sender: TObject; Msg: String);
    procedure Uidlevent(Sender: TObject);
    procedure Pop3ClientUidlBegin(Sender: TObject);
    procedure Pop3ClientUidlEnd(Sender: TObject);
    procedure Pop3ClientUidlLine(Sender: TObject);
    procedure Pop3ClientMessageBegin(Sender: TObject);
    procedure Pop3ClientMessageEnd(Sender: TObject);
    procedure Pop3ClientMessageLine(Sender: TObject);
    procedure Pop3ClientListBegin(Sender: TObject);
    procedure Pop3ClientListEnd(Sender: TObject);
    procedure Pop3ClientListLine(Sender: TObject);

    function  ConnectSyncEvent (sender: TObject):boolean;
    function  OpenSyncEvent    (sender: TObject):boolean;
    function  UserSyncEvent    (sender: TObject):boolean;
    function  PassSyncEvent    (sender: TObject):boolean;
    function  QuittSyncEvent   (sender: TObject):boolean;
    function  AbortSyncEvent   (sender: TObject):boolean;
    function  RetrSyncEvent    (sender: TObject):boolean;
    function  StatSyncEvent    (sender: TObject):boolean;
    function  ListAllSyncEvent (sender: TObject):boolean;
    function  ListSyncEvent    (sender: TObject):boolean;
    function  DeleteSyncEvent  (sender: TObject):boolean;
    function  NoopSyncEvent    (sender: TObject):boolean;
    function  LastSyncEvent    (sender: TObject):boolean;
    function  ResetSyncEvent   (sender: TObject):boolean;
    function  TopSyncEvent     (sender: TObject):boolean;
    function  RpopSyncEvent    (sender: TObject):boolean;
    function  UidlSyncEvent    (sender: TObject):boolean;
    function  ApopSyncEvent    (sender: TObject):boolean;

    procedure Pop3ClientRequestDone(Sender: TObject; RqType: TPop3Request;
                                    ErrCode: Word);
    procedure Openevent(Sender: TObject);
    procedure Abortevent(Sender: TObject);
    procedure Pop3ClientHeaderEnd(Sender: TObject);
  private
    OkMsgnumber : TList;
    SubjectOk    : Boolean;
    Waitfor      : TPop3Request;
    FFile        : TextFile;
    FFileName    : String;
    FFileOpened  : Boolean;
    FGetAllState : Integer;
    FMsgPath     : String;
    fHost,
    fPort,
    fUser,
    fPasswd      : string;
    MsgCount,
    Lines,
    msgnum       : integer;
    procedure Exec(MethodPtr  : TPop3NextProc;
                   MethodName : String); overload;
    Function Exec(MethodPtr  : TPop3Method;
                   MethodName  : String):Boolean; overload;
    procedure MessageBegin(Sender: TObject);
    procedure MessageLine(Sender: TObject);
    procedure GetAllMessageLine(Sender: TObject);
    procedure GetAllRequestDone(Sender: TObject;
                                RqType: TPop3Request; ErrCode: Word);
    procedure NextMessageRequestDone(Sender: TObject;
                                     RqType: TPop3Request; ErrCode: Word);
   public
     Constructor Create(AOwner:TComponent); override;
     Destructor  Destroy; override;
     procedure   Wait;
     procedure   WaitReady;
     procedure   waitfordone(ev: TPop3Request);
  end;


const
    IniFileName = 'CONPOP3.INI';
    MsgFileName = 'CONPOP3.TXT';

    SectionData = 'Data';
    KeyMsgPath  = 'MsgPath';
    KeyHost     = 'Host';
    KeyPort     = 'Port';
    KeyUserName = 'UserName';
    KeyPassword = 'Password';
    

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TPOP3ExercizerApp.Create(AOwner: TComponent);
begin
    inherited Create(aowner);
    Pop3Client  := TSyncPop3Cli.create(Self);
    OkMsgNumber := TList.Create;
    Lines       := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TPOP3ExercizerApp.Destroy;
begin
    OkMsgNumber.Free;
    Pop3Client.Free;
    inherited;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Restore some data from the INI file                                       }
procedure TPOP3ExercizerApp.ReadDataFromIni(Sender: TObject);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(IniFileName);
    FHost   := IniFile.ReadString(SectionData, KeyHost,     'pop3.isp.com');
    FPort   := IniFile.ReadString(SectionData, KeyPort,     'pop3');
    FUser   := IniFile.ReadString(SectionData, KeyUserName, 'myname');
    FPasswd := IniFile.ReadString(SectionData, KeyPassword, 'mypassword');
    // Write the values to create the ini file is non exitent
    IniFile.WriteString(SectionData, KeyHost,     FHost);
    IniFile.WriteString(SectionData, KeyPort,     FPort);
    IniFile.WriteString(SectionData, KeyUserName, FUser);
    IniFile.WriteString(SectionData, KeyPassword, FPasswd);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Save data to INI file   (unused)                                          }
procedure TPOP3ExercizerApp.FormCloseQuery(
    Sender       : TObject;
    var CanClose : Boolean);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(IniFileName);
    IniFile.WriteString(SectionData, 'Host',     fHost);
    IniFile.WriteString(SectionData, 'Port',     fPort);
    IniFile.WriteString(SectionData, 'UserName', fUser);
    IniFile.WriteString(SectionData, 'Password', fPassWd);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when the TPop3Client object wants to display }
{ some information such as connection progress or errors.                   }
procedure TPOP3ExercizerApp.Pop3ClientDisplay(Sender: TObject;
  Msg: String);
begin
  {$IFDEF DEBUG}
  WriteLn('TPop3Cli: ',Msg);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ All the TPop3Client method are of the same type. The "sync" versions      }
{ are a different type from the other (async) methods, so we need two       }
{ execs()                                                                   }
{ To simplify quickly "scripting" together certain POP3 actions, the Exec   }
{ methods stuffs some commonly used handlers and fields in load default     }
{ values a lot of methods that are dispatched application, we al            }
{ Exec transfert the parameters form the various EditBoxes                  }
{ to the Pop3Client instance and then call the appropriate method, showing  }
{ the result in the InfoLabel.Caption.                                      }
procedure TPOP3ExercizerApp.Exec(
    MethodPtr  : TPop3NextProc;
    MethodName : String);

begin
    Pop3Client.Host           := fHost;
    Pop3Client.Port           := fPort;
    Pop3Client.UserName       := fUser;
    Pop3Client.PassWord       := fPassWd;
    Pop3Client.MsgNum         := msgnum; //    MsgNumEdit.Text);
    Pop3Client.MsgLines       := Lines; //Msg.count;
    { We need to reassign event handlers because we may have changed them }
    { doing GetAllMessages for example                                    }
    Pop3Client.OnRequestDone  := Pop3ClientRequestDone;
    Pop3Client.OnMessageBegin := Pop3ClientMessageBegin;
    Pop3Client.OnMessageEnd   := Pop3ClientMessageEnd;
    Pop3Client.OnMessageLine  := Pop3ClientMessageLine;
    Pop3Client.OnListLine     := Pop3ClientListLine;
    {$IFDEF DEBUG}
    WriteLn('status :', MethodName + ' started');
    {$ENDIF}
    try
        MethodPtr;
        {$IFDEF DEBUG}
        WriteLn('status :',MethodName, ' ok');
        {$ENDIF}
    except
        on E:Exception do begin
            {$IFDEF DEBUG}
             WriteLn('status :',MethodName, ' failed ',E.Message, ')');
            {$ENDIF}
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.Exec(
    MethodPtr  : TPop3Method;
    MethodName : String):boolean;
begin
    Pop3Client.Host           := fHost;
    Pop3Client.Port           := fPort;
    Pop3Client.UserName       := fUser;
    Pop3Client.PassWord       := fPassWd;
    Pop3Client.MsgNum         := msgnum; //    MsgNumEdit.Text);
    Pop3Client.MsgLines       := Lines; //Msg.count;
    { We need to reassign event handlers because we may have changed them }
    { doing GetAllMessages for example                                    }
    Pop3Client.OnRequestDone  := Pop3ClientRequestDone;
    Pop3Client.OnMessageBegin := Pop3ClientMessageBegin;
    Pop3Client.OnMessageEnd   := Pop3ClientMessageEnd;
    Pop3Client.OnMessageLine  := Pop3ClientMessageLine;
    Pop3Client.OnListLine     := Pop3ClientListLine;

    {$IFDEF DEBUG}
    WriteLn('status :', MethodName + ' started');
    {$ENDIF}
    try
        Result:=MethodPtr;
        {$IFDEF DEBUG}
        WriteLn('status :',MethodName, ' ok');
        {$ENDIF}
    except
        on E:Exception do begin
            {$IFDEF DEBUG}
            WriteLn('status :',MethodName, ' failed ',E.Message, ')');
            {$ENDIF}
            Result:=false;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Connectevent(Sender: TObject);
begin
    Exec(Pop3Client.Connect, 'Connect');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Openevent(Sender: TObject);
begin
    Exec(Pop3Client.Open, 'Open');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Userevent(Sender: TObject);
begin
    Exec(Pop3Client.User, 'User');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Passevent(Sender: TObject);
begin
    Exec(Pop3Client.Pass, 'Pass');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Quittevent(Sender: TObject);
begin
    Exec(Pop3Client.Quit, 'Quit');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Abortevent(Sender: TObject);
begin
    Exec(Pop3Client.Abort, 'Abort');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Retrevent(Sender: TObject);
begin
    Exec(Pop3Client.Retr, 'Retr');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Statevent(Sender: TObject);
begin
    Exec(Pop3Client.Stat, 'Stat');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.ListAllevent(Sender: TObject);
begin
    MsgNum := 0;
    Exec(Pop3Client.List, 'List All');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Listevent(Sender: TObject);
begin
    WaitFor:=Pop3Custom;
    Exec(Pop3Client.List, 'List');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Deleteevent(Sender: TObject);
begin
    Exec(Pop3Client.Dele, 'Delete');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Noopevent(Sender: TObject);
begin
    Exec(Pop3Client.Noop, 'Noop');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Lastevent(Sender: TObject);
begin
    Exec(Pop3Client.Last, 'Last');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Resetevent(Sender: TObject);
begin
    Exec(Pop3Client.RSet, 'Rset');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Topevent(Sender: TObject);
begin
    Exec(Pop3Client.Top, 'Top');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Rpopevent(Sender: TObject);
begin
    Exec(Pop3Client.RPop, 'Rpop');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Uidlevent(Sender: TObject);
begin
    Exec(Pop3Client.Uidl, 'Uidl');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Apopevent(Sender: TObject);
begin
    Exec(Pop3Client.APop, 'Apop');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.ConnectSyncEvent(Sender: TObject):Boolean;
begin
    Result := Exec(Pop3Client.ConnectSync, 'ConnectSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.OpenSyncEvent(Sender: TObject):Boolean;
begin
    Result := Exec(Pop3Client.OpenSync, 'OpenSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.UserSyncEvent(Sender: TObject):Boolean;
begin
    Result := Exec(Pop3Client.UserSync, 'UserSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.PassSyncEvent(Sender: TObject):Boolean;
begin
    Result := Exec(Pop3Client.PassSync, 'PassSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.QuittSyncEvent(Sender: TObject):Boolean;
begin
    Result := Exec(Pop3Client.QuitSync, 'QuitSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.AbortSyncEvent(Sender: TObject):Boolean;
begin
    Result := Exec(Pop3Client.AbortSync, 'AbortSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.RetrSyncEvent(Sender: TObject):Boolean;
begin
    Result := Exec(Pop3Client.RetrSync, 'RetrSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.StatSyncEvent(Sender: TObject):Boolean;
begin
    Result := Exec(Pop3Client.StatSync, 'StatSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.ListAllSyncEvent(Sender: TObject):Boolean;
begin
    MsgNum := 0;
    Result := Exec(Pop3Client.ListSync, 'List AllSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.ListSyncEvent(Sender: TObject):Boolean;
begin
    WaitFor := Pop3Custom;
    Result  := Exec(Pop3Client.ListSync, 'ListSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.DeleteSyncEvent(Sender: TObject):Boolean;
begin
    Result := Exec(Pop3Client.DeleSync, 'DeleteSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.NoopSyncEvent(Sender: TObject):Boolean;
begin
    Result := Exec(Pop3Client.NoopSync, 'NoopSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.LastSyncEvent(Sender: TObject):Boolean;
begin
    Result := Exec(Pop3Client.LastSync, 'LastSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.ResetSyncEvent(Sender: TObject):Boolean;
begin
    Result := Exec(Pop3Client.RSetSync, 'RsetSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.TopSyncEvent(Sender: TObject):Boolean;
begin
    Result := Exec(Pop3Client.TopSync, 'TopSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.RpopSyncEvent(Sender: TObject):Boolean;
begin
    Result := Exec(Pop3Client.RPopSync, 'RpopSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.UidlSyncEvent(Sender: TObject):Boolean;
begin
    Result := Exec(Pop3Client.UidlSync, 'UidlSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.ApopSyncEvent(Sender: TObject):Boolean;
begin
    Result := Exec(Pop3Client.APopSync, 'ApopSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when TPop3Client is about to receive a       }
{ message. The MsgNum property gives the message number.                    }
{ This event handler could be used to open the file used to store the msg.  }
{ The file handle could be stored in the TPop3Client.Tag property to be     }
{ easily retrieved by the OnMessageLine and OnMessageEnd event handlers.    }
procedure TPOP3ExercizerApp.Pop3ClientMessageBegin(Sender: TObject);
begin
    SubjectOk := False;
    {$IFDEF DEBUG}
    WriteLn('*** Message ' +IntToStr((Sender as TPop3Cli).MsgNum) +
                          ' begin ***');
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when TPop3Client has detected the end of a   }
{ message, even if there is an error or exception, this event gets called.  }
{ This event handler could be used to close the file used to store the msg. }
procedure TPOP3ExercizerApp.Pop3ClientMessageEnd(Sender: TObject);
begin
    if SubjectOk then begin
        OkMsgNumber.add(Pointer(msgnum));  // no beauty price here, no dynarr
        {$IFDEF DEBUG}
         WriteLn('THIS (',(Sender as TPop3Cli).MsgNum,') IS A GOOD ONE!');
        {$ENDIF}
    end;
    {$IFDEF DEBUG}
    WriteLn('*** Message ' +
            IntToStr((Sender as TPop3Cli).MsgNum) +
            ' end ***');
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called for each message line that TPop3Client is    }
{ receiveing. This could be used to write the message lines to a file.      }
procedure TPOP3ExercizerApp.Pop3ClientMessageLine(Sender: TObject);
var
    S : AnsiString;
begin
    S := (Sender as TPop3Cli).LastResponse;
    if Copy(S, 1, 8) = 'Subject:' then begin
        S         := Uppercase(S);
    {$IF CompilerVersion >= 20}
        SubjectOk := (PosEx('RUN SCRIPT', S) > 0);
    {$ELSE}
        SubjectOk := (Pos('RUN SCRIPT', S) > 0);
    {$IFEND}
    end;
    // Some extra checks on origin here.
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when TPop3Client is about to receive a       }
{ list line. The MsgNum property gives the message number.                  }
procedure TPOP3ExercizerApp.Pop3ClientListBegin(Sender: TObject);
begin
    {$IFDEF DEBUG}
    WriteLn('*** List begin ***');
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when TPop3Client has received the last list  }
{ line.                                                                     }
procedure TPOP3ExercizerApp.Pop3ClientListEnd(Sender: TObject);
begin

    {$IFDEF DEBUG}
    WriteLn('*** List End ***');
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called for each list line received by TPop3Client.  }
procedure TPOP3ExercizerApp.Pop3ClientListLine(Sender: TObject);
var
    Buffer : String;
begin
    Buffer := 'MsgNum = ' + IntToStr((Sender as TPop3Cli).MsgNum) + ' ' +
              'MsgSize = ' + IntToStr((Sender as TPop3Cli).MsgSize) + ' ' +
              'Line = ''' + String((Sender as TPop3Cli).LastResponse) + '''';
    {$IFDEF DEBUG}
    WriteLn(Buffer);
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Pop3ClientUidlBegin(Sender: TObject);
begin
    {$IFDEF DEBUG}
    WriteLn('*** Uidl begin ***');
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Pop3ClientUidlEnd(Sender: TObject);
begin
    {$IFDEF DEBUG}
    WriteLn('*** Uidl end ***');
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Pop3ClientUidlLine(Sender: TObject);
var
    Buffer : String;
begin
    Buffer := 'MsgNum = ' + IntToStr((Sender as TPop3Cli).MsgNum) + ' ' +
              'MsgUidl = ' + String((Sender as TPop3Cli).MsgUidl) + '''';
    {$IFDEF DEBUG}
    WriteLn(Buffer);
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.MessageBegin(Sender: TObject);
begin
    {$IFDEF DEBUG}
    WriteLn('Message ' +
                           IntToStr((Sender as TPop3Cli).MsgNum));
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.MessageLine(Sender: TObject);
begin
    {$IFDEF DEBUG}
    WriteLn((Sender as TPop3Cli).LastResponse);
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Nextevent(Sender: TObject);
begin

    Pop3Client.OnMessageBegin := MessageBegin;
    Pop3Client.OnMessageEnd   := nil;
    Pop3Client.OnMessageLine  := MessageLine;
    Pop3Client.OnRequestDone  := NextMessageRequestDone;
    Pop3Client.MsgNum         := MsgNum;
    Pop3Client.Retr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.NextMessageRequestDone(
    Sender  : TObject;
    RqType  : TPop3Request;
    ErrCode : Word);
begin
    if ErrCode <> 0 then
        Exit;
    Inc(MsgNum);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.GetAllMessageLine(Sender: TObject);
begin
    WriteLn(FFile, (Sender as TPop3Cli).LastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The procedure here after will start an event chain that will eventually   }
{ download all messages for the POP3 server. We cannot simply loop because  }
{ the POP3 compomnet is asynchronous: it will not wait for operation done   }
{ before returning. We must "chain" operations one after the other using    }
{ the OnRequestDone event handler. We use the variable FGetAllState to keep }
{ track of where we are.                                                    }
{ To get all messages, we must first call Stat to know how many messages    }
{ are on the server, then for each message we call Uidl to get a unique     }
{ identifier for each message to build a file name and know if we already   }
{ have a message, then we retrieve the message, then we increment the       }
{ message number and continue until the number of messages is reached.      }
{ We should start a TTimer to handle timeout...                             }
procedure TPOP3ExercizerApp.GetAllevent(Sender: TObject);
var
    IniFile : TIniFile;
begin
    { Get path from INI file }
    IniFile := TIniFile.Create(IniFileName);
    FMsgPath    := IniFile.ReadString(SectionData, KeyMsgPath,
                                      ExtractFilePath(ParamStr(0)));
    IniFile.Free;

    { Be sure to have an ending backslash }
    if (Length(FMsgPath) > 0) and (FMsgPath[Length(FMsgPath)] <> '\') then
        FMsgPath := FMsgPath + '\';

    FGetAllState := 0;
    FFileOpened  := FALSE;
    Pop3Client.OnRequestDone  := GetAllRequestDone;
    Pop3Client.OnMessageBegin := nil;
    Pop3Client.OnMessageEnd   := nil;
    Pop3Client.OnMessageLine  := GetAllMessageLine;
    Pop3Client.Stat;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when a request related to GetAll is done.    }
{ We check for errors and our state variable FGetAllState which tells us    }
{ where we are (stat, uidl or retr which are the 4 commands we use.         }
{ Note that we also could use Dele to remove the messages from the server.  }
procedure TPOP3ExercizerApp.GetAllRequestDone(
    Sender: TObject;
    RqType: TPop3Request; ErrCode: Word);
begin
    if ErrCode <> 0 then begin
        if FFileOpened then begin
            FFileOpened := FALSE;
            CloseFile(FFile);
        end;
        WriteLn('Error ' + Pop3Client.ErrorMessage);
        Exit;
    end;

    try
        case FGetAllState of
        0: begin     { Comes from the Stat command }
                if Pop3Client.MsgCount < 1 then begin
                    WriteLn('No message to download.');
                    Exit;
                end;
                Pop3Client.MsgNum := 1;    { Start with first message }
                FGetAllState := 1;
                Pop3Client.Uidl;
           end;
        1: begin     { Comes from the Uidl command }
                FFileName := FMsgPath + 'Msg ' + String(Pop3Client.MsgUidl) + '.txt';
                if FileExists(FFileName) then begin
                    WriteLn('Message ' + IntToStr(Pop3Client.MsgNum) + ' already here');
                    if Pop3Client.MsgNum >= Pop3Client.MsgCount then begin
                        WriteLn('Finished');
                        Exit;
                    end;
                    Pop3Client.MsgNum := Pop3Client.MsgNum + 1;
                    FGetAllState := 1;
                    Pop3Client.Uidl;
                end
                else begin
                    WriteLn('Message ' + IntToStr(Pop3Client.MsgNum));
                    AssignFile(FFile, FFileName);
                    Rewrite(FFile);
                    FFileOpened  := TRUE;
                    FGetAllState := 2;
                    Pop3Client.Retr;
                end;
           end;
        2: begin     { Comes from the Retr command }
                FFileOpened := FALSE;
                CloseFile(FFile);
                if Pop3Client.MsgNum >= Pop3Client.MsgCount then begin
                    WriteLn('Finished');
                    Exit;
                end;
                Pop3Client.MsgNum := Pop3Client.MsgNum + 1;
                FGetAllState := 1;
                Pop3Client.Uidl;
           end;
        else
            WriteLn('Invalid state');
            Exit;
        end;
    except
        on E:Exception do begin
            if FFileOpened then begin
                FFileOpened := FALSE;
                CloseFile(FFile);
            end;
            WriteLn('Error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Pop3ClientRequestDone(
    Sender  : TObject;
    RqType  : TPop3Request;
    ErrCode : Word);
begin
    WaitFor:=rqtype;
    {$IFDEF DEBUG}
    WriteLn('Request Done Rq=' + IntToStr(Integer(RqType)) +
                          ' Error=' + IntToStr(ErrCode));
    {$ENDIF}

    if RqType = pop3Stat then begin
                                  MsgCount:=Pop3Client.MsgCount;
        WriteLn('Stat ok, ' +
                             IntToStr(Pop3Client.MsgCount) + ' messages ' +
                             IntToStr(Pop3Client.MsgSize) + ' bytes');
    end
    else if RqType = pop3List then begin
        WriteLn('List ok, ' +
                             IntToStr(Pop3Client.MsgNum)  + ' message ' +
                             IntToStr(Pop3Client.MsgSize) + ' bytes');
    end
    else if RqType = pop3Last then begin
        WriteLn('Last = ' + IntToStr(Pop3Client.MsgNum));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Pop3ClientHeaderEnd(Sender: TObject);
begin
//    SubjectEdit.Text := Pop3Client.HeaderSubject;
//    FromEdit.Text    := Pop3Client.HeaderFrom;
//    ToEdit.Text      := Pop3Client.HeaderTo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Wait;
var
    St : TPop3State;
begin
    St := Pop3Client.State;
    WriteLn('pre state: ', ORD(Pop3Client.state));
    while Pop3Client.State = St do
        ProcessMessages;
    WriteLn('post state: ', ORD(Pop3Client.state));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.WaitReady;
var
    Current: TPop3State;
begin
    Current:=Pop3Client.state;
    while (Pop3Client.State<>Pop3Ready) do begin
        if Current<>Pop3Client.State then begin
            Current := Pop3Client.State;
            WriteLn('stage change: ',ORD(current));
        end;
        ProcessMessages;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.waitfordone(ev: TPop3Request);
begin
    while (Waitfor <> ev) and (Pop3Client.State <> Pop3Abort) do
        ProcessMessages;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
var
    App : TPOP3ExercizerApp;
    I   : Integer;
begin
    WriteLn(Trim(CopyRight));
    App := TPOP3ExercizerApp.Create(nil);
    App.ReadDataFromIni(nil);
    WriteLn('Connecting to: ',
            App.FHost, '/', App.FPort, '   user/pw=',
            App.FUser, '/', App.FPasswd);
    while TRUE do begin
        if not App.ConnectSyncEvent(App) then
            break;
        if not App.UserSyncEvent(App) then
            break;
        if not App.PassSyncEvent(App) then
            break;
        if not App.StatSyncEvent(App) then  // loads nr of msgs in App.MsgCount
            break;
        {$IFDEF DOHEADERS}
        App.OkMsgNumber.Clear;
        App.Lines := 0;                     // Iterate through all headers.
        WriteLn(App.msgcount);
        WriteLn(App.OkMsgNumber.Count);
        if App.MsgCount > 0 then begin
            for I := 1 to App.MsgCount do begin
                App.msgnum := I;
                App.TopSyncEvent(App);
            end;
        end;
        if App.OkMsgNumber.Count > 0 then begin
            for I:=0 to App.OkMsgNumber.Count - 1 do begin
                App.msgnum := Integer(App.OkMsgNumber[I]); // no beauty either
                App.RetrSyncEvent(App);
            end;
        end;
        {$ELSE}
        App.ListAllEvent(App);       // async is also possible.
        App.WaitForDone(Pop3list);   // but for the linear sequence like this not interesting.
        {$ENDIF}
    end;
    App.AbortEvent(App);
    App.Free;
end.

