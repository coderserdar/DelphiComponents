{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Oct 10, 1999
Description:  WebSrv1 show how to use THttpServer component to implement
              a web server. WARNING: The code below is for demonstration
              only. You need to add code to fit your needs about security.
              The code below allows to get all files on the computer running
              the demo. Add code in OnGetDocument, OnHeadDocument and
              OnPostDocument to check for authorized access to files.
Version:      1.08
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1999-2007 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>

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
May 21, 2000 V1.01 Worked around a bug with Delphi 3 and lpVendorInfo
Oct 07, 2001 V1.02 Added Logfile feature
                   Added display if time and IP Addr for GET command.
Feb 15, 2003 V1.03 Call PostedDataReceived so that things are handled
                   correctly with HTTP 1.1 version.
Mar 11, 2003 V1.04 Changer LingerOnOff to LingerNotSet (Wilfried)
Jul 19, 2003 V1.05 Support SSL (that is HTTPS protocol)
Nov 19, 2005 V1.06 Make this sample support both HTTPS (SSL) and HTTP on two
                   different ports (2 component used)
Dec 14, 2005 V1.07 A. Garrels fixed the call to get a session ID string,
                   added a simple SSL renegotiation request (doesn't work
                   with IE 6 so far!?).
Aug 04, 2005 V1.08 A. Garrels made a few changes to prepare code for Unicode.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSslWebServ1;
{$I OverbyteIcsDefs.inc}
{$IFNDEF DELPHI7_UP}
    Bomb('This sample requires Delphi 7 or later');
{$ENDIF}
{$IFNDEF USE_SSL}
    Bomb('Add USE_SSL in the define section in project options');
{$ENDIF}
{$B-}                 { Enable partial boolean evaluation   }
{$T-}                 { Untyped pointers                    }
{$X+}                 { Enable extended syntax              }
{$I+}                 { Turn IO exceptions to on            }
{$H+}                 { Use long strings                    }
{$J+}                 { Allow typed constant to be modified }
{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  OFF}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}


interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Controls, Forms,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, WinSock, OverbyteIcsWSocket,
  OverbyteIcsWSocketS, OverbyteIcsHttpSrv, OverbyteIcsLIBEAY,
  OverbyteIcsSslSessionCache, OverbyteIcsLogger;

const
  CopyRight : String         = 'WebServ (c) 1999-2006 F. Piette V1.07 ';
  Ssl_Session_ID_Context     = 'WebServ_Test';

type
  { This component is used for client connection instead of default one.    }
  { This enables to add any data we need to handle our application.         }
  { As this data is located in client component, each connected client has  }
  { his own private data.                                                   }
  TMyHttpConnection = class(THttpConnection)
  protected
    FPostedRawData    : PAnsiChar; { Will hold dynamically allocated buffer }
    FPostedDataBuffer : PChar;     { Contains either Unicode or Ansi data   } 
    FPostedDataSize   : Integer;   { Databuffer size                        }
    FDataLen          : Integer;   { Keep track of received byte count.     }
    LastHandshake     : Longword;
  public
    destructor  Destroy; override;
    constructor Create(AOwner: TComponent); override;
  end;

  { This is the main form for our application. Any data here is global for  }
  { all clients. Put provate data in TMyHttpConnection class (see above).   }
  TSslWebServForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    Label1: TLabel;
    DocDirEdit: TEdit;
    Label2: TLabel;
    DefaultDocEdit: TEdit;
    StartHttpsButton: TButton;
    StopButton: TButton;
    Label3: TLabel;
    PortHttpsEdit: TEdit;
    ClientHttpsCountLabel: TLabel;
    Label5: TLabel;
    ClearButton: TButton;
    DisplayHeaderCheckBox: TCheckBox;
    WriteLogFileCheckBox: TCheckBox;
    Label4: TLabel;
    CertFileEdit: TEdit;
    Label6: TLabel;
    PrivKeyFileEdit: TEdit;
    Label7: TLabel;
    PassPhraseEdit: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    AcceptableHostsEdit: TEdit;
    Label10: TLabel;
    CAPathEdit: TEdit;
    Label11: TLabel;
    CAFileEdit: TEdit;
    VerifyPeerCheckBox: TCheckBox;
    SslHttpServer1: TSslHttpServer;
    SslContext1: TSslContext;
    HttpServer2: THttpServer;
    StartHttpButton: TButton;
    PortHttpEdit: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    ClientHttpCountLabel: TLabel;
    RenegotiationIntervalEdit: TEdit;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    ButtonOSSLVersion: TButton;
    DisplaySslInfoCheckBox: TCheckBox;
    IcsLogger1: TIcsLogger;
    SslAvlSessionCache1: TSslAvlSessionCache;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SslHttpServer1GetDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure StartHttpsButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure SslHttpServer1ClientConnect(Sender: TObject;
      Client: TObject; Error: Word);
    procedure SslHttpServer1ClientDisconnect(Sender: TObject;
      Client: TObject; Error: Word);
    procedure SslHttpServer1ServerStarted(Sender: TObject);
    procedure SslHttpServer1ServerStopped(Sender: TObject);
    procedure SslHttpServer1HeadDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure SslHttpServer1PostedData(Sender: TObject;
      Client: TObject; Error: Word);
    procedure SslHttpServer1PostDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure ClearButtonClick(Sender: TObject);
    procedure WriteLogFileCheckBoxClick(Sender: TObject);
    procedure SslHttpServer1SslVerifyPeer(Sender: TObject; var Ok: Integer;
      Cert: TX509Base);
    procedure FormDestroy(Sender: TObject);
    procedure SslHttpServer1SslSvrGetSession(Sender: TObject;
      var SslSession: Pointer; SessId: Pointer; Idlen: Integer;
      var IncRefCount: Boolean);
    procedure SslHttpServer1SslSvrNewSession(Sender: TObject; SslSession,
      SessId: Pointer; Idlen: Integer; var AddToInternalCache: Boolean);
    procedure StartHttpButtonClick(Sender: TObject);
    procedure HttpServer2ServerStopped(Sender: TObject);
    procedure HttpServer2ServerStarted(Sender: TObject);
    procedure HttpServer2ClientConnect(Sender, Client: TObject;
      Error: Word);
    procedure HttpServer2ClientDisconnect(Sender, Client: TObject;
      Error: Word);
    procedure RenegotiationIntervalEditChange(Sender: TObject);
    procedure SslHttpServer1BeforeProcessRequest(Sender, Client: TObject);
    procedure SslHttpServer1SslHandshakeDone(Sender: TObject;
      ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
    procedure ButtonOSSLVersionClick(Sender: TObject);
    procedure SslHttpServer1SslSetSessionIDContext(Sender: TObject;
      var SessionIDContext: String);
  private
    FIniFileName            : String;
    FInitialized            : Boolean;
    FCountRequests          : Integer;
    FLogFile                : TextFile;
    FLogFileName            : String;
    FLogFileOpened          : Boolean;
    FRenegotiationInterval  : Longword;
    procedure CreateVirtualDocument_Demo(Sender    : TObject;
                                         Client    : TObject;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_time_htm(Sender    : TObject;
                                    Client    : TObject;
                                    var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_redir_htm(Sender    : TObject;
                                    Client    : TObject;
                                    var Flags : THttpGetFlag);
    procedure BackgroundException(Sender : TObject;
                                  E            : Exception;
                                  var CanClose : Boolean);

    procedure DisplayHeader(Client : TMyHttpConnection);
    procedure ProcessPostedData_CgiFrm1(Client : TMyHttpConnection);
    procedure CloseLogFile;
    procedure OpenLogFile;
  public
    procedure Display(Msg : String);
    property  IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  SslWebServForm: TSslWebServForm;

implementation

{$R *.DFM}

const
    { IniFile layout for persistent data }
    SectionWindow      = 'WindowMain';
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeyDocDir          = 'DocDir';
    KeyDefaultDoc      = 'DefaultDoc';
    KeyPortHttps       = 'PortHttps';
    KeyPortHttp        = 'PortHttp';
    KeyDisplayHeader   = 'DisplayHeader';
    KeyDisplaySslInfo  = 'DisplaySslInfo';
    KeyLogToFile       = 'LogToFile';
    KeyCertFile        = 'CertFile';
    KeyPassPhrase      = 'PassPhrase';
    KeyPrivKeyFile     = 'PrivKeyFile';
    KeyVerifyPeer      = 'VerifyPeer';
    KeyCAFile          = 'CAFile';
    KeyCAPath          = 'CAPath';
    KeyAcceptableHosts = 'AcceptableHosts';
    KeyRenegInterval   = 'RenegotiationInterval';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.FormCreate(Sender: TObject);
begin
{$IFDEF DELPHI10_UP}
    // BDS2006 has built-in memory leak detection and display
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$ENDIF}
    //IsConsole := AllocConsole;
    { Create IniFileName based on EXE file name; }
    FIniFileName := GetIcsIniFileName;
    FLogFileName := ChangeFileExt(FIniFileName, '.log');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.FormDestroy(Sender: TObject);
begin
    //
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
    wsi     : TWSADATA;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        { Restore persistent data from INI file }
        IniFile      := TIcsIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        DocDirEdit.Text     := IniFile.ReadString(SectionData, KeyDocDir,
                                                  '..\Internet\WebServData\WwwRoot');
        DefaultDocEdit.Text := IniFile.ReadString(SectionData, KeyDefaultDoc,
                                                  'index.html');
        PortHttpsEdit.Text  := IniFile.ReadString(SectionData, KeyPortHttps,
                                                  '443');
        PortHttpEdit.Text   := IniFile.ReadString(SectionData, KeyPortHttp,
                                                  '80');
        DisplayHeaderCheckBox.Checked :=
                Boolean(IniFile.ReadInteger(SectionData, KeyDisplayHeader, 0));
        WriteLogFileCheckBox.Checked :=
                Boolean(IniFile.ReadInteger(SectionData, KeyLogToFile, 0));
        DisplaySslInfoCheckBox.Checked :=
                Boolean(IniFile.ReadInteger(SectionData, KeyDisplaySslInfo, 0));
        CertFileEdit.Text    := IniFile.ReadString(SectionData, KeyCertFile,
                                                   '01cert.pem');
        PrivKeyFileEdit.Text := IniFile.ReadString(SectionData, KeyPrivKeyFile,
                                                   '01key.pem');
        PassPhraseEdit.Text  := IniFile.ReadString(SectionData, KeyPassPhrase,
                                                   'password');
        CAFileEdit.Text      := IniFile.ReadString(SectionData, KeyCAFile,
                                                   'cacert.pem');
        CAPathEdit.Text      := IniFile.ReadString(SectionData, KeyCAPath,
                                                   '');
        AcceptableHostsEdit.Text := IniFile.ReadString(SectionData, KeyAcceptableHosts,
                                                       'www.overbyte.be;www.borland.com');
        VerifyPeerCheckBox.Checked := Boolean(IniFile.ReadInteger(SectionData,
                                                                  KeyVerifyPeer,
                                                                  0));
        FRenegotiationInterval := IniFile.ReadInteger(SectionData,
                                                      KeyRenegInterval, 0);
        IniFile.Free;

        RenegotiationIntervalEdit.Text := IntToStr(FRenegotiationInterval);
        
        { Start log file }
        if WriteLogFileCheckBox.Checked then begin
            OpenLogFile;
            WriteLogFileCheckBox.Checked := FLogFileOpened;
        end;
        { Initialize client count caption }
        ClientHttpsCountLabel.Caption := '0';
        ClientHttpCountLabel.Caption  := '0';
        { Display version info for program and used components }
        wsi := WinsockInfo;
        DisplayMemo.Clear;
        Display(CopyRight);
        Display('Using:');
        Display('   ' + OverbyteIcsWSocket.CopyRight);
        Display('   ' + OverbyteIcsWSocketS.CopyRight);
        Display('   ' + OverbyteIcsHttpSrv.CopyRight);
        Display('    Winsock:');
        Display('        Version ' +
                Format('%d.%d', [WinsockInfo.wHighVersion shr 8,
                                 WinsockInfo.wHighVersion and 15]));
        Display('        ' + StrPas(wsi.szDescription));
        Display('        ' + StrPas(wsi.szSystemStatus));
{$IFNDEF VER100}
        { A bug in Delphi 3 makes lpVendorInfo invalid }
        if wsi.lpVendorInfo <> nil then
            Display('        ' + StrPas(wsi.lpVendorInfo));
{$ENDIF}
        { Automatically start server }
        StartHttpsButtonClick(Self);
        StartHttpButtonClick(Self);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    { Save persistent data to INI file }
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.WriteString(SectionData,    KeyDocDir,      SslHttpServer1.DocDir);
    IniFile.WriteString(SectionData,    KeyDefaultDoc,  SslHttpServer1.DefaultDoc);
    IniFile.WriteString(SectionData,    KeyPortHttps,   SslHttpServer1.Port);
    IniFile.WriteString(SectionData,    KeyPortHttp,    HttpServer2.Port);
    IniFile.WriteInteger(SectionData,   KeyDisplayHeader,
                                        ord(DisplayHeaderCheckBox.Checked));
    IniFile.WriteInteger(SectionData,   KeyLogToFile,
                                        ord(WriteLogFileCheckBox.Checked));
    IniFile.WriteInteger(SectionData,   KeyDisplaySslInfo,
                                        ord(DisplaySslInfoCheckBox.Checked));
    IniFile.WriteString(SectionData,    KeyCertFile,    CertFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyPrivKeyFile, PrivKeyFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyPassPhrase,  PassPhraseEdit.Text);
    IniFile.WriteString(SectionData,    KeyCAFile,      CAFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyCAPath,      CAPathEdit.Text);
    IniFile.WriteString(SectionData,    KeyAcceptableHosts, AcceptableHostsEdit.Text);
    IniFile.WriteInteger(SectionData,   KeyVerifyPeer,  Ord(VerifyPeerCheckBox.Checked));
    IniFile.WriteInteger(SectionData,   KeyRenegInterval, FRenegotiationInterval);
    IniFile.UpdateFile;
    IniFile.Free;
    CloseLogFile;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Display a message in display memo box, making sure we don't overflow it.  }
procedure TSslWebServForm.Display(Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        { We preserve only 200 lines }
        while DisplayMemo.Lines.Count > 200 do
            DisplayMemo.Lines.Delete(0);
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        { Makes last line visible }
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
    if FLogFileOpened then begin
        try
            WriteLn(FLogFile, Msg);
        except
            on E:Exception do begin
                DisplayMemo.Lines.Add('*** Exception' +
                                      E.CLassName + ': ' + E.Message +
                                      ' writing to log file ***');
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when user clicks on start button. It is also }
{ called from FormShow event handler, at program startup. It starts server. }
{ We need to pass default document, document directory and client class     }
{ to HTTP server component. Client class is very usefull because it         }
{ instruct server component to instanciate our own client class instead of  }
{ defualt client class. Using our own client class will enables you to add  }
{ any data we need to handle our application. This data is private for each }
{ client.                                                                   }
{ When server is started, we will get OnServerStarted event triggered.      }
procedure TSslWebServForm.StartHttpsButtonClick(Sender: TObject);
begin
    SslHttpServer1.DocDir           := Trim(DocDirEdit.Text);
    SslHttpServer1.DefaultDoc       := Trim(DefaultDocEdit.Text);
    SslHttpServer1.Port             := Trim(PortHttpsEdit.Text);
    SslHttpServer1.ClientClass      := TMyHttpConnection;
    SslHttpServer1.SetAcceptableHostsList(AcceptableHostsEdit.Text); 
    SslContext1.SslCertFile         := CertFileEdit.Text;
    SslContext1.SslPassPhrase       := PassPhraseEdit.Text;
    SslContext1.SslPrivKeyFile      := PrivKeyFileEdit.Text;
    SslContext1.SslCAFile           := CAFileEdit.Text;
    SslContext1.SslCAPath           := CAPathEdit.Text;
    SslContext1.SslVerifyPeer       := VerifyPeerCheckBox.Checked;

    SslHttpServer1.Start;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.StartHttpButtonClick(Sender: TObject);
begin
    // Just a little quick test to support also HTTP without SSL
    HttpServer2.DocDir         := Trim(DocDirEdit.Text);
    HttpServer2.DefaultDoc     := Trim(DefaultDocEdit.Text);
    HttpServer2.Port           := Trim(PortHttpEdit.Text);
    HttpServer2.ClientClass    := TMyHttpConnection;
    HttpServer2.Start;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when user clicks on stop button. We just  }
{ stop the server. We will get OnServerStopped event triggered.             }
procedure TSslWebServForm.StopButtonClick(Sender: TObject);
begin
    SslHttpServer1.Stop;
    HttpServer2.Stop;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when user clicks on clear buttoN; We just }
{ clear the memo used for displaying activity.                              }
procedure TSslWebServForm.ClearButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when HTTP server is started, that is when }
{ server socket has started listening.                                      }
procedure TSslWebServForm.SslHttpServer1ServerStarted(Sender: TObject);
begin
    DocDirEdit.Enabled       := FALSE;
    DefaultDocEdit.Enabled   := FALSE;
    PortHttpsEdit.Enabled    := FALSE;
    PortHttpEdit.Enabled     := FALSE;
    StartHttpsButton.Enabled := FALSE;
    StopButton.Enabled       := TRUE;
    Display('HTTPS Server is waiting for connections');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.HttpServer2ServerStarted(Sender: TObject);
begin
    StartHttpButton.Enabled    := FALSE;
    Display('HTTP Server is waiting for connections');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when server has been stopped, that is     }
{ when server socket stop listening.                                        }
procedure TSslWebServForm.SslHttpServer1ServerStopped(Sender: TObject);
begin
    DocDirEdit.Enabled       := TRUE;
    DefaultDocEdit.Enabled   := TRUE;
    PortHttpsEdit.Enabled    := TRUE;
    StartHttpsButton.Enabled := TRUE;
    //StopButton.Enabled     := FALSE;
    Display('HTTPS Server stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.HttpServer2ServerStopped(Sender: TObject);
begin
    PortHttpEdit.Enabled    := TRUE;
    StartHttpButton.Enabled := TRUE;
    Display('HTTP Server stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when a new client has connected.          }
procedure TSslWebServForm.SslHttpServer1ClientConnect(
    Sender : TObject;               { HTTP server component                 }
    Client : TObject;               { Client connecting                     }
    Error  : Word);                 { Error in connection                   }
begin
    ClientHttpsCountLabel.Caption :=
        IntToStr((Sender as THttpServer).ClientCount);
    TMyHttpConnection(Client).OnBgException := BackgroundException;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when a client is disconnecting, just      }
{ before client component is closed.                                        }
procedure TSslWebServForm.SslHttpServer1ClientDisconnect(
    Sender : TObject;               { HTTP server component                 }
    Client : TObject;               { Client connecting                     }
    Error  : Word);                 { Error in disconnection                }
begin
    ClientHttpsCountLabel.Caption :=
        IntToStr((Sender as THttpServer).ClientCount - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.HttpServer2ClientConnect(
    Sender : TObject;               { HTTP server component                 }
    Client : TObject;               { Client connecting                     }
    Error  : Word);                 { Error in connection                   }
begin
    ClientHttpCountLabel.Caption :=
        IntToStr((Sender as THttpServer).ClientCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.HttpServer2ClientDisconnect(
    Sender : TObject;               { HTTP server component                 }
    Client : TObject;               { Client connecting                     }
    Error  : Word);                 { Error in disconnection                }
begin
    ClientHttpCountLabel.Caption :=
        IntToStr((Sender as THttpServer).ClientCount - 1);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when HTTP server component receive a HEAD }
{ command from any client.                                                  }
{ We just count the request, display a message and let HTTP server          }
{ component handle everything.                                              }
{ We should trap every URI we handle internally...                          }
procedure TSslWebServForm.SslHttpServer1HeadDocument(
    Sender    : TObject;            { HTTP server component                 }
    Client    : TObject;            { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
begin
    Inc(FCountRequests);
    Display(IntToStr(FCountRequests) +
            ': HEAD ' + TMyHttpConnection(Client).Path);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when HTTP server component receive a GET  }
{ command from any client.                                                  }
{ We count the request, display a message and trap '/time.htm' path for     }
{ special handling.                                                         }
{ There is no document time.htm on disk, we will create it on the fly. With }
{ a classic webserver we would have used a CGI or ISAPI/NSAPI to achieve    }
{ the same goal. It is much easier here since we can use Delphi code        }
{ directly to generate whatever we wants. Here for the demo we generate a   }
{ page with server data and time displayed.                                 }
procedure TSslWebServForm.SslHttpServer1GetDocument(
    Sender    : TObject;            { HTTP server component                 }
    Client    : TObject;            { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next } 
begin
    { Count request and display a message }
    Inc(FCountRequests);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
            TWSocket(Client).GetPeerAddr + '] ' + IntToStr(FCountRequests) +
            ': GET ' + TMyHttpConnection(Client).Path);
    DisplayHeader(TMyHttpConnection(Client));

    { Trap '/time.htm' path to dynamically generate an answer. }
    if CompareText(THttpConnection(Client).Path, '/demo.html') = 0 then
        CreateVirtualDocument_Demo(Sender, Client, Flags)
    else if CompareText(THttpConnection(Client).Path, '/time.html') = 0 then
        CreateVirtualDocument_time_htm(Sender, Client, Flags)
    { Trap '/redir.htm' to dynamically generate a redirection answer }
    else if CompareText(THttpConnection(Client).Path, '/redir.html') = 0 then
        CreateVirtualDocument_redir_htm(Sender, Client, Flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /redir.htm document                     }
procedure TSslWebServForm.CreateVirtualDocument_redir_htm(
    Sender    : TObject;            { HTTP server component                 }
    Client    : TObject;            { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
var
    Body     : String;
    Header   : String;
    Stream   : TMemoryStream;
    Location : String;
begin
    Location := (Client as THttpConnection).Params;
    if Location = '' then
        Location := '/time.html';

    { Let HTTP server component know we will send data to client }
    Flags  := hgWillSendMySelf;
    { Create a stream to hold data sent to client that is the answer }
    { made of a HTTP header and a body made of HTML code.            }
    Stream := TMemoryStream.Create;
    Body   := '<HTML>' +
                '<HEAD>' +
                  '<TITLE>ICS WebServer Demo - Redir</TITLE>' +
                '</HEAD>' + #13#10 +
                '<BODY>' +
                  'You should be redirected automatically !<BR>' + #13#10 +
                  '<A HREF="' + Location + '">Click Here</A><BR>' + #13#10 +
                '</BODY>' +
              '</HTML>' + #13#10;
    Header := TMyHttpConnection(Client).Version + ' 302 OK' + #13#10 +
              'Content-Type: text/html' + #13#10 +
              'Location: ' + Location + #13#10 +
              'Content-Length: ' +
              IntToStr(Length(Body)) + #13#10 +
              #13#10;
    //Stream.Write(Header[1], Length(Header));
    StreamWriteStrA(Stream, Header);
    //Stream.Write(Body[1],   Length(Body));
    StreamWriteStrA(Stream, Body);
    { We need to seek to start of stream ! }
    Stream.Seek(0, 0);
    { We ask server component to send the stream for us. }
    TMyHttpConnection(Client).DocStream := Stream;
    TMyHttpConnection(Client).SendStream;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.CreateVirtualDocument_Demo(
    Sender    : TObject;
    Client    : TObject;
    var Flags : THttpGetFlag);
begin
    TMyHttpConnection(Client).AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS-SSL WebServer Demo - Menu</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS-SSL WebServer Demo Menu</H2>' +
            '<A HREF="/time.html">Server time</A><BR>'  +
            '<A HREF="/redir.html">Redirection</A><BR>' +
            '<A HREF="/">Default document</A><BR>'     +
            '<A HREF="http://www.overbyte.be">ICS Home page</A><BR>' +
            'Note: You can find a better demo in the non-SSL ICS.<BR>' +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /time.htm document                      }
procedure TSslWebServForm.CreateVirtualDocument_time_htm(
    Sender    : TObject;            { HTTP server component                 }
    Client    : TObject;            { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
var
    Body   : String;
    Header : String;
    Stream : TMemoryStream;
begin
    { Let HTTP server component know we will send data to client }
    Flags  := hgWillSendMySelf;
    { Create a stream to hold data sent to client that is the answer }
    { made of a HTTP header and a body made of HTML code.            }
    Stream := TMemoryStream.Create;
    Body   := '<HTML>' +
                '<HEAD>' +
                  '<TITLE>ICS WebServer Demo</TITLE>' +
                '</HEAD>' + #13#10 +
                '<BODY>' +
                  '<H2>Time at server side:</H2>' + #13#10 +
                  '<P>' + DateTimeToStr(Now) +'</P>' + #13#10 +
                '</BODY>' +
              '</HTML>' + #13#10;
    Header := TMyHttpConnection(Client).Version + ' 200 OK' + #13#10 +
              'Content-Type: text/html' + #13#10 +
              'Content-Length: ' +
              IntToStr(Length(Body)) + #13#10 +
              #13#10;
    //Stream.Write(Header[1], Length(Header));
    StreamWriteStrA(Stream, Header);
    //Stream.Write(Body[1],   Length(Body));
    StreamWriteStrA(Stream, Body);
    { We need to seek to start of stream ! }
    Stream.Seek(0, 0);
    { We ask server component to send the stream for us. }
    TMyHttpConnection(Client).DocStream := Stream;
    TMyHttpConnection(Client).SendStream;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when HTTP server component receive a POST }
{ command from any client.                                                  }
{ We count the request, display a message and trap posted data.             }
{ To check for posted data, you may construct the following HTML document:  }
{ <HTML>                                                                    }
{   <HEAD>                                                                  }
{     <TITLE>Test Form 1</TITLE>                                            }
{   </HEAD>                                                                 }
{   <BODY>                                                                  }
{     <H2>Enter your first and last name</H2>                               }
{     <FORM METHOD="POST" ACTION="/cgi-bin/cgifrm1.exe">                    }
{       <TABLE BORDER="0" ALIGN="DEFAULT" WIDTH="100%">                     }
{         <TR>                                                              }
{           <TD>First name</TD>                                             }
{           <TD><INPUT TYPE="TEXT" NAME="FirstName"                         }
{                      MAXLENGTH="25" VALUE="YourFirstName"></TD>           }
{         </TR>                                                             }
{         <TR>                                                              }
{           <TD>Last name</TD>                                              }
{           <TD><INPUT TYPE="TEXT" NAME="LastName"                          }
{                      MAXLENGTH="25" VALUE="YourLastName"></TD>            }
{         </TR>                                                             }
{       </TABLE>                                                            }
{       <P><INPUT TYPE="SUBMIT" NAME="Submit" VALUE="Button"></P>           }
{     </FORM>                                                               }
{   </BODY>                                                                 }
{ </HTML>                                                                   }
procedure TSslWebServForm.SslHttpServer1PostDocument(
    Sender    : TObject;            { HTTP server component                 }
    Client    : TObject;            { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
var
    Remote  : TMyHttpConnection;
begin
    { It's easyer to do the cast one time. Could use with clause... }
    Remote := TMyHttpConnection(Client);
    
    { Count request and display a message }
    Inc(FCountRequests);
    Display(IntToStr(FCountRequests) + ': POST ' + Remote.Path);
    DisplayHeader(Remote);

    { Check for request past. We only accept data for '/cgi-bin/cgifrm1.exe' }
    if CompareText(Remote.Path, '/cgi-bin/cgifrm1.exe') = 0 then begin
        { Tell HTTP server that we will accept posted data        }
        { OnPostedData event will be triggered when data comes in }
        Flags := hgAcceptData;
        { We wants to receive any data type. So we turn line mode off on   }
        { client connection.                                               }
        Remote.LineMode := FALSE;
        { We need a buffer to hold posted data. We allocate as much as the }
        { size of posted data plus one byte for terminating nul char.      }
        { We should check for ContentLength = 0 and handle that case...    }
        ReallocMem(Remote.FPostedRawData, Remote.RequestContentLength + 1);
        { Clear received length }
        Remote.FDataLen := 0;
    end
    else
        Flags := hg404;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered for each data packet posted by client     }
{ when we told HTTP server component that we will accept posted data.       }
{ We have to receive ALL data which is sent by remote client, even if there }
{ is more than what ContentLength tells us !                                }
{ If ContentLength = 0, then we should receive data until connection is     }
{ closed...                                                                 }
procedure TSslWebServForm.SslHttpServer1PostedData(
    Sender : TObject;               { HTTP server component                 }
    Client : TObject;               { Client posting data                   }
    Error  : Word);                 { Error in data receiving               }
var
    Len     : Integer;
    Remains : Integer;
    Junk    : array [0..255] of AnsiChar;
    Remote  : TMyHttpConnection;
begin
    { It's easyer to do the cast one time. Could use with clause... }
    Remote := TMyHttpConnection(Client);

    { How much data do we have to receive ? }
    Remains := Remote.RequestContentLength - Remote.FDataLen;
    if Remains <= 0 then begin
        { We got all our data. Junk anything else ! }
        Len := Remote.Receive(@Junk, SizeOf(Junk) - 1);
        if Len >= 0 then
            Junk[Len] := #0;
        Exit;
    end;
    { Receive as much data as we need to receive. But warning: we may       }
    { receive much less data. Data will be split into several packets we    }
    { have to assemble in our buffer.                                       }
    Len := Remote.Receive(Remote.FPostedRawData + Remote.FDataLen, Remains);
    { Sometimes, winsock doesn't wants to givve any data... }
    if Len <= 0 then
        Exit;

    { Add received length to our count }
    Inc(Remote.FDataLen, Len);
    { Add a nul terminating byte (handy to handle data as a string) }
    Remote.FPostedDataBuffer[Remote.FDataLen] := #0;
    { Display receive data so far }
    Display('Data: ''' + StrPas(Remote.FPostedRawData) + '''');

    { When we received the whole thing, we can process it }
    if Remote.FDataLen = Remote.RequestContentLength then begin
{$IFDEF COMPILER12_UP}
        Remote.FPostedDataBuffer := Pointer(UnicodeString(Remote.FPostedRawData)); // Cast to Unicode
{$ELSE}
        Remote.FPostedDataBuffer := Remote.FPostedRawData;
{$ENDIF}
        { First we must tell the component that we've got all the data }
        Remote.PostedDataReceived;
        { Then we check if the request is one we handle }
        if CompareText(Remote.Path, '/cgi-bin/cgifrm1.exe') = 0 then
            { We are happy to handle this one }
            ProcessPostedData_CgiFrm1(Remote)
        else
            { We don't accept any other request }
            Remote.Answer404;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will process posted data for CgiFrm1.exe                             }
procedure TSslWebServForm.ProcessPostedData_CgiFrm1(Client : TMyHttpConnection);
var
    Stream    : TStream;
    FileName  : String;
    Body      : String;
    Header    : String;
    FirstName : String;
    LastName  : String;
    HostName  : String;
    Buf       : String;
begin
    { Extract fields from posted data. }
    ExtractURLEncodedValue(Client.FPostedDataBuffer, 'FirstName', FirstName);
    ExtractURLEncodedValue(Client.FPostedDataBuffer, 'LastName',  LastName);
    { Get client IP address. We could to ReverseDnsLookup to get hostname }
    HostName := Client.PeerAddr;
    { Build the record to write to data file }
    Buf      := FormatDateTime('YYYYMMDD HHNNSS ', Now) +
                FirstName + '.' + LastName + '@' + HostName + #13#10;

    { Save data to a text file }
    FileName := ExtractFilePath(Application.ExeName) + 'CgiFrm1.txt';
    if FileExists(FileName) then
        Stream := TFileStream.Create(FileName, fmOpenWrite)
    else
        Stream := TFileStream.Create(FileName, fmCreate);
    Stream.Seek(0, soFromEnd);
    StreamWriteStrA(Stream, Buf);
    Stream.Destroy;

    { Now create output stream to send back to remote client }
    Stream := TMemoryStream.Create;
    Body   := '<HTML>' +
                '<HEAD>' +
                  '<TITLE>ICS WebServer Demo</TITLE>' +
                '</HEAD>' + #13#10 +
                '<BODY>' +
                  '<H2>Your data has been recorded:</H2>' + #13#10 +
                  '<P>' + FirstName + '.' + LastName + '@' + HostName +'</P>' +
                '</BODY>' +
              '</HTML>' + #13#10;
    Header := Client.Version + ' 200 OK' + #13#10 +
              'Content-Type: text/html' + #13#10 +
              'Content-Length: ' +
              IntToStr(Length(Body)) + #13#10 +
              #13#10;
    //Stream.Write(Header[1], Length(Header));
    StreamWriteStrA(Stream, Header);
    //Stream.Write(Body[1],   Length(Body));
    StreamWriteStrA(Stream, Body);
    Stream.Seek(0, 0);
    { Ask HTTP server component to send data stream for us }
    Client.DocStream := Stream;
    Client.SendStream;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.DisplayHeader(Client : TMyHttpConnection);
var
    I : Integer;
begin
    if not DisplayHeaderCheckBox.Checked then
        Exit;
    for I := 0 to Client.RequestHeader.Count - 1 do
        Display('HDR' + IntToStr(I + 1) + ') ' +
                Client.RequestHeader.Strings[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMyHttpConnection.Create(AOwner: TComponent);
begin
    inherited;
    { Little speed test }
    //SndBlkSize := 10 * 1024;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We need to override parent class destructor because we have allocated     }
{ memory for our data buffer.                                               }
destructor TMyHttpConnection.Destroy;
begin
    if Assigned(FPostedDataBuffer) then begin
        FreeMem(FPostedDataBuffer, FPostedDataSize);
        FPostedDataBuffer := nil;
        FPostedDataSize   := 0;
    end;
    if Assigned(FPostedRawData) then begin
        FreeMem(FPostedRawData);
        FPostedRawData := nil;
        FPostedDataSize   := 0;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.OpenLogFile;
begin
    if FLogFileOpened then
        Exit;
    try
        AssignFile(FLogFile, FLogFileName);
        if FileExists(FLogFileName) then
            Append(FLogFile)
        else
            Rewrite(FLogFile);
        WriteLn(FLogFile, '[' + FormatDateTime('HH:NN:SS YYYY/MM/DD', Now) +
                          ' Log file opened.]');
        FLogFileOpened := TRUE;
    except
        FLogFileOpened := FALSE;
        Display('*** Unable to open log file ***');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.CloseLogFile;
begin
    if not FLogFileOpened then
        Exit;
    FLogFileOpened := FALSE;
    WriteLn(FLogFile, '[' + FormatDateTime('HH:NN:SS YYYY/MM/DD', Now) +
                      ' Log file Closed.]');
    CloseFile(FLogFile);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.WriteLogFileCheckBoxClick(Sender: TObject);
begin
    if WriteLogFileCheckBox.Checked then
        OpenLogFile
    else
        CloseLogFile;
    WriteLogFileCheckBox.Checked := FLogFileOpened;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.SslHttpServer1SslVerifyPeer(
    Sender  : TObject;
    var Ok  : Integer;
    Cert    : TX509Base);
begin
    if DisplaySslInfoCheckBox.Checked then
        Display('Received certificate'#13#10 +
                'Subject: ' + Cert.SubjectOneLine + #13#10 +
                'Issuer: '  + Cert.IssuerOneLine);
    if OK <> 1 then begin
        if DisplaySslInfoCheckBox.Checked then
            Display('Error msg: ' + Cert.VerifyErrMsg + #13#10 +
                    'In this example we accept any cert');
        OK := 1; //In this example we accept any client.
    end;    
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.SslHttpServer1SslSetSessionIDContext(
    Sender               : TObject;
    var SessionIDContext : String);
begin
    { Tell Openssl a Session_ID_Context.                                    }
    { Openssl uses this data to tag a session before it's cached.           }
    SessionIDContext := Ssl_Session_ID_Context;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.SslHttpServer1SslSvrGetSession(
    Sender          : TObject;
    var SslSession  : Pointer;
    SessId          : Pointer;
    Idlen           : Integer;
    var IncRefCount : Boolean);
var
    LookupKey : string;
begin
{$IFDEF UNICODE}
    { We need to get binary data into a UnicodeString, allocate enough space. }
    { Not nice, however works in this case.                                   }
    SetLength(LookupKey, (IDLen div 2) + (IdLen mod 2));
{$ELSE}
    SetLength(LookupKey, IDLen);
{$ENDIF}
    Move(SessId^, Pointer(LookupKey)^, IDLen);
    SslSession  := SslAvlSessionCache1.GetSvrSession(LookupKey +
                                                     Ssl_Session_ID_Context,
                                                     IncRefCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.SslHttpServer1SslSvrNewSession(
    Sender                 : TObject;
    SslSession,
    SessId                 : Pointer;
    Idlen                  : Integer;
    var AddToInternalCache : Boolean);
var
    LookupKey : string;
begin
{$IFDEF UNICODE}
    { We need to get binary data into a UnicodeString, allocate enough space. }
    { Not nice, however works in this case.                                   }
    SetLength(LookupKey, (IDLen div 2) + (IdLen mod 2));
{$ELSE}
    SetLength(LookupKey, IDLen);
{$ENDIF}
    Move(SessId^, Pointer(LookupKey)^, IDLen);
    SslAvlSessionCache1.CacheSvrSession(SslSession,
                                        LookupKey + Ssl_Session_ID_Context,
                                        AddToInternalCache);
    if DisplaySslInfoCheckBox.Checked then
        Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
                TWSocket(Sender).GetPeerAddr + '] New SSL session created and ' +
                'cached in external cache class.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.SslHttpServer1BeforeProcessRequest(
    Sender,
    Client : TObject);
var
    Ticks  : Longword;
    Remote : TMyHttpConnection;
begin
    Remote := Client as TMyHttpConnection;
    { Request SSL3 renegotiation - doesn't work with IE so far!? }
    if Remote.SslEnable and (FRenegotiationInterval > 0) then begin
        Ticks := GetTickCount;
        if Remote.LastHandshake + FRenegotiationInterval < Ticks then begin
            if not Remote.SslStartRenegotiation then begin
                if DisplaySslInfoCheckBox.Checked then
                    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
                       Remote.GetPeerAddr + '] SslStartRenegotiation failed ');
            end
            else
                if DisplaySslInfoCheckBox.Checked then
                    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
                       Remote.GetPeerAddr + '] SSL renegotiation flag set.');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.SslHttpServer1SslHandshakeDone(
    Sender         : TObject;
    ErrCode        : Word;
    PeerCert       : TX509Base;
    var Disconnect : Boolean);
var
    Remote : TMyHttpConnection;
begin
    Remote := Sender as TMyHttpConnection;
    if ErrCode = 0 then begin
        Remote.LastHandshake := GetTickCount;
        if DisplaySslInfoCheckBox.Checked then
            Display(Format('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
                    Remote.GetPeerAddr + '] SslHandshakeDone. Secure ' +
                    'connection with %s, cipher %s, %d secret bits ' +
                    '(%d total), SessionReused %d',
                    [Remote.SslVersion, Remote.SslCipher,
                    Remote.SslSecretBits, Remote.SslTotalBits,
                    Ord(Remote.SslSessionReused)]));
    end                   
    else
        if DisplaySslInfoCheckBox.Checked then
            Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
                       Remote.GetPeerAddr + '] SslHandshake failed.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.ButtonOSSLVersionClick(Sender: TObject);
begin
    SslContext1.InitContext; //Pre-loads OpenSSL DLL's
    Display(OpenSslVersion);
    Display(OpenSslCompilerFlags);
    Display(OpenSslBuiltOn);
    Display(OpenSslPlatForm);
    Display(OpenSslDir);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.RenegotiationIntervalEditChange(Sender: TObject);
begin
    try
        FRenegotiationInterval := StrToInt((Sender as TEdit).Text);
    except
        FRenegotiationInterval := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWebServForm.BackgroundException(
    Sender       : TObject;
    E            : Exception;
    var CanClose : Boolean);
begin
    raise Exception.Create('BgException: ' + E.ClassName + ': ' + E.Message); //Test
    CanClose := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

