{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Oct 10, 1999
Description:  WebSrv1 show how to use THttpServer component to implement
              a web server. WARNING: The code below is for demonstration
              only. You need to add code to fit your needs about security.
              The code below allows to get all files on the computer running
              the demo. Add code in OnGetDocument, OnHeadDocument and
              OnPostDocument to check for authorized access to files.
Version:      1.01
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1999 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be><francois.piette@swing.be>

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


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit WebServ1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, StdCtrls, ExtCtrls, WinSock, WSocket, WSocketS, HttpSrv;

const
  CopyRight : String     = 'WebServ (c) 1999-2000 F. Piette V1.00 BETA ';

type
  { This component is used for client connection instead of default one.    }
  { This enables to add any data we need to handle our application.         }
  { As this data is located in client component, each connected client has  }
  { his own private data.                                                   }
  TMyHttpConnection = class(THttpConnection)
  protected
    FPostedDataBuffer : PChar;     { Will hold dynamically allocated buffer }
    FPostedDataSize   : Integer;   { Databuffer size                        }
    FDataLen          : Integer;   { Keep track of received byte count.     }
  public
    destructor  Destroy; override;
  end;

  { This is the main form for our application. Any data here is global for  }
  { all clients. Put provate data in TMyHttpConnection class (see above).   }
  TWebServForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    HttpServer1: THttpServer;
    Label1: TLabel;
    DocDirEdit: TEdit;
    Label2: TLabel;
    DefaultDocEdit: TEdit;
    StartButton: TButton;
    StopButton: TButton;
    Label3: TLabel;
    PortEdit: TEdit;
    ClientCountLabel: TLabel;
    Label5: TLabel;
    ClearButton: TButton;
    DisplayHeaderCheckBox: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HttpServer1GetDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure StartButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure HttpServer1ClientConnect(Sender: TObject;
      Client: TObject; Error: Word);
    procedure HttpServer1ClientDisconnect(Sender: TObject;
      Client: TObject; Error: Word);
    procedure HttpServer1ServerStarted(Sender: TObject);
    procedure HttpServer1ServerStopped(Sender: TObject);
    procedure HttpServer1HeadDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure HttpServer1PostedData(Sender: TObject;
      Client: TObject; Error: Word);
    procedure HttpServer1PostDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure ClearButtonClick(Sender: TObject);
  private
    FIniFileName   : String;
    FInitialized   : Boolean;
    FCountRequests : Integer;
    procedure CreateVirtualDocument_time_htm(Sender    : TObject;
                                    Client    : TObject;
                                    var Flags : THttpGetFlag);
    procedure DisplayHeader(Client : TMyHttpConnection);
    procedure ProcessPostedData_CgiFrm1(Client : TMyHttpConnection);
  public
    procedure Display(Msg : String);
    property  IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  WebServForm: TWebServForm;

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
    KeyPort            = 'Port';
    KeyDisplayHeader   = 'DisplayHeader';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
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


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.FormCreate(Sender: TObject);
begin
    { Create IniFileName based on EXE file name; }
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
    wsi     : TWSADATA;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        { Restore persistent data from INI file }
        IniFile      := TIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        DocDirEdit.Text     := IniFile.ReadString(SectionData, KeyDocDir,
                                                  'c:\WwwRoot');
        DefaultDocEdit.Text := IniFile.ReadString(SectionData, KeyDefaultDoc,
                                                  'index.html');
        PortEdit.Text       := IniFile.ReadString(SectionData, KeyPort,
                                                  '80');
        DisplayHeaderCheckBox.Checked :=
                Boolean(IniFile.ReadInteger(SectionData, KeyDisplayHeader, 0));
        IniFile.Destroy;
        { Initialize client count caption }
        ClientCountLabel.Caption := '0';
        { Display version info for program and use components }
        wsi := WinsockInfo;
        DisplayMemo.Clear;
        Display(CopyRight);
        Display('Using:');
        Display('   ' + WSocket.CopyRight);
        Display('   ' + WSocketS.CopyRight);
        Display('   ' + HttpSrv.CopyRight);
        Display('    Winsock:');
        Display('        Version ' +
                Format('%d.%d', [WinsockInfo.wHighVersion shr 8,
                                 WinsockInfo.wHighVersion and 15]));
        Display('        ' + StrPas(@wsi.szDescription));
        Display('        ' + StrPas(@wsi.szSystemStatus));
{$IFNDEF VER100}
        { A bug in Delphi 3 makes lpVendorInfo invalid }
        if wsi.lpVendorInfo <> nil then
            Display('        ' + StrPas(wsi.lpVendorInfo));
{$ENDIF}
        { Automatically start server }
        StartButtonClick(Self);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    { Save persistent data to INI file }
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.WriteString(SectionData,    KeyDocDir,      HttpServer1.DocDir);
    IniFile.WriteString(SectionData,    KeyDefaultDoc,  HttpServer1.DefaultDoc);
    IniFile.WriteString(SectionData,    KeyPort,        HttpServer1.Port);
    IniFile.WriteInteger(SectionData,   KeyDisplayHeader,
                                        ord(DisplayHeaderCheckBox.Checked));
    IniFile.Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Display a message in display memo box, making sure we don't overflow it.  }
procedure TWebServForm.Display(Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            { We preserve only 200 lines }
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        { Makes last line visible }
        {$IFNDEF VER80}
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
        {$ENDIF}
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
procedure TWebServForm.StartButtonClick(Sender: TObject);
begin
    HttpServer1.DocDir      := Trim(DocDirEdit.Text);
    HttpServer1.DefaultDoc  := Trim(DefaultDocEdit.Text);
    HttpServer1.Port        := Trim(PortEdit.Text);
    HttpServer1.ClientClass := TMyHttpConnection;
    HttpServer1.Start;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when user clicks on stop button. We just  }
{ stop the server. We will get OnServerStopped event triggered.             }
procedure TWebServForm.StopButtonClick(Sender: TObject);
begin
    HttpServer1.Stop;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when user clicks on clear buttoN; We just }
{ clear the memo used for displaying activity.                              }
procedure TWebServForm.ClearButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when HTTP server is started, that is when }
{ server socket has started listening.                                      }
procedure TWebServForm.HttpServer1ServerStarted(Sender: TObject);
begin
    DocDirEdit.Enabled     := FALSE;
    DefaultDocEdit.Enabled := FALSE;
    PortEdit.Enabled       := FALSE;
    StartButton.Enabled    := FALSE;
    StopButton.Enabled     := TRUE;
    Display('Server is waiting for connections');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when server has been stopped, that is     }
{ when server socket stop listening.                                        }
procedure TWebServForm.HttpServer1ServerStopped(Sender: TObject);
begin
    DocDirEdit.Enabled     := TRUE;
    DefaultDocEdit.Enabled := TRUE;
    PortEdit.Enabled       := TRUE;
    StartButton.Enabled    := TRUE;
    StopButton.Enabled     := FALSE;
    Display('Server stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when a new client has connected.          }
procedure TWebServForm.HttpServer1ClientConnect(
    Sender : TObject;               { HTTP server component                 }
    Client : TObject;               { Client connecting                     }
    Error  : Word);                 { Error in connection                   }
begin
    ClientCountLabel.Caption := IntToStr(HttpServer1.ClientCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when a client is disconnecting, just      }
{ before client component is closed.                                        }
procedure TWebServForm.HttpServer1ClientDisconnect(
    Sender : TObject;               { HTTP server component                 }
    Client : TObject;               { Client connecting                     }
    Error  : Word);                 { Error in disconnection                }
begin
    ClientCountLabel.Caption := IntToStr(HttpServer1.ClientCount - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when HTTP server component receive a HEAD }
{ command from any client.                                                  }
{ We just count the request, display a message and let HTTP server          }
{ component handle everything.                                              }
{ We should trap every URI we handle internally...                          }
procedure TWebServForm.HttpServer1HeadDocument(
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
procedure TWebServForm.HttpServer1GetDocument(
    Sender    : TObject;            { HTTP server component                 }
    Client    : TObject;            { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
begin
    { Count request and display a message }
    Inc(FCountRequests);
    Display(IntToStr(FCountRequests) +
            ': GET ' + TMyHttpConnection(Client).Path);
    DisplayHeader(TMyHttpConnection(Client));

    { Trap '/time.htm' path to dynamically generate an answer. }
    if CompareText(THttpConnection(Client).Path, '/time.htm') = 0 then
        CreateVirtualDocument_time_htm(Sender, Client, Flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /time.htm document                      }
procedure TWebServForm.CreateVirtualDocument_time_htm(
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
    Stream.Write(Header[1], Length(Header));
    Stream.Write(Body[1],   Length(Body));
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
procedure TWebServForm.HttpServer1PostDocument(
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
{$IFDEF VER80}
        if Remote.FPostedDataSize = 0 then begin
            Remote.FPostedDataSize := Remote.RequestContentLength + 1;
            GetMem(Remote.FPostedDataBuffer, Remote.FPostedDataSize);
        end
        else begin
            ReallocMem(Remote.FPostedDataBuffer, Remote.FPostedDataSize, Remote.RequestContentLength + 1);
            Remote.FPostedDataSize := Remote.RequestContentLength + 1;
        end;
{$ELSE}
        ReallocMem(Remote.FPostedDataBuffer, Remote.RequestContentLength + 1);
{$ENDIF}
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
procedure TWebServForm.HttpServer1PostedData(
    Sender : TObject;               { HTTP server component                 }
    Client : TObject;               { Client posting data                   }
    Error  : Word);                 { Error in data receiving               }
var
    Len     : Integer;
    Remains : Integer;
    Junk    : array [0..255] of char;
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
    Len := Remote.Receive(Remote.FPostedDataBuffer + Remote.FDataLen, Remains);
    { Sometimes, winsock doesn't wants to givve any data... }
    if Len <= 0 then
        Exit;

    { Add received length to our count }
    Inc(Remote.FDataLen, Len);
    { Add a nul terminating byte (handy to handle data as a string) }
    Remote.FPostedDataBuffer[Remote.FDataLen] := #0;
    { Display receive data so far }
    Display('Data: ''' + StrPas(Remote.FPostedDataBuffer) + '''');

    { When we received the whole thing, we can process it }
    if Remote.FDataLen = Remote.RequestContentLength then begin
        if CompareText(Remote.Path, '/cgi-bin/cgifrm1.exe') = 0 then
            ProcessPostedData_CgiFrm1(Remote)
        else
            Remote.Answer404;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will process posted data for CgiFrm1.exe                             }
procedure TWebServForm.ProcessPostedData_CgiFrm1(Client : TMyHttpConnection);
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
    Stream.Write(Buf[1], Length(Buf));
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
    Stream.Write(Header[1], Length(Header));
    Stream.Write(Body[1],   Length(Body));
    Stream.Seek(0, 0);
    { Ask HTTP server component to send data stream for us }
    Client.DocStream := Stream;
    Client.SendStream;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.DisplayHeader(Client : TMyHttpConnection);
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
{ We need to override parent class destructor because we have allocated     }
{ memory for our data buffer.                                               }
destructor TMyHttpConnection.Destroy;
begin
    if Assigned(FPostedDataBuffer) then begin
        FreeMem(FPostedDataBuffer, FPostedDataSize);
        FPostedDataBuffer := nil;
        FPostedDataSize   := 0;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

