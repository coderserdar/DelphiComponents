{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This sample program show how to use TNntpCli to write a news
              enabled application.
Creation:     December 24, 1997
Version:      1.01
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2011 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
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

Updates:
Dec 29, 1997 V0.91 Adapted to be compatible with Delphi 1
Jan 04, 1998 V0.92 Added LIST OVERVIEW.FMT, XOVER and DATE
Jan 31, 1998 V0.93 Added the UserEditBox (used for Post command)
                   Added code to get UserName and EMail from IE settings
Aug 14, 1999 V0.94 Added support for XHDR and MODE READER.
                   Corrected a bug that let Connect and Abort button
                   disabled when DNS lookup failed.
Jan 11, 2004 V1.00 Jumped to version 1.00.
                   Add ListMOTD button


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSslNewsRdr1;

{$IFNDEF USE_SSL}
  {$MESSAGE FATAL 'Define conditional define "USE_SSL" in the project options'};
{$ENDIF}
{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, OverbyteIcsNntpCli, StdCtrls, ExtCtrls, OverbyteIcsIniFiles,
  OverbyteIcsWSocket, OverbyteIcsWndControl;

const
  NewsRdrVersion     = 101;
  CopyRight : String = ' NewsRdr (c) 1997-2010 F. Piette V1.01 ';

type
  TNNTPForm = class(TForm)
    NntpCli1: TSslNntpCli;
    Panel1: TPanel;
    ServerEdit: TEdit;
    ConnectButton: TButton;
    Label1: TLabel;
    DisplayMemo: TMemo;
    AbortButton: TButton;
    GroupButton: TButton;
    GroupEdit: TEdit;
    ArticleNumEdit: TEdit;
    ArticleByNumberButton: TButton;
    ArticleByIDButton: TButton;
    NextButton: TButton;
    LastButton: TButton;
    HeadByNumberButton: TButton;
    HeadByIDButton: TButton;
    BodyByNumberButton: TButton;
    BodyByIDButton: TButton;
    StatByNumberButton: TButton;
    StatByIDButton: TButton;
    ListButton: TButton;
    ArticleIDEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PostButton: TButton;
    QuitButton: TButton;
    FileEdit: TEdit;
    Label5: TLabel;
    NewGroupsButton: TButton;
    NewNewsButton: TButton;
    HelpButton: TButton;
    XOverButton: TButton;
    OverViewFmtButton: TButton;
    DateButton: TButton;
    UserEdit: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    UserNameEdit: TEdit;
    Label8: TLabel;
    PasswordEdit: TEdit;
    AuthenticateButton: TButton;
    ModeReaderButton: TButton;
    XHdrButton: TButton;
    ListMotdButton: TButton;
    Label9: TLabel;
    PortEdit: TEdit;
    SslContext1: TSslContext;
    procedure ConnectButtonClick(Sender: TObject);
    procedure NntpCli1SessionConnected(Sender: TObject; Error: Word);
    procedure NntpCli1SessionClosed(Sender: TObject; Error: Word);
    procedure AbortButtonClick(Sender: TObject);
    procedure GroupButtonClick(Sender: TObject);
    procedure NntpCli1RequestDone(Sender: TObject; RqType: TNntpRequest; Error: Word);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ArticleByNumberButtonClick(Sender: TObject);
    procedure NntpCli1DataAvailable(Sender: TObject; Error: Word);
    procedure NntpCli1MessageLine(Sender: TObject);
    procedure NntpCli1MessageBegin(Sender: TObject);
    procedure NntpCli1MessageEnd(Sender: TObject);
    procedure ArticleByIDButtonClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure LastButtonClick(Sender: TObject);
    procedure HeadByIDButtonClick(Sender: TObject);
    procedure HeadByNumberButtonClick(Sender: TObject);
    procedure BodyByIDButtonClick(Sender: TObject);
    procedure BodyByNumberButtonClick(Sender: TObject);
    procedure StatByIDButtonClick(Sender: TObject);
    procedure StatByNumberButtonClick(Sender: TObject);
    procedure ListButtonClick(Sender: TObject);
    procedure PostButtonClick(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
    procedure NewGroupsButtonClick(Sender: TObject);
    procedure NewNewsButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure XOverButtonClick(Sender: TObject);
    procedure OverViewFmtButtonClick(Sender: TObject);
    procedure DateButtonClick(Sender: TObject);
    procedure AuthenticateButtonClick(Sender: TObject);
    procedure ModeReaderButtonClick(Sender: TObject);
    procedure XHdrButtonClick(Sender: TObject);
    procedure NntpCli1XHdrBegin(Sender: TObject);
    procedure NntpCli1XHdrEnd(Sender: TObject);
    procedure NntpCli1XHdrLine(Sender: TObject);
    procedure ListMotdButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FIniFileName  : String;
    FInitialized  : Boolean;
    FDataStream   : TStream;
    function  GetStream : TStream;
    procedure Display(Msg : String);
    procedure LineToStream(Buf : AnsiString);
  end;

var
  NNTPForm: TNNTPForm;

implementation

{$R *.DFM}

{$IFNDEF VER80}
uses
    Registry;
{$ENDIF}

const
    SectionWindow     = 'Window';
    KeyTop            = 'Top';
    KeyLeft           = 'Left';
    KeyWidth          = 'Width';
    KeyHeight         = 'Height';
    SectionData       = 'Data';
    KeyServer         = 'Server';
    KeyPort           = 'Port';
    KeyGroup          = 'Group';
    KeyArticleNum     = 'ArticleNum';
    KeyArticleID      = 'ArticleID';
    KeyFile           = 'File';
    KeyUser           = 'User';
    KeyUserName       = 'UserName';
    KeyPassword       = 'Password';



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
    I : Integer;
begin
    if Str[1] <> ' ' then
        Result := Str
    else begin
        I := 1;
        while (I <= Length(Str)) and (Str[I] = ' ') do
            I := I + 1;
        Result := Copy(Str, I, Length(Str) - I + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.FormCreate(Sender: TObject);
begin
{$IFDEF DELPHI10}
    // BDS2006 has built-in memory leak detection and display
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$ENDIF}
    FIniFileName := GetIcsIniFileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.FormShow(Sender: TObject);
var
    IniFile  : TIcsIniFile;
    EMail    : String;
    UserName : String;
{$IFNDEF VER80}
    Reg      : TRegistry;
    Key      : String;
{$ENDIF}
begin
    if FInitialized then
        Exit;
    FInitialized        := TRUE;

    EMail    := 'your.name@yourcompany.domain';
    UserName := 'Your Name';

{$IFNDEF VER80}
    { Get username and EMail from the Internet Explorer settings }
    { Should add code for Netscape Navigator...                  }
    Reg          := TRegistry.Create;
    Reg.RootKey  := HKEY_CURRENT_USER;
    Key          := '\Software\Microsoft\Internet Mail and News\Mail';
    if Reg.OpenKey(Key, FALSE) then begin
        EMail    := Reg.ReadString('Sender EMail');
        UserName := Reg.ReadString('Sender Name');
    end;
    Reg.CloseKey;
    Reg.Free;
{$ENDIF}

    IniFile             := TIcsIniFile.Create(FIniFileName);
    Top                 := IniFile.ReadInteger(SectionWindow, KeyTop,    Top);
    Left                := IniFile.ReadInteger(SectionWindow, KeyLeft,   Left);
    Width               := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
    Height              := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
    ServerEdit.Text     := IniFile.ReadString(SectionData, KeyServer,
                                              'forums.embarcadero.com');
    PortEdit.Text       := IniFile.ReadString(SectionData, KeyPort,    '563');
    ArticleNumEdit.Text := IniFile.ReadString(SectionData, KeyArticleNum, '');
    ArticleIDEdit.Text  := IniFile.ReadString(SectionData, KeyArticleID,  '');
    FileEdit.Text       := IniFile.ReadString(SectionData, KeyFile,
                                              'nntprdr.txt');
    UserNameEdit.Text   := IniFile.ReadString(SectionData, KeyUserName,   '');
    PasswordEdit.Text   := IniFile.ReadString(SectionData, KeyPassword,   '');
    UserEdit.Text       := IniFile.ReadString(SectionData, KeyUser,
                                              '"' + UserName + '" <' + EMail + '>');
    GroupEdit.Text      := IniFile.ReadString(SectionData, KeyGroup,
                         'embarcadero.public.delphi.thirdpartytools.general');
    IniFile.Free;
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteString(SectionData,    KeyServer,     ServerEdit.Text);
    IniFile.WriteString(SectionData,    KeyPort,       PortEdit.Text);
    IniFile.WriteString(SectionData,    KeyGroup,      GroupEdit.Text);
    IniFile.WriteString(SectionData,    KeyArticleNum, ArticleNumEdit.Text);
    IniFile.WriteString(SectionData,    KeyArticleID,  ArticleIDEdit.Text);
    IniFile.WriteString(SectionData,    KeyFile,       FileEdit.Text);
    IniFile.WriteString(SectionData,    KeyUser,       UserEdit.Text);
    IniFile.WriteString(SectionData,    KeyUserName,   UserNameEdit.Text);
    IniFile.WriteString(SectionData,    KeyPassword,   PasswordEdit.Text);
    IniFile.WriteInteger(SectionWindow, KeyTop,        Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,       Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,      Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,     Height);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.Display(Msg : String);
begin
    { Limit the memo to 100 lines }
    while DisplayMemo.Lines.Count > 100 do
         DisplayMemo.Lines.Delete(1);
    DisplayMemo.Lines.Add(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.NntpCli1SessionConnected(Sender: TObject; Error: Word);
begin
    AbortButton.Enabled := TRUE;
    Display('Connected, StatusCode = ' + IntToStr(NntpCli1.StatusCode));
    if NntpCli1.PostingPermited then
        Display('Posting permited')
    else
        Display('Posting not permited');
    Display(String(NntpCli1.LastResponse));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.NntpCli1SessionClosed(Sender: TObject; Error: Word);
begin
    AbortButton.Enabled   := FALSE;
    ConnectButton.Enabled := TRUE;
    Display('Connection closed');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called for each NNTP command when the command has   }
{ been exected (correctly or not).                                          }
procedure TNNTPForm.NntpCli1RequestDone(
    Sender: TObject;
    RqType: TNntpRequest;
    Error: Word);
begin
    Display(String('Request done. LastResponse = ' + NntpCli1.LastResponse));

    if Error = 0 then
        Display('No error')
    else
        Display('Error #' + IntToStr(Error));

    case RqType of
    nntpConnect:
        begin
            if Error <> 0 then begin
                AbortButton.Enabled   := FALSE;
                ConnectButton.Enabled := TRUE;
                Display('Connect failed');
            end;
        end;
    nntpGroup:
        begin
            Display('ArticleEstimated = ' + IntToStr(NntpCli1.ArticleEstimated));
            Display('ArticleFirst     = ' + IntToStr(NntpCli1.ArticleFirst));
            Display('ArticleLast      = ' + IntToStr(NntpCli1.ArticleLast));
            ArticleNumEdit.Text := IntToStr(NntpCli1.ArticleFirst);
        end;
    nntpPost, nntpQuit, nntpAbort, nntpHelp, nntpNewGroups, nntpNewNews,
    nntpXOver, nntpListOverViewFmt, nntpAuthenticate, nntpModeReader,
    nntpXHdr:
        begin
            { Nothing to do }
        end;
    nntpDate:
        begin
            Display('Server Date is ' + DateTimeToStr(NntpCli1.ServerDate));
        end;
    nntpStatByNumber,    nntpStatByID,
    nntpHeadByNumber,    nntpHeadByID,
    nntpBodyByNumber,    nntpBodyByID,
    nntpArticleByNumber, nntpArticleByID,
    nntpNext,            nntpLast:
        begin
            Display('ArticleNumber    = ' +
                                  IntToStr(NntpCli1.ArticleNumber));
            Display(String('ArticleID        = ' +
                                  '<' + NntpCli1.ArticleID + '>'));
            if Error = 0 then begin
                ArticleNumEdit.Text := IntToStr(NntpCli1.ArticleNumber);
                ArticleIDEdit.Text  := String(NntpCli1.ArticleID);
            end;
        end;
    else
        Display('Unknown request type.');
    end;

    { If any stream where used, destroy it }
    if Assigned(FDataStream) then begin
        FDataStream.Free;
        FDataStream := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called by TNntpCli when it has received data and    }
{ don't know what to do with it. It should normally not occur !             }
procedure TNNTPForm.NntpCli1DataAvailable(Sender: TObject; Error: Word);
begin
    Display(String('Data: ' + NntpCli1.LastResponse));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called by TNntpCli component just before the        }
{ component will begin receiving a message. It's a good place to open a     }
{ file or start a progress bar.                                             }
procedure TNNTPForm.NntpCli1MessageBegin(Sender: TObject);
begin
    Display('Message begin');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called by TNntpCli component for each line of an    }
{ incomming message. Header line as well as body lines are comming here.    }
{ It's a good place to write to a file or update screen or progress bar.    }
{ It's also the place to intercept header lines.                            }
procedure TNNTPForm.NntpCli1MessageLine(Sender: TObject);
var
    NewsGroupName   : AnsiString;
    LastArticle     : Integer;
    FirstArticle    : Integer;
    PostingFlag     : AnsiChar;
begin
    Display(String('Line: ' + NntpCli1.LastResponse));
    ParseListLine(NntpCli1.LastResponse,
                  NewsGroupName,
                  LastArticle,
                  FirstArticle,
                  PostingFlag);
    { It the place to do something with NewsGroupName, LastArticle, }
    { FirstArticle and PostingFlag                                  }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called by TNntpCli component when a message has     }
{ been received completely. It's a good place to close a file, delete the   }
{ progress bar and alert user.                                              }
procedure TNNTPForm.NntpCli1MessageEnd(Sender: TObject);
begin
    Display('Message End');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This function is called internally to create a TFileStream if any file    }
{ name is specified in the FileEdit box. If the edit box is blank, nil is   }
{ returned. The TFileStream will be supplyed to the comoponent for every    }
{ command which can take a TStream to store data such as ArticleByNum.      }
{ The stream is destroyed in the OnRequestDone event handler.               }
function TNNTPForm.GetStream : TStream;
begin
    { Delete the previous stream if not already done }
    if Assigned(FDataStream) then begin
        FDataStream.Free;
        FDataStream := nil;
    end;

    if Trim(FileEdit.Text) = '' then
        FDataStream := nil
    else begin
        { Try to open the file stream. Trap errors. }
        try
            FDataStream := TFileStream.Create(Trim(FileEdit.Text), fmCreate);
        except
            on E:Exception do begin
                { Display an error message in our TMemo }
                Display(E.Message);
                FDataStream := nil;
                raise;  { Show the exception box }
            end;
        end;
    end;
    Result := FDataStream;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.ConnectButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
    ConnectButton.Enabled := FALSE;
    NntpCli1.Host         := ServerEdit.Text;
    NntpCli1.Port         := PortEdit.Text;
    NntpCli1.Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.AbortButtonClick(Sender: TObject);
begin
    NntpCli1.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.QuitButtonClick(Sender: TObject);
begin
    NntpCli1.Quit;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.GroupButtonClick(Sender: TObject);
begin
    NntpCli1.Group(AnsiString(GroupEdit.Text));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.NextButtonClick(Sender: TObject);
begin
    NntpCli1.Next;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.LastButtonClick(Sender: TObject);
begin
    NntpCli1.Last;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.ArticleByIDButtonClick(Sender: TObject);
begin
    NntpCli1.ArticleByID(AnsiString(ArticleIDEdit.Text), GetStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.ArticleByNumberButtonClick(Sender: TObject);
begin
    NntpCli1.ArticleByNumber(StrToInt(ArticleNumEdit.Text), GetStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.HeadByIDButtonClick(Sender: TObject);
begin
    NntpCli1.HeadByID(AnsiString(ArticleIDEdit.Text), GetStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.HeadByNumberButtonClick(Sender: TObject);
begin
    NntpCli1.HeadByNumber(StrToInt(ArticleNumEdit.Text), GetStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.BodyByIDButtonClick(Sender: TObject);
begin
    NntpCli1.BodyByID(AnsiString(ArticleIDEdit.Text), GetStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.BodyByNumberButtonClick(Sender: TObject);
begin
    NntpCli1.BodyByNumber(StrToInt(ArticleNumEdit.Text), GetStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.StatByIDButtonClick(Sender: TObject);
begin
    NntpCli1.StatByID(AnsiString(ArticleIDEdit.Text));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.StatByNumberButtonClick(Sender: TObject);
begin
    NntpCli1.StatByNumber(StrToInt(ArticleNumEdit.Text));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.ListButtonClick(Sender: TObject);
begin
    if Application.MessageBox('This could take a VERY long time, proceed ? ',
                              'Warning', MB_YESNO) <> ID_YES then
        Exit;
    NntpCli1.List(GetStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.NewGroupsButtonClick(Sender: TObject);
begin
    NntpCli1.NewGroups(Now - 10, FALSE, '', GetStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.NewNewsButtonClick(Sender: TObject);
begin
    NntpCli1.NewNews(Now - 1, FALSE, AnsiString(GroupEdit.Text),
                     AnsiString(''), GetStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.HelpButtonClick(Sender: TObject);
begin
    NntpCli1.Help(GetStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpForm.LineToStream(Buf : AnsiString);
begin
    Display(String('Line: ' + Buf));
    Buf := Buf + #13#10;
    FDataStream.WriteBuffer(Buf[1], Length(Buf));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Posting a message require to build the message, including his header.     }
{ Here we use a TMemoryStream to create a message on the fly. Normally we   }
{ should use a TFileStream to get the message from a file where it has      }
{ been written by some user interface.                                      }
procedure TNNTPForm.PostButtonClick(Sender: TObject);
begin
    { Delete the stream if not already done }
    if Assigned(FDataStream) then begin
        FDataStream.Free;
        FDataStream := nil;
    end;

    { Create a new stream in memory }
    FDataStream := TMemoryStream.Create;

    { Write the message header }
    LineToStream(AnsiString('From: ' + UserEdit.Text));
    LineToStream(AnsiString('Newsgroups: ' + GroupEdit.Text));
    LineToStream('Subject: Internet Components Suite (ICS)');
    LineToStream('Organization: None');
    LineToStream('X-Newsreader: NNTP component ' +
                 '(http://www.overbyte.be)');

    { End of header is a blank line }
    LineToStream('');

    { Write the message body }
    LineToStream('');
    LineToStream('The Internet Component Suite is a set of native');
    LineToStream('components for Borland Delphi (all versions,');
    LineToStream('including 16 bits) and Borland C++ Builder. The');
    LineToStream('major TCP/IP protocols are supported for building');
    LineToStream('client/server, intranet or Internet applications.');
    LineToStream('');
    LineToStream('TCP, UDP, TELNET, FTP, SMTP, POP3, PING, FINGER, HTTP,');
    LineToStream('NNTP and more. Each component has samples writen');
    LineToStream('in Delphi and in C++ Builder. Several client/server');
    LineToStream('applications, including an event-driven and a');
    LineToStream('multi-threaded server, a complete FTP client and');
    LineToStream('TELNET client with ansi emulation are provided.');
    LineToStream('Full source code provided for everything.');
    LineToStream('');
    LineToStream('The Internet Component Suite is freeware, royalty');
    LineToStream('free and support is done using a mailing list.');
    LineToStream('Visit our website and download now from');
    LineToStream('http://www.overbyte.be');

    { Set stream pointer to beginning of stream because TNntpCli will post }
    { from the current position                                            }
    FDataStream.Seek(0, soFromBeginning	);

    { Ask the component to post the stream. The posting occurs in the      }
    { background ! We will receive the OnRequestDone event when done.      }
    { It's in this event handler that the stream must be destroyed         }
    NntpCli1.Post(FDataStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.XOverButtonClick(Sender: TObject);
begin
    NntpCli1.XOver(AnsiString(ArticleNumEdit.Text), GetStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.OverViewFmtButtonClick(Sender: TObject);
begin
    NntpCli1.ListOverViewFmt(GetStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.DateButtonClick(Sender: TObject);
begin
    NntpCli1.Date;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.AuthenticateButtonClick(Sender: TObject);
begin
    NntpCli1.UserName := UserNameEdit.Text;
    NntpCli1.Password := PasswordEdit.Text;
    NntpCli1.Authenticate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.ModeReaderButtonClick(Sender: TObject);
begin
    NntpCli1.ModeReader;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.XHdrButtonClick(Sender: TObject);
begin
    NntpCli1.XHdr(GetStream, 'subject', AnsiString(ArticleNumEdit.Text));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.NntpCli1XHdrBegin(Sender: TObject);
begin
    Display('XHdr begin');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.NntpCli1XHdrEnd(Sender: TObject);
begin
    Display('Xhdr End');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.NntpCli1XHdrLine(Sender: TObject);
begin
    Display('XHdr: ' + String(NntpCli1.LastResponse));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNNTPForm.ListMotdButtonClick(Sender: TObject);
begin
    NntpCli1.ListMotd(GetStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

