{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Oct 30, 2005
Description:  Basic NNTP client program using ICS and demonstrating how to use
              asynchronous, event-driven programming. This demo connect to a
              news server, select a group and download the last 5 messages.
Version:      1.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2005-2010 by François PIETTE
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

History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsBasNntp1;

{$I Include\OverbyteIcsDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls,
{$IFDEF COMPILER12_UP}
  AnsiStrings,
{$ENDIF}
  OverbyteIcsNntpCli,
  OverbyteIcsWndControl;

type
  TBasicNntpForm = class(TForm)
    NntpCli1: TNntpCli;
    DisplayMemo: TMemo;
    Panel1: TPanel;
    ExecButton: TButton;
    procedure ExecButtonClick(Sender: TObject);
    procedure NntpCli1SessionConnected(Sender: TObject; ErrCode: Word);
    procedure NntpCli1RequestDone(Sender: TObject; RqType: TNntpRequest;
      ErrCode: Word);
    procedure NntpCli1MessageLine(Sender: TObject);
    procedure NntpCli1SessionClosed(Sender: TObject; ErrCode: Word);
  private
    FCurrentArticle : Integer;
    procedure Display(Msg: String);
  end;

var
  BasicNntpForm: TBasicNntpForm;

implementation

{$R *.dfm}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBasicNntpForm.Display(Msg : String);
begin
    DisplayMemo.Lines.Add(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBasicNntpForm.ExecButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
    NntpCli1.Host := 'news.online.de';//'news.delphinaute.be';
    Display('Connecting to ' + NntpCli1.Host);
    NntpCli1.Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBasicNntpForm.NntpCli1SessionConnected(Sender: TObject; ErrCode: Word);
begin
    if ErrCode <> 0 then begin
        Display('Connect failed, error #' + IntToStr(ErrCode));
        Exit;
    end;
    Display('Connected to server');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBasicNntpForm.NntpCli1SessionClosed(Sender: TObject; ErrCode: Word);
begin
    Display('TCP session disconnected');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBasicNntpForm.NntpCli1RequestDone(
    Sender  : TObject;
    RqType  : TNntpRequest;
    ErrCode : Word);
begin
    if ErrCode <> 0 then begin
        Display('Request done error #' + IntToStr(ErrCode));
        Exit;
    end;
    case RqType of
    nntpConnect :
        begin
            Display('Selecting group "delphi"');
            //NntpCli1.Group('delphi');
            NntpCli1.Group('de.comp.lang.delphi.misc');
        end;
    nntpGroup :
        begin
            Display('ArticleFirst     = ' + IntToStr(NntpCli1.ArticleFirst));
            Display('ArticleLast      = ' + IntToStr(NntpCli1.ArticleLast));
            Display('Start downloading articles');
            FCurrentArticle := NntpCli1.ArticleLast;
            NntpCli1.ArticleByNumber(FCurrentArticle, nil);
        end;
    nntpArticleByNumber :
        begin
            Dec(FCurrentArticle);
            if FCurrentArticle < (NntpCli1.ArticleLast - 5) then begin
                Display('Done with 5 articles, disconnecting');
                NntpCli1.Quit;
                Exit;
            end;
            if FCurrentArticle < NntpCli1.ArticleFirst then begin
                Display('No more article, disconnecting');
                NntpCli1.Quit;
                Exit;
            end;
            NntpCli1.ArticleByNumber(FCurrentArticle, nil);
        end;
    nntpQuit :
        begin
            Display('Disconnected from server');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBasicNntpForm.NntpCli1MessageLine(Sender: TObject);
begin
    // Display only subject line in this demo
    if SameText(Copy(NntpCli1.LastResponse, 1, 8), AnsiString('subject:')) then
        Display(String(NntpCli1.LastResponse));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
