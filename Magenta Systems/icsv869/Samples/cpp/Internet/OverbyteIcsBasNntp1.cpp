/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Nov 06, 2005
Description:  Basic NNTP client program using ICS and demonstrating how to use
			  asynchronous, event-driven programming. This demo connect to a
			  news server, select a group and download the last 5 messages.
Version:      1.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2005 by François PIETTE
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


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
//$$---- Form CPP ----
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "OverbyteIcsBasNntp1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "OverbyteIcsNntpCli"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
TBasicNntpForm *BasicNntpForm;
//---------------------------------------------------------------------------
__fastcall TBasicNntpForm::TBasicNntpForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TBasicNntpForm::Display(const System::String Msg)
{
    DisplayMemo->Lines->Add(Msg);
}
//---------------------------------------------------------------------------
void __fastcall TBasicNntpForm::ExecButtonClick(TObject *Sender)
{
	DisplayMemo->Clear();
	NntpCli1->Host = "news.delphinaute.be";
	Display("Connecting to " + NntpCli1->Host);
	NntpCli1->Connect();
}
//---------------------------------------------------------------------------
void __fastcall TBasicNntpForm::NntpCli1SessionConnected(TObject *Sender, WORD ErrCode)
{
	if (ErrCode) {
		Display("Connect failed, error #" + IntToStr(ErrCode));
		return;
	}
	Display("Connected to server");
}
//---------------------------------------------------------------------------
void __fastcall TBasicNntpForm::NntpCli1SessionClosed(TObject *Sender, WORD ErrCode)
{
	Display("TCP session disconnected");
}
//---------------------------------------------------------------------------
void __fastcall TBasicNntpForm::NntpCli1RequestDone(TObject *Sender,
      TNntpRequest RqType, WORD ErrCode)
{
	if (ErrCode) {
		Display("Request done. RqType = " + IntToStr(RqType) +
                ". Error #" + IntToStr(ErrCode));
		return;
	}
	switch (RqType) {
	case nntpConnect :
		Display("Selecting group \"delphi\"");
		NntpCli1->Group("delphi");
		break;
	case nntpGroup :
		Display("ArticleFirst     = " + IntToStr(NntpCli1->ArticleFirst));
		Display("ArticleLast      = " + IntToStr(NntpCli1->ArticleLast));
		Display("Start downloading articles");
		FCurrentArticle = NntpCli1->ArticleLast;
		NntpCli1->ArticleByNumber(FCurrentArticle, NULL);
		break;
	case nntpArticleByNumber :
		FCurrentArticle--;
		if (FCurrentArticle < (NntpCli1->ArticleLast - 5)) {
			Display("Done with 5 articles, disconnecting");
			NntpCli1->Quit();
			return;
		}
		if (FCurrentArticle < NntpCli1->ArticleFirst) {
			Display("No more article, disconnecting");
			NntpCli1->Quit();
			return;
		}
		NntpCli1->ArticleByNumber(FCurrentArticle, NULL);
		break;
	case nntpQuit :
		Display("Disconnected from server");
		break;
	}
}
//---------------------------------------------------------------------------
void __fastcall TBasicNntpForm::NntpCli1MessageLine(TObject *Sender)
{
	// Display only subject line in this demo
	if (strnicmp(NntpCli1->LastResponse.c_str(), "subject:", 8) == 0)
		Display(NntpCli1->LastResponse);
}
//---------------------------------------------------------------------------

