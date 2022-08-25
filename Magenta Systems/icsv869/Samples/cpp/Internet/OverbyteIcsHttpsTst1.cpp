/*---------------------------------------------------------------------------


Author:       François PIETTE
Creation:     November 23, 1997
Version:      1.04
Description:  Sample program to demonstrate some of the THttpCli features.
Email         francois.piette@overbyte.be      http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2006 by François PIETTE
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

Updates:
Jan 16, 1998  V1.00 Adapted for reviced HTTP component.
Mar 31, 1998  V1.01 Adapted for BCB3
Apr 13, 1998  V1.02 Call HttpCli1DocEnd when get failed to close the document
              file in he case it is already opened.
Aug 15, 1999  V1.03 Adapted for BCB4 (Moved FIniFileName initialization from
              FormCreate to form constructor).
Apr 02, 2000  V1.04 Adapted for BCB5    

  ---------------------------------------------------------------------------*/

#if __BORLANDC__ == 0x520     // BCB1 is BC5.20   BCB3 is BC5.30
    #define _WINSOCKAPI_      // Prevent winsock.h from being included
#endif
#include <vcl.h>
#include <inifiles.hpp>
#pragma hdrstop

#include "OverbyteIcsHttpsTst1.h"
//---------------------------------------------------------------------------
#pragma link "OverbyteIcsHttpProt"
#pragma link "OverbyteIcsWSocket"
#pragma link "OverbyteIcsLogger"
#pragma link "OverbyteIcsWndControl"
#pragma link "OverbyteIcsSslSessionCache"
#pragma resource "*.dfm"
#define HttpTstVersion  104
THttpTestForm *HttpTestForm;
//---------------------------------------------------------------------------
__fastcall THttpTestForm::THttpTestForm(TComponent* Owner)
    : TForm(Owner)
{
    // Build Ini file name
    FIniFileName = LowerCase(ChangeFileExt(Forms::Application->ExeName, ".ini"));
    FIniFileName = FIniFileName.SubString(1, FIniFileName.Length() - 3) + "ini";
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::FormCreate(TObject *Sender)
{
    FTrustedList = new TStringList;
    FClientCerts = NULL;
    FMyExternalSslSessionCache                 = new TSslAvlSessionCache(NULL);
    FMyExternalSslSessionCache->FlushInterval  = 3000;
    SslHttpCli1->CtrlSocket->OnBgException     = BackgroundException;
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::FormDestroy(TObject *Sender)
{
    if (FTrustedList != NULL) {
        delete FTrustedList;
        FTrustedList = NULL;
    }
    if (FClientCerts != NULL) {
        delete FClientCerts;
        FClientCerts = NULL;
    }
    if (FMyExternalSslSessionCache != NULL) {
        delete FMyExternalSslSessionCache;
        FMyExternalSslSessionCache = NULL;
    }
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::FormShow(TObject *Sender)
{
    TIniFile *IniFile;

    if (!Initialized) {
        Initialized         = TRUE;
        IniFile             = new TIniFile(FIniFileName);

        Width        = IniFile->ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       = IniFile->ReadInteger(SectionWindow, KeyHeight, Height);
        Top          = IniFile->ReadInteger(SectionWindow, KeyTop,
                                            (Screen->Height - Height) / 2);
        Left         = IniFile->ReadInteger(SectionWindow, KeyLeft,
                                            (Screen->Width  - Width)  / 2);
        URLEdit->Text         = IniFile->ReadString(SectionData, KeyUrl,
                                                   "https://localhost");
        DateTimeEdit->Text  = IniFile->ReadString("Data", "DateTime",  "");
        SocksServerEdit->Text = IniFile->ReadString(SectionData, KeySocksServer,
                                                   "");
        SocksPortEdit->Text   = IniFile->ReadString(SectionData, KeySocksPort,
                                                   "1080");
        ProxyHostEdit->Text   = IniFile->ReadString(SectionData, KeyProxyHost,
                                                   "");
        ProxyPortEdit->Text   = IniFile->ReadString(SectionData, KeyProxyPort,
                                                   "8080");
        DocEdit->Text         = IniFile->ReadString(SectionData, KeyDoc,
                                                   "/index.html");
        CertFileEdit->Text    = IniFile->ReadString(SectionData, KeyCertFile,
                                                   "01cert.pem");
        PrivKeyFileEdit->Text = IniFile->ReadString(SectionData, KeyPrivKeyFile,
                                                   "01key.pem");
        PassPhraseEdit->Text  = IniFile->ReadString(SectionData, KeyPassPhrase,
                                                   "password");
        CAFileEdit->Text      = IniFile->ReadString(SectionData, KeyCAFile,
                                                   "cacert.pem");
        CAPathEdit->Text      = IniFile->ReadString(SectionData, KeyCAPath,
                                                   "");
        SocksLevelComboBox->ItemIndex = IniFile->ReadInteger(SectionData, KeySocksLevel,
                                                   0);
        AcceptableHostsEdit->Text = IniFile->ReadString(SectionData, KeyAcceptableHosts,
                                                       "www.overbyte.be;www.borland.com");
        VerifyPeerCheckBox->Checked = IniFile->ReadBool(SectionData,
                                                        KeyVerifyPeer,
                                                        0);
        HttpVersionComboBox->ItemIndex = IniFile->ReadInteger(SectionData,
                                                              KeyHttpVer, 0);
        SessCacheCheckBox->Checked     = IniFile->ReadBool(SectionData,
                                                           KeySessCache, False);
        DebugEventCheckBox->Checked    = IniFile->ReadBool(SectionData, KeyDebugEvent,  False);
        DebugOutputCheckBox->Checked   = IniFile->ReadBool(SectionData, KeyDebugOutput, False);
        DebugFileCheckBox->Checked     = IniFile->ReadBool(SectionData, KeyDebugFile,   False);
        delete IniFile;
        DisplayMemo->Clear();
    }
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::FormClose(TObject *Sender, TCloseAction &Action)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(FIniFileName);
    IniFile->WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile->WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile->WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile->WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile->WriteString(SectionData,    KeyUrl,         URLEdit->Text);
    IniFile->WriteString(SectionData,    KeySocksServer, SocksServerEdit->Text);
    IniFile->WriteString(SectionData,    KeySocksPort,   SocksPortEdit->Text);
    IniFile->WriteString(SectionData,    KeyProxyHost,   ProxyHostEdit->Text);
    IniFile->WriteString(SectionData,    KeyProxyPort,   ProxyPortEdit->Text);
    IniFile->WriteBool(SectionData,      KeyVerifyPeer,  VerifyPeerCheckBox->Checked);
    IniFile->WriteString(SectionData,    KeyDoc,         DocEdit->Text);
    IniFile->WriteString(SectionData,    KeyCertFile,    CertFileEdit->Text);
    IniFile->WriteString(SectionData,    KeyPrivKeyFile, PrivKeyFileEdit->Text);
    IniFile->WriteString(SectionData,    KeyPassPhrase,  PassPhraseEdit->Text);
    IniFile->WriteString(SectionData,    KeyCAFile,      CAFileEdit->Text);
    IniFile->WriteString(SectionData,    KeyCAPath,      CAPathEdit->Text);
    IniFile->WriteString(SectionData,    KeyAcceptableHosts, AcceptableHostsEdit->Text);
    IniFile->WriteInteger(SectionData,   KeySocksLevel,  SocksLevelComboBox->ItemIndex);
    IniFile->WriteInteger(SectionData,   KeyHttpVer,     HttpVersionComboBox->ItemIndex);
    IniFile->WriteBool(SectionData,      KeySessCache,   SessCacheCheckBox->Checked);
    IniFile->WriteBool(SectionData,      KeyDebugEvent,  DebugEventCheckBox->Checked);
    IniFile->WriteBool(SectionData,      KeyDebugOutput, DebugOutputCheckBox->Checked);
    IniFile->WriteBool(SectionData,      KeyDebugFile,   DebugFileCheckBox->Checked);
    delete IniFile;
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::Display(const String Msg)
{
    DisplayMemo->Lines->Add(Msg);
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::PrepareConnection(void)
{
    AnsiString SocksLevelValues[3] = {"5", "4A", "4"};

    if (SocksServerEdit->Text > "") {
        SslHttpCli1->SocksServer = SocksServerEdit->Text;
        SslHttpCli1->SocksPort   = SocksPortEdit->Text;
        SslHttpCli1->SocksLevel  = SocksLevelValues[SocksLevelComboBox->ItemIndex];
    }
    else {
        SslHttpCli1->SocksServer = "";
        SslHttpCli1->SocksPort   = "";
        SslHttpCli1->SocksLevel  = "5";
    }

    if (ProxyHostEdit->Text > "" ) {
        SslHttpCli1->Proxy         = ProxyHostEdit->Text;
        SslHttpCli1->ProxyPort     = ProxyPortEdit->Text;
    }
    else {
        SslHttpCli1->Proxy         = ProxyHostEdit->Text;
        SslHttpCli1->ProxyPort     = ProxyPortEdit->Text;
    }
    SslHttpCli1->URL            = URLEdit->Text;
    SslHttpCli1->AcceptLanguage = "en, fr";
    SslHttpCli1->Connection     = "Keep-Alive";
    SslHttpCli1->RequestVer     = "1." + IntToStr(HttpVersionComboBox->ItemIndex);

    if (DateTimeEdit->Text != "")
        SslHttpCli1->ModifiedSince = StrToDateTime(DateTimeEdit->Text);
    else
        SslHttpCli1->ModifiedSince = 0;

    //SslHttpCli1.SetAcceptableHostsList(AcceptableHostsEdit.Text);

    SslContext1->SslCertFile         = CertFileEdit->Text;
    SslContext1->SslPassPhrase       = PassPhraseEdit->Text;
    SslContext1->SslPrivKeyFile      = PrivKeyFileEdit->Text;
    SslContext1->SslCAFile           = CAFileEdit->Text;
    SslContext1->SslCAPath           = CAPathEdit->Text;
    SslContext1->SslVerifyPeer       = VerifyPeerCheckBox->Checked;
    SslContext1->SslVersionMethod    = sslV23_CLIENT;

    IcsLogger1->LogOptions = TLogOptions();
    if (DebugEventCheckBox->Checked )
		IcsLogger1->LogOptions = IcsLogger1->LogOptions << loDestEvent;
    if (DebugOutputCheckBox->Checked )
		IcsLogger1->LogOptions = IcsLogger1->LogOptions << loDestOutDebug;
    if (DebugFileCheckBox->Checked ) {
        IcsLogger1->LogFileName   = "Debug_Out_HttpsTst.txt";
        IcsLogger1->LogFileOption = lfoOverwrite;
        IcsLogger1->LogOptions    = IcsLogger1->LogOptions << loDestFile;
    }
    if (IcsLogger1->LogOptions != TLogOptions())
        IcsLogger1->LogOptions = (IcsLogger1->LogOptions +
                                 LogAllOptInfo) << loAddStamp;
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::GetButtonClick(TObject *Sender)
{
    try {
        PrepareConnection();
        Display("Connecting...");
        GetButton->Enabled = FALSE;
        DocumentMemo->Clear();
        SslHttpCli1->GetASync();
    } catch (Exception& E) {
        Display("Connect error. " + E.ClassName() + ": " + E.Message);
        return;
    }
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::ClearButtonClick(TObject *Sender)
{
    DocumentMemo->Clear();
    DisplayMemo->Clear();
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::CloseButtonClick(TObject *Sender)
{
    SslHttpCli1->CloseAsync();
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::SslHttpCli1Command(TObject *Sender,
      String &S)
{
    Display("cmd> " + S);
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::SslHttpCli1Cookie(TObject *Sender,
      const String Data, bool &Accept)
{
    Display("cookie: \"" + Data + "\"");
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::SslHttpCli1DocBegin(TObject *Sender)
{
    TSslHttpCli *HttpCli;

    HttpCli = (TSslHttpCli*)Sender;
    Display(HttpCli->ContentType + " => " + HttpCli->DocName);
    Display("Document = " + HttpCli->DocName);

	FDocFileName = HttpCli->DocName;

	if (HttpCli->ContentType == "image/gif")
		ReplaceExt(FDocFileName, "gif");
    else if (HttpCli->ContentType == "image/jpeg")
		ReplaceExt(FDocFileName, "jpg");
    else if (HttpCli->ContentType == "image/bmp")
		ReplaceExt(FDocFileName, "bmp");

    if (FDocFileName == "")
        FDocFileName = "HttpTst.htm";
    try {
        HttpCli->RcvdStream = new TFileStream(FDocFileName, fmCreate);
    } catch (const Exception& E) {
        Display("Error opening file: " + E.Message);
        FDocFileName = "HttpTst.htm";
        Display("Using default file name: " + FDocFileName);
        HttpCli->RcvdStream = new TFileStream(FDocFileName, fmCreate);
    }
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::SslHttpCli1DocEnd(TObject *Sender)
{
    TSslHttpCli* HttpCli;

    HttpCli = (TSslHttpCli*)Sender;
    if (HttpCli->RcvdStream != NULL) {
        HttpCli->RcvdStream->Free();
        HttpCli->RcvdStream = NULL;
    }
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::SslHttpCli1LocationChange(TObject *Sender)
{
    TSslHttpCli* SslHttpCli;
    int          I;

    SslHttpCli = (TSslHttpCli*)Sender;
    for(I = 0; I < SslHttpCli->RcvdHeader->Count; I++)
        Display("hdr>" + SslHttpCli->RcvdHeader->Strings[I]);
    Display("Location changed to \"" + SslHttpCli->Location + "\"");
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::SslHttpCli1RequestDone(TObject *Sender,
	THttpRequest RqType, WORD ErrCode)
{
    TStream* DataIn;
    int      I;

    GetButton->Enabled = TRUE;
    if (ErrCode != 0) {
        Display("Request done, error #" + IntToStr(ErrCode));
        return;
    }

    Display("Request done, StatusCode #" + IntToStr(SslHttpCli1->StatusCode));

    for(I = 0; I < SslHttpCli1->RcvdHeader->Count; I++)
        Display("hdr>" + SslHttpCli1->RcvdHeader->Strings[I]);

    if (SslHttpCli1->DocName == "")
        DocumentMemo->Lines->Add("*** NO DOCUMENT FILE NAME ***");
    else {
        if (!FileExists(SslHttpCli1->DocName))
            DocumentMemo->Lines->Add("*** NO DOCUMENT FILE ***");
        else {
            DataIn = new TFileStream(SslHttpCli1->DocName, fmOpenRead);

            try {
                if (SslHttpCli1->ContentType.SubString(1, 5) == "text/")
                    DocumentMemo->Lines->LoadFromStream(DataIn);
                else {
                    DocumentMemo->Lines->Add("Content type is " +
                                             SslHttpCli1->ContentType);
                    DocumentMemo->Lines->Add("Document stored in \"" +
                                             SslHttpCli1->DocName +
                                             "\" Size=" +
                                             IntToStr(DataIn->Size));
                }
            }
            __finally {
                DataIn->Free();
            }
        }
    }
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::SslHttpCli1HeaderData(TObject *Sender)
{
    Display(SslHttpCli1->LastResponse);
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::SslHttpCli1SslCliNewSession(TObject *Sender,
      Pointer SslSession, bool WasReused, bool &IncRefCount)
{
    TSslHttpCli* HttpCli;

    // SslCliNewSession/SslCliGetSession allow external, client-side session
    // caching.
    if (!SessCacheCheckBox->Checked)
        return;
    HttpCli = (TSslHttpCli*)Sender;
    if (!WasReused) {
        FMyExternalSslSessionCache->CacheCliSession(SslSession,
                                      HttpCli->CtrlSocket->PeerAddr +
                                      HttpCli->CtrlSocket->PeerPort,
                                      IncRefCount);
        Display("! New SSL session");
    }
    else
        Display("! SSL Session reused");
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::SslHttpCli1SslCliGetSession(TObject *Sender,
      Pointer &SslSession, bool &FreeSession)
{
    TSslHttpCli* HttpCli;

    // SslCliNewSession/SslCliGetSession allow external, client-side session
    // caching.                                                              
    if (!SessCacheCheckBox->Checked)
        return;
    HttpCli = (TSslHttpCli*)Sender;
    SslSession  = FMyExternalSslSessionCache->GetCliSession(
                                      HttpCli->CtrlSocket->PeerAddr +
                                      HttpCli->CtrlSocket->PeerPort,
                                      FreeSession);
    FreeSession = TRUE;
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::SslHttpCli1SslVerifyPeer(TObject *Sender,
      int &Ok, TX509Base *Cert)
{
    // Alternate verification takes place in event HandshakeDone, we
    // accept anything temporarily.
    if (!Ok)
        Cert->CustomVerifyResult = X509_V_OK;
    Ok = 1;
    Display("Received certificate\r\n"
                            "Subject: \"" + Cert->SubjectOneLine + "\"\r\n"
                            "Issuer:  \"" + Cert->IssuerOneLine + "\"\r\n"
                            "Verify result: " + Cert->VerifyErrMsg);
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::WmSslNotTrusted(TMessage Message)
{
    SslHttpCli1->GetASync();
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::SslHttpCli1SslHandshakeDone(TObject *Sender,
      WORD ErrCode, TX509Base *PeerCert, bool &Disconnect)
{
     TX509List*   CertChain;
     AnsiString   Msg;
     int          I;
     AnsiString   IP;
     AnsiString   Hash;
	 TSslHttpCli* HttpCli;

    Display("Handshake done, error #" + IntToStr(ErrCode));
	HttpCli = (TSslHttpCli*)Sender;
	CertChain = HttpCli->CtrlSocket->SslCertChain;
    Display("! " + IntToStr(CertChain->Count) +
            " Certificate(s) in the verify chain.");

    // A simple custom verification that may be unsecure!
    // See also SslVerifyPeer above->

    if ((ErrCode != 0) || (HttpCli->CtrlSocket->SslSessionReused()) ||
       !HttpCli->SslContext->SslVerifyPeer)
        return;

    IP   = HttpCli->CtrlSocket->GetPeerAddr();
    Hash = PeerCert->Sha1Hex;

    if (HttpCli->SslAcceptableHosts->IndexOf(IP + Hash) > -1)
        return;

    HttpCli->Abort();

    if (CertChain->Count > 0) {
        Msg = "Certificates in chain:\r\n";
        for (I = 0; I < CertChain->Count; I++) {
            if (Msg.Length() > 0)
                Msg = Msg + "\r\n";
            Msg = Msg +  IntToStr(I + 1) + ")" +
                " SubjectCommonName: " + CertChain->Items[I]->SubjectCName + "\r\n" +
                " Last VerifyResult: " + CertChain->Items[I]->VerifyErrMsg + "\r\n";
        }
    }

    // Post connection check in case of SSL handshake went well
    // Checks whether argument HostOrIp matches dnsName or commonName
    // of a peer certificate, this is the string-argument of the function
    // For demo purposes a simple, constant string is passed->
    if (!PeerCert->PostConnectionCheck("www->overbyte->be")) {
        if (Msg.Length() > 0)
            Msg = Msg + "\r\n";
        if (MessageDlg(Msg + "Post connection check:\r\n" +
                      "The name specified in the peer certificate is\r\n" +
                      "invalid or does not match the site!\r\n\r\n" +
                      "Do you want to trust the connection anyway ?",
                       mtWarning, TMsgDlgButtons() << mbYes << mbNo, 0) != mrYes) {
            Disconnect = TRUE;
            Display("Post connection check failed, peer will be disconnected");
        }
        else {
            // Add peer cert to the trusted certs
            HttpCli->SslAcceptableHosts->Add(IP + Hash);
            PostMessage(Handle, WM_SSL_NOT_TRUSTED, 0, (int)Sender);
        }
    }
    else {
        if (MessageDlg(Msg + "\r\n" +
                      "Peer certificate was issued to the site.\r\n\r\n" +
                      "Do you want to trust the connection anyway ?",
                       mtWarning, TMsgDlgButtons() << mbYes << mbNo, 0) != mrYes) {
            Disconnect = TRUE;
            Display("Custom verification failed, peer will be disconnected");
        }
        else {
            // Add peer cert to the trusted certs
            HttpCli->SslAcceptableHosts->Add(IP + Hash);
            PostMessage(Handle, WM_SSL_NOT_TRUSTED, 0, Integer(Sender));
        }
    }
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::SslHttpCli1SslCliCertRequest(TObject *Sender,
      TX509Base *&Cert)
{
    TX509Base *X;

    // A very simple test of the SslCliCertRequest event.               }
    // This event is triggered only if CertFileEdit.Text is empty,      }
    // the server requested a certificate from the client,              }
    // and of course only in case of the SSL session wasn't reused.     }
    if (FClientCerts == NULL) {
        // Create a pool of client certs
        ClientCertDlg->CertListBox->Clear();
        FClientCerts = new TX509List(this, true);
        try {
            X = FClientCerts->Add();
            X->LoadFromPemFile("01cert.pem");
            X->PrivateKeyLoadFromPemFile("01key.pem", "password");
            ClientCertDlg->CertListBox->Items->Add(X->SubjectOneLine);
            X = FClientCerts->Add();
            X->LoadFromPemFile("client.pem", True, "password");
            ClientCertDlg->CertListBox->Items->Add(X->SubjectOneLine);
        } catch (const Exception& E) {
            FreeAndNil(FClientCerts);
            throw(E);
        }
    }
    ClientCertDlg->CertListBox->ItemIndex = 0;
    if (ClientCertDlg->ShowModal() == mrOk)
        Cert = FClientCerts->Items[ClientCertDlg->CertListBox->ItemIndex];
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::ButtonOSSLVersionClick(TObject *Sender)
{
    SslContext1->InitContext(); //Pre-loads OpenSSL DLL's
    Display(OpenSslVersion());
    Display(OpenSslCompilerFlags());
    Display(OpenSslBuiltOn());
    Display(OpenSslPlatForm());
    Display(OpenSslDir());
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::BackgroundException(
    TObject*   Sender,
    Exception* E,
    bool&      CanClose)
{
    Display("!" + E->ClassName() + ": " + E->Message);
    CanClose = TRUE;
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::IcsLogger1IcsLogEvent(TObject *Sender,
      TLogOption LogOption, const String Msg)
{
    Display(Msg);
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::HeadButtonClick(TObject *Sender)
{
    int I;

    DisplayMemo->Clear();
    DocumentMemo->Clear();
    SetButtonState(FALSE);

    PrepareConnection();
    SslHttpCli1->RcvdStream = NULL;

    try {
        SslHttpCli1->Head();
    } __except (TRUE) {
        SetButtonState(TRUE);
        Display("HEAD Failed !");
        Display("StatusCode   = " + IntToStr(SslHttpCli1->StatusCode));
        Display("ReasonPhrase = " + SslHttpCli1->ReasonPhrase);
        return;
    }

    Display("StatusCode = " + IntToStr(SslHttpCli1->StatusCode));

    for (I = 0; I < SslHttpCli1->RcvdHeader->Count; I++)
        Display("hdr>" + SslHttpCli1->RcvdHeader->Strings[I]);

    SetButtonState(TRUE);
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::SetButtonState(BOOL State)
{
    GetButton->Enabled   = State;
    HeadButton->Enabled  = State;
    AbortButton->Enabled = !State;
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::AbortButtonClick(TObject *Sender)
{
    SslHttpCli1->Abort();
}
//---------------------------------------------------------------------------

