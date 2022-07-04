/*********************************************************/
/* XMLPartner: ExURL.CPP 2.55                            */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor URL Edit form                  */
/*********************************************************/
//---------------------------------------------------------------------------
#ifndef ExURLH
#define ExURLH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TURLForm : public TForm
{
__published:	// IDE-managed Components
    TLabel *Label1;
    TLabel *Label2;
    TLabel *Label3;
    TLabel *Label4;
    TEdit *UrlEdit;
    TEdit *FtpUserIdEdit;
    TEdit *FtpPasswordEdit;
    TButton *OkBtn;
    TButton *CancelBtn;
    void __fastcall OkBtnClick(TObject *Sender);

    void __fastcall FormShow(TObject *Sender);
private:	// User declarations
    String __fastcall GetFTPPassword(void);
    String __fastcall GetFTPUser(void);
    String __fastcall GetURL(void);

    void __fastcall SetFTPPassword(const String aPassword);
    void __fastcall SetFTPUser(const String aUser);
    void __fastcall SetURL(const String aURL);
public:		// User declarations
    __fastcall TURLForm(TComponent* Owner);

    __property String FTPPassword = {read=GetFTPPassword, write=SetFTPPassword, nodefault};
    __property String FTPUser = {read=GetFTPUser, write=SetFTPUser, nodefault};
    __property String URL = {read=GetURL, write=SetURL, nodefault};
};
//---------------------------------------------------------------------------
extern PACKAGE TURLForm *URLForm;
//---------------------------------------------------------------------------
#endif
