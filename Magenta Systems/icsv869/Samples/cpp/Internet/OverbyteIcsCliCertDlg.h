//---------------------------------------------------------------------------

#ifndef OverbyteIcsCliCertDlgH
#define OverbyteIcsCliCertDlgH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TClientCertDlg : public TForm
{
__published:	// IDE-managed Components
        TLabel *Label1;
        TListBox *CertListBox;
        TButton *OKButton;
        TButton *CancelButton;
private:	// User declarations
public:		// User declarations
        __fastcall TClientCertDlg(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TClientCertDlg *ClientCertDlg;
//---------------------------------------------------------------------------
#endif
