//---------------------------------------------------------------------------
#ifndef AboutH
#define AboutH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\ExtCtrls.hpp>
#include <vcl\Buttons.hpp>
#include <vcl\ShellApi.hpp>
#include "RXCtrls.hpp"
#include "RXConst.hpp"
//---------------------------------------------------------------------------
class TAboutForm : public TForm
{
__published:	// IDE-managed Components
        TSecretPanel *SecretPanel1;
        TImage *AppIcon;
        TRxLabel *WebLabel;
        TLabel *Label1;
        TLabel *Label2;
        TLabel *Label3;
        TBitBtn *OkBtn;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall AppIconDblClick(TObject *Sender);
        void __fastcall SecretPanel1DblClick(TObject *Sender);
        void __fastcall WebLabelClick(TObject *Sender);
        void __fastcall WebLabelActivate(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TAboutForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TAboutForm *AboutForm;
//---------------------------------------------------------------------------
#endif
