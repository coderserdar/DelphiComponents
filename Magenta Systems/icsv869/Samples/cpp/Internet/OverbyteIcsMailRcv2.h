//---------------------------------------------------------------------------
#ifndef OverbyteIcsMailRcv2H
#define OverbyteIcsMailRcv2H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TMessageForm : public TForm
{
__published:	// IDE-managed Components
    TMemo *DisplayMemo;
private:	// User declarations
public:		// User declarations
    __fastcall TMessageForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TMessageForm *MessageForm;
//---------------------------------------------------------------------------
#endif
