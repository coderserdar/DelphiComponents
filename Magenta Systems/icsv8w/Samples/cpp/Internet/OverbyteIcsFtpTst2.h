//---------------------------------------------------------------------------
#ifndef OverbyteIcsFtpTst2H
#define OverbyteIcsFtpTst2H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TDirectoryForm : public TForm
{
__published:	// IDE-managed Components
    TListBox *DirListBox;
private:	// User declarations
public:		// User declarations
    __fastcall TDirectoryForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TDirectoryForm *DirectoryForm;
//---------------------------------------------------------------------------
#endif
