//---------------------------------------------------------------------------
#ifndef FtpTst2H
#define FtpTst2H
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
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
