//---------------------------------------------------------------------------
#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "ViewUpdate.h"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
    TTreeView *TreeView1;
    TLabel *Label1;
    TProgressBar *ProgressBar1;
    TBMTreeViewUpdate *BMTreeViewUpdate1;
    TRadioGroup *RadioGroup1;
    void __fastcall BMTreeViewUpdate1CustomCompare(TObject *Sender,
          TTreeNode *OldItem, TTreeNode *NewItem, bool &Changed);
    
    
    void __fastcall BMTreeViewUpdate1TreeInvalidate(TObject *Sender,
          TTreeNode *Item);
    void __fastcall BMTreeViewUpdate1TreeValidate(TObject *Sender,
          TTreeNode *Item);
    void __fastcall BMTreeViewUpdate1TreeUpdate(TObject *Sender,
          TTreeNode *OldItem, TTreeNode *NewItem);
    
    void __fastcall RadioGroup1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
    __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
