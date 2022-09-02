//---------------------------------------------------------------------------

#ifndef uEmpCfgH
#define uEmpCfgH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "ffdb.hpp"
#include <Db.hpp>
#include <DBCtrls.hpp>
#include <DBGrids.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <ExtDlgs.hpp>
#include <Grids.hpp>
//---------------------------------------------------------------------------
class TfrmEmployees : public TForm
{
__published:	// IDE-managed Components
  TPanel *pnlBottom;
  TButton *btnClose;
  TButton *btnNew;
  TButton *btnDelete;
  TPanel *pnlBack;
  TSplitter *Splitter1;
  TDBGrid *grdEmp;
  TPanel *pnlPhotoTools;
  TButton *btnSelectPhoto;
  TButton *btnClearPhoto;
  TDBImage *dbiEmployee;
  TffTable *tblEmployees;
  TDataSource *dtsemployees;
  TOpenPictureDialog *dlgOpenPicture;
  void __fastcall btnDeleteClick(TObject *Sender);
  void __fastcall btnNewClick(TObject *Sender);
  void __fastcall btnClearPhotoClick(TObject *Sender);
  void __fastcall btnSelectPhotoClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
private:	// User declarations
  TDataSet* GetDataset();
public:		// User declarations
  __fastcall TfrmEmployees(TComponent* Owner);
  // property used to simplify access to the employee table
  __property TDataSet* Dataset = {read = GetDataset};
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmEmployees *frmEmployees;
//---------------------------------------------------------------------------
#endif
