//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "fsclreng.hpp"
#include "fsdb.hpp"
#include "fsdbbase.hpp"
#include "fsllbase.hpp"
#include "fsllcomm.hpp"
#include "fsllcomp.hpp"
#include "fslleng.hpp"
#include "fslllgcy.hpp"
#include "fssrintm.hpp"
#include <Db.hpp>
#include <DBGrids.hpp>
#include <Grids.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TFSRemoteServer *FSRemoteServer1;
        TFSClient *FSClient1;
        TFSSession *FSSession1;
        TFSDatabase *FSDatabase1;
        TFSTable *FSTable1;
        TFSParamConnect *FSParamConnect1;
        TDataSource *DataSource1;
        TDBGrid *DBGrid1;
private:	// User declarations
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
