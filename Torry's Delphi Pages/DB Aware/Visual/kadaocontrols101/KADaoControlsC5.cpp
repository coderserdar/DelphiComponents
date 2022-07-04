//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEUNIT("KADaoCVFButton.pas");
USEUNIT("KADaoDateTime.pas");
USEUNIT("KADaoDBGrid.pas");
USERES("KADaoDBGrid.dcr");
USEUNIT("KADaoDBColumnCombo.pas");
USEUNIT("KADaoDBColumnListBox.pas");
USEUNIT("KADaoDBColumnCheckListBox.pas");
USEUNIT("KADaoDbGUIDEdit.pas");
USEUNIT("KADaoDBStringList.pas");
USEUNIT("KADaoExportButton.pas");
USEUNIT("KADaoFilterByButton.pas");
USEUNIT("KADaoFindButton.pas");
USEUNIT("KADaoSeekButton.pas");
USEUNIT("KADaoIndexesCombo.pas");
USEUNIT("KADaoIndexesListBox.pas");
USEUNIT("KADaoSearch.pas");
USEUNIT("KADaoIndexesCheckListBox.pas");
USEUNIT("KADaoTablesListBox.pas");
USEUNIT("KADaoTablesCombo.pas");
USEUNIT("KADaoTablesCheckListBox.pas");
USEUNIT("KAHRollForm.pas");
USEPACKAGE("Vcl50.bpi");
USEPACKAGE("VCLDB50.bpi");
USEPACKAGE("vclbde50.bpi");
USEPACKAGE("Vclx50.bpi");
USEUNIT("KADaoSelectIndexButton.pas");
USEPACKAGE("KADao.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
 
