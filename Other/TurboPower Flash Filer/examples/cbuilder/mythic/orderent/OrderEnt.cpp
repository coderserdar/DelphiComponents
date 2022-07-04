//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop
USERES("OrderEnt.res");
USEFORM("uMain.cpp", frmMain);
USEFORM("uOrdEnt.cpp", frmOrderEntry);
USEFORM("uCustEnt.cpp", frmCustomerEntry);
USEFORM("uEmpCfg.cpp", frmEmployees);
USEFORM("uItemSel.cpp", frmItemSelection);
USEFORM("dMythic.cpp", dtmMythic); /* TDataModule: DesignClass */
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
    Application->Initialize();
    Application->CreateForm(__classid(TfrmMain), &frmMain);
    Application->CreateForm(__classid(TdtmMythic), &dtmMythic);
    Application->Run();
  }
  catch (Exception &exception)
  {
    Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------
