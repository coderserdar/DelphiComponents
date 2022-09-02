//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop
USERES("ExChtClt.res");
USEFORM("ExClMain.cpp", frmCltMain);
USEUNIT("ExChtMsg.cpp");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
    Application->Initialize();
    Application->CreateForm(__classid(TfrmCltMain), &frmCltMain);
    Application->Run();
  }
  catch (Exception &exception)
  {
    Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------
