//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop
USERES("ExChtSrv.res");
USEFORM("ExSvMain.cpp", Form1);
USEUNIT("ExChtMsg.cpp");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TForm1), &Form1);
    Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
