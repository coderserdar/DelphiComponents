//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("TnClient.res");
USEFORM("..\tncli1.cpp", TelnetForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TTelnetForm), &TelnetForm);
        Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
