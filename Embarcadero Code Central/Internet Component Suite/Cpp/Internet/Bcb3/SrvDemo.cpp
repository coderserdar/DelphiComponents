//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("SrvDemo.res");
USEFORM("..\SrvDemo1.cpp", SrvForm);
USEFORM("..\SrvDemo2.cpp", CliForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TSrvForm), &SrvForm);
        Application->CreateForm(__classid(TCliForm), &CliForm);
        Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
