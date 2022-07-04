//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("TcpSrv.res");
USEFORM("..\TcpSrv1.cpp", TcpSrvForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TTcpSrvForm), &TcpSrvForm);
        Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
