//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("SocksTst.res");
USEFORM("..\Socks1.cpp", SocksTestForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TSocksTestForm), &SocksTestForm);
        Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
