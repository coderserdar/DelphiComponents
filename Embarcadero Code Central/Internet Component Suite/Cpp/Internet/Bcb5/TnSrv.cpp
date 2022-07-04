//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("TnSrv.res");
USEFORM("..\tnsrv1.cpp", ServerForm);
USEFORM("..\tnsrv2.cpp", ClientForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TServerForm), &ServerForm);
                 Application->CreateForm(__classid(TClientForm), &ClientForm);
                 Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
