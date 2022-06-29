//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("Recv.res");
USEFORM("..\Recv1.cpp", RecvForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TRecvForm), &RecvForm);
                 Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
