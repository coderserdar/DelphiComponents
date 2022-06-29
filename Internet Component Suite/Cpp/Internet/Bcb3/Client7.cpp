//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("Client7.res");
USEFORM("..\Cli7.cpp", Cli7Form);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TCli7Form), &Cli7Form);
        Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
