//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("Clidemo.res");
USEFORM("..\CliDemo1.cpp", ClientForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
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
