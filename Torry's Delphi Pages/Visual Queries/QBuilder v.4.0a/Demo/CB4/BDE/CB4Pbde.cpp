//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("CB4Pbde.res");
USEFORM("CB4Ubde.cpp", QBDemoForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TQBDemoForm), &QBDemoForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
