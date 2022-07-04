//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("Demo.res");
USEFORM("Main.cpp", FormMain);
USEOBJ("..\WinJson.obj");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TFormMain), &FormMain);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
