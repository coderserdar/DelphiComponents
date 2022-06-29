//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("DllTst1.res");
USEFORM("..\DllTst1.cpp", DllTestForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TDllTestForm), &DllTestForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
