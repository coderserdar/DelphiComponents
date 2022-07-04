//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("HttpTst.res");
USEFORM("..\httptst1.cpp", HttpTestForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(THttpTestForm), &HttpTestForm);
        Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
