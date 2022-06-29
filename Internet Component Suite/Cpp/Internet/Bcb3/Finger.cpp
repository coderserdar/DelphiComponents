//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("Finger.res");
USEFORM("..\finger1.cpp", FingerDemoForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TFingerDemoForm), &FingerDemoForm);
        Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
