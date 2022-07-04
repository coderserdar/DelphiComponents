//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("UdpLstn.res");
USEFORM("..\udplstn1.cpp", MainForm);
USEUNIT("..\formpos.cpp");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TMainForm), &MainForm);
        Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
