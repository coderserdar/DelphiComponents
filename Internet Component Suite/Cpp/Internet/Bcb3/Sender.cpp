//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("Sender.res");
USEFORM("..\Sender1.cpp", SenderForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TSenderForm), &SenderForm);
        Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
