//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("NsLookup.res");
USEFORM("..\NsLook1.cpp", NsLookupForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TNsLookupForm), &NsLookupForm);
                 Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
