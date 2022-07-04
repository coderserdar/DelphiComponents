//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("tnclient.res");
USEFORM("..\tncli1.cpp", TelnetForm);
USEFORMNS("..\..\..\Delphi\VC32\tnoptfrm.pas", Tnoptfrm, OptForm);
USEUNIT("..\..\..\Delphi\VC32\formpos.pas");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TTelnetForm), &TelnetForm);
        Application->CreateForm(__classid(TOptForm), &OptForm);
        Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
