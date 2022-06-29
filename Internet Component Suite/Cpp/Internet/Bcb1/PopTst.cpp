//---------------------------------------------------------------------------
// This is an outdated sample program which use an outdated POP3 component.
// For new programs, use new POP3 component in Pop3Prot.pas source file.
// New sample program is called MailRcv. The new component is not 100%
// compatible with the old one because the new is fully asynchronous.
// Name has been slightly changed to allow installation of both component
// if you need to support old code.
#include <vcl\vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("PopTst.res");
USEFORM("..\Poptst1.cpp", POP3ExcercizerForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TPOP3ExcercizerForm), &POP3ExcercizerForm);
        Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
