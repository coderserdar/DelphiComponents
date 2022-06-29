//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("MailSnd.res");
USEFORM("..\MailSnd1.cpp", MailSndForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TMailSndForm), &MailSndForm);
                 Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
