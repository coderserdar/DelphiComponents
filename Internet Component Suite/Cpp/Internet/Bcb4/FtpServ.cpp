//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("Ftpserv.res");
USEFORM("..\FtpSrv1.cpp", FtpServerForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TFtpServerForm), &FtpServerForm);
                 Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
