//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("FtpTst.res");
USEFORM("..\ftptst1.cpp", FtpReceiveForm);
USEFORM("..\Ftptst2.cpp", DirectoryForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TFtpReceiveForm), &FtpReceiveForm);
        Application->CreateForm(__classid(TDirectoryForm), &DirectoryForm);
        Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
