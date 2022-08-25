//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("..\OverbyteIcsFtpTst1.cpp", FtpReceiveForm);
USEFORM("..\OverbyteIcsFtpTst2.cpp", DirectoryForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
         Application->Initialize();
         SetApplicationMainFormOnTaskBar(Application, true);
         Application->CreateForm(__classid(TFtpReceiveForm), &FtpReceiveForm);
         Application->CreateForm(__classid(TDirectoryForm), &DirectoryForm);
         Application->Run();
    }
    catch (Exception &exception)
    {
         Application->ShowException(&exception);
    }
    catch (...)
    {
         try
         {
             throw Exception("");
         }
         catch (Exception &exception)
         {
             Application->ShowException(&exception);
         }
    }
    return 0;
}
//---------------------------------------------------------------------------
