//$$---- EXE CPP ----
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("..\OverbyteIcsHttpsTst1.cpp", HttpTestForm);
USEFORM("..\OverbyteIcsCliCertDlg.cpp", ClientCertDlg);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(THttpTestForm), &HttpTestForm);
		Application->CreateForm(__classid(TClientCertDlg), &ClientCertDlg);
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
