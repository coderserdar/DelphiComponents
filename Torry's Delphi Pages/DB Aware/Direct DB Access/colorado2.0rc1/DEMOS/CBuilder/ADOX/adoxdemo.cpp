//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("adoxdemo.res");
USEFORMNS("main.pas", Main, MainForm);
USEFORMNS("dm.pas", Dm, DataModule1);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TMainForm), &MainForm);
                 Application->CreateForm(__classid(TDataModule1), &DataModule1);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
