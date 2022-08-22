//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("TFormIdleSampleProject.res");
USEFORM("TFormIdleSampleUnit.cpp", Form1);
USEFORM("TFormIdleSampleUnit2.cpp", Form2);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TForm1), &Form1);
                 Application->CreateForm(__classid(TForm2), &Form2);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
