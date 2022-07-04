//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("Project1.res");
USEFORMNS("Unit1.pas", Unit1, Form1);
USEFORMNS("CustPrev.pas", Custprev, fCustomPreview);
USEFORMNS("Unit2.pas", Unit2, Form2);
USEFORMNS("DM1.pas", Dm1, DataModule1); /* TDataModule: DesignClass */
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TDataModule1), &DataModule1);
                 Application->CreateForm(__classid(TForm1), &Form1);
                 Application->CreateForm(__classid(TfCustomPreview), &fCustomPreview);
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
