//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("..\OverbyteIcsMailRcv1.cpp", POP3ExcercizerForm);
USEFORM("..\OverbyteIcsMailRcv2.cpp", MessageForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
         Application->Initialize();
         SetApplicationMainFormOnTaskBar(Application, true);
         Application->CreateForm(__classid(TPOP3ExcercizerForm), &POP3ExcercizerForm);
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
