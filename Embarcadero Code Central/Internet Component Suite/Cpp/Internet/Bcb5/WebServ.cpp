//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("WebServ.res");
USEFORM("..\WebServ1.cpp", AppBaseForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TAppBaseForm), &AppBaseForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
