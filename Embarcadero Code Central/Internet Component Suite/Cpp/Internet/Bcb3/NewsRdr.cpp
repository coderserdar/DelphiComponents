//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("NewsRdr.res");
USEFORM("..\newsrdr1.cpp", NNTPForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TNNTPForm), &NNTPForm);
        Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
