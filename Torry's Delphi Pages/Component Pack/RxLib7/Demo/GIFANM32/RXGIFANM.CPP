//---------------------------------------------------------------------------
// Delphi VCL Extensions (RX) demo program
// Copyright (c) 1997, 1998 Master-Bank
//---------------------------------------------------------------------------
// RxGIFAnm.cpp
//
// RX GIF Animator
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORMNS("Gifmain.pas", Gifmain, AnimatorForm);
USEFORMNS("Gifpal.pas", Gifpal, PaletteForm);
USEFORMNS("Gifprvw.pas", Gifprvw, PreviewForm);
USEUNIT("About.pas");
USERES("Rxgifanm.res");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->Title = "RX GIF Animator";
                 Application->CreateForm(__classid(TAnimatorForm), &AnimatorForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
