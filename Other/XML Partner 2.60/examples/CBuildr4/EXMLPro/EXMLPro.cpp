//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("EXMLPro.res");
USEFORM("ExMain.cpp", MainForm);
USEFORM("ExAttr.cpp", AttributeForm);
USEFORM("ExCommnt.cpp", CommentForm);
USEFORM("ExURL.cpp", URLForm);
USEFORM("ExText.cpp", TextForm);
USEUNIT("ExUtil.cpp");
USEFORM("ExSelAtt.cpp", SelAttrsForm);
USEFORM("ExProcIn.cpp", PIForm);
USEFORM("exPrefs.cpp", PrefsForm);
USEFORM("exErr.cpp", frmErrors);
USEFORM("ExElemnt.cpp", ElementForm);
USEFORM("ExChildW.cpp", XMLChild);
USEFORM("ExView.cpp", frmPreview);
USEUNIT("ExProps.pas");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TMainForm), &MainForm);
                 Application->CreateForm(__classid(TSelAttrsForm), &SelAttrsForm);
                 Application->CreateForm(__classid(TCommentForm), &CommentForm);
                 Application->CreateForm(__classid(TTextForm), &TextForm);
                 Application->CreateForm(__classid(TfrmErrors), &frmErrors);
                 Application->CreateForm(__classid(TElementForm), &ElementForm);
                 Application->CreateForm(__classid(TfrmPreview), &frmPreview);
                 Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
