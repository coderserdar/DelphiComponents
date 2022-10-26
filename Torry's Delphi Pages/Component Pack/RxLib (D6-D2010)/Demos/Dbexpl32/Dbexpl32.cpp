//---------------------------------------------------------------------------
// Delphi VCL Extensions (RX) demo program
// Copyright (c) 1997, 1998 Master-Bank
//---------------------------------------------------------------------------
// DBExpl32.cpp
//
// Database Explorer
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#include "dbexcpt.hpp"
#pragma hdrstop
#pragma link "DbExcpt"
//---------------------------------------------------------------------------
USERES("Dbexpl32.res");
USEFORMNS("Main.pas", Main, DBExplorerMainForm);
USEFORMNS("About.pas", About, AboutDlg);
USEFORMNS("BdeProp.pas", Bdeprop, BdePropertyDlg);
USEFORMNS("Childwin.pas", Childwin, MDIChild);
USEFORMNS("Dbcbrest.pas", Dbcbrest, RestructureDialog);
USEFORMNS("Desttab.pas", Desttab, DestTableDlg);
USEFORMNS("Editpict.pas", Editpict, PictEditDlg);
USEFORMNS("Editstr.pas", Editstr, StrEditDlg);
USEFORMNS("Filtdlg.pas", Filtdlg, FilterDialog);
USEFORMNS("Opendlg.pas", Opendlg, OpenDatabaseDlg);
USEFORMNS("Optdlg.pas", Optdlg, OptionsDialog);
USEFORMNS("RenDlg.pas", Rendlg, RenameDialog);
USEFORMNS("Sqlmon.pas", Sqlmon, TraceSQL);
USEFORMNS("Srctab.pas", Srctab, SrcTableDlg);
USEFORMNS("Userhelp.pas", Userhelp, CustomizeHelpDlg);
USEFORMNS("Viewblob.pas", Viewblob, BlobViewDlg);
USEUNIT("Options.pas");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->Title = "Database Explorer";
                 DbErrorIntercept();
                 Application->CreateForm(__classid(TDBExplorerMainForm), &DBExplorerMainForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
