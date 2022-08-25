#include <SysUtils.hpp>
#include <DDSvcMgr.hpp>
#pragma hdrstop
#pragma resource "SvcVistaAdminManifest.res"
#pragma resource "DDSvcEventlog.res"
#include <tchar.h>

USEFORM("uMain.cpp", DDService1); /* TDDService: File Type */
//---------------------------------------------------------------------------
#define Application Ddsvcmgr::Application
WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
 try
 {
   // Windows 2003 Server requires StartServiceCtrlDispatcher to be
   // called before CoRegisterClassObject, which can be called indirectly
   // by Application.Initialize. TServiceApplication->DelayInitialize allows
   // Application->Initialize to be called from TService->Main (after
   // StartServiceCtrlDispatcher has been called).
   //
   // Delayed initialization of the Application object may affect
   // events which then occur prior to initialization, such as
   // TService->OnCreate. It is only recommended if the ServiceApplication
   // registers a class object with OLE and is intended for use with
   // Windows 2003 Server.
   //
   // Application->DelayInitialize = true;
   //
   if ((!Application->DelayInitialize) || (Application->Installing()))
   {
     Application->Initialize();
   }
        Application->CreateForm(__classid(TDDService1), &DDService1);
		Application->Run();
 }
 catch (Exception &exception)
 {
   Sysutils::ShowException(&exception, System::ExceptAddr());
 }
       catch(...)
       {
   try
   {
           throw Exception("");
   }
   catch(Exception &exception)
   {
     Sysutils::ShowException(&exception, System::ExceptAddr());
   }
       }
 return 0;
}