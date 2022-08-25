program StartProcSvc;

{$R 'SvcVistaAdminManifest.res' 'SvcVistaAdminManifest.rc'}

uses
  DDSvcMgr,
  StartProcMain in 'StartProcMain.pas' {DDService1: TDDService};

{$R *.RES}
{$R ..\..\Source\DDSvcEventlog.res}

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TDDService1, DDService1);
  Application.Run;
end.
