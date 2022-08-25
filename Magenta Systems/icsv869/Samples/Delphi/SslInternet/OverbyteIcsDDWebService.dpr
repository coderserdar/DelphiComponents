program OverbyteIcsDDWebService;

{
DD Service Application Framework Requirement
DDService is an enhanced Windows NT service application framework for Delphi and C++
Builder based on the original VCL service framework. In addition to it it also encapsulates
new Windows NT service APIs introduced since Windows 2000. Original author was the late
Arno Garrels of Duodata in Berlin. Now being maintained by Magenta Systems Ltd.

https://www.magsys.co.uk/delphi/ddservice.asp

DDService must be installed before opening this sample.  Once the sample is built, it
may be installed as a Windows service from an elevated command line prompt with the
command OverbyteIcsDDWebService.exe /install. But this sample is written to it may
also be run as a GUI for debugging.
}

uses
  Forms,
  SysUtils,
  DDSvcMgr,
  OverbyteIcsDDWebServiceCtl in 'OverbyteIcsDDWebServiceCtl.pas' {DDWebServiceCtl: TDDService},
  OverbyteIcsDDWebServiceSrv in 'OverbyteIcsDDWebServiceSrv.pas' {DDWebServiceSrv},
  OverbyteIcsSslMultiWebDataModule in 'OverbyteIcsSslMultiWebDataModule.pas' {SslMultiWebDataModule: TDataModule};

{$R *.RES}

function MyStartService: Boolean;
begin
    GInstalling := FindCmdLineSwitch('install', ['-','\','/'], True) or
                     FindCmdLineSwitch('uninstall', ['-','\','/'], True);
    if GInstalling then
    begin
        Result := True;
        Exit;
    end;
    GStartedByScm := FindCmdLineSwitch(GServiceIdent, ['-','\','/'], False);
    Result := GStartedByScm;
end;

begin
    if MyStartService then
    begin
        DDSvcMgr.Application.Initialize;
        Application.CreateForm(TDDWebServiceCtl, DDWebServiceCtl);
        if NOT GInstalling then begin // only installing windows service
            Forms.Application.CreateForm(TDDWebServiceSrv, DDWebServiceSrv);
            Forms.Application.CreateForm(TSslMultiWebDataModule, SslMultiWebDataModule);
        end;
        DDSvcMgr.Application.Run;
    end
    else
    begin
// windows GUI
        Forms.Application.Initialize;
        Forms.Application.Title := 'Overbyte DDService Web Server';
        Forms.Application.CreateForm(TDDWebServiceSrv, DDWebServiceSrv);
        Forms.Application.CreateForm(TSslMultiWebDataModule, SslMultiWebDataModule);
        Forms.Application.Run;
    end;

end.