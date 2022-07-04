// For non interactive services set TOCIDatabase.SilentMode to True
// In silent mode TOCIDatabase does not:
// - shows Wait Cursor
// - shows OCI Login Dialog
// - hooks exceptions and shows OCI Exception Dialog.
// So, You must setup TOCIDatabase.ConnectString to valid value
// before connecting...
// ---------------------------------------------------------------
// 1) Run first with parameter: '/INSTALL'
// 2) After that run Control Panel -> Services.
//    Find NCOCI service. Start it and Stop it.
// 3) To remove service run with parameter: '/UNINSTALL'

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
  Db, NCOci, NCOciWrapper, NCOciDB;

type
  TService1 = class(TService)
    OCIDatabase1: TOCIDatabase;
    OCIQuery1: TOCIQuery;
    OCIQuery2: TOCIQuery;
    procedure Service1Execute(Sender: TService);
  private
    { Private declarations }
  public
    function GetServiceController: {$IFDEF VER140} TServiceController {$ELSE} PServiceController {$ENDIF}; override;
    { Public declarations }
  end;

var
  Service1: TService1;

implementation

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  Service1.Controller(CtrlCode);
end;

function TService1.GetServiceController: {$IFDEF VER140} TServiceController {$ELSE} PServiceController {$ENDIF};
begin
  Result := @ServiceController;
end;

procedure TService1.Service1Execute(Sender: TService);
begin
    OCIDatabase1.SilentMode := True;
    OCIDatabase1.Open;
    OCIQuery2.ExecSQL;
    OCIQuery1.ExecSQL;
end;

end.
