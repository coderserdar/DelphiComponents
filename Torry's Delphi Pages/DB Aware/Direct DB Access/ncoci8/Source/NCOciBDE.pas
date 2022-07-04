{*******************************************************}
{File:      NCOciBDE.PAS                                }
{Revision:  0.01.00 / 4.11.2001                         }
{Comment:   NC OCI8 VCL: BDE compatibility unit         }
{Copyright: (c) 1999-2001, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{*******************************************************}
{$I NCOciDef.inc}

unit NCOciBDE;

interface

Uses BDE, Windows, SysUtils, Classes, DB, DBTables, NCOci, NCOciWrapper,
     NCOciUtil, NCOciDB;

type
    TOCIBDEDatabase = class(TOCICustomDatabase)
    private
        FBDEDatabase: TDatabase;
        procedure SetBDEDatabase(AValue: TDatabase);
    protected
        function InterceptHandles(var hEnv, hSrvc, hSrv, hSes,
            hErr, hTX: pOCIHandle): Boolean; override;
        procedure ReleaseHandles; override;
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    published
        property BDEDatabase: TDatabase read FBDEDatabase write SetBDEDatabase;
        
        property CachedUpdates;
        property DatabaseName;
        property NonBlockingMode;

        property Connected;
        property KeepConnection;

        property AutoCommit;
        property TransIsolation;
        property TransactionManager;
        property TransactionManagersSerialize;

        property DefaultFetchParams;
        property DefaultDataFormat;

        property SilentMode;
        property WaitCursor;
        property ShowWaitForm;

        property SQLMonitor;

        property MaxCursors;

        property BeforeConnect;
        property AfterConnect;
        property BeforeDisConnect;
        property AfterDisConnect;
        property BeforeStartTransaction;
        property AfterStartTransaction;
        property BeforeCommit;
        property AfterCommit;
        property BeforeRollback;
        property AfterRollback;
        property OnYield;
        property BeforeTMConnect;
        property AfterTMConnect;

        property OnError;
        property OnWarning;
    end;

implementation

Uses NCOciMsg;

var
  FCurHackSvc: TOCIHack;
  FCurHackEnv: TOCIHack;
  FCatched_hEnv: pOCIHandle;
  FCatched_hSvc: pOCIHandle;
  FCatched_hSrv: pOCIHandle;
  FCatched_hSes: pOCIHandle;
  FCatched_hErr: pOCIHandle;
  FCatched_hTX: pOCIHandle;

function OCIHandleAllocSpy(parenth: pOCIHandle; var hndlpp: pOCIHandle; atype: ub4;
  xtramem_sz: size_T; usrmempp: PPointer): sword; cdecl;
begin
    FCurHackSvc.UnHack;
    try
        Result := NCOci.OCIHandleAlloc(parenth, hndlpp, atype, xtramem_sz, usrmempp);
        if atype = OCI_HTYPE_SVCCTX then
            FCatched_hSvc := hndlpp
        else if atype = OCI_HTYPE_SERVER then
            FCatched_hSrv := hndlpp
        else if atype = OCI_HTYPE_SESSION then
            FCatched_hSes := hndlpp
        else if atype = OCI_HTYPE_ERROR then
            FCatched_hErr := hndlpp
        else if atype = OCI_HTYPE_TRANS then
            FCatched_hTX := hndlpp;
    finally
        FCurHackSvc.Hack;
    end;
end;

function OCIEnvInitSpy(var envhpp: pOCIEnv; mode: ub4; xtramemsz: size_T;
  usrmempp: PPointer): sword; cdecl;
begin
    FCurHackEnv.UnHack;
    try
        Result := NCOci.OCIEnvInit(envhpp, mode, xtramemsz, usrmempp);
        FCatched_hEnv := envhpp;
    finally
        FCurHackEnv.Hack;
    end;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
// TOCIBDEDatabase

function TOCIBDEDatabase.InterceptHandles(var hEnv, hSrvc, hSrv, hSes,
    hErr, hTX: pOCIHandle): Boolean;
var
  oHackSvc: TOCIHack;
  oHackEnv: TOCIHack;
begin
    if FBDEDatabase.Connected then
        OCIDBError(msgOCIBDEMBInActive, Self);
    NCOci.FOCIBDECompatibility := True;
    NCOci.InitOCI;
    oHackEnv := TOCIHack.Create(@NCOci.OCIEnvInit, @OCIEnvInitSpy);
    try
        oHackSvc := TOCIHack.Create(@NCOci.OCIHandleAlloc, @OCIHandleAllocSpy);
        try
            oHackSvc.Hack;
            oHackEnv.Hack;
            FCurHackSvc := oHackSvc;
            FCurHackEnv := oHackEnv;
            FCatched_hEnv := nil;
            FCatched_hSvc := nil;
            FCatched_hSrv := nil;
            FCatched_hSes := nil;
            FCatched_hErr := nil;
            FCatched_hTX := nil;
            FBDEDatabase.Connected := True;
        finally
            oHackSvc.Free;
        end;
    finally
        oHackEnv.Free;
    end;
    hEnv := FCatched_hEnv;
    hSrvc := FCatched_hSvc;
    hSrv := FCatched_hSrv;
    hSes := FCatched_hSes;
    hErr := FCatched_hErr;
    hTX := FCatched_hTX;
    Result := (hEnv <> nil) and (hSrvc <> nil) and (hSrv <> nil) and
        (hSes <> nil) and (hErr <> nil) and (hTX <> nil);
end;

procedure TOCIBDEDatabase.ReleaseHandles;
begin
    if FBDEDatabase <> nil then
        FBDEDatabase.Connected := False;
end;

procedure TOCIBDEDatabase.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then
        if AComponent = FBDEDatabase then begin
            FBDEDatabase := nil;
            Connected := False;
        end;
end;

procedure TOCIBDEDatabase.SetBDEDatabase(AValue: TDatabase);
begin
    Connected := False;
    if FBDEDatabase <> AValue then begin
        FBDEDatabase := AValue;
        if FBDEDatabase <> nil then
            FBDEDatabase.FreeNotification(Self);
    end;
end;

end.
