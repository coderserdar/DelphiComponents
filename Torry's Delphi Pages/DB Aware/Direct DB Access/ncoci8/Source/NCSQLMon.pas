{*******************************************************}
{File:      NCSQLMon.PAS                                }
{Revision:  0.01.04 / 17.04.2000                        }
{Comment:   NC OCI8 VCL: SQLMonitor client              }
{Copyright: portions - (c) 1999-2000, Dmitry Arefiev    }
{           most - (c) 1995,99 Inprise Corporation      }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{*******************************************************}
{$I NCOciDef.inc}
{CE_Desc_Include(HelpText\NCSQLMon.txt)}

unit NCSQLMon;

interface

Uses Windows, SysUtils, Classes;

type
    TNCTraceFlag = (tfQPrepare, tfQExecute, tfError, tfStmt, tfConnect,
        tfTransact, tfBlob, tfMisc, tfVendor, tfDataIn, tfDataOut);
    TNCTraceFlags = set of TNCTraceFlag;
    TNCSMAddStatementEvent = procedure (ASender: TObject; var AStmt: String;
        var Ok: Boolean) of object;

    TNCSQLMonitorClient = class(TComponent)
    private
        FSMClient: IUnknown;
        FSMLoadFailed: Boolean;
        FTraceFlags: TNCTraceFlags;
        FClientTitle: String;
        FClientObjName: String;
        FOnTraceFlagChange: TNotifyEvent;
        FOnAddStatement: TNCSMAddStatementEvent;
        FBeforeStop: TNotifyEvent;
        FBeforeStart: TNotifyEvent;
        FAfterStop: TNotifyEvent;
        FAfterStart: TNotifyEvent;
        FTraceFormat: String;
        procedure SMClientSignal(Sender: TObject; Data: Integer);
        procedure SetTraceFlags(Value: TNCTraceFlags);
        procedure LoadSMClient(DesignTime: Boolean);
        procedure UnloadSMClient;
        function GetActive: Boolean;
        function GetClientTitle: String;
        function IsCTS: Boolean;
        procedure SetActive(const Value: Boolean);
        procedure SetClientTitle(const Value: String);
        procedure SetClientObjName(const Value: String);
        function IsCONS: Boolean;
        function GetSMRunning: Boolean;
        function IsTFS: Boolean;
    public
        constructor Create(AOwner: TComponent); override;
        procedure AddStatement(AType: TNCTraceFlag; AStmt: String);
        property TraceFlags: TNCTraceFlags read FTraceFlags write SetTraceFlags;
        property SMRunning: Boolean read GetSMRunning;
    published
        property Active: Boolean read GetActive write SetActive default False;
        property ClientTitle: String read GetClientTitle write SetClientTitle
            stored IsCTS;
        property ClientObjName: String read FClientObjName write SetClientObjName
            stored IsCONS;
        property TraceFormat: String read FTraceFormat write FTraceFormat
            stored IsTFS;
        property OnTraceFlagChange: TNotifyEvent read FOnTraceFlagChange
            write FOnTraceFlagChange;
        property OnAddStatement: TNCSMAddStatementEvent read FOnAddStatement
            write FOnAddStatement;
        property BeforeStart: TNotifyEvent read FBeforeStart write FBeforeStart;
        property AfterStart: TNotifyEvent read FAfterStart write FAfterStart;
        property BeforeStop: TNotifyEvent read FBeforeStop write FBeforeStop;
        property AfterStop: TNotifyEvent read FAfterStop write FAfterStop;
    end;

implementation

Uses Forms, ActiveX;

const
    SDefCON: String = '<*>';
    SDefCT: String = 'NCOCI8';
    SStmtKinds: array[TNCTraceFlag] of String = ('Prepare', 'Execute', 'Error',
        'Stmt', 'Connect', 'Transact', 'Blob', 'Misc', 'Vendor', 'DataIn',
        'DataOut');
    SDefTF = 'SQL %s: NCOCI8 - %s';

// This moved to there, because SMIntf.pas is located in BDE package
const
    Class_SMClient: TGUID = '{CB9879E2-4395-11D0-9FFC-00A0248E4B9A}';
type
    ISMClient = interface(IUnknown)
    ['{CB9879E1-4395-11D0-9FFC-00A0248E4B9A}']
        function RegisterClient(ID: Integer; Name: PChar;
            Instance, SignalProc: Pointer): WordBool; stdcall;
        function AddStatement(Statement: PChar; Len: Integer): WordBool; stdcall;
    end;

constructor TNCSQLMonitorClient.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FClientObjname := SDefCON;
    FTraceFormat := SDefTF;
end;
    
function TNCSQLMonitorClient.GetSMRunning: Boolean;
var
    FM: THandle;
begin
    FM := OpenFileMapping(FILE_MAP_READ, False, 'SMBuffer');
    Result := FM <> 0;
    if Result then
        CloseHandle(FM);
end;
    
procedure TNCSQLMonitorClient.LoadSMClient(DesignTime: Boolean);
var
    ClientName: string;
begin
    if Assigned(FSMClient) or FSMLoadFailed then
        Exit;
    try
        if not DesignTime and not SMRunning then
            Exit;
        if Assigned(FBeforeStart) then
            FBeforeStart(Self);
        if not Assigned(FSMClient) then begin
            CoCreateInstance(Class_SMClient, nil, CLSCTX_INPROC_SERVER,
                             ISMClient, FSMClient);
        end;
        if Assigned(FSMClient) then begin
            ClientName := Format('%s.%s', [ClientTitle, ClientObjName]);
            FSMLoadFailed := not (FSMClient as ISMClient).RegisterClient(Random(MAXINT),
                PChar(ClientName), Self, @TNCSQLMonitorClient.SMClientSignal);
            if FSMLoadFailed then
                FSMClient := nil;
        end;
        if not FSMLoadFailed and Assigned(FAfterStart) then
            FAfterStart(Self);
    except
        FSMLoadFailed := True;
        raise;
    end;
end;

procedure TNCSQLMonitorClient.UnloadSMClient;
begin
    if Assigned(FSMClient) then
    try
        if Assigned(FBeforeStop) then
            FBeforeStop(Self);
        FSMClient := nil;
        if Assigned(FAfterStop) then
            FAfterStop(Self);
    except
        raise;
    end;
end;

procedure TNCSQLMonitorClient.SetTraceFlags(Value: TNCTraceFlags);
begin
    FTraceFlags := Value;
    if Assigned(FOnTraceFlagChange) then
        FOnTraceFlagChange(Self);
end;

procedure TNCSQLMonitorClient.SMClientSignal(Sender: TObject; Data: Integer);
begin
    TraceFlags := TNCTraceFlags(Word(Data));
end;

procedure TNCSQLMonitorClient.AddStatement(AType: TNCTraceFlag; AStmt: String);
var
    ok: Boolean;
    s: String;
begin
    if (AType in TraceFlags) and Active then begin
        ok := True;
        if Assigned(FOnAddStatement) then
            FOnAddStatement(Self, AStmt, ok);
        try
            if ok then begin
                s := Format(TraceFormat, [SStmtKinds[AType], AStmt]);
                (FSMClient as ISMClient).AddStatement(PChar(s), Length(s));
            end;
        except
            TraceFlags := [];
            raise;
        end;
    end;
end;

function TNCSQLMonitorClient.GetActive: Boolean;
begin
    Result := Assigned(FSMClient) and not FSMLoadFailed;
end;

procedure TNCSQLMonitorClient.SetActive(const Value: Boolean);
begin
    if Active <> Value then
        if Value then
            LoadSMClient(csDesigning in ComponentState)
        else
            UnloadSMClient;
end;

function TNCSQLMonitorClient.GetClientTitle: String;
begin
    Result := FClientTitle;
    if Result = '' then
        Result := Application.Title;
    if Result = '' then
        Result := SDefCT;
end;

procedure TNCSQLMonitorClient.SetClientTitle(const Value: String);
var
    prevActive: Boolean;
begin
    if ClientTitle <> Value then begin
        prevActive := Active;
        FClientTitle := Value;
        Active := False;
        Active := prevActive;
    end;
end;

function TNCSQLMonitorClient.IsCTS: Boolean;
begin
    Result := (FClientTitle <> '') and (FClientTitle <> Application.Title);
end;

procedure TNCSQLMonitorClient.SetClientObjName(const Value: String);
var
    prevActive: Boolean;
begin
    if ClientObjName <> Value then begin
        prevActive := Active;
        FClientObjName := Value;
        Active := False;
        Active := prevActive;
    end;
end;

function TNCSQLMonitorClient.IsCONS: Boolean;
begin
    Result := (FClientObjName <> '') and (FClientObjName <> SDefCON);
end;

function TNCSQLMonitorClient.IsTFS: Boolean;
begin
    Result := (FTraceFormat <> '') and (FTraceFormat <> SDefTF);
end;

var
    NeedToUninitialize: Boolean;
    tmp: LongInt;

initialization

    Randomize;
    if (CoRegisterClassObject(GUID_NULL, nil, 0, 0, tmp) = CO_E_NOTINITIALIZED) then
        NeedToUninitialize := Succeeded(CoInitialize(nil))
    else
        NeedToUninitialize := False;

finalization

    if NeedToUninitialize then
      CoUninitialize;

end.

