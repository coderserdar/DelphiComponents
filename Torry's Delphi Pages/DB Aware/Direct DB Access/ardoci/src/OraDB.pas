unit OraDB;

{$INCLUDE dOCI.inc}

{
  TOraDB - component to connect to Oracle
  All other components (such as TOraSQL, TAOraSQL) use this component to get access to Oracle server

  You can get more information about TOraDB in documentation.

   procedure Open; - establishes connect to server
   procedure Close; - closes connect
   procedure StartTransaction; - sets InTransaction=True and sets transaction isolation level. no OCI calls performed.
   procedure CommitTransaction; - calls before and after events and executes OCITransCommit.
   procedure RollbackTransaction; - calls OnRollback event and executes OCITransRollback.
   procedure Break; // executes OCIBreak
   property  InTransaction:boolean - determines is transaction in process.
}


interface

uses
  Db, Classes, Windows, OraDefines, ADataSet, dOCIMessages
  {$IFDEF D4} ,Dsgnintf {$ENDIF}
  {$IFDEF D5} ,Dsgnintf {$ENDIF}
  {$IFDEF D6} ,DesignIntf, DesignEditors {$ENDIF}
  {$IFDEF D7} ,DesignIntf, DesignEditors {$ENDIF}
;

const OraLibName='oci.dll'; // OCI library main file name (can be changed in OnBeforeInitOCI event)

type
  TOraDB=class;

  TOraConnectAs   = (caNormal, caSYSDBA, caSYSOPER);

  TOraTransIsolationLevel   = (tiDefault, tiReadCommited, tiRepeatableRead, tiReadOnly);
  TOraSessionIsolationLevel = (siDefault, siReadCommited, siRepeatableRead);
  TOraSQLTrace  = (stDefault, stTrue, stFalse);

  TBeforeLoginEvent = procedure(Sender: TOraDB;var ConnectString, Username, Password: string;Accept:boolean) of object;
  TAfterLoginEvent  = procedure(Sender: TOraDB) of object;
  TBeforeInitOCI    = procedure(Sender: TOraDB;LibName:string;Accept:boolean) of object;
  TAfterInitOCI     = procedure(Sender: TOraDB) of object;


  TOraPreferences = class(TPersistent)
  private
    FConvertCRLF:boolean;
    FFloatPrecision:integer;
    FIntegerPrecision:integer;
    FSmallIntPrecision:integer;
//   FMaxStringFieldSize:integer;
  public
    constructor Create;
  published
    property ConvertCRLF:boolean read FConvertCRLF write FConvertCRLF default True;
    property FloatPrecision:integer read FFloatPrecision write FFloatPrecision default 0;
    property IntegerPrecision:integer read FIntegerPrecision write FIntegerPrecision default 0;
    property SmallIntPrecision:integer read FSmallIntPrecision write FSmallIntPrecision default 0;
//   property MaxStringFieldSize:integer read FMaxStringFieldSize write FMaxStringFieldSize;
  end;

  TOraDB = class(TADataBase)
  private
   hDll:THandle;
   FStreamedActive:boolean;
   FActive:boolean;
   FStarted:boolean;
   FName,FPassword,FServer:string;
   FLoginPrompt:boolean;
   FTransaction:boolean;
   FOraTransIsolationLevel:TOraTransIsolationLevel;
   FOraSessionIsolationLevel:TOraSessionIsolationLevel;
   FRollbackOnDisconnect:boolean;
   FConnectAs:TOraConnectAs;
   FSQLTrace:TOraSQLTrace;
   FPreferences:TOraPreferences;

   FBeforeInitOCI:TBeforeInitOCI;
   FAfterInitOCI:TAfterInitOCI;
   FBeforeLoginEvent:TBeforeLoginEvent;
   FAfterLoginEvent:TAfterLoginEvent;
   FBeforeCommit:TNotifyEvent;
   FAfterCommit:TNotifyEvent;
   FOnStartTransaction:TNotifyEvent;
   FOnRollback:TNotifyEvent;



    FOraRegKey:string;
    FOraHome:string;
    FOraDllName:string;
    FOraIsPO8:boolean;
    FOraTnsNames:string;
    FOraDllVersion:integer;

   procedure GetOCIVersion;
   procedure LoadTNSPaths;
   procedure LoadOCIPaths;
   procedure InitOCI;

  protected
   procedure Loaded; override;
   procedure SetSessionIsolationLevel;
   procedure SetTransIsolationLevel;
   procedure SetSQLTrace;
   procedure SetActive(Value:boolean);override;
   function GetActive:boolean;override;

  public
   myenvhp:pOCIEnv;
   mysrvhp:pOCIServer;
   dberrhp:pOCIError;
   myusrhp:pOCISession;
   mysvchp:pOCISvcCtx;

   OCIEnvCreate:TOCIEnvCreate;
   OCIInitialize:TOCIInitialize;
   OCIEnvInit:TOCIEnvInit;
   OCIHandleAlloc:TOCIHandleAlloc;
   OCIServerAttach:TOCIServerAttach;
   OCIAttrSet:TOCIAttrSet;
   OCISessionBegin:TOCISessionBegin;
   OCISessionEnd:TOCISessionEnd;
   OCIServerDetach:TOCIServerDetach;
   OCIHandleFree:TOCIHandleFree;
   OCIErrorGet:TOCIErrorGet;
   OCIStmtPrepare:TOCIStmtPrepare;
   OCIStmtExecute:TOCIStmtExecute;
   OCIParamGet:TOCIParamGet;
   OCIAttrGet:TOCIAttrGet;
   OCIStmtFetch:TOCIStmtFetch;
   OCIDefineByPos:TOCIDefineByPos;
   OCIDefineArrayOfStruct:TOCIDefineArrayOfStruct;
   OCIBindByPos:TOCIBindByPos;
   OCIBindByName:TOCIBindByName;
   OCITransStart:TOCITransStart;
   OCITransCommit:TOCITransCommit;
   OCITransRollback:TOCITransRollback;
   OCIDescribeAny:TOCIDescribeAny;
   OCIBreak:TOCIBreak;
   OCIDescriptorAlloc:TOCIDescriptorAlloc;
   OCIDescriptorFree:TOCIDescriptorFree;
   OCILobRead:TOCILobRead;
   OCILobWrite:TOCILobWrite;
   OCIStmtGetPieceInfo:TOCIStmtGetPieceInfo;
   OCIStmtSetPieceInfo:TOCIStmtSetPieceInfo;
   OCILobGetLength:TOCILobGetLength;
   OCILobErase:TOCILobErase;
   OCILobTrim:TOCILobTrim;

   procedure GetServicesList(AList: TStrings);

   function TestError(where:string;ex:sword):sword;

   constructor Create(AOwner:TComponent); override;
   destructor Destroy; override;
   procedure Open;override;
   procedure Close;override;
   procedure StartTransaction;
   procedure CommitTransaction;
   procedure RollbackTransaction;
   procedure Break; // executes OCIBreak
   property  InTransaction:boolean read FTransaction;
  published
   property Active;//:boolean read FActive write DoActive default False;
   property DBLogin:string read FName write FName;
   property DBPassword:string read FPassword write FPassword;
   property DBServer:string read FServer write FServer;
   property LoginPrompt:boolean read FLoginPrompt write FLoginPrompt default True;
   property OraTransIsolationLevel:TOraTransIsolationLevel read FOraTransIsolationLevel write FOraTransIsolationLevel;
   property OraSessionIsolationLevel:TOraSessionIsolationLevel read FOraSessionIsolationLevel write FOraSessionIsolationLevel;
   property RollbackOnDisconnect:boolean read FRollbackOnDisconnect write FRollbackOnDisconnect;
   property ConnectAs:TOraConnectAs read FConnectAs write FConnectAs;
   property SQLTrace:TOraSQLTrace read FSQLTrace write FSQLTrace;
   property Preferences:TOraPreferences read FPreferences write FPreferences;
   property BeforeInitOCI:TBeforeInitOCI read FBeforeInitOCI write FBeforeInitOCI;
   property AfterInitOCI :TAfterInitOCI read FAfterInitOCI write FAfterInitOCI;
   property BeforeLogin:TBeforeLoginEvent read FBeforeLoginEvent write FBeforeLoginEvent;
   property AfterLogin:TAfterLoginEvent read FAfterLoginEvent write FAfterLoginEvent;
   property OnStartTransaction:TNotifyEvent read FOnStartTransaction write FOnStartTransaction;
   property BeforeCommit:TNotifyEvent read FBeforeCommit write FBeforeCommit;
   property AfterCommit:TNotifyEvent read FAfterCommit write FAfterCommit;
  end;

  TTNSNamesProperty = class(TStringProperty)
    public
      function GetAttributes: TPropertyAttributes; override;
      procedure GetValueList(List: TStrings); virtual;
      procedure GetValues(Proc: TGetStrProc); override;
    end;

procedure Register;

const
   // Oracle client version constants
    OraVer80000 = 800000000;
    OraVer80400 = 800040000;
    OraVer80500 = 800050000;
    OraVer80501 = 800050001;
    OraVer81000 = 801000000;
    OraVer81500 = 801050000;

implementation

uses SysUtils, Dblogdlg, Registry, OraError, OraUtils;

procedure Register;
begin
  RegisterComponents('Data Access', [TOraDB]);
  RegisterPropertyEditor(TypeInfo(string), TOraDB,'DBServer', TTNSNamesProperty);
end;

(*
const

NumberOfUsedOCIFunctions = 33;

UsedOCIFunctionNames : array[1..NumberOfUsedOCIFunctions ] of string = (
{'OCIEnvCreate',}'OCIInitialize','OCIEnvInit','OCIHandleAlloc','OCIServerAttach',
'OCIAttrSet','OCISessionBegin','OCISessionEnd','OCIServerDetach','OCIHandleFree',
'OCIErrorGet','OCIStmtPrepare','OCIStmtExecute','OCIParamGet','OCIAttrGet',
'OCIStmtFetch','OCIDefineByPos','OCIDefineArrayOfStruct','OCIBindByPos',
'OCIBindByName','OCITransStart','OCITransCommit','OCITransRollback','OCIDescribeAny',
'OCIBreak','OCIDescriptorAlloc','OCIDescriptorFree','OCILobRead','OCILobWrite',
'OCIStmtGetPieceInfo','OCIStmtSetPieceInfo','OCILobGetLength','OCILobErase','OCILobTrim');
*)


 { TOraPreferences }

constructor TOraPreferences.Create;
begin
// inherited Create;
 ConvertCRLF:=True;
end;


 { TTNSNamesProperty }

function TTNSNamesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList{, paMultiSelect}];
end;

procedure TTNSNamesProperty.GetValues(Proc: TGetStrProc);
var
    I: Integer;
    Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

procedure TTNSNamesProperty.GetValueList(List: TStrings);
var DB:TOraDB;
begin
 DB:=GetComponent(0) as TOraDB;
 if DB<>nil then DB.GetServicesList(List);
end;


 { TOraDB }

constructor TOraDB.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FTransaction:=False;
 FLoginPrompt:=True;
 FRollbackOnDisconnect:=False;
 FConnectAs:=caNormal;
 FSQLTrace:=stDefault;
 FPreferences:=TOraPreferences.Create;
end;

destructor TOraDB.Destroy;
begin
{$ifdef ADEBUG}LogMessage('TOraDB.Destroy BEGIN');{$endif}

 FPreferences.Free;
 if Active then Close;
 inherited Destroy;

{$ifdef ADEBUG}LogMessage('TOraDB.Destroy END');{$endif}
end;


procedure TOraDB.Loaded;
begin
 inherited Loaded;
 Active:=FStreamedActive;
end;

function TOraDB.TestError(where:string;ex:sword):sword;
var errcode:sb4;
    errbuf:array[0..511] of char;
begin
 Result:=ex;
 case ex of
  OCI_SUCCESS: exit;
  OCI_SUCCESS_WITH_INFO: raise EDatabaseError.Create(sOraErrSuccessWithInfo);
  OCI_NEED_DATA: raise EDatabaseError.Create(sOraErrNeedData);
  OCI_NO_DATA: raise EDatabaseError.Create(sOraErrNoData);
  OCI_ERROR: begin
              OCIErrorGet(dberrhp,1,nil,errcode,errbuf,sizeof(errbuf),OCI_HTYPE_ERROR);
              raise EDatabaseError.Create('Oracle error #'+inttostr(errcode)+': '+strpas(errbuf));
             end;
  OCI_INVALID_HANDLE: raise EDatabaseError.Create(sOraErrInvalidHandle);
  OCI_STILL_EXECUTING: raise EDatabaseError.Create(sOraErrStillExecute);
  else raise EDatabaseError.Create(sOraErrUNKNOWN);
 end;
end;

procedure TOraDB.InitOCI;
var Accept:boolean;
    LibName:string;
//    FirstFunc:pointer;
//    p:^pointer;
    errstr:string;
//    i:integer;
    errbuf: PChar;
begin
 LoadOCIPaths;
 GetOCIVersion;
 LoadTNSPaths;
 LibName:=FOraDllName;

 Accept:=True;
 if Assigned(FBeforeInitOCI) then FBeforeInitOCI(self, LibName, Accept);
 if not Accept then exit;

 if not FStarted then begin
  hDll:=LoadLibrary(PChar(LibName));
  if hDll=0 then begin
     FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,
                   nil, GetLastError(), (SUBLANG_DEFAULT shl 10) or LANG_NEUTRAL,
                   PChar(@errbuf), 0, nil);
     errstr := errbuf;
     LocalFree(HLOCAL(errbuf));
//     ADatabaseErrorFmt(sOraErrLoadLibrary, [s], nil);
     raise Exception.CreateFmt(sOraErrLoadLibrary, [errstr]);
  end;

  @OCIEnvCreate:=GetProcAddress(hDll,'OCIEnvCreate');
  @OCIInitialize:=GetProcAddress(hDll,'OCIInitialize');
  @OCIEnvInit:=GetProcAddress(hDll,'OCIEnvInit');
  @OCIHandleAlloc:=GetProcAddress(hDll,'OCIHandleAlloc');
  @OCIServerAttach:=GetProcAddress(hDll,'OCIServerAttach');
  @OCIAttrSet:=GetProcAddress(hDll,'OCIAttrSet');
  @OCISessionBegin:=GetProcAddress(hDll,'OCISessionBegin');
  @OCISessionEnd:=GetProcAddress(hDll,'OCISessionEnd');
  @OCIServerDetach:=GetProcAddress(hDll,'OCIServerDetach');
  @OCIHandleFree:=GetProcAddress(hDll,'OCIHandleFree');
  @OCIErrorGet:=GetProcAddress(hDll,'OCIErrorGet');
  @OCIStmtPrepare:=GetProcAddress(hDll,'OCIStmtPrepare');
  @OCIStmtExecute:=GetProcAddress(hDll,'OCIStmtExecute');
  @OCIParamGet:=GetProcAddress(hDll,'OCIParamGet');
  @OCIAttrGet:=GetProcAddress(hDll,'OCIAttrGet');
  @OCIStmtFetch:=GetProcAddress(hDll,'OCIStmtFetch');
  @OCIDefineByPos:=GetProcAddress(hDll,'OCIDefineByPos');
  @OCIDefineArrayOfStruct:=GetProcAddress(hDll,'OCIDefineArrayOfStruct');
  @OCIBindByPos:=GetProcAddress(hDll,'OCIBindByPos');
  @OCIBindByName:=GetProcAddress(hDll,'OCIBindByName');
  @OCITransStart:=GetProcAddress(hDll,'OCITransStart');
  @OCITransCommit:=GetProcAddress(hDll,'OCITransCommit');
  @OCITransRollback:=GetProcAddress(hDll,'OCITransRollback');
  @OCIDescribeAny:=GetProcAddress(hDll,'OCIDescribeAny');
  @OCIBreak:=GetProcAddress(hDll,'OCIBreak');
  @OCIDescriptorAlloc:=GetProcAddress(hDll,'OCIDescriptorAlloc');
  @OCIDescriptorFree:=GetProcAddress(hDll,'OCIDescriptorFree');
  @OCILobRead:=GetProcAddress(hDll,'OCILobRead');
  @OCILobWrite:=GetProcAddress(hDll,'OCILobWrite');
  @OCIStmtGetPieceInfo:=GetProcAddress(hDll,'OCIStmtGetPieceInfo');
  @OCIStmtSetPieceInfo:=GetProcAddress(hDll,'OCIStmtSetPieceInfo');
  @OCILobGetLength:=GetProcAddress(hDll,'OCILobGetLength');
  @OCILobErase:=GetProcAddress(hDll,'OCILobErase');
  @OCILobTrim:=GetProcAddress(hDll,'OCILobTrim');
  FStarted:=True;
 end;

 {errstr:='';
 FirstFunc:=@@OCIInitialize;
 for i:=1 to NumberOfUsedOCIFunctions do begin
   p:=pointer(integer(FirstFunc)+sizeof(pointer)*(i-1));
   if p^=nil then errstr:=errstr+UsedOCIFunctionNames[i]+' ';
 end;
 if errstr<>'' then raise EDatabaseError.CreateFmt(sOraErrCannotGetFunctions,[errstr]);
  }
 if Assigned(FAfterInitOCI) then FAfterInitOCI(self);
end;

procedure TOraDB.SetSessionIsolationLevel;
var str:array[0..1023] of char;
    mystmthp:pOCIStmt;
begin
 if not FActive then exit;

 case FOraSessionIsolationLevel of
  siReadCommited   : str:='ALTER SESSION SET ISOLATION_LEVEL = READ COMMITTED'; // don't translate
  siRepeatableRead : str:='ALTER SESSION SET ISOLATION_LEVEL = SERIALIZABLE';   // don't translate
  siDefault        : exit;
 end;

 // setting Isolation Level for current session
 TestError('OCIHandleAlloc - ',OCIHandleAlloc(myenvhp,mystmthp,OCI_HTYPE_STMT,0,nil));

 TestError('OCIStmtPrepare - ',OCIStmtPrepare(mystmthp,dberrhp,str,strlen(str),OCI_NTV_SYNTAX,OCI_DEFAULT));

 TestError('OCIStmtExecute ',OCIStmtExecute(mysvchp,mystmthp,dberrhp,1,0,nil,nil,OCI_DEFAULT));

 TestError('OCIHandleFree - ',OCIHandleFree(mystmthp,OCI_HTYPE_STMT));
end;

procedure TOraDB.SetTransIsolationLevel;
var str:array[0..256] of char;
    mystmthp:pOCIStmt;
begin
 if not FActive then exit;

 case FOraTransIsolationLevel of
  tiReadCommited   : str:='SET TRANSACTION ISOLATION LEVEL READ COMMITTED'; // don't translate
  tiRepeatableRead : str:='SET TRANSACTION ISOLATION LEVEL SERIALIZABLE';   // don't translate
  tiReadOnly       : str:='SET TRANSACTION READ ONLY';                      // don't translate
  tiDefault        : exit;
 end;

 // setting Isolation Level for current beginning transaction
 TestError('OCIHandleAlloc - ',OCIHandleAlloc(myenvhp,mystmthp,OCI_HTYPE_STMT,0,nil));

 TestError('OCIStmtPrepare - ',OCIStmtPrepare(mystmthp,dberrhp,str,strlen(str),OCI_NTV_SYNTAX,OCI_DEFAULT));

 TestError('OCIStmtExecute ',OCIStmtExecute(mysvchp,mystmthp,dberrhp,1,0,nil,nil,OCI_DEFAULT));

 TestError('OCIHandleFree - ',OCIHandleFree(mystmthp,OCI_HTYPE_STMT));
end;

procedure TOraDB.SetSQLTrace;
var str:array[0..256] of char;
    mystmthp:pOCIStmt;
begin
 if not FActive then exit;
 if FConnectAs=caSYSOPER then exit; // SYSOPER does not have privileges to execute smth like "alter session set sql_trace = true"

 case FSQLTrace of
  stTrue    : str:='alter session set sql_trace = true';    // don't translate
  stFalse   : str:='alter session set sql_trace = false';   // don't translate
  stDefault : exit;
 end;

 // setting sql_trace for current session
 TestError('OCIHandleAlloc - ',OCIHandleAlloc(myenvhp,mystmthp,OCI_HTYPE_STMT,0,nil));

 TestError('OCIStmtPrepare - ',OCIStmtPrepare(mystmthp,dberrhp,str,strlen(str),OCI_NTV_SYNTAX,OCI_DEFAULT));

 TestError('OCIStmtExecute ',OCIStmtExecute(mysvchp,mystmthp,dberrhp,1,0,nil,nil,OCI_DEFAULT));

 TestError('OCIHandleFree - ',OCIHandleFree(mystmthp,OCI_HTYPE_STMT));
end;


procedure TOraDB.Open;
var str:array[0..1023] of char;
    Accept:boolean;
    ConnAs:array[TOraConnectAs] of integer;
begin
 ConnAs[caNormal]:=OCI_DEFAULT;
 ConnAs[caSYSDBA]:=OCI_SYSDBA;
 ConnAs[caSYSOPER]:=OCI_SYSOPER;

 InitOCI;

 Accept:=True;
 if Assigned(FBeforeLoginEvent) then FBeforeLoginEvent(self,FServer,FName,FPassword,Accept);
 if not Accept then exit;

 if FLoginPrompt then
  if not LoginDialogEx(FServer, FName, FPassword, False) then
      ADatabaseErrorFmt(SOraErrLogin, [FServer]);

 if @OCIEnvCreate=nil then begin
  TestError('OCIInitialize ',OCIInitialize(OCI_DEFAULT {OCI_THREADED},nil,nil,nil,nil));
  TestError('OCIEnvInit ',OCIEnvInit(myenvhp, OCI_DEFAULT, 0, nil));
 end else
   TestError('OCIEnvCreate',OCIEnvCreate(myenvhp, OCI_DEFAULT,nil,nil,nil,nil,0, nil));

 // allocation of handlers
 TestError('OCIHandleAlloc ',OCIHandleAlloc(myenvhp,mysrvhp,OCI_HTYPE_SERVER,0,nil));
 TestError('OCIHandleAlloc ',OCIHandleAlloc(myenvhp,dberrhp,OCI_HTYPE_ERROR,0,nil));
 TestError('OCIHandleAlloc ',OCIHandleAlloc(myenvhp,mysvchp,OCI_HTYPE_SVCCTX,0,nil));
 strpcopy(str,FServer);

 // initialization mysrvhp (server context handle)
 TestError('OCIServerAttach ',OCIServerAttach(mysrvhp,dberrhp,@str,strlen(str),OCI_DEFAULT));

 // create association between server and service context handlers
 TestError('OCIAttrSet ',OCIAttrSet(mysvchp,OCI_HTYPE_SVCCTX,mysrvhp,0,OCI_ATTR_SERVER,dberrhp));
 TestError('OCIHandleAlloc ',OCIHandleAlloc(myenvhp,myusrhp,OCI_HTYPE_SESSION,0,nil));
 strpcopy(str,FName);
 TestError('OCIAttrSet ',OCIAttrSet(myusrhp, OCI_HTYPE_SESSION, @str, strlen(str), OCI_ATTR_USERNAME, dberrhp));
 strpcopy(str,FPassword);
 TestError('OCIAttrSet ',OCIAttrSet(myusrhp, OCI_HTYPE_SESSION, @str, strlen(str), OCI_ATTR_PASSWORD, dberrhp));
 TestError('OCISessionBegin ',OCISessionBegin(mysvchp, dberrhp, myusrhp, OCI_CRED_RDBMS, ConnAs[FConnectAs]{OCI_DEFAULT}));
 TestError('OCIAttrSet ',OCIAttrSet(mysvchp, OCI_HTYPE_SVCCTX, myusrhp, 0, OCI_ATTR_SESSION, dberrhp));
 FActive:=True;

 SetSQLTrace;
 SetSessionIsolationLevel;

 if Assigned(FAfterLoginEvent) then FAfterLoginEvent(self);
end;

procedure TOraDB.Close;
begin
 if not FActive then exit;
 CloseLinkedDataSets; // we need to close all DataSets which are linked to this OraDB.

// doing RollBack if RollbackOnDisconnect is set
 if FRollbackOnDisconnect
   then TestError('OCITransRollback ',OCITransRollback(mysvchp,dberrhp,OCI_DEFAULT));
 //??? may be we need to add OCITransCommit here ???

 FTransaction:=False;
 TestError('OCISessionEnd ',OCISessionEnd(mysvchp,dberrhp,myusrhp,OCI_DEFAULT));
 TestError('OCIServerDetach ',OCIServerDetach(mysrvhp,dberrhp,OCI_DEFAULT));
 TestError('OCIHandleFree ',OCIHandleFree(mysrvhp,OCI_HTYPE_SERVER));
 TestError('OCIHandleFree ',OCIHandleFree(mysvchp,OCI_HTYPE_SVCCTX));
 TestError('OCIHandleFree ',OCIHandleFree(myusrhp,OCI_HTYPE_SESSION));
 TestError('OCIHandleFree ',OCIHandleFree(dberrhp,OCI_HTYPE_ERROR));
 TestError('OCIHandleFree ',OCIHandleFree(myenvhp,OCI_HTYPE_ENV));
 FActive:=False;

 FreeLibrary(hDll);
 FStarted:=False;
end;

procedure TOraDB.Break;
begin
 TestError('OCIBreak ',OCIBreak(mysvchp,dberrhp));
end;

function TOraDB.GetActive:boolean;
begin
 Result:=FActive;
end;

procedure TOraDB.SetActive(Value:boolean);
begin
  if (csReading in ComponentState) then  begin
    if Value then FStreamedActive := True;
    exit;
  end;
 if (csDestroying in ComponentState) then exit;
 if Value and not Active then Open;
 if not Value and Active then Close;
end;

procedure TOraDB.StartTransaction;
begin
 if not Active then begin
  raise Exception.Create(sOraErrDatabaseNotActive);
 end;

 SetTransIsolationLevel;

// TestError('OCIHandleFree ',OCITransStart(mysvchp,dberrhp,30,OCI_TRANS_NEW+OCI_TRANS_SERIALIZABLE));
 FTransaction:=True;

 if Assigned(FOnStartTransaction) then FOnStartTransaction(self);
end;

procedure TOraDB.CommitTransaction;
begin
 if not Active then begin
  raise Exception.Create(sOraErrDatabaseNotActive);
 end;

 if Assigned(FBeforeCommit) then FBeforeCommit(self);

 TestError('OCITransCommit ',OCITransCommit(mysvchp,dberrhp,OCI_DEFAULT));
 FTransaction:=False;

 if Assigned(FAfterCommit) then FAfterCommit(self);
end;

procedure TOraDB.RollbackTransaction;
begin
 if not Active then begin
  raise Exception.Create(sOraErrDatabaseNotActive);
 end;

 if Assigned(FOnRollback) then FOnRollback(self);

 TestError('OCITransRollback ',OCITransRollback(mysvchp,dberrhp,OCI_DEFAULT));
 FTransaction:=False;
end;

procedure TOraDB.LoadOCIPaths;
var
    s: string;
    i, n: Integer;
    reg: TRegistry;
    defhome:string;

begin
    reg := TRegistry.Create;
    try
        reg.RootKey := HKEY_LOCAL_MACHINE;
//{ $IFDEF OCI_D4}
//            if reg.OpenKeyReadOnly('\Software\Oracle\All_Homes') then begin
//{ $ELSE}
            if reg.OpenKey('\Software\Oracle\All_Homes', False) then begin
//{ $ENDIF}
                defhome :=reg.ReadString('DEFAULT_HOME');
                s := reg.ReadString('HOME_COUNTER');
                if s = ''
                  then n := 0
                  else n := StrToInt(s);

                i := 0;
                while i<n do begin
                  if reg.OpenKey('\Software\Oracle\Home' + IntToStr(i), False) then
                    if AnsiCompareText(defhome, reg.ReadString('ORACLE_HOME_NAME'))=0 then begin
                      FOraRegKey:='\Software\Oracle\Home' + IntToStr(i);
                      FOraHome:= reg.ReadString('ORACLE_HOME');
                      FOraDllName := reg.ReadString('ORAOCI');
                      FOraIsPO8 := (CompareText(reg.ReadString('PO8'), 'YES') = 0);
                      i:=n;//break;
                    end;
                  inc(i);
               end;
            end;

        if FOraHome = '' then
             ADatabaseError(sOraErrOCINotInstalled, nil);
        if FOraDllName = '' then begin
            FOraDllName := FOraHome + '\bin\'+OraLibName;
            // single case, than things differs - 8.0.3
            if not FileExists(FOraDllName) then
                FOraDllName := FOraHome + '\bin\ora803.dll';
        end;
    finally
        reg.Free;
    end;
end;

procedure TOraDB.LoadTNSPaths;
var
    reg: TRegistry;
begin
    reg := TRegistry.Create;
    try
        reg.RootKey := HKEY_LOCAL_MACHINE;
//{ $IFDEF OCI_D4}
//        reg.OpenKeyReadOnly(FOCIKey);
//{ $ELSE}
        reg.OpenKey(FOraRegKey, False);
//{ $ENDIF}
        FOraTnsNames := reg.ReadString('TNS_NAMES');
        if FOraTnsNames = '' then begin
            FOraTnsNames := CorrectPath(reg.ReadString('TNS_ADMIN'));
            if FOraTnsNames = '' then begin
                if FOraDllVersion >= OraVer81000 then
                    FOraTnsNames := CorrectPath(reg.ReadString('NETWORK'))
                else
                    FOraTnsNames := CorrectPath(reg.ReadString('NET80'));
                if FOraTnsNames = '' then begin
                    if FOraDllVersion >= OraVer81000 then
                        FOraTnsNames := FOraHome + '\Network'
                    else
                        FOraTnsNames := FOraHome + '\net80';
                end;
                FOraTnsNames := FOraTnsNames + '\ADMIN';
            end;
            FOraTnsNames := FOraTnsNames + '\tnsnames.ora';
        end;
    finally
        reg.Free;
    end;
end;

procedure TOraDB.GetOCIVersion;
const
    verstr:string = '\StringFileInfo\040904B0\FileVersion';
    undef:string = '<UNDEFINED>';
var
    hndl, sz: DWORD;
    buf: pointer;
    len: UINT;
    pStrVer, errbuf: PChar;
    s:string;
begin
    FOraDllVersion := 0;
    pStrVer := nil;
    sz := GetFileVersionInfoSize(PChar(FOraDllName), hndl);
    if sz > 0 then begin
      GetMem(buf, sz);
      try
        if GetFileVersionInfo(PChar(FOraDllName), hndl, sz, buf) then
          if VerQueryValue(buf, PChar(verstr), Pointer(pStrVer), Len)
            then FOraDllVersion := VerStr2Int(pStrVer);
      finally
        FreeMem(buf, sz);
      end;
    end
    else begin
      FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,
                   nil, GetLastError(), (SUBLANG_DEFAULT shl 10) or LANG_NEUTRAL,
                   PChar(@errbuf), 0, nil);
      s := errbuf;
      LocalFree(HLOCAL(errbuf));
      ADatabaseErrorFmt(sOraErrOCIVersionInfoAbsent, [s], nil);  //OCIDBErrorFmt(msgOCINotLoaded, [s], nil);
    end;

    if FOraDllVersion < OraVer80000 then begin
      if pStrVer = nil
        then  pStrVer := PChar(undef);
      ADatabaseErrorFmt(sOraErrBadOCIVersion, [StrPas(pStrVer)], nil);
    end;
end;

procedure TOraDB.GetServicesList(AList: TStrings);
var
    InComment, InStr: Boolean;
    pCh, pStParam: PChar;
    s, buff: String;
    BraceLevel: Integer;
    f: TFileStream;
begin
    InitOCI;
    AList.Clear;
    if FOraIsPO8 then
        AList.Add('<LOCAL>');
    try
        f := TFileStream.Create(FOraTnsNames, fmOpenRead or fmShareDenyWrite);
        try
            SetLength(buff, f.Size);
            f.Read(PChar(Buff)^, f.Size);
        finally
            f.Free;
        end;
    except
        Exit;
    end;
    InComment := False;
    InStr := False;
    BraceLevel := 0;
    pCh := PChar(Buff) - 1;
    repeat
        Inc(pCh);
        case pCh^ of
        '#':
            begin
                if not InComment and not InStr then
                    InComment := True;
            end;
        '''':
            if not InComment then
                InStr := not InStr;
        '(':
            if not InComment and not InStr then
                Inc(BraceLevel);
        ')':
            if not InComment and not InStr then
                Dec(BraceLevel);
        #13, #10:
            if InComment then
                InComment := False;
        'a'..'z', 'A'..'Z', '0'..'9':
            if not InComment and not InStr and (BraceLevel = 0) then begin
                pStParam := pCh;
                while pCh^ in ['a'..'z', 'A'..'Z', '0'..'9', '#', '$', '_', '.', '-'] do
                    Inc(pCh);
                SetString(s, pStParam, pCh - pStParam);
                AList.Add(s);
                Dec(pCh);
            end;
        end;
    until (pCh^ = #0);
end;


end.

