unit KDaoWorkspace;
{$B-}
//******************************************************************************
//                           Delphi Dao Project
//                 Copyright (c) 2000-2001 by Kiril Antonov
//******************************************************************************
{$I KADaoCommonDirectives.pas}
interface

uses
DAOApi,
ComObj,
{$IFDEF DAO35}
DAO35Api,
{$ENDIF}
{$IFDEF DAO36}
DAO36Api,
{$ENDIF}
{$IFDEF DAO120}
DAO120Api,
{$ENDIF}
Windows, Messages, SysUtils, Classes, Forms, Dialogs, KDaoDBEngine
{$IFDEF D6UP}, Variants{$ENDIF};

//******************************************************* DatabaseError Messages
{$I ErrLangDB.pas}
//******************************************************************************

type
  TKADaoWorkspace = class(TComponent)
  private
    { Private declarations }
    F_OLE_ON                    : Boolean;
    F_RuntimeLicense            : String;
    F_Destroyng                 : Boolean;
  protected
    { Protected declarations }
    F_ConnectedDatabases        : Integer;
    F_DBEngine                  : TKADaoDBEngine;
    F_DefaultCursorDriver       : Integer;
    F_EngineType                : Integer;
    F_IsolateODBCTrans          : Boolean;
    F_LoginTimeout              : Integer;
    F_Password                  : String;
    F_UserName                  : String;
    F_Workspace                 : String;
    F_Connected                 : Boolean;
    F_Databases                 : TStringList;
    F_ComponentVersion          : String;
    Procedure F_Set_ComponentVersion      (Value: String);
    Procedure F_Set_ConnectedDatabases    (Value : Integer);
    Procedure F_Set_DBEngine              (Value : TKADaoDBEngine);
    Procedure F_Set_DefaultCursorDriver   (Value : Integer);
    Procedure F_Set_EngineType            (Value : Integer);
    Procedure F_Set_IsolateODBCTrans      (Value : Boolean);
    Procedure F_Set_LoginTimeout          (Value : Integer);
    Procedure F_Set_Password              (Value : String);
    Procedure F_Set_UserName              (Value : String);
    Procedure F_Set_Workspace             (Value : String);
    Procedure F_Set_Connected             (Value : Boolean);
    Procedure Loaded; override;
  public
    { Public declarations }
    {$IFDEF DYNADAO} //****************************************************
    CoreWorkspace               : OleVariant;
    {$ENDIF}
    {$IFDEF DAO35}
    CoreWorkspace               : DAO35Api.Workspace;
    {$ENDIF}
    {$IFDEF DAO36}
    CoreWorkspace               : DAO36Api.Workspace;
    {$ENDIF}
    {$IFDEF DAO120}
    CoreWorkspace               : DAO120Api.Workspace;
    {$ENDIF}
    Procedure                     Attach(Name : String; Value : TComponent);
    Procedure                     Detach(Name : String; Value : TComponent);
    Procedure                     ReleaseCore;
    Procedure                     RecreateCore;
    Procedure                     Open;
    Procedure                     Close;
    Procedure                     StartTransaction;
    Procedure                     Commit;
    Procedure                     Rollback;
    Procedure                     RollbackRefresh;
    Procedure                     CreateAccessDatabase    (DatabaseName:String);
    Procedure                     CreateAccessDatabaseEx  (DatabaseName,LANGID,CP,COUNTRY,Password,Version:String;Encrypt:Boolean);
    Procedure                     CreateAccessDatabaseEx2 (DatabaseName,Language,Password,Version:String;Encrypt:Boolean);
    Constructor                   Create(AOwner : TComponent); override;
    Destructor                    Destroy; override;
  published
    { Published declarations }
    Property ComponentVersion   : String         Read F_ComponentVersion    Write F_Set_ComponentVersion;
    Property ConnectedDatabases : Integer        Read F_ConnectedDatabases  Write F_Set_ConnectedDatabases;
    Property DaoDBEngine        : TKADaoDBEngine Read F_DBEngine            Write F_Set_DBEngine;
    Property DefaultCursorDriver: Integer        Read F_DefaultCursorDriver Write F_Set_DefaultCursorDriver;
    Property EngineType         : Integer        Read F_EngineType          Write F_Set_EngineType;
    Property IsolateODBCTrans   : Boolean        Read F_IsolateODBCTrans    Write F_Set_IsolateODBCTrans;
    Property LoginTimeout       : Integer        Read F_LoginTimeout        Write F_Set_LoginTimeout;
    Property Password           : String         Read F_Password            Write F_Set_Password;
    Property UserName           : String         Read F_UserName            Write F_Set_UserName;
    Property WorkspaceName      : String         Read F_Workspace           Write F_Set_Workspace;
    Property Connected          : Boolean        Read F_Connected           Write F_Set_Connected;
  end;

procedure Register;

implementation
Uses
  ActiveX;

{$IFNDEF D5UP}
var
  //   ***************************************************
  //   Defined only for Delphi3 and Delphi4
  //   Delphi5 has buildin support for EmptyParam
  //   ***************************************************
  EmptyParam : OleVariant;
  Unassigned : OleVariant;
{$ENDIF}


Procedure DatabaseError(Msg:String);
Begin
  Raise Exception.Create(Msg);
End;


Constructor TKADaoWorkspace.Create(AOwner : TComponent);
Begin
 Inherited Create(AOwner);
 F_ComponentVersion     := '10.00';
 F_Destroyng            := False;
 F_ConnectedDatabases   := 0;
 F_RuntimeLicense       := '';
 F_DBEngine := Nil;
 {$IFDEF DYNADAO}
 CoreWorkspace := Unassigned;
 {$ELSE}
 CoreWorkspace := Nil;
 {$ENDIF}
 F_DefaultCursorDriver := DaoApi.dbUseDefaultCursor;
 F_EngineType          := DaoApi.dbUseJet;
 F_IsolateODBCTrans    := False;
 F_LoginTimeout        := 0;
 F_Password            := '';
 F_UserName            := 'Admin';
 F_Workspace           := 'DaoWorkspace';
 F_Connected           := False;
 F_OLE_ON              := False;
 F_Databases           := TStringList.Create;
 F_Databases.Clear;
End;

Destructor  TKADaoWorkspace.Destroy;
Begin
  F_Destroyng := True;
  Connected   := False;
  F_Databases.Free;
  ReleaseCore;
  if F_DBEngine <> Nil Then F_DBEngine.Detach(Name,Self);
  Inherited Destroy;
End;

Procedure TKADaoWorkspace.Attach(Name : String; Value : TComponent);
Begin
  F_Databases.AddObject(Name,TObject(Value));
  Inc(F_ConnectedDatabases);
End;

Procedure TKADaoWorkspace.Detach(Name : String; Value : TComponent);
Var
  I : Integer;
Begin
  I := F_Databases.IndexOf(Name);
  if I <> -1 Then
     Begin
       F_Databases.Delete(I);
       Dec(F_ConnectedDatabases);
     End;
End;

Procedure TKADaoWorkspace.ReleaseCore;
Begin
  {$IFDEF DYNADAO}
  If (Not VarIsNull(CoreWorkspace)) And (Not VarIsEmpty(CoreWorkspace)) Then CoreWorkspace.Close;
  Try
     VarClear(CoreWorkspace);
   Except
   End;
  {$ELSE}
  if (CoreWorkspace <> Nil) Then CoreWorkspace.Close;
  CoreWorkspace := Nil;                                        
  {$ENDIF}
End;

Procedure TKADaoWorkspace.RecreateCore;
Begin
 ReleaseCore;
 CoreWorkspace := F_DBEngine.CoreDBEngine.CreateWorkspace(F_Workspace,F_UserName,F_Password,F_EngineType);
 F_DBEngine.CoreDBEngine.Workspaces.Append(CoreWorkspace);
 if F_EngineType=dbUseODBC Then
    Begin
      CoreWorkspace.LoginTimeOut        := F_LoginTimeOut;
      CoreWorkspace.DefaultCursorDriver := F_DefaultCursorDriver;
    End
 Else
    Begin
      CoreWorkspace.IsolateODBCTrans    := SmallInt(F_IsolateODBCTrans);
    End;
End;

Procedure TKADaoWorkspace.Loaded;
Begin
  try
    inherited Loaded;
    if F_Connected Then RecreateCore;
  except
    Application.HandleException(Self)
  end;
End;

Procedure TKADaoWorkspace.F_Set_ComponentVersion(Value: String);
Begin
  //******************************************************************* ReadOnly
End;

Procedure TKADaoWorkspace.F_Set_ConnectedDatabases(Value : Integer);
Begin
  //******************************************************************* ReadOnly
End;

Procedure TKADaoWorkspace.F_Set_DBEngine(Value : TKADaoDBEngine);
Begin
 if (NOT (csLoading in ComponentState)) And (F_Connected) Then DatabaseError(E1033);
 if Name = '' Then Name:='KADaoWorkspace#'+IntToStr(GetTickCount);
 if F_DBEngine <> Nil Then F_DBEngine.Detach(Name,Self);
 F_DBEngine := Value;
 if F_DBEngine <> Nil Then F_DBEngine.Attach(Name,Self);
 if csLoading in ComponentState Then Exit;
 if F_DBEngine=Nil Then Exit;
 F_EngineType   := F_DBEngine.EngineType;
 F_LoginTimeout := F_DBEngine.LoginTimeout;
 F_Password     := F_DBEngine.DefaultPassword;
 F_UserName     := F_DBEngine.DefaultUser;
End;

Procedure TKADaoWorkspace.F_Set_DefaultCursorDriver(Value : Integer);
Begin
  if F_Connected Then DatabaseError(E1034);
  F_DefaultCursorDriver:=Value;
  if csLoading in ComponentState Then Exit;
End;

Procedure TKADaoWorkspace.F_Set_EngineType(Value : Integer);
Begin
  if F_Connected Then DatabaseError(E1034);
  F_EngineType := Value;
  if (F_DBEngine <> Nil) And (F_EngineType <> F_DBEngine.EngineType) Then F_EngineType := F_DBEngine.EngineType;
End;

Procedure TKADaoWorkspace.F_Set_IsolateODBCTrans(Value : Boolean);
Begin
  if F_Connected Then DatabaseError(E1034);
  F_IsolateODBCTrans := Value;
  if csLoading in ComponentState Then Exit;
End;

Procedure TKADaoWorkspace.F_Set_LoginTimeout(Value : Integer);
Begin
  if F_Connected Then DatabaseError(E1034);
  F_LoginTimeout := Value;
  if csLoading in ComponentState Then Exit;
End;

Procedure TKADaoWorkspace.F_Set_Password(Value : String);
Begin
  if F_Connected Then DatabaseError(E1034);
  F_Password := Value;
  if csLoading in ComponentState Then Exit;
End;

Procedure TKADaoWorkspace.F_Set_UserName(Value : String);
Begin
  if F_Connected Then DatabaseError(E1034);
  F_UserName := Value;
  if csLoading in ComponentState Then Exit;
End;

Procedure TKADaoWorkspace.F_Set_Workspace(Value : String);
Begin
  if F_Connected Then DatabaseError(E1034);
  F_Workspace := Value;
  if csLoading in ComponentState Then Exit;
End;

Procedure TKADaoWorkspace.F_Set_Connected(Value : Boolean);
Begin
  if csLoading in ComponentState Then
     Begin
       F_Connected := Value;
       Exit;
     End;
  if Value Then
     Begin
       if Not Assigned(F_DBEngine) Then DatabaseError(E1035);
       if Not F_DBEngine.Connected Then DatabaseError(E1036);
       RecreateCore;
     End;
  F_Connected := Value;
End;

Procedure TKADaoWorkspace.Open;
Begin
  Connected := True;
End;

Procedure TKADaoWorkspace.Close;
Begin
  Connected := False;
End;

Procedure TKADaoWorkspace.StartTransaction;
Begin
  if (NOT F_Connected) Then
     Begin
       DatabaseError(E1023);
       Exit;
     End;
  CoreWorkspace.BeginTrans;
End;

Procedure TKADaoWorkspace.Commit;
Begin
 if (NOT F_Connected) Then
     Begin
       DatabaseError(E1024);
       Exit;
     End;
 CoreWorkspace.CommitTrans(dbForceOSFlush);
End;

Procedure TKADaoWorkspace.Rollback;

Begin
 CoreWorkspace.Rollback;
End;

Procedure TKADaoWorkspace.RollbackRefresh;
Begin
//**************************************** NOOP
End;

Procedure TKADaoWorkspace.CreateAccessDatabase(DatabaseName:String);
Var
 CreateOptions : String;
Begin
 CreateOptions:=Format(dbLangGeneral,['0x0409','1252','0']);
 {$IFDEF DAO35}
 CoreWorkspace.CreateDatabase(DatabaseName,CreateOptions, dbVersion30);
 {$ENDIF}
 {$IFDEF DAO36}
 CoreWorkspace.CreateDatabase(DatabaseName,CreateOptions, dbVersion40);
 {$ENDIF}
 {$IFDEF DAO120}
 CoreWorkspace.CreateDatabase(DatabaseName,CreateOptions, dbVersion120);
 {$ENDIF}
 {$IFDEF DYNADAO}
 if F_DBEngine.Version =  '3.5' then CoreWorkspace.CreateDatabase(DatabaseName,CreateOptions, dbVersion30);
 if F_DBEngine.Version =  '3.6' then CoreWorkspace.CreateDatabase(DatabaseName,CreateOptions, dbVersion40);
 if F_DBEngine.Version = '12.0' then CoreWorkspace.CreateDatabase(DatabaseName,CreateOptions, dbVersion120);
 {$ENDIF}
End;


Procedure TKADaoWorkspace.CreateAccessDatabaseEx(DatabaseName,LANGID,CP,COUNTRY,Password,Version:String;Encrypt:Boolean);
Var
 CreateOptions:String;
Begin
 CreateOptions:=Format(dbLangGeneral,[LANGID,CP,COUNTRY]);
 if Password <> '' Then CreateOptions:=CreateOptions+';PWD='+Password;
 {$IFDEF DAO35}
 if Encrypt Then
    CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
 Else
    CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30);
 {$ENDIF}

 {$IFDEF DAO36}
  if Version='30' Then
     if Encrypt Then
        CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
     Else
        CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30)
  Else
     if Encrypt Then
        CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40 OR dbEncrypt)
     Else
        CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40);
 {$ENDIF}

 {$IFDEF DAO120}
  if Version='30' Then
     Begin
        if Encrypt Then
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
        Else
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30)
     End
  Else
  if Version='40' Then
     Begin
       if Encrypt Then
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40 OR dbEncrypt)
       Else
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40);
     End
 Else
  if Version='120' Then
     Begin
       if Encrypt Then
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion120 OR dbEncrypt)
       Else
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion120);
     End
 {$ENDIF}

 {$IFDEF DYNADAO}
 if F_DBEngine.Version='3.5'  Then
    Begin
       if Encrypt Then
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
       Else
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30);
    End;
 //*****************************************************************************
  if F_DBEngine.Version='3.6'  Then
     Begin
        if Version='30' Then
           Begin
             if Encrypt Then
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
             Else
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30)
           End
        Else
        if Version='40' Then
           Begin
             if Encrypt Then
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40 OR dbEncrypt)
             Else
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40);
           End;
     End;
   if F_DBEngine.Version='12.0'  Then
     Begin
        if Version='30' Then
           Begin
             if Encrypt Then
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
             Else
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30)
           End
        Else
        if Version='40' Then
           Begin
             if Encrypt Then
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40 OR dbEncrypt)
             Else
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40);
           End
        Else
        if Version='120' Then
           Begin
             if Encrypt Then
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion120 OR dbEncrypt)
             Else
                CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion120);
           End

     End;
 {$ENDIF}
End;

Procedure TKADaoWorkspace.CreateAccessDatabaseEx2(DatabaseName,Language,Password,Version:String;Encrypt:Boolean);
Var
 CreateOptions:String;
Begin
 CreateOptions:=Language;
 if Password <> '' Then CreateOptions:=CreateOptions+';PWD='+Password;

 {$IFDEF DAO35}
 if Encrypt Then
    CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
 Else
    CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30);
 {$ENDIF}

 {$IFDEF DAO36}
  if Version='30' Then
     if Encrypt Then
        CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
     Else
        CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30)
  Else
     if Encrypt Then
        CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40 OR dbEncrypt)
     Else
        CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40);
 {$ENDIF}

 {$IFDEF DAO120}
  if Version='30' Then
     Begin
       if Encrypt Then
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
       Else
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30)
    End
  Else
  if Version='40' Then
     Begin
       if Encrypt Then
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40 OR dbEncrypt)
       Else
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40);
     End
  Else
  if Version='120' Then
     Begin
       if Encrypt Then
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion120 OR dbEncrypt)
       Else
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion120);
     End;
 {$ENDIF}

 {$IFDEF DYNADAO}
 if F_DBEngine.Version='3.5'  Then
    Begin
       if Encrypt Then
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
       Else
          CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30);
    End;
  //*****************************************************************************
  if F_DBEngine.Version='3.6'  Then
     Begin
        if Version='30' Then
           Begin
               if Encrypt Then
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
               Else
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30)
           End
        Else
        if Version='40' Then
           Begin
               if Encrypt Then
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40 OR dbEncrypt)
               Else
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40);
           End;
    End;
  //*****************************************************************************
  if F_DBEngine.Version='12.0'  Then
     Begin
        if Version='30' Then
           Begin
               if Encrypt Then
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30 OR dbEncrypt)
               Else
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion30)
           End
        Else
        if Version='40' Then
           Begin
               if Encrypt Then
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40 OR dbEncrypt)
               Else
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion40);
           End
        Else
        if Version='120' Then
           Begin
               if Encrypt Then
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion120 OR dbEncrypt)
               Else
                  CoreWorkspace.CreateDatabase(DatabaseName, CreateOptions, dbVersion120);
           End;
    End;
 {$ENDIF}
End;

procedure Register;
begin
  RegisterComponents('KA Dao', [TKADaoWorkspace]);
end;

Initialization
 {$IFNDEF D5UP}
  TVarData(Unassigned).VType := varEmpty;
  TVarData(EmptyParam).VType := varError;
  TVarData(EmptyParam).VError := $80020004;
 {$ENDIF}
end.
