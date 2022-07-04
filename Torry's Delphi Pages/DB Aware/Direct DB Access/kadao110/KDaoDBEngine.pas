unit KDaoDBEngine;
{$B-}
//******************************************************************************
//                           Delphi Dao Project
//                 Copyright (c) 2000-2001 by Kiril Antonov
//******************************************************************************
{$I KADaoCommonDirectives.pas}

interface

Uses
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
Windows, Messages, SysUtils, Classes, Forms, Dialogs, TypInfo
{$IFDEF D6UP}, Variants{$ENDIF};

//******************************************************* DatabaseError Messages
{$I ErrLangDB.pas}
//******************************************************************************


type
  TKADaoDBEngine = class(TComponent)
  private
    { Private declarations }
    F_OLE_ON               : Boolean;
    F_Destroyng            : Boolean;
    F_RuntimeLicense       : String;
  protected
    { Protected declarations }
    F_DefaultPassword      : String;
    F_DefaultUser          : String;
    F_EngineType           : Integer;
    F_IniPath              : String;
    F_LoginTimeout         : Integer;
    F_PrivateEngine        : Boolean;
    F_SystemDB             : String;
    F_UsesDynaDao          : Boolean;
    F_DaoVersion           : String;
    F_ActualDaoVersion     : String;
    F_VersionInfo          : String;
    F_ConnectedWorkspaces  : Integer;
    F_Connected            : Boolean;
    F_Workspaces           : TStringList;
    F_ComponentVersion     : String;
    Procedure F_Set_ComponentVersion   (Value: String);
    Procedure F_Set_DefaultPassword    (Value : String);
    Procedure F_Set_DefaultUser        (Value : String);
    Procedure F_Set_EngineType         (Value : Integer);
    Procedure F_Set_IniPath            (Value : String);
    Procedure F_Set_LoginTimeout       (Value : Integer);
    Procedure F_Set_PrivateEngine      (Value : Boolean);
    Procedure F_Set_SystemDatabase     (Value : String);
    Procedure F_Set_UsesDynaDao        (Value : Boolean);
    Procedure F_Set_DaoVersion         (Value : String);
    Procedure F_Set_ActualDaoVersion   (Value : String);
    Procedure F_Set_VersionInfo        (Value : String);
    Procedure F_Set_ConnectedWorkspaces(Value : Integer);
    Procedure F_Set_Connected          (Value : Boolean);

    //**************************************************************************
    {$IFDEF DYNADAO}
    Function                      CreateOleDBEngine(const ClassName: string): IDispatch;
    {$ELSE}
    Function                      CreateOleDBEngine(const ClassID: TGUID): DBEngine;
    {$ENDIF}
    Function                      CreateOleDBEngine_II(const ClassName: string): IDispatch;
    Procedure                     CheckEngines;
    Procedure                     CreateDBEngine(DaoVer:String);
    //**************************************************************************

    Procedure                     Loaded; override;
  public
    //******************************* Required to be public for Property Editors
    F_DaoVersionList       : TStringList;
    //**************************************************************************
    { Public declarations }
    {$IFDEF DYNADAO} //****************************************************
    CoreDBEngine                 : OleVariant;
    {$ENDIF}
    {$IFDEF DAO35}
    CoreDBEngine                 : DAO35Api.DBEngine;
    {$ENDIF}
    {$IFDEF DAO36}
     CoreDBEngine                : DAO36Api.DBEngine;
    {$ENDIF}
    {$IFDEF DAO120}
     CoreDBEngine                : DAO120Api.DBEngine;
    {$ENDIF}
     Function                      GetSystemDatabaseFromRegistry:String;
     Procedure                     Attach(Name : String; Value : TComponent);
     Procedure                     Detach(Name : String; Value : TComponent);
     Procedure                     ReleaseCore;
     Procedure                     RecreateCore;
     Procedure                     Open;
     Procedure                     Close;
     Procedure                     StartTransaction;
     Procedure                     Commit;
     Procedure                     Rollback;

     Procedure                     RepairAccessDatabase  (DatabaseName,Password:String);
     Procedure                     RepairAccessDatabaseEx(DatabaseName : String;
                                                          NewLocale    : String;
                                                          Encrypt      : Boolean;
                                                          Decrypt      : Boolean;
                                                          NewVersion   : Integer;
                                                          Password     : String);
     Procedure                     CompactAccessDatabase  (DatabaseName,Password:String);
     Procedure                     CompactAccessDatabaseEx(DatabaseName: String;
                                                           NewLocale   : String;
                                                           Encrypt     : Boolean;
                                                           Decrypt     : Boolean;
                                                           NewVersion  : Integer;
                                                           Password    : String);

     Function                      RegisterDatabase        (DatabaseName, DriverName:String; Silent:Boolean; Attributes:String):Boolean;
     Procedure                     Idle;
     Constructor                   Create(AOwner : TComponent); override;
     Destructor                    Destroy; override;
  published
    { Published declarations }
    Property ComponentVersion    : String  Read F_ComponentVersion    Write F_Set_ComponentVersion;
    Property DefaultPassword     : String  Read F_DefaultPassword     Write F_Set_DefaultPassword;
    Property DefaultUser         : String  Read F_DefaultUser         Write F_Set_DefaultUser;
    Property EngineType          : Integer Read F_EngineType          Write F_Set_EngineType;
    Property IniPath             : String  Read F_IniPath             Write F_Set_IniPath;
    Property LoginTimeout        : Integer Read F_LoginTimeout        Write F_Set_LoginTimeout;
    Property PrivateEngine       : Boolean Read F_PrivateEngine       Write F_Set_PrivateEngine;
    Property SystemDatabase      : String  Read F_SystemDB            Write F_Set_SystemDatabase;
    Property UsesDynaDao         : Boolean Read F_UsesDynaDao         Write F_Set_UsesDynaDao;
    Property Version             : String  Read F_DaoVersion          Write F_Set_DaoVersion;
    Property VersionDetails      : String  Read F_ActualDaoVersion    Write F_Set_ActualDaoVersion;
    Property VersionInfo         : String  Read F_VersionInfo         Write F_Set_VersionInfo;
    Property ConnectedWorkspaces : Integer Read F_ConnectedWorkspaces Write F_Set_ConnectedWorkspaces;
    Property Connected           : Boolean Read F_Connected           Write F_Set_Connected;
  end;

procedure Register;

implementation
Uses ActiveX, Registry, KDaoWorkspace, KDaoDatabase;

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

Constructor TKADaoDBEngine.Create(AOwner : TComponent);
Var
 OLE_INIT : Integer;
 X        : Integer;
 Prop     : Pointer;
Begin
 Inherited Create(AOwner);
 OLE_INIT  := CoInitialize(NIL);
 if (OLE_INIT = S_OK) or (OLE_INIT = S_FALSE) then F_OLE_ON:= True;
 F_ComponentVersion     := '10.00';
 F_Destroyng            := False;
 {$IFDEF DYNADAO}
 CoreDBEngine           := Unassigned;
 {$ELSE}
 CoreDBEngine           := Nil;
 {$ENDIF}
 F_RuntimeLicense       := '';
 F_DefaultPassword      := '';
 F_DefaultUser          := 'Admin';
 F_EngineType           := DaoApi.dbUseJet;
 F_PrivateEngine        := False;
 F_ConnectedWorkspaces  := 0;
 {$IFDEF DYNADAO}
 F_UsesDynaDao          := True;
 {$ELSE}
 F_UsesDynaDao          := False;
 {$ENDIF}
 F_DaoVersionList       := TStringList.Create;
 F_DaoVersionList.Clear;
 F_Workspaces           := TStringList.Create;
 F_Workspaces.Clear;
 For X := 0 To Owner.ComponentCount-1 do
      Begin
       Prop := GetPropInfo(Owner.Components[X].ClassInfo, 'DaoLicence');
       if Prop <> Nil Then
           Begin
             F_RuntimeLicense := GetStrProp(Owner.Components[X], Prop);
             Break;
           End;
      End;
 CheckEngines;
 {$IFDEF DYNADAO}
 if F_DaoVersionList.Count > 0 Then
    Begin
      if F_DaoVersionList.Strings[0]='3.5'  Then F_DaoVersion := '3.5';
      if F_DaoVersionList.Strings[0]='3.6'  Then F_DaoVersion := '3.6';
      if F_DaoVersionList.Strings[0]='12.0' Then F_DaoVersion := '12.0';
    End
  Else
     Begin
       DatabaseError(E1004);
     End;
 {$ENDIF}
 {$IFDEF DAO35}
 F_DaoVersion               := '3.5';
 {$ENDIF}
 {$IFDEF DAO36}
 F_DaoVersion               := '3.6';
 {$ENDIF}
 {$IFDEF DAO120}
 F_DaoVersion               := '12.0';
 {$ENDIF}
 F_Connected            := False;
 CreateDBEngine(F_DaoVersion);
 F_IniPath      := CoreDBEngine.IniPath;
 F_EngineType   := CoreDBEngine.DefaultType;
 F_SystemDB     := CoreDBEngine.SystemDB;
 F_LoginTimeout := CoreDBEngine.LoginTimeout;
 RecreateCore;
End;

Destructor  TKADaoDBEngine.Destroy;
Begin
 F_Destroyng := True;
 Connected   := False;
 ReleaseCore;
 F_DaoVersionList.Free;
 F_Workspaces.Free;
 if F_OLE_ON Then CoUninitialize;
 F_OLE_ON:=False;
 Inherited Destroy;
End;

Procedure TKADaoDBEngine.Attach(Name : String; Value : TComponent);
Begin
  F_Workspaces.AddObject(Name,TObject(Value));
  Inc(F_ConnectedWorkspaces);
End;

Procedure TKADaoDBEngine.Detach(Name : String; Value : TComponent);
Var
  I : Integer;
Begin
  I := F_Workspaces.IndexOf(Name);
  if I <> -1 Then
     Begin
       F_Workspaces.Delete(I);
       Dec(F_ConnectedWorkspaces);
     End;
End;



{$IFDEF DYNADAO}
Function TKADaoDBEngine.CreateOleDBEngine(const ClassName: string): IDispatch;
{$ELSE}
Function TKADaoDBEngine.CreateOleDBEngine(const ClassID: TGUID): DBEngine;
{$ENDIF}
Const
  DBEngine_TGUID: TGUID = '{00000021-0000-0010-8000-00AA006D2EA4}';
Var
  LicenseClass       : IClassFactory2;
  DWReserved         : DWORD;
  LicenseString      : Widestring;
{$IFDEF DYNADAO}
  ClassID : TGUID;
Begin
  ClassID := ProgIDToClassID(ClassName);
{$ELSE}
Begin
{$ENDIF}
  //****************************************************************************
  LicenseClass := Nil;
  OleCheck(CoGetClassObject(ClassID,CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, nil, IClassFactory2, LicenseClass));
  if Assigned(LicenseClass) Then
     Begin
       SetLength(LicenseString,2000);
       DWReserved:=0;
       if F_RuntimeLicense <> '' Then
          LicenseString := F_RuntimeLicense
       Else
          LicenseClass.RequestLicKey(DWReserved,LicenseString);
       OleCheck(LicenseClass.CreateInstanceLic (nil, nil, DBEngine_TGUID, LicenseString, Result));
     End;
  //****************************************************************************
End;

Function TKADaoDBEngine.CreateOleDBEngine_II(const ClassName: string): IDispatch;
Const
  DBEngine_TGUID: TGUID = '{00000021-0000-0010-8000-00AA006D2EA4}';
Var
  LicenseClass       : IClassFactory2;
  DWReserved         : DWORD;
  LicenseString      : Widestring;
  ClassID : TGUID;
Begin
  ClassID := ProgIDToClassID(ClassName);
  //****************************************************************************
  LicenseClass := Nil;
  OleCheck(CoGetClassObject(ClassID,CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, nil, IClassFactory2, LicenseClass));
  if Assigned(LicenseClass) Then
     Begin
       SetLength(LicenseString,2000);
       DWReserved:=0;
       if F_RuntimeLicense <> '' Then
          LicenseString := F_RuntimeLicense
       Else
          LicenseClass.RequestLicKey(DWReserved,LicenseString);
       OleCheck(LicenseClass.CreateInstanceLic (nil, nil, DBEngine_TGUID, LicenseString, Result));
     End;
  //****************************************************************************
End;

Procedure TKADaoDBEngine.CheckEngines;
Var
 V35               : String;
 V36               : String;
 V120              : String;
 Reg               : TRegistry;
 S                 : String;
 TempDBEngine      : OleVariant;
Begin
  if F_PrivateEngine Then
    Begin
     V35  := 'DAO.PrivateDBEngine.35';
     V36  := 'DAO.PrivateDBEngine.36';
     V120 := 'DAO.PrivateDBEngine.120';
    End
 Else
    Begin
     V35  := 'DAO.DBEngine.35';
     V36  := 'DAO.DBEngine.36';
     V120 := 'DAO.DBEngine.120';
    End;

  Reg := TRegistry.Create;  
  {$IFDEF VER130} Reg.Access:=KEY_READ; {$ENDIF}
  Reg.RootKey := HKEY_CLASSES_ROOT;
  {$IFNDEF D4UP}
  if Reg.OpenKey(V35,False) then
  {$ELSE}
  if Reg.OpenKeyReadOnly(V35) then
  {$ENDIF}
     Begin
       Reg.CloseKey;
       Try
        TempDBEngine               := CreateOleDBEngine_II(V35);
        VarClear(TempDBEngine);
        F_DaoVersionList.Add('3.5');
       Except
         on E:Exception do
            Begin
              S:=E.Message;
              if Pos('80040112',S) > 0 Then
                 Begin
                   Reg.CloseKey;
                   Reg.Free;
                   DatabaseError(E1001);
                 End;
            End;
       End;
     End;
  Reg.Free;
   
  Reg := TRegistry.Create;
  {$IFDEF VER130} Reg.Access:=KEY_READ; {$ENDIF}
  Reg.RootKey := HKEY_CLASSES_ROOT;
  {$IFNDEF D4UP}
  if Reg.OpenKey(V36,False) then
  {$ELSE}
  if Reg.OpenKeyReadOnly(V36) then
  {$ENDIF}
     Begin
       Reg.CloseKey;
       Try
        TempDBEngine               := CreateOleDBEngine_II(V36);
        VarClear(TempDBEngine);
        F_DaoVersionList.Add('3.6');
       Except
         on E:Exception do
            Begin
              S:=E.Message;
              if Pos('80040112',S) > 0 Then
                 Begin
                   Reg.CloseKey;
                   Reg.Free;
                   DatabaseError(E1001);
                 End;
            End;
       End;
     End;
  Reg.Free;

  Reg := TRegistry.Create;
  {$IFDEF VER130} Reg.Access:=KEY_READ; {$ENDIF}
  Reg.RootKey := HKEY_CLASSES_ROOT;
  {$IFNDEF D4UP}
  if Reg.OpenKey(V120,False) then
  {$ELSE}
  if Reg.OpenKeyReadOnly(V120) then
  {$ENDIF}
     Begin
       Reg.CloseKey;
       Try
        TempDBEngine               := CreateOleDBEngine_II(V120);
        VarClear(TempDBEngine);
        F_DaoVersionList.Add('12.0');
       Except
         on E:Exception do
            Begin
              S:=E.Message;
              if Pos('80040112',S) > 0 Then
                 Begin
                   Reg.CloseKey;
                   Reg.Free;
                   DatabaseError(E1001);
                 End;
            End;
       End;
     End;
  Reg.Free;
  If (Not VarIsNull(TempDBEngine)) And (Not VarIsEmpty(TempDBEngine)) Then VarClear(TempDBEngine);
End;

//*************************************************************************************************
Procedure TKADaoDBEngine.CreateDBEngine(DaoVer:String);
Var
  V35  : String;
  V36  : String;
  V120 : String;
Begin
 if F_PrivateEngine Then
    Begin
     V35  := 'DAO.PrivateDBEngine.35';
     V36  := 'DAO.PrivateDBEngine.36';
     V120 := 'DAO.PrivateDBEngine.120';
    End
 Else
    Begin
     V35  := 'DAO.DBEngine.35';
     V36  := 'DAO.DBEngine.36';
     V120 := 'DAO.DBEngine.120';
    End;

 {$IFDEF DYNADAO}
  if DaoVer='3.5' Then
     Begin
       Try
         CoreDBEngine               := CreateOleDBEngine(V35);
         F_DaoVersion               := '3.5';
       Except
         Try
           CoreDBEngine             := CreateOleDBEngine(V36);
           F_DaoVersion             := '3.6';
         Except
           Try
             CoreDBEngine           := CreateOleDBEngine(V120);
             F_DaoVersion           := '12.0';
           Except
             DatabaseError(E1002);
           End;
         End;
       End;
   End;

  if DaoVer='3.6' Then
     Begin
       Try
        CoreDBEngine             := CreateOleDBEngine(V36);
        F_DaoVersion             := '3.6';
       Except
         Try
           CoreDBEngine          := CreateOleDBEngine(V120);
           F_DaoVersion          := '12.0';
         Except
           DatabaseError(E1002);
         End;
       End;
   End;

  if DaoVer='12.0' Then
     Begin
       Try
         CoreDBEngine             := CreateOleDBEngine(V120);
         F_DaoVersion             := '12.0';
       Except
         DatabaseError(E1002);
       End;
   End;

  {$ELSE}
  F_DynaDao := False;
  CoreDBEngine               := Nil;
  Try
    if F_PrivateEngine Then
       CoreDBEngine          := CreateOleDBEngine(Class_PrivDBEngine)
    Else
       CoreDBEngine          := CreateOleDBEngine(Class_DBEngine);
  Except
    on E:Exception do
       Begin
         if Pos('80040112',E.Message) > 0 Then
            Begin
              DatabaseError(E1001);
            End
          Else DatabaseError(E.Message);
       End;
  End;
  {$ENDIF}
End;

Function TKADaoDBEngine.GetSystemDatabaseFromRegistry:String;
Var
  RS   : String;
  Reg : TRegistry;
Begin
  Result                         := '';
  RS                             := 'SOFTWARE\Microsoft\JET\3.5\Engines';
  if F_DaoVersion='3.5'  Then RS := 'SOFTWARE\Microsoft\JET\3.5\Engines';
  if F_DaoVersion='3.6'  Then RS := 'SOFTWARE\Microsoft\JET\4.0\Engines';
  if F_DaoVersion='12.0' Then RS := 'SOFTWARE\Microsoft\Office\12.0\Access Connectivity Engine\Engines';
  Reg := TRegistry.Create;
  {$IFDEF VER130} Reg.Access:=KEY_READ; {$ENDIF}
  Try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    {$IFNDEF D4UP}
    if Reg.OpenKey(RS,False) then
    {$ELSE}
    if Reg.OpenKeyReadOnly(RS) then
    {$ENDIF}
       Begin
         Result:=Reg.ReadString('SystemDB');
         Reg.CloseKey;
       End;
  Finally
    Reg.Free;
  End;
End;

Procedure TKADaoDBEngine.ReleaseCore;
Begin
  {$IFDEF DYNADAO}
  If (Not VarIsNull(CoreDBEngine))  And (Not VarIsEmpty(CoreDBEngine))  Then VarClear(CoreDBEngine);
  {$ELSE}
  CoreDBEngine  := Nil;
  {$ENDIF}
  if F_OLE_ON Then CoUninitialize;
  F_OLE_ON:=False;
End;

Procedure TKADaoDBEngine.RecreateCore;
Var
  OLE_INIT     : Integer;
  TempPrivate  : Boolean;
Begin
   ReleaseCore;
   OLE_INIT:= CoInitialize(NIL);
   if (OLE_INIT = S_OK) or (OLE_INIT = S_FALSE) then F_OLE_ON:= True
   Else DatabaseError(E1003);
   //*************************************************** Borland, Microsoft ...
   TempPrivate:=True;
   if (csDesigning in ComponentState) And (F_EngineType=dbUseJet) Then
      Begin
        TempPrivate      := F_PrivateEngine;
        F_PrivateEngine  := True;
      End;
   CreateDBEngine(F_DaoVersion);
   if (csDesigning in ComponentState) And (F_EngineType=dbUseJet) Then F_PrivateEngine  := TempPrivate;
   //***************************************************************************
   CoreDBEngine.SystemDB                                   := F_SystemDB;
   CoreDBEngine.DefaultUser                                := F_DefaultUser;
   CoreDBEngine.DefaultPassword                            := F_DefaultPassword;
   CoreDBEngine.IniPath                                    := F_IniPath;
   CoreDBEngine.LoginTimeout                               := F_LoginTimeout;
   F_ActualDaoVersion                                      := CoreDBEngine.Version;
   if Pos( '3.5',F_ActualDaoVersion)=1  Then F_VersionInfo := '(In Access''97 mode)';
   if Pos( '3.6',F_ActualDaoVersion)=1  Then F_VersionInfo := '(In Access''2000 mode)';
   if Pos('12.0',F_ActualDaoVersion)=1  Then F_VersionInfo := '(In Access''2007 mode)';
End;


Procedure TKADaoDBEngine.Loaded;
begin
  try
    inherited Loaded;
    if F_Connected Then RecreateCore Else ReleaseCore;
  except
    Application.HandleException(Self)
  end;
end;

Procedure TKADaoDBEngine.F_Set_ComponentVersion(Value: String);
Begin
  //******************************************************************* ReadOnly
End;

Procedure TKADaoDBEngine.F_Set_DefaultPassword(Value : String);
Begin
  if F_Connected Then DatabaseError(E1033);
  F_DefaultPassword:=Value;
  if csLoading in ComponentState Then Exit;
  RecreateCore;
  ReleaseCore;
End;

Procedure TKADaoDBEngine.F_Set_DefaultUser(Value : String);
Begin
  if F_Connected Then DatabaseError(E1033);
  F_DefaultUser:=Value;
  if csLoading in ComponentState Then Exit;
  RecreateCore;
  ReleaseCore;
End;

Procedure TKADaoDBEngine.F_Set_IniPath(Value : String);
Begin
  if F_Connected Then DatabaseError(E1033);
  F_IniPath:=Value;
  if csLoading in ComponentState Then Exit;
  RecreateCore;
  ReleaseCore;
End;

Procedure TKADaoDBEngine.F_Set_LoginTimeout(Value : Integer);
Begin
  if F_Connected Then DatabaseError(E1033);
  F_LoginTimeout:=Value;
  if csLoading in ComponentState Then Exit;
  RecreateCore;
  ReleaseCore;
End;

Procedure TKADaoDBEngine.F_Set_EngineType(Value : Integer);
Begin
  if F_Connected Then DatabaseError(E1033);
  F_EngineType:=Value;
  if csLoading in ComponentState Then Exit;
  RecreateCore;
  ReleaseCore;
End;

Procedure TKADaoDBEngine.F_Set_PrivateEngine(Value : Boolean);
Begin
  if F_Connected Then DatabaseError(E1033);
  F_PrivateEngine:=Value;
  if csLoading in ComponentState Then Exit;
  RecreateCore;
  ReleaseCore;
End;

Procedure TKADaoDBEngine.F_Set_SystemDatabase(Value : String);
Begin
  if F_Connected Then DatabaseError(E1033);
  F_SystemDB:=Value;
  if csLoading in ComponentState Then Exit;
  RecreateCore;
  ReleaseCore;
End;

Procedure TKADaoDBEngine.F_Set_UsesDynaDao(Value : Boolean);
Begin
 //******************************************************************** ReadOnly
End;

Procedure TKADaoDBEngine.F_Set_DaoVersion(Value : String);
Begin
  if NOT F_UsesDynaDao Then Exit;
  if F_Connected Then DatabaseError(E1033);
  F_DaoVersion:=Value;
  if csLoading in ComponentState Then Exit;
  RecreateCore;
  ReleaseCore;
End;

Procedure TKADaoDBEngine.F_Set_ActualDaoVersion(Value : String);
Begin
  //******************************************************************* ReadOnly
End;

Procedure TKADaoDBEngine.F_Set_VersionInfo(Value : String);
Begin
  //******************************************************************* ReadOnly
End;

Procedure TKADaoDBEngine.F_Set_ConnectedWorkspaces(Value : Integer);
Begin
  //******************************************************************* ReadOnly
End;

Procedure TKADaoDBEngine.F_Set_Connected(Value : Boolean);
Var
  X : Integer;
Begin
  if Not Value Then
     Begin
        For X := 0 To F_Workspaces.Count-1 do
            Begin
              (F_Workspaces.Objects[X] as TKAdaoWorkspace).Connected := False;
              if F_Destroyng Then (F_Workspaces.Objects[X] as TKAdaoWorkspace).DaoDbEngine := Nil;
            End;
     End;
  if Value Then RecreateCore Else ReleaseCore;
  F_Connected := Value;
  if csLoading in ComponentState Then Exit;
End;

Procedure TKADaoDBEngine.Open;
Begin
  Connected := True;
End;

Procedure TKADaoDBEngine.Close;
Begin
  Connected := False;
End;

Procedure TKADaoDBEngine.StartTransaction;
Begin
  if (NOT F_Connected) Then
     Begin
       DatabaseError(E1023);
       Exit;
     End;
  CoreDBEngine.BeginTrans;
End;

Procedure TKADaoDBEngine.Commit;
Begin
 if (NOT F_Connected) Then
     Begin
       DatabaseError(E1024);
       Exit;
     End;
 CoreDBEngine.CommitTrans(dbForceOSFlush);
End;

Procedure TKADaoDBEngine.Rollback;
Var
  X       : Integer;
  ATable  : TKADaoWorkspace;
Begin
 CoreDBEngine.Rollback;
 For X :=0 To F_Workspaces.Count-1 do
     Begin
      ATable:=TKADaoWorkspace(F_Workspaces.Objects[X]);
      ATable.RollbackRefresh;
     End;
End;

//********************************************** WORKS ONLY ON DAO 3.5X
//                                              ON DAO 3.6 USE COMPACT DATABASE
//                                              WICH ALSO DOES REPAIR
//******************************************************************************
Procedure TKADaoDBEngine.RepairAccessDatabase(DatabaseName,Password:String);
Begin
  if F_DaoVersion='3.5' Then
     CoreDBEngine.RepairDatabase(DatabaseName)
  Else
     CompactAccessDatabase(DatabaseName,Password);
End;

Procedure TKADaoDBEngine.RepairAccessDatabaseEx(DatabaseName : String;
                                               NewLocale    : String;
                                               Encrypt      : Boolean;
                                               Decrypt      : Boolean;
                                               NewVersion   : Integer;
                                               Password     : String);
Begin
  if F_DaoVersion = '3.5' Then
     CoreDBEngine.RepairDatabase(DatabaseName)
  Else
     CompactAccessDatabaseEx(DatabaseName,NewLocale,Encrypt,Decrypt,NewVersion,Password);
End;

Procedure  TKADaoDBEngine.CompactAccessDatabase(DatabaseName,Password:String);
Var
  TempName : Array[0..1000] of Char;
  TempPath : String;
  Name     : String;
Begin
  TempPath:=ExtractFilePath(DatabaseName);
  if TempPath='' Then TempPath:=GetCurrentDir;
  GetTempFileName(PChar(TempPath),'mdb',0,TempName);
  Name:=StrPas(TempName);
  DeleteFile(Name);
  if Password <> '' Then Password:=';pwd='+Password;
  OleVariant(CoreDBEngine).CompactDatabase(DatabaseName,Name,,,Password);
  DeleteFile(DatabaseName);
  RenameFile(Name,DatabaseName);
End;

Procedure  TKADaoDBEngine.CompactAccessDatabaseEx(DatabaseName: String;
                                                  NewLocale   : String;
                                                  Encrypt     : Boolean;
                                                  Decrypt     : Boolean;
                                                  NewVersion  : Integer;
                                                  Password    : String);
Var
  TempName : Array[0..1000] of Char;
  TempPath : String;
  Name     : String;
  Options  : Integer;
Begin
  TempPath:=ExtractFilePath(DatabaseName);
  if TempPath='' Then TempPath:=GetCurrentDir;
  GetTempFileName(PChar(TempPath),'mdb',0,TempName);
  Name:=StrPas(TempName);
  DeleteFile(Name);
  Options:=0;
  if Encrypt Then Options := dbEncrypt;
  if Decrypt Then Options := dbDecrypt;
  if NewVersion <> 0 Then Options:=Options+NewVersion;
  if Password <> '' Then Password:=';pwd='+Password;
  CoreDBEngine.CompactDatabase(DatabaseName,Name,NewLocale,Options,Password);
  DeleteFile(DatabaseName);
  RenameFile(Name,DatabaseName);
End;

Function TKADaoDBEngine.RegisterDatabase(DatabaseName, DriverName:String; Silent:Boolean; Attributes:String):Boolean;
Begin
  Result := False;
  Try
    CoreDBEngine.RegisterDatabase(DatabaseName,DriverName,Silent,Attributes);
  Except
   Exit;
  End;
  Result := True;
End;

Procedure TKADaoDBEngine.Idle;
Begin
 CoreDBEngine.Idle(dbRefreshCache);
End;                                                

procedure Register;
begin
  RegisterComponents('KA Dao', [TKADaoDBEngine]);
end;

Initialization
 {$IFNDEF D5UP}
  TVarData(Unassigned).VType := varEmpty;
  TVarData(EmptyParam).VType := varError;
  TVarData(EmptyParam).VError := $80020004;
 {$ENDIF}
end.
