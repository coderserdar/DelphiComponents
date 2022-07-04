{$B-}
unit KADaoInfo;
//******************************************************************************
//                         Delphi Dao Project
//                 Copyright (c) 2000 by Kiril Antonov
//******************************************************************************
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TInfoCheckEvent = Procedure of object;
  TKADaoInfo = class(TComponent)
  private
    { Private declarations }
    LU  : Array[0..255,0..63] of Char;
  protected
    { Protected declarations }
    F_Active             : Boolean;
    F_Database           : String;
    F_DaoDll             : String;
    F_DatabaseVersion    : Integer;
    F_NumberOfUsers      : Integer;
    F_LoggedUsers        : TStringList;
    F_LoggedInfo         : TStringList;
    F_LoggedNowUsers     : TStringList;
    F_ErrorUsers         : TStringList;
    F_LastError          : Integer;
    F_LastErrorText      : String;

    F_AfterGetDatabaseVersion : TInfoCheckEvent;
    F_AfterGetNumberOfUsers   : TInfoCheckEvent;
    F_AfterGetLoggedUsers     : TInfoCheckEvent;
    F_AfterGetLoggedUsersEx   : TInfoCheckEvent;
    F_AfterGetLoggedNowUsers  : TInfoCheckEvent;
    F_AfterGetErrorUsers      : TInfoCheckEvent;

    Procedure F_Set_Active(Value:Boolean);

    Function  F_GET_LoggedUsers : TStringList;
    Procedure F_SET_LoggedUsers(Value:TStringList);

    Function  F_GET_LoggedUsersEx : TStringList;
    Procedure F_SET_LoggedUsersEx(Value:TStringList);

    Function  F_GET_LoggedNowUsers : TStringList;
    Procedure F_SET_LoggedNowUsers(Value:TStringList);

    Function  F_GET_ErrorUsers : TStringList;
    Procedure F_SET_ErrorUsers(Value:TStringList);

    Function  F_GET_DatabaseVersion :Integer;
    Procedure F_SET_DatabaseVersion (Value:Integer);

    Function  F_GET_NumberOfUsers :Integer;
    Procedure F_SET_NumberOfUsers (Value:Integer);

    Procedure F_SET_LastError(Value:Integer);
    Procedure F_SET_LastErrorText(Value:String);

  public
    { Public declarations }
    Constructor   Create(AOwner: TComponent); override;
    Destructor    Destroy; override;

    Function KAGetDatabaseVersion(LibraryName,DatabasePath:String):Integer;
    Function KAGetNumberOfUsers(LibraryName,DatabasePath:String):Integer;
    Function KAGetLoggedUsers(LibraryName,DatabasePath:String):Integer;
    Function KAGetLoggedNowUsers(LibraryName,DatabasePath:String):Integer;
    Function KAGetErrorUsers(LibraryName,DatabasePath:String):Integer;
    Function KAGetLoggedInfo(DatabasePath:String):Boolean;

  published
    { Published declarations }
    Property Database           : String        Read F_Database             Write F_Database;
    Property DaoInfoDll         : String        Read F_DaoDll               Write F_DaoDll;
    Property DatabaseVersion    : Integer       Read F_GET_DatabaseVersion  Write F_SET_DatabaseVersion;
    Property NumberOfUsers      : Integer       Read F_GET_NumberOfUsers    Write F_SET_NumberOfUsers;
    Property LoggedUsers        : TStringList   Read F_GET_LoggedUsers      Write F_SET_LoggedUsers;
    Property LoggedUsersEx      : TStringList   Read F_GET_LoggedUsersEx    Write F_SET_LoggedUsersEx;
    Property LoggedNowUsers     : TStringList   Read F_GET_LoggedNowUsers   Write F_SET_LoggedNowUsers;
    Property ErrorUsers         : TStringList   Read F_GET_ErrorUsers       Write F_SET_ErrorUsers;
    Property LastError          : Integer       Read F_LastError            Write F_SET_LastError;
    Property LastErrorText      : String        Read F_LastErrorText        Write F_SET_LastErrorText;
    Property AfterGetDatabaseVersion : TInfoCheckEvent Read F_AfterGetDatabaseVersion Write F_AfterGetDatabaseVersion;
    Property AfterGetNumberOfUsers   : TInfoCheckEvent Read F_AfterGetNumberOfUsers   Write F_AfterGetNumberOfUsers;
    Property AfterGetLoggedUsers     : TInfoCheckEvent Read F_AfterGetLoggedUsers     Write F_AfterGetLoggedUsers;
    Property AfterGetLoggedUsersEx   : TInfoCheckEvent Read F_AfterGetLoggedUsersEx   Write F_AfterGetLoggedUsersEx;
    Property AfterGetLoggedNowUsers  : TInfoCheckEvent Read F_AfterGetLoggedNowUsers  Write F_AfterGetLoggedNowUsers;
    Property AfterGetErrorUsers      : TInfoCheckEvent Read F_AfterGetErrorUsers      Write F_AfterGetErrorUsers;

    Property Active                  : Boolean       Read F_Active                    Write F_Set_Active;
  end;

procedure Register;

implementation
Uses
  ActiveX, DaoUtils;

Constructor TKADaoInfo.Create(AOwner: TComponent);
var
   Tmp : String;
Begin
  Inherited Create(AOwner);
  F_Active             := False;
  F_Database           := '';
  F_DatabaseVersion    := 0;
  F_NumberOfUsers      := 0;
  F_LoggedUsers        := TStringList.Create;
  F_LoggedInfo         := TStringList.Create;
  F_LoggedNowUsers     := TStringList.Create;
  F_ErrorUsers         := TStringList.Create;
  F_LastError          := 0;
  F_LastErrorText      := '';
  GetDir(0,Tmp);
  F_DaoDll             := 'msldbusr.dll';
End;

Destructor TKADaoInfo.Destroy;
Begin
  F_LoggedUsers.Free;
  F_LoggedInfo.Free;
  F_LoggedNowUsers.Free;
  F_ErrorUsers.Free;
  Inherited Destroy;
End;

Procedure TKADaoInfo.F_Set_Active(Value:Boolean);
Begin
 F_Active := Value;
 if F_Active Then
    Begin
      KAGetDatabaseVersion(F_DaoDll,F_Database);
      if Assigned(F_AfterGetDatabaseVersion) Then F_AfterGetDatabaseVersion;
      KAGetNumberOfUsers(F_DaoDll,F_Database);
      if Assigned(F_AfterGetNumberOfUsers) Then F_AfterGetNumberOfUsers;
      KAGetLoggedUsers(F_DaoDll,F_Database);
      if Assigned(F_AfterGetLoggedUsers) Then F_AfterGetLoggedUsers;
      KAGetLoggedNowUsers(F_DaoDll,F_Database);
      if Assigned(F_AfterGetLoggedNowUsers) Then F_AfterGetLoggedNowUsers;
      KAGetErrorUsers(F_DaoDll,F_Database);
      if Assigned(F_AfterGetErrorUsers) Then F_AfterGetErrorUsers;
      KAGetLoggedInfo(F_Database);
      if Assigned(F_AfterGetLoggedUsersEx) Then F_AfterGetLoggedUsersEx;
    End;
End;

Function TKADaoInfo.KAGetDatabaseVersion(LibraryName,DatabasePath:String):Integer;
Var
  LibHandle          : HMODULE;
  GetDatabaseVersion : function (DBPath:PChar): Integer; stdcall;
  GetLastError       : function (ErrorNo:Integer): PChar; stdcall;
Begin
  Result := 0;
  LibHandle:=LoadLibrary(PChar(LibraryName));
  IF LibHandle=0 Then
     Begin
       Result:=-1000;
       F_LastError:=Result;
       F_LastErrorText:='DLL Library not found!';
       Exit;
     End;
  @GetDatabaseVersion:=GetProcAddress(LibHandle,'LDBUser_GetDatabaseVersion');
  @GetLastError:=GetProcAddress(LibHandle,'LDBUser_GetError');
  if @GetDatabaseVersion <> Nil Then
     Begin
       Result:=GetDatabaseVersion(PChar(DatabasePath));
       if Result < 0 Then
          Begin
            F_LastError:=Result;
            F_LastErrorText:=StrPas(GetLastError(F_LastError));
          End;
     End;
  FreeLibrary(LibHandle);
End;

Function TKADaoInfo.KAGetNumberOfUsers(LibraryName,DatabasePath:String):Integer;
Var
  LibHandle          : HMODULE;
  GetNumberOfUsers   : function (UserBuffer:PChar;DBPath:PChar;Options:Integer): Integer; stdcall;
  GetLastError       : function (ErrorNo:Integer): PChar; stdcall;
  UserBuffer         : PChar;
Begin
  Result := 0;
  LibHandle:=LoadLibrary(PChar(LibraryName));
  IF LibHandle=0 Then
     Begin
       Result:=-1000;
       F_LastError:=Result;
       F_LastErrorText:='DLL Library not found!';
       Exit;
     End;
  @GetNumberOfUsers:=GetProcAddress(LibHandle,'LDBUser_GetUsers');
  @GetLastError:=GetProcAddress(LibHandle,'LDBUser_GetError');
  if @GetNumberOfUsers <> Nil Then
     Begin
       UserBuffer:=Nil;
       Result:=GetNumberOfUsers(UserBuffer,PChar(DatabasePath),8);
       if Result < 0 Then
          Begin
            F_LastError:=Result;
            F_LastErrorText:=StrPas(GetLastError(F_LastError));
          End;
     End;
  FreeLibrary(LibHandle);
End;

Function TKADaoInfo.KAGetLoggedUsers(LibraryName,DatabasePath:String):Integer;
Var
  LibHandle          : HMODULE;
  GetLoggedUsers     : function (UserBuffer:PSafeArray;DBPath:PChar;Options:Integer): Integer; stdcall;
  GetLastError       : function (ErrorNo:Integer): PChar; stdcall;
  UserList           : PSafeArray;
  sabUserList        : TSafeArrayBound;
  X                  : Integer;
  HR                 : HResult;
  P                  : PChar;
Begin
  Result := 0;
  F_LoggedUsers.Clear;
  LibHandle:=LoadLibrary(PChar(LibraryName));
  IF LibHandle=0 Then
     Begin
       Result:=-1000;
       F_LastError:=Result;
       F_LastErrorText:='DLL Library not found!';
       Exit;
     End;
  @GetLoggedUsers:=GetProcAddress(LibHandle,'LDBUser_GetUsers');
  @GetLastError:=GetProcAddress(LibHandle,'LDBUser_GetError');
  if @GetLoggedUsers <> Nil Then
     Begin
       F_LastError:=0;
       F_LastErrorText:='';
       sabUserList.lLbound	:= 1;
       sabUserList.cElements	:= 1;
       UserList			:= SafeArrayCreate(VT_BSTR, 1, sabUserList);
       Result:=GetLoggedUsers(@UserList,PChar(DatabasePath),1);
       if Result > 0 Then
          Begin
            For X:=1 To Result do
                Begin
                  HR := SafeArrayGetElement(UserList, X, P);
                  if HR=S_OK Then
                     Begin
                       F_LoggedUsers.Add(StrPas(P));
                     End;
                End;
          End
       Else
       if Result < 0 Then
          Begin
            F_LastError:=Result;
            F_LastErrorText:=StrPas(GetLastError(F_LastError));
          End;
       SafeArrayDestroy(UserList);
     End;
  FreeLibrary(LibHandle);
End;

Function TKADaoInfo.KAGetLoggedNowUsers(LibraryName,DatabasePath:String):Integer;
Var
  LibHandle          : HMODULE;
  GetLoggedNowUsers  : function (UserBuffer:PSafeArray;DBPath:PChar;Options:Integer): Integer; stdcall;
  GetLastError       : function (ErrorNo:Integer): PChar; stdcall;
  UserList           : PSafeArray;
  sabUserList        : TSafeArrayBound;
  X                  : Integer;
  HR                 : HResult;
  P                  : PChar;
Begin
  Result := 0;
  F_LoggedNowUsers.Clear;
  LibHandle:=LoadLibrary(PChar(LibraryName));
  IF LibHandle=0 Then
     Begin
       Result:=-1000;
       F_LastError:=Result;
       F_LastErrorText:='DLL Library not found!';
       Exit;
     End;
  @GetLoggedNowUsers:=GetProcAddress(LibHandle,'LDBUser_GetUsers');
  @GetLastError:=GetProcAddress(LibHandle,'LDBUser_GetError');
  if @GetLoggedNowUsers <> Nil Then
     Begin
       F_LastError:=0;
       F_LastErrorText:='';
       sabUserList.lLbound	:= 1;
       sabUserList.cElements	:= 1;
       UserList			:= SafeArrayCreate(VT_BSTR, 1, sabUserList);
       Result:=GetLoggedNowUsers(@UserList,PChar(DatabasePath),2);
       if Result > 0 Then
          Begin
            For X:=1 To Result do
                Begin
                  HR := SafeArrayGetElement(UserList, X, P);
                  if HR=S_OK Then
                     Begin
                       F_LoggedNowUsers.Add(StrPas(P));
                     End;
                End;
          End
       Else
       if Result < 0 Then
          Begin
            F_LastError:=Result;
            F_LastErrorText:=StrPas(GetLastError(F_LastError));
          End;
       SafeArrayDestroy(UserList);
     End;
  FreeLibrary(LibHandle);
End;

Function TKADaoInfo.KAGetErrorUsers(LibraryName,DatabasePath:String):Integer;
Var
  LibHandle          : HMODULE;
  GetErrorUsers      : function (UserBuffer:PSafeArray;DBPath:PChar;Options:Integer): Integer; stdcall;
  GetLastError       : function (ErrorNo:Integer): PChar; stdcall;
  UserList           : PSafeArray;
  sabUserList        : TSafeArrayBound;
  X                  : Integer;
  HR                 : HResult;
  P                  : PChar;
Begin
  Result := 0;
  F_ErrorUsers.Clear;
  LibHandle:=LoadLibrary(PChar(LibraryName));
  IF LibHandle=0 Then
     Begin
       Result:=-1000;
       F_LastError:=Result;
       F_LastErrorText:='DLL Library not found!';
       Exit;
     End;
  @GetErrorUsers:=GetProcAddress(LibHandle,'LDBUser_GetUsers');
  @GetLastError:=GetProcAddress(LibHandle,'LDBUser_GetError');
  if @GetErrorUsers <> Nil Then
     Begin
       F_LastError:=0;
       F_LastErrorText:='';
       sabUserList.lLbound	:= 1;
       sabUserList.cElements	:= 1;
       UserList			:= SafeArrayCreate(VT_BSTR, 1, sabUserList);
       Result:=GetErrorUsers(@UserList,PChar(DatabasePath),4);
       if Result > 0 Then
          Begin
            For X:=1 To Result do
                Begin
                  HR := SafeArrayGetElement(UserList, X, P);
                  if HR=S_OK Then
                     Begin
                       F_ErrorUsers.Add(StrPas(P));
                     End;
                End;
          End
       Else
       if Result < 0 Then
          Begin
            F_LastError:=Result;
            F_LastErrorText:=StrPas(GetLastError(F_LastError));
          End;
       SafeArrayDestroy(UserList);
     End;
  FreeLibrary(LibHandle);
End;

Function TKADaoInfo.KAGetLoggedInfo(DatabasePath:String):Boolean;
Var
  FP  : String;
  FN  : String;
  S   : String;
  FNA : Array[0..1000] of Char;
  P   : PChar;
  F   : File;
  NR  : Integer;
  X   : Integer;
  I   : Integer;
Begin
  Result := False;
  F_LoggedInfo.Clear;
  if NOT FileExists(DatabasePath) Then Exit;
  FP := ExtractFilePath(DatabasePath);
  FN := ExtractFileName(DatabasePath);
  StrPCopy(FNA,FN);
  P:=StrRScan(FNA,'.');
  if P <> Nil Then P[0]:=#0;
  StrCat(FNA,'.ldb');
  FN  :=FP+StrPas(FNA);
  if NOT FileExists(FN) Then Exit;
  AssignFile(F,FN);
  FileMode := 0;
  Reset(F,1);
  BlockRead(F,LU,SizeOf(LU),NR);
  Close(F);
  if NR=0 Then Exit;
  For X :=0 To (NR DIV 64)-1 do
      Begin
        SetString(S,LU[X],64);
        FP:=Copy(S,1,32);
        Delete(S,1,32);
        I:=Pos(#0,S);
        if I > 0 Then S := Copy(S,1,I-1);
        I:=Pos(#0,FP);
        if I > 0 Then FP := Copy(FP,1,I-1);
        F_LoggedInfo.Add(FP+'/'+S);
      End;
  Result := True;
End;

Function TKADaoInfo.F_GET_LoggedUsers : TStringList;
Begin
  if F_Active Then KAGETLoggedUsers(F_DaoDll,F_Database);
  Result := F_LoggedUsers;
End;

Procedure TKADaoInfo.F_SET_LoggedUsers(Value:TStringList);
Begin
 //****************************************************************** Read Onlly
End;

Function  TKADaoInfo.F_GET_LoggedNowUsers : TStringList;
Begin
  if F_Active Then KAGETLoggedNowUsers(F_DaoDll,F_Database);
  Result := F_LoggedNowUsers;
End;

Procedure TKADaoInfo.F_SET_LoggedNowUsers(Value:TStringList);
Begin
 //****************************************************************** Read Onlly
End;

Function  TKADaoInfo.F_GET_ErrorUsers : TStringList;
Begin
  if F_Active Then KAGETErrorUsers(F_DaoDll,F_Database);
  Result := F_ErrorUsers;
End;

Procedure TKADaoInfo.F_SET_ErrorUsers(Value:TStringList);
Begin
 //****************************************************************** Read Onlly
End;

Function  TKADaoInfo.F_GET_DatabaseVersion :Integer;
Begin
 Result:=0;
 if F_Active Then Result:=KAGETDatabaseVersion(F_DaoDll,F_Database);
 if Result < 0 Then Result:=0;
End;

Procedure TKADaoInfo.F_SET_DatabaseVersion (Value:Integer);
Begin
 //****************************************************************** Read Onlly
End;

Function TKADaoInfo.F_GET_NumberOfUsers :Integer;
Begin
  Result:=0;
  if F_Active Then Result:=KAGETNumberOfUsers(F_DaoDll,F_Database);
  if Result < 0 Then Result:=0;
End;

Procedure  TKADaoInfo.F_SET_NumberOfUsers (Value:Integer);
Begin
 //****************************************************************** Read Onlly
End;

Procedure  TKADaoInfo.F_SET_LastError(Value:Integer);
Begin
 //****************************************************************** Read Onlly
End;

Procedure  TKADaoInfo.F_SET_LastErrorText(Value:String);
Begin
 //****************************************************************** Read Onlly
End;

Function TKADaoInfo.F_GET_LoggedUsersEx:TStringList;
Begin
 Result := F_LoggedInfo;
 if F_Active Then KAGetLoggedInfo(F_Database);
End;

Procedure TKADaoInfo.F_SET_LoggedUsersEx(Value:TStringList);
Begin
 //****************************************************************** Read Onlly
End;

procedure Register;
begin
  RegisterComponents('KA Dao', [TKADaoInfo]);
end;

end.
