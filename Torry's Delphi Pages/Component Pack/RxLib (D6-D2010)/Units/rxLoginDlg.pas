{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit rxLoginDlg;

{$I RX.INC}

interface

uses
  SysUtils, Messages, Classes, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, DB, DBTables, rxDBLists, RxLogin, rxBdeUtils;

type
  TCheckUserNameEvent = function(UsersTable: TTable;
    const UserName, Password: string): Boolean of object;

  TDialogMode = (dmAppLogin, dmDBLogin, dmUnlock);

  TDBLoginDialog = class
  private
    FDialog: TRxLoginForm;
    FMode: TDialogMode;
    FSelectDatabase: Boolean;
    FIniAliasName: string;
    FCheckUserEvent: TCheckUserNameEvent;
    FCheckUnlock: TCheckUnlockEvent;
    FIconDblClick: TNotifyEvent;
    procedure Login(Database: TDatabase; LoginParams: TStrings);
    function GetUserInfo: Boolean;
    function CheckUser(Table: TTable): Boolean;
    function CheckUnlock: Boolean;
    procedure OkBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function ExecuteAppLogin: Boolean;
    function ExecuteDbLogin(LoginParams: TStrings): Boolean;
    function ExecuteUnlock: Boolean;
  public
    Database: TDatabase;
    AttemptNumber: Integer;
    ShowDBName: Boolean;
    UsersTableName: string;
    UserNameField: string;
    MaxPwdLen: Integer;
    LoginName: string;
    IniFileName: string;
    UseRegistry: Boolean;
    constructor Create(DialogMode: TDialogMode; DatabaseSelect: Boolean);
    destructor Destroy; override;
    function Execute(LoginParams: TStrings): Boolean;
    function GetUserName: string;
    function CheckDatabaseChange: Boolean;
    procedure FillParams(LoginParams: TStrings);
    property Mode: TDialogMode read FMode;
    property SelectDatabase: Boolean read FSelectDatabase;
    property OnCheckUnlock: TCheckUnlockEvent read FCheckUnlock write FCheckUnlock;
    property OnCheckUserEvent: TCheckUserNameEvent read FCheckUserEvent write FCheckUserEvent;
    property OnIconDblClick: TNotifyEvent read FIconDblClick write FIconDblClick;
  end;

procedure OnLoginDialog(Database: TDatabase; LoginParams: TStrings;
  AttemptNumber: Integer; ShowDBName: Boolean);

function LoginDialog(Database: TDatabase; AttemptNumber: Integer;
  const UsersTableName, UserNameField: string; MaxPwdLen: Integer;
  CheckUserEvent: TCheckUserNameEvent; IconDblClick: TNotifyEvent;
  var LoginName: string; const IniFileName: string;
  UseRegistry, SelectDatabase: Boolean): Boolean;

function UnlockDialog(const UserName: string; OnUnlock: TCheckUnlockEvent;
  IconDblClick: TNotifyEvent): Boolean;
function UnlockDialogEx(const UserName: string; OnUnlock: TCheckUnlockEvent;
  IconDblClick: TNotifyEvent; MaxPwdLen, AttemptNumber: Integer): Boolean;

implementation

uses
  Windows, Registry, BDE,
  IniFiles, Graphics, Consts,
  rxAppUtils, RxDConst, rxVclUtils, RxConst;

const
  keyLastLoginUserName = 'LastUser';
  keySelectDatabase = 'SelectDatabase'; { dialog never writes this value }
  keyLastAliasName = 'LastAlias';       { used if SelectDatabase = True  }

{ TDBLoginDialog }

constructor TDBLoginDialog.Create(DialogMode: TDialogMode; DatabaseSelect: Boolean);
begin
  inherited Create;
  FMode := DialogMode;
  FSelectDatabase := DatabaseSelect;
  FDialog := CreateLoginDialog((FMode = dmUnlock), FSelectDatabase,
    FormShow, OkBtnClick);
  AttemptNumber := 3;
  ShowDBName := True;
end;

destructor TDBLoginDialog.Destroy;
begin
  FDialog.Free;
  inherited Destroy;
end;

procedure TDBLoginDialog.OkBtnClick(Sender: TObject);
var
  Ok: Boolean;
  SaveLogin: TDatabaseLoginEvent;
  SetCursor: Boolean;
begin
  if FMode = dmUnlock then
  begin
    Ok := False;
    try
      Ok := CheckUnlock;
    except
      Application.HandleException(Self);
    end;
    if Ok then
      FDialog.ModalResult := mrOk
    else
      FDialog.ModalResult := mrCancel;
  end
  else
  if Mode = dmAppLogin then
  begin
    SetCursor := GetCurrentThreadID = MainThreadID;
    SaveLogin := Database.OnLogin;
    try
      try
        if Database.Connected then
          Database.Close; //Polaris
        if FSelectDatabase then
          Database.AliasName := FDialog.CustomCombo.Text;
        Database.OnLogin := Login;
        if SetCursor then
          Screen.Cursor := crHourGlass;
        try
          Database.Open;
        finally
          if SetCursor then
            Screen.Cursor := crDefault;
        end;
      except
        Application.HandleException(Self);
      end;
    finally
      Database.OnLogin := SaveLogin;
    end;
    if Database.Connected then
    try
      if SetCursor then
        Screen.Cursor := crHourGlass;
      Ok := False;
      try
        Ok := GetUserInfo;
      except
        Application.HandleException(Self);
      end;
      if Ok then
        FDialog.ModalResult := mrOk
      else
      begin
        FDialog.ModalResult := mrNone;
        Database.Close;
      end;
    finally
      if SetCursor then
        Screen.Cursor := crDefault;
    end;
  end
  else { dmDBLogin }
    FDialog.ModalResult := mrOk
end;

procedure TDBLoginDialog.FormShow(Sender: TObject);
var
  S: string;
begin
  if (FMode in [dmAppLogin, dmDBLogin]) and FSelectDatabase then
  begin
    with TBDEItems.Create(FDialog) do
    try
      SessionName := Database.SessionName;
      ItemType := bdDatabases;
      FDialog.CustomCombo.Items.Clear;
      Open;
      while not Eof do
      begin
        FDialog.CustomCombo.Items.Add(FieldByName('NAME').AsString);
        Next;
      end;
      if FIniAliasName = '' then
        S := Database.AliasName
      else
        S := FIniAliasName;
      with FDialog.CustomCombo do
        ItemIndex := Items.IndexOf(S);
    finally
      Free;
    end;
  end;
end;

function TDBLoginDialog.ExecuteAppLogin: Boolean;
var
  Ini: TObject;
begin
  try
    if UseRegistry then
    begin
      Ini := TRegIniFile.Create(IniFileName);
{$IFDEF RX_D5}
      TRegIniFile(Ini).Access := KEY_READ;
{$ENDIF}
    end
    else
      Ini := TIniFile.Create(IniFileName);
    try
      FDialog.UserNameEdit.Text := IniReadString(Ini, FDialog.ClassName,
        keyLastLoginUserName, LoginName);
      FSelectDatabase := IniReadBool(Ini, FDialog.ClassName,
        keySelectDatabase, FSelectDatabase);
      FIniAliasName := IniReadString(Ini, FDialog.ClassName, keyLastAliasName, '');
    finally
      Ini.Free;
    end;
  except
    IniFileName := '';
  end;
  FDialog.SelectDatabase := SelectDatabase;
  Result := (FDialog.ShowModal = mrOk);
  Database.OnLogin := nil;
  if Result then
  begin
    LoginName := GetUserName;
    if IniFileName <> '' then
    begin
      if UseRegistry then
        Ini := TRegIniFile.Create(IniFileName)
      else
        Ini := TIniFile.Create(IniFileName);
      try
        IniWriteString(Ini, FDialog.ClassName, keyLastLoginUserName, GetUserName);
        IniWriteString(Ini, FDialog.ClassName, keyLastAliasName, Database.AliasName);
      finally
        Ini.Free;
      end;
    end;
  end;
end;

function TDBLoginDialog.ExecuteDbLogin(LoginParams: TStrings): Boolean;
var
  CurrSession: TSession;
begin
  Result := False;
  if (Database = nil) or not Assigned(LoginParams) then
    Exit;
  if ShowDBName then
    FDialog.AppTitleLabel.Caption := FmtLoadStr(SDatabaseName,
      [Database.DatabaseName]);
  FDialog.UserNameEdit.Text := LoginParams.Values[szUSERNAME];
  CurrSession := Sessions.CurrentSession;
  try
    Result := FDialog.ShowModal = mrOk;
    if Result then
      FillParams(LoginParams)
    else
      SysUtils.Abort;
  finally
    Sessions.CurrentSession := CurrSession;
  end;
end;

function TDBLoginDialog.ExecuteUnlock: Boolean;
begin
  with FDialog.UserNameEdit do
  begin
    Text := LoginName;
    ReadOnly := True;
    Font.Color := clGrayText;
  end;
  Result := (FDialog.ShowModal = mrOk);
end;

function TDBLoginDialog.Execute(LoginParams: TStrings): Boolean;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crDefault;
  try
    if Assigned(FIconDblClick) then
    begin
      with FDialog.AppIcon do
      begin
        OnDblClick := OnIconDblClick;
        Cursor := crHand;
      end;
      with FDialog.KeyImage do
      begin
        OnDblClick := OnIconDblClick;
        Cursor := crHand;
      end;
    end;
    FDialog.PasswordEdit.MaxLength := MaxPwdLen;
    FDialog.AttemptNumber := AttemptNumber;
    case FMode of
      dmAppLogin: Result := ExecuteAppLogin;
      dmDBLogin: Result := ExecuteDbLogin(LoginParams);
      dmUnlock: Result := ExecuteUnlock;
    else
      Result := False;
    end;
    if Result then
      LoginName := GetUserName;
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

function TDBLoginDialog.GetUserName: string;
begin
  if CheckDatabaseChange then
    Result := Copy(FDialog.UserNameEdit.Text, 1,
      Pos('@', FDialog.UserNameEdit.Text) - 1)
  else
    Result := FDialog.UserNameEdit.Text;
end;

function TDBLoginDialog.CheckDatabaseChange: Boolean;
begin
  Result := (FMode in [dmAppLogin, dmDBLogin]) and
    (Pos('@', Fdialog.UserNameEdit.Text) > 0) and
    ((Database <> nil) and (Database.DriverName <> '') and
    (CompareText(Database.DriverName, szCFGDBSTANDARD) <> 0));
end;

procedure TDBLoginDialog.FillParams(LoginParams: TStrings);
begin
  LoginParams.Values[szUSERNAME] := GetUserName;
  LoginParams.Values['PASSWORD'] := FDialog.PasswordEdit.Text;
  if CheckDatabaseChange then
    LoginParams.Values[szSERVERNAME] := Copy(FDialog.UserNameEdit.Text,
      Pos('@', FDialog.UserNameEdit.Text) + 1, MaxInt);
end;

procedure TDBLoginDialog.Login(Database: TDatabase; LoginParams: TStrings);
begin
  FillParams(LoginParams);
end;

function TDBLoginDialog.GetUserInfo: Boolean;
var
  Table: TTable;
begin
  if UsersTableName = '' then
    Result := CheckUser(nil)
  else
  begin
    Result := False;
//    Table := TTable.Create(Database);
    Table := TTable.Create(Application);
    try
      try
        Table.DatabaseName := Database.DatabaseName;
        Table.SessionName := Database.SessionName;
        Table.TableName := UsersTableName;
        Table.IndexFieldNames := UserNameField;
        Table.Open;
        if Table.FindKey([GetUserName]) then
        begin
          Result := CheckUser(Table);
          if not Result then
            raise EDatabaseError.Create(LoadStr(SInvalidUserName));
        end
        else
          raise EDatabaseError.Create(LoadStr(SInvalidUserName));
      except
        Application.HandleException(Self);
      end;
    finally
      Table.Free;
    end;
  end;
end;

function TDBLoginDialog.CheckUser(Table: TTable): Boolean;
begin
  if Assigned(FCheckUserEvent) then
    Result := FCheckUserEvent(Table, GetUserName, FDialog.PasswordEdit.Text)
  else
    Result := True;
end;

function TDBLoginDialog.CheckUnlock: Boolean;
begin
  if Assigned(FCheckUnlock) then
    Result := FCheckUnlock(FDialog.PasswordEdit.Text)
  else
    Result := True;
end;

{ Utility routines }

procedure OnLoginDialog(Database: TDatabase; LoginParams: TStrings;
  AttemptNumber: Integer; ShowDBName: Boolean);
var
  Dlg: TDBLoginDialog;
begin
  Dlg := TDBLoginDialog.Create(dmDBLogin, False);
  try
    Dlg.Database := Database;
    Dlg.ShowDBName := ShowDBName;
    Dlg.AttemptNumber := AttemptNumber;
    Dlg.Execute(LoginParams);
  finally
    Dlg.Free;
  end;
end;

function UnlockDialogEx(const UserName: string; OnUnlock: TCheckUnlockEvent;
  IconDblClick: TNotifyEvent; MaxPwdLen, AttemptNumber: Integer): Boolean;
var
  Dlg: TDBLoginDialog;
begin
  Dlg := TDBLoginDialog.Create(dmUnlock, False);
  try
    Dlg.LoginName := UserName;
    Dlg.OnIconDblClick := IconDblClick;
    Dlg.OnCheckUnlock := OnUnlock;
    Dlg.MaxPwdLen := MaxPwdLen;
    Dlg.AttemptNumber := AttemptNumber;
    Result := Dlg.Execute(nil);
  finally
    Dlg.Free;
  end;
end;

function UnlockDialog(const UserName: string; OnUnlock: TCheckUnlockEvent;
  IconDblClick: TNotifyEvent): Boolean;
begin
  Result := UnlockDialogEx(UserName, OnUnlock, IconDblClick, 0, 1);
end;

function LoginDialog(Database: TDatabase; AttemptNumber: Integer;
  const UsersTableName, UserNameField: string; MaxPwdLen: Integer;
  CheckUserEvent: TCheckUserNameEvent; IconDblClick: TNotifyEvent;
  var LoginName: string; const IniFileName: string;
  UseRegistry, SelectDatabase: Boolean): Boolean;
var
  Dlg: TDBLoginDialog;
begin
  Dlg := TDBLoginDialog.Create(dmAppLogin, SelectDatabase);
  try
    Dlg.LoginName := LoginName;
    Dlg.OnIconDblClick := IconDblClick;
    Dlg.OnCheckUserEvent := CheckUserEvent;
    Dlg.MaxPwdLen := MaxPwdLen;
    Dlg.Database := Database;
    Dlg.AttemptNumber := AttemptNumber;
    Dlg.UsersTableName := UsersTableName;
    Dlg.UserNameField := UserNameField;
    Dlg.IniFileName := IniFileName;
    Dlg.UseRegistry := UseRegistry;
    Result := Dlg.Execute(nil);
    if Result then
      LoginName := Dlg.LoginName;
  finally
    Dlg.Free;
  end;
end;

end.
