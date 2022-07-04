unit UsersListBoxUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, LMAccess, NetUtilsUnit;

type
  TUserInfo = class
    Name: string;
    Comment: string;
    Flags: DWord;
    FullName: string;
    UserId: DWord;
  end;

  TUsersListBox = class(TListBox)
  private
    FServerName: string;
    function GetUser(const Index: Integer): TUserInfo;
    procedure SetServerName(const Value: string);
    { Private declarations }
  protected
    { Protected declarations }
    FInnerList: TStringList;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SuperClear;
    procedure LoadUsers;
    property Users[const Index: Integer]: TUserInfo read GetUser;
  published
    { Published declarations }
    property ServerName: string read FServerName write SetServerName;
  end;

procedure Register;

implementation

uses
  LMApiBuf;

procedure Register;
begin
  RegisterComponents('MLR Sysco', [TUsersListBox]);
end;

{ TUsersListBox }

constructor TUsersListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInnerList := TStringList.Create;
end;

destructor TUsersListBox.Destroy;
begin
  SuperClear;
  inherited Destroy;
end;

function TUsersListBox.GetUser(const Index: Integer): TUserInfo;
var s: string; i: Integer;
begin
  s := Items[Index];
  i := FInnerList.IndexOf(s);
  Result := TUserInfo(FInnerList.Objects[i]);
end;

procedure TUsersListBox.LoadUsers;
var
  Buffer: PNET_DISPLAY_USER;
  Walk: PNET_DISPLAY_USER;
  Res, Count,i :DWord;
  PServerName: PWideChar;
  WideServerName: WideString;
  NewUser: TUserInfo;
begin
  SuperClear;
  Items.BeginUpdate;
  try
    if FServerName = '' then
      PServerName := nil
    else begin
      if Copy(FServerName, 1, 2) <> '\\' then
        FServerName := '\\' + FServerName;
      WideServerName := FServerName;
      PServerName := PWideChar(WideServerName);
    end;
    i := 0;
    repeat
      // Call the NetQueryDisplayInformation function;
      //   specify information level 3 (group account information).
      Res := NetQueryDisplayInformation(PServerName, 1, i, 1000, $FFFFFFFF, Count,
        PByte(Buffer));
      Walk := Buffer;
      if Res <> ERROR_MORE_DATA then
        CheckNet(Res);
      while Count > 0 do begin
        NewUser := TUserInfo.Create;
        NewUser.Name := WideCharToString(Walk^.usri1_name);
        NewUser.Comment := WideCharToString(Walk^.usri1_comment);
        NewUser.FullName := WideCharToString(Walk^.usri1_full_name);
        NewUser.Flags := Walk^.usri1_flags;
        NewUser.UserId := Walk^.usri1_user_id;
        FInnerList.AddObject(NewUser.Name, NewUser);
        Dec(Count);
        Inc(Walk);
      end;
      if Assigned(Buffer) then
        CheckNet(NetApiBufferFree(PByte(Buffer)));
    until Res <> ERROR_MORE_DATA;
    Items.Assign(FInnerList);
  finally
    Items.EndUpdate;
  end;
end;

procedure TUsersListBox.SetServerName(const Value: string);
begin
  FServerName := Value;
end;

procedure TUsersListBox.SuperClear;
var i: Integer;
begin
  if not (csDestroying in ComponentState) then
    Clear;
  for i := 0 to FInnerList.Count - 1 do
    FInnerList.Objects[i].Free;
  FInnerList.Clear;
end;

end.
