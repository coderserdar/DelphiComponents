{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtAutoAppStarter                               }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_GTAutoAppStarter;

interface
uses
  Classes
  ;
type
{------------------------------------------------------------------------------}
  TgtStartUpScope = (
                      susNone
                     ,susCurrentUser
                     ,susAllUsers
                    )
                    ;
{------------------------------------------------------------------------------}
  TgtAutoAppStarter = class(TComponent)
  private
    FEnabled: Boolean;
    FStartUpScope: TgtStartUpScope;
    FApplicationPath: string;
    FApplicationName: string;
    procedure SetEnabled(const Value: Boolean);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Apply;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
    procedure  AutoRunOnce(ApplicationName : string;ApplicationPath : string;StartUpScope : TgtStartUpScope);
  published
    { Published declarations}
    property Enabled         : Boolean           read FEnabled         write SetEnabled;
    property StartUpScope    : TgtStartUpScope   read FStartUpScope    write FStartUpScope;
    property ApplicationPath : string            read FApplicationPath write FApplicationPath;
    property ApplicationName : string            read FApplicationName write FApplicationName;
  end;
{------------------------------------------------------------------------------}

var
  _AutoAppStarter : TgtAutoAppStarter = nil;

function AutoAppStarter : TgtAutoAppStarter;

implementation


uses
   Registry
  ,Windows
  ,SysUtils
  ;

const
  AUTO_RUN_KEY      = '\Software\Microsoft\Windows\CurrentVersion\Run';
  AUTO_RUN_ONCE_KEY = '\Software\Microsoft\Windows\CurrentVersion\RunOnce';



function AutoAppStarter : TgtAutoAppStarter;
begin
  if not Assigned(_AutoAppStarter) then
    _AutoAppStarter := TgtAutoAppStarter.Create(nil);
  Result := _AutoAppStarter;
end;



{ TgtAutoAppStarter }
{------------------------------------------------------------------------------}
constructor TgtAutoAppStarter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStartUpScope := susCurrentUser;
end;
{------------------------------------------------------------------------------}
destructor TgtAutoAppStarter.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtAutoAppStarter.Apply;
var
  R : TRegistry;
begin
  R := TRegistry.Create;
  try
    case FStartUpScope of
      susNone        : Exit;
      susCurrentUser : R.RootKey := HKEY_CURRENT_USER;
      susAllUsers    : R.RootKey := HKEY_LOCAL_MACHINE;
    end;
    if Length(FApplicationPath) = 0 then
    begin
      FApplicationPath := ParamStr(0);
      FApplicationName := ExtractFileName(ParamStr(0));
    end;
    if R.OpenKey(AUTO_RUN_KEY,False) then
    begin
      if FEnabled then
      begin
        if not R.ValueExists(FApplicationName) then
          R.WriteString(FApplicationName,FApplicationPath)
        else
        begin
          if not SameText(FApplicationPath,R.ReadString(FApplicationName)) then
            R.WriteString(FApplicationName,FApplicationPath);
        end;
      end
      else
      begin
        if R.ValueExists(FApplicationName) then
          R.DeleteValue(FApplicationName);
      end;
    end;
  finally
    R.CloseKey;
    R.Free;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtAutoAppStarter.AutoRunOnce(ApplicationName,ApplicationPath: string; StartUpScope: TgtStartUpScope);
var
  R : TRegistry;
begin
  R := TRegistry.Create;
  try
    case StartUpScope of
      susNone        : Exit;
      susCurrentUser : R.RootKey := HKEY_CURRENT_USER;
      susAllUsers    : R.RootKey := HKEY_LOCAL_MACHINE;
    end;
    if R.OpenKey(AUTO_RUN_ONCE_KEY,False) then
    begin
      if not R.ValueExists(FApplicationName) then
        R.WriteString(FApplicationName,FApplicationPath)
      else
      begin
        if not SameText(FApplicationPath,R.ReadString(FApplicationName)) then
          R.WriteString(FApplicationName,FApplicationPath);
      end;
    end;
  finally
    R.CloseKey;
    R.Free;
  end;
end;
{------------------------------------------------------------------------------}



{------------------------------------------------------------------------------}
procedure TgtAutoAppStarter.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Apply;
  end;
end;
{------------------------------------------------------------------------------}





initialization
finalization
  if Assigned(_AutoAppStarter) then
    FreeAndNil(_AutoAppStarter);

end.
