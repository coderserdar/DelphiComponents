{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtExplorerSysObjs                              }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_GTExplorerSysObjects;

interface
uses
   Classes
  ;

type
  TgtExplorerSysObjs  = class(TComponent)
  private
    FShowSystemObjects: Boolean;
    procedure SetShowSystemObjects(const Value: Boolean);
    { Private declarations }
  protected
    { Protected declarations }
    FDefaultHiddenStatus : Integer;
    procedure  UpdateExplorer;
    procedure  ApplySysObjsVisible(Visible : Boolean);
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property ShowSystemObjects : Boolean read FShowSystemObjects write SetShowSystemObjects;
  end;


implementation

uses
   Windows
  ,Messages
  ,Registry
  ;

const
  EXPLORER_KEY   = '\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced';
  EXPLORER_VALUE = 'ShowSuperHidden';
  EXPLORER_CLASS = 'ExploreWClass';

{ TgtExplorerSysObjs }
{------------------------------------------------------------------------------}
constructor TgtExplorerSysObjs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultHiddenStatus := 0;
end;
{------------------------------------------------------------------------------}
destructor TgtExplorerSysObjs.Destroy;
begin
  ApplySysObjsVisible(False);
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtExplorerSysObjs.ApplySysObjsVisible(Visible: Boolean);
var
  R : TRegistry;
begin
  //ShowSuperHidden is a flag for system objects
  //files,folders,e.t.c
  //0=Hide Files 1= ShowFiles
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    if R.OpenKey(EXPLORER_KEY,False) then
    begin
      case Visible of
        True :
          begin
            if not R.ValueExists(EXPLORER_VALUE) then
              R.WriteInteger(EXPLORER_VALUE,1)
            else
            begin
              FDefaultHiddenStatus := R.ReadInteger(EXPLORER_VALUE);
              R.WriteInteger(EXPLORER_VALUE,1)
            end;
          end;
        False :
          begin
            if not R.ValueExists(EXPLORER_VALUE) then
              R.WriteInteger(EXPLORER_VALUE,FDefaultHiddenStatus)
            else
              R.WriteInteger(EXPLORER_VALUE,FDefaultHiddenStatus)
          end;
      end;
    end;
  finally
    R.CloseKey;
    R.Free;
  end;
  UpdateExplorer;
end;
{------------------------------------------------------------------------------}
procedure TgtExplorerSysObjs.UpdateExplorer;
var
  H : LongInt;
begin
 //Refresh the explorer.
 //This applies to Windows XP with SP2
 //Not tested with any other version of Windows.
 H := FindWindow(EXPLORER_CLASS,nil);
 if H <> 0 then
  SendMessage(H,WM_COMMAND,28931,0);
end;
{------------------------------------------------------------------------------}




//Getters - Setters\\
{------------------------------------------------------------------------------}
procedure TgtExplorerSysObjs.SetShowSystemObjects(const Value: Boolean);
begin
  if FShowSystemObjects <> Value then
  begin
    FShowSystemObjects := Value;
    ApplySysObjsVisible(FShowSystemObjects);
  end;
end;
{------------------------------------------------------------------------------}



end.
 