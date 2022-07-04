{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtFormEvents                                   }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}

unit o_FormEvents;

interface
uses
   Classes
  ,Forms
  ,Messages
  ;
type
{------------------------------------------------------------------------------}
  TgtFormEvents = class(TComponent)
  private
    FForm          : TForm;
    FOnAfterShow   : TNotifyEvent;
    FOnMinimize    : TNotifyEvent;
    FOnBeforeShow  : TNotifyEvent;
    FOnRestore     : TNotifyEvent;
    procedure SetForm(const Value: TForm);
    { Private declarations }
  protected
    { Protected declarations }
    FOldFormWndProc  : TWndMethod;
    FOldAppOnRestore : TNotifyEvent;
    FRestoreHasRun   : Boolean;
    procedure Notification(AComponent: TComponent;Operation: TOperation);override;
    procedure NewWndProc(var Message : TMessage);
    procedure InternalOnAppRestore(Sender : TObject);
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property Form : TForm read FForm write SetForm;
  published
    property OnBeforeShow : TNotifyEvent read FOnBeforeShow write FOnBeforeShow;
    property OnAfterShow  : TNotifyEvent read FOnAfterShow  write FOnAfterShow;
    property OnMinimize   : TNotifyEvent read FOnMinimize   write FOnMinimize;
    property OnRestore    : TNotifyEvent read FOnRestore    write FOnRestore;
  end;
{------------------------------------------------------------------------------}

implementation
uses
  Windows
  ;
const
  WM_AFTER_SHOW = WM_USER + 1001;

{ TgtFormEvents }
{------------------------------------------------------------------------------}
constructor TgtFormEvents.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TForm then
    Form := TForm(AOwner);
  FRestoreHasRun := False;
end;
{------------------------------------------------------------------------------}
destructor TgtFormEvents.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtFormEvents.Notification(AComponent: TComponent;Operation: TOperation);
begin
  if Operation = opRemove then
    if AComponent = FForm then
      Form := nil;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
procedure TgtFormEvents.NewWndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_SHOWWINDOW :
      begin
        case Message.WParam of
          0 : ;
          1 :
            begin
              if Assigned(FOnBeforeShow) then
                FOnBeforeShow(FForm);
              PostMessage(FForm.Handle,WM_AFTER_SHOW,0,0);
            end
        end;
      end;
    WM_AFTER_SHOW :
      begin
        if Assigned(FOnAfterShow) then
          FOnAfterShow(FForm);
      end;
    WM_SYSCOMMAND :
      begin
        case Message.WParam of
          SC_MINIMIZE :
            begin
              if Assigned(FOnMinimize) then
                FOnMinimize(FForm);
              FRestoreHasRun := False;
            end;
          SC_RESTORE  :
            begin
              if Assigned(FOnRestore) then
                FOnRestore(FForm);
            end;
        end;
     end;
  end;
  FOldFormWndProc(Message);
end;
{------------------------------------------------------------------------------}
procedure TgtFormEvents.InternalOnAppRestore(Sender: TObject);
begin
  if not FRestoreHasRun then
  begin
    FRestoreHasRun := True;
    if Assigned(FOldAppOnRestore) then
      FOldAppOnRestore(Application);
    if Assigned(FOnRestore) then
      FOnRestore(FForm);
  end;
end;
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
procedure TgtFormEvents.SetForm(const Value: TForm);
begin
  if Assigned(FForm) then
  begin
    FForm.RemoveFreeNotification(Self);
    FForm.WindowProc := FOldFormWndProc;
    //Unhooking the forms WndProc
    if Assigned(FOldAppOnRestore) then
      Application.OnRestore :=   FOldAppOnRestore;
  end;

  FForm := Value;

  if Assigned(FForm) then
  begin
    FForm.FreeNotification(Self);
    FOldFormWndProc  := FForm.WindowProc;
    //Keeping the original Forms WndProc
    FForm.WindowProc := NewWndProc;
    FOldAppOnRestore := Application.OnRestore;
    Application.OnRestore := InternalOnAppRestore;
    //Hooking the forms WndProc
  end;
end;
{------------------------------------------------------------------------------}


end.

