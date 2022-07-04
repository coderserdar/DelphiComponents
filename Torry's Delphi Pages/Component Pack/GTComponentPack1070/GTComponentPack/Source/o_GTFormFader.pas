unit o_GTFormFader;

interface
uses
   Classes
  ,Forms
  ,ExtCtrls
  ;

type
{------------------------------------------------------------------------------}
  TgtFaderStatus = (
                      fsUnKnown
                     ,fsFadeIn
                     ,fsFadeOut
                    )
                    ;
{------------------------------------------------------------------------------}
  TgtFormFader = class(TComponent)
  private
    FForm: TForm;
    FCloseAction: TCloseAction;
    FFadeIn: Boolean;
    FFadeOut: Boolean;
    FFaderStatus: TgtFaderStatus;
    FFaderSpeed: Integer;
    procedure SetForm(const Value: TForm);
    procedure SetFadeIn(const Value: Boolean);
    procedure SetFadeOut(const Value: Boolean);
    { Private declarations }
  protected
    { Protected declarations }
    FFaderTimer    : TTimer;
    FOldFormOnShow : TNotifyEvent;
    FOldFormOnClose: TCloseEvent;
    FFadeCloseDone : Boolean;
    procedure InternalFormOnShow (Sender : TObject);
    procedure InternalFormOnClose(Sender: TObject; var Action: TCloseAction);
    procedure InternalOnFaderTimer(Sender : TObject);
    procedure Notification(AComponent : TComponent ; Operation : TOperation);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property Form        : TForm          read FForm         write SetForm;
    property FadeIn      : Boolean        read FFadeIn       write SetFadeIn;
    property FadeOut     : Boolean        read FFadeOut      write SetFadeOut;
    property CloseAction : TCloseAction   read FCloseAction  write FCloseAction;
    property FaderStatus : TgtFaderStatus read FFaderStatus;
    property FaderSpeed  : Integer        read FFaderSpeed   write FFaderSpeed;
  end;
{------------------------------------------------------------------------------}

implementation

{ TgtFormFader }
{------------------------------------------------------------------------------}
constructor TgtFormFader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFaderTimer          := TTimer.Create(Self);
  FFaderTimer.Enabled  := False;
  FFaderTimer.OnTimer  := InternalOnFaderTimer;
  FFaderTimer.Interval := 1;
  FCloseAction         := caFree;
  FFaderStatus         := fsUnKnown;
  FFaderSpeed          := 10;
end;
{------------------------------------------------------------------------------}
destructor TgtFormFader.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtFormFader.Notification(AComponent: TComponent;Operation: TOperation);
begin
  if Operation = opRemove then
    if AComponent = FForm then
      Form := nil;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
procedure TgtFormFader.InternalFormOnClose(Sender: TObject; var Action: TCloseAction);
begin
  if FFadeCloseDone then
    Action                := caFree
  else
  begin
    FFadeCloseDone        := False;
    Action                := caNone;
    if not FForm.AlphaBlend then
      FForm.AlphaBlend      := True;
    FForm.AlphaBlendValue := 255;
    FFaderStatus          := fsFadeOut;
    FFaderTimer.Enabled   := True;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtFormFader.InternalFormOnShow(Sender: TObject);
begin
  FForm.AlphaBlend      := True;
  FForm.AlphaBlendValue := 0;
  FFaderStatus          := fsFadeIn;
  FForm.Enabled         := False;
  FFaderTimer.Enabled   := True;
end;
{------------------------------------------------------------------------------}
procedure TgtFormFader.InternalOnFaderTimer(Sender: TObject);
begin
  case FFaderStatus of
    fsFadeIn :
      begin
        if (FForm.AlphaBlendValue < 255)  then
        begin
          if (FForm.AlphaBlendValue + FFaderSpeed) < 255 then
            FForm.AlphaBlendValue := FForm.AlphaBlendValue + FFaderSpeed
          else
            FForm.AlphaBlendValue := 255;
          Application.ProcessMessages;
        end;
        if FForm.AlphaBlendValue >= 255 then
        begin
          FForm.Enabled          := True;
          FForm.AlphaBlendValue  := 255;
          TTimer(Sender).Enabled := False;
          if Assigned(FOldFormOnShow) then
            FOldFormOnShow(FForm);
        end;
      end;
    fsFadeOut :
      begin
        if (FForm.AlphaBlendValue > 0) then
        begin
          if (FForm.AlphaBlendValue - FFaderSpeed) > 0 then
            FForm.AlphaBlendValue := FForm.AlphaBlendValue - FFaderSpeed
          else
            FForm.AlphaBlendValue := 0;
          Application.ProcessMessages;
        end;
        if FForm.AlphaBlendValue = 0 then
        begin

          TTimer(Sender).Enabled := False;
          if Assigned(FOldFormOnClose) then
          begin
            FForm.Visible    := False;
            FForm.AlphaBlendValue  := 255;
            FOldFormOnClose(FForm,FCloseAction);
            FFadeCloseDone := True;
            FForm.Close;
          end
          else
          begin
            FForm.Visible    := False;
            FForm.AlphaBlendValue  := 255;
            FFadeCloseDone   := True;
            FForm.Close;
          end;
        end;
      end;
  end;
end;
{------------------------------------------------------------------------------}





{------------------------------------------------------------------------------}
procedure TgtFormFader.SetFadeIn(const Value: Boolean);
begin
  FFadeIn := Value;
  if Assigned(FForm) then
  begin
      if FFadeIn then
      begin
        FOldFormOnShow  := FForm.OnShow;
        FForm.OnShow    := InternalFormOnShow;
      end
      else
      begin
        FForm.OnShow    := FOldFormOnShow;
        FOldFormOnShow  := nil;
      end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtFormFader.SetFadeOut(const Value: Boolean);
begin
  FFadeOut := Value;
  if Assigned(FForm) then
  begin
    if FFadeOut then
    begin
      FOldFormOnClose := FForm.OnClose;
      FForm.OnClose   := InternalFormOnClose;
    end
    else
    begin
      FForm.OnClose   := FOldFormOnClose;
      FOldFormOnClose := nil;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtFormFader.SetForm(const Value: TForm);
begin
  if FForm <> Value then
  begin
    if Assigned(FForm) then
      FForm.RemoveFreeNotification(Self);

    FForm := Value;

    if Assigned(FForm) then
    begin
      FForm.FreeNotification(Self);
      FadeIn  := FadeIn;
      FadeOut := FadeOut;
    end;
  end;
end;
{------------------------------------------------------------------------------}



end.
