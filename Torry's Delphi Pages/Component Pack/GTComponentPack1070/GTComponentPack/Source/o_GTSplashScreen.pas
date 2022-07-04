{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtSplashScreen                                 }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_GTSplashScreen;

interface
uses
   Classes
  ,Forms
  ,Graphics
  ,ExtCtrls
  ,StdCtrls
  ;
type
{------------------------------------------------------------------------------}
  TgtSplashPosition = (
                         spCustom
                        ,spScreenCenter
                       )
                      ;
{------------------------------------------------------------------------------}
  TgtSplashScreen = class(TComponent)
  private
    FWidth: Integer;
    FDisplayDuration: Byte;
    FHeight: Integer;
    FPictureFileName: string;
    FPicture: TPicture;
    FLeft: Integer;
    FTop: Integer;
    FSplashPosition: TgtSplashPosition;
    FMainForm: TForm;
    FStretchPicture: Boolean;
    FInfoText: string;
    FInfoTextFont: TFont;
    FManualHide: Boolean;
    procedure SetPicture(const Value: TPicture);
    procedure SetDisplayDuration(const Value: Byte);
    procedure SetAsMainForm(aForm: TForm);
    procedure SetInfoText(const Value: string);
    procedure SetInfoTextFont(const Value: TFont);
    { Private declarations }
  protected
    { Protected declarations }
    FSplashForm    : TForm;
    FImage         : TImage;
    FDurationTimer : TTimer;
    FInfoLabel     : TLabel;
    procedure CreateSplash;
    procedure InternalOnDurationTimer(Sender : TObject);
    procedure InternalOnMainFormShow (Sender : TObject);
    procedure Notification(AComponent : TComponent ; Operation : TOperation);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    procedure CloseSplashScreen;
  published
    { Published declarations}
    property Picture         : TPicture           read FPicture         write SetPicture;
    property PictureFileName : string             read FPictureFileName write FPictureFileName;
    property Width           : Integer            read FWidth           write FWidth;
    property Height          : Integer            read FHeight          write FHeight;
    property Left            : Integer            read FLeft            write FLeft;
    property Top             : Integer            read FTop             write FTop;
    property DisplayDuration : Byte               read FDisplayDuration write SetDisplayDuration;
    property SplashPosition  : TgtSplashPosition  read FSplashPosition  write FSplashPosition;
    property StretchPicture  : Boolean            read FStretchPicture  write FStretchPicture;
    property InfoTextFont    : TFont              read FInfoTextFont    write SetInfoTextFont;
    property InfoText        : string             read FInfoText        write SetInfoText;
    property ManualHide      : Boolean            read FManualHide      write FManualHide;
  end;
{------------------------------------------------------------------------------}

implementation

uses
   SysUtils
  ,Windows
  ,Controls
  ;


const
  ERR_OWNER_MUST_BE_FORM =
   'Owner of TgtSplashScreen must be a TForm'#13+
   'or a descentant of it!                  '#13+
   '';

{ TgtSplashScreen }


{------------------------------------------------------------------------------}
constructor TgtSplashScreen.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not AOwner.InheritsFrom(TForm) then
    raise Exception.Create(ERR_OWNER_MUST_BE_FORM);
  FPicture               := TPicture.Create;
  FWidth                 := 400;
  FHeight                := 270;
  FLeft                  := Screen.Width  div 2;
  FTop                   := Screen.Height div 2;
  FSplashPosition        := spScreenCenter;
  FDisplayDuration       := 5;
  FManualHide            := False;
  FStretchPicture        := False;

  FDurationTimer         := TTimer.Create(Self);
  FDurationTimer.Enabled := False;
  FDurationTimer.OnTimer := InternalOnDurationTimer;

  FInfoTextFont          := TFont.Create;

  if not (csDesigning in ComponentState) then
  begin
    FMainForm := TForm(AOwner);
    CreateSplash;
    SetAsMainForm(FSplashForm);
  end;
end;
{------------------------------------------------------------------------------}
destructor TgtSplashScreen.Destroy;
begin
  FPicture.Free;
  FInfoTextFont.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtSplashScreen.Notification(AComponent: TComponent;Operation: TOperation);
begin
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
procedure TgtSplashScreen.SetAsMainForm(aForm:TForm);
var
  P:Pointer;
begin
  P := @Application.Mainform;
  Pointer(P^) := aForm;
end;
{------------------------------------------------------------------------------}
procedure TgtSplashScreen.CreateSplash;
begin
  FSplashForm             := TForm.Create(Self);
  FSplashForm.FormStyle   := fsStayOnTop;
  FSplashForm.BorderStyle := bsNone;
  FSplashForm.Width       := Self.Width;
  FSplashForm.Height      := Self.Height;
  FSplashForm.OnShow      := InternalOnMainFormShow;
  FSplashForm.Color       := clBlack;

  if Self.SplashPosition = spScreenCenter then
    FSplashForm.Position    := poScreenCenter;

  FImage                  := TImage.Create(FSplashForm);
  FImage.Align            := alClient;
  FImage.Parent           := FSplashForm;

  FInfoLabel              := TLabel.Create(FSplashForm);
  FInfoLabel.Parent       := FSplashForm;
  FInfoLabel.Transparent  := True;
  FInfoLabel.ParentColor  := True;

  FInfoLabel.Left         := 1;

end;
{------------------------------------------------------------------------------}
procedure TgtSplashScreen.CloseSplashScreen;
begin
  if Assigned(FSplashForm) then
  begin
    Windows.ShowWindow(Application.Handle, SW_SHOWNORMAL);
    SetAsMainForm(FMainForm);
    FSplashForm.Close;
    FreeAndNil(FSplashForm);
    Application.MainForm.Visible := True;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtSplashScreen.InternalOnDurationTimer(Sender: TObject);
begin
  FDurationTimer.Enabled := False;
  CloseSplashScreen;
end;
{------------------------------------------------------------------------------}
procedure TgtSplashScreen.InternalOnMainFormShow(Sender: TObject);
begin
  try
    if FileExists(FPictureFileName) then
      Self.Picture.LoadFromFile(FPictureFileName);
    FImage.Picture.Assign(Self.Picture);
    FImage.Stretch := StretchPicture;
  except
    if not FManualHide then
    begin
      FDurationTimer.Interval := FDisplayDuration * 1000;
      FDurationTimer.Enabled  := True;
    end;
  end;

  if Self.SplashPosition = spCustom then
  begin
    FSplashForm.Left   := Self.Left;
    FSplashForm.Top    := Self.Top;
  end;

  FSplashForm.Height := Self.Height;
  FSplashForm.Width  := Self.Width;

  FInfoLabel.Top         := FSplashForm.Height - (FInfoLabel.Height * 2);
  FInfoLabel.Caption     := Self.InfoText;
  FInfoLabel.Font        := Self.InfoTextFont;


  Windows.ShowWindow(Application.Handle, SW_HIDE);
  if not FManualHide then
  begin
    FDurationTimer.Interval := FDisplayDuration * 1000;
    FDurationTimer.Enabled  := True;
  end;
end;
{------------------------------------------------------------------------------}





{------------------------------------------------------------------------------}
procedure TgtSplashScreen.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TgtSplashScreen.SetDisplayDuration(const Value: Byte);
begin
  FDisplayDuration := Value;
  FDurationTimer.Interval := FDisplayDuration * 1000;
end;
{------------------------------------------------------------------------------}
procedure TgtSplashScreen.SetInfoText(const Value: string);
begin
  FInfoText := Value;
  if (Assigned(FSplashForm)) and (Assigned(FInfoLabel)) then
    FInfoLabel.Caption := FInfoText;
end;
{------------------------------------------------------------------------------}
procedure TgtSplashScreen.SetInfoTextFont(const Value: TFont);
begin
  FInfoTextFont.Assign(Value);
end;
{------------------------------------------------------------------------------}



end.
 