unit About;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, RXCtrls, ExtCtrls, rxAnimate;

type
  TAboutForm = class(TForm)
    SecretPanel1: TSecretPanel;
    AppIcon: TImage;
    WebLabel: TRxLabel;
    Label1: TLabel;
    Label2: TLabel;
    OkBtn: TBitBtn;
    Label3: TLabel;
    Bevel1: TBevel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure AppIconDblClick(Sender: TObject);
    procedure SecretPanel1DblClick(Sender: TObject);
    procedure LabelMouseEnter(Sender: TObject);
    procedure LabelClick(Sender: TObject);
    procedure SecretPanel1PaintClient(Sender: TObject; Canvas: TCanvas;
      Rect: TRect);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure RxWebSite;

implementation

uses
  RxConst, ShellAPI, rxVclUtils;

{$R *.DFM}

procedure RxWebSite;
begin
  ShellExecute(Application.Handle, nil, 'http://www.rxlib.com', nil,
    nil, SW_SHOWNOACTIVATE);
end;

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  AppIcon.Picture.Icon := Application.Icon;
  AppIcon.Cursor := crHand;
  WebLabel.Cursor := crHand;
end;

procedure TAboutForm.AppIconDblClick(Sender: TObject);
begin
  SecretPanel1.Active := True;
end;

procedure TAboutForm.SecretPanel1DblClick(Sender: TObject);
begin
  SecretPanel1.Active := False;
end;

procedure TAboutForm.LabelMouseEnter(Sender: TObject);
begin
  with Sender as TRxLabel do begin
    if MouseInControl then begin
      Font.Color := clHighlight;
    end
    else begin
      Font.Color := clWindowText;
    end;
  end;
end;

procedure TAboutForm.LabelClick(Sender: TObject);
begin
  RxWebSite;
end;

procedure TAboutForm.SecretPanel1PaintClient(Sender: TObject;
  Canvas: TCanvas; Rect: TRect);
begin
  if SecretPanel1.Active then
    GradientFillRect(Canvas, Rect, clSilver, clGray, fdBottomToTop, 64)
  else Canvas.FillRect(Rect);
end;

end.
