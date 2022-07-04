unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  EkImgForm, EkImgLED, EkImgFadeBase, EkImgBtn, EkLabel, ExtCtrls;

type
  TForm1 = class(TForm)
    EkImgForm1: TEkImgForm;
    EkImgLED1: TEkImgLED;
    BtnMinimize: TEkImgBtn;
    BtnClose: TEkImgBtn;
    Timer1: TTimer;
    EkImgLED2: TEkImgLED;
    EkImgLED3: TEkImgLED;
    Image1: TImage;
    EkImgLED4: TEkImgLED;
    EkImgBtn1: TEkImgBtn;
    EkImgLED5: TEkImgLED;
    EkImgLED6: TEkImgLED;
    EkLabel1: TEkLabel;
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnMinimizeClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure EkImgBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

//==============================================================================

procedure TForm1.BtnCloseClick(Sender: TObject);
begin
Close(); 
end;

//==============================================================================

procedure TForm1.BtnMinimizeClick(Sender: TObject);
begin
Application.Minimize;
end;

//==============================================================================

procedure TForm1.EkImgBtn1Click(Sender: TObject);
begin
EkImgBtn1.FadeLoop := not EkImgBtn1.FadeLoop;
EkImgLED1.Enabled := not EkImgLED1.Enabled;
EkImgLED2.Enabled := not EkImgLED2.Enabled;
EkImgLED3.Enabled := not EkImgLED3.Enabled;
EkImgLED4.Enabled := not EkImgLED4.Enabled;
EkImgLED5.Enabled := not EkImgLED5.Enabled;
EkImgLED6.Enabled := not EkImgLED6.Enabled;
end;

//==============================================================================

procedure TForm1.Timer1Timer(Sender: TObject);
begin
if EkImgLED1.Enabled then
begin
  EkImgLED1.Value := EkImgLED1.Value + 1;
  EkImgLED2.Value := EkImgLED2.Value - 1;
  EkImgLED3.Value := EkImgLED3.Value + 1;
  EkImgLED4.Value := EkImgLED4.Value + 1;
  EkImgLED5.Value := EkImgLED5.Value + 1;
  EkImgLED6.Value := EkImgLED6.Value + 1;
end;
end;

//==============================================================================

procedure TForm1.FormCreate(Sender: TObject);
begin
EkImgForm1.LoadNewImage('Graphics\WMP11-2-3.bmp');
end;

//==============================================================================

end.
