unit Unit1;

interface

uses
  FreeButton, StdCtrls, Controls, Classes, ExtCtrls, Forms, ShellAPI,
  Graphics, Windows;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    FreeButton2: TFreeButton;
    FreeButton7: TFreeButton;
    FreeButton8: TFreeButton;
    FreeButton1: TFreeButton;
    FreeButton3: TFreeButton;
    FreeButton4: TFreeButton;
    FreeButton5: TFreeButton;
    FreeButton6: TFreeButton;
    FreeButton9: TFreeButton;
    FreeButton10: TFreeButton;
    FreeButton11: TFreeButton;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    FreeButton14: TFreeButton;
    FreeButton12: TFreeButton;
    FreeButton15: TFreeButton;
    Image1: TImage;
    FreeButton17: TFreeButton;
    FreeButton13: TFreeButton;
    FreeButton19: TFreeButton;
    FreeButton16: TFreeButton;
    FreeButton18: TFreeButton;
    FreeButton20: TFreeButton;
    FreeButton21: TFreeButton;
    Image2: TImage;
    Label6: TLabel;
    procedure FreeButton10Click(Sender: TObject);
    procedure FreeButton11Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FreeButton15Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FreeButton10Click(Sender: TObject);
begin
     Close;
end;

procedure TForm1.FreeButton11Click(Sender: TObject);
begin
     Form1.Width := Panel1.Width+Panel2.Width;
     Form1.Left := Screen.Width div 2 - Form1.Width div 2;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
     Form1.Width := Panel1.Width;
end;

procedure TForm1.FreeButton15Click(Sender: TObject);
begin
     ShellExecute(GetDesktopWindow, 'open', PChar('mailto:TheB_6030@caramail.com'), nil, nil, SW_SHOWNORMAL);
end;

end.
