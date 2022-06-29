unit Unit9;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FR_Ctrls, FR_View, ExtCtrls;

type
  TForm9 = class(TForm)
    Panel1: TPanel;
    frPreview1: TfrPreview;
    frSpeedButton1: TfrSpeedButton;
    frSpeedButton2: TfrSpeedButton;
    frSpeedButton3: TfrSpeedButton;
    frSpeedButton4: TfrSpeedButton;
    frSpeedButton5: TfrSpeedButton;
    frSpeedButton6: TfrSpeedButton;
    frSpeedButton7: TfrSpeedButton;
    frSpeedButton8: TfrSpeedButton;
    frSpeedButton9: TfrSpeedButton;
    frSpeedButton10: TfrSpeedButton;
    frSpeedButton11: TfrSpeedButton;
    procedure frSpeedButton4Click(Sender: TObject);
    procedure frSpeedButton5Click(Sender: TObject);
    procedure frSpeedButton6Click(Sender: TObject);
    procedure frSpeedButton7Click(Sender: TObject);
    procedure frSpeedButton8Click(Sender: TObject);
    procedure frSpeedButton9Click(Sender: TObject);
    procedure frSpeedButton10Click(Sender: TObject);
    procedure frSpeedButton1Click(Sender: TObject);
    procedure frSpeedButton2Click(Sender: TObject);
    procedure frSpeedButton3Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure frSpeedButton11Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form9: TForm9;

implementation

{$R *.DFM}

procedure TForm9.frSpeedButton4Click(Sender: TObject);
begin
  frPreview1.First;
end;

procedure TForm9.frSpeedButton5Click(Sender: TObject);
begin
  frPreview1.Prev;
end;

procedure TForm9.frSpeedButton6Click(Sender: TObject);
begin
  frPreview1.Next;
end;

procedure TForm9.frSpeedButton7Click(Sender: TObject);
begin
  frPreview1.Last;
end;

procedure TForm9.frSpeedButton8Click(Sender: TObject);
begin
  frPreview1.LoadFromFile;
end;

procedure TForm9.frSpeedButton9Click(Sender: TObject);
begin
  frPreview1.SaveToFile;
end;

procedure TForm9.frSpeedButton10Click(Sender: TObject);
begin
  frPreview1.Print;
end;

procedure TForm9.frSpeedButton1Click(Sender: TObject);
begin
  frPreview1.OnePage;
end;

procedure TForm9.frSpeedButton2Click(Sender: TObject);
begin
  frPreview1.Zoom := 100;
end;

procedure TForm9.frSpeedButton3Click(Sender: TObject);
begin
  frPreview1.PageWidth;
end;

procedure TForm9.FormActivate(Sender: TObject);
begin
  frSpeedButton2.Down := True;
  frSpeedButton2Click(nil);
end;

procedure TForm9.frSpeedButton11Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.
