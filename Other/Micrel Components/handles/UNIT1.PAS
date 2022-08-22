unit Unit1;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Handles, StdCtrls, ExtCtrls, Glassbut;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Button3: TButton;
    Button4: TButton;
    StretchHandle1: TStretchHandle;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  StretchHandle1: TStretchHandle;

implementation

{$R *.DFM}

procedure TForm1.Button3Click(Sender: TObject);
begin
  StretchHandle1.Detach;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin
  StretchHandle1.Attach(Panel1);
end;

procedure TForm1.Edit1Click(Sender: TObject);
begin
  StretchHandle1.Attach(Edit1);
end;

procedure TForm1.Label1Click(Sender: TObject);
begin
  StretchHandle1.Attach(Label1);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  StretchHandle1.Attach(Button1);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  StretchHandle1.Attach(Button2);
end;

end.
