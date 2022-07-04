unit Main;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TFormMain = class(TForm)
    Button2: TButton;
    Button1: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  UnitDesign;

{$R *.dfm}

procedure TFormMain.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.Button2Click(Sender: TObject);
begin
  FormDesign.RLReport1.Preview;
end;

end.

