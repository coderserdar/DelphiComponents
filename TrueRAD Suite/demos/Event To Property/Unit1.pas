unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, radconlist, rade2p;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edX: TEdit;
    Label2: TLabel;
    edY: TEdit;
    lbArea: TLabel;
    radConnectionList1: TradConnectionList;
    Button1: TButton;
    E2PConnection1: TradE2PConnection;
    E2PConnection2: TradE2PConnection;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
    Close;
end;

end.
