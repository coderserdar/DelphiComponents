unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  radvar, StdCtrls, radconlist, radp2p, rade2m;

type
  TForm1 = class(TForm)
    radConnectionList1: TradConnectionList;
    Edit1: TEdit;
    ListBox1: TListBox;
    radVariable1: TradVariable;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    P2PConnection1: TradP2PConnection;
    E2MConnection1: TradE2MConnection;
    E2MConnection2: TradE2MConnection;
    E2MConnection3: TradE2MConnection;
    Button4: TButton;
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button4Click(Sender: TObject);
begin
    Close;
end;

end.
