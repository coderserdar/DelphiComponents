unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, radconlist, radp2p;

type
  TForm1 = class(TForm)
    radConnectionList1: TradConnectionList;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    Edit2: TEdit;
    GroupBox2: TGroupBox;
    Edit3: TEdit;
    Edit4: TEdit;
    Button1: TButton;
    GroupBox3: TGroupBox;
    Edit5: TEdit;
    Edit6: TEdit;
    P2PConnection1: TradP2PConnection;
    P2PConnection2: TradP2PConnection;
    P2PConnection3: TradP2PConnection;
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
