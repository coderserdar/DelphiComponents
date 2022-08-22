unit Inst002;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, DdeMan, Progman, ExtCtrls, Buttons;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    ProgMan1: TProgMan;
    Memo2: TMemo;
    Label1: TLabel;
    Memo3: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    Image1: TImage;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Image2: TImage;
    Label7: TLabel;
    Label8: TLabel;
    BitBtn1: TBitBtn;
    procedure Button1Click(Sender: TObject);
    procedure ProgMan1GroupError(Sender: TObject);
    procedure ProgMan1ItemError(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
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
  progman1.makeGroup;
end;

procedure TForm1.ProgMan1GroupError(Sender: TObject);
begin
  MessageDlg('Could not create group.', mtError, [mbOk], 0);
end;

procedure TForm1.ProgMan1ItemError(Sender: TObject);
begin
  MessageDlg('Could not create item.', mtError, [mbOk], 0);
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  close;
end;

end.
