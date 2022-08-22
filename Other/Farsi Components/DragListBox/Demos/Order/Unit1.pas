unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, OrderListBox, Buttons, ExtCtrls, DragListBox;

type
  TfrmOrderedList = class(TForm)
    Bevel1: TBevel;
    Label1: TLabel;
    btnMoveUp: TBitBtn;
    bynMoveDown: TBitBtn;
    btnOK: TButton;
    btnCancel: TButton;
    btnMoveTop: TBitBtn;
    btnMoveBottom: TBitBtn;
    OrderList: TDragListBox;
    procedure btnMoveUpClick(Sender: TObject);
    procedure bynMoveDownClick(Sender: TObject);
    procedure btnMoveTopClick(Sender: TObject);
    procedure btnMoveBottomClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmOrderedList: TfrmOrderedList;

implementation

{$R *.DFM}

procedure TfrmOrderedList.btnMoveUpClick(Sender: TObject);
begin
  OrderList.MoveUp;
end;

procedure TfrmOrderedList.bynMoveDownClick(Sender: TObject);
begin
  OrderList.MoveDown;
end;

procedure TfrmOrderedList.btnMoveTopClick(Sender: TObject);
begin
  OrderList.MoveTop
end;

procedure TfrmOrderedList.btnMoveBottomClick(Sender: TObject);
begin
  OrderList.MoveBottom
end;

procedure TfrmOrderedList.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmOrderedList.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmOrderedList.FormCreate(Sender: TObject);
var I: Integer;
begin
  //Fill list
  Randomize;
  for I:= 0 to 15 do
  begin
    OrderList.Items.Insert( Random(OrderList.Items.Count),
      'Item'+IntToHex(I,1));

  end;
end;

end.
