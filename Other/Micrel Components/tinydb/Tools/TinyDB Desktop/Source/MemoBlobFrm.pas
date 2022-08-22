unit MemoBlobFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, DBCtrls,
  ExtCtrls, Db;

type
  TMemoBlobForm = class(TForm)
    Panel1: TPanel;
    BottomPanel: TPanel;
    OkButton: TButton;
    Button1: TButton;
    Memo: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    { Private declarations }
    FDBMemo: TDBMemo;
  public
    { Public declarations }
    procedure SetData(DBMemo: TDBMemo);
  end;


function ShowMemoBlobDataForm(DBMemo: TDBMemo): Boolean;

implementation

{$R *.DFM}

var
  MemoBlobForm: TMemoBlobForm;

  
function ShowMemoBlobDataForm(DBMemo: TDBMemo): Boolean;
var
  Frm: TMemoBlobForm;
begin
  Frm := TMemoBlobForm.Create(Application);
  try
    Frm.SetData(DBMemo);
    Result := Frm.ShowModal = mrOk;
  finally
    Frm.Free;
  end;
end;

procedure TMemoBlobForm.SetData(DBMemo: TDBMemo);
begin
  FDBMemo := DBMemo;
  Memo.Lines.Assign(DBMemo.Lines);
end;

procedure TMemoBlobForm.Button1Click(Sender: TObject);
begin
  FDBMemo.Lines.Assign(Memo.Lines);
  //FDBMemo.Field.AsString := 'asdf';
  ModalResult := mrOk;
end;

procedure TMemoBlobForm.OkButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.

