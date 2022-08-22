unit DBCommentsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, TinyDB, BaseFrm;

type
  TDBCommentsForm = class(TBaseForm)
    GroupBox1: TGroupBox;
    Memo: TMemo;
    OkButton: TButton;
    CancelButton: TButton;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    { Private declarations }
    FTinyDB: TTinyDatabase;
  public
    { Public declarations }
    procedure SetData(ATinyDB: TTinyDatabase);
  end;

var
  DBCommentsForm: TDBCommentsForm;

function ShowDBCommentsForm(ATinyDB: TTinyDatabase): Boolean;

implementation

uses LangMgr;

{$R *.DFM}

function ShowDBCommentsForm(ATinyDB: TTinyDatabase): Boolean;
var
  Frm: TDBCommentsForm;
begin
  Frm := TDBCommentsForm.Create(Application);
  Frm.SetData(ATinyDB);
  Result := Frm.ShowModal = mrOk;
  Frm.Free;
end;

procedure TDBCommentsForm.SetData(ATinyDB: TTinyDatabase);
var
  S: string;
begin
  FTinyDB := ATinyDB;
  FTinyDB.GetComments(S);
  Memo.Lines.Text := S;
end;

procedure TDBCommentsForm.OkButtonClick(Sender: TObject);
begin
  FTinyDB.SetComments(Memo.Lines.Text);
  ModalResult := mrOk;
end;

procedure TDBCommentsForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
