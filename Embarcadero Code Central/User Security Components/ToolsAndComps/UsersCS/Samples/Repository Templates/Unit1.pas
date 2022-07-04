unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FRMMAINFORM_REP, Menus, users_basic, users_cs, DBTables, StdCtrls, Db;

type
  TfrmMainForm = class(TfrmMainForm_Rep)
    Database1: TDatabase;
    Memo1: TMemo;
    InheritedForms1: TMenuItem;
    Form011: TMenuItem;
    Form021: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Form021Click(Sender: TObject);
    procedure Form011Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMainForm: TfrmMainForm;

implementation

uses Unit2, Unit3;

{$R *.DFM}

procedure TfrmMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  if not UsersCS1.Login then
    Application.Terminate;
end;

procedure TfrmMainForm.Form021Click(Sender: TObject);
begin
  inherited;
  frm02:=Tfrm02.Create(Application);
  frm02.ShowModal;
  frm02.Free;
end;

procedure TfrmMainForm.Form011Click(Sender: TObject);
begin
  inherited;
  frm01:=Tfrm01.Create(Application);
  frm01.ShowModal;
  frm01.Free;
end;

end.
