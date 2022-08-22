unit InputFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TInputForm = class(TForm)
    GroupBox1: TGroupBox;
    PromptLabel: TLabel;
    ValueEdit: TEdit;
    OkButton: TButton;
    CancelButton: TButton;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetData(Value: string);
    procedure GetData(var Value: string);
  end;

var
  InputForm: TInputForm;

function ShowInputForm(var Value: string; Title, Prompt: string; AsPassword: Boolean = False): Boolean;

implementation

{$R *.DFM}

function ShowInputForm(var Value: string; Title, Prompt: string; AsPassword: Boolean = False): Boolean;
var
  Frm: TInputForm;
begin
  Frm := TInputForm.Create(Application);
  Frm.Caption := Title;
  Frm.PromptLabel.Caption := Prompt;
  Frm.SetData(Value);
  if AsPassword then Frm.ValueEdit.PasswordChar := '*';
  Result := (Frm.ShowModal = mrOk);
  if Result then Frm.GetData(Value);
  Frm.Free;
end;

{ TInputForm }

procedure TInputForm.SetData(Value: string);
begin
  ValueEdit.Text := Value;
end;

procedure TInputForm.GetData(var Value: string);
begin
  Value := ValueEdit.Text;
end;

procedure TInputForm.OkButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TInputForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
