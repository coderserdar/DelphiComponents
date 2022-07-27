unit FileDate;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls;

type
  TFileDateForm = class(TForm)
    DTPicker1: TDateTimePicker;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Label1: TLabel;
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SelectedDateTime: TDateTime;
  end;

var
  FileDateForm: TFileDateForm;

implementation

uses FileExpl2886_Demo1Main;

{$R *.DFM}

procedure TFileDateForm.BtnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
  SelectedDateTime := DTPicker1.Date;
end;

procedure TFileDateForm.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFileDateForm.RadioButton1Click(Sender: TObject);
begin
  DTPicker1.Kind := dtkDate;
end;

procedure TFileDateForm.RadioButton2Click(Sender: TObject);
begin
  DTPicker1.Kind := dtkTime;
end;

procedure TFileDateForm.FormActivate(Sender: TObject);
begin
{$IFDEF VER100}
  DTPicker1.Date := Now;
{$ELSE}
  DTPicker1.DateTime := Now;
{$ENDIF}
end;

end.
