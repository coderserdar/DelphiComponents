unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CalcExpress;

type
  TDemoForm = class(TForm)
    ExprEdt: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    CalcBtn: TButton;
    CalcExpress1: TCalcExpress;
    Label3: TLabel;
    Vars: TMemo;
    Values: TMemo;
    procedure CalcBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DemoForm: TDemoForm;

implementation

{$R *.DFM}

procedure TDemoForm.CalcBtnClick(Sender: TObject);
var args : array [0..100] of extended; // array of arguments - variable values
    i : integer;
begin
  if (Vars.Lines.Count = Values.Lines.Count) then
   begin
    // set expression to calculate
    CalcExpress1.Formula := ExprEdt.Text;
    // set used variables list
    CalcExpress1.Variables:= Vars.Lines;
    // prepare arguments
//    SetLength(args,Values.Lines.Count);
    for i:=0 to Values.Lines.Count-1 do
     args[i] := StrToFloat(Values.Lines[i]);
    // calculate expression
    ShowMessage(CalcExpress1.Formula+'='+
                FloatToStr(CalcExpress1.calc(args)));
   end
  else
    MessageDlg('Variables and Variable values lists should have the same lines quantity.', mtInformation, [mbOk], 0);
end;

end.
