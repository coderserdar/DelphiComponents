unit FormMathematic;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,FormAbout, Mathematic;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    EvalExp: TEdit;
    EvalVar1: TEdit;
    EvalResult: TEdit;
    EvalCal: TButton;
    EvalVal1: TEdit;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    DiffExpress: TEdit;
    DiffVariable: TEdit;
    DiffResult: TEdit;
    EvalDiff: TButton;
    GroupBox3: TGroupBox;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    SumExp: TEdit;
    SumVar: TEdit;
    SumFrom: TEdit;
    SumResult: TEdit;
    SumCal: TButton;
    SumTo: TEdit;
    GroupBox4: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Button2: TButton;
    Math: TMathematic;
    procedure EvalDiffClick(Sender: TObject);
    procedure EvalCalClick(Sender: TObject);
    procedure SumCalClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.EvalDiffClick(Sender: TObject);
var s:string;
begin
    Math.Diff(DiffExpress.Text,DiffVariable.Text[1],s);
    DiffResult.Text:=s;
end;

procedure TForm1.EvalCalClick(Sender: TObject);
var
    f:extended;
    val1,res:real;
begin
    trystrtofloat(EvalVal1.Text,f);
    val1:=f;
    Math.Evaluate(EvalExp.Text,EvalVar1.Text[1],val1,res);
    EvalResult.Text:=FloattoStr(res);
end;

procedure TForm1.SumCalClick(Sender: TObject);
var
    TempVar,TempResult:array[0..100]of real;
    i,Count,from,too:integer;
    S:real;
begin
    from:=strtoint(SumFrom.Text);
    too:=strtoint(SumTo.Text);
    Count:=0;
    for i:=from to too do begin
        TempVar[Count]:=i;
        inc(Count);
    end;
    Math.Evaluate(SumExp.Text,SumVar.text[1],Count,TempVar,TempResult);

    S:=0;
    for i:=0 to Count-1 do 
        S:=S+TempResult[i];
    SumResult.Text:=floattostr(S);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
    Form2.ShowModal;
end;

end.
