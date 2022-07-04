
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, NCOci, NCOciWrapper, NCOciDB, Grids, DBGrids, Buttons, StdCtrls,
  ComCtrls, NCOciFilter, ExtCtrls, DBCtrls
{$IFDEF VER140}
  , Variants
{$ENDIF}
  ;


type
  TForm1 = class(TForm)
    OCIDatabase1: TOCIDatabase;
    OCIQuery1: TOCIQuery;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    mm: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Panel1: TPanel;
    Edit1: TEdit;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    Label3: TLabel;
    DBNavigator1: TDBNavigator;
    TabSheet3: TTabSheet;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    Edit3: TEdit;
    Button5: TButton;
    TabSheet4: TTabSheet;
    Button6: TButton;
    Label4: TLabel;
    Label5: TLabel;
    Edit4: TEdit;
    Label6: TLabel;
    Edit5: TEdit;
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
    FExpr: TOCIExpression;
begin
    with TOCIExpressionParser.Create do
    try
        FExpr := BuildExpression(OCIQuery1, Edit1.Text, [],
            [poExtSyntax, poNaturalLang{, poAggregate, poDefaultExpr, poFieldNameGiven}], '');
        FExpr.Dump(mm.Lines);
        FExpr.Free;
    finally
        Free;
    end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
    OCIQuery1.Filter := Edit1.Text;
    OCIQuery1.FindFirst;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
    OCIQuery1.Filter := Edit1.Text;
    OCIQuery1.FindPrior;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
    OCIQuery1.Filter := Edit1.Text;
    OCIQuery1.FindNext;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
    OCIQuery1.Filter := Edit1.Text;
    OCIQuery1.FindLast;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
    i: Integer;
    w: dword;
begin
    OCIQuery1.Active := True;
    w := GetTickCount;
    for i := 0 to 1000 do begin
        if Edit4.Text <> '' then
            OCIQuery1.Locate(Edit2.Text + ';' + Edit4.Text, VarArrayOf([Edit3.Text, Edit5.Text]), [loCaseInsensitive, loPartialKey])
        else
            OCIQuery1.Locate(Edit2.Text, Edit3.Text, [loCaseInsensitive, loPartialKey]);
    end;
    w := GetTickCount - w;
    ShowMessage(FloatToStr(w / 1000.0));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    OCIQuery1.Open;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
    FExpr: TOCIExpression;
begin
    with TOCIExpressionParser.Create do
    try
        FExpr := BuildExpression(OCIQuery1, Edit1.Text, [],
            [poExtSyntax, poNaturalLang, {poAggregate,} poDefaultExpr{, poFieldNameGiven}], '');
        Label4.Caption := FExpr.Execute;
        FExpr.Free;
    finally
        Free;
    end;
end;

end.

