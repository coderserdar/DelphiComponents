//  FastReport 2.3 demo reports.

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, DB, FR_Class, FR_E_TXT, FR_E_HTM, FR_E_CSV,
  FR_E_RTF, FR_Desgn, FR_Shape, FR_OLE, FR_BarC, FR_Rich, FR_RRect; 
                                             
type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Group1: TRadioGroup;
    frCompositeReport1: TfrCompositeReport;
    frTextExport1: TfrTextExport;
    frCSVExport1: TfrCSVExport;
    frHTMExport1: TfrHTMExport;
    Label3: TLabel;
    frRTFExport1: TfrRTFExport;
    frDesigner1: TfrDesigner;
    frOLEObject1: TfrOLEObject;
    frShapeObject1: TfrShapeObject;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    RB1: TRadioButton;
    RB2: TRadioButton;
    frBarCodeObject1: TfrBarCodeObject;
    frRichObject1: TfrRichObject;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Group1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function CurDoc: Integer;
  end;

var
  Form1: TForm1;

implementation

uses DataSets, Unit3, Unit4, Unit5, Unit6, Unit7, Unit8, Unit9;

{$R *.DFM}

var
  DocArr: Array[0..13] of TfrReport;
  Path: String;

{-----------------------------------------------------------------------------}
function TForm1.CurDoc:Integer;
begin
  Result := Group1.ItemIndex;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  i: Integer;
begin
  DocArr[0] := Form3.frReport1; DocArr[1] := Form3.frReport2;
  DocArr[2] := Form4.frReport1; DocArr[3] := Form3.frReport3;
  DocArr[4] := Form5.frReport1; DocArr[5] := Form6.frReport1;
  DocArr[6] := Form7.frReport1; DocArr[7] := frCompositeReport1;
  DocArr[8] := Form6.frReport1; DocArr[9] := Form3.frReport4;
  DocArr[10]:= Form8.frReport1; DocArr[11]:= Form8.frReport2;
  DocArr[12]:= Form3.frReport5; DocArr[13]:= Form3.frReport6;
  with frCompositeReport1 do
  begin
    Reports.Clear;
    for i := 0 to 5 do
      Reports.Add(DocArr[i]);
    DoublePass := True;
  end;
  Path := ExtractFilePath(ParamStr(0));
  Group1Click(nil);
end;

procedure TForm1.Group1Click(Sender: TObject);
begin
  if CurDoc <> 7 then
  begin
    Button1.Enabled := True;
    Button2.Enabled := True;
    DocArr[CurDoc].LoadFromFile(Path + 'Doc' + IntToStr(CurDoc) + '.frf');
    DocArr[CurDoc].ModalPreview := False;
  end
  else
  begin
    Button1.Enabled := False;
    Button2.Enabled := False;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  DocArr[CurDoc].DesignReport;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  DocArr[CurDoc].SaveToFile(Path + 'Doc' + IntToStr(CurDoc) + '.frf');
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 13 do
    if RB1.Checked then
      DocArr[i].Preview := nil else
      DocArr[i].Preview := Form9.frPreview1;

  if CurDoc = 4 then
    Form5.StartReport
  else if CurDoc = 6 then
    Form7.StartReport
  else if CurDoc = 7 then
  begin
    for i := 0 to 6 do
      DocArr[i].LoadFromFile(Path + 'Doc' + IntToStr(i) + '.frf');
    DocArr[CurDoc].ShowReport;
  end
  else DocArr[CurDoc].ShowReport;

  if RB2.Checked then
    Form9.ShowModal;
end;

end.
