unit UnitDesign;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DB, RLReport, DBCtrls;

type
  TFormDesign = class(TForm)
    RLReport1: TRLReport;
    RLDetailGrid1: TRLDetailGrid;
    RLBand1: TRLBand;
    RLBand2: TRLBand;
    RLSystemInfo1: TRLSystemInfo;
    RLLabel1: TRLLabel;
    RLLabel2: TRLLabel;
    RLText2: TRLLabel;
    RLLabel3: TRLLabel;
    RLText1: TRLLabel;
    RLText3: TRLLabel;
    RLText4: TRLLabel;
    RLImage1: TRLImage;
    RLBand3: TRLBand;
    RLLabel4: TRLLabel;
    procedure RLReport1BeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLReport1AfterPrint(Sender: TObject);
    procedure RLReport1NeedData(Sender: TObject; var MoreData: Boolean);
    procedure RLLabel3BeforePrint(Sender: TObject; var Text: String;
      var PrintIt: Boolean);
    procedure RLText3BeforePrint(Sender: TObject; var Text: String;
      var PrintIt: Boolean);
    procedure RLText4BeforePrint(Sender: TObject; var Text: String;
      var PrintIt: Boolean);
    procedure RLText1BeforePrint(Sender: TObject; var Text: String;
      var PrintIt: Boolean);
    procedure RLReport1DataCount(Sender: TObject; var DataCount: Integer);
    procedure RLImage1BeforePrint(Sender: TObject; var PrintIt: Boolean);
  private
    { Private declarations }
    hand:textfile;
  public
    { Public declarations }
    d:TRLDBText;
    wSPECIES_NO  :string;
    wCATEGORY    :string;
    wCOMMON_NAME :string;
    wSPECIES_NAME:string;
    wLENGTH_CM   :string;
    wLENGTH_IN   :string;
    wNOTES       :string;
    wGRAPHIC     :string;
//    procedure ProduceFile;
  end;

var
  FormDesign: TFormDesign;

implementation

{$R *.dfm}

var
  digits:array[0..15] of char='0123456789ABCDEF';

function bytetohex(n:byte):string;
begin
  result:=digits[n and $0f]+digits[n and $f0 shr 4];
end;

function hextobyte(const h:string):byte;
begin
  result:=(pos(h[1],digits)-1)+(pos(h[2],digits)-1) shl 4;
end;

function stringtohex(const s:string):string;
var
  i:integer;
  c:string;
begin
  try
  setlength(result,length(s)*2);
  for i:=1 to length(s) do
  begin
    c:=bytetohex(ord(s[i]));
    move(c[1],result[(i-1)*2+1],2);
  end;
  except
    showmessage('pau1');
  end;
end;

function hextostring(const s:string):string;
var
  i:integer;
begin
  try
  setlength(result,length(s) div 2);
  for i:=1 to length(result) do
    result[i]:=char(hextobyte(copy(s,(i-1)*2+1,2)));
  except
    showmessage('pau2');
  end;
end;

procedure TFormDesign.RLReport1BeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  FileMode:=fmOpenRead+fmShareDenyNone;
  AssignFile(hand,ExtractFilePath(Application.ExeName)+'biolife.dat');
  Reset(hand);
  tag:=0;
end;

procedure TFormDesign.RLReport1AfterPrint(Sender: TObject);
begin
  CloseFile(hand);
end;

{procedure TForm2.ProduceFile;
var
  s:TStringStream;
  i:TDBImage;
begin
  AssignFile(hand,ExtractFilePath(Application.ExeName)+'biolife.dat');
  Rewrite(hand);
  try
    SQLClientDataSet1.Open;
    while not SQLClientDataSet1.Eof do
    begin
      s:=TStringStream.Create('');
      try
        i:=TDBImage.Create(nil);
        i.DataSource:=DataSource1;
        i.DataField :='GRAPHIC';
        i.LoadPicture;
        i.Picture.SaveToStream(s);
        i.free;

        writeln(hand,stringtohex(SQLClientDataSet1SPECIES_NO.AsString)+#9+
                     stringtohex(SQLClientDataSet1CATEGORY.AsString)+#9+
                     stringtohex(SQLClientDataSet1COMMON_NAME.AsString)+#9+
                     stringtohex(SQLClientDataSet1SPECIES_NAME.AsString)+#9+
                     stringtohex(SQLClientDataSet1LENGTH_CM.AsString)+#9+
                     stringtohex(SQLClientDataSet1LENGTH_IN.AsString)+#9+
                     stringtohex(SQLClientDataSet1NOTES.AsString)+#9+
                     stringtohex(s.DataString));


      finally
        s.free;
      end;
      SQLClientDataSet1.Next;
    end;

  finally
    CloseFile(hand);
  end;
end;
}

procedure TFormDesign.RLReport1NeedData(Sender: TObject; var MoreData: Boolean);
var
  s:string;
function next:string;
var
  i:integer;
begin
  i:=pos(#9,s);
  if i=0 then
    i:=length(s)+1;
  result:=copy(s,1,i-1);
  delete(s,1,i);  
end;
begin
  MoreData:=not Eof(hand);
  if MoreData then
    ReadLn(hand,s);

  wSPECIES_NO  :=hextostring(next);
  wCATEGORY    :=hextostring(next);
  wCOMMON_NAME :=hextostring(next);
  wSPECIES_NAME:=hextostring(next);
  wLENGTH_CM   :=hextostring(next);
  wLENGTH_IN   :=hextostring(next);
  wNOTES       :=hextostring(next);
  wGRAPHIC     :=hextostring(next);
end;

procedure TFormDesign.RLLabel3BeforePrint(Sender: TObject; var Text: String;
  var PrintIt: Boolean);
begin
  Text:='Species No: '+wSPECIES_NO;
end;

procedure TFormDesign.RLText3BeforePrint(Sender: TObject; var Text: String;
  var PrintIt: Boolean);
begin
  Text:=wCATEGORY;
end;

procedure TFormDesign.RLText4BeforePrint(Sender: TObject; var Text: String;
  var PrintIt: Boolean);
begin
  Text:=wLENGTH_IN;
end;

procedure TFormDesign.RLText1BeforePrint(Sender: TObject; var Text: String;
  var PrintIt: Boolean);
begin
  Text:=wCOMMON_NAME;
end;

procedure TFormDesign.RLReport1DataCount(Sender: TObject;
  var DataCount: Integer);
var
  h:textfile;
  s:string;
  n:string;
begin
  n:=ExtractFilePath(Application.ExeName)+'biolife.dat';

  DataCount:=0;
  AssignFile(h,n);
  FileMode:=fmOpenRead+fmShareDenyNone;
  Reset(h);
  try
    while not Eof(h) do
    begin
      ReadLn(h,s);
      inc(DataCount);
    end;  
  finally
    CloseFile(h);
  end;
end;

procedure TFormDesign.RLImage1BeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  s:TStringStream;
begin
  s:=TStringStream.Create(wGRAPHIC);
  s.Seek(0,0);
  RLImage1.Picture.Bitmap.LoadFromStream(s);
  s.free;
end;

end.

