unit ConverterUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SXSkinLibrary;

type
  TForm1 = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function FmtBytes(const S:String):String; overload;
var A,B:Integer;
begin
 if S='' then
  begin
   Result:='';
   exit;
  end;
 SetLength(Result,length(S)+(length(S)-1) div 3);
 A:=-1; B:=length(S);
 repeat
  Inc(A);
  if (A+1) mod 4=0 then Result[length(Result)-A]:=' ' else
   begin
    Result[length(Result)-A]:=S[B];
    Dec(B);
   end;
 until A=length(Result)-1;
end;

function FmtBytes(C:Int64):String; overload;
begin
 Result:=FmtBytes(inttostr(C));
end;

function BytesToKBorMBE(A:Int64;Precision:Integer):String;
var SType:Byte;
        B:Double;
        C:Integer;
        N:Int64;
begin
 N:=A;
 SType:=1;
 while A>=1024 do
  begin
   Inc(SType);
   A:=A div 1024;
  end;
 Result:=FmtBytes(A);
 case SType of
  2:   B:=N/1024-A;
  3:   B:=N/1024/1024-A;
  4:   B:=N/1024/1024/1024-A;
  5:   B:=N/1024/1024/1024/1024-A;
  else B:=0;
 end;
 if SType<>1 then
  begin
   Dec(Precision,length(Result));
   if Precision>0 then
    begin
     Result:=Result+'.';
     for C:=0 to Precision-1 do
      begin
       Result:=Result+inttostr(Trunc(B*10));
       B:=B*10-Trunc(B*10);
      end;
     repeat
      if Result='' then break;
      if Result[length(Result)]='0' then Delete(Result,length(Result),1);
      if Result[length(Result)]='.' then
       begin
        Delete(Result,length(Result),1);
        break;
       end;
      if not (Result[length(Result)] in ['0','.']) then break;
     until False;
    end;
  end;
 case SType of
  1: Result:=Result+' b';
  2: Result:=Result+' Kb';
  3: Result:=Result+' Mb';
  4: Result:=Result+' Gb';
  5: Result:=Result+' Tb';
 end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var    S:String;
       L:TSXSkinLibrary;
       A:Cardinal;
 SZ1,SZ2:Integer;
      FF:File of Byte;
 Q1,Q2,F:Int64;
begin
 OpenDialog1.InitialDir:=ExtractFilePath(Application.ExeName);
 if OpenDialog1.Execute then
  begin
   L:=TSXSkinLibrary.Create(nil);
   try
    L.LoadFromFile(OpenDialog1.FileName);
    S:=ExtractFileExt(OpenDialog1.FileName);
    S:=Copy(OpenDialog1.FileName,1,length(OpenDialog1.FileName)-length(S))+'.sxs';
    L.SaveToSXSFile(S);
   finally
    L.Free;
   end;
   //INI file size
   AssignFile(FF,OpenDialog1.FileName);
   Reset(FF);
   SZ1:=FileSize(FF);
   Label7.Caption:=BytesToKBorMBE(SZ1,4);
   CloseFile(FF);
   //SXS file size
   AssignFile(FF,S);
   Reset(FF);
   SZ2:=FileSize(FF);
   Label8.Caption:=BytesToKBorMBE(SZ2,4);
   CloseFile(FF);
   if SZ1>0 then
    Label10.Caption:=inttostr(round(SZ2/SZ1*100))+'%';
   Application.ProcessMessages;
   //SXS loading time
   L:=TSXSkinLibrary.Create(nil);
   try
    QueryPerformanceCounter(Q1);
    for A:=1 to 100 do
     begin
      L.LoadFromFile(S);
      L.LoadFromFile('');
     end;
    QueryPerformanceCounter(Q2);
    QueryPerformanceFrequency(F);
    Label5.Caption:=FloatToStrF((Q2-Q1)/F*10,ffNumber,4,4)+' msec';
   finally
    L.Free;
   end;
   Application.ProcessMessages;
   //INI loading time
   S:=OpenDialog1.FileName;
   L:=TSXSkinLibrary.Create(nil);
   try
    QueryPerformanceCounter(Q1);
    for A:=1 to 2 do
     begin
      L.LoadFromFile(S);
      L.LoadFromFile('');
     end;
    QueryPerformanceCounter(Q2);
    QueryPerformanceFrequency(F);
    Label4.Caption:=FloatToStrF((Q2-Q1)/F*500,ffNumber,4,4)+' msec';
   finally
    L.Free;
   end;
  end;
end;

end.
