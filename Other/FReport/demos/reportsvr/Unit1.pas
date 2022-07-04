unit Unit1;

interface

uses
  SysUtils, Classes, HTTPApp, DBXpress, FMTBcd,

  RLReport, RLFilters, RLHTMLFilter;

type
  TWebModule1 = class(TWebModule)
    procedure WebModule1WebActionItem1Action(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1WebActionItem2Action(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SendReport(Report: TRLReport; Response: TWebResponse);
  end;

var
  WebModule1: TWebModule1;

implementation

uses
  Unit2, Unit3;

{$R *.DFM}

procedure TWebModule1.SendReport(Report:TRLReport; Response:TWebResponse);
var
  s:TFileStream;
  f:TRLHTMLFilter;
begin
  // prepara as páginas em background (ShowProgress=false)
  Report.Prepare;

  f:=TRLHTMLFilter.Create(nil);
  try
    Randomize;
    
    // aqui deve-se utilizar um algorítmo melhor para a escolha do nome de arquivo temporário
    f.FileName:='T'+IntToStr(Random(MAXINT))+'.tmp';

    // manda as páginas para o arquivo temporário html
    FilterPages(Report.Pages,f);

    // carrega o arquivo na stream e posiciona no início
    s:=TFileStream.Create(f.FileName,fmOpenRead);
    s.Seek(0,soFromBeginning);
    
    // envia o stream como resposta
    Response.ContentStream:=s;
    Response.SendResponse;

    //
    DeleteFile(f.FileName);
  finally
    f.free;
  end;
end;

procedure TWebModule1.WebModule1WebActionItem1Action(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  with TForm2.Create(nil) do
    try
      SendReport(RLReport1,Response);
    finally
      free;
    end;
  Handled:=true;
end;

procedure TWebModule1.WebModule1WebActionItem2Action(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  with TForm3.Create(nil) do
    try
      SendReport(RLReport1,Response);
    finally
      free;
    end;
  Handled:=true;
end;

end.

