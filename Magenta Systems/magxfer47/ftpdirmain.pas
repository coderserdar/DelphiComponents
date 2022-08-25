unit ftpdirmain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OverbyteIcsWndControl,
  OverbyteIcsFtpCli, MagentaFtp3, MagentaCopy, MagSubs1 ;

type
  TFormMain = class(TForm)
    FtpLines: TMemo;
    FtpResult: TMemo;
    Label1: TLabel;
    doParse: TButton;
    doExit: TButton;
    doClear: TButton;
    procedure doExitClick(Sender: TObject);
    procedure doParseClick(Sender: TObject);
    procedure doClearClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.doClearClick(Sender: TObject);
begin
    FtpLines.Lines.Clear ;
    FtpResult.Lines.Clear ;
end;

procedure TFormMain.doExitClick(Sender: TObject);
begin
    Close ;
end;

procedure TFormMain.doParseClick(Sender: TObject);
var
    RemDir, BaseDir: string;
    Level: integer ;
    HostType: THostType;
    TotFiles: integer;
    RemFiles: TFileRecs ;
    MagFtp: TMagFtp  ;
    DirStream: TStream ;
    DCodePage: Cardinal ;
begin
    DirStream := TMemoryStream.Create ;
    MagFtp := TMagFtp.Create (self) ;
    try
    SetLength (RemFiles, 0) ;
    totfiles := 0 ;
    RemDir := '' ;
    BaseDir := '' ;
    HostType := FTPTYPE_NONE ;
    Level := 0 ;
    DCodePage := 0 ; 
    FtpLines.Lines.SaveToStream (DirStream) ;
    MagFtp.UnpackFtpFDir (DirStream, RemDir, BaseDir, Level, DCodePage, HostType, TotFiles, RemFiles) ;
    if TotFiles = 0 then
    begin
        FtpResult.Lines.Add ('No files parsed') ;
    end
    else
    begin
        FtpResult.Lines.Add ('Parsed Directory: ' + CRLF_ + FmtFileDir (RemFiles, false)) ;
    end;
    finally
        DirStream.Free ;
        MagFtp.Free ;
    end;
end;

end.
