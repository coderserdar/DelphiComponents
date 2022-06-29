unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    BtnGzip: TButton;
    OpenDialog: TOpenDialog;
    procedure BtnGzipClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure CompressFile(const ASourceFile, ADestinationFile: string);
    procedure CompressFileDirect(const ASourceFile, ADestinationFile: string);
    procedure DecompressFile(const ASourceFile, ADestinationFile: string);
    procedure DecompressFileDirect(const ASourceFile, ADestinationFile: string);
  end;

var
  Form1: TForm1;

implementation

uses
  dateutils,
  ZLib,
  ZLibPas;

{$R *.dfm}

procedure TForm1.BtnGzipClick(Sender: TObject);
var
  AFileName: string;
begin
  if not OpenDialog.Execute then exit;
  AFileName := OpenDialog.FileName;
  //AFileName := Application.ExeName;
  CompressFile(AFileName, AFileName + '.gz');
  DecompressFile(AFileName + '.gz', AFileName + '.orig');
end;

procedure TForm1.CompressFile(const ASourceFile, ADestinationFile: string);
var
  S : TStream;
  D : TStream;
begin
  S := nil; D := nil;
  try
    S := TFileStream.Create(ASourceFile, fmOpenRead or fmShareDenyWrite);
    D := TFileStream.Create(ADestinationFile, fmCreate);

    zlib.CompressStream(S, D, clMax, zlibpas.zsGZip);

  finally
    FreeAndNil(D);
    FreeAndNil(S);
  end;
end;

procedure TForm1.CompressFileDirect(const ASourceFile, ADestinationFile: string);
var
  S, D : TMemoryStream;
begin
  S := nil; D := nil;
  try
    S := TMemoryStream.Create;
    S.LoadFromFile(ASourceFile);
    D := TMemoryStream.Create;
    zlib.CompressStreamEx(S, D, clMax, zlibpas.zsGZip);
    D.SaveToFile(ADestinationFile);
  finally
    FreeAndNil(D);
    FreeAndNil(S);
  end;

end;

procedure TForm1.DecompressFile(const ASourceFile, ADestinationFile: string);
var
  S : TStream;
  D : TStream;
begin
  S := nil; D := nil; 
  try
    S := TFileStream.Create(ASourceFile, fmOpenRead or fmShareDenyWrite);
    D := TFileStream.Create(ADestinationFile, fmCreate);

    zlib.DecompressStream(S, D);
  finally
    FreeAndNil(D);
    FreeAndNil(S);
  end;
end;

procedure TForm1.DecompressFileDirect(const ASourceFile, ADestinationFile: string);
var
  S, D : TMemoryStream;
begin
  try
    S := TMemoryStream.Create;
    S.LoadFromFile(ASourceFile);
    D := TMemoryStream.Create;

    DecompressStream(S, D);

    D.SaveToFile(ADestinationFile);
  finally
    FreeAndNil(D);
    FreeAndNil(S);
  end;
end;

end.
