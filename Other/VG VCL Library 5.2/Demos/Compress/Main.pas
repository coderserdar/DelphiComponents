unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TMainForm = class(TForm)
    meSrc: TMemo;
    cmTestV: TButton;
    meDest: TMemo;
    cmStreams: TButton;
    procedure cmTestVClick(Sender: TObject);
    procedure cmStreamsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation
uses vgUtils, vgSystem, vgOleUtl, vgLZ;

{$R *.DFM}

const
  Sign    = SignLZ;

procedure TMainForm.cmStreamsClick(Sender: TObject);
var
  S: string;
  C, D: TMemoryStream;
begin
  C := TMemoryStream.Create;
  try
    S := meSrc.Lines.Text;
    Compress(Sign, C, S[1], Length(S), nil);

    D := TMemoryStream.Create;
    try
      UnCompress(Sign, D, C.Memory^, Length(S), nil);
      Move(D.Memory^, S[1], D.Size);
    finally
      D.Free;
    end;
    meDest.Lines.Text := S; 
  finally
    C.Free;
  end;
end;

procedure TMainForm.cmTestVClick(Sender: TObject);
var
  C, D: Variant;
begin
  C := CompressVariant(Sign, meSrc.Lines.Text, nil);
  D := UncompressVariant(C, nil);
  meDest.Text := D;
end;

end.
