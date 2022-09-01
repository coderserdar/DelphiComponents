unit bvRes;

interface

uses classes,sysutils;

procedure bvWriteComponentTXTFile(const FileName: string; Instance: TComponent);

function bvReadComponentTXTFile(const FileName: string; Instance: TComponent): TComponent;

implementation

procedure bvWriteComponentTXTFile(const FileName: string; Instance: TComponent);
var
  Stream: TMemoryStream;
  Stream1:TFileStream;
begin
  Stream := TMemoryStream.create;
  try
    Stream.WriteComponent(Instance);

    Stream1:=TFileStream.create(Filename, fmCreate);
    try
      Stream.position:=0;
      ObjectBinaryToText(Stream,Stream1);
    finally
      Stream1.free
    end;
  finally
    Stream.Free;
  end;
end;

function bvReadComponentTXTFile(const FileName: string; Instance: TComponent): TComponent;

var
  Stream: TMemoryStream;
  Stream1:TFileStream;
begin

  Stream1:=TFileStream.create(Filename,  fmOpenRead or fmShareDenyNone);
  try




    Stream := TMemoryStream.create;
    try
      Stream1.position:=0;
      ObjectTextToBinary(Stream1,Stream);

      Stream.position:=0;
      Result := Stream.ReadComponent(Instance);
    finally
      Stream.Free;
    end;

  finally
    Stream1.free
  end;

end;

end.
