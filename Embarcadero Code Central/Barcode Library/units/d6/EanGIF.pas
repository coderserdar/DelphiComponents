unit EanGIF;

interface


implementation

uses classes;

const HT_SIZE=1000;
var   HT : TList;

procedure EncodeLine(LineNo:Integer);
var i:Integer;
begin
    HT:= TList.Create;
    try
      for i := 0 to HT_SIZE - 1 do HT.Add(nil);

    finally
      HT.Free;
    end;

end;

end.
