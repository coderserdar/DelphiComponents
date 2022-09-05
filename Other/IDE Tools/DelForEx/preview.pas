unit Preview;
{*******************************
If you change any text in
this memo the AFTER memo
is updated.
*********************************}

interface
uses Windows, SysUtils, Classes,
  Graphics, Forms;

type
  TClass = class(TObject)
  private
    procedure M1(S: Integer); virtual;
  public
    function Foo: Integer;
  end;

implementation

function Foo: Integer;
begin
  Result := 0;
end;

procedure TClass.M1(S: Integer);
  procedure SubProc;
  begin
    S := 9;
  end;
const
  J = 10;
var
  J: Double;
begin
  if S > J then
  begin
    SubProc;
  end
  else
  begin
    S := Foo;
  end;
end;
end.
