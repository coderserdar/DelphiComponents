unit MyMath;

interface

function arccos(x:extended):extended;
function arcsin(x:extended):extended;
function arctan2(a,b:extended):extended;
function tan(x:extended):extended;

implementation

function Tan(x:extended):extended;
begin
  Result:=sin(x)/cos(x);
end;

function ArcTan2(a,b:extended):extended;
begin
  Result:=ArcTan(a/b);
  if b<0 then
    Result:=Result+pi;
end;

function ArcSin(x:extended):extended;
begin
  Result:=ArcTan(x/sqrt(1-x*x));
end;

function ArcCos(x:extended):extended;
begin
  Result:=pi/2-ArcSin(x);
end;

end.
