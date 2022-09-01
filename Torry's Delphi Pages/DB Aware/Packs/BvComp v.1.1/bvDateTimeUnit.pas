unit bvDateTimeUnit;

interface

function bvDateTimeToDate(pDate:TDateTime):TDatetime;

implementation

uses sysutils;

function bvDateTimeToDate(pDate:TDateTime):TDatetime;
var Year,Month,Day:Word;
begin
  DecodeDate(pDate,year,month,day);
  Result:=EncodeDate(year,month,day)
end;

end.
