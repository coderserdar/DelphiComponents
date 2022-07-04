unit ipsFormEditUtils;

interface
uses Classes, TypInfo;

procedure FillEnumList(List:TStrings; ListClass:TClass; Prop:String);

implementation



procedure FillEnumList(List:TStrings; ListClass:TClass; Prop:String);
var PI : PPropInfo;
    TI : PTypeInfo;
    TD : PTypeData;
    i  : Integer;
begin
  PI:=GetPropInfo(ListClass.ClassInfo,Prop);
  TI:=PI^.PropType^;
  TD:=GetTypeData(TI);
  List.Clear;
  if TI^.Kind=tkEnumeration then
        for i:=TD^.MinValue to TD^.MaxValue do
            List.Add( GetEnumName(TI,i));
end;


end.
