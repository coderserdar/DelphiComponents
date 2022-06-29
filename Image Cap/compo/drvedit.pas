unit drvedit;

interface



procedure Register;

implementation

 uses classes,DesignEditors,DesignIntf,sysutils,vfw,videocap;

// Property Editor for driver selection   in Video-Cap
type TDrivereditor= class (TPropertyEditor)
  function GetAttributes:TPropertyAttributes;override;
  procedure GetValues(Proc: TGetStrProc);override;
  function GetValue:string;override;
  procedure SetValue(const Value: string);override;
end;

function TDriverEditor.GetAttributes:TPropertyAttributes;
begin
 result :=[paRevertable,paValueList];
end;


procedure TDriverEditor.GetValues(Proc: TGetStrProc);
var i:integer;
    name:array[0..80] of char;
    ver :array[0..80] of char;
    s:string;

begin
 for i:= 0 to 9 do
  begin
   if capGetDriverDescription( i,name,80,ver,80) then
      s:=strpas(name)
   else
      s:='' ;
   proc(s);
  end;

end;



function TDriverEditor.GetValue:string;

var  n:array[0..80] of char;
     ver :array[0..80] of char;
     s:string;


begin
 with Getcomponent(0) as TVideoCap do
   begin
    if capGetDriverDescription( DriverIndex,n,80,ver,80) then
      s:=strpas(n)
    else
      s:='' ;
   end;
   result:= s;               //fVideoDrivername;
end;



procedure TDriverEditor.SetValue(const Value: string);
begin
  with Getcomponent(0) as TVideoCap do
    SetDrivername( value) ;
  Modified;
end;






procedure Register;
begin

  RegisterPropertyEditor(TypeInfo(string),TVideoCap,'DriverName',TDriverEditor);
end;

end.
