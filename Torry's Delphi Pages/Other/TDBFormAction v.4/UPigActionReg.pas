unit UPigActionReg;

interface

uses Classes, DBFrmActn, {DBQryActn,} ActnList;

procedure Register;

implementation

procedure Register;
begin
  RegisterActions('DataSet', [TDBFormAction], nil);
  //RegisterActions('DataSet',[TQueryAction], nil);
end;


end.
