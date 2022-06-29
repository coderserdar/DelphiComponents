unit EanReportBuilderReg;

interface

uses Classes;

procedure Register;

implementation

uses EanKod,EanRB, EanRBDb;

{$R EanRB.Res}

procedure Register;
begin
     RegisterNoIcon([TRbEan,TRbDBEan]);
end;

end.
