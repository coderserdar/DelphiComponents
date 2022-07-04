unit RegDSADO;

interface
       
uses Classes, DMaster, DDB, DTables;

procedure Register;

implementation

procedure Register;
begin
     RegisterComponents('DeerSoft', [TDMaster,
                                     TDTable,
                                     TDQuery,
                                     TDUpdateSQL]);
end;


end.
