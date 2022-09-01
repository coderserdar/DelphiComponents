unit bvRegisterUnit;

interface

uses classes,bvDBGrid,
     bvBookmark,bvColorEdit,bvFindUnit,bvdbGridSaver,
     {$ifndef LINUX}
     bvdbTableSaver,
     {$endif}
     bvFormSaver{,bvdbTablePrinter};

procedure Register;

implementation

const bvSoftPage='bvSoft';

procedure Register;
begin
  RegisterComponents(bvSoftPage, [TbvDBGrid]);
  RegisterComponents(bvSoftPage, [TDBGridSaver]);
  {$ifndef LINUX}
  RegisterComponents(bvSoftPage, [TDBTableSaver]);
  {$endif}
  RegisterComponents(bvSoftPage, [bv_Find]);
  RegisterComponents(bvSoftPage, [TBVBookMark]);
  //RegisterComponents(bvSoftPage, [TDBTablePrinter]);
  RegisterComponents(bvSoftPage, [TbvColorEdit]);
  RegisterComponents(bvSoftPage, [TbvFormSaver]);
end;

end.
