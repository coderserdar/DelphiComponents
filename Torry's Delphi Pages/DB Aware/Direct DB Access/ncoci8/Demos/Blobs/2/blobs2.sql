create table test_blob2 (
  f1 number(10),
  f2 BLOB
);

create or replace procedure fetch_blob(Af1 in number, Af2 out BLOB) is
begin
  select f2 into Af2 from test_blob2 where f1 = Af1 for update nowait;
end;
/
