create or replace package test_refcrs is
    type TCrs is ref cursor return product%rowtype;
    procedure P(C in out TCrs);
end;
/

create or replace package body test_refcrs is
    procedure P(C in out TCrs) is
    begin
        open C for select * from product order by product_id;
    end;
end;
/

set serveroutput on

declare
    c test_refcrs.TCrs;
    r product%Rowtype;
begin
    test_refcrs.P(c);
    loop
        exit when not c%found;
        fetch c into r;
        dbms_output.put_line(r.description);
    end loop;
    close c;
end;
/