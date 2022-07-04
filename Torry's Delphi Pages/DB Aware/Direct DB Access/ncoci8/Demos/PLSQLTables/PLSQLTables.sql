create or replace package test_tab is
    type TOCIVC2Tbl is table of varchar2(50) index by binary_integer;
    procedure TansformArray(ATable in out TOCIVC2Tbl);
end;
/
create or replace package body test_tab is
    procedure TansformArray(ATable in out TOCIVC2Tbl) is
      tmp binary_integer;      
    begin
        for i in ATable.First .. round(ATable.Last / 2) loop
            tmp := ATable(ATable.First + (ATable.Last - i));
            ATable(ATable.First + (ATable.Last - i)) := ATable(i);
            ATable(i) := tmp;
        end loop;
    end;
end;
/

