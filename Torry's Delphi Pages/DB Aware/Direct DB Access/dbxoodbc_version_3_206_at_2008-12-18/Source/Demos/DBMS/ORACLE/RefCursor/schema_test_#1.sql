CREATE OR REPLACE PROCEDURE test_proc_refcursor_a(
       rc1 OUT sys_refcursor,
       rc2 OUT sys_refcursor
    ) AS
BEGIN
  OPEN rc1 FOR
    select
      cast(1 as numeric(11)) as f_num1
    from dual
  ;
  OPEN rc2 FOR
    select
      cast(2 as numeric(11)) as f_num2,
      cast(3 as numeric(7)) as f_num3,
      cast('OK' as varchar2(64)) as f_varchar64
    from dual
  ;
END;
