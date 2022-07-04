create or replace package pkg_test_dbxoodbc_01 is

PROCEDURE "Test_Proc_RefCursor_A"(
       rc1 OUT sys_refcursor,
       rc2 OUT sys_refcursor
    );

PROCEDURE test_proc_refcursor_a(
       rc1 OUT sys_refcursor,
       rc2 OUT sys_refcursor
    );

PROCEDURE test_proc_refcursor_b(
       rc1 OUT sys_refcursor,
       rc2 OUT sys_refcursor
    );
                         
end pkg_test_dbxoodbc_01;
/
create or replace package body pkg_test_dbxoodbc_01 is

PROCEDURE "Test_Proc_RefCursor_A"(
       rc1 OUT sys_refcursor,
       rc2 OUT sys_refcursor
    ) AS
BEGIN
  OPEN rc1 FOR
    select
      'case sensitivity proc name'as f_str1
    from dual
  ;
  OPEN rc2 FOR
    select
      cast(9 as numeric(11)) as f_num2,
      cast(10 as numeric(7)) as f_num3,
      cast('PACKAGE(Test_Proc_RefCursor_A)==OK' as varchar2(64)) as f_varchar64
    from dual
  ;
END;

PROCEDURE test_proc_refcursor_a(
       rc1 OUT sys_refcursor,
       rc2 OUT sys_refcursor
    ) AS
BEGIN
  OPEN rc1 FOR
    select
      cast(4 as numeric(11)) as f_num1
    from dual
  ;
  OPEN rc2 FOR
    select
      cast(5 as numeric(11)) as f_num2,
      cast(6 as numeric(7)) as f_num3,
      cast('PACKAGE(test_proc_refcursor_a)==OK' as varchar2(64)) as f_varchar64
    from dual
  ;
END;

PROCEDURE test_proc_refcursor_b(
       rc1 OUT sys_refcursor,
       rc2 OUT sys_refcursor
    ) AS
BEGIN
  OPEN rc1 FOR
    select
      cast(4 as numeric(11)) as f_num1
    from dual
  ;
  OPEN rc2 FOR
    select
      cast(7 as numeric(11)) as f_num2,
      cast(8 as numeric(7)) as f_num3,
      cast('PACKAGE(test_proc_refcursor_b)==OK' as varchar2(64)) as f_varchar64
    from dual
  ;
END;

end pkg_test_dbxoodbc_01;
/
