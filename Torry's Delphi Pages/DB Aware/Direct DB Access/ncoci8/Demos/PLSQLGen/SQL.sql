DECLARE
  c integer;
  cust_id NUMBER;
  cust_name VARCHAR2(45);
  cust_address VARCHAR2(40);
  cust_cred NUMBER;
  cust_comm VARCHAR2(256);
  rn integer;
BEGIN
  c := dbms_sql.open_cursor;
  dbms_sql.parse(c,
    'SELECT' ||
    ' customer_id,' ||    -- num(6)
    ' name,' ||           -- vc2(45)
    ' address,' ||        -- vc2(40)
    ' credit_limit,' ||   -- num(9,2)
    ' comments ' ||       -- long
    'FROM customer WHERE customer_id = :ID', dbms_sql.native);
  dbms_sql.define_column(c, 1, cust_id);
  dbms_sql.define_column(c, 2, cust_name, 45);
  dbms_sql.define_column(c, 3, cust_address, 40);
  dbms_sql.define_column(c, 4, cust_cred);
  dbms_sql.define_column(c, 5, cust_comm, 256);
  dbms_sql.bind_variable(c, 'ID', 106.0);
  rn := dbms_sql.execute_and_fetch(c, true);
  dbms_sql.column_value(c, 1, cust_id);
  dbms_sql.column_value(c, 2, cust_name);
  dbms_sql.column_value(c, 3, cust_address);
  dbms_sql.column_value(c, 4, cust_cred);
  dbms_sql.column_value(c, 5, cust_comm);
  dbms_sql.close_cursor(c);
  dbms_output.put_line('rows = ' || to_char(rn));
  dbms_output.put_line('ID = ' || to_char(cust_id));
  dbms_output.put_line('name = "' || cust_name || '"');
  dbms_output.put_line('address = "' || cust_address || '"');
  dbms_output.put_line('credit_limit = ' || to_char(cust_cred));
  dbms_output.put_line('comments = "' || cust_comm || '"');
END;