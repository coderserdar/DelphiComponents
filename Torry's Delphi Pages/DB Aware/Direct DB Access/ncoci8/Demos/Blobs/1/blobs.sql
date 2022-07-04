CREATE TABLE test_blob (
  f1 NUMBER(10) NOT NULL,
  f2 BLOB       NULL,
  f3 CLOB       NULL
)
/

ALTER TABLE test_blob
  ADD PRIMARY KEY (
    f1
  )
/


