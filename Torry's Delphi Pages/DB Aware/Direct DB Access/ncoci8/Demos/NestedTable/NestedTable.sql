CREATE or replace TYPE TNestedType AS OBJECT (
  Num NUMBER,
  Str VARCHAR2(30),
  Dat DATE
);
/

CREATE or replace TYPE TNestedTable AS TABLE OF TNestedType;
/

CREATE TABLE ParentTable (
  Code NUMBER PRIMARY KEY,
  Content TNestedTable
)
NESTED TABLE Content STORE AS NestedTable;

INSERT INTO ParentTable
  (Code, Content)
VALUES
  (1, TNestedTable(TNestedType(111, 'AAAAA', NULL)));

INSERT INTO ParentTable
  (Code, Content)
VALUES
  (2, TNestedTable(TNestedType(22, 'BBB', NULL),
                   TNestedType(333, 'TTT', NULL)));

INSERT INTO ParentTable
  (Code)
VALUES
  (3);

COMMIT;

-- selects only rows from nested table
select *
from THE(select content from ParentTable where code = 1);

-- selects rows from parent table with rows from nested table
SELECT t.code, t.rowid, CURSOR(SELECT * FROM TABLE(t.content))
FROM ParentTable t;
