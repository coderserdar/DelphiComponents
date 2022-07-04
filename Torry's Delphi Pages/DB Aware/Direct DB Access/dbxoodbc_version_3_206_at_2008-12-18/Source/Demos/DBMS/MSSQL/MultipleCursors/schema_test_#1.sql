drop table table1;
drop table table2;
drop table table3;

-- Create table: table 1
create table table1
(
  tab1_id   integer not null,
  tab1_name varchar(64)
)
;

-- Create/Recreate primary, unique and foreign key constraints
alter table table1
  add constraint pk_tab1_id primary key (tab1_id);

insert into table1 (tab1_id, tab1_name) values (1, 'Hello table1. # 1');
insert into table1 (tab1_id, tab1_name) values (2, 'Hello table1. # 2');


-- Create table: table 2
create table table2
(
  tab2_id   integer not null,
  tab2_name varchar(192)
)
;

-- Create/Recreate primary, unique and foreign key constraints
alter table table2
  add constraint pk_tab2_id primary key (tab2_id);

insert into table2 (tab2_id, tab2_name) values (2, 'Hello table2. # 1');
insert into table2 (tab2_id, tab2_name) values (3, 'Hello table2. # 2');


-- Create table: table 3
create table table3
(
  tab3_id   integer not null,
  tab3_name varchar(138),
  tab3_desc varchar(138)
)
;

-- Create/Recreate primary, unique and foreign key constraints
alter table table3
  add constraint pk_tab3_id primary key (tab3_id);

insert into table3 (tab3_id, tab3_name, tab3_desc) values (3, 'Hello table3. # 1', 'Desc 1');
insert into table3 (tab3_id, tab3_name, tab3_desc) values (4, 'Hello table3. # 2', 'Desc 2');
insert into table3 (tab3_id, tab3_name, tab3_desc) values (5, 'Hello table3. # 3', 'Desc 3');
