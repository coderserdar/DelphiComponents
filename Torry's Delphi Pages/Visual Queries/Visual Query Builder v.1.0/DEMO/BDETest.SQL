[#SQLBuilder#]
--------------
[Revision]
1
--------------
[FORMS]
3
customer.db
customer
372
122
153
160
custoly.db
custoly
140
123
153
160
country.db
country
596
118
153
160
--------------
[LINKS]
3
6
4
custoly
customer
0
1
7
0
customer
country
0
3
4
2
custoly
customer
0
1
--------------
[GRIDS]
7
custoly
Address1 (String)
Yes


custoly
Address2 (String)
Yes


custoly
City (String)
Yes


customer
Country (String)
Yes


customer
State (String)
Yes


customer
Zip (String)
Yes


--------------
[EXPRESSIONS]
4
custoly.Address1 NOT LIKE 'Marmo%'
customer.Country NOT LIKE 'Cyp%'
customer.State <> 'FL'
--------------
[SORTING]
4
custoly.Address1 (String)
custoly.Address2 (String)
customer.State (String)
customer.Zip (String)
2
custoly.City (String)
ASC
customer.Country (String)
ASC
--------------
[OPTIONS]
-1
1
--------------
[END OF FILE]
