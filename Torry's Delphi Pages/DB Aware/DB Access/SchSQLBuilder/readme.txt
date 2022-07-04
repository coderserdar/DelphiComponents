Component Name: TSchSQLBuilder
        Author: Snuki
 Creation Date: 2005.11.29.
       Version: 1.1
   Description: Simple SELECT SQL script builder from field-, table-, where info
        E-mail: snuki@freemail.hu
       Website: - none -
  Legal Issues: All rigths reserved 1996-2005® by Snuki

This component is FREEWARE!

Usage:
  Generated format:
    SELECT
      Field1 [AS "Field1Alias"]
      [, Field2 [AS "Field2Alias"], .. Fieldn [AS "FieldnAlias"]]
    FROM
      Table1 [Table1Alias]
      [, Table2 [Table2Alias], .. Tablen [TablenAlias]]
    [WHERE
      [Join1]
      [AND Join2 .. AND Joinn]
      [AND [(]]
      [Where1 .. Wheren]
      [)] ]
    [GROUP BY
      GroupBy1 [, GroupBy2, .. GroupByn] ]
    [ORDER BY
      OrderBy1 [, OrderBy2, .. OrderByn] ]

Other infos:
  - Cannot add any item with empty name
  - Fields must be TABLENAME.FIELDNAME format (case insensitive, without spaces)
  - Aliases can contain special character, space, etc.
  - Field items have alias and aggregate informations
    (TSchFieldDef and TSchAggregate)
  - Table items have alias informations
    (TSchTableDef)
  - OrderBy items have sortorder informations
    (TSchOrderByDef and TSchOrderBy)
  - You can use english or hungarian language
