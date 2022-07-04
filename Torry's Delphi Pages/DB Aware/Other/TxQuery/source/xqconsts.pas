{**************************************************************************}
{   TxQuery DataSet                                                        }
{                                                                          }
{   The contents of this file are subject to the Mozilla Public License    }
{   Version 1.1 (the "License"); you may not use this file except in       }
{   compliance with the License. You may obtain a copy of the License at   }
{   http://www.mozilla.org/MPL/                                            }
{                                                                          }
{   Software distributed under the License is distributed on an "AS IS"    }
{   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the}
{   License for the specific language governing rights and limitations     }
{   under the License.                                                     }
{                                                                          }
{   The Original Code is XQConsts.pas                                      }
{                                                                          }
{   The Initial Developer of the Original Code is Alfonso Moreno.          }
{   Portions created by Alfonso Moreno are Copyright (C) Alfonso Moreno.   }
{   All Rights Reserved.                                                   }
{                                                                          }
{   Alfonso Moreno (Hermosillo, Sonora, Mexico)                            }
{   email: luisarvayo@yahoo.com                                            }
{     url: http://www.ezsoft.com                                           }
{          http://www.sigmap.com/txquery.htm                               }
{                                                                          }
{   Contributor(s): Chee-Yang, CHAU (Malaysia) <cychau@gmail.com>          }
{                   Sherlyn CHEW (Malaysia)                                }
{              url: http://code.google.com/p/txquery/                      }
{                   http://groups.google.com/group/txquery                 }
{                                                                          }
{**************************************************************************}

Unit XQConsts;

{$I XQ_FLAG.INC}
{$DEFINE LANG_ENG}
{.$DEFINE LANG_SPA}
Interface

Resourcestring

  SXQUERY_ABOUT = 'TxQuery Version 2.0 (Ene 2009)';

  //SDefaultDateFormat = 'm/d/yyyy';
  SAggrSUM = 'SUM OF ';
  SAggrAVG = 'AVG OF ';
  SAggrSTDEV = 'STDEV OF ';
  SAggrMIN = 'MIN OF ';
  SAggrMAX = 'MAX OF ';
  SAggrCOUNT = 'COUNT(*)';

{$IFDEF xqdemo}
  SDelphiIsNotRunning = 'TxQuery Dataset (c) 2004 Alfonso Moreno' + #13#10 +
    'This is a demo version. Delphi must be running !';
{$ENDIF}

{$IFDEF LANG_ENG}
  SExprParserError = ' %s at line : %d, Column: %d, token: %s';
  SInsertWrongFieldName = 'INSERT: Field: %s does not exist in the dataset';
  SGroupByIncongruent = 'Fields in GROUP BY must also exists in SELECT columns';
  SIsNotValidInExecSQL = 'You must use SELECT only with Open method or Active property!';
  SIsNotValidInSelect = 'You must use UPDATE, DELETE, INSERT or CREATE TABLE only with ExecSQL method !';
  SGetRecordInvalid = 'GetRecord: Invalid record';
  SWrongParamsInExtract = 'Parameters for extract must be float or integer !';
  SWrongParamsInDateTime = 'Parameters for day/time function must be float or integer !';
  SSQLIsEmpty = 'SQL statement cannot be empty';
  SWrongLengthInTrim = 'Length of trimmed char in TRIM function must be 1';
  SHavingExprWrong = 'Wrong expression in HAVING predicate';
  SHavingWrong = 'Aggregate function not found in SELECT';
  SCircularReference = 'Circular reference not allowed';
  SJoinPredicateWrong = 'Number of tables in JOIN predicate must meet number of tables in FROM !';
  SDataSetNotOpened = 'DataSet %s is not opened !';
  SWrongDataSetnameInExpr = 'Wrong dataset name ''%s'' in expression';
  SWrongResultsetFieldname = 'Wrong field name ''%s'' in expression';
  SJoinExpectedDataSet = 'The joining is not correctly defined !';
  SWrongParameters = 'Parameter(s) invalid for function %s';
  SWrongFirstArg = 'Argument %s must be alfanumeric !';
  SXQueryNotDefined = 'XQuery component not defined !';
  SDuplicateDataSets = 'DataSet name %s is duplicated in the table list';
  SColumnRepeated = 'There are more than one column with name %s';
  SRecnoInvalid = 'Recno out of range';
  SGroupBySelectWrong = 'Every single field in GROUP BY clause must be in SELECT clause';
  SGroupByWrongNum = 'No. of fields in GROUP unmatched with SELECT';
  SWrongTableName = 'Wrong table name in format %s.*';
  SWrongIndexField = 'Field index incorrect';
  SFieldNotFound = 'Field %s was not found';
  SJoinOnMustHaveDiffTables = 'Left and right tables must be different in JOIN';
  SJoinOnWrongRightTable = 'Right table in JOIN must meet second table in FROM';
  SJoinOnWrongLeftTable = 'Left table in JOIN must meet first table in FROM';
  SJoinOnWrongTableNum = 'Number of tables in FROM is wrong for JOIN ON clause';
  SSubqueryWrongCols = 'Subquery with more than one column';
  SSubqueryWrongTables = 'More than one table in FROM clause in subquery';
  SWrongTableNumber = 'No tables defined in FROM clause!';
  SWrongFieldName = 'Field Name %s not found';
  SWrongDataSetName = 'DataSet %s doesn''t exists';
  SErrorInDBField = 'Error specifying a database field !';
  SCannotContainParams = 'Field %s of database cannot have parameters !';
  SExpresionNull = 'Expression is empty';
  SExprNotBoolean = 'Expresion is not of boolean type !';
  SRecordNotFound = 'Cannot found a record that correspond to the expression !';
  SBookmarkNotFound = 'Bookmark %d not found';
  SIndexOutOfRange = 'Index out of range';
  SErrorInWhere = 'Error in where expression';
  SParameterNotFound = 'Parameter %s not found in the list of params';
  SFileNotExists = 'LoadFromBinaryFile: File does not exists !';
  SCircularDataLink = 'Circular data link';
  SDuplicateFieldName = 'Duplicate field name';
  SFieldNameNotFound = 'PRIMARY KEY: field not found';
  SBlobFieldWrongType = 'BLOB FIELD: wrong type (1-5)';
  SDataSetUnexpected = 'DataSet is not of the expected class: %s';
  SReadBooleanField = 'Cannot read field as boolean';
  SReadFloatField = 'Cannot read field as float';
  SReadIntegerField = 'Cannot read field as integer';
  SReadStringField = 'Cannot read field as string';
  SReadVariantField = 'Cannot read field as variant';
  SWriteBooleanField = 'Cannot assign field as boolean';
  SWriteFloatField = 'Cannot assign field as float';
  SWriteIntegerField = 'Cannot assign field as integer';
  SWriteStringField = 'Cannot assign field as string';
  SWriteVariantField = 'Cannot assign field as variant';
  SIsInvalidFloatValue = 'Invalid floating point value %s';
  SIsInvalidIntegerValue = 'Invalid integer value %s';
  SIsInvalidBoolValue = 'Invalid boolean value %s';
  SNotAnAggregate = 'This is not an aggregate field !';
  SInvalidFieldNo = 'Invalid field number %d';
  STransfColumnsMismatch = 'Number of columns in SELECT mismatch in GROUP BY';
  STransfOrderByMismatch = 'Number of columns in SELECT mismatch in ORDER BY';
  STransfWrongColumnGroup = 'Column order in GROUP BY must match in SELECT';
  STransfWrongColumnoRDER = 'Column order in order BY must match in SELECT';
  SFailOpenFile = 'Failed to open or create file';
  SFailCreateMapping = 'Failed to create file mapping';
  SFailMapView = 'Failed to map view of file';
  SBeyondEOF = 'Position beyond EOF';
  SListError = 'List index error';
  SSyntaxErrorMsg = ' %s at line : %d, Column: %d, token: %s';
  SParamsError = 'Params were not replaced !';
  SDuplicateAlias = 'Alias already exists';
  SSubqueryInSelectsError = 'Not allowed nested subqueries in subqueries in SELECT';
  SWrongParameterQuotes = 'It is not possible to coexist both quotes (") and (''))';
  SWrongJoin = 'More than one table but no JOINing defined';
  SSubqueryKindWrong = 'In this version all subqueries must be ANY or ALL, but not both';
  SWrongIntoTable = 'The INTO table defined does not exists';
  SxqFieldNotFound = 'Field not found ';
  SCaseListMissingAlias = 'CASE statement need an alias.';
  SCaseExprNotBoolean = 'Expresion is not of boolean type in CASE clause !';
  SJoinNotMismatch = 'Tables referenced in JOIN does not correspond';
  SDupParamsAsFields = 'ParamsAsFields: duplicate name';
  SJoinFieldIncongruency= 'Field in join for corresponding table not found';
  SJoinInvalidFieldName= 'Invalid field or table name in join clause';
  SEXPR_WRONGWHENEXPR = 'Expression in Case must be boolean';
  SEXPR_WRONGTHENEXPR = 'Expressions in THEN section must be all of same type';
  SEXPR_UNKNOWNID = 'Unknown Identifier %s';
  SEXPR_OPERATORINCOMPAT = 'Operator %s incompatible';
  SEXPR_CANNOTCASTTOSTRING = 'Cannot read %s as String';
  SEXPR_CANNOTCASTTOFLOAT = 'Cannot read %s as Float';
  SEXPR_CANNOTCASTTOINTEGER = 'Cannot read %s as Integer';
  SEXPR_CANNOTCASTTOBOOLEAN = 'Cannot read %s as boolean';
  SEXPR_WRONGUNARYOP = '%s is not simple unary operator';
  SEXPR_WRONGBINARYOP = '%s is not a simple binary operator';
  SEXPR_WRONGBOOLEANOP = 'cannot apply %s to boolean operands';
  SEXPR_WRONGRELATIONALOP = '%s is not relational operator';
  SEXPR_WRONGPARAMETER = 'Invalid parameter to %s';
  SEXPR_INVALIDPARAMETERTO = 'Invalid parameter to %s';
{$ENDIF}

{$IFDEF LANG_SPA}

  { Resource strings en Español - Mexicano       }
  { Traducido por Francisco Dueñas               }
  { Email: fduenas@flashmail.com                 }

  SExprParserError = ' %s Línea : %d, Columna: %d, Identificador: %s'; //' %s at line : %d, Column: %d, token: %s';
  SInsertWrongFieldName = 'INSERT: El Campo: %s no existe en el dataset'; //'INSERT: Field: %s does not exist in the dataset';
  SGroupByIncongruent = 'Los campos del GROUP BY deben también existir en las columnas del SELECT'; //'Fields in GROUP BY must also exists in SELECT columns';
  SIsNotValidInExecSQL = '¡ Solo se debe usar SELECT con el método Open o la propiedad Active !'; //'You must use SELECT only with Open method or Active property!';
  SIsNotValidInSelect = '¡ Solo se deben usar UPDATE, DELETE, INSERT or CREATE TABLE con el método ExecSQL !'; //'You must use UPDATE, DELETE, INSERT or CREATE TABLE only with ExecSQL method !';
  SGetRecordInvalid = 'GetRecord: Registro inválido'; //'GetRecord: Invalid record';
  SWrongParamsInExtract = '¡ Los Parámetros para función Extract deben ser de tipo float ó integer !'; //'Parameters for extract must be float or integer !';
  SWrongParamsInDateTime = '¡ Los Parámetros para función day/time deben ser de tipo float ó integer !'; //'Parameters for day/time function must be float or integer !';
  SSQLIsEmpty = 'La sentencia SQL no puede estar vacía'; //'SQL statement cannot be empty';
  SWrongLengthInTrim = 'La longitud de la cadena a truncar en la función TRIM debe ser por lo menos 1'; //'Length of trimmed char in TRIM function must be 1';
  SHavingExprWrong = 'Expresión incorrecta en el predicado HAVING'; //'Wrong expression in HAVING predicate';
  SHavingWrong = 'La Función Agregada no se encuentra en la cláusula SELECT'; //'Aggregate function not found in SELECT';
  SCircularReference = 'La referencia circular no es permitida';//'Circular reference not allowed';
  SJoinPredicateWrong = 'El número de tablas en el predicado JOIN debe ser el mismo que el de la clásusula FROM !'; //'Number of tables in JOIN predicate must meet number of tables in FROM !';
  SDataSetNotOpened = '¡ El DataSet %s no está abierto !'; //'DataSet %s is not opened !';
  SWrongDataSetnameInExpr = 'Nombre del Dataset ''%s'' incorrecto en la expresión'; //'Wrong dataset name ''%s'' in expression';
  SWrongResultsetFieldname = 'Nombre de Campo ''%s'' incorrecto en la expresión'; //'Wrong field name ''%s'' in expression';
  SJoinExpectedDataSet = '¡ El JOINing no está definido correctamente !'; //'The joining is not correctly defined !';
  SWrongParameters = 'Parámetro(s) inválido(s) para la función %s'; //'Parameter(s) invalid for function %s';
  SWrongFirstArg = '¡ El Argumento %s debe ser alfanumérico !'; //'Argument %s must be alfanumeric !';
  SXQueryNotDefined = '¡ Componente XQuery no definido !'; //'XQuery component not defined !';
  SDuplicateDataSets = 'El Nombre del DataSet %s está duplicado en la lista de tablas'; //'DataSet name %s is duplicated in the table list';
  SColumnRepeated = 'Existe más de una columna con el nombre %s'; //'There are more than one column with name %s';
  SRecnoInvalid = 'RecNo fuera de rango'; //'Recno out of range';
  SGroupBySelectWrong = 'Cada Campo en la claúsula GROUP BY debe estar en la claúsula SELECT'; //'Every single field in GROUP BY clause must be in SELECT clause';
  SGroupByWrongNum = 'El No. de Campos en la claúsula GROUP no concuerda con los de la claúsula SELECT'; //'No. of fields in GROUP unmatched with SELECT';
  SWrongTableName = 'Nombre de Tabla incorrecto con formato %s.*'; //'Wrong table name in format %s.*';
  SWrongIndexField = 'Indice del Campo incorrecto'; //'Field index incorrect';
  SFieldNotFound = 'No se encontró el campo %s'; //'Field %s was not found';
  SJoinOnMustHaveDiffTables = 'Tablas Izquierda y Derecha deben ser diferentes en el JOIN'; //'Left and right tables must be different in JOIN';
  SJoinOnWrongRightTable = 'La Tabla Derecha en el JOIN debe ser la segunda tabla en el FROM'; //'Right table in JOIN must meet second table in FROM';
  SJoinOnWrongLeftTable = 'Left table in JOIN must meet first table in FROM';
  SJoinOnWrongTableNum = 'El número de tablas en el FROM es incorrecto para la claúsula JOIN ON'; //'Number of tables in FROM is wrong for JOIN ON clause';
  SSubqueryWrongCols = 'Sunconsulta con más de una columna'; //'Subquery with more than one column';
  SSubqueryWrongTables = 'Más de una tabla en la claúsula FROM en la subconsulta'; //'More than one table in FROM clause in subquery';
  SWrongTableNumber = '¡No hay tablas definidas en la claúsula FROM!'; //'No tables defined in FROM clause!';
  SWrongFieldName = 'Nombre del Campo %s no encontrado'; //'Field Name %s not found';
  SWrongDataSetName = 'El Dataset %s no existe'; //'DataSet %s doesn''t exists';
  SErrorInDBField = '¡ Error especificando un campo de base de datos !'; //'Error specifying a database Field !';
  SCannotContainParams = '¡ El Campo %s de la Base de datos no puede tener parámetros !'; //'Field %s of database cannot have parameters !';
  SExpresionNull = 'La expresión está vacía'; //'Expression is empty';
  SExprNotBoolean = 'La expresión no es de tipo lógico'; //'Expresion is not of boolean type !';
  SRecordNotFound = '¡ No se pudo encontrar el registro correspondiente a la expresión !'; //'Cannot found a record that correspond to the expression !';
  SBookmarkNotFound = 'Bookmark %d no encontrado'; //'Bookmark %d not found';
  SIndexOutOfRange = 'Indice fuera de rango'; //'Index out of range';
  SErrorInWhere = 'Error en la expresión WHERE'; //'Error in where expression';
  SParameterNotFound = 'El Parámetro %s no se encontró en la lista de parámetros'; //'Parameter %s not found in the list of params';
  SFileNotExists = 'LoadFromBinaryFile: ¡ El archivo no existe !'; //'LoadFromBinaryFile: File does not exists !';
  SCircularDataLink = 'La liga de datos es circular'; //'Circular data link';
  SDuplicateFieldName = 'Nombre del campo duplicado'; //'Duplicate field name';
  SFieldNameNotFound = 'LLAVE PRIMARIA: Campo no econtrado'; //'PRIMARY KEY: field not found';
  SBlobFieldWrongType = 'CAMPO BLOB: tipo incorrecto (1-5)'; //'BLOB FIELD: wrong type (1-5)';
  SDataSetUnexpected = 'El Dataset no es de la clase esperada: %s'; //'DataSet is not of the expected class: %s';
  SReadBooleanField = 'El campo no puede leerse como tipo lógico o buleano'; //'Cannot read field as boolean';
  SReadFloatField = 'El campo no puede leerse como tipo punto flotante'; //'Cannot read field as float';
  SReadIntegerField = 'El campo no puede leerse como tipo entero'; //'Cannot read field as integer';
  SReadStringField = 'El campo no puede leerse como tipo cadena'; //'Cannot read field as string';
  SReadVariantField = 'El campo no puede leerse como variant';
  SWriteBooleanField = 'El campo no puede asignarse valores como tipo lógico o buleano'; //'Cannot assign field as boolean';
  SWriteFloatField = 'El campo no puede asignarse valores como tipo float (punto flotante)'; //'Cannot assign field as float';
  SWriteIntegerField = 'El campo no puede asignarse valores como tipo entero'; //'Cannot assign field as integer';
  SWriteStringField = 'El campo no puede asignarse valores como tipo string (cadena)'; //'Cannot assign field as string';
  SWriteVariantField = 'El campo no puede asignarse como variant';
  SIsInvalidFloatValue = 'valor de punto flotante inválido: %s'; //'Invalid floating point value %s';
  SIsInvalidIntegerValue = 'Valor entero inválido: %s'; //'Invalid integer value %s';
  SIsInvalidBoolValue = 'Valor lógico o buleano inválido: %s'; //'Invalid boolean value %s';
  SNotAnAggregate = '¡ Este no es un campo de agregado (aggregate) !'; //'This is not an aggregate field !';
  SInvalidFieldNo = 'Número de campo inválido: %d'; //'Invalid field number %d';
  STransfColumnsMismatch = 'El Número de columnas en SELECT no coincide con el de GROUP BY'; //'Number of columns in SELECT mismatch in GROUP BY';
  STransfOrderByMismatch = 'El Número de columnas en SELECT no coincide con el de ORDER BY'; //'Number of columns in SELECT mismatch in ORDER BY';
  STransfWrongColumnGroup = 'El orden de las columnas en GROUP BY debe coincidir con el de SELECT'; //'Column order in GROUP BY must match in SELECT';
  STransfWrongColumnoRDER = 'El orden de las columnas en ORDER BY debe coincidir con el de SELECT'; //'Column order in ORDER BY must match in SELECT';
  SFailOpenFile = 'Fallo al abrir o crear el archivo'; //'Failed to open or create file';
  SFailCreateMapping = 'Fallo al crear el mapeado de archivo'; //'Failed to create file mapping';
  SFailMapView = 'Fallo al mapear la vista del archivo'; //'Failed to map view of file';
  SBeyondEOF = 'Posición más allá de EOF'; //'Position beyond EOF';
  SListError = 'Error en el índice de la lista'; //'List index error';
  SSyntaxErrorMsg = ' %s Línea: %d, Columna: %d, Identificador: %s'; //' %s at line : %d, Column: %d, token: %s';
  SParamsError = '¡ Los parámetros no fueron reemplazados !'; //'Params were not replaced !';
  SDuplicateAlias = 'El alias ya existe'; //'Alias already exists';
  SSubqueryInSelectsError = 'No se permiten subconsultas anidades en subcosultas del SELECT'; //'Not allowed nested subqueries in subqueries in SELECT';
  SWrongParameterQuotes = 'No es posible que coexistan ambas comillas, (") y ('')'; //'It is not possible to coexist both quotes (") and (''))';
  SWrongJoin = 'Hay más de una tabla pero no existe un JOIN definido'; //'More than one table but no JOINing defined';
  SSubqueryKindWrong = 'En esta versión las subconsultas deben ser del tipo ANY o del tipo ALL, pero no ambos'; //'In this version all subqueries must be ANY or ALL, but not both';
  SWrongIntoTable = 'La tabla definida en la claúsula INTO no existe'; //'The INTO table defined does not exists';
  SxqFieldNotFound = 'Campo no encontrado'; //'Field not found ';
  SCaseListMissingAlias = 'La sentencia CASE requiere un alias.'; //'CASE statement need an alias.';
  SCaseExprNotBoolean = '¡ La expresión no es del tipo lógico o booleano en la claúsula CASE !'; //'Expresion is not of boolean type in CASE clause !';
  SJoinNotMismatch = 'No existe correspondencia entre las tablas referenciadas en el JOIN'; //'Tables referenced in JOIN does not correspond';
  SDupParamsAsFields = 'ParamsAsFields: nombre duplicado'; //'ParamsAsFields: duplicate name';
  SJoinFieldIncongruency= 'Un campo de una tabla no fue encontrado en expresion del JOIN';
  SJoinInvalidFieldName= 'Campo o tabla no encontrado en un JOIN';
  SEXPR_WRONGWHENEXPR = 'La expresión en el Case debe ser de tipo lógico'; //'Expression in Case must be boolean';
  SEXPR_WRONGTHENEXPR = 'Todas las expresiones en la sección THEN deben de ser del mismo tipo'; //'Expressions in THEN section must be all of same type';
  SEXPR_UNKNOWNID = 'Identificador desconocido %s'; //'Unknown Identifier %s';
  SEXPR_OPERATORINCOMPAT = 'El operador %s no es compatible con %s'; //'Operator %s incompatible with %s';
  SEXPR_CANNOTCASTTOSTRING = 'No se puede leer %s como tipo String'; //'Cannot read %s as String';
  SEXPR_CANNOTCASTTOFLOAT = 'No se puede leer %s como tipo Float'; //'Cannot read %s as Float';
  SEXPR_CANNOTCASTTOINTEGER = 'No se puede leer %s como tipo Integer'; //'Cannot read %s as Integer';
  SEXPR_CANNOTCASTTOBOOLEAN = 'No se puede leer %s como tipo bolean'; //'Cannot read %s as boolean';
  SEXPR_WRONGUNARYOP = '%s no es un operador unario simple'; //'%s is not simple unary operator';
  SEXPR_WRONGBINARYOP = '%s no es un operador binario simple'; //'%s is not a simple binary operator';
  SEXPR_WRONGBOOLEANOP = 'no se puede aplicar %s a operadores lógicos'; //'cannot apply %s to boolean operands';
  SEXPR_WRONGRELATIONALOP = '%s no es un operador relacional'; //'%s is not relational operator';
  SEXPR_WRONGPARAMETER = 'Parámetro no válido para %s'; //'Invalid parameter to %s';
  SEXPR_INVALIDPARAMETERTO = 'Parámetro no válido para %s'; //'Invalid parameter to %s';
{$ENDIF}

Implementation

End.
