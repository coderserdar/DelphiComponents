{
   Firebird Library
   Open Source Library No Data Aware for direct access to Firebird
   Relational Database from Borland Delphi / Kylix and Freepascal

   File:FBLConst.pas
   Copyright (c) 2002-2004 Alessandro Batisti
   fblib@altervista.org
   http://fblib.altervista.org

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
}

unit FBLConst;

interface

const
  (* Error *)
  E_DB_ALREADY_CON = 'Database already Connected';
  E_DB_NOACTIVE_CON = 'No Active Connection';
  E_DB_SQLDIALECT_INVALID = 'Sql Dialect invalid';
  E_DB_NO_HOST = 'Host name cannot be blank';
  E_TR_DB_NOT_ASSIGNED = 'Database not assigned';
  E_TR_ACTIVE = 'Transaction is active';
  E_TR_NOACTIVE = 'Transaction is not active';
  E_TR_NOT_ASSIGNED = 'Transaction is not Assigned';
  E_QR_PAR_OUT_OFF_RANGE = 'Param index %d out of range';
  E_QR_PARAM_NOT_SET = 'Param %d is not set';
  E_QR_FIELD_OUT_OFF_RANGE = 'Field index %d out of range';
  E_QR_FIELD_NAME_NOT_EXISTS = 'Field %s does not exist';
  E_QR_EMPTY_SQL = 'Empty query';
  E_QR_UNKNOW = 'Unknow Query';
  E_QR_NOTPERMITTED = 'Statement: %s' + #10 + 'Not permitted';
  E_QR_DATATYPE_UNKNOW = 'Data type unknow';
  E_QR_NUMERIC_CONV = 'Numeric convertion type error';
  E_QR_INFO = 'Error unknow in isc_dsql_sql_info';
  E_QR_FIELD_CONV = 'Invalid data convertion in field n. %d';
  E_QR_FIELD_TIMESTAMP_CONV = 'DATETIME convertion error in field n .%d';
  E_QR_FIELD_DATE_CONV = 'DATE convertion error in field n .%d';
  E_QR_FIELD_TIME_CONV = 'TIME convertion error in field n .%d';
  E_QR_FIELD_NOT_BLOB = 'Field %d is not BLOB type';
  E_QR_PARAM_NOTNULL = 'Param %d cannot have not null value';
  E_QR_PARAM_SHORT_OVERFLOW = 'Param %d type short overflow error';
  E_QR_PARAM_LONG_OVERFLOW = 'Param %d type long overflow error';
  E_QR_PARAM_INT64_OVERFLOW = 'Param %d type int64 overflow error';
  E_QR_PARAM_TYPE = 'Param %d type error';
  E_QR_PARAM_CONV = 'Invalid data convertion in param n. %d';
  E_QR_PARAM_TIMESTAMP_CONV = 'DATETIME convertion error in param n .%d';
  E_QR_PARAM_DATE_CONV = 'DATE convertion error in param n .%d';
  E_QR_PARAM_TIME_CONV = 'TIME convertion error in param n .%d';
  E_QR_PARAM_NOT_BLOB = 'Param %d is not BLOB type';
  E_SCRIPT_COMMENT = 'Invalid comment in statement ' + #10 + '%s';
  E_SM_ALREADY_CON = 'Service Manager already Connected';
  E_SM_NO_CON = 'Service Manager not Connected';
  E_SM_RES_NO_ACTION = 'Service Manager Restore Error : no action [resCreate,resReplace] in Options';
  E_SM_RES_PARAMS_ACCESSMODE =
    'Service Manager Restore Error: params error [resAccessModeReadOnly,resAccessModeReadWrite]';
  E_SM_USER_NOT_EXIST = 'Request user "%s" not exists in security database';
  E_SM_NOT_FUNC_SUPPORT = 'Function not supported in this server version';
  E_EV_LIST_EMPTY = 'Event list empty';
  E_SC_INVALID_LEN_SET_TERM = 'Invalid "SET TERM" lenght in script';
  E_SC_INVALID_CHAR_IN_SET_TERM = 'Invalid char  in "SET TERM" %s';
  (* Global *)
  // PALETTE_NAME = 'FBL';

  (* Character *)

  CR = #13;
  LF = #10;
  INTEND = '  ';
  {$IFDEF UNIX}
  NEW_LINE = LF;
  {$ELSE}
  NEW_LINE = CR + LF;
  {$ENDIF}

implementation

end.
