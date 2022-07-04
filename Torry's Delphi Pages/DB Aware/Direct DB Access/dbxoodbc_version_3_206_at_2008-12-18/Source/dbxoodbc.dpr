{
  Kylix / Delphi open source DbExpress driver for ODBC
  Version 3.100, 2008-02-24: Beta

  Copyright (c) 2001, 2008 Edward Benson

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
}
library dbxoodbc;
// DLL version of DbExpress Open Source Odbc Driver.

{$INCLUDE DbxOpenOdbc.inc}

uses
{$IFDEF _FASTMM_}
  FastMM4,
{$ENDIF}
{$IFDEF LINUX}
  ShareExcept,
{$ENDIF}
  DbxOpenOdbc{$IFDEF _DBX30_}, DbxOpenOdbc3{$ENDIF};

{$IFDEF MSWINDOWS}
  {$R *.res} // Include Library information
{$ENDIF}

//
// DBX2:
//

// priority ansi odbc api
exports getSQLDriverODBC;
// priority unicode odbc api
exports getSQLDriverODBCAW;

//
// DBX3:
//

{$IFDEF _DBX30_}
// priority unicode odbc api
exports getSQLDriverODBCW;
// priority ansi odbc api
exports getSQLDriverODBCWA;
{$ENDIF}

begin
  { empty }
end.
