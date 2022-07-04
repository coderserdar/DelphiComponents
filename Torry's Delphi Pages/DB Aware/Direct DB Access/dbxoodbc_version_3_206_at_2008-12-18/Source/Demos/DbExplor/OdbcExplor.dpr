{
  Delphi Test program for Open Odbc DbExpress Driver.
  Version 2008.03.24

  Copyright (c) 2001-2006 Edward Benson

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.
}
program OdbcExplor;

uses
  { Memory manager }
  FastMM4,
  { Exception stack handler }
  ExceptDlg,
  { Link ClientDatSet statically }
  MidasLib,
  { ... }
  Windows,
  SysUtils,
  Forms,
  { ... }
  frmOdbcExplor in 'frmOdbcExplor.pas' {FormOdbcExplor};

{$R *.res}

begin
  try
    {$if CompilerVersion >= 18.50}
    Application.MainFormOnTaskbar := True;
    {$ifend}
    Application.Initialize;
    Application.Title := 'ODBCExplor';
    Application.CreateForm(TFormOdbcExplor, FormOdbcExplor);

    Application.Run;
  except
    on e:exception do
    begin
      //Application.HandleException(e);
      {Application.}MessageBox(0,
        PAnsiChar('Error: [' + e.ClassName + '] "'+ e.Message+'".'),
        PAnsiChar(ExtractFileName(ParamStr(0))),
        MB_OK + MB_ICONERROR);
    end;
  end;
end.
