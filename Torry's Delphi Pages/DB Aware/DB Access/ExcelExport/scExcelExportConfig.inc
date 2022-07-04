{--------------------------------------------------------------------------------
* Description : scExcelExportConfig.inc - compiler directives
--------------------------------------------------------------------------------}

{$IFDEF VER185} // Delphi 2007 :  Default Excel2000, ExcelXP can also be used
  {$DEFINE EXCEL2000}
{$ELSE} // else, because VER180 can also be used in Delphi 2007
{$IFDEF VER180} // Delphi 2006 : Default Excel2000, ExcelXP can also be used
  {$DEFINE EXCEL2000}
{$ENDIF}
{$ENDIF}

{$IFDEF VER170} // Delphi 2005 : Default Excel2000, ExcelXP can also be used
  {$DEFINE EXCEL2000}
{$ENDIF}

{$IFDEF VER150} // Delphi 7 : Default Excel2000, ExcelXP and Excel97 can also be used
  {$DEFINE EXCEL2000}
{$ENDIF}

{$IFDEF VER140} //Delphi 6 : Default Excel2000, Excel97 can also be used
  {$DEFINE EXCEL2000}
{$ENDIF}

{$IFDEF VER130} //Delphi 5 : Default Excel97, Excel2000 can also be used when SP1 is installed
  {$DEFINE EXCEL97}
  {$DEFINE DELPHI5}
{$ENDIF}

// To override the default which is specified for each Delphi version
// just uncomment some of the following lines

// {$UNDEF EXCEL97}
// {$UNDEF EXCEL2000}
// {$UNDEF EXCELXP}

// {$DEFINE EXCEL97}
// {$DEFINE EXCEL2000}
// {$DEFINE EXCELXP}

// Excel 2000 is de default type library for compatibilty reasons with previous versions
// of the TscExcelExport component
// When using the TscExcelExport component for the first time, it is better to use the
// ExcelXP type library because this is a more recent version
