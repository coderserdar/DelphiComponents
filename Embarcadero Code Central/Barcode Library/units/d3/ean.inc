{.$define PSOFT_JPEG}

{$define PSOFT_QREPORT}
{$define PSOFT_FONT}
{$define PSOFT_LABEL}
{$define PSOFT_BARCODE_EDITOR}
{$define PSOFT_DEMO}
{$define PSOFT_BARCODE_DB}

{.$DEFINE BARCODE_OCX}
{$define PSOFT_PDF417}






// *********************************************************************
// ***************  PLEASE, DON'T MODIFY LINES BELOW   *****************
// *********************************************************************

{$ifndef MSWINDOWS}
        {$ifdef WIN32}
                {$define MSWINDOWS}
        {$endif}
{$endif}

{$ifdef MSWINDOWS}
	{$ifdef VER_80} // NOT SUPPORTED
	{$endif}


	{$IFDEF VER100} {Borland Delphi 3.0 }
	  {$DEFINE PSOFT_D3}
	{$ENDIF}

	{$IFDEF VER110} { Borland C++Builder 3.0 }
	  {$DEFINE PSOFT_CBUILDER3}
	{$ENDIF}

	{$IFDEF VER120} {Borland Delphi 4.0 }
	  {$DEFINE PSOFT_D3}
	  {$DEFINE PSOFT_D4}
	{$ENDIF}

	{$IFDEF VER125} {Borland C++ Builder 4.0 }
	  {$DEFINE PSOFT_CBUILDER4}
	{$ENDIF}

	{$IFDEF VER130} {Borland Delphi 5.0 }
	    {$ifdef BCB}
        	{$DEFINE PSOFT_BCB5}
	        {$ObjExportAll On}
	    {$else}
	        {$DEFINE PSOFT_D5}
	    {$endif}
	{$ENDIF}

	{$IFDEF VER140} {Borland Delphi 6.0 }
	  {$DEFINE PSOFT_D3}
	  {$DEFINE PSOFT_D4}
	  {$DEFINE PSOFT_D5}
	  {$define PSOFT_D6} 	
	{$endif}

{$endif}

{$ifdef LINUX}
        {$ifdef VER140}
                {$DEFINE PSOFT_KYLIX1}
        {$else}
        {$endif}
{$endif}
