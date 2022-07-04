{$DEFINE KLIB100} 		{ Define the current library version         		   }
{$DEFINE REG_CHECK_ENABLED}     { Define if the registry checker will run or not 	   }
{$DEFINE EXCLUDED_CLASSES}	{ Define classes that will not be compiled in this version } 

{.$DEFINE DEBUG}		{ Define the library in debug version		  	   }
{.$DEFINE INTERNAL_VERSION}	{ Define the library as automatic Registered		   }
{.$DEFINE KLIB100_SHAREWARE}    { Define the library as internal Shareware   		   }
{.$DEFINE CREATE_KEY_FILES}     { Define if the Key Creation algorithm is enabled! 	   }

{$DEFINE DEF_LANG_USA}		{ Define the default library LanguageID 		   }
{.$DEFINE DEF_LANG_BRZ}		

{$IFDEF DEBUG}
	{$C+}
	{$DEFINE PACKREG_TESTING}
	{$DEFINE DSGN_SHAREWARE_TESTING}
{$ELSE}
	{$C-}
	{$UNDEF PACKREG_TESTING}
	{$UNDEF DSGN_SHAREWARE_TESTING}
{$ENDIF}

{$IFDEF VER80}
	{$DEFINE DELPHI1}
{$ENDIF}
{$IFDEF VER90}
	{.$DEFINE DELPHI1}
	{$DEFINE DELPHI2}
{$ENDIF}
{$IFDEF VER100}
	{.$DEFINE DELPHI1}
	{.$DEFINE DELPHI2}
	{$DEFINE DELPHI3}
{$ENDIF}
{$IFDEF VER120}
	{.$DEFINE DELPHI1}
	{.$DEFINE DELPHI2}
	{.$DEFINE DELPHI3}
	{$DEFINE DELPHI4}
{$ENDIF}

{$IFNDEF DELPHI4}
	{$IFDEF DELPHI3}
		{$DEFINE KLIB100} 							{ Define the current library version         }
	{$ELSE}
		{$UNDEF KLIB100}
	{$ENDIF}
{$ELSE}
	{.$UNDEF KLIB100}
	{$DEFINE KLIB100}
{$ENDIF}