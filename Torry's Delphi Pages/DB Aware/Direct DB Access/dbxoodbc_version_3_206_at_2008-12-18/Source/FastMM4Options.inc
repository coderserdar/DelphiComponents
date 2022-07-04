{$INCLUDE DbxOpenOdbc_options.inc}

{$IFDEF _RELEASE_}
  {$INCLUDE FastMM4Options.rel.inc}
{$ELSE}
  {$IFDEF _DEBUG_}
    {$INCLUDE FastMM4Options.dbg.inc}
  {$ELSE}
    {$INCLUDE FastMM4Options.rel.inc}
  {$ENDIF}
{$ENDIF}
