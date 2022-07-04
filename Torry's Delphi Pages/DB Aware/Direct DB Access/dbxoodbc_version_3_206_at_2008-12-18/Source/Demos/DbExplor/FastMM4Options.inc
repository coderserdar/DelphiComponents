{$D-}
{$IFNDEF _DEBUG_}
  {$INCLUDE FastMM4Options.rel.inc}
{$ELSE}
  {$INCLUDE FastMM4Options.dbg.inc}
  {.$D+,O-}
{$ENDIF}
