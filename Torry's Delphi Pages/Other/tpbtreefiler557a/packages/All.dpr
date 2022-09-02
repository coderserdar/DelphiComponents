program All;

{$I BTDEFINE.INC}

uses
  BTFileIO,
  BTIsBase,

  {$IFNDEF Win32}
  {$IFDEF VER80}
  DOSSupp,
  {$ENDIF}
  BaseSupp,
  {$ENDIF}

  {$IFDEF MSDOS}
  EMSHeap,
  EMSSupp,
  {$ENDIF}

  BufRecIO,
  FixToVar,
  Rebuild,
  Reindex,
  Reorg,
  Restruct,
  VRebuild,
  VReorg,

  {$IFDEF Win32}
  NumKey32,
  {$ELSE}
  NumKeys,
  {$ENDIF}

  IsamTool,

  {$IFNDEF Win32}
  CarrConv,
  DBImpExp,
  {$ENDIF}

  Filer,
  VRec;

begin
end.
