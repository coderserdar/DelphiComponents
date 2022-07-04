
type
  TLZReadEvent = procedure (var Buff; Count: Integer; var ActualCount: Integer) of object;
  TLZWriteEvent = procedure (var Buff; Count: Integer; var ActualCount: Integer) of object;
