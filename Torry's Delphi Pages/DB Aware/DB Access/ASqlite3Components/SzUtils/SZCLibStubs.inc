//-----------------------------------
{
  Part of unit SZCLibStubs, a donation for Aducom SQLite component

  Stubs for external C RTL BCC 5.5.1 functions.
  Currently are suported few basic functions

  Author : Sasa Zeman Copyright (C) 2006-2007
  Website: www.szutils.net
  E-mail : public@szutils.net or sasaz72@mail.ru
}

function _malloc(Size: Cardinal): Pointer; cdecl;
begin
  GetMem(Result, Size);
end;

procedure _realloc(P: Pointer; Size: Cardinal); cdecl;
begin
  ReallocMem(P,Size)
end;

procedure _free(P: Pointer); cdecl;
begin
  FreeMem(P);
end;

function _calloc(n, size: Cardinal): Pointer;
begin
  Result := AllocMem(n * size);
end;

procedure _memset(P: Pointer; B: Byte; count: Cardinal); cdecl;
begin
  FillChar(P^, count, B);
end;

function _memmove(dest, source: Pointer; count: Cardinal):Pointer; cdecl;
begin
  Move(source^, dest^, count);
  Result:=dest;
end;

procedure _memcpy(dest, source: Pointer; count: Cardinal); cdecl;
begin
  Move(source^, dest^, count);
end;

function _memcmp(S1, S2: Pointer; MaxLen: Cardinal): integer; cdecl;
begin
  Result := StrLComp(S1, S2, MaxLen);
end;

function _strcpy(S1, S2: PChar): PChar; cdecl;
begin
  Result := StrCopy(S1, S2);
end;

function _strncpy(S1, S2: PChar; MaxLen: Cardinal): PChar; cdecl;
begin
  Result := StrLCopy(S1, S2, MaxLen);
end;

function _strncmp(S1, S2: PChar; MaxLen: Cardinal): integer; cdecl;
begin
  Result := StrLComp(S1, S2, MaxLen);
end;

function _strcmp(S1, S2: PChar): Cardinal; cdecl;
begin
  Result := StrComp(S1, S2);
end;

function _strlen(const Str: PChar): Cardinal; cdecl;
begin
  Result := StrLen(Str);
end;

// End of donation
//-----------------------------------


Var
  __HandlerPtr:Pointer;  
  __turboFloat: LongBool = False;

