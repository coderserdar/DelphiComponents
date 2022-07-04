{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{                                                       }
{         Native Windows API memory manager             }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgMemory;

interface

function WinGetMem(Size: Integer): Pointer;
function WinFreeMem(P: Pointer): Integer;
function WinReallocMem(P: Pointer; Size: Integer): Pointer;

var
  WinMemoryManager: TMemoryManager = (
    GetMem: WinGetMem;
    FreeMem: WinFreeMem;
    ReallocMem: WinReallocMem;
  );

implementation
uses Windows;

const
  MinBlock = 16;

function WinGetMem(Size: Integer): Pointer;
begin
  Size := Size + Size mod MinBlock;
  Result := GlobalAllocPtr(HeapAllocFlags, Size);
end;

function WinFreeMem(P: Pointer): Integer;
begin
  Result := GlobalFreePtr(P);
end;

function WinReallocMem(P: Pointer; Size: Integer): Pointer;
begin
  Size := Size + Size mod MinBlock;
  Result := GlobalReallocPtr(P, Size, HeapAllocFlags);
end;

initialization
{$IFDEF _D3_}
  if not IsMemoryManagerSet then
{$ENDIF}
    SetMemoryManager(WinMemoryManager);
end.
