{********************************************************************}
{* BTFileIO.PAS - B-Tree Filer low-level file I/O routines          *}
{********************************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower B-Tree Filer
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I BTDEFINE.INC}

unit BTFileIO;

interface

uses
  {$IFDEF Win32}                                               {!!.54}
  Windows,                                                     {!!.54}
  {$ENDIF}                                                     {!!.54}
  BTBase;

var
  btfDOSFunc  : word;
  btfDOSError : TbtfErrorCode;

type
  TbtfOpenMode = (bomReadOnly, bomReadWrite);
  TbtfShareMode = (bsmExclusive, bsmShared);

function btfCloseFile(aHandle : TbtfHandle) : boolean;
 {-close the file}
function btfDeleteFile(aName : PAnsiChar) : boolean;
 {-delete the file}
function btfFlushFile(aHandle : TbtfHandle) : boolean;
 {-flush the file}
function btfGetPositionFile(aHandle : TbtfHandle;
                        var aOffset : DWORD) : boolean;        {!!.54}
 {-return the current file position}
function btfLockFile(aHandle : TbtfHandle;
                     aStart  : longint;
                     aCount  : longint) : boolean;
 {-lock the file}
function btfOpenFile(aName       : PAnsiChar;
                     aOpenMode   : TbtfOpenMode;
                     aShareMode  : TbtfShareMode;
                     aWriteThru  : boolean;
                     aCreateFile : boolean;
                     aInheritable: boolean;
                 var aHandle     : TbtfHandle) : boolean;
 {-open the file, return the handle}
function btfPositionFile(aHandle : TbtfHandle;
                         aOffset : longint) : boolean;
 {-set the file position}
function btfPositionFileEOF(aHandle   : TbtfHandle;
                        var aFileSize : DWORD) : boolean;      {!!.54}
 {-set the file position at EOF, return the filesize}
function btfReadFile(aHandle    : TbtfHandle;
                     aToRead    : longint;
                 var aBuffer;
                 var aBytesRead : longint) : boolean;
 {-read from the file, return the number of bytes read}
function btfRenameFile(aName    : PAnsiChar;
                       aNewName : PAnsiChar) : boolean;
 {-rename the file}
function btfSetEOF(aHandle : TbtfHandle;
                   aOffset : longint) : boolean;
 {-truncate the file}
function btfUnlockFile(aHandle : TbtfHandle;
                       aStart  : longint;
                       aCount  : longint) : boolean;
 {-unlock the file}
function btfWriteFile(aHandle       : TbtfHandle;
                      aToWrite      : longint;
                const aBuffer;
                  var aBytesWritten : longint) : boolean;
 {-write to the file, return the number of bytes written}

implementation

{$IFDEF Windows}
uses
  WinTypes, WinProcs;
{$ENDIF}
{$IFDEF DPMI}
uses
  WinAPI;
{$ENDIF}

{===Common routines==================================================}
function MaxLong(a, b : longint) : longint;
begin
  if (a > b) then
    MaxLong := a
  else
    MaxLong := b;
end;
{--------}
function MinLong(a, b : longint) : longint;
begin
  if (a < b) then
    MinLong := a
  else
    MinLong := b;
end;
{====================================================================}


{===Win16 and DOS calls==============================================}
{$IFNDEF Win32}
const
  {constants for opening a file, etc}
  OPEN_ACCESS_READONLY = 0;
  OPEN_ACCESS_READWRITE = 2;
  OPEN_SHARE_DENYREADWRITE = $10;
  OPEN_SHARE_DENYWRITE = $20;
  OPEN_SHARE_DENYNONE = $40;
  OPEN_FLAGS_NOINHERIT = $80;
  OPEN_FLAGS_NOCRITERR = $2000;
  OPEN_FLAGS_COMMIT = $4000;
  FILE_CREATE = $10;
  FILE_OPEN = $01;
  FILE_TRUNCATE = $02;
  FILE_CREATE_ALWAYS = FILE_CREATE + FILE_TRUNCATE;

{---Base file I/O routines-------------------------------------------}
function CheckForSuccess(aDosFunc  : word;
                         aDosError : TbtfErrorCode) : boolean;
begin
  if (aDosError <> 0) then begin
    btfDOSFunc := aDosFunc;
    btfDOSError := aDosError;
    CheckForSuccess := false;
  end
  else
    CheckForSuccess := true;
end;
{--------}
function baseReadFile(Handle      : TbtfHandle;
                      BytesToRead : word;
                  var Buffer;
                  var BytesRead   : word) : boolean;
var
  DosError  : word;
begin
  asm
    push ds
    mov ah, $3F
    mov bx, Handle
    mov cx, BytesToRead
    lds dx, Buffer
    {$IFDEF DPMIOrWnd}
    call DOS3Call
    {$ELSE}
    int $21
    {$ENDIF}
    pop ds
    jc @@Error
    les di, BytesRead
    mov es:[di], ax
    xor ax, ax
  @@Error:
    mov DosError, ax
  end;
  baseReadFile := CheckForSuccess($3F00, DosError);
end;
{--------}
function baseWriteFile(Handle       : TbtfHandle;
                       BytesToWrite : word;
                   var Buffer;
                   var BytesWritten : word) : boolean;
var
  DosError  : word;
begin
  asm
    push ds
    mov ah, $40
    mov bx, Handle
    mov cx, BytesToWrite
    lds dx, Buffer
    {$IFDEF DPMIOrWnd}
    call DOS3Call
    {$ELSE}
    int $21
    {$ENDIF}
    pop ds
    jc @@Error
    les di, BytesWritten
    mov es:[di], ax
    xor ax, ax
  @@Error:
    mov DosError, ax
  end;
  baseWriteFile := CheckForSuccess($4000, DosError);
end;
{--------------------------------------------------------------------}

{---File access routines---------------------------------------------}
function btfCloseFile(aHandle : TbtfHandle) : boolean;
var
  DosError : word;
begin
  asm
    mov ah, $3E
    mov bx, aHandle
    {$IFDEF DPMIOrWnd}
    call DOS3Call
    {$ELSE}
    int $21
    {$ENDIF}
    jc @@Error
    xor ax, ax
  @@Error:
    mov DosError, ax
  end;
  btfCloseFile := CheckForSuccess($3E00, DosError);
end;
{--------}
function btfDeleteFile(aName : PAnsiChar) : boolean;
var
  DosError : word;
begin
  asm
    push ds
    mov ah, $41
    lds dx, aName
    {$IFDEF DPMIOrWnd}
    call DOS3Call
    {$ELSE}
    int $21
    {$ENDIF}
    pop ds
    jc @@Error
    xor ax, ax
  @@Error:
    mov DosError, ax
  end;
  btfDeleteFile := CheckForSuccess($4100, DosError);
end;
{--------}
function btfFlushFile(aHandle : TbtfHandle) : boolean;
var
  DosError : word;
  DosFunc  : word;
begin
  asm
    mov ax, $3000      {get DOS version}
    {$IFDEF DPMIOrWnd}
    call DOS3Call
    {$ELSE}
    int $21
    {$ENDIF}
    cmp al, 3          {..major in al}
    jg @@CommitCall
    jl @@CloseDupCall
    cmp ah, 30         {..minor in ah}
    jge @@CommitCall

  @@CloseDupCall:      {flush by dup'ing handle, closing dup}
    mov ax, $4500
    mov DosFunc, ax
    mov bx, aHandle
    {$IFDEF DPMIOrWnd}
    call DOS3Call
    {$ELSE}
    int $21
    {$ENDIF}
    jc @@Error
    mov bx, ax
    mov ax, $3E00
    mov DosFunc, ax
    {$IFDEF DPMIOrWnd}
    call DOS3Call
    {$ELSE}
    int $21
    {$ENDIF}
    jc @@Error
    xor ax, ax
    jmp @@Error

  @@CommitCall:        {flush by using commit}
    mov ax, $6800
    mov DosFunc, ax
    mov bx, aHandle
    {$IFDEF DPMIOrWnd}
    call DOS3Call
    {$ELSE}
    int $21
    {$ENDIF}
    jc @@Error
    xor ax, ax
  @@Error:
    mov DosError, ax
  end;
  btfFlushFile := CheckForSuccess(DosFunc, DosError);
end;
{--------}
function btfGetPositionFile(aHandle : TbtfHandle;
                        var aOffset : DWORD) : boolean;        {!!.54}
var
  DosError   : word;
  SeekResult : longint;
begin
  asm
    mov ax, $4201
    mov bx, aHandle
    xor cx, cx
    xor dx, dx
    {$IFDEF DPMIOrWnd}
    call DOS3Call
    {$ELSE}
    int $21
    {$ENDIF}
    jc @@Error
    les di, aOffset
    mov es:[di].word[2], dx
    mov es:[di].word[0], ax
    xor ax, ax
  @@Error:
    mov DosError, ax
  end;
  btfGetPositionFile := CheckForSuccess($4201, DosError);
end;
{--------}
function btfLockFile(aHandle : TbtfHandle;
                     aStart  : longint;
                     aCount  : longint) : boolean;
var
  DosError : word;
begin
  asm
    mov ax, $5C00
    mov bx, aHandle
    mov cx, aStart.word[2]
    mov dx, aStart.word[0]
    mov si, aCount.word[2]
    mov di, aCount.word[0]
    {$IFDEF DPMIOrWnd}
    call DOS3Call
    {$ELSE}
    int $21
    {$ENDIF}
    jc @@Error
    xor ax, ax
  @@Error:
    mov DosError, ax
  end;
  btfLockFile := CheckForSuccess($5C00, DosError);
end;
{--------}
function btfOpenFile(aName       : PAnsiChar;
                     aOpenMode   : TbtfOpenMode;
                     aShareMode  : TbtfShareMode;
                     aWriteThru  : boolean;
                     aCreateFile : boolean;
                     aInheritable: boolean;
                 var aHandle     : TbtfHandle) : boolean;
var
  OpenMode   : word;
  CreateMode : word;
  DosError   : word;
  DosFunc    : word;
  DosVer     : word;
begin
  {assume file won't open}
  aHandle := INVALID_HANDLE_VALUE;
  {$IFDEF DPMI}                                                {!!.54}
  {for protected mode, 'pretend' that the DOS version is 3.30:  !!.54
   RTM.EXE does not support later DOS calls}                   {!!.54}
  DosVer := $031E;                                             {!!.54}
  {$ELSE}                                                      {!!.54}
  {get the DOS version (and get it the right way round)}
  asm
    mov ax, $3000
    {$IFDEF DPMIOrWnd}
    call DOS3Call
    {$ELSE}
    int $21
    {$ENDIF}
    xchg al, ah
    mov DosVer, ax
  end;
  {$ENDIF}                                                     {!!.54}
  {decide what to do based on the DOS version}
  if (DosVer >= $0400) then begin
    {initialise parameters to DOS open file function}
    if (aOpenMode = bomReadOnly) then
      OpenMode := OPEN_ACCESS_READONLY
    else
      OpenMode := OPEN_ACCESS_READWRITE;
    if (aShareMode = bsmExclusive) then
      OpenMode := OpenMode or OPEN_SHARE_DENYREADWRITE
    else
      OpenMode := OpenMode or OPEN_SHARE_DENYNONE;
    if aWriteThru then
      OpenMode := OpenMode or OPEN_FLAGS_COMMIT;
    if not aInheritable then
      OpenMode := OpenMode or OPEN_FLAGS_NOINHERIT;
    OpenMode := OpenMode or OPEN_FLAGS_NOCRITERR;
    if aCreateFile then
      CreateMode := FILE_CREATE_ALWAYS
    else
      CreateMode := FILE_OPEN;
    {open the file}
    asm
      push ds
      mov ax, $6C00
      mov DosFunc, ax
      mov bx, OpenMode
      xor cx, cx
      mov dx, CreateMode
      lds si, aName
      {$IFDEF DPMIOrWnd}
      call DOS3Call
      {$ELSE}
      int $21
      {$ENDIF}
      pop ds
      jc @@Error
      les di, aHandle
      mov es:[di], ax
      xor ax, ax
    @@Error:
      mov DosError, ax
    end;
  end
  else {DOS version is less than 4.00} begin
    if aCreateFile then {create the file} begin
      asm
        push ds
        mov ax, $3C00
        mov DosFunc, ax
        xor cx, cx                                             {!!.54}
        lds dx, aName
        {$IFDEF DPMIOrWnd}
        call DOS3Call
        {$ELSE}
        int $21
        {$ENDIF}
        pop ds
        jc @@Error
        les di, aHandle
        mov es:[di], ax
        xor ax, ax
      @@Error:
        mov DosError, ax
      end;
    end
    else {open the existing file} begin
      {initialise parameters to DOS open file function}
      if (aOpenMode = bomReadOnly) then
        OpenMode := OPEN_ACCESS_READONLY
      else
        OpenMode := OPEN_ACCESS_READWRITE;
      if (aShareMode = bsmExclusive) then
        OpenMode := OpenMode or OPEN_SHARE_DENYREADWRITE
      else
        OpenMode := OpenMode or OPEN_SHARE_DENYNONE;
      if not aInheritable then
        OpenMode := OpenMode or OPEN_FLAGS_NOINHERIT;
      {open the file}
      asm
        push ds
        mov ax, OpenMode
        mov ah, $3D
        mov DosFunc, ax
        lds dx, aName
        {$IFDEF DPMIOrWnd}
        call DOS3Call
        {$ELSE}
        int $21
        {$ENDIF}
        pop ds
        jc @@Error
        les di, aHandle
        mov es:[di], ax
        xor ax, ax
      @@Error:
        mov DosError, ax
      end;
    end;
  end;
  btfOpenFile := CheckForSuccess(DosFunc, DosError);
end;
{--------}
function btfPositionFile(aHandle : TbtfHandle;
                         aOffset : longint) : boolean;
var
  DosError   : word;
begin
  asm
    mov ax, $4200
    mov bx, aHandle
    mov cx, aOffset.word[2]
    mov dx, aOffset.word[0]
    {$IFDEF DPMIOrWnd}
    call DOS3Call
    {$ELSE}
    int $21
    {$ENDIF}
    jc @@Error
    xor ax, ax
  @@Error:
    mov DosError, ax
  end;
  btfPositionFile := CheckForSuccess($4200, DosError);
end;
{--------}
function btfPositionFileEOF(aHandle   : TbtfHandle;
                        var aFileSize : DWORD) : boolean;      {!!.54}
var
  DosError   : word;
  SeekResult : longint;
  Offset     : longint;
begin
  asm
    mov ax, $4202
    mov bx, aHandle
    xor cx, cx
    mov dx, cx
    {$IFDEF DPMIOrWnd}
    call DOS3Call
    {$ELSE}
    int $21
    {$ENDIF}
    jc @@Error
    les di, aFileSize
    mov es:[di].word[2], dx
    mov es:[di].word[0], ax
    xor ax, ax
  @@Error:
    mov DosError, ax
  end;
  btfPositionFileEOF := CheckForSuccess($4202, DosError);
end;
{--------}
function btfReadFile(aHandle    : TbtfHandle;
                     aToRead    : longint;
                 var aBuffer;
                 var aBytesRead : longint) : boolean;
const
  BlockSize = 32*1024;
  {$IFDEF MSDOS}
  SelectorInc = $1000;
  {$ENDIF}
var
  DestPtr     : PAnsiChar;
  BytesToGo   : longint;
  BytesToRead : word;
  BytesRead   : word;
  MustIncSelector : boolean;
begin
  btfReadFile := true;
  aBytesRead := 0;
  BytesToGo := aToRead;
  DestPtr := @aBuffer;

  MustIncSelector := (Ofs(DestPtr^) >= BlockSize);
  if ((Ofs(DestPtr^) mod BlockSize) <> 0) then begin
    if MustIncSelector then
      BytesToRead := MinLong(BytesToGo, succ($FFFF - Ofs(DestPtr^)))
    else
      BytesToRead := MinLong(BytesToGo, ($8000 - Ofs(DestPtr^)))
  end
  else begin
    BytesToRead := MinLong(BytesToGo, BlockSize);
  end;
  if not baseReadFile(aHandle, BytesToRead, DestPtr^, BytesRead) then begin
    btfReadFile := false;
    Exit;
  end;
  inc(aBytesRead, BytesRead);
  if (BytesToRead <> BytesRead) then
    Exit;
  dec(BytesToGo, BytesRead);

  while (BytesToGo > BlockSize) do begin
    if MustIncSelector then
      DestPtr := Ptr(Seg(DestPtr^) + SelectorInc, 0)
    else
      DestPtr := DestPtr + BytesRead;
    MustIncSelector := not MustIncSelector;
    BytesToRead := MinLong(BytesToGo, BlockSize);
    if not baseReadFile(aHandle, BytesToRead, DestPtr^, BytesRead) then begin
      btfReadFile := false;
      Exit;
    end;
    inc(aBytesRead, BytesRead);
    if (BytesToRead <> BytesRead) then
      Exit;
    dec(BytesToGo, BlockSize);
  end;
  if (BytesToGo <> 0) then begin
    if MustIncSelector then
      DestPtr := Ptr(Seg(DestPtr^) + SelectorInc, 0)
    else
      DestPtr := DestPtr + BytesRead;
    if not baseReadFile(aHandle, BytesToGo, DestPtr^, BytesRead) then begin
      btfReadFile := false;
      Exit;
    end;
    inc(aBytesRead, BytesRead);
  end;
end;
{--------}
function btfRenameFile(aName    : PAnsiChar;
                       aNewName : PAnsiChar) : boolean;
var
  DosError : word;
begin
  asm
    push ds
    mov ah, $56
    lds dx, aName
    les di, aNewName
    {$IFDEF DPMIOrWnd}
    call DOS3Call
    {$ELSE}
    int $21
    {$ENDIF}
    pop ds
    jc @@Error
    xor ax, ax
  @@Error:
    mov DosError, ax
  end;
  btfRenameFile := CheckForSuccess($5600, DosError);
end;
{--------}
function btfSetEOF(aHandle : TbtfHandle;
                   aOffset : longint) : boolean;
var
  DosError : word;
begin
  if not btfPositionFile(aHandle, aOffset) then begin
    btfSetEOF := false;
    Exit;
  end;
  asm
    push ds
    mov ah, $40
    mov bx, aHandle
    xor cx, cx
    mov ds, cx
    mov dx, cx
    {$IFDEF DPMIOrWnd}
    call DOS3Call
    {$ELSE}
    int $21
    {$ENDIF}
    pop ds
    jc @@Error
    xor ax, ax
  @@Error:
    mov DosError, ax
  end;
  btfSetEOF := CheckForSuccess($4000, DosError);
end;
{--------}
function btfUnlockFile(aHandle : TbtfHandle;
                       aStart  : longint;
                       aCount  : longint) : boolean;
var
  DosError : word;
begin
  asm
    mov ax, $5C01
    mov bx, aHandle
    mov cx, aStart.word[2]
    mov dx, aStart.word[0]
    mov si, aCount.word[2]
    mov di, aCount.word[0]
    {$IFDEF DPMIOrWnd}
    call DOS3Call
    {$ELSE}
    int $21
    {$ENDIF}
    jc @@Error
    xor ax, ax
  @@Error:
    mov DosError, ax
  end;
  btfUnlockFile := CheckForSuccess($5C01, DosError);
end;
{--------}
function btfWriteFile(aHandle       : TbtfHandle;
                      aToWrite      : longint;
                const aBuffer;
                  var aBytesWritten : longint) : boolean;
const
  BlockSize = 32*1024;
  {$IFDEF MSDOS}
  SelectorInc = $1000;
  {$ENDIF}
var
  SourcePtr   : PAnsiChar;
  BytesToGo   : longint;
  BytesToWrite: word;
  BytesWrit   : word;
  MustIncSelector : boolean;
begin
  btfWriteFile := true;
  aBytesWritten := 0;
  BytesToGo := aToWrite;
  SourcePtr := @aBuffer;

  MustIncSelector := (Ofs(SourcePtr^) >= BlockSize);
  if ((Ofs(SourcePtr^) mod BlockSize) <> 0) then begin
    if MustIncSelector then
      BytesToWrite := MinLong(BytesToGo, succ($FFFF - Ofs(SourcePtr^)))
    else
      BytesToWrite := MinLong(BytesToGo, ($8000 - Ofs(SourcePtr^)))
  end
  else begin
    BytesToWrite := MinLong(BytesToGo, BlockSize);
  end;
  if not baseWriteFile(aHandle, BytesToWrite, SourcePtr^, BytesWrit) then begin
    btfWriteFile := false;
    Exit;
  end;
  inc(aBytesWritten, BytesWrit);
  if (BytesToWrite <> BytesWrit) then
    Exit;
  dec(BytesToGo, BytesWrit);

  while (BytesToGo > BlockSize) do begin
    if MustIncSelector then
      SourcePtr := Ptr(Seg(SourcePtr^) + SelectorInc, 0)
    else
      SourcePtr := SourcePtr + BytesWrit;
    MustIncSelector := not MustIncSelector;
    BytesToWrite := MinLong(BytesToGo, BlockSize);
    if not baseWriteFile(aHandle, BytesToWrite, SourcePtr^, BytesWrit) then begin
      btfWriteFile := false;
      Exit;
    end;
    inc(aBytesWritten, BytesWrit);
    if (BytesToWrite <> BytesWrit) then
      Exit;
    dec(BytesToGo, BlockSize);
  end;
  if (BytesToGo <> 0) then begin
    if MustIncSelector then
      SourcePtr := Ptr(Seg(SourcePtr^) + SelectorInc, 0)
    else
      SourcePtr := SourcePtr + BytesWrit;
    if not baseWriteFile(aHandle, BytesToGo, SourcePtr^, BytesWrit) then begin
      btfWriteFile := false;
      Exit;
    end;
    inc(aBytesWritten, BytesWrit);
  end;
end;
{$ENDIF}
{====================================================================}


{===Win32 calls======================================================}
{$IFDEF Win32}
function SetErrorCode(aDosFunc  : word) : boolean;
begin
  Result := false;
  btfDOSFunc := aDosFunc;
  btfDOSError := GetLastError;
end;
{--------}
function btfCloseFile(aHandle : TbtfHandle) : boolean;
begin
  if not CloseHandle(aHandle) then
    Result := SetErrorCode($3E00)
  else
    Result := true;
end;
{--------}
function btfDeleteFile(aName : PAnsiChar) : boolean;
begin
  if not DeleteFile(aName) then
    Result := SetErrorCode($4100)
  else
    Result := true;
end;
{--------}
function btfFlushFile(aHandle : TbtfHandle) : boolean;
begin
  if not FlushFileBuffers(aHandle) then
    Result := SetErrorCode($6800)
  else
    Result := true;
end;
{--------}
function btfGetPositionFile(aHandle : TbtfHandle;
                        var aOffset : DWORD) : boolean;        {!!.54}
begin
  aOffset := SetFilePointer(aHandle, 0, nil, FILE_CURRENT);
  if (aOffset = $FFFFFFFF) then
    Result := SetErrorCode($4201)
  else
    Result := true;
end;
{--------}
function btfLockFile(aHandle : TbtfHandle;
                     aStart  : longint;
                     aCount  : longint) : boolean;
begin
  if not LockFile(aHandle, aStart, 0, aCount, 0) then
    Result := SetErrorCode($5C00)
  else
    Result := true;
end;
{--------}
function btfOpenFile(aName       : PAnsiChar;
                     aOpenMode   : TbtfOpenMode;
                     aShareMode  : TbtfShareMode;
                     aWriteThru  : boolean;
                     aCreateFile : boolean;
                     aInheritable: boolean;
                 var aHandle     : TbtfHandle) : boolean;
var
  OpenMode   : longint;
  ShareMode  : longint;
  CreateMode : longint;
  AttrFlags  : longint;
begin
  {initialise parameters to CreateFile}
  if (aOpenMode = bomReadOnly) then
    OpenMode := GENERIC_READ
  else
    OpenMode := GENERIC_READ or GENERIC_WRITE;
  if (aShareMode = bsmExclusive) then
    ShareMode := 0
  else
    ShareMode := FILE_SHARE_READ or FILE_SHARE_WRITE;
  if aCreateFile then
    CreateMode := CREATE_ALWAYS
  else
    CreateMode := OPEN_EXISTING;
  if aWriteThru then
    AttrFlags := FILE_ATTRIBUTE_NORMAL or FILE_FLAG_WRITE_THROUGH
  else
    AttrFlags := FILE_ATTRIBUTE_NORMAL;
  {the aInheritable flag is ignored}
  {open the file}
  aHandle := CreateFile(aName,
                        OpenMode,
                        ShareMode,
                        nil,            {!! Security attrs}
                        CreateMode,
                        AttrFlags,
                        0);
  if (aHandle = INVALID_HANDLE_VALUE) then
    Result := SetErrorCode($6C00)
  else
    Result := true;
end;
{--------}
function btfPositionFile(aHandle : TbtfHandle;
                         aOffset : longint) : boolean;
var
  SeekResult : DWORD;                                          {!!.54}
begin
  SeekResult := SetFilePointer(aHandle, aOffset, nil, FILE_BEGIN);
  if (SeekResult = $FFFFFFFF) then
    Result := SetErrorCode($4200)
  else
    Result := true;
end;
{--------}
function btfPositionFileEOF(aHandle   : TbtfHandle;
                        var aFileSize : DWORD) : boolean;      {!!.54}
begin
  aFileSize := SetFilePointer(aHandle, 0, nil, FILE_END);
  if (aFileSize = $FFFFFFFF) then
    Result := SetErrorCode($4202)
  else
    Result := true;
end;
{--------}
function btfReadFile(aHandle    : TbtfHandle;
                     aToRead    : longint;
                 var aBuffer;
                 var aBytesRead : longint) : boolean;
var
  BR : DWORD;
begin
  if not ReadFile(aHandle, aBuffer, aToRead, BR, nil) then
    Result := SetErrorCode($3F00)
  else begin
    Result := true;
    aBytesRead := BR;
  end;
end;
{--------}
function btfRenameFile(aName    : PAnsiChar;
                       aNewName : PAnsiChar) : boolean;
begin
  if not MoveFile(aName, aNewName) then
    Result := SetErrorCode($5600)
  else
    Result := true;
end;
{--------}
function btfSetEOF(aHandle : TbtfHandle;
                   aOffset : longint) : boolean;
begin
  if not btfPositionFile(aHandle, aOffset) then begin
    Result := false;
    Exit;
  end;
  if not Windows.SetEndOfFile(aHandle) then
    Result := SetErrorCode($4000)
  else
    Result := true;
end;
{--------}
function btfUnlockFile(aHandle : TbtfHandle;
                       aStart  : longint;
                       aCount  : longint) : boolean;
begin
  if not UnlockFile(aHandle, aStart, 0, aCount, 0) then
    Result := SetErrorCode($5C01)
  else
    Result := true;
end;
{--------}
function btfWriteFile(aHandle       : TbtfHandle;
                      aToWrite      : longint;
                const aBuffer;
                  var aBytesWritten : longint) : boolean;
var
  BW : DWORD;
begin
  if not WriteFile(aHandle, aBuffer, aToWrite, BW, nil) then
    Result := SetErrorCode($4000)
  else begin
    Result := true;
    aBytesWritten := BW;
  end;
end;
{$ENDIF}
{====================================================================}

end.
