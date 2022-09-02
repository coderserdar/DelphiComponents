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
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

const
  PageSize         = 16384;
  fmEMS            = $D7BF;
  EMSFILEMAX       = 128;
  EmsErrorCode     = $0FFFF; {Used by several functions to signal an error}

type
  FileRec = record
              Handle: Word;
              Mode: Word;
              RecSize: Word;
              Private: array[1..26] of Byte;
              CurrentPage : Byte;
              NumPages    : Byte;
              Index       : LongInt;
              ActualSize  : LongInt;
              EMSHandle   : Word;
              UserData: array[13..16] of Byte;
              Name        : String[79];
            end;

  EMSFatRec        = record
                       ID      : Word;
                       Handle  : Word;
                       Size    : LongInt;
                     end;
  EMSFatType       = Array[1..EMSFILEMAX] of EMSFatRec;
  EMSFatTypePtr    = ^EMSFatType;
  EMSPage          =  Array[1..PAGESIZE] of Byte;
  ShortEMSPage     =  Array[1..16000] of Byte;
  EMSPageFrameType = record
                       Page0,Page1,Page2   : EMSPage;
                       Page3 : ShortEMSPage;
                     end;

  PageFramePtr     = ^EMSPageFrameType;

var
  EMSFat           : EMSFatTypePtr;
  EMSPageFrame     : PageFramePtr;
  EMSSaveExitProc  : Pointer;
  NumInEMSFat      : Word;
  Pages            : Array[0..3] of ^EMSPage;
  CurrentMapping   : record
                       StartPage     : Word;
                       PagesMapped   : Word;
                       Hand          : Word;
                     end;

procedure EMS_Close_All;
var
  I                : Word;

begin
  if not UsingEMS then Exit;
  for I := 1 to NumInEMSFat do
    if not DeallocateEMSHandle(EMSFat^[I].Handle) then
      MSortResult := MSortEMSError;
  NumInEMSFat := 0;
end;

procedure EMS_ExitProc;
begin
  ExitProc := EMSSaveExitProc;
  EMS_Close_All;
end;

function EMS_EOF(var F : File) : Boolean;

begin
  with FileRec(F) do
    EMS_EOF := (Index >= ActualSize);
end;

function PosDigit(var S : String) : Word;
Inline(
  $5F/                   {    pop    di}
  $07/                   {    pop    es}
  $30/$E4/               {    xor    ah,ah}
  $26/                   {    es:}
  $8A/$05/               {    mov    al,[di]}
  $89/$C1/               {    mov    cx,ax}
                         {NextChar:}
  $47/                   {    inc    di}
  $26/                   {    es:}
  $8A/$15/               {    mov    dl,[di]}
  $80/$FA/$30/           {    cmp    dl,'0'}
  $72/$0A/               {    jb     KeepLooking}
  $80/$FA/$39/           {    cmp    dl,'9'}
  $77/$05/               {    ja     KeepLooking}
  $49/                   {    dec    cx}
  $29/$C8/               {    sub    ax,cx}
  $EB/$04/               {    jmp    short ExitPoint}
                         {KeepLooking:}
  $E2/$EB/               {    loop   NextChar}
  $31/$C0);              {    xor    ax,ax}
                         {ExitPoint:}

function JustFilename(var PathName : string) : string;  {!!.05}
  {-Return just the filename of a pathname}             {!!.05}
var                                                     {!!.05}
  I : Word;                                             {!!.05}
begin                                                   {!!.05}
  I := Succ(Word(Length(PathName)));                    {!!.05}
  repeat                                                {!!.05}
    Dec(I);                                             {!!.05}
  until (PathName[I] in ['\', ':', #0]) or (I = 0);     {!!.05}
  JustFilename := Copy(PathName, Succ(I), 64);          {!!.05}
end;                                                    {!!.05}

function ExtractNumber(var Name : PathName) : Word;     {!!.05}

var
  I,P,FNum         : Word;
  NStr             : String[3];
  FName            : PathName;
begin
  {find the number from the filename}
  FName := JustFileName(Name);
  P := PosDigit(FName);
  if P <> 0 then begin
    NStr := Copy(FName,P,Length(FName));
    Val(NStr,FNum,P);
    if P <> 0 then
      Exit;
  end
  else
    FNum := 0;
  ExtractNumber := FNum;
end;

function FileInEMS(var Name : PathName; var FSize : LongInt;
                   var EMSHandle : Word; var I : Word) : Boolean;

var
  FNum             : Word;
  KeepLooking      : Boolean;

begin
  FileInEMS := FALSE;
  FNum := ExtractNumber(Name);
  if FNum = 0 then
    Exit;

  I := 1;
  KeepLooking := TRUE;
  while (KeepLooking) and (I <= NumInEMSFat) do begin
    if FNum = EMSFat^[I].ID then begin
      KeepLooking := FALSE;
      with EMSFat^[I] do begin
        FileInEMS := TRUE;

        EMSHandle := Handle;

        FSize     := Size;
      end;
    end
    else
      Inc(I);
  end;
end;

function EMS_Init : Boolean;

begin
  if UseEMS and EMSInstalled and (EMSPagesAvail > 0) then
  begin
    EMS_Init := TRUE;
    EMSPageFrame := EMSPageFramePtr;
    with EMSPageFrame^ do begin
      Pages[0]  := @Page0;
      Pages[1]  := @Page1;
      Pages[2]  := @Page2;
      Pages[3]  := @Page3;
    end;
    NumInEMSFat  := 0;
    FillChar(CurrentMapping,SizeOf(CurrentMapping),0);
  end
  else
    EMS_Init := FALSE;
end;

function MapTheEMSPages(EMSHandle : Word; Page : Word; Index : LongInt;
                         Size : Word) : Word;
{***************}
var
  HighPageNeeded,I,MaxPage
                   : Word;
  MappingRequired  : Boolean;

begin
  MapTheEMSPages := 0;
  HighPageNeeded := ((Index + Size - 2) DIV PAGESIZE);

  with CurrentMapping do begin
    Hand            := EMSHandle;
    PagesMapped     := (HighPageNeeded - Page) + 1;
    StartPage       := Page;
    for I := 0 to HighPageNeeded do begin
      if not MapEMSPage(EMSHandle,Page+I,I) then
        MSortResult := MSortEMSError;
    end;
  end;
end;

procedure EMSBlockWrite(var F : File; var Rec; Size : Word; var Num : Word);

var
  PageNum,ActualIndex
                   : Word;

begin
  with FileRec(F) do begin
    PageNum     := Word(Index DIV PAGESIZE);
    ActualIndex := Word(Index MOD PAGESIZE);

    PageNum := MapTheEMSPages(EMSHandle,PageNum,ActualIndex,Size);

    Move(Rec,Pages[PageNum]^[ActualIndex],Size);
    Index := Index + Size;
    ActualSize := ActualSize + Size;
  end
end;

procedure EMSBlockRead(var F : File; var Rec; Size : Word; var Num : Word);

var
  PageNum,ActualIndex
                   : Word;
  I                : LongInt;

begin
  with FileRec(F) do begin
    PageNum     := Word(Index DIV PAGESIZE);
    ActualIndex := Word(Index MOD PAGESIZE);
    I := Index + Size;
    if Index + Size > ActualSize then begin
      Size  := (ActualSize - Index) + 1;
      Index := Index + Size;
    end
    else
      Index := I;

    PageNum := MapTheEMSPages(EMSHandle,PageNum,ActualIndex,Size);

    Move(Pages[PageNum]^[ActualIndex],Rec,Size);
    Num := Size;
  end;
end;

procedure Open_InFile(var F : File; FName : PathName; I : Word);

var
  Hand,Ix          : Word;
  Sz               : LongInt;

begin
  if FileInEMS(FName,Sz,Hand,Ix) and (NumInEMSFat <= EMSFILEMAX) then begin {!!.52}
    with FileRec(F) do begin
      Mode        := fmEMS;
      CurrentPage := 0;
      Index       := 1;
      ActualSize  := Sz;
      EMSHandle   := Hand;
    end;
  end
  else begin
    Assign(F,FName);
    Reset(F,1);
    MSortIOResult := IOResult;
    if MSortIOResult <> 0 then
      MSortResult := MSortDiskError;
    BufferPos[I]  := FileBufferMax + 1;
    BufferSize[I] := FileBufferMax;
  end;
end;

procedure Open_OutFile(var F : File; FName : PathName; FSize : LongInt);

var
  H                : Word;
  SizeInPages      : Word;
  Avail            : Word;

begin
  {before we can open a new EMS file file for Output, we must first }
  {determine if enough EMS memory is avilable, if not, the file is  }
  {opened on disk}
  SizeInPages := Word(FSize DIV PAGESIZE);
  if FSize MOD PAGESIZE <> 0 then
    Inc(SizeInPages);
  if UsingEms then                               {!!.02}
    Avail       := EMSPagesAvail;
  if UsingEMS and (NumInEMSFat < EMSFILEMAX) and (SizeInPages <= Avail)
      and (SizeInPages > 0) then begin
    {we have the needed space, so set up the FileRec and allocate the EMS RAM}
    with FileRec(F) do begin
      Mode := fmEMS;
      H := AllocateEMSPages(SizeInPages);
      if H = EMSErrorCode then begin
        {an EMM error has occurred}
        MSortResult := MSortEMSError;
        Exit;
      end;
      NumPages := SizeInPages;
      ActualSize := 0;
      Inc(NumInEMSFat);
      with EMSFat^[NumInEMSFat] do begin
        Handle   := H;
        Size     := FSize;
        ID       := ExtractNumber(FName);
      end;
      Name        := FName;
      EMSHandle   := H;
      CurrentPage := 0;
      Index := 1;
    end;
  end
  else begin
    Assign(F,FName);
    Rewrite(F,1);
    BufferPos[0]  := 1;
    MSortIOResult := IOResult;
    if MSortIOResult <> 0 then
      MSortResult := MSortDiskError;
  end;
end;

procedure RemoveFromEMSFat(var F : File);


begin
end;

procedure CloseEMSFile(var F : File);
var
  FName            : PathName;
  H,Ix             : Word;
  Sz               : LongInt;

begin
  FName := FileRec(F).Name;
  if FileInEMS(FName,Sz,H,Ix) then
    with EMSFat^[Ix] do
      Size := FileRec(F).ActualSize;
end;

procedure DisposeEMSFile(var F : File);

begin
  with FileRec(F) do begin
    if not DeallocateEMSHandle(EMSHandle) then
      MSortResult := MSortEMSError;
    Mode := fmClosed;
  end;
  RemoveFromEMSFat(F);
end;
