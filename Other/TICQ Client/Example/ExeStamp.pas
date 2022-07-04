(*

eICQ: the free ICQ for Microsoft(tm) Windows(tm)

Copyright 2003-2004 eICQ ICQ project,
all portions of this codebase are copyrighted to the people
listed in contributors.txt.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*)

unit ExeStamp;

interface

function GetExeTimeStamp(pData: PChar): Cardinal;

{-

GetExeTimeStamp takes a pointer which is assumed to point to the image of an
executable file. This can be either a file loaded from disk, or can be the
HInstance pointer to the currently running program. This allows extracting the
compile date directly from the current process. It returns a date in DOS date
time format, a longint broken into bit fields. Use FileDateToDateTime() to
convert it into a Delphi TDateTime value.

-}

implementation

uses
   Windows;

function GetExeTimeStamp(pData: PChar): Cardinal;
var
   p : PChar;
   i : Integer;

   pDosHeader   : PImageDosHeader;
   pFileHeader  : PImageFileHeader;
   pSection     : PImageSectionHeader;
   pResSection  : PImageSectionHeader;
   pResDirEntry : PImageExportDirectory;

begin
  Result := 0;

  p := pData;

  pDosHeader := PImageDosHeader(p);

  Inc(p, pDosHeader^._lfanew);            // advance to "new" header
  Inc(p, SizeOf(Cardinal));               // move past signature

  pFileHeader := PImageFileHeader(p);     // point to PE file header
  Inc(p, SizeOf(TImageFileHeader));       // move past it
  Inc(p, SizeOf(TImageOptionalHeader));   // move past optional header

  pResSection := nil;

  i := 0;                                 // scan remaining sections
  while (i < pFileHeader^.NumberOfSections) do begin
    pSection := PImageSectionHeader(p);
    Inc(p, SizeOf( TImageSectionHeader));

    if pchar( @pSection^.Name ) = '.rsrc' then
       pResSection := pSection;           // store pointer to resources

    Inc(i);
  end;

  if pResSection <> nil then begin
    p := pData;                           // go back to start of image

    if p = Pointer(HInstance) then        // advance to resource area
       Inc(p, pResSection^.VirtualAddress)
    else
       Inc(p, pResSection^.PointerToRawData);

    pResDirEntry := PImageExportDirectory(p); // point to first resource
    Result := pResDirEntry^.TimeDateStamp;    // return the timestamp
  end;
end;

end.
