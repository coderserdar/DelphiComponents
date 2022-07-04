{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Creation:     Oct 25, 2005
Description:  Fast streams for ICS tested on D5 and D7.
Version:      6.10
Legal issues: Copyright (C) 2005-2009 by Arno Garrels, Berlin, Germany,
              contact: <arno.garrels@gmx.de>
              
              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

History:
Jan 05, 2006 V1.01 F. Piette added missing resourcestring for Delphi 6
Mar 26, 2006 V6.00 F. Piette started new version 6
Jun 26, 2006 V6.01 A. Garrels fixed corrupted data when Mode contained
             fmOpenWrite. Even in case of fmOpenWrite is set the class needs
             to be able to read from file to keep it's buffer in sync.
Aug 28, 2006 V6.02 Tobias Giesen <tobias_subscriber@tgtools.com> fixed a
             bug in SetSize.
Aug 31, 2006 V6.03 A. Garrels added TMultipartFileReader, a read only file
             stream capable to merge a custom header as well as a footer
             with the file. For instance usefull as a HTTP-POST-stream.             
Aug 31, 2006 V6.04 Do not call SetSize in destroy by A. Garrels.
Jun 01, 2007 V6.05 A. Garrels added TTextStream. A very fast stream wrapper,
             optimized to read and write lines. Methods ReadLn as well
             as WriteLn benefit from internal buffering as long as the same
             method is called more than once in sequence, intended as a
             replacement of ReadLn(TextFile) and WriteLn(TextFile).
Jan 22, 2008 V6.06 Angus allow for read file shrinking with fmShareDenyNone
Mar 24, 2008 V6.07 Francois Piette made some changes to prepare code
                   for Unicode:
                   TTextStream use AnsiString.
Apr 15, 2008 V6.08 A. Garrels, in FBuf of TBufferedFileStream changed to
                   PAnsiChar
Aug 27, 2008 V6.09 Arno added a WideString overload to TBufferedFileStream
Jan 20, 2009 V6.10 Arno added property Mode to TBufferedFileStream.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsStreams;

interface
{$Q-}           { Disable overflow checking           }
{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$I OverbyteIcsDefs.inc}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFNDEF VER80}   { Not for Delphi 1                    }
    {$H+}         { Use long strings                    }
    {$J+}         { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

uses
    Windows, SysUtils, Classes,
{$IFDEF COMPILER6_UP}
    RTLConsts
{$ELSE}
    Consts
{$ENDIF},
    OverByteIcsUtils;

{$IFDEF COMPILER6_UP}
resourcestring
  SFCreateErrorEx = 'Cannot create file "%s". %s';
  SFOpenErrorEx   = 'Cannot open file "%s". %s';
{$ENDIF}

const
    DEFAULT_BUFSIZE = 4096;
    MIN_BUFSIZE     = 512;
    MAX_BUFSIZE     = 1024 * 64;

type
    BigInt = {$IFDEF COMPILER6_UP} Int64 {$ELSE} Longint {$ENDIF};
    TBufferedFileStream = class(TStream)
    private
        FHandle     : Longint;
        FFileSize   : BigInt;
        FFileOffset : BigInt;
        FBuf        : PAnsiChar;
        FBufSize    : Longint;
        FBufCount   : Longint;
        FBufPos     : Longint;
        FDirty      : Boolean;
        FMode       : Word;
        //FmWriteFlag : Boolean;  { V1.04 }        
    protected
        procedure   SetSize(NewSize: Longint); override;
{$IFDEF COMPILER6_UP}
        procedure   SetSize(const NewSize: Int64); override;
{$ENDIF}
        function    GetFileSize: BigInt;
        procedure   Init(BufSize: Longint);
        procedure   ReadFromFile;
        procedure   WriteToFile;
    public
        constructor Create(const FileName: String; Mode: Word; BufferSize: Longint);{$IFDEF COMPILER6_UP} overload; {$ENDIF}
        constructor Create(const FileName: WideString; Mode: Word; BufferSize: Longint); {$IFDEF COMPILER6_UP} overload; {$ENDIF}
{$IFDEF COMPILER6_UP}
        constructor Create(const FileName: String; Mode: Word; Rights: Cardinal; BufferSize: Longint); overload;
        constructor Create(const FileName: WideString; Mode: Word; Rights: Cardinal; BufferSize: Longint); overload;
{$ENDIF}
        destructor  Destroy; override;

        procedure   Flush;
        function    Read(var Buffer; Count: Longint): Longint; override;

        function    Seek(Offset: Longint; Origin: Word): Longint; override;
{$IFDEF COMPILER6_UP}
        function    Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
{$ENDIF}
        function    Write(const Buffer; Count: Longint): Longint; override;
        property    FastSize : BigInt read FFileSize;
        property    Mode : Word read FMode;
    end;

    EMultiPartFileReaderException = class(Exception);
{ Read only file stream capable to merge a custom header as well as a footer }
{ with the file. For instance usefull as a HTTP-POST-stream.                 }
    TMultiPartFileReader = class(TFileStream)
    private
        FHeader     : String;
        FFooter     : String;
        FFooterLen  : Integer;
        FHeaderLen  : Integer;
        FCurrentPos : BigInt;
        FFileSize   : BigInt;
    protected
        function    GetSize: {$IFDEF COMPILER6_UP}Int64{$ELSE}Longint{$ENDIF}; {$IFDEF Compiler7_UP}override;{$ENDIF}
    public
        constructor Create(const FileName: String; Mode: Word; const Header, Footer: String); {$IFDEF COMPILER6_UP}overload;{$ENDIF}
{$IFDEF COMPILER6_UP}
        constructor Create(const FileName: String; Mode: Word; Rights: Cardinal; const Header, Footer: String); overload;
{$ENDIF}
        procedure   SetSize(const NewSize: {$IFDEF COMPILER6_UP}Int64{$ELSE}Longint{$ENDIF}); override;
        function    Seek(Offset: Longint; Origin: Word): Longint; override;
{$IFDEF COMPILER6_UP}
        function    Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
{$ENDIF}
        function    Read(var Buffer; Count: Longint): Longint; override;
        function    Write(const Buffer; Count: Longint): Longint; override;
        property    Header : String read FHeader;
        property    Footer : String read FFooter;
    end;

    TTextStreamMode = (tsmReadLn, tsmWriteLn, tsmRead, tsmWrite);
    TTextStream = class(TStream)
    private
        FStream : TStream;
        FBuf    : array of AnsiChar;
        TF      : TextFile;
        FMode   : TTextStreamMode;
        procedure SetRealPos;
        procedure SetMode(NewMode: TTextStreamMode);
    protected
{$IFDEF COMPILER6_UP}
        procedure SetSize(const NewSize: Int64); override;
{$ELSE}
        procedure SetSize(NewSize: Longint); override;
{$ENDIF}
    public
        constructor Create(AStream : TStream; BufferSize : Integer = 256
{$IFDEF COMPILER6_UP}
            ; Style: TTextLineBreakStyle = tlbsCRLF
{$ENDIF}
            );
        destructor Destroy; override;
        procedure Flush;
        function  ReadLn: Boolean; overload;
        function  ReadLn(var S: AnsiString): Boolean; overload;
{$IFDEF COMPILER6_UP}
        function  ReadLn(var WS: WideString): Boolean; overload;
{$ENDIF}
        procedure WriteLn(const S: AnsiString); {$IFDEF COMPILER6_UP} overload;
        procedure WriteLn(const WS: WideString); overload; {$ENDIF}

        function  Read(var Buffer; Count: Longint): Longint; override;
        function  Write(const Buffer; Count: Longint): Longint; override;

        function  Seek(Offset: Longint; Origin: Word): Longint; override;
{$IFDEF COMPILER6_UP}
        function  Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
{$ENDIF}
    end;

implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Min(IntOne, IntTwo: BigInt): BigInt;
begin
    if IntOne > IntTwo then
        Result := IntTwo
    else
        Result := IntOne;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBufferedFileStream.Init(BufSize: Longint);
begin
    FBufSize := BufSize;
    if FBufSize < MIN_BUFSIZE then
        FBufsize := MIN_BUFSIZE
    else
    if FBufSize > MAX_BUFSIZE then
        FBufSize := MAX_BUFSIZE
    else
    if (FBufSize mod MIN_BUFSIZE) <> 0 then
        FBufSize := DEFAULT_BUFSIZE;
    GetMem(FBuf, FBufSize);
    FFileSize   := GetFileSize;
    FBufCount   := 0;
    FFileOffset := 0;
    FBufPos     := 0;
    FDirty      := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TBufferedFileStream.Create(const FileName: String; Mode: Word;
    BufferSize: Longint);
begin
{$IFDEF COMPILER6_UP}
    Create(Filename, Mode, 0, BufferSize);
{$ELSE}
    inherited Create;
    FHandle := -1;
    FBuf    := nil;
    //FmWriteFlag := FALSE;   { V1.04 }
    if Mode = fmCreate then begin
        FHandle := FileCreate(FileName);
        if FHandle < 0 then
            raise EFCreateError.CreateFmt(SFCreateError, [FileName]);
    end
    else begin
        { Even in mode fmOpenWrite we need to read from file as well }
        if Mode and fmOpenWrite <> 0 then begin
            Mode := Mode and not fmOpenWrite;
            Mode := Mode or fmOpenReadWrite;
            //FmWriteFlag := TRUE; { V1.04 }
        end;
        FHandle := FileOpen(FileName, Mode);
        if FHandle < 0 then
            raise EFOpenError.CreateFmt(SFOpenError, [FileName]);
    end;
    FMode := Mode;
    Init(BufferSize);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER6_UP}
constructor TBufferedFileStream.Create(const FileName : String; Mode: Word;
    Rights: Cardinal; BufferSize: Longint);
begin
    inherited Create;
    FHandle := -1;
    FBuf    := nil;
    //FmWriteFlag := FALSE;  { V1.04 }
    if Mode = fmCreate then begin
        FHandle := FileCreate(FileName, Rights);
        if FHandle < 0 then
            raise EFCreateError.CreateResFmt(@SFCreateErrorEx,
                                             [ExpandFileName(FileName),
                                             SysErrorMessage(GetLastError)]);
    end
    else begin
        { Even in mode fmOpenWrite we need to read from file as well }
        if (Mode and fmOpenWrite <> 0) then begin
            Mode := Mode and not fmOpenWrite;
            Mode := Mode or fmOpenReadWrite;
            //FmWriteFlag := TRUE;  { V1.04 }
        end;
        FHandle := FileOpen(FileName, Mode);
        if FHandle < 0 then
            raise EFOpenError.CreateResFmt(@SFOpenErrorEx,
                                           [ExpandFileName(FileName),
                                           SysErrorMessage(GetLastError)]);
    end;
    FMode := Mode;
    Init(BufferSize);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TBufferedFileStream.Create(const FileName: WideString; Mode: Word;
    BufferSize: Longint);
begin
{$IFDEF COMPILER6_UP}
    Create(Filename, Mode, 0, BufferSize);
{$ELSE}
    inherited Create;
    FHandle := -1;
    FBuf    := nil;
    //FmWriteFlag := FALSE;   { V1.04 }
    if Mode = fmCreate then begin
        FHandle := IcsFileCreateW(FileName);
        if FHandle < 0 then
            raise EFCreateError.CreateFmt(SFCreateError, [FileName]);
    end
    else begin
        { Even in mode fmOpenWrite we need to read from file as well }
        if Mode and fmOpenWrite <> 0 then begin
            Mode := Mode and not fmOpenWrite;
            Mode := Mode or fmOpenReadWrite;
            //FmWriteFlag := TRUE; { V1.04 }
        end;
        FHandle := IcsFileOpenW(FileName, Mode);
        if FHandle < 0 then
            raise EFOpenError.CreateFmt(SFOpenError, [FileName]);
    end;
    FMode := Mode;
    Init(BufferSize);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER6_UP}
constructor TBufferedFileStream.Create(const FileName : WideString; Mode: Word;
    Rights: Cardinal; BufferSize: Longint);
begin
    inherited Create;
    FHandle := -1;
    FBuf    := nil;
    //FmWriteFlag := FALSE;  { V1.04 }
    if Mode = fmCreate then begin
        FHandle := IcsFileCreateW(FileName, Rights);
        if FHandle < 0 then
            raise EFCreateError.CreateResFmt(@SFCreateErrorEx,
                                             [ExpandFileName(FileName),
                                             SysErrorMessage(GetLastError)]);
    end
    else begin
        { Even in mode fmOpenWrite we need to read from file as well }
        if (Mode and fmOpenWrite <> 0) then begin
            Mode := Mode and not fmOpenWrite;
            Mode := Mode or fmOpenReadWrite;
            //FmWriteFlag := TRUE;  { V1.04 }
        end;
        FHandle := IcsFileOpenW(FileName, Mode);
        if FHandle < 0 then
            raise EFOpenError.CreateResFmt(@SFOpenErrorEx,
                                           [ExpandFileName(FileName),
                                           SysErrorMessage(GetLastError)]);
    end;
    FMode := Mode;
    Init(BufferSize);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TBufferedFileStream.Destroy;
begin
    if FHandle >= 0 then begin
        if FDirty then
            WriteToFile;
        //if FmWriteFlag then      { V1.04 }
            //SetSize(Position);   { V1.04 }
        FileClose(FHandle);
    end;
    if FBuf <> nil then
        FreeMem(FBuf, FBufSize);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TBufferedFileStream.GetFileSize: BigInt;
var
    OldPos : BigInt;
begin
    OldPos := FileSeek(FHandle,
                      {$IFDEF DELPHI6_UP} Int64(0) {$ELSE} 0 {$ENDIF},
                      soFromCurrent);
    Result := FileSeek(FHandle,
                      {$IFDEF DELPHI6_UP}Int64(0){$ELSE}0{$ENDIF},
                      soFromEnd);
    FileSeek(FHandle, OldPos, soFromBeginning);
    if Result < 0 then
        raise Exception.Create('Cannot determine correct file size');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBufferedFileStream.ReadFromFile;
var
    NewPos : BigInt;
begin
    NewPos := FileSeek(FHandle, FFileOffset, soFromBeginning);
    if (NewPos <> FFileOffset) then
        raise Exception.Create('Seek before read from file failed');
    FBufCount := FileRead(FHandle, FBuf^, FBufSize);
    if FBufCount = -1 then
        raise Exception.Create(SysErrorMessage(GetLastError));
        //FBufCount := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBufferedFileStream.WriteToFile;
var
    NewPos : BigInt;
    BytesWritten : Longint;
begin
    NewPos := FileSeek(FHandle, FFileOffset, soFromBeginning);
    if (NewPos <> FFileOffset) then
        raise Exception.Create('Seek before write to file failed');
    BytesWritten := FileWrite(FHandle, FBuf^, FBufCount);
    if (BytesWritten <> FBufCount) then
        raise Exception.Create('Could not write to file');
    FDirty := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBufferedFileStream.Flush;
begin
    if FDirty and (FHandle >= 0) and (FBuf <> nil) then
        WriteToFile;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TBufferedFileStream.Read(var Buffer; Count: Longint): Longint;
var
    Remaining   : Longint;
    Copied      : Longint;
    DestPos     : Longint;
begin
    Result := 0;
    if FHandle < 0 then Exit;
    Remaining := Min(Count, FFileSize - (FFileOffset + FBufPos));
    Result := Remaining;
    if (Remaining > 0) then begin
        if (FBufCount = 0) then
            ReadFromFile;
        Copied := Min(Remaining, FBufCount - FBufPos);
        Move(FBuf[FBufPos], TByteArray(Buffer)[0], Copied);
        Inc(FBufPos, Copied);
        Dec(Remaining, Copied);
        DestPos := 0;
        while Remaining > 0 do begin
            if FDirty then
                WriteToFile;
            FBufPos := 0;
            Inc(FFileOffset, FBufSize);
            ReadFromFile;
            Inc(DestPos, Copied);
            Copied := Min(Remaining, FBufCount - FBufPos);
            if Copied <= 0 then break;  { V6.06 angus, nothing more to read, break loop }
            Move(FBuf[FBufPos], TByteArray(Buffer)[DestPos], Copied);
            Inc(FBufPos, Copied);
            Dec(Remaining, Copied);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TBufferedFileStream.Write(const Buffer; Count: Longint): Longint;
var
    Remaining : Longint;
    Copied    : Longint;
    DestPos   : Longint;
begin
    Result := 0;
    if FHandle < 0 then Exit;
    Remaining := Count;
    Result := Remaining;
    if (Remaining > 0) then begin
        if (FBufCount = 0) and ((FFileOffset + FBufPos) <= FFileSize) then
            ReadFromFile;
        Copied := Min(Remaining, FBufSize - FBufPos);
        Move(PAnsiChar(Buffer), FBuf[FBufPos], Copied);
        FDirty := True;
        Inc(FBufPos, Copied);
        if (FBufCount < FBufPos) then begin
            FBufCount := FBufPos;
            FFileSize := FFileOffset + FBufPos;
        end;
        Dec(Remaining, Copied);
        DestPos := 0;
        while Remaining > 0 do begin
            WriteToFile;
            FBufPos := 0;
            Inc(FFileOffset, FBufSize);
            if (FFileOffset < FFileSize) then
                ReadFromFile
            else
                FBufCount := 0;
            Inc(DestPos, Copied);
            Copied := Min(Remaining, FBufSize - FBufPos);
            if Copied <= 0 then break;  { V6.06 angus, nothing more to read, break loop }
            Move(TByteArray(Buffer)[DestPos], FBuf[0], Copied);
            FDirty := True;
            Inc(FBufPos, Copied);
            if (FBufCount < FBufPos) then begin
                FBufCount := FBufPos;
                FFileSize := FFileOffset + FBufPos;
            end;
            Dec(Remaining, Copied);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TBufferedFileStream.Seek(Offset: Longint; Origin: Word): Longint;
{$IFNDEF COMPILER6_UP}
var
    NewPos    : Longint;
    NewOffset : Longint;
{$ENDIF}
begin
{$IFDEF COMPILER6_UP}
    Result := Seek(Int64(Offset), TSeekOrigin(Origin));
{$ELSE}
    Result := 0;
    if FHandle < 0 then Exit;
    if (Offset = 0) and (Origin = soFromCurrent) then begin
        Result := FFileOffset + FBufPos;
        Exit;
    end;

    case Origin of
        soFromBeginning : NewPos := Offset;
        soFromCurrent   : NewPos := (FFileOffset + FBufPos) + Offset;
        soFromEnd       : NewPos := FFileSize + Offset;
      else
        raise Exception.Create('Invalid seek origin');
    end;

    if (NewPos < 0) then
        NewPos := 0
    else
    if (NewPos > FFileSize) then
        FFileSize := FileSeek(FHandle, NewPos - FFileSize, soFromEnd);

    NewOffset := (NewPos div FBufSize) * FBufSize;

    if (NewOffset <> FFileOffset) then begin
        if FDirty then
            WriteToFile;
        FFileOffset := NewOffset;
        FBufCount := 0;
    end;
    FBufPos := NewPos - FFileOffset;
    Result  := NewPos;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER6_UP}
function TBufferedFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
    NewPos        : BigInt;
    NewFileOffset : BigInt;
begin
    Result := 0;
    if FHandle < 0 then Exit;

    if (Offset = 0) and (Origin = soCurrent) then begin
        Result := FFileOffset + FBufPos;
        Exit;
    end;

    case Origin of
        soBeginning : NewPos := Offset;
        soCurrent   : NewPos := (FFileOffset + FBufPos) + Offset;
        soEnd       : NewPos := FFileSize + Offset;
    else
        raise Exception.Create('Invalid seek origin');
    end;

    if (NewPos < 0) then
        NewPos := 0
    else if (NewPos > FFileSize) then
        FFileSize := FileSeek(FHandle, NewPos - FFileSize, soFromEnd);

    NewFileOffset := (NewPos div FBufSize) * FBufSize;

    if (NewFileOffset <> FFileOffset) then begin
        if FDirty then
            WriteToFile;
        FFileOffset := NewFileOffset;
        FBufCount := 0;
    end;
    FBufPos := NewPos - FFileOffset;
    Result  := NewPos;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBufferedFileStream.SetSize(NewSize: Integer);
begin
{$IFDEF COMPILER6_UP}
    SetSize(Int64(NewSize));
{$ELSE}
    if FHandle < 0 then Exit;
    Seek(NewSize, soFromBeginning);
    if NewSize < FFileSize then
        FFileSize := FileSeek(FHandle, NewSize, soFromBeginning);
    if not SetEndOfFile(FHandle) then
        RaiseLastWin32Error;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER6_UP}
procedure TBufferedFileStream.SetSize(const NewSize: Int64);
begin
    if FHandle < 0 then Exit;
    Seek(NewSize, {sofromBeginning} soBeginning);   {TG 08/28/2006} { V1.03 }
    if NewSize < FFileSize then
        FFileSize := FileSeek(FHandle, NewSize, soFromBeginning);
{$IFDEF MSWINDOWS}
    if not SetEndOfFile(FHandle) then
        RaiseLastOSError;
{$ELSE}
    if ftruncate(FHandle, Position) = -1 then
        raise EStreamError(sStreamSetSize);
{$ENDIF}
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TMultiPartFileReader }

constructor TMultiPartFileReader.Create(const FileName: String; Mode: Word;
    const Header, Footer: String);
begin
{$IFDEF COMPILER6_UP}
    Create(Filename, Mode, 0, Header, Footer);
{$ELSE}
    if (Mode and fmOpenWrite <> 0) or
       (Mode and fmOpenReadWrite <> 0)  then
        raise EMultiPartFileReaderException.Create('Invalid open mode');
    inherited Create(FileName, Mode);
    FHeader     := Header;
    FHeaderLen  := Length(FHeader);
    FFooter     := Footer;
    FFooterLen  := Length(FFooter);
    FCurrentPos := 0;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER6_UP}
constructor TMultiPartFileReader.Create(const FileName: String; Mode: Word;
    Rights: Cardinal; const Header, Footer: String);
begin
    if (Mode and fmOpenWrite <> 0) or
       (Mode and fmOpenReadWrite <> 0)  then
        raise EMultiPartFileReaderException.Create('Invalid open mode');
    inherited Create(FileName, Mode, Rights);
    FHeader     := Header;
    FHeaderLen  := Length(FHeader);
    FFooter     := Footer;
    FFooterLen  := Length(FFooter);
    FCurrentPos := 0;
    FFileSize   := inherited Seek(0, soEnd);
    inherited Seek(0, soBeginning);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMultiPartFileReader.GetSize:{$IFDEF COMPILER6_UP}Int64{$ELSE}Longint{$ENDIF};
begin
    Result := FHeaderLen + FFileSize + FFooterLen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER6_UP}
procedure TMultiPartFileReader.SetSize(const NewSize: Int64);
{$ELSE}
procedure TMultiPartFileReader.SetSize(NewSize: Longint);
{$ENDIF}
begin
    raise EMultiPartFileReaderException.Create('Class is read only');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMultiPartFileReader.Read(var Buffer; Count: Integer): Longint;
var
    NewCount : Integer;
    Cnt      : Integer;
begin
    Result := 0;
    if Count <= 0 then
        Exit;
    NewCount   := Count;
    if (FCurrentPos <= FHeaderLen) then begin
        Cnt := (FHeaderLen - FCurrentPos);
        if Count < Cnt then
            Cnt := Count;
        Move(FHeader[FCurrentPos + 1], TByteArray(Buffer)[0], Cnt);
        Result := Cnt;
        NewCount := NewCount - Result;
    end;
    if Result <> Count then begin
        Cnt := inherited Read(TByteArray(Buffer)[Result], NewCount);
        Inc(Result, Cnt);
        if Result <> Count then begin
            if (Cnt < NewCount) then
                Dec(NewCount, Cnt);
            if NewCount > FFooterLen then
                NewCount := FFooterLen;
            if NewCount > 0 then begin
                Move(FFooter[1], TByteArray(Buffer)[Result], NewCount);
                Inc(Result, NewCount);
            end;
        end;
    end;
    if Result < 0 then
        Result := 0;
    FCurrentPos := Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMultiPartFileReader.Write(const Buffer; Count: Integer): Longint;
begin
    Result := 0;  // Read only!
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMultiPartFileReader.Seek(Offset: Integer; Origin: Word): Longint;
{$IFNDEF COMPILER6_UP}
var
    NewPos   : Integer;
{$ENDIF}
begin
{$IFDEF COMPILER6_UP}
    Result := Seek(Int64(Offset), TSeekOrigin(Origin));
{$ELSE}
    case Origin of
        sofromBeginning : if Offset < 0 then
                              NewPos := 0
                          else
                              NewPos := Offset;
        sofromCurrent   : NewPos := FCurrentPos + Offset;
        sofromEnd       : NewPos := (FFileSize + FFooterLen + FHeaderLen) + Offset;
        else
            raise EMultiPartFileReaderException.Create('Invalid seek origin');
    end;
    if NewPos < 0 then
        NewPos := 0;
    if NewPos <> FCurrentPos then begin
        if NewPos > FHeaderLen then begin
            if NewPos <= (FHeaderLen + FFileSize) then
                inherited Seek(NewPos - FHeaderLen , sofromBeginning)
            else
                if NewPos >= (FHeaderLen + FFileSize + FFooterLen) then begin
                    inherited Seek(NewPos - FHeaderLen - FFooterLen, sofromBeginning);
                    NewPos := FHeaderLen + FFileSize + FFooterLen;
                end
                else
                    inherited Seek(NewPos - FHeaderLen, sofromBeginning);
            end
        else
            inherited Seek(0, sofromBeginning);
    end;
    FCurrentPos := NewPos;
    Result      := NewPos;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER6_UP}
function TMultiPartFileReader.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
    NewPos   : Int64;
begin
    case Origin of
        soBeginning : if Offset < 0 then
                          NewPos := 0
                      else
                          NewPos := Offset;
        soCurrent   : NewPos := FCurrentPos + Offset;
        soEnd       : NewPos := (FFileSize + FFooterLen + FHeaderLen) + Offset;
        else
            raise EMultiPartFileReaderException.Create('Invalid seek origin');
    end;
    if NewPos < 0 then
        NewPos := 0;
    if NewPos <> FCurrentPos then begin
        if NewPos > FHeaderLen then begin
            if NewPos <= (FHeaderLen + FFileSize) then
                inherited Seek(NewPos - FHeaderLen , soBeginning)
            else
                if NewPos >= (FHeaderLen + FFileSize + FFooterLen) then begin
                    inherited Seek(NewPos - FHeaderLen - FFooterLen, soBeginning);
                    NewPos := FHeaderLen + FFileSize + FFooterLen;
                end
                else
                    inherited Seek(NewPos - FHeaderLen, soBeginning);
            end
        else
            inherited Seek(0, soBeginning);
    end;
    FCurrentPos := NewPos;
    Result      := NewPos;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TTextStream }

type
    { 32 bytes max }
    TTextStreamUserData = record
        Stream    : TStream;
        StreamPos : BigInt;
    end;
    PTextStreamUserData = ^TTextStreamUserData;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TextStreamDummy(var TR: TTextRec ): Integer;
begin
    Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetTextStream(var TR: TTextRec): TStream;
begin
    Result := PTextStreamUserData(@TR.Userdata)^.Stream;
    if Result = nil then
        raise Exception.Create('Stream not assigned');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TextStreamIn(var TR: TTextRec): Integer;
begin
    Result := 0;
    TR.BufEnd := GetTextStream(TR).Read(TR.BufPtr^, TR.BufSize);
    Inc(PTextStreamUserData(@TR.Userdata)^.StreamPos, TR.BufEnd);
    TR.BufPos := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TextStreamOut(var TR: TTextRec): Integer;
var
    Len : Cardinal;
begin
    Result := 0;
    if TR.BufPos > 0 then
    begin
        Len := GetTextStream(TR).Write(TR.BufPtr^, TR.BufPos);
        if Len <> TR.BufPos then
            Result := GetLastError;
        Inc(PTextStreamUserData(@TR.Userdata)^.StreamPos, Len);
        TR.BufPos := TR.BufPos - Len;
        if Result <> 0 then
            raise Exception.Create(SWriteError + ' ' + SysErrorMessage(Result));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TextStreamFlushOut(var TR: TTextRec): Integer;
begin
    Result := TextStreamOut(TR);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TTextStream.Create(
    AStream    : TStream;
    BufferSize : Integer
{$IFDEF COMPILER6_UP} ;
    Style      : TTextLineBreakStyle
{$ENDIF}
    );
var
    UData : TTextStreamUserData;
begin
    inherited Create;
    FStream := AStream;
    if not Assigned(FStream) then
        raise Exception.Create('Stream not assigned');
    // FStream.Position  := 0;

    FillChar(TTextRec(TF), Sizeof(TFileRec), #0);
    with TTextRec(TF) do
    begin
        { Init TextRec and put it into input mode }
        Mode      := fmInput;
        InOutFunc := @TextStreamIn;
        FlushFunc := @TextStreamDummy;
        //BufPos    := 0;
        //BufEnd    := 0;
{$IFDEF COMPILER6_UP}
        Flags     := (Flags and not tfCRLF) or (tfCRLF * Byte(Style));
{$ENDIF}        
        OpenFunc  := @TextStreamDummy;
        CloseFunc := @TextStreamDummy;
        //Name[0]  := #0;

        {Set and allocate buffer }
        BufSize := BufferSize;
        if BufSize < SizeOf(Buffer) then
            BufSize := SizeOf(Buffer)
        else if BufSize mod SizeOf(Buffer) <> 0 then
            BufSize := ((BufSize div SizeOf(Buffer)) * SizeOf(Buffer));
        if BufSize > SizeOf(Buffer) then
        begin
            SetLength(FBuf, BufSize);
            BufPtr := PAnsiChar(FBuf);
        end
        else begin
            BufSize := SizeOf(Buffer);
            BufPtr  := @Buffer;
        end;

        { Userdata }
        UData.Stream    := FStream;
        UData.StreamPos := 0;
        Move(UData, Userdata, Sizeof(UData));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TTextStream.Destroy;
begin
    try
        System.CloseFile(TF);
    finally
        SetLength(FBuf, 0);
        inherited Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTextStream.SetRealPos;
begin
    PTextStreamUserData(@TTextRec(TF).UserData)^.StreamPos :=
    FStream.Seek(- BigInt(TTextRec(TF).BufSize - TTextRec(TF).BufPos),
                 {$IFDEF COMPILER6_UP} soCurrent {$ELSE} sofromCurrent {$ENDIF});
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTextStream.SetMode(NewMode: TTextStreamMode);
begin
    if (FMode = NewMode) then
        Exit;
    case NewMode of
        tsmReadLn :
            with TTextRec(TF) do
            begin
                if FMode = tsmWriteLn then
                    System.Flush(TF);
                Mode      := fmInput;
                InOutFunc := @TextStreamIn;
                FlushFunc := @TextStreamDummy;
                BufPos    := 0;
                BufEnd    := 0;
            end;
        tsmWriteLn :
            with TTextRec(TF) do
            begin
                if FMode = tsmReadLn then
                    SetRealPos;
                Mode      := fmOutput;
                InOutFunc := @TextStreamOut;
                FlushFunc := @TextStreamFlushOut;
                BufPos    := 0;
                BufEnd    := 0;
            end;
        tsmWrite, tsmRead :
            if FMode = tsmReadLn then
                SetRealPos
            else if FMode = tsmWriteLn then
                System.Flush(TF);
    end;
    FMode := NewMode
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTextStream.ReadLn : Boolean;
begin
    SetMode(tsmReadLn);
    if not System.Eof(TF) then
    begin
        System.ReadLn(TF);
        Result := TRUE;
    end
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTextStream.ReadLn(var S: AnsiString): Boolean;
begin
    SetMode(tsmReadLn);
    if not System.Eof(TF) then
    begin
        System.ReadLn(TF, S);
        Result := TRUE;
    end
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER6_UP}
function TTextStream.ReadLn(var WS: WideString): Boolean;
begin
    SetMode(tsmReadLn);
    if not System.Eof(TF) then
    begin
        System.ReadLn(TF, WS);
        Result := TRUE;
    end
    else
        Result := FALSE;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTextStream.WriteLn(const S: AnsiString);
begin
    SetMode(tsmWriteLn);
    System.Writeln(TF, S);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER6_UP}
procedure TTextStream.WriteLn(const WS: WideString);
begin
    SetMode(tsmWriteLn);
    System.Writeln(TF, WS);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTextStream.Read(var Buffer; Count: Integer): Longint;
begin
    SetMode(tsmRead);
    Result := FStream.Read(Buffer, Count)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTextStream.Write(const Buffer; Count: Integer): Longint;
begin
    SetMode(tsmWrite);
    Result := FStream.Write(Buffer, Count);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTextStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
{$IFDEF COMPILER6_UP}
    Result := Seek(Int64(Offset), TSeekOrigin(Origin));
{$ELSE}
    SetMode(tsmRead);
    Result := FStream.Seek(Offset, Origin);
    PTextStreamUserData(@TTextRec(TF).UserData)^.StreamPos := Result;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER6_UP}
function TTextStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
    SetMode(tsmRead);
    Result := FStream.Seek(Offset, Origin);
    PTextStreamUserData(@TTextRec(TF).UserData)^.StreamPos := Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTextStream.SetSize(const NewSize: Int64);
begin
    FStream.Size := NewSize;
end;

{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF COMPILER6_UP}
procedure TTextStream.SetSize(NewSize: Longint);
begin
    FStream.Size := NewSize;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTextStream.Flush;
begin
    if FMode = tsmWriteLn then
        System.Flush(TF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
