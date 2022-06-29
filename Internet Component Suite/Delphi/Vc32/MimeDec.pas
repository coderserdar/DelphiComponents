{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Object:       TMimeDecode is a component whose job is to decode MIME encoded
              EMail messages (file attach). You can use it for example to
              decode messages received with a POP3 component.
              MIME is described in RFC-1521. headers are described if RFC-822.
Creation:     March 08, 1998
Version:      1.19
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
              http://www.rtfm.be/fpiette   francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1996-2001 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be><francois.piette@overbyte.be>

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

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

QUICK REFERENCE:
----------------
TMimeDecode take a file or a stream as input and produce several event when
the message is parsed. each event can be used to display or to save to a file
the message parts.

Two methods can be called to decode either a file or a stream:
procedure DecodeFile(FileName : String);
procedure DecodeStream(aStream : TStream);

During the decode process, the component trigger several events. You have to
use those events to save data to a file or to display somehow on the
user interface.

Events are organized by groups of three for message header, part header and
part data:
Message header events: OnHeaderBegin      OnHeaderLine      OnHeaderEnd
Part header events:    OnPartHeaderBegin  OnPartHeaderLine  OnPartHeaderEnd
Part data events:      OnPartDataBegin    OnPartDataLine    OnPartDataEnd

The 'Begin' event is triggered once just before the first item will occur.
The 'Line' event is triggered for each item of the given type.
The 'End' event is triggered once after the last item.

For a multi-part message, we have this sequence:
a) The message header
OnHeaderBegin, then many OnHeaderLine, one for each line in the header. Lines
can be continuated in the message. The event here is triggered with continuated
lines concatenated (so it can be quite large !). After the last header line
has been processed, the OnHeaderEnd is triggered once.
b) The non-significant message part which can be empty. This is part 0. We
get OnPartBegin once, then OnPartLine for each line and finally OnPartEnd once.
c) The first significant part header with his three events, just like the
message header: OnPartHeaderBegin, OnPartHeaderLine and OnPartHeaderEnd.
d) The first significant part data with his three events: OnPartBegin once,
OnPartLine for each line and OnPartEnd once at the end of the part.
It's possible to have an empty part. This gives the OnPartBegin and OnPartEnd
events and NO OnPartLine event.
e) We can have many other parts. The sequence is always the same. We restart
at point (b) here above for each part (header, then data). Note that there is
often en empty part at the end of a message.

TMimeDecode decode encoded parts using 'base64' and 'quoted-printable' methods.
For those parts, the OnPartLine event will gives DECODED data. Other methods
are passed not decoded. You can use the property ContentTransferEncoding to
know which encoding method is used and add your own decoding mechanism.

For each OnHeaderLine, OnPartHeaderLine and OnPartLine, you can find the
actual data at the address pointed by the property CurrentData (a PChar).
The reason for a PChar is that the data can be quite large. The data pointed
is a null terminated string. You can get the length using StrLen, or convert
to a string with StrPas. It is more efficient to process the data using a
pointer. Using strings tends to copy the data several times.
The OnPartLine event passes a PChar and a length to the handler. This actully
point to the internal buffer and overwrite the original data (base64 and
quote-printable method produce decoded data smaller tha encoded one).

>From the message header, the component extract the following values:
>From         The message author. Not necessary the real author...
             Looks like "Francois Piette" <francois.piette@pophost.eunet.be>
Dest         The message destination (To field, but To is a reserved word)
             Looks like "Francois Piette" <francois.piette@pophost.eunet.be>
Subject      The message subject. Free text.
Date         The message date.
             Look like: Mon, 16 Feb 1998 12:45:11 -0800
ContentType  'multipart/mixed' or empty.
For details about those header fields and others, read RFC-822

For each part, we have the following properties updated (the header is parsed
on the fly):
PartNumber     Starting from 0 for the non-significant part
PartLine                  Starting 1 for the first line of each part or header
PartContentType           Such as 'text/plain' or 'application/x-zip-compressed'
PartCharset               This is a complement for the PartContentType.
ApplicationType           When PartContentType is 'application/something', we
                          get the 'something' extracted
PartName                  This is the value for 'name=something' in the
                          Content-Type header line.
PartEncoding              Encoding method (Content-Transfer-Encoding).
                          Can be used to decode unsupported
                          methods (supported methods are 'base64' and
                          'quoted-printable'. '7bit' and '8bit' does'nt
                          generally require processing.
PartDisposition           Can be 'inline' or 'attachement' and is generally
                          followed by a 'filename=something'
PartFileName              The specified filename in Content-Disposition header
                          line. Be aware that the file name is not necessary
                          suitable for windows ! Use it with caution...
For details about those header fields and others, read RFC-1521.

To write part data to files, you can either implement your own writing in
the OnPartLine event handler, or use the DestStream property. If assigned,
this property will be used to write the data. If not assigned, it will be
ignore.

To select a file name for each part, you can use the PartFileName property or
the 'PartName' property or a comnination of both. But be aware that those value
can be either missing or even invalid as a filename because the message was
generated with another opertaing system which has different filename
conventions.

Updates:
Apr 13, 1998  V1.01 Corrected a bug in ProcessLineBase64 which decoded one
              byte too much. Thanks to Rune Fredriksen <runefr@mail.link.no>.
Apr 15, 1998  V1.02 Corrected bug in ProcessHeaderLine which retreived only
              the first word for each item.
              Added the ReturnPath property.
Apr 24, 1998  V1.03 Removed the modification made in version 1.01 !
Apr 26, 1998  V1.04 Corrected a bug in ReallocMem with Delphi 1
Aug 27, 1998  V1.05 Corrected a bug in decoding which incorrectly merge
              the first message line with the header when the line begon
              by a space. Thanks to Mitch Cant <mitchcant@hotmail.com> for
              finding the bug and correction.
Sep 13, 1998  V1.06 Correctly handled unterminated messages.
              Correctly handled parts without header.
Dec 26, 1998  V1.07 Added features coded by Eric Fortier <efortier@videotron.ca>
              (Embedded mime parts, UUDecode).
Dec 30, 1998  V1.08 Check for header end when a header line begin with a
              space or tab character. (Normally a header end with a blank
              line, we also accept invalid header line).
Feb 01, 1999  V1.09 Corrected a bug ProcessLineUUDecode where 'end' was not
              checked. Thanks to Eric Fortier.
Feb 16, 1999  V1.10 Added UUEncoded embedded parts. Thanks to Eric Fortier.
              Corrected a line termination problem in ProcessLineBase64.
Jul 21, 1999  V1.11 Added support for encoded message without multipart.
              Added Encoding property with the encoding value.
              Thanks to Marcelo S Massuda <massuda@4web.com.br> for pinting this
              lack of feature.
Aug 20, 1999  V1.12 Added compile time options. Revised for BCB4.
Nov 25, 1999  V1.13 Changed continuation line character for quoted printable
              encoding. By Ken Petersen <KPT@edbgruppen.dk>.
	      Created GetTokenEx function to take care of comments in header
	      lines. This affect ProcessPartHeaderLine and ProcessHeaderLine.
	      Thanks to Boris Daljevic <biber@eunet.yu> for his code.
	      Added CharSet property related to main part charset (see also
	      existing PartCharset property). Thanks to Boris Daljevic
              <biber@eunet.yu> for his code.
Jun 20, 2000  V1.14 Poessler Thomas <Thomas.Poessler@uta.at> corrected a bug in
              ProcessLineQuotedPrintable.
Jul 02, 2000  V1.15 Added OnMessageEnd event
Jul 15, 2000  V1.16 Added code from Wolfgang Baron <Wolfgang.Baron@gwtel.de>
              to support content-description header line.
              Changed GetToken and GetTokenEx so that a space before a delimiter
              will not break token parsing. Outlook generate such invalid
              formatting thanks for Arno van Rossum <a.van.rossum@mmp-obec.nl>
              for finding this bug.
              Revised code to handle inline UUEncoded messages.
Jul 21, 2000  V1.17 Use GetValue instead of GetToken to solve problem with
              boundaries of embbeded parts.
              With help of Jan Bartak <bart@seznam.cz>.
              As suggested by Sebastien Gariepy <beeper@globetrotter.net>, I
              added PartContentID.
Oct 29, 2000  V1.18 Checked for missing content-type before calling
              UUProcessLine. Without the check, a part with a line beginning
              with 'begin 666' will be wrongly decoded.
Feb 17, 2001  V1.19 Top of the messages with a field multipart was incorrectly
              processed.Property FCharset was not initialized in procedure
              MessageBegin. Thanks to Bayanov <bayanov@alt.ru>

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit MimeDec;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$IFNDEF VER80} { Not for Delphi 1                    }
    {$H+}       { Use long strings                    }
    {$J+}       { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF VER110} { C++ Builder V3.0                    }
    {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER125} { C++ Builder V4.0                    }
    {$ObjExportAll On}
{$ENDIF}

interface

uses
    WinTypes, WinProcs, SysUtils, Classes;

const
    MimeDecodeVersion  = 119;
    CopyRight : String = ' TMimeDecode (c) 1998-2001 F. Piette V1.19 ';

type
    TMimeDecodePartLine = procedure (Sender  : TObject;
                                     Data    : PChar;
                                     DataLen : Integer) of object;

    TInlineDecodeBegin = procedure (Sender: TObject; Filename: string) of object;
    TInlineDecodeLine = procedure (Sender: TObject; Line: pchar) of object;
    TInlineDecodeEnd = procedure (Sender: TObject; Filename: string) of object;

    TMimeDecode = class(TComponent)
    private
        FFrom                     : String;
        FDest                     : String;
        FSubject                  : String;
        FDate                     : String;
        FReturnPath               : String;
        FEncoding                 : String;
        FCharSet                  : String;
        FContentType              : String;
        FMimeVersion              : String;
        FPartContentType          : String;
        FPartEncoding             : String;
        FPartNumber               : Integer;
        FPartHeaderBeginSignaled  : Boolean;
        FPartName                 : String;
        FPartDisposition          : String;
        FPartContentID            : String;
        FPartFileName             : String;
        FPartCharset              : String;
        FApplicationType          : String;
        FPartOpened               : Boolean;
        FHeaderFlag               : Boolean;
        FLineNum                  : Integer;
        FBuffer                   : PChar;
        FBufferSize               : Integer;
        FCurrentData              : PChar;
        FBoundary                 : String;
        FUUProcessFlag            : Boolean;
        FNext                     : procedure of object;
        FDestStream               : TStream;
        FOnHeaderBegin            : TNotifyEvent;
        FOnHeaderLine             : TNotifyEvent;
        FOnHeaderEnd              : TNotifyEvent;
        FOnPartHeaderBegin        : TNotifyEvent;
        FOnPartHeaderLine         : TNotifyEvent;
        FOnPartHeaderEnd          : TNotifyEvent;
        FOnPartBegin              : TNotifyEvent;
        FOnPartLine               : TMimeDecodePartLine;
	FOnPartEnd                : TNotifyEvent;
        FOnMessageEnd             : TNotifyEvent;
        cUUFilename               : String;		{ ##ERIC }
        FEmbeddedBoundary         : TStringList;	{ ##ERIC }
        cIsEmbedded               : Boolean;		{ ##ERIC }
        FInlineBegin              : TInlineDecodeBegin;
        FInlineLine               : TInlineDecodeLine;
        FInlineEnd                : TInlineDecodeEnd;
        procedure TriggerHeaderBegin; virtual;
        procedure TriggerHeaderLine; virtual;
        procedure TriggerHeaderEnd; virtual;
        procedure TriggerPartHeaderBegin; virtual;
        procedure TriggerPartHeaderLine; virtual;
        procedure TriggerPartHeaderEnd; virtual;
        procedure TriggerPartBegin; virtual;
        procedure TriggerPartLine(Data : PChar; DataLen : Integer); virtual;
        procedure TriggerPartEnd; virtual;
        procedure TriggerMessageEnd; virtual;

        procedure ProcessLineBase64;
        procedure ProcessLineUUDecode;
        function  UUProcessLine(FCurrentData: pchar): boolean;   
        procedure ProcessLineQuotedPrintable;
        procedure ProcessHeaderLine;
        procedure ProcessPartHeaderLine;
        procedure ProcessPartLine;
        procedure ProcessWaitBoundary;
        procedure ProcessMessageLine;
        procedure PreparePart;
        procedure PrepareNextPart;
        procedure ProcessDecodedLine(Line : PChar; Len : Integer);
        procedure InternalDecodeStream(aStream : TStream);
        procedure MessageBegin;
        procedure MessageEnd;
    public
        procedure DecodeFile(FileName : String);
        procedure DecodeStream(aStream : TStream);
        property From             : String             read  FFrom;
        property Dest             : String             read  FDest;
        property Subject          : String             read  FSubject;
        property Date             : String             read  FDate;
        property ReturnPath       : String             read  FReturnPath;
        property ContentType      : String             read  FContentType;
        property Encoding         : String             read  FEncoding;
        property Charset          : String             read  FCharset;
        property MimeVersion      : String             read  FMimeVersion;
        property PartContentType  : String             read  FPartContentType;
        property PartEncoding     : String             read  FPartEncoding;
        property PartName         : String             read  FPartName;
        property PartDisposition  : String             read  FPartDisposition;
        property PartContentID    : String             read  FPartContentID;
        property PartFileName     : String             read  FPartFileName;
        property PartCharset      : String             read  FPartCharset;
        property ApplicationType  : String             read  FApplicationType;
        property PartNumber       : Integer            read  FPartNumber;
        property CurrentData      : PChar              read  FCurrentData;
        property DestStream       : TStream            read  FDestStream
                                                       write FDestStream;
    published
        property OnHeaderBegin : TNotifyEvent          read  FOnHeaderBegin
                                                       write FOnHeaderBegin;
        property OnHeaderLine : TNotifyEvent           read  FOnHeaderLine
                                                       write FOnHeaderLine;
        property OnHeaderEnd : TNotifyEvent            read  FOnHeaderEnd
                                                       write FOnHeaderEnd;
        property OnPartHeaderBegin : TNotifyEvent      read  FOnPartHeaderBegin
                                                       write FOnPartHeaderBegin;
        property OnPartHeaderLine : TNotifyEvent       read  FOnPartHeaderLine
                                                       write FOnPartHeaderLine;
        property OnPartHeaderEnd : TNotifyEvent        read  FOnPartHeaderEnd
                                                       write FOnPartHeaderEnd;
        property OnPartBegin : TNotifyEvent            read  FOnPartBegin
                                                       write FOnPartBegin;
        property OnPartLine : TMimeDecodePartLine      read  FOnPartLine
                                                       write FOnPartLine;
        property OnPartEnd : TNotifyEvent              read  FOnPartEnd
                                                       write FOnPartEnd;
        property OnMessageEnd : TNotifyEvent           read  FOnMessageEnd
                                                       write FOnMessageEnd;
        property OnInlineDecodeBegin : TInlineDecodeBegin
                                                       read  FInlineBegin
													   write FInlineBegin;
        property OnInlineDecodeLine  : TInlineDecodeLine
                                                       read  FInlineLine
                                                       write FInlineLine;
        property OnInlineDecodeEnd   : TInlineDecodeEnd
                                                       read  FInlineEnd
                                                       write FInlineEnd; 
    end;

procedure Register;

implementation

type
  TLookup = array [0..127] of Byte;

const
Base64In: TLookup = (
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255,  62, 255, 255, 255,  63,  52,  53,  54,  55,
     56,  57,  58,  59,  60,  61, 255, 255, 255,  64, 255, 255, 255,
      0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,
     13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,
    255, 255, 255, 255, 255, 255,  26,  27,  28,  29,  30,  31,  32,
     33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,
     46,  47,  48,  49,  50,  51, 255, 255, 255, 255, 255
);


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [TMimeDecode]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
function TrimRight(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] in [' ', #9]) do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimLeft(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function HexConv(Ch : Char) : Integer;
begin
    if Ch in ['0'..'9'] then
        Result := Ord(Ch) - Ord('0')
    else
    	Result := (Ord(Ch) and 15) + 9;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerHeaderBegin;
begin
    if Assigned(FOnHeaderBegin) then
        FOnHeaderBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerHeaderLine;
begin
    if Assigned(FOnHeaderLine) then
        FOnHeaderLine(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerHeaderEnd;
begin
    if Assigned(FOnHeaderEnd) then
        FOnHeaderEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartHeaderBegin;
begin
    if Assigned(FOnPartHeaderBegin) then
        FOnPartHeaderBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartHeaderLine;
begin
    if Assigned(FOnPartHeaderLine) then
    	FOnPartHeaderLine(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartHeaderEnd;
begin
    if Assigned(FOnPartHeaderEnd) then
        FOnPartHeaderEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartBegin;
begin
    if Assigned(FOnPartBegin) then
    	FOnPartBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartLine(Data : PChar; DataLen : Integer);
begin
    if Assigned(FOnPartLine) then
        FOnPartLine(Self, Data, DataLen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartEnd;
begin
    if Assigned(FOnPartEnd) then
        FOnPartEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerMessageEnd;
begin
    if Assigned(FOnMessageEnd) then
        FOnMessageEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessDecodedLine(Line : PChar; Len : Integer);
begin
    if Len > 0 then begin
    	if (FPartContentType = '')  { Not sure it is always OK !              }
                                    { As such we can't have a MIME part which }
                                    { is uu-encoded.                          }
           and uuprocessline(line) then
            Exit;
    end;
    TriggerPartLine(Line, Len);

    { Write decoded characters to the destination stream }
    if Assigned(FDestStream) and (Len > 0) then
    	FDestStream.WriteBuffer(Line^, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This works if charset="iso-8859-1" !                                        }
procedure TMimeDecode.ProcessLineQuotedPrintable;
var
    SourceIndex         : Integer;
    DecodedIndex        : Integer;
    Ch                  : Char;
    Code                : Integer;
    OldCh		: Char; {tap}
begin
    SourceIndex  := 0;
    DecodedIndex := 0;
    OldCh := #0;{tap}
    if (FCurrentData <> nil) and (FCurrentData^ <> #0) then begin
        while TRUE do begin
            Ch := FCurrentData[SourceIndex];
            if Ch = #0 then begin
                { Nov 25, 1999: Ken Petersen told me to insert CRLF }
                FCurrentData[DecodedIndex] := #13;
                Inc(DecodedIndex);
                FCurrentData[DecodedIndex] := #10;
                Inc(DecodedIndex);
                break;
            end;
            if Ch <> '=' then begin
                FCurrentData[DecodedIndex] := Ch;
                Inc(SourceIndex);
                Inc(DecodedIndex);
            end
            else begin
                Inc(SourceIndex);
                Ch := FCurrentData[SourceIndex];
                if Ch <> #0 then begin
                    Code := HexConv(Ch);
                    Inc(SourceIndex);
                    Ch := FCurrentData[SourceIndex];
                    if Ch <> #0 then begin
                        Code := (Code shl 4) + HexConv(Ch);
                        Inc(SourceIndex);
                    end;
                    FCurrentData[DecodedIndex] := Chr(Code);
                    Inc(DecodedIndex);
                end
                else begin {tap: "=\0" means line continuation}
                    break; {tap}
                end;
            end;
        end;
        {tap: if there is no quoted-string-mapping then the first char of the next msgline will be overwritten}
        OldCh := FCurrentData[DecodedIndex]; {tap}
        FCurrentData[DecodedIndex] := #0;
    end;

    ProcessDecodedLine(FCurrentData, DecodedIndex);
    if (OldCh <> #0) then {tap}
        FCurrentData[DecodedIndex] := OldCh; {tap}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessLineBase64;
var
    ByteCount           : Integer;
    SourceIndex         : Integer;
    DataOut             : array[0..2] of Byte;
    DataIn0             : Byte;
    DataIn1             : Byte;
    DataIn2             : Byte;
    DataIn3             : Byte;
    DecodedIndex        : Integer;
begin
    DecodedIndex := 0;

    { Skip white spaces }
    SourceIndex := 0;
    while (FCurrentData[SourceIndex] <> #0) and
          (FCurrentData[SourceIndex] = ' ') do
        Inc(SourceIndex);

    { Decode until end of line. Replace coded chars by decoded ones }
    while (FCurrentData[SourceIndex] <> #0) and
          (FCurrentData[SourceIndex] <> ' ') do begin
        DataIn0 := Base64In[Byte(FCurrentData[SourceIndex + 0])];
        DataIn1 := Base64In[Byte(FCurrentData[SourceIndex + 1])];
        DataIn2 := Base64In[Byte(FCurrentData[SourceIndex + 2])];
        DataIn3 := Base64In[Byte(FCurrentData[SourceIndex + 3])];

        DataOut[0] := (DataIn0 and $3F) shl 2 + (DataIn1 and $30) shr 4;
        if DataIn2 <> $40 then begin
            DataOut[1] := (DataIn1 and $0F) shl 4 + (DataIn2 and $3C) shr 2;
            if DataIn3 <> $40 then begin
                DataOut[2] := (DataIn2 and $03) shl 6 + (DataIn3 and $3F);
                ByteCount := 3;
            end
            else
                ByteCount := 2;
        end
        else
            ByteCount := 1;

		{ Replace coded characters (4) by decoded characters (up to 3) }
        Move(DataOut, FCurrentData[DecodedIndex], ByteCount);
        DecodedIndex := DecodedIndex + ByteCount;

        SourceIndex := SourceIndex + 4;
    end;

    { Nul terminate decoded line }
    FCurrentData[DecodedIndex] := #0; { 16/02/99 }
    ProcessDecodedLine(FCurrentData, DecodedIndex);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UUDec(Sym : Char): Word;
begin
    Result := (Ord(Sym) - Ord(' ')) and $3F;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure UUOutDec(buf: PChar; n: Integer; var out1 : String);
var
    c1, c2, c3: Char;
begin
    c1 := Chr((word(UUDec(buf[0])) SHL 2) or (word(UUDec(buf[1])) SHR 4));
    c2 := Chr((word(UUDec(buf[1])) SHL 4) or (word(UUDec(buf[2])) SHR 2));
    c3 := Chr((word(UUDec(buf[2])) SHL 6) or (word(UUDec(buf[3]))));
    if n >= 1 then
        out1 := out1 + c1;
    if n >= 2 then
        out1 := out1 + c2;
    if n >= 3 then
        out1 := out1 + c3;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessLineUUDecode; { ##ERIC }
var
    count, Size : Integer;
    s           : String;
    out1        : String;
    bp          : PChar;
    pos1        : Integer;
begin
    if FCurrentData^ = #0 then
        exit;
    s := StrPas(FCurrentData);

    if LowerCase(copy(s, 1, 6)) = 'begin ' then begin
        out1:=lowercase(s);
        if (Pos('--', out1) > 0) and (Pos('cut here', out1) > 0) then
            Exit;
        pos1 := Pos(' ', s);
        s    := Copy(s, pos1 + 1, 255);
        pos1 := Pos(' ', s);
        s    := Copy(s, pos1 + 1, 255);
        cUUFilename := s;
        exit;
    end
    else if LowerCase(Copy(s, 1, 3)) = 'end' then begin
        out1 := LowerCase(s);
        if (Pos('--', out1) > 0) and (Pos('cut here', out1) > 0) then
            Exit;
        cUUFilename := '';
        exit;
    end;

    { if no filename defined yet, exit }
    if cUUFilename = '' then
        exit;

    { decode the line }
    count := UUDec(s[1]);
    Size  := Count;
    if count > 0 then begin
        bp := @s[2];
        repeat
            UUOutDec(bp, count, out1);
            count := count - 3;
            bp    := bp + 4;
        until count <= 0;
    end;

    { we're done. copy and leave }
    Move(Out1[1], FCurrentData[0], Size);
    ProcessDecodedLine(FCurrentData, Size);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMimeDecode.UUProcessLine(FCurrentData: PChar): Boolean;
var
    s           : String;
    out1        : String;
    pos1        : Integer;
    count       : Integer;
    bp          : PChar;
begin
    Result := TRUE;
{   if FCurrentData^ = #0 then begin
        result := false;
        Exit;
    end; }
    s := Trim(StrPas(FCurrentData));
    if S = '' then begin
        Result := FALSE;
        Exit;
    end;

    if (not FUUProcessFlag) and
       (CompareText(Copy(s, 1, 9), 'begin 666') = 0) then begin
        out1 := LowerCase(s);
        if (Pos('--', out1) > 0) and (Pos('cut here', out1) > 0) then
            Exit;
        pos1 := Pos(' ', s);
        s    := Copy(s, pos1 + 1, 255);
        pos1 := Pos(' ', s);
        s    := Copy(s, pos1 + 1, 255);
        cUUFilename    := s;
        FUUProcessFlag := TRUE;
        if Assigned(FInlineBegin) then
            FInlineBegin(Self, cUUFilename);
        Exit;
    end;

    if not FUUProcessFlag then begin
        Result := FALSE;
        Exit;
    end;

    if CompareText(Copy(s, 1, 3), 'end') = 0 then begin
        out1 := LowerCase(s);
        if (Pos('--', out1) > 0) and (Pos('cut here', out1) > 0) then
            Exit;
        FUUProcessFlag := FALSE;
        if Assigned(FInlineEnd) then
            { I also use the filename here in case the client prefer to save   }
            { data to a stream and save to a file when the decoding is complete }
            FInlineEnd(self, cUUfilename);
        cUUFilename := '';
        Exit;
    end;
{   if cUUFilename = '' then begin
        Result := FALSE;
        Exit;
    end; }

    if Assigned(FInlineLine) then begin
        { decode the line }
        count := UUDec(s[1]);
        if count > 0 then begin
            bp := @s[2];
            repeat
                UUOutDec(bp, count, out1);
                count := count - 3;
                bp    := bp + 4;
            until count <= 0;
        end;
        {$IFDEF VER80}
        if Length(Out1) = 0 then
            FInlineLine(Self, nil)
        else
            FInlineLine(Self, @Out1[1]);
        {$ELSE}
        FInlineLine(Self, PChar(Out1));
        {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function stpblk(PValue : PChar) : PChar;
begin
    Result := PValue;
    while Result^ in [' ', #9, #10, #13] do
        Inc(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetValue(Src : PChar; var Dst : String; var Delim : Char) : PChar;
begin
    Result := StpBlk(Src);
    Dst    := '';
    Delim  := Result^;
    if Delim = '"' then begin
        Inc(Result);
        while TRUE do begin
            Delim  := Result^;
            if Delim = #0 then
                break;
            if Delim = '"' then begin
                Inc(Result);
                Delim := Result^;
                break;
            end;
            Dst := Dst + Delim;
            Inc(Result);
        end;
    end
    else begin
        while TRUE do begin
            Delim  := Result^;
            if Delim in [':', ' ', ';', '=', #9, #0] then
                break;
            Dst := Dst + LowerCase(Result^);
            Inc(Result);
        end;
    end;
    if Delim in [' ', #9] then begin
        Result := stpblk(Result);
        if Result^ in [':', ';', '=', #9] then
            Inc(Result);
    end
    else if Delim <> #0 then
	Inc(Result);
    Result := stpblk(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetToken(Src : PChar; var Dst : String; var Delim : Char) : PChar;
begin
    Result := StpBlk(Src);
    Dst    := '';
    while TRUE do begin
        Delim := Result^;
        if Delim in [':', ' ', ';', '=', #9, #0] then
            break;
        Dst := Dst + LowerCase(Result^);
        Inc(Result);
    end;
    if Delim in [' ', #9] then begin
        Result := stpblk(Result);
        if Result^ in [':', ';', '=', #9] then
            Inc(Result);
    end
    else if Delim <> #0 then
	Inc(Result);
    Result := stpblk(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Same as GetToken, but take be aware of comments                           }
function GetTokenEx(Src : PChar; var Dst : String; var Delim : Char) : PChar;
var
    Comment: Integer;
begin
    Result  := StpBlk(Src);
    Dst     := '';
    Comment := 0;
    while TRUE do begin
	Delim := Result^;
        if Delim = #0 then
            break;
        if Delim = '(' then begin
            Inc(comment); { Comments can be nested }
            Inc(Result);
            Continue;
        end
        else if Delim = ')' then begin
            Dec(Comment);
            Inc(Result);
            Continue;
        end
        else if (Comment = 0) and (Delim in [':', ' ', ';', '=', #9]) then
            break;
        Dst := Dst + LowerCase(Result^);
        Inc(Result);
    end;
    if Delim in [' ', #9] then begin
        Result := stpblk(Result);
        if Result^ in [':', ';', '=', #9] then
            Inc(Result);
    end
    else if Delim <> #0 then
	Inc(Result);
    Result := StpBlk(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetQuoted(Src : PChar; var Dst : String) : PChar;
var
    Quote : Char;
begin
    Result := StpBlk(Src);
    Dst    := '';
    Quote  := Result^;
    if Quote <> #34 then begin	{ ##ERIC }
    	Dst := StrPas(Src);	{ ##ERIC }
    	Exit;										{ ##ERIC }
    end;											{ ##ERIC }

    Inc(Result);
    while (Result^ <> #0) and (Result^ <> Quote) do begin
        Dst := Dst + Result^;
        Inc(Result);
    end;
    Result := stpblk(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.PreparePart;
begin
    FPartOpened := FALSE;
    TriggerPartEnd;
    PrepareNextPart;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessWaitBoundary;	{ ##ERIC }
var
    t : Integer;
    s : String;
begin
    s := LowerCase(StrPas(FCurrentData));
    if s = FBoundary then begin
        PreparePart;
        Exit;
    end
    else begin
        { are we in the embedded boundaries ? }
        for t := 0 to FEmbeddedBoundary.Count - 1 do begin
            if FEmbeddedBoundary[t] = s then begin
                cIsEmbedded := true;
                PreparePart;
                Exit;
            end;
        end;

       { if not in primary boundary or embedded boundaries, then process it.}
       ProcessDecodedLine(FCurrentData, StrLen(FCurrentData));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessMessageLine;
begin
    Inc(FLineNum);
    if FLineNum = 1 then
        TriggerPartBegin;
    if FEncoding = 'base64' then
        ProcessLineBase64
    else if FEncoding = 'quoted-printable' then
        ProcessLineQuotedPrintable
    else if FEncoding = 'x-uuencode' then
        ProcessLineUUDecode										{ ##ERIC }
    else begin {tap}
        ProcessDecodedLine(FCurrentData, StrLen(FCurrentData));
        ProcessDecodedLine(#13, 1); {tap: add \r\n to other encodings}
        ProcessDecodedLine(#10, 1); {tap}
    end; {tap}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.PrepareNextPart;
begin
    FPartEncoding            := '';
    FPartContentType         := '';
    FPartDisposition         := '';
    FPartContentID           := '';
    FPartName                := '';
    FPartFileName            := '';
    FHeaderFlag              := TRUE;  { We begin by a header }
    FLineNum                 := 0;
    FUUProcessFlag           := FALSE;
    FPartHeaderBeginSignaled := FALSE;
    FNext                    := ProcessPartHeaderLine;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessPartLine;	{ ##ERIC }
var
    Len : Integer;
    t   : Integer;
	s   : String;		{ ##ERIC }
begin
    { Check if end of part (boundary line found) }
    if (FCurrentData <> nil) and (FCurrentData^ <> #0) then begin
    	s := LowerCase(StrPas(FCurrentData));
        if (s = FBoundary) or
           (s = (FBoundary + '--')) then begin
            PreparePart;
            exit;
        end
        else begin
            for t := 0 to FEmbeddedBoundary.Count - 1 do begin
                if (s = FEmbeddedBoundary[t]) or
                   (s = (FEmbeddedBoundary[t] + '--')) then begin
                    { we now have to wait for the next part }
          	    PreparePart;
                    exit;
                end
            end;
        end;
    end;

    if not FPartOpened then begin
        FPartOpened := TRUE;
        TriggerPartBegin;
    end;

    if FPartEncoding = 'base64' then
        ProcessLineBase64
    else if FPartEncoding = 'quoted-printable' then
        ProcessLineQuotedPrintable
    else if FPartEncoding = 'x-uuencode' then	{ ##ERIC }
        ProcessLineUUDecode										{ ##ERIC }
    else begin
        if FCurrentData = nil then
            Len := 0
        else
            Len := StrLen(FCurrentData);
        ProcessDecodedLine(FCurrentData, Len);
        ProcessDecodedLine(#13, 1); {tap: add \r\n to other encodings}
        ProcessDecodedLine(#10, 1); {tap}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessPartHeaderLine;
var
    p       : PChar;
    Delim   : Char;
    Token   : String;
    KeyWord : String;
    Value   : String;
{   Value1  : String; }
begin
    if (FCurrentData = nil) or (FCurrentData^ = #0) then begin
        { End of part header }
        if not FPartHeaderBeginSignaled then begin
            Inc(FPartNumber);
            TriggerPartHeaderBegin;
        end;
        TriggerPartHeaderEnd;
        FHeaderFlag    := FALSE;  { Remember we are no more in a header }
        FLineNum       := 0;
        FUUProcessFlag := FALSE;
        FNext          := ProcessPartLine;
        Exit;
    end;

    Inc(FLineNum);
    if FLineNum = 1 then begin
        Inc(FPartNumber);
        FPartHeaderBeginSignaled := TRUE;
        TriggerPartHeaderBegin;
{       FEmbeddedBoundary.clear; }
    end;

    { A header line can't begin with a space nor tab char. If we got that }
    { then we consider the header as begin finished and process line      }
    if FHeaderFlag and (FCurrentData[0] in [' ', #9]) then begin
        TriggerPartHeaderEnd;
        FHeaderFlag    := FALSE;
        FLineNum       := 0;
        FUUProcessFlag := FALSE;
        FNext          := ProcessPartLine;
        ProcessPartLine;
        Exit;
    end;

    p := GetToken(FCurrentData, KeyWord, Delim);
    if KeyWord = 'content-type' then begin
        p := GetTokenEx(p, FPartContentType, Delim);
        while Delim = ';' do begin
            p := GetToken(p, Token, Delim);
            if Delim = '=' then begin
                p := GetValue(p, Value, Delim);
                if Token = 'name' then
                    FPartName := Value
                else if Token = 'charset' then
                    FPartCharset := Value
                else if Token = 'boundary' then begin
                    { we have an embedded boundary }
                    FEmbeddedBoundary.Add('--' + LowerCase(Value));
{                   Value := Value + #0;  }{ NUL terminate string for Delphi 1 }
{                   GetQuoted(@Value[1], Value1);}		     { ##ERIC }
{                   FEmbeddedBoundary.Add('--' + LowerCase(Value1));} { ##ERIC }
                end;																				{ ##ERIC }
            end;
        end;
    end
    else if KeyWord = 'content-transfer-encoding' then begin
        GetTokenEx(p, FPartEncoding, Delim);
    end
    else if KeyWord = 'content-id' then begin
        FPartContentID := StrPas(p);
        if (Length(FPartContentID) >= 2) and
           (FPartContentID[1] = '<') and
           (FPartContentID[Length(FPartContentID)] = '>') then
               FPartContentID := Copy(FPartContentID, 2, Length(FPartContentID) - 2);
    end
    else if KeyWord = 'content-disposition' then begin
        p := GetTokenEx(p, FPartDisposition, Delim);
        while Delim = ';' do begin
            p := GetToken(p, Token, Delim);
            if Delim = '=' then begin
                p := GetQuoted(p, Value);
                if Token = 'filename' then
                    FPartFileName := Value;
            end;
        end;
    end
    else if (KeyWord = 'content-description') and (FPartFileName = '') then begin
        Delim:= ';';
        while Delim = ';' do begin
            p := GetToken(p, Token, Delim);
            if Delim = '=' then begin
                p := GetQuoted(p, Value);
                if Token = 'filename' then
                    FPartFileName := Value;
            end;
        end;
    end;

    TriggerPartHeaderLine;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessHeaderLine;
var
    p     : PChar;
    pVal  : PChar;
    Delim : Char;
    Token : String;
    Value : String;
begin
    if (FCurrentData = nil) or (FCurrentData^ = #0) then begin
        FHeaderFlag := FALSE;  { We are no more in a header }
        TriggerHeaderEnd;
        FLineNum       := 0;
        FUUProcessFlag := FALSE;
        if FBoundary = '' then begin
            FNext := ProcessMessageLine;
        end
        else begin
            TriggerPartBegin;
            FNext := ProcessWaitBoundary;
        end;
        Exit;
    end;

    Inc(FLineNum);
    if FLineNum = 1 then
        TriggerHeaderBegin;

    p    := GetToken(FCurrentData, Token, Delim);
    pVal := StpBlk(p);
    if Delim = ':' then begin
        p := GetTokenEx(p, Value, Delim);

        if Token = 'content-type' then begin
            FContentType := Value;
            if Pos('text/', FContentType) = 1 then begin
                p := GetToken(p, Token, Delim);
                if Token = 'charset' then
                    GetQuoted(p, FCharset);
            end
            else if Pos('multipart/', FContentType) = 1 then begin
                while Delim = ';' do begin
                    p := GetToken(p, Token, Delim);
                    if Token = 'boundary' then begin
                        GetQuoted(p, FBoundary);
                        FBoundary := LowerCase('--' + FBoundary);
                    end
                    else
                        p := GetToken(p, Token, Delim);
                end;
            end;
        end
        else if Token = 'mime-version' then
            FMimeVersion := StrPas(pVal)
        else if Token = 'from' then
            FFrom := StrPas(pVal)
        else if Token = 'to' then
            FDest := StrPas(pVal)
        else if Token = 'subject' then
            FSubject := StrPas(pVal)
        else if Token = 'return-path' then begin
            FReturnPath := StrPas(pVal);
            if (Length(FReturnPath) >= 2) and
               (FReturnPath[1] = '<') and
               (FReturnPath[Length(FReturnPath)] = '>') then
                FReturnPath := Copy(FReturnPath, 2, Length(FReturnPath) - 2);
        end
        else if Token = 'date' then
            FDate := StrPas(pVal)
        else if Token = 'content-transfer-encoding' then
            GetToken(pVal, FEncoding, Delim);
    end;

    TriggerHeaderLine;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.MessageEnd;
begin
    if (FBoundary = '') or FPartOpened then
        TriggerPartEnd;
    TriggerMessageEnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.MessageBegin;
begin
    FFrom             := '';
    FDest             := '';
    FSubject          := '';
    FContentType      := '';
    FMimeVersion      := '';
    FPartContentType  := '';
    FPartEncoding     := '';
    FApplicationType  := '';
    FPartName         := '';
    FPartFileName     := '';
    FPartDisposition  := '';
    FPartCharset      := '';
    FApplicationType  := '';
    FCharset          := '';
    FPartNumber       := 0;
    FLineNum          := 0;
    FUUProcessFlag    := FALSE;
    FBoundary         := '';
    FCurrentData      := nil;
    FHeaderFlag       := TRUE;
    FPartOpened       := FALSE;
    FNext             := ProcessHeaderLine;
    FEmbeddedBoundary.Clear; { ##ERIC }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.DecodeFile(FileName : String);
var
    aStream  : TStream;
begin
    aStream  := TFileStream.Create(FileName, fmOpenRead);
    try
        DecodeStream(aStream);
    finally
        aStream.Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.DecodeStream(aStream : TStream);
begin
    FBufferSize := 2048;      { Start with a reasonable FBuffer }
    GetMem(FBuffer, FBufferSize);
    try
        cUUFilename       := '';                    { ##ERIC }
        FEmbeddedBoundary := TStringList.Create;    { ##ERIC }
        try
            InternalDecodeStream(aStream);
        finally
            FEmbeddedBoundary.Free;		    { ##ERIC }
        end;
    finally
        FreeMem(FBuffer, FBufferSize);
        FBuffer     := nil;
        FBufferSize := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This routine use an intelligent buffer management, trying to move data    }
{ the less possible times. The buffer is enlarged as necessary to contains  }
{ the largest line we encounter.                                            }
procedure TMimeDecode.InternalDecodeStream(aStream : TStream);
var
    RdCnt   : LongInt;
    nUsed   : Integer;
    nStart  : Integer;
    nLast   : Integer;
    nSearch : Integer;
    I, J    : Integer;
begin
    nUsed  := 0;
    nStart := 0;
    MessageBegin;
    while TRUE do begin
        nSearch := nStart + nUsed;
        RdCnt   := aStream.Read(FBuffer[nSearch],
                                FBufferSize - nUsed - nStart -
                                2);  { for next char and #0 }
        if RdCnt <= 0 then begin
            break;
        end;

        nUsed  := nUsed + RdCnt;
        nLast  := nStart + nUsed;

        { Nul terminate the FBuffer }
        FBuffer[nLast] := #0;

        { Search for terminating line feed }
        while TRUE do begin
            I := nSearch;
            while (I < nLast) and (FBuffer[I] <> #10) do
                Inc(I);
            if I >= nLast then begin
                { We did'nt find any LF in the FBuffer, need to read more ! }
                if nStart > (3 * (FBufferSize div 4)) then begin
                    { Reuse start of FBuffer because 3/4 buffer is unused   }
                    Move(FBuffer[nStart], FBuffer[0], nUsed + 1);
                    nStart := 0;
                end
                else begin
                    { Makes FBuffer larger }
                    {$IFDEF VER80}
                    FBuffer := ReallocMem(FBuffer, FBufferSize, FBufferSize + 32);
                    {$ELSE}
                    ReallocMem(FBuffer, FBufferSize + 32);
                    {$ENDIF}
                    FBufferSize := FBufferSize + 32;
                end;
                break;
            end;

            { We found a line feed, process FBuffer up to this point }
            { Remove any preceding CR                               }
            if (I > nStart) and (FBuffer[I - 1] = #13) then
                j := I - 1
            else
                J := I;

            { We found a LF, if we are processing a header, we must     }
            { have the next character to see if the line is continuated }
            if FHeaderFlag then begin
                if I >= (nLast - 1) then begin
                    { We don't have the next character in our FBuffer, }
                    { we need to read more data                        }
                    { Read a single byte at the end of the FBuffer     }
                    { We have room because we preserved it previously  }
                    RdCnt := aStream.Read(FBuffer[I + 1], 1);
                    if RdCnt > 0 then begin
                        { We have read the next char }
                        Inc(nLast);
                        Inc(nUsed);
                        FBuffer[I + 2] := #0;
                    end;
                end;

                if I < nLast then begin
                    if (not (FBuffer[nStart] in [#10, #13])) and  { 27/08/98 }
                       (FBuffer[I + 1] in [' ', #9]) then begin
                        { We have a continuation line, replace CR/LF by spaces }
                        FBuffer[I]     := ' ';
                        FBuffer[J]     := ' ';
                        FBuffer[I + 1] := ' ';
                        nSearch   := I;
                        { and search new end of line }
                        continue;
                    end;
                end;
            end;

            FBuffer[J] := #0;
            FCurrentData := FBuffer + nStart;

            FNext;
            FBuffer[J] := #10;         {tap: ERROR ? #13}
            nStart     := I + 1;
            nUsed      := nLast - nStart;
            nSearch    := nStart;
        end;
    end;
    { Process the last line }
    if nUsed > 0 then begin
        FCurrentData := FBuffer + nStart;
        FNext;
    end;

    MessageEnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

