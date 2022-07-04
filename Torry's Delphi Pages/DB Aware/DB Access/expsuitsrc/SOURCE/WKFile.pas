{
    Firesoft - ExportSuite
    Copyright (C) 1997-2006 Federico Firenze

    This library is free software; you can redistribute it and/or modify it
    under the terms of the GNU Library General Public License as published
    by the Free Software Foundation; either version 2 of the License,
    or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    Federico Firenze,
    Buenos Aires, Argentina
    webmaster@delphi.com.ar

}

unit WKFile;

{$I DELPHI.VER}

interface

uses
  Classes, SysUtils;

{
var
  FFile: TFileStream;
  WKFile: TWKFile;
begin
  FFile := TFileStream.Create('C:\PRUEBA.WK1', fmCreate);
  try
    WKFile := TWKFile.Create(FFile);
    try
      WKFile.WriteCell(0, 0, 'Federico Firenze');
      WKFile.WriteCell(1, 1, 1525);
      WKFile.WriteCell(1, 2, 20);
      WKFile.WriteCell(1, 3, 20.34);
      WKFile.WriteCell(1, 4, -12.34);
      WKFile.SetColWidth(1, 200);
    finally
      WKFile.Free ;
    end;
  finally
    FFile.Free ;
  end;
end;
}

const

{ Lotus Records Types }
{------------------------------------------------------------------------------}
{ Constant Name       Value  Record Size     Description  }
{------------------------------------------------------------------------------}
  LRT_BOF            = $0;  {2               Beginning of file }
  LRT_EOF            = $1;  {0               End of file }
  LRT_CALCMODE       = $2;  {1               Calculation mode }
  LRT_CALCORDER      = $3;  {1               Calculation order }
  LRT_SPLIT          = $4;  {1               Split window type }
  LRT_SYNC           = $5;  {1               Split window sync }
  LRT_RANGE          = $6;  {8               Active worksheet range }
  LRT_WINDOW1        = $7;  {31              Window 1 record }
  LRT_COLW1          = $8;  {3               Column width, window 1 }
  LRT_WINTWO         = $9;  {31              Window 2 record }
  LRT_COLW2          = $A;  {3               Column width, window 2 }
  LRT_NAME           = $B;  {24              Named range }
  LRT_BLANK          = $C;  {5               Blank cell }
  LRT_INTEGER        = $D;  {7               Integer number cell }
  LRT_NUMBER         = $E;  {13              Floating point number }
  LRT_LABEL          = $F;  {variable        Label cell }
  LRT_FORMULA        = $10; {variable        Formula cell }
  LRT_TABLE          = $18; {25              Data table range }
  LRT_ORANGE         = $19; {25              Query range }
  LRT_PRANGE         = $1A; {8               Print range }
  LRT_SRANGE         = $1B; {8               Sort range }
  LRT_FRANGE         = $1C; {8               Fill range }
  LRT_KRANGE1        = $1D; {9               Primary sort key range }
  LRT_HRANGE         = $20; {16              Distribution range }
  LRT_KRANGE2        = $23; {9               Secondary sort key range }
  LRT_PROTEC         = $24; {1               Global protection }
  LRT_FOOTER         = $25; {242             Print footer }
  LRT_HEADER         = $26; {242             Print header }
  LRT_SETUP          = $27; {40              Print setup }
  LRT_MARGINS        = $28; {10              Print margins code }
  LRT_LABELFMT       = $29; {1               Label alignment }
  LRT_TITLES         = $2A; {16              Print borders }
  LRT_GRAPH          = $2D; {437             Current graph settings }
  LRT_NGRAPH         = $2E; {453             Named graph settings }
  LRT_CALCCOUNT      = $2F; {1               Iteration count }
  LRT_UNFORMATTED    = $30; {1               Print Un/formatted }
  LRT_CURSORW12      = $31; {1               Cursor location }
  LRT_WINDOW         = $32; {144             Symphony window settings }
  LRT_STRING         = $33; {variable        Value of string formula }
  LRT_PASSWORD       = $37; {4               File lockout (CHKSUM) }
  LRT_LOCKED         = $38; {1               Lock flag }
  LRT_QUERY          = $3C; {127             Symphony query settings }
  LRT_QUERYNAME      = $3D; {16              Query name }
  LRT_PRINT          = $3E; {679             Symphony print record }
  LRT_PRINTNAME      = $3F; {16              Print record name }
  LRT_GRAPH2         = $40; {499             Symphony graph record }
  LRT_GRAPHNAME      = $41; {16              Graph record name }
  LRT_ZOOM           = $42; {9               Orig coordinates expanded window }
  LRT_SYMSPLIT       = $43; {2               Nos. of split windows }
  LRT_NSROWS         = $44; {2               Nos. of screen rows }
  LRT_NSCOLS         = $45; {2               Nos. of screen columns }
  LRT_RULER          = $46; {25              Named ruler range }
  LRT_NNAME          = $47; {25              Named sheet range }
  LRT_ACOMM          = $48; {65              Autoload.comm code }
  LRT_AMACRO         = $49; {8               Autoexecute macro address }
  LRT_PARSE          = $4A; {16              Query parse information }
{ LRT_SPLIT            4                    1             Split window type
  LRT_SYNC             5                    1             Split window sync
  LRT_WINDOW_1         7                    31            Window 1 record
  LRT_WINTWO           9                    31            Window 2 record
  LRT_COLW2            A                    3             Column width, window 2
  LRT_NAME             B                    24            Named range
  LRT_QRANGE           19                   25            Query range
  LRT_PRANGE           1A                   8             Print range
  LRT_SRANGE           1B                   8             Sort range
  LRT_KRANGE1          1D                   9             Primary sort key range
  LRT_KRANGE2          23                   9             Secondary sort key
  LRT_FOOTER           25                   242           Print footer
  LRT_HEADER           26                   242           Print header
  LRT_SETUP            27                   40            Print setup
  LRT_MARGINS          28                   10            Print margins code
  LRT_TITLES           2A                   16            Print borders
  LRT_GRAPH            2D                   437           Current graph settings
  LRT_NGRAPH           2E                   453           Named graph settings
  LRT_BOF               0                   2             Beginning of file
  LRT_EOF               1                   0             End of file
  LRT_CALCMODE          2                   1             Calculation mode
  LRT_CALCORDER         3                   1             Calculation order
  LRT_RANGE             6                   8             Active worksheet range
  LRT_COLW1             8                   3             Column width
  LRT_BLANK             C                   5             Blank cell
  LRT_INTEGER           D                   7             Integer number cell
  LRT_NUMBER            E                   13            Floating point number
  LRT_LABEL             F                   variable      Label cell
  LRT_FORMULA           10                  variable      Formula cell
  LRT_TABLE             18                  25            Data table range
  LRT_FRANGE            1C                  8             Fill range
  LRT_HRANGE            20                  16            Distribution range
  LRT_PROTEC            24                  1             Global protection
  LRT_LABELFMT          29                  1             Label alignment
  LRT_CALCCOUNT         2F                  1             Iteration count
  LRT_UNFORMATTED       30                  1             Formatted/unformatted print
  LRT_CURSORW12         31                  1             Cursor location
  LRT_WINDOW           32                 144             Symphony window settings
  LRT_STRING           33                 variable        Value of string formula
  LRT_PASSWORD         37                 4               File lockout (CHKSUM)
  LRT_LOCKED           38                 1               Lock flag
  LRT_QUERY            3C                 127             Symphony query settings
  LRT_QUERYNAME        3D                 16              Query name
  LRT_PRINT            3E                 679             Symphony print record
  LRT_PRINTNAME        3F                 16              Print record name
  LRT_GRAPH2           40                 499             Symphony graph record
  LRT_GRAPHNAME        41                 16              Graph rocord name
  LRT_ZOOM             42                 9               Orig coordinates expanded window
  LRT_SYMSPLIT         43                 2               Nos. of split windows
  LRT_NSROWS           44                 2               Nos. of screen rows
  LRT_NSCOLS           45                 2               Nos. of screen columns
  LRT_RULER            46                 25              Named ruler range
  LRT_NNAME            47                 25              Named sheet range
  LRT_ACOMM            48                 65              Autoload. comm code
  LRT_AMACRO           49                 8               Autoexecute macro address
  LRT_PARSE            4A                 16              Query parse information
}
{
 Table 8   Cell Format Encoding

 Bit number     Description               Code             Meaning 
 7              protection                1                protected 
                                          0                unprotected 

 4,5,6          format type               0                fixed 
                                          1                scientific 
                                                             notation 
                                          2                currency 
                                          3                percent 
                                          4                comma
                                          5                unused 
                                          6                unused 
                                          7                special 
 0,1,2,3        number of decimal         0-15 
                 places decoded  
                 (if format type = 0-6) 

                special format type       0                +/-
                (if format type = 7)      1                general 
                                          2                day-month-year 
                                          3                day-month 
                                          4                month-year 
                                          5                text 
                (Symphony only)           6                hidden 
                (Symphony only)           7                date;hour-min-sec 
                (Symphony only)           8                date;hour-min 
                (Symphony only)           9                date;intnt'l1 
                (Symphony only)           10               date;intnt'l1 
                (Symphony only)           11               time;intnt'l1 
                (Symphony only)           12               time;intnt'l2 
                                          13-14            unused 
                                          15               default 
}

Type
  TwkLabelString = String[240];
  TwkCellType = (wtBlank, wtInteger, wtDouble, wtLabel); {ctBoolean}

  TWKRecordHeader = record
    wType: Word;   { Record type code }
    wLength: Word; { Record body length (bytes) }
  end;
  PWKRecordHeader = ^TWKRecordHeader;

  TWKBlank = packed record
    rhType: TWKRecordHeader;
    bFmt: Byte;
    wColumn: Word;
    wRow: Word;
  end;
  PWKBlank = ^TWKBlank;

  TWKInteger = packed record
    rhType: TWKRecordHeader;
    bFmt: Byte;
    wColumn: Word;
    wRow: Word;
    wValue: Word;
  end;
  PWKInteger = ^TWKInteger;

  TWKFloat = packed record
    rhType: TWKRecordHeader;
    bFmt: Byte;
    wColumn: Word;
    wRow: Word;
    wValue: Double;
  end;
  PWKFloat = ^TWKFloat;

  TWKLabel = packed record
    rhType: TWKRecordHeader;
    bFmt: Byte;
    wColumn: Word;
    wRow: Word;
    pValue: array[0..239] of char;
  end;
  PWKLabel = ^TWKLabel;

  TWKColumnWidth = packed record
    rhType: TWKRecordHeader;
    wColumn: Word;
    wValue: Word;
  end;
  PWKColumnWidth = ^TWKColumnWidth;

  TCellFormat = set of (cfNone);

  TWKFile = class(TObject)
  private
  protected
    FStream: TStream;
    function Write(const Buffer; Count: Integer): Longint; {$IFNDEF LESS160} inline;{$ENDIF}
  public
    procedure WriteCell(ARow, ACol: Word; AData: Variant; ACellType: TwkCellType; AFormat: Byte {$IFNDEF LESS110}= 255{$ENDIF});
    procedure WriteWordCell(ARow, ACol: Word; AData: Word; AFormat: Byte {$IFNDEF LESS110}= 255{$ENDIF});
    procedure WriteDoubleCell(ARow, ACol: Word; AData: Double; AFormat: Byte {$IFNDEF LESS110}= 255{$ENDIF});
    procedure WriteStringCell(ARow, ACol: Word; AData: TWKLabelString; AAttributes: Byte {$IFNDEF LESS110}= 255{$ENDIF});
    procedure SetColWidth(ACol, AWidth: Word); { Cantidad de Caracteres de Ancho }
    procedure ClearCell(ARow, ACol: Word; AFormat: Byte {$IFNDEF LESS110}= 255{$ENDIF});

    constructor Create(AStream : TStream);
    destructor Destroy; override;
  end;


implementation

uses
  Windows{$IFNDEF LESS140}, Variants{$ENDIF};

const
  BOF_INFO: array [0..2] of word = (LRT_BOF, 2, 1030);
  EOF_INFO: array [0..1] of word = (LRT_EOF, 0);

{ TWKFile }

procedure TWKFile.ClearCell(ARow, ACol: Word; AFormat: Byte);
var
 WKData: TWKBlank;
begin
  WKData.rhType.wType := LRT_BLANK;
  WKData.rhType.wLength := SizeOf(TWKBlank) - SizeOf(TWKRecordHeader);
  WKData.bFmt := AFormat;
  WKData.wRow := ARow;
  WKData.wColumn := ACol;

  Write(WKData, SizeOf(TWKBlank));
end;

constructor TWKFile.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  Write(BOF_INFO, SizeOf(BOF_INFO));
end;

destructor TWKFile.Destroy;
begin
  Write(EOF_INFO, SizeOf(EOF_INFO));
  inherited;
end;

procedure TWKFile.SetColWidth(ACol, AWidth: Word);
var
  WKData: TWKColumnWidth;
begin
  WKData.rhType.wType := LRT_COLW1;
  WKData.rhType.wLength := SizeOf(TWKColumnWidth) - SizeOf(TWKRecordHeader);
  WKData.wColumn := ACol;
  WKData.wValue := AWidth;

  Write(WKData, SizeOf(TWKColumnWidth));
end;

function TWKFile.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FStream.Write(Buffer, Count);
end;

procedure TWKFile.WriteWordCell(ARow, ACol: Word; AData: Word; AFormat : Byte);
var
  WKData: TWKInteger;
begin
  WKData.rhType.wType := LRT_INTEGER;
  WKData.rhType.wLength := SizeOf(TWKInteger) - SizeOf(TWKRecordHeader);
  WKData.bFmt := 255;
  WKData.wRow := ARow;
  WKData.wColumn := ACol;
  WKData.wValue := AData;

  Write(WKData, SizeOf(TWKInteger));
end;

procedure TWKFile.WriteDoubleCell(ARow, ACol: Word; AData: Double; AFormat: Byte);
var
  WKData: TWKFloat;
begin
  WKData.rhType.wType := LRT_NUMBER;
  WKData.rhType.wLength := SizeOf(TWKFloat) - SizeOf(TWKRecordHeader);
  WKData.bFmt := 2 or 128;
  WKData.wRow := ARow;
  WKData.wColumn := ACol;
  WKData.wValue := AData;

  Write(WKData, SizeOf(TWKFloat));
end;

procedure TWKFile.WriteStringCell(ARow, ACol: Word; AData: TWKLabelString; AAttributes: Byte);
var
  WKData: TWKLabel;
  iSize: Cardinal;
begin
  iSize := Length(AData)+2;

  WKData.rhType.wType := LRT_LABEL;
  WKData.rhType.wLength := SizeOf(TWKLabel) + iSize - (SizeOf(TWKRecordHeader) + SizeOf(WKData.pValue));
  WKData.bFmt := 255;
  WKData.wRow := ARow;
  WKData.wColumn := ACol;
  ZeroMemory(@WKData.pValue, SizeOf(WKData.pValue));
  WKData.pValue[0] := '''';
  StrPCopy(@WKData.pValue[1], AData);

  Write(WKData, SizeOf(TWKLabel) + iSize - SizeOf(WKData.pValue));
end;

procedure TWKFile.WriteCell(ARow, ACol: Word; AData: Variant; ACellType: TwkCellType; AFormat: Byte);
begin
  if VarIsNull(AData) Then
    ACellType := wtBlank;

  case ACellType of
    wtBlank:
      ClearCell(ARow, ACol, AFormat);
    wtInteger:
      WriteWordCell(ARow, ACol, Integer(AData), AFormat);
    wtDouble:
      WriteDoubleCell(ARow, ACol, Double(AData), AFormat);
    wtLabel:
      WriteStringCell(ARow, ACol, string(AData), AFormat);
    {ctBoolean:
      WriteCell(ARow, ACol, Boolean(AData), AFormat);}
  end;
end;

end.
