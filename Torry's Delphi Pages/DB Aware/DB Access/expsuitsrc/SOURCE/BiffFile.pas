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

{$I-,N+}
unit BiffFile;

{$I DELPHI.VER}

interface

uses
  Classes, SysUtils, Graphics;

Const
  { LenData }
  LEN_BOF_BIFF5 = 6;
  LEN_EOF_BIFF5 = 0;
  LEN_DIM_BIFF5 = 10;
  LEN_RECORDHEADER = 4;

  { Record Types Cells by BIFFn}
  RT5_CELL_EMPTY = 1;
  RT5_CELL_INT = 2;
  RT5_CELL_DOUBLE = 3;
  RT5_CELL_LABEL = 4;
  RT5_CELL_BOOL = 5;

  { Document types }
  DOCTYPE_XLS = $0010;
  DOCTYPE_XLC = $0020;
  DOCTYPE_XLM = $0040;
  DOCTYPE_XLW = $0100;

  MAX_DIM = 65535; { High(TCellCoord) }

type
  LabelString = string[255];
  rgbAttrib =  array[0..2] of Byte;
  TCellCoord = Word;

  TCellAttribute = (caHidden, caLocked, caShaded,
                    caBottomBorder, caTopBorder, caRightBorder, caLeftBorder,
                    caFont1, caFont2, caFont3, caFont4,
                    caLeft, caCenter, caRight, caFill);
  TCellAttributes = set of TCellAttribute;
  TCellType = (ctBlank, ctInteger, ctDouble, ctLabel, ctBoolean);
 {TCellType = (ctString, ctInteger, ctDouble, ctBoolean, ctNull);}

{ TBiffFile class }
{ This can be used to write any BIFF5 file to any type of streams. }

  TBiffFile = class(TObject)
  private
  protected
     FStream: TStream;
     function Write(const Buffer; Count: Integer): Longint; {$IFNDEF LESS160} inline;{$ENDIF}

     procedure Internal_WriteBOF;
     procedure Internal_WriteEOF;
     procedure WriteRecordHeader(ALen, AType: Integer);
     procedure SetAttributes(ARow, ACol: TCellCoord; AAttributes: TCellAttributes
       {$IFNDEF LESS110}= []{$ENDIF});
     function GetRgbAttrib(AAttributes: TCellAttributes): rgbAttrib;
  public
     procedure SetDimensions(AFirstRow: Word {$IFNDEF LESS110}= 1{$ENDIF};
       ALastRow: Word {$IFNDEF LESS110}= MAX_DIM{$ENDIF};
       AFirstCol: Word {$IFNDEF LESS110}= 1{$ENDIF};
       ALastCol: Word {$IFNDEF LESS110}= MAX_DIM{$ENDIF});
     procedure WriteCell(ARow, ACol: TCellCoord; AData: Variant;
       ACellType: TCellType; AAttributes: TCellAttributes {$IFNDEF LESS110}= []{$ENDIF});
     procedure WriteStringCell(ARow, ACol: TCellCoord; AData: LabelString;
       AAttributes: TCellAttributes {$IFNDEF LESS110}= []{$ENDIF});
     procedure WriteWordCell(ARow, ACol: TCellCoord; AData: Word;
       AAttributes: TCellAttributes {$IFNDEF LESS110}= []{$ENDIF});
     procedure WriteDoubleCell(ARow, ACol: TCellCoord; AData: Double;
       AAttributes: TCellAttributes {$IFNDEF LESS110}= []{$ENDIF});
     procedure WriteBooleanCell(ARow, ACol: TCellCoord; AData: Boolean;
       AAttributes: TCellAttributes {$IFNDEF LESS110}= []{$ENDIF});
     procedure ClearCell(ARow, ACol: TCellCoord; AAttributes: TCellAttributes
       {$IFNDEF LESS110}= []);{$ELSE});{$ENDIF}
     procedure SetColRangeWidth(AFirstCol, ALastCol: Byte; AWidth: Word);
     procedure SetColWidth(ACol: Byte; AWidth: Word);
     procedure WriteCellNote(ARow, ACol: TCellCoord; AText: LabelString);
     procedure WriteFontByName(AHeight: Word; AFontStyle: TFontStyles;
       AFontName: LabelString); {overload;}
     procedure WriteFont(AFont: TFont); {overload;}
     procedure SetRowAttributes(ARow, FirstDefCol, ALastDefCol: TCellCoord;
       ARowHeight: Word; ADefAttibutes: Boolean {$IFNDEF LESS110}= True{$ENDIF};
       AAttributes: TCellAttributes {$IFNDEF LESS110}= []{$ENDIF};
       AOffset: Word {$IFNDEF LESS110}= 0{$ENDIF});
     procedure SetDefaultRowHeight(AHeight: Word);

     constructor Create(AStream: TStream);
     destructor Destroy; override;
  end;

implementation

{$IFNDEF LESS130}
uses
  Variants;
{$ENDIF}

{ TBiffFile }

constructor TBiffFile.Create(AStream : TStream);
begin
  FStream := AStream;
  Internal_WriteBOF;
end;

destructor TBiffFile.Destroy;
begin
  Internal_WriteEOF;
end;

procedure TBiffFile.Internal_WriteBOF;
var
  aBuf: array[0..1] of Word;
begin
  aBuf[0] := 2;
  aBuf[1] := 10;
  WriteRecordHeader(4, 9);
  Write(aBuf, SizeOf(aBuf));
end;

procedure TBiffFile.Internal_WriteEOF;
begin
  WriteRecordHeader(0, $000a);
end;

procedure TBiffFile.SetDimensions(AFirstRow: Word; ALastRow: Word;
  AFirstCol: Word; ALastCol: Word);
var
  aBuf: array[0..3] of Word;
begin
  aBuf[0] := AFirstRow;
  aBuf[1] := ALastRow;
  aBuf[2] := AFirstCol;
  aBuf[3] := ALastCol;
  WriteRecordHeader(8, $0000);
  Write(aBuf, SizeOf(aBuf));
end;

procedure TBiffFile.WriteRecordHeader(ALen, AType : Integer);
var
  aBuf: array[0..1] of Word;
begin
  aBuf[0] := AType;
  aBuf[1] := ALen;
  Write(aBuf, LEN_RECORDHEADER);
end;

function TBiffFile.GetRgbAttrib(AAttributes: TCellAttributes): rgbAttrib;
begin
  Result[0] := 0;
  Result[1] := 0;
  Result[2] := 0;

  if caHidden in AAttributes then  { byte 0 bit 7 }
    Result[0] := Result[0] + 128;

  if caLocked in AAttributes then  { byte 0 bit 6 }
    Result[0] := Result[0] + 64 ;


  if caFont1 in AAttributes then
    {Result[1] := Result[1]}
  else if caFont2 in AAttributes then
    Result[1] := Result[1] + 64
  else if caFont3 in AAttributes then
    Result[1] := Result[1] + 128
  else if caFont4 in AAttributes then
    Result[1] := Result[1] + 192;


  if caShaded in AAttributes then  { byte 2 bit 7 }
    Result[2] := Result[2] + 128;

  if caBottomBorder in AAttributes then  { byte 2 bit 6 }
    Result[2] := Result[2] + 64 ;

  if caTopBorder in AAttributes then  { byte 2 bit 5 }
    Result[2] := Result[2] + 32;

  if caRightBorder in AAttributes then  { byte 2 bit 4 }
    Result[2] := Result[2] + 16;

  if caLeftBorder in AAttributes then  { byte 2 bit 3 }
    Result[2] := Result[2] + 8;

  if caLeft in AAttributes then  { byte 2 bit 1 }
    Result[2] := Result[2] + 1
  else if caCenter in AAttributes then  { byte 2 bit 1 }
    Result[2] := Result[2] + 2
  else if caRight in AAttributes then  { byte 2, bit 0 dan bit 1 }
    Result[2] := Result[2] + 3;

  if caFill in AAttributes then  { byte 2, bit 0 }
    Result[2] := Result[2] + 4;
end;

procedure TBiffFile.SetAttributes(ARow, ACol: TCellCoord; AAttributes: TCellAttributes);
var
  aBuf: array[0..1] of TCellCoord;
  AAttribute: rgbAttrib;
begin
  aBuf[0] := ARow;
  aBuf[1] := ACol;
  AAttribute := GetRgbAttrib(AAttributes);
  Write(aBuf, SizeOf(aBuf));
  Write(AAttribute, SizeOf(AAttribute));
end;

procedure TBiffFile.ClearCell(ARow, ACol: TCellCoord; AAttributes: TCellAttributes);
begin
  WriteRecordHeader(7, RT5_CELL_EMPTY);
  SetAttributes(ARow, ACol, AAttributes);
end;

procedure TBiffFile.WriteCell(ARow, ACol: TCellCoord; AData: Variant;
  ACellType: TCellType; AAttributes: TCellAttributes); {override;}
begin
  if VarIsNull(AData) Then
    ACellType := ctBlank;

  case ACellType of
    ctBlank:
      ClearCell(ARow, ACol, AAttributes);
    ctInteger:
      WriteWordCell(ARow, ACol, Integer(AData), AAttributes);
    ctDouble:
      WriteDoubleCell(ARow, ACol, Double (AData), AAttributes);
    ctLabel:
      WriteStringCell(ARow, ACol, string(AData), AAttributes);
    ctBoolean:
      WriteBooleanCell(ARow, ACol, Boolean(AData), AAttributes);
  end;
end;

procedure TBiffFile.WriteWordCell(ARow, ACol : TCellCoord; AData: Word;
  AAttributes: TCellAttributes); {override;}
begin
  WriteRecordHeader(9, RT5_CELL_INT);
  SetAttributes(ARow, ACol, AAttributes);
  Write(AData, 2);
end;

procedure TBiffFile.WriteStringCell(ARow, ACol: TCellCoord; AData: LabelString;
  AAttributes: TCellAttributes); {override;}
var
  ALen: Byte;
begin
  ALen := Length(AData);
  WriteRecordHeader(ALen + 8 , RT5_CELL_LABEL);
  SetAttributes(ARow, ACol, AAttributes);
  Write(ALen, SizeOf(ALen));
  Write(Pointer(string(AData))^, ALen);
end;

procedure TBiffFile.WriteBooleanCell(ARow, ACol: TCellCoord; AData: Boolean; AAttributes: TCellAttributes); {override;}
var
  ABoolResult : Byte;
begin
  WriteRecordHeader(9, RT5_CELL_BOOL);
  SetAttributes(ARow, ACol, AAttributes);

  if AData then
    ABoolResult := 1
  else
    ABoolResult := 0;

  Write(ABoolResult, SizeOf(ABoolResult));
  ABoolResult := 0;
  Write(ABoolResult, SizeOf(ABoolResult));
end;

procedure TBiffFile.WriteDoubleCell(ARow, ACol: TCellCoord; AData: Double; AAttributes: TCellAttributes); {override;}
begin
  WriteRecordHeader(15, RT5_CELL_DOUBLE);
  SetAttributes(ARow, ACol, AAttributes);
  Write(AData, 8);
end;

procedure TBiffFile.SetColRangeWidth(AFirstCol, ALastCol: Byte; AWidth: Word);
var
  aBuf: array[0..1] of Byte;
begin
  { 256 puntos por Caracter }
  aBuf[0] := AFirstCol;
  aBuf[1] := ALastCol;
  WriteRecordHeader(4, $0024);
  Write(aBuf, 2);
  Write(AWidth, 2);
end;

procedure TBiffFile.SetColWidth(ACol: Byte; AWidth: Word);
begin
  SetColRangeWidth(ACol, ACol, AWidth);
end;

procedure TBiffFile.WriteCellNote(ARow, ACol: TCellCoord; AText: LabelString);
var
  aBuf: array[0..2] of Word;
begin
  aBuf[0] := ARow;
  aBuf[1] := ACol;
  aBuf[2] := Length(AText);

  WriteRecordHeader(aBuf[2] + 6 , 28);
  Write(aBuf, 6);
  Write(Pointer(string(AText))^, aBuf[2]);
end;

procedure TBiffFile.WriteFontByName(AHeight: Word; AFontStyle: TFontStyles; AFontName: LabelString);
var
  ALen: Byte;
  AStyle: array[0..1] of Byte;
begin
  ALen := Length(AFontName);
  WriteRecordHeader(ALen + 5 , $0031);
  Write(AHeight, 2);

  { Segun la documentación, es el segundo byte el que guarda el estilo }
  { pero en la práctica es alreves                                     }
  AStyle[0] := 0;
  AStyle[1] := 0;

  if fsBold in AFontStyle Then
    AStyle[0] := AStyle[0] + 1;

  if fsItalic in AFontStyle Then
    AStyle[0] := AStyle[0] + 2;

  if fsUnderline in AFontStyle Then
    AStyle[0] := AStyle[0] + 4;

  if fsStrikeOut in AFontStyle Then
    AStyle[0] := AStyle[0] + 8;

   Write(AStyle, SizeOf(AStyle));

  Write(ALen, 1);
  Write(Pointer(string(AFontName))^, ALen); // Guada el Nombre de la Fuente

  { Los archivos de excel, despues de la fuente guardan este registro }
{
  WriteRecordHeader(2 , $0045);
  ALen := $FF;
  Write(ALen, 1);
  ALen := $7F;
  Write(ALen, 1);
}
end;

procedure TBiffFile.WriteFont(AFont: TFont);
begin
  WriteFontByName(AFont.Size * 20, AFont.Style, AFont.Name);
end;

procedure TBiffFile.SetRowAttributes(ARow, FirstDefCol, ALastDefCol : TCellCoord;
  ARowHeight: Word; ADefAttibutes: Boolean; AAttributes: TCellAttributes; AOffset: Word);
var
  aBuf: array[0..4] of Word;
  aByte: Byte;
  AAttribute: rgbAttrib;
begin
  WriteRecordHeader(13, $0008);
  aBuf[0] := ARow;
  aBuf[1] := FirstDefCol;
  aBuf[2] := ALastDefCol + 1;
  aBuf[3] := ARowHeight;
  aBuf[4] := 0;
  Write(aBuf, 10);

  if ADefAttibutes then
    aByte := 1
  else
    aByte := 0;

  AAttribute := GetRgbAttrib(AAttributes);

  Write(aByte, SizeOf(aByte));
  Write(AOffset, SizeOf(AOffset));
 {Write(AAttribute, SizeOf(AAttribute));}
end;

procedure TBiffFile.SetDefaultRowHeight(AHeight: Word);
begin
  WriteRecordHeader(2, $0025);
  Write(AHeight, 2);
end;

function TBiffFile.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FStream.Write(Buffer, Count)
end;

end.
