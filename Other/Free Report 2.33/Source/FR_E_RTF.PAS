
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            RTF export filter            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_E_RTF;

interface

{$I FR.inc}

uses
  SysUtils, Windows, Messages, Classes, Graphics, Forms, StdCtrls, ComCtrls,
  FR_Class, FR_E_TXT;

type
  TfrRTFExport = class(TComponent) // fake component
  end;

  TfrRTFExportFilter = class(TfrTextExportFilter)
  private
    TempStream: TStream;
    FontTable, ColorTable: TStringList;
    DataList: TList;
    NewPage: Boolean;
  public
    constructor Create(AStream: TStream); override;
    destructor Destroy; override;
    procedure OnEndPage; override;
    procedure OnData(x, y: Integer; View: TfrView); override;
  end;


implementation

uses FR_Const;

constructor TfrRTFExportFilter.Create(AStream: TStream);
var
  s: String;
begin
  inherited Create(AStream);
  FontTable := TStringList.Create;
  ColorTable := TStringList.Create;
  DataList := TList.Create;
  TempStream := TFileStream.Create('Export.tmp', fmCreate);
  s := '{\rtf1\ansi' + #13#10;
  Stream.Write(s[1], Length(s));
end;

destructor TfrRTFExportFilter.Destroy;
var
  i, c: Integer;
  s, s1: String;
begin
  s := '\par}';
  TempStream.Write(s[1], Length(s));
  s := '{\fonttbl';
  for i := 0 to FontTable.Count - 1 do
  begin
    s1 := '{\f' + IntToStr(i) + ' ' + FontTable[i] + '}';
    if Length(s + s1) < 255 then
      s := s + s1
    else
    begin
      s := s + #13#10;
      Stream.Write(s[1], Length(s));
      s := s1;
    end;
  end;
  s := s + '}' + #13#10;
  Stream.Write(s[1], Length(s));

  s := '{\colortbl;';
  for i := 0 to ColorTable.Count - 1 do
  begin
    c := StrToInt(ColorTable[i]);
    s1 := '\red' + IntToStr(GetRValue(c)) +
          '\green' + IntToStr(GetGValue(c)) +
          '\blue' + IntToStr(GetBValue(c)) + ';';
    if Length(s + s1) < 255 then
      s := s + s1
    else
    begin
      s := s + #13#10;
      Stream.Write(s[1], Length(s));
      s := s1;
    end;
  end;
  s := s + '}' + #13#10;
  Stream.Write(s[1], Length(s));

  Stream.CopyFrom(TempStream, 0);
  TempStream.Free;
  DeleteFile('Export.tmp');
  FontTable.Free;
  ColorTable.Free;
  DataList.Free;
  inherited Destroy;
end;

procedure TfrRTFExportFilter.OnEndPage;
var
  i, j, n, n1, x, y, dx, dy: Integer;
  p: PfrTextRec;
  s0, s, s1: String;
  fSize, fStyle, fColor: Integer;
  fName: String;
  Str: TStream;
  bArr: Array[0..1023] of Byte;

  function GetFontStyle(f: Integer): String;
  begin
    Result := '';
    if (f and $1) <> 0 then Result := '\i';
    if (f and $2) <> 0 then Result := Result + '\b';
    if (f and $4) <> 0 then Result := Result + '\u';
  end;

  function GetFontColor(f: String): String;
  var
    i: Integer;
  begin
    i := ColorTable.IndexOf(f);
    if i <> -1 then
      Result := IntToStr(i + 1)
    else
    begin
      ColorTable.Add(f);
      Result := IntToStr(ColorTable.Count);
    end;
  end;

  function GetFontName(f: String): String;
  var
    i: Integer;
  begin
    i := FontTable.IndexOf(f);
    if i <> -1 then
      Result := IntToStr(i)
    else
    begin
      FontTable.Add(f);
      Result := IntToStr(FontTable.Count - 1);
    end;
  end;

begin
  if NewPage then
  begin
    s := '\page' + #13#10;
    TempStream.Write(s[1], Length(s));
  end;

  for i := 0 to DataList.Count - 1 do
  begin
    Str := TStream(DataList[i]);
    Str.Position := 0;
    Str.Read(x, 4);
    Str.Read(y, 4);
    Str.Read(dx, 4);
    Str.Read(dy, 4);
    s := '\pard\phmrg\posx' + IntToStr(Round(x / 1.3 * 20)) +
         '\posy' + IntToStr(y * 20) +
         '\absh' + IntToStr(dy * 20) +
         '\absw' + IntToStr(dx * 20) +
         '{\pict\wmetafile8\picw' + IntToStr(Round(dx * 26.46875)) +
         '\pich' + IntToStr(Round(dy * 26.46875)) + ' \picbmp\picbpp4' + #13#10;
    TempStream.Write(s[1], Length(s));
// shit begins
    Str.Read(dx, 4);
    Str.Read(dy, 4);
    Str.Read(n, 2);
    Str.Read(n, 4);
    n := n div 2 + 7;
    s0 := IntToHex(n + $24, 8);
    s := '010009000003' + Copy(s0, 7, 2) + Copy(s0, 5, 2) +
         Copy(s0, 3, 2) + Copy(s0, 1, 2) + '0000';
    s0 := IntToHex(n, 8);
    s1 := Copy(s0, 7, 2) + Copy(s0, 5, 2) + Copy(s0, 3, 2) + Copy(s0, 1, 2);
    s := s + s1 + '0000050000000b0200000000050000000c02';
    s0 := IntToHex(dy, 4);
    s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2);
    s0 := IntToHex(dx, 4);
    s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2) +
         '05000000090200000000050000000102ffffff000400000007010300' + s1 +
         '430f2000cc000000';
    s0 := IntToHex(dy, 4);
    s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2);
    s0 := IntToHex(dx, 4);
    s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2) + '00000000';
    s0 := IntToHex(dy, 4);
    s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2);
    s0 := IntToHex(dx, 4);
    s := s + Copy(s0, 3, 2) + Copy(s0, 1, 2) + '00000000' + #13#10;
    TempStream.Write(s[1], Length(s));
// shit ends

    Str.Read(bArr[0], 8);
    n1 := 0; s := '';
    repeat
      n := Str.Read(bArr[0], 1024);
      for j := 0 to n - 1 do
      begin
        s := s + IntToHex(bArr[j], 2);
        Inc(n1);
        if n1 > 63 then
        begin
          n1 := 0;
          s := s + #13#10;
          TempStream.Write(s[1], Length(s));
          s := '';
        end;
      end;
    until n < 1024;
    Str.Free;
    if n1 <> 0 then
      TempStream.Write(s[1], Length(s));

    s := '030000000000}\par' + #13#10;
    TempStream.Write(s[1], Length(s));
  end;

  n := Lines.Count - 1;
  while n >= 0 do
  begin
    if Lines[n] <> nil then break;
    Dec(n);
  end;

  for i := 0 to n do
  begin
    p := PfrTextRec(Lines[i]);
    s0 := '\pard'; s := '';
    fSize := -1; fStyle := -1; fColor := -1; fName := '';
    while p <> nil do
    begin
      s0 := s0 + '\tx' + IntToStr(Round(p^.X / 1.3 * 20));
      s1 := '';
      if p^.FontColor = clWhite then
        p^.FontColor := clBlack;
      if fName <> p^.FontName then
        s1 := '\f' + GetFontName(p^.FontName);
      if fSize <> p^.FontSize then
        s1 := s1 + '\fs' + IntToStr(p^.FontSize * 2);
      if fStyle <> p^.FontStyle then
        s1 := s1 + GetFontStyle(p^.FontStyle);
      if fColor <> p^.FontColor then
        s1 := s1 + '\cf' + GetFontColor(IntToStr(p^.FontColor));

      s := s + '\tab' + s1 + ' ' + p^.Text;

      fSize := p^.FontSize; fStyle := p^.FontStyle;
      fColor := p^.FontColor; fName := p^.FontName;
      p := p^.Next;
    end;
    s := s0 + '{' + s + '\par}' + #13#10;
    TempStream.Write(s[1], Length(s));
  end;

  NewPage := True;
  DataList.Clear;
end;

procedure TfrRTFExportFilter.OnData(x, y: Integer; View: TfrView);
var
  Str: TStream;
  n: Integer;
begin
  if View is TfrPictureView then
  begin
    Str := TMemoryStream.Create;
    Str.Write(x, 4);
    Str.Write(y, 4);
    Str.Write(View.dx, 4);
    Str.Write(View.dy, 4);
    n := TfrPictureView(View).Picture.Graphic.Width;
    Str.Write(n, 4);
    n := TfrPictureView(View).Picture.Graphic.Height;
    Str.Write(n, 4);
    TfrPictureView(View).Picture.Graphic.SaveToStream(Str);
    DataList.Add(Str);
  end;
end;


initialization
  frRegisterExportFilter(TfrRTFExportFilter, LoadStr(SRTFFile) + ' (*.rtf)', '*.rtf');

end.
