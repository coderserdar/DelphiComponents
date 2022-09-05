
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            Various routines             }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_Utils;

interface

{$I FR.inc}

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  FR_DBRel, Forms, StdCtrls, ClipBrd, Menus, FR_Class;


procedure frReadMemo(Stream: TStream; l: TStrings);
procedure frReadMemo22(Stream: TStream; l: TStrings);
procedure frWriteMemo(Stream: TStream; l: TStrings);
function frReadString(Stream: TStream): String;
function frReadString22(Stream: TStream): String;
procedure SaveToFR3Stream(Report: TfrReport; Stream: TStream);
function StrToXML(const s: String): String;
function frStreamToString(Stream: TStream): String;
{$IFDEF FREEREP2217READ}
function frReadString2217(Stream: TStream): String;
{$ENDIF}
procedure frWriteString(Stream: TStream; s: String);
procedure frEnableControls(c: Array of TControl; e: Boolean);
function frControlAtPos(Win: TWinControl; p: TPoint): TControl;
function frGetDataSet(ComplexName: String): TfrTDataSet;
procedure frGetDataSetAndField(ComplexName: String;
  var DataSet: TfrTDataSet; var Field: TfrTField);
function frGetFontStyle(Style: TFontStyles): Integer;
function frSetFontStyle(Style: Integer): TFontStyles;
function frFindComponent(Owner: TComponent; Name: String): TComponent;
procedure frGetComponents(Owner: TComponent; ClassRef: TClass;
  List: TStrings; Skip: TComponent);
function frGetWindowsVersion: String;



implementation

uses FR_DSet, Printers;


function frSetFontStyle(Style: Integer): TFontStyles;
begin
  Result := [];
  if (Style and $1) <> 0 then Result := Result + [fsItalic];
  if (Style and $2) <> 0 then Result := Result + [fsBold];
  if (Style and $4) <> 0 then Result := Result + [fsUnderLine];
end;

function frGetFontStyle(Style: TFontStyles): Integer;
begin
  Result := 0;
  if fsItalic in Style then Result := Result or $1;
  if fsBold in Style then Result := Result or $2;
  if fsUnderline in Style then Result := Result or $4;
end;

procedure RemoveQuotes(var s: String);
begin
  if (s[1] = '"') and (s[Length(s)] = '"') then
    s := Copy(s, 2, Length(s) - 2);
end;

procedure frReadMemo(Stream: TStream; l: TStrings);
var
  s: String;
  b: Byte;
  n: Word;
begin
  l.Clear;
  Stream.Read(n, 2);
  if n > 0 then
    repeat
      Stream.Read(n, 2);
      SetLength(s, n);
      Stream.Read(s[1], n);
      l.Add(s);
      Stream.Read(b, 1);
    until b = 0
  else
    Stream.Read(b, 1);
end;

procedure frWriteMemo(Stream: TStream; l: TStrings);
var
  s: String;
  i: Integer;
  n: Word;
  b: Byte;
begin
  n := l.Count;
  Stream.Write(n, 2);
  for i := 0 to l.Count - 1 do
  begin
    s := l[i];
    n := Length(s);
    Stream.Write(n, 2);
    Stream.Write(s[1], n);
    b := 13;
    if i <> l.Count - 1 then Stream.Write(b, 1);
  end;
  b := 0;
  Stream.Write(b, 1);
end;

function frReadString(Stream: TStream): String;
var
  s: String;
  n: Word;
  b: Byte;
begin
  Stream.Read(n, 2);
  SetLength(s, n);
  Stream.Read(s[1], n);
  Stream.Read(b, 1);
  Result := s;
end;

procedure frWriteString(Stream: TStream; s: String);
var
  b: Byte;
  n: Word;
begin
  n := Length(s);
  Stream.Write(n, 2);
  Stream.Write(s[1], n);
  b := 0;
  Stream.Write(b, 1);
end;

procedure frReadMemo22(Stream: TStream; l: TStrings);
var
  s: String;
  i: Integer;
  b: Byte;
begin
  SetLength(s, 4096);
  l.Clear;
  i := 1;
  repeat
    Stream.Read(b,1);
    if (b = 13) or (b = 0) then
    begin
      SetLength(s, i - 1);
      if not ((b = 0) and (i = 1)) then l.Add(s);
      SetLength(s, 4096);
      i := 1;
    end
    else if b <> 0 then
    begin
      s[i] := Chr(b);
      Inc(i);
      if i > 4096 then
        SetLength(s, Length(s) + 4096);
    end;
  until b = 0;
end;

function frReadString22(Stream: TStream): String;
var
  s: String;
  i: Integer;
  b: Byte;
begin
  SetLength(s, 4096);
  i := 1;
  repeat
    Stream.Read(b, 1);
    if b = 0 then
      SetLength(s, i - 1)
    else
    begin
      s[i] := Chr(b);
      Inc(i);
      if i > 4096 then
        SetLength(s, Length(s) + 4096);
    end;
  until b = 0;
  Result := s;
end;

{$IFDEF FREEREP2217READ}
function frReadString2217(Stream: TStream): String;
var
  I: Integer;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  SetLength(Result, I);
  Stream.ReadBuffer(PChar(Result)^, I);
end;
{$ENDIF}

type
  THackWinControl = class(TWinControl)
  end;

procedure frEnableControls(c: Array of TControl; e: Boolean);
const
  Clr1: Array[Boolean] of TColor = (clGrayText,clWindowText);
  Clr2: Array[Boolean] of TColor = (clBtnFace,clWindow);
var
  i: Integer;
begin
  for i := Low(c) to High(c) do
    if c[i] is TLabel then
      with c[i] as TLabel do
      begin
        Font.Color := Clr1[e];
        Enabled := e;
      end
    else if c[i] is TWinControl then
      with THackWinControl(c[i]) do
      begin
        Color := Clr2[e];
        Enabled := e;
      end;
end;

function frControlAtPos(Win: TWinControl; p: TPoint): TControl;
var
  i: Integer;
  c: TControl;
  p1: TPoint;
begin
  Result := nil;
  with Win do
  begin
    for i := ControlCount - 1 downto 0 do
    begin
      c := Controls[i];
      if c.Visible and PtInRect(Rect(c.Left, c.Top, c.Left + c.Width, c.Top + c.Height), p) then
        if (c is TWinControl) and (csAcceptsControls in c.ControlStyle) and
           (TWinControl(c).ControlCount > 0) then
        begin
          p1 := p;
          Dec(p1.X, c.Left); Dec(p1.Y, c.Top);
          c := frControlAtPos(TWinControl(c), p1);
          if c <> nil then
          begin
            Result := c;
            Exit;
          end;
        end
        else
        begin
          Result := c;
          Exit;
        end;
    end;
  end;
end;

function frGetDataSet(ComplexName: String): TfrTDataSet;
begin
  Result := TfrTDataSet(frFindComponent(CurReport.Owner, ComplexName));
end;

procedure frGetDataSetAndField(ComplexName: String; var DataSet: TfrTDataSet;
  var Field: TfrTField);
var
  n: Integer;
  f: TComponent;
  s1, s2, s3: String;
begin
  Field := nil;
  f := CurReport.Owner;
  n := Pos('.', ComplexName);
  if n <> 0 then
  begin
    s1 := Copy(ComplexName, 1, n - 1);        // table name
    s2 := Copy(ComplexName, n + 1, 255);      // field name
    if Pos('.', s2) <> 0 then                 // module name present
    begin
      s3 := Copy(s2, Pos('.', s2) + 1, 255);
      s2 := Copy(s2, 1, Pos('.', s2) - 1);
      f := FindGlobalComponent(s1);
      if f <> nil then
      begin
        DataSet := TfrTDataSet(f.FindComponent(s2));
        RemoveQuotes(s3);
        if DataSet <> nil then
          Field := TfrTField(DataSet.FindField(s3));
      end;
    end
    else
    begin
      DataSet := TfrTDataSet(frFindComponent(f, s1));
      RemoveQuotes(s2);
      if DataSet <> nil then
        Field := TfrTField(DataSet.FindField(s2));
    end;
  end
  else if DataSet <> nil then
  begin
    RemoveQuotes(ComplexName);
    Field := TfrTField(DataSet.FindField(ComplexName));
  end;
end;

function frFindComponent(Owner: TComponent; Name: String): TComponent;
var
  n: Integer;
  s1, s2: String;
begin
  Result := nil;
  n := Pos('.', Name);
  try
    if n = 0 then
      Result := Owner.FindComponent(Name)
    else
    begin
      s1 := Copy(Name, 1, n - 1);        // module name
      s2 := Copy(Name, n + 1, 255);      // component name
      Owner := FindGlobalComponent(s1);
      if Owner <> nil then
        Result := Owner.FindComponent(s2);
    end;
  except
    on Exception do
      raise EClassNotFound.Create('Отсутствует ' + Name);
  end;
end;

procedure frGetComponents(Owner: TComponent; ClassRef: TClass;
  List: TStrings; Skip: TComponent);
var
  i: Integer;
  procedure EnumComponents(f: TComponent);
  var
    i: Integer;
    c: TComponent;
  begin
    for i := 0 to f.ComponentCount - 1 do
    begin
      c := f.Components[i];
      if (c <> Skip) and (c is ClassRef) then
        if f = Owner then
          List.Add(c.Name) else
          List.Add(f.Name + '.' + c.Name);
    end;
  end;
begin
  List.Clear;
  for i := 0 to Screen.FormCount - 1 do
    EnumComponents(Screen.Forms[i]);
  for i := 0 to Screen.DataModuleCount - 1 do
    EnumComponents(Screen.DataModules[i]);
end;

function frGetWindowsVersion: String;
var Ver: TOsVersionInfo;
begin
  Ver.dwOSVersionInfoSize := SizeOf(Ver);
  GetVersionEx(Ver);
  with Ver do begin
    case dwPlatformId of
      VER_PLATFORM_WIN32s: Result := '32s';
      VER_PLATFORM_WIN32_WINDOWS:
        begin
          dwBuildNumber := dwBuildNumber and $0000FFFF;
          if (dwMajorVersion > 4) or ((dwMajorVersion = 4) and
            (dwMinorVersion >= 10)) then
            Result := '98' else
            Result := '95';
        end;
      VER_PLATFORM_WIN32_NT: Result := 'NT';
    end;
  end;
end;

procedure SaveToFR3Stream(Report: TfrReport; Stream: TStream);
const
  fr01cm = 3.77953; // 96 / 25.4
  frKx = 96 / (93 / 1.015); // convert from 2.4 units to 3.0 units

  procedure WriteStr(const s: String);
  begin
    Stream.Write(s[1], Length(s));
  end;

  procedure WriteLn(const s: String);
  begin
    WriteStr(s + #13#10);
  end;

  function EncodePwd(const s: String): String;
  var
    i: Integer;
  begin
    Result := '';
    for i := 1 to Length(s) do
      Result := Result + Chr(Ord(s[i]) - 10);
  end;
  procedure WriteReportProp;

    procedure WriteScript;
    var
      i, j: Integer;
      Page: TfrPage;
      v: TfrView;
      Script: TStringList;

      procedure AddScript(const vName: String; vScript: TStrings);
      var
        i: Integer;
      begin
        if vScript.Count <> 0 then
        begin
          Script.Add('procedure ' + vName + 'OnBeforePrint(Sender: TfrxComponent);');
          Script.Add('begin');
          Script.Add('  with ' + vName + ', Engine do');
          Script.Add('  begin');
          if vScript[0] <> 'begin' then
            Script.Add(vScript[0]);

          for i := 1 to vScript.Count - 2 do
            Script.Add(vScript[i]);

          if vScript[0] <> 'begin' then
          begin
            if vScript.Count <> 1 then
              Script.Add(vScript[vScript.Count - 1]);
            Script.Add('  end');
            Script.Add('end;');
          end
          else
          begin
            Script.Add('  end');
            Script.Add(vScript[vScript.Count - 1] + ';');
          end;
          Script.Add('');
        end;
      end;

    begin
      Script := TStringList.Create;
      for i := 0 to Report.Pages.Count - 1 do
      begin
        Page := Report.Pages[i];
//        AddScript('Page' + IntToStr(i + 1), Page.Script);
        for j := 0 to Page.Objects.Count - 1 do
        begin
          v := Page.Objects[j];
          AddScript(v.Name, v.Script);
        end;
      end;

      Script.Add('begin');
      Script.Add('');
      Script.Add('end.');
      WriteStr(StrToXML(Script.Text) + '" ');
      Script.Free;
    end;

    procedure WriteVariables;
    var
      i: Integer;
      wr: TWriter;
      ms: TMemoryStream;
      v: TValueType;
      varName, varValue: String;
      dsList: TStringList;
    begin
      ms := TMemoryStream.Create;
      wr := TWriter.Create(ms, 4096);

      dsList := TStringList.Create;
      frGetComponents(Report.Owner, TfrDataset, dsList, nil);

      v := vaCollection;
      wr.WriteStr('Datasets');
      wr.Write(v, SizeOf(v));
      for i := 0 to dsList.Count - 1 do
      begin
//        varName := TfrDataset(dsList.Objects[i]).Name;
        varName := dsList.Strings[i];
        wr.WriteListBegin;
        wr.WriteStr('DataSet');
        v := vaNil;
        wr.Write(v, SizeOf(v));
        wr.WriteStr('DataSetName');
        wr.WriteString(varName);
        wr.WriteListEnd;
      end;
      wr.WriteListEnd;

      wr.WriteStr('Variables');
      wr.Write(v, SizeOf(v));
      for i := 0 to Report.Variables.Count - 1 do
      begin
        varName := Report.Variables.Names[i];
        varValue := Report.Variables.Values[varName];

{        ds := nil;
        fld := '';
        frGetDatasetAndField(varValue, ds, fld);
        if (ds <> nil) and (fld <> '') then
        begin
          dsFound := nil;
          for j := 0 to dsList.Count - 1 do
            if TObject(dsList.Objects[j]) is TfrDBDataSet then
              if TfrDBDataset(dsList.Objects[j]).GetDataSet = ds then
              begin
                dsFound := TfrDataset(dsList.Objects[j]);
                break;
              end;
          if dsFound <> nil then
          begin
            s := dsFound.Name;
            if Pos('_', s) = 1 then
              s := Copy(s, 2, 255);
            varValue := '<' + s + '."' + fld + '">';
          end;
        end;
 }
        wr.WriteListBegin;
        wr.WriteStr('Name');
        wr.WriteString(varName);
        wr.WriteStr('Value');
        wr.WriteString(varValue);
        wr.WriteListEnd;
      end;

      wr.WriteListEnd;
      wr.Free;
      WriteStr('Propdata="' + frStreamToString(ms) + '"');
      ms.Free;
      dsList.Free;
    end;

  begin
    WriteStr('<TfrxReport ScriptLanguage="PascalScript" ScriptText.text="');
    WriteScript;
    WriteVariables;

//    WriteStr(' ReportOptions.Name="' + {StrToXML(Report.ReportName)}' ' +
//      '" ReportOptions.Author="' + {StrToXML(Report.ReportAutor)}' ' +
//    '" ReportOptions.Description.text="' + {StrToXML(Report.ReportComment)}' ' +
//    '" ReportOptions.CreateDate="' + {FloatToStr(Report.ReportCreateDate)}' ' +
//    '" ReportOptions.LastChange="' + {FloatToStr(Report.ReportLastChange)}' ' +
//    '" ReportOptions.VersionMajor="' + {StrToXML(Report.ReportVersionMajor)}' ' +
//    '" ReportOptions.VersionMinor="' + {StrToXML(Report.ReportVersionMinor)}' ' +
//    '" ReportOptions.VersionRelease="' + {StrToXML(Report.ReportVersionRelease)}' ' +
//    '" ReportOptions.VersionBuild="' + {StrToXML(Report.ReportVersionBuild)}' ' +
//    '" ReportOptions.Password="' + {StrToXML(EncodePwd(Report.ReportPassword))}''+ '"');
    WriteLn('>');
  end;

  procedure WritePages;
  var
    i, j, ofx, savex: Integer;
    Page: TfrPage;
    v: TfrView;

    procedure WritePageProp(Page: TfrPage; const PageName: String);
    var
      s: String;
    begin
      ofx := 0;
        if Page.pgOr = poPortrait then
          s := 'poPortrait' else
          s := 'poLandscape';
        WriteStr('<TfrxReportPage Name="' + PageName + '" ');
        WriteStr('Orientation="' + s +
          '" PaperWidth="' + IntToStr(Round(Page.prnInfo.PgW / fr01cm * frKx)) +
          '" PaperHeight="' + IntToStr(Round(Page.prnInfo.PgH / fr01cm * frKx)) +
          '" PaperSize="' + IntToStr(Page.pgSize) + '" ');
        WriteStr('LeftMargin="' + IntToStr(Round(Page.LeftMargin / fr01cm * frKx)) +
          '" RightMargin="' + IntToStr(Round((Page.prnInfo.PgW - Page.RightMargin) / fr01cm * frKx)) +
          '" TopMargin="' + IntToStr(Round(Page.TopMargin / fr01cm * frKx)) +
          '" BottomMargin="' + IntToStr(Round((Page.prnInfo.PgH - Page.BottomMargin) / fr01cm * frKx)) +
          '" Columns="' + IntToStr(Page.ColCount) +
          '" ColumnWidth="' + IntToStr(Page.ColWidth) + '"');
        if Page.PrintToPrevPage then
          WriteStr(' PrintOnPreviousPage="True"');
//        if Page.Script.Count > 0 then
//          WriteStr(' OnBeforePrint="' + PageName + 'OnBeforePrint"');

        ofx := -Page.LeftMargin;
      if Page.Objects.Count = 0 then
        WriteLn('/>') else
        WriteLn('>');
    end;

  begin
    for i := 0 to Report.Pages.Count - 1 do
    begin
      Page := Report.Pages[i];
      WritePageProp(Page, 'Page' + IntToStr(i + 1));

      for j := 0 to Page.Objects.Count - 1 do
      begin
        v := Page.Objects[j];
        savex := v.x;
        v.x := v.x + ofx;
        v.SaveToFR3Stream(Stream);
        v.x := savex;
        WriteLn('/>');
      end;

      if Page.Objects.Count <> 0 then
        WriteLn('</TfrxReportPage>')
    end;
  end;

begin
  WriteLn('<?xml version="1.0" encoding="utf-8"?>');
  WriteReportProp;
  WritePages;
  WriteLn('</TfrxReport>');
end;

function StrToXML(const s: String): String;
const
  SpecChars = ['<', '>', '"', #10, #13];
var
  i: Integer;

  procedure ReplaceChars(var s: String; i: Integer);
  begin
    Insert('#' + IntToStr(Ord(s[i])) + ';', s, i + 1);
    s[i] := '&';
  end;

begin
  Result := s;
  for i := Length(s) downto 1 do
    if s[i] in SpecChars then
      ReplaceChars(Result, i);
end;

function frStreamToString(Stream: TStream): String;
var
  b: Byte;
begin
  Result := '';
  Stream.Position := 0;
  while Stream.Position < Stream.Size do
  begin
    Stream.Read(b, 1);
    Result := Result + IntToHex(b, 2);
  end;
end;

end.
