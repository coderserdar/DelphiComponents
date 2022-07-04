unit Basic;

{$include kcontrols.inc}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  Classes, SysUtils, Graphics, StdCtrls, KHexEditor, KControls;

type
  TEnvironmentPacked = packed record
    Version: Byte;
    // options
    DropFiles,
    GroupUndo,
    UndoAfterSave: Boolean;
    // appearance
    ShowAddress,
    ShowDigits,
    ShowText,
    ShowHorzLines,
    ShowVertLines,
    ShowSeparators,
    ShowInactiveCaret: Boolean;
    // settings
    AddressMode,
    AddressSize,
    CharSpacing,
    DigitGrouping,
    DisabledDrawStyle,
    LineHeightPercent,
    LineSize,
    UndoLimit: Integer;
    // font
    FontSize: Integer;
    FontStyle: TFontStyles;
  end;

  TEnvironment = record
    P: TEnvironmentPacked;
    AddressPrefix: string;
    FontName: string;
  end;

  PEnvironment = ^TEnvironment;

var
  Environment: TEnvironment;
  Colors: TKColorArray;

  AppName: string;
  IniPath: string;
  IniVersion: Integer;

const
  secSettings = 'Settings';
  secMRUFs = 'MRUFs';
  secColors = 'Custom colors';

procedure DataToString(Buffer: Pointer; Size: Integer; var S: string);
procedure StringToData(const S: string; Buffer: Pointer; Size: Integer);

function Modified2Text(Modified: Boolean): string;
function InsertMode2Text(Mode: Boolean): string;

procedure AddFontsToList(DC: HDC; L: TStrings; Pitch: TFontPitch);
function EditStrToInt(Handle: HWND; Edit: TEdit; AMin, AMax, Default: Integer; var Ok: Boolean): Integer;

procedure InitEnvironment(var Data: TEnvironment);

procedure InitColors(var Colors: TKColorArray);
procedure CopyColors(Src, Dest: TKColorArray);

implementation

uses
  Math, Res, Forms;

type
  PEnumFontData = ^TEnumFontData;
  TEnumFontData = record
    List: TStrings;
    Pitch: TFontPitch;
  end;

procedure DataToString(Buffer: Pointer; Size: Integer; var S: string);
var
  I: Integer;
  T: string;
begin
  SetLength(S, Size * 2);
  for I := 1 to Size do
  begin
    T := Format('%.2x' , [PByteArray(Buffer)^[I - 1]]);
    S[I * 2 - 1] := T[1];
    S[I * 2] := T[2];
  end;
end;

procedure StringToData(const S: string; Buffer: Pointer; Size: Integer);
var
  I, Code: Integer;
  T: string;
begin
  T := '$00';
  for I := 1 to Min(Size, Length(S) div 2) do
  begin
    T[2] := S[I * 2 - 1];
    T[3] := S[I * 2];
    Val(T, PByteArray(Buffer)^[I - 1], Code);
  end;
end;

function EditStrToInt(Handle: HWND; Edit: TEdit; AMin, AMax, Default: Integer; var Ok: Boolean): Integer;
var
  I, Code: Integer;
  S: string;
begin
  Result := Default;
  if Ok then
  begin
    Val(Edit.Text, I, Code);
    if Code > 0 then
      S := sErrIntegerValue
    else if (I < AMin) or (I > AMax) then
      S := Format(sErrIntegerValueOutOfRange, [AMin, AMax])
    else
      S := '';
    if S <> '' then
    begin
      MessageBox(Handle, PChar(S), PChar(sAppName), MB_OK);
      Ok := False;
//      Edit.Text := IntToStr(Default);
      if Edit.CanFocus then
      try
        GetParentForm(Edit).ActiveControl := Edit;
      except
      end;  
    end else
      Result := I;
  end;
end;

function Modified2Text(Modified: Boolean): string;
begin
  if Modified then Result := sModified else Result := '';
end;

function InsertMode2Text(Mode: Boolean): string;
begin
  if Mode then Result := sInsert else Result := sOverWrite;
end;

function EnumFontFamProc(var LFData: TEnumLogFont; var PFData: TNewTextMetric;
  FontType: Integer; Data: PEnumFontData): Integer; stdcall;
begin
  if Data.Pitch = fpFixed then
  begin
    if LFData.elfLogFont.lfPitchAndFamily and 1 = 1 then
      Data.List.Add(LFData.elfLogFont.lfFaceName);
  end else
    Data.List.Add(LFData.elfLogFont.lfFaceName);
  Result := 1;
end;

procedure AddFontsToList(DC: HDC; L: TStrings; Pitch: TFontPitch);
var
  Data: TEnumFontData;
begin
  Data.List := L;
  Data.Pitch := Pitch;
  EnumFontFamilies(DC, nil, @EnumFontFamProc, Integer(@Data));
end;

procedure InitEnvironment(var Data: TEnvironment);
begin
  with Data.P do
  begin
    Version := IniVersion;
    DropFiles := True;
    GroupUndo := True;
    UndoAfterSave := False;
    ShowAddress := True;
    ShowDigits := True;
    ShowText := True;
    ShowHorzLines := False;
    ShowVertLines := False;
    ShowSeparators := True;
    ShowInactiveCaret := True;
    DisabledDrawStyle := Integer(cDisabledDrawStyleDef);
    AddressMode := Integer(cAddressModeDef);
    AddressSize := cAddressSizeDef;
    CharSpacing := cCharSpacingDef;
    LineSize := cLineSizeDef;
    DigitGrouping := cDigitGroupingDef;
    LineHeightPercent := cLineHeightPercentDef;
    UndoLimit := cUndoLimitDef;
    FontSize := cFontSizeDef;
    FontStyle := cFontStyleDef;
  end;
  with Data do
  begin
    AddressPrefix := cAddressPrefixDef;
    FontName := cFontNameDef;
  end;
end;

procedure InitColors(var Colors: TKColorArray);
var
  I: TKHexEditorColorIndex;
begin
  SetLength(Colors, ciHexEditorColorsMax + 1);
  for I := 0 to Length(Colors) - 1 do
    Colors[I] := GetColorSpec(I).Def;
end;

procedure CopyColors(Src, Dest: TKColorArray);
var
  I: TKHexEditorColorIndex;
begin
  for I := 0 to Min(Length(Src), Length(Dest)) - 1 do
    Dest[I] := Src[I];
end;

initialization
  AppName := 'Hex Editor (Demo)';
  IniPath := ExtractFilePath(Application.ExeName) +  'hexeditor.ini';
  IniVersion := 103
end.
