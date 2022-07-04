{ Object XML storage

  Copyright (C) 2001-2008, written by Mike Shkolnik, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

Sample:

type
  TAppOptions = class(TPersistent)
  private
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
  protected
  public
  published
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
  end;

var
  AppOptions: TAppOptions;

begin
  AppOptions := TAppOptions.Create;
  AppOptions.Left := 100;

  ObjectToXML('d:\options.xml', AppOptions);

  <work with options here>

  XMLToObject('d:\options.xml', AppOptions);
end;

To save/load a form:
  ObjectToXML('d:\form1.xml', Form1);
  XMLToObject('d:\form1.xml', Form1);
}
unit Obj2XML;

interface

{$IFDEF VER100}
  {$DEFINE SMForDelphi3}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE SMForDelphi3}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER185}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
{$ENDIF}

{$IFDEF VER190}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
{$ENDIF}

{$IFDEF VER200}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
    {$DEFINE SMForBCB2009}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
  {$DEFINE SMForDelphi2009}
{$ENDIF}

{$IFDEF SMForDelphi6}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF SMForDelphi7}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

uses Windows, Classes;

procedure ObjectToXML(const FileName: string; Instance: TObject);
procedure XMLToObject(const FileName: string; Instance: TObject);

implementation
uses SysUtils, TypInfo, Graphics
     {$IFDEF SMForDelphi6} , Variants {$ENDIF};

type
  TIntegerSet = set of 0..SizeOf(Integer)*8-1;

function GetSetProp(Instance: TObject; PropInfo: PPropInfo;
  Brackets: Boolean): string;
var
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
  I: Integer;
begin
  Integer(S) := GetOrdProp(Instance, PropInfo);
  TypeInfo := GetTypeData(PropInfo^.PropType^)^.CompType^;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
  if Brackets then
    Result := '[' + Result + ']';
end;

procedure SetEnumProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: string);
var
  Data: Longint;
begin
  Data := GetEnumValue(PropInfo^.PropType^, Value);
  if Data >= 0 then
{  if Data < 0 then
    raise EPropertyConvertError.CreateResFmt(@SInvalidPropertyElement, [Value]);
}  SetOrdProp(Instance, PropInfo, Data);
end;

procedure SetSetProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: string);
var
  Left, EnumName: string;
  Data, EnumValue: Longint;
  EnumInfo: PTypeInfo;

  // grab the next enum name
  function NextWord: string;
  begin
    Result := '';

    // while we are still dealing with non-whitespace
    while not {$IFDEF SMForDelphi2009}CharInSet{$ENDIF}(Left[1] {$IFDEF SMForDelphi2009},{$ELSE} in {$ENDIF} [',', ' ']) do
    begin
      Result := Result + Left[1];
      Delete(Left, 1, 1);
      if Left = '' then
        Exit;
    end;

    // skip any whitespace
    while {$IFDEF SMForDelphi2009}CharInSet{$ENDIF}(Left[1] {$IFDEF SMForDelphi2009},{$ELSE} in {$ENDIF} [',', ' ']) do
      Delete(Left, 1, 1);
  end;

begin
  // bracket reduction
  Left := Value;
  if Left[1] = '[' then
    Delete(Left, 1, 1);
  if Left[Length(Left)] = ']' then
    Delete(Left, Length(Left), 1);

  // loop it dude!
  EnumInfo := GetTypeData(PropInfo^.PropType^)^.CompType^;
  Data := 0;
  while Left <> '' do
  begin
    EnumName := NextWord;
    if EnumName = '' then
      Break;
    EnumValue := GetEnumValue(EnumInfo, EnumName);
    if EnumValue >= 0 then
{    if EnumValue < 0 then
      raise EPropertyConvertError.CreateResFmt(@SInvalidPropertyElement, [EnumName]);
}    Include(TIntegerSet(Data), EnumValue);
  end;
  SetOrdProp(Instance, PropInfo, Data);
end;

function GetPropValue(Instance: TObject; const PropName: string;
  PreferStrings: Boolean): Variant;
var
  PropInfo: PPropInfo;
  TypeData: PTypeData;

  i: Integer;
  ss: TStringStream;
begin
  // assume failure
  Result := Null;

  // get the prop info
  PropInfo := GetPropInfo(Instance.ClassInfo, PropName);
  if PropInfo <> nil then
  begin
    TypeData := GetTypeData(PropInfo^.PropType^);

    // return the right type
    case PropInfo^.PropType^^.Kind of
      tkInteger, tkChar, tkWChar:
        Result := GetOrdProp(Instance, PropInfo);

      tkEnumeration:
        if PreferStrings then
          Result := GetEnumName(PropInfo^.PropType^, GetOrdProp(Instance, PropInfo))
        else
          if TypeData^.BaseType^ = TypeInfo(Boolean) then
            Result := Boolean(GetOrdProp(Instance, PropInfo))
          else
            Result := GetOrdProp(Instance, PropInfo);
      tkSet:
        if PreferStrings then
          Result := GetSetProp(Instance, PropInfo, True)
        else
          Result := GetOrdProp(Instance, PropInfo);

      tkFloat:
          Result := GetFloatProp(Instance, PropInfo);

      tkMethod:
        Result := PropInfo^.PropType^.Name;

      tkString, tkLString, tkWString:
        Result := GetStrProp(Instance, PropInfo);

      tkVariant:
        Result := GetVariantProp(Instance, PropInfo);
//      tkInt64:
//        Result := GetInt64Prop(Instance, PropInfo) + 0.0;

      tkClass: begin
                 i := GetOrdProp(Instance, PropInfo);
                 if (TObject(i) is TStrings) then
                   Result := TStrings(i).Text
                 else
                 if (TObject(i) is TPicture) then
                 begin
                   if Assigned(TPicture(i).Graphic) then
                   begin
                     ss := TStringStream.Create('');
                     try
                       TPicture(i).Graphic.SaveToStream(ss);
                       Result := '[CDATA[' + ss.DataString + ']]'
                     finally
                       ss.Free
                     end
                   end
                 end
                 else
                   Result := i
               end;
    else
//      raise EPropertyConvertError.CreateResFmt(@SInvalidPropertyType,
//                                            [PropInfo.PropType^^.Name]);
    end;
  end;
end;

procedure SetPropValue(Instance: TObject; const strPropertyName, strValue: string);
var
  PropInfo: PPropInfo;
begin
  if not (Instance is TPersistent) then exit;

  PropInfo := GetPropInfo(Instance.ClassInfo, strPropertyName);

  {if such properties exists}
  if Assigned(PropInfo) then
  begin
    case PropInfo^.PropType^^.Kind of
      tkInteger,
      tkChar,
      tkWChar,
      tkClass: SetOrdProp(Instance, PropInfo, VarAsType(strValue, varInteger));
      tkEnumeration:
        if VarType(strValue) = varString then
          SetEnumProp(Instance, PropInfo, VarToStr(strValue));
{        if TypeData^.BaseType^ = TypeInfo(Boolean) then
          SetOrdProp(Instance, PropInfo, VarAsType(strValue, varBoolean))
        else
          SetOrdProp(Instance, PropInfo, VarAsType(strValue, varInteger))}
      tkSet: SetSetProp(Instance, PropInfo, VarToStr(strValue));
      tkFloat: SetFloatProp(Instance, PropInfo, VarAsType(strValue, varDouble));
      tkString,
      tkLString,
      tkWString: SetStrProp(Instance, PropInfo, strValue);
      tkVariant: SetVariantProp(Instance, PropInfo, strValue);
    else
//      raise EPropertyConvertError.CreateResFmt(@SInvalidPropertyType,
//                                            [PropInfo.PropType^^.Name]);
    end;
  end;
end;

procedure ObjectToXML(const FileName: string; Instance: TObject);
var
  fs: TFileStream;

  procedure WriteString(const s: string);
  begin
    fs.Write(s[1], Length(s));
  end;

  procedure WriteObj(Obj: TObject; Level: Integer);
  var
    i, intPropCount: Integer;
    PropList: TPropList;
    strPrefix: string;
  begin
    strPrefix := '';
    for i := 0 to Level-1 do
      strPrefix := strPrefix + '  ';
    try
      WriteString(strPrefix + '<PROPERTIES>'#13#10);

      intPropCount := GetPropList(Obj.ClassInfo, tkProperties, @PropList);
      for i := 0 to intPropCount-1 do
        WriteString(strPrefix + '  <' + PropList[i].Name + '>' +
                                     VarToStr(GetPropValue(Obj, PropList[i].Name, True)) +
                                  '</' + PropList[i].Name + '>'#13#10);

      if (Obj is TComponent) then
      begin
        with TComponent(Obj) do
        begin
          for i := 0 to ComponentCount-1 do
            WriteObj(Components[i], Level+1)
        end
      end
    finally
      WriteString(strPrefix + '</PROPERTIES>'#13#10);
    end
  end;

begin
  if not Assigned(Instance) then exit;

  fs := TFileStream.Create(FileName, fmCreate);
  try
    WriteString('<?xml version="1.0" encoding="windows-' + IntToStr(GetACP()) + '" standalone="yes"?>'#13#10 +
                 '<!-- Generated by Scalabium OBJ2XML-->'#13#10);

    WriteObj(Instance, 0)
  finally
    fs.Free
  end;
end;

procedure XMLToObject(const FileName: string; Instance: TObject);
const
  TagBegin = '<';
  TagEnd = '>';
  TagSlash = '/';
  PropData = 'PROPERTIES';

  procedure SetTextStr(const Value: string; intLength: Integer);
  var
    P: PChar;
    s, sTrimmed, strPropName, strPropValue: string;
    IsRowData, IsMetaData, IsPropName: Boolean;
    i: Integer;
  begin
    P := Pointer(Value);
    IsRowData := False;
    s := '';
    while (P <> nil) and (P^ <> #0) do
    begin
      if not {$IFDEF SMForDelphi2009}CharInSet{$ENDIF}(P^ {$IFDEF SMForDelphi2009},{$ELSE} in {$ENDIF}[#13, #10]) then
      begin
        s := s + P^;
        if P^ = TagEnd then
        begin
          sTrimmed := Trim(s);

          if IsRowData or
             (sTrimmed = TagBegin + PropData + TagEnd) then
          begin
            IsRowData := True;
          end;

          if IsRowData and (sTrimmed = TagBegin + TagSlash + PropData + TagEnd) then
            IsRowData := False;

        end
      end
      else
      begin
        if (s <> '') and
           IsRowData and
           (sTrimmed <> TagBegin + PropData + TagEnd) and
           (sTrimmed <> TagBegin + TagSlash + PropData + TagEnd) then
        begin
          {<Left>200</Left>}
          IsMetaData := True;
          IsPropName := False;
          strPropName := '';
          strPropValue := '';
          for i := 1 to Length(s) do
            if IsMetaData then
            begin
              if (s[i] = TagEnd) then
              begin
                IsMetaData := False;
                IsPropName := False;
              end;
              if IsPropName then
                strPropName := strPropName + s[i];
              if (s[i] = TagBegin) then
                IsPropName := True;
            end
            else
              if (s[i] = TagBegin) or (s[i] = TagEnd) then
                break
              else
              begin
                strPropValue := strPropValue + s[i]
              end;

          if strPropName <> '' then
            SetPropValue(Instance, strPropName, strPropValue);
        end;
        s := ''

      end;
      Inc(P)
    end;
  end;


var
  fs: TFileStream;

  intSize: Integer;
  s: string;
begin
  if not Assigned(Instance) then exit;

  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    intSize := fs.Size - fs.Position;
    SetString(s, nil, intSize);
    fs.Read(Pointer(s)^, intSize);
    SetTextStr(s, intSize);
    if (intSize > 0) and (s[intSize] = #27) then
      SetLength(s, intSize-1)
  finally
    fs.Free
  end;
end;

end.
