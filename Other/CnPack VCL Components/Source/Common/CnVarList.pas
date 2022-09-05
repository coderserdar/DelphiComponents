{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnVarList;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ������б���
* ��Ԫ���ߣ��簲������zzzl��
* ��    ֲ��Chide Ng
*           Liu Xiao
* ��    ע���õ�Ԫʵ����һ�����б��ࣨ������CList�����ܽ������������ͻ����ʵ��
            ������б��У�����ת��Ϊ�ַ�������֮Ҳ֧�ִ��ַ����л�ԭ�б����ݣ�
            ���л�ԭ������֧�ֶ������͡�
* ����ƽ̨��PWin2K SP3 + Delphi 7
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 6/7 C++Builder 6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.05.16
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Windows, Classes, Math{$IFDEF COMPILER6_UP}, Variants, StrUtils{$ENDIF};

type
  TCnVars = array of Variant;

  TCnVarList = class(TObject)
  private
    FValues: TCnVars;
    FValType: TCnVars;
    function GetCount: Integer;
    function Get(Index: Integer): Variant;
  protected
    function GetString(cList : TCnVarList): WideString;
    procedure DynArrayDelete(var A; elSize: Longint; Index, Count: Integer); virtual;
    function GetList(Index: Integer): TCnVarList;
    {* ����� I ���� CnVarList���򷵻ش˷�װ�� List, �ⲿʹ����Ϻ����ͷš�}
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetValues(const AValues: array of Variant);
    procedure Clear;
    procedure Add(AValue: Variant); overload;
    {* ��һ������������ӵ��б��У�����������һ�� VarArray}
    procedure Add(AValue: TCnVarList); overload;
    {* ��һ�������б���ӵ��б��У�ʵ�����ǽ�����һ�������б��е� Values ��ӽ���}
    procedure Add(AValue: TObject); overload;
    {* ���һ��ͨ����ע������޷����ַ�����ԭ}
    procedure Remove(Index: Integer);
    {* ɾ�� Index λ����ָ��ֵ}
    function GetType(Index: Integer): string;
    {* ����б�ĳλ�õı���Ķ�Ӧ���͵��ַ���}
    function GetObject(Index: Integer): TObject;
    {* ����б�ĳλ�õĶ���ʵ����������Ƕ����򷵻� nil}
    function ToString: WideString;
    {* �������б�ת�����ַ���������Ƕ��}
    function FromString(Text: WideString; var Error: string): Boolean;
    {* ���ַ����лָ������б����ʽ�����򷵻� False��������Ϣ�� Error ��}
    
    property Count: Integer read GetCount;
    {* �б�Ԫ������}
    property Items[Index: Integer]: Variant read Get;
    {* ���� Index ��ñ���ֵ}
    property Values: TCnVars read FValues write FValues;
    {* ��������}
    property ValType: TCnVars read FValType write FValType;
    {* ��������������}
  end;

implementation

{$IFDEF DEBUG}
uses
  CnDebug;
{$ENDIF}

{$IFNDEF COMPILER6_UP}
type
  TVarType = Word;

function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

function StrToBool(const S: string): Boolean;
begin
  if UpperCase(S) = 'TRUE' then
    Result := True
  else if UpperCase(S) = 'FALSE' then
    Result := False
  else
    raise EConvertError.CreateFmt('Invalid Boolean Value: %s', [S]);
end;

function AnsiDequotedStr(const S: string; AQuote: Char): string;
var
  LText: PChar;
begin
  LText := PChar(S);
  Result := AnsiExtractQuotedStr(LText, AQuote);
  if Result = '' then
    Result := S;
end;

const
  cSimpleBoolStrs: array [Boolean] of string = ('False', 'True');

function BoolToStr(B: Boolean; UseBoolStrs: Boolean): string;
begin
  Result := cSimpleBoolStrs[B];
end;

function VarTypeAsText(const AType: TVarType): string;
const
  CText: array [varEmpty..varByte] of string = ('Empty', 'Null', 'Smallint', //Do not localize
    'Integer', 'Single', 'Double', 'Currency', 'Date', 'OleStr', 'Dispatch', //Do not localize
    'Error', 'Boolean', 'Variant', 'Unknown', 'Decimal', '$0F', 'ShortInt', //Do not localize
    'Byte'); //Do not localize
begin
  if AType and varTypeMask <= varByte then
    Result := CText[AType and varTypeMask]
  else if AType = varString then
    Result := 'String' //Do not localize
{$IFDEF UNICODE}
  else if AType = varUString then
    Result := 'UString' //Do not localize
{$ENDIF}
  else if AType = varAny then
    Result := 'Any' //Do not localize
  else
    Result := HexDisplayPrefix + IntToHex(AType and varTypeMask, 4);

  if AType and varArray <> 0 then
    Result := 'Array ' + Result; //Do not localize
  if AType and varByRef <> 0 then
    Result := 'ByRef ' + Result; //Do not localize
end;

{$ENDIF}

var
  FVarList: TList = nil;

{ TCnVarList }

procedure TCnVarList.Add(AValue: TCnVarList);
begin
  if (AValue = nil) or (AValue = Self) then Exit;
  SetLength(FValues, Length(FValues) + 1);
  SetLength(FValType, Length(FValType) + 1);
  FValues[High(FValues)] := AValue.FValues;
  FValType[High(FValType)] := AValue.FValType;
end;

procedure TCnVarList.Add(AValue: Variant);
begin
  SetLength(FValues, Length(FValues)+ 1);
  SetLength(FValType, Length(FValType)+ 1);
  FValues[High(FValues)] := AValue;
  FValType[High(FValType)] := VarTypeAsText(VarType(AValue));
end;

procedure TCnVarList.Add(AValue: TObject);
begin
  if not Assigned(Avalue) then Exit;
  SetLength(FValues, Length(FValues)+ 1);
  SetLength(FValType, Length(FValType)+ 1);
  FValues[High(FValues)] := Integer(AValue);
  FValType[High(FValType)] := 'Object:' + AValue.ClassName;
end;

procedure TCnVarList.Clear;
begin
  SetLength(FValues, 0);
  SetLength(FValType, 0);
end;

{
  A �������ͣ�elSize = SizeOf(A) Index ��ʼɾ����λ������ ��Count ɾ��������
}
constructor TCnVarList.Create;
begin
  FVarList.Add(Self);
end;

destructor TCnVarList.Destroy;
begin
  FVarList.Remove(Self);
  inherited;
end;

procedure TCnVarList.DynArrayDelete(var A; elSize, Index, Count: Integer);
var
  Len, MaxDelete: Integer;
  P : PLongint;
begin
  P := PLongint(A);
  if P = nil then
   Exit;
{ ���������ȫ��ͬ�� Dec(P) ; Len := P^
  ��Ϊ Dec(P) = Pchar(P) �C 4  ͬ�����ƶ�4 �ֽڵ�ƫ������ֻ�������߰��ֽ����ƶ�    }
  Len := PLongint(PChar(P) - 4)^; // �����ĳ��� ��ƫ���� -4
  if Index >= Len then //Ҫɾ����λ�ó�����Χ���˳�
   Exit;

  MaxDelete := Len - Index; // ���ɾ��������
  Count := Min(Count, MaxDelete); // ȡ��һ����Сֵ
  if Count = 0 then  Exit;
  Dec(Len, Count);// �ƶ���Ҫɾ����λ��
  MoveMemory(PChar(P)+ Index * elSize,
             PChar(P)+ (Index + Count) * elSize ,
             (Len- Index) * elSize); //�ƶ��ڴ�
  Dec(P);  //�Ƴ� �����鳤�ȡ�λ��
  Dec(P);  //�Ƴ������ü����� λ��
  //�����ٷ�������ڴ�,Len �µĳ���. Sizeof(Longint) * 2 = 2*Dec(P)
  ReallocMem(P, Len * elSize + Sizeof(Longint) * 2);
  Inc(P); // ָ�����鳤��
  P^ := Len; // new length
  Inc(P); // ָ������Ԫ�أ���ʼ��λ��
  PLongint(A) := P;
end;

type
  TCharState = (csUnknown,
                csBracketLeft,
                csBracketRight,  //��������
                csComma,
                csSq,        //������
                csInteger, csString, csBoolean);

function TCnVarList.FromString(Text: WideString;
  var Error: string): Boolean;
  
  function ParseText(Text: WideString): TCnVars;
  var
    I: Integer;
    State: TCharState;
    Num: Integer;
    Element: WideString;
    eType: TVarType;
    C: WideChar;
    sqCount : Integer;
    //ÿ��Ԫ�ر�ʾΪһ���㡣��0��Ԫ��Ϊself,��1��Ϊself�µĵ�һ����List
    List : TList;
    cvl : TCnVarList;

    procedure _PushElement(); //��Elementװ�뵱ǰList�У������Element
    var
      v: Variant;
    begin
      case eType of
        varString {$IFDEF UNICODE}, varUString {$ENDIF}:  v := AnsiDequotedStr(Element, '''');
        varInteger: v := StrToInt(Element);
        varDouble:  v := StrToFloat(Element);
        varBoolean: v := StrToBool(Element);
      end;

      if List.Count > 0 then
      begin
        TCnVarList(List[List.Count - 1]).Add(v);
{$IFDEF DEBUG}
        CnDebugger.LogFmt('Add Value %d VarList: %s.', [List.Count - 1,
          TCnVarList(List[List.Count - 1]).ToString]);
{$ENDIF}
      end;
      Element := '';
    end;

    procedure _NewLayer(); 
    begin
      cvl := TCnVarList.Create;
      List.Add(cvl);
    end;

    procedure _PopLayer();
    var
      L : SmallInt;
    begin
      L := List.Count - 1;
      if L <= 0 then Exit;
{$IFDEF DEBUG}
      CnDebugger.LogFmt('The %d VarList Before Add: %s.', [L - 1, TCnVarList(List[L - 1]).ToString]);
      CnDebugger.LogFmt('The %d VarList Before Add: %s.', [L, TCnVarList(List[L]).ToString]);
{$ENDIF}
      TCnVarList(List[L - 1]).Add(TCnVarList(List[L]));
{$IFDEF DEBUG}
      CnDebugger.LogFmt('The %d VarList After Add: %s.', [L - 1, TCnVarList(List[L - 1]).ToString]);
{$ENDIF}
      TCnVarList(List[L]).Free;
{$IFDEF DEBUG}
      CnDebugger.LogFmt('The %d VarList After Free: %s.', [L - 1, TCnVarList(List[L - 1]).ToString]);
{$ENDIF}
      List.Delete(L);
    end;

    procedure _WantNewElement(); //��Text��Iλ�ÿ�ʼ����Ϊ��һ���µ�Ԫ�ؿ�ʼ����
    begin
      if C = '''' then
      begin
        Element := '''';
        eType := varString;
        State := csSq;
      end
      else if TryStrToInt(C, Num) then
      begin
        Element := C;
        eType := varInteger;
        State := csInteger;
      end
      else if (C = 't') or (C = 'T') or (C = 'f') or (C = 'F') then
      begin
        Element := C;
        eType := varBoolean;
        State := csBoolean;
      end
      else if C = '(' then
        _NewLayer;
    end;
  begin
    State := csUnknown;
    sqCount := 0;
    for I := 1 to Length(Text) do
    begin
      C := Text[I];
      case State of
        csUnknown:
          begin
            if C = '(' then
            begin // �ҵ���ʼ��
              State := csBracketLeft;
              List := TList.Create;
              List.Add(Self);
            end;
          end;
        csBracketLeft:
          begin
            if C = ')' then
            begin
              if Element <> '' then
                _PushElement;

              if List.Count > 0 then
                _PopLayer;

              State := csBracketRight;
              end
            else
              if C = '(' then
              begin
                _NewLayer;
              end
              else
                _WantNewElement;
          end;
        csBracketRight:
          begin
            if Element <> '' then
              _PushElement;
            if List.Count > 0 then
                _PopLayer;
            if C = ',' then
              State := csComma
            else
              _WantNewElement;
            end;
        csComma:
          begin
            if Element <> '' then
              _PushElement;
            _WantNewElement;
          end;
        csSq:
          begin
            Element := Element + C;
            State := csString;
          end;
        csString:
          begin
            if C = ')' then
            begin
              if List.Count > 0 then
                _PopLayer;
              State := csBracketRight;
            end
            else if C = ',' then
              if sqCount mod 2 = 0 then
              begin
                State := csComma;
                sqCount := 0;
              end
              else
                Element := Element + C
            else
              Element := Element + C
          end;
        csInteger:
          begin
            if C = ',' then
              State := csComma
            else if C = ')' then
            begin
              if Element <> '' then
                _PushElement;
              if List.Count > 0 then
                _PopLayer;
              State := csBracketRight;
            end
            else if C = '.' then
            begin
              Element := Element + '.';
              eType := varDouble;
              end
            else if TryStrToInt(C, Num) then
            begin
              Element := Element + C;
            end
          end;
        csBoolean:
          begin
            if C = ')' then
            begin
              _PushElement;
              if List.Count > 0 then
                _PopLayer;
              State := csBracketRight;
            end
            else if C = ',' then
              State := csComma
            else
              Element := Element + C;
          end;
        end;
      end;
  end;
begin
  Clear;
  try
    ParseText(Text);
    Result := True;
  except
    on E: Exception do
    begin
      Error := E.Message;
      Result := False;
    end;
  end;
end;

function TCnVarList.Get(Index: Integer): Variant;
begin
  Result := FValues[Index];
end;

function TCnVarList.GetCount: Integer;
begin
  Result := Length(FValues);
end;

function TCnVarList.GetList(Index: Integer): TCnVarList;
var
  I: Integer;
  vVal, vType: Variant;
begin
  vVal := FValues[Index];
  if VarIsArray(vVal) then
  begin
    Result := TCnVarList.Create;
    vType := FValType[Index];
    SetLength(Result.FValues, VarArrayHighBound(vVal, 1) + 1);
    SetLength(Result.FValType, VarArrayHighBound(vType, 1) + 1);
    for I := VarArrayLowBound(vVal, 1) to VarArrayHighBound(vVal, 1) do
    begin
      Result.FValues[I] := vVal[I];
      Result.FValType[I] := vType[I];
    end;
  end
  else
    Result := nil;
end;

function TCnVarList.GetObject(Index: Integer): TObject;
var
  v: Variant;
begin
  Result := nil;      
  v := FValues[Index];
  if VarIsArray(v) then
    Exit;
  if Pos('Object', FValType[Index]) = 1 then
    Result := TObject(Integer(v));
end;

function TCnVarList.GetString(cList: TCnVarList): WideString;
var
  I: Integer;
  v: Variant;
  aList: TCnVarList;
begin
  Result := '';
  if not Assigned(cList) then Exit;
  Result := '(';
  for I := 0 to High(cList.Values) do
  begin
    v := cList.Values[I];
    if VarArrayDimCount(v) > 0 then
    begin
      // ���Ԫ�ر������vararray��������CnVarList���ģ���ݹ������չ֮��
      aList := cList.GetList(I);
      Result := Result + GetString(aList);
      FreeAndNil(aList);
    end
    else
    begin
      if Pos('Object', cList.ValType[I]) = 1 then
        Result := Result + QuotedStr(cList.ValType[I])
      else
        case VarType(v) of
          varString, {$IFDEF UNICODE} varUString, {$ENDIF} varOleStr:
            Result := Result + QuotedStr(v);
          varByte, {$IFDEF COMPILER6_UP}varShortInt,{$ENDIF} varSmallint,
          varInteger, varSingle, varDouble,
          {$IFDEF COMPILER6_UP}varWord, varLongWord, varInt64, {$ENDIF}
          varCurrency:
            Result := Result + VarToStr(v);
          varDate:
            Result := Result + '''' + DateTimeToStr(VarToDateTime(v)) + '''';
          varBoolean:
            Result := Result + BoolToStr(v, True);
          varVariant:
            Result := Result + QuotedStr(v);
        else
          Result := Result + 'Unknown:' + cList.ValType[I];
        end;
    end;
    if I < High(cList.Values) then
      Result := Result + ',';
  end;
  Result := Result + ')';
end;

function TCnVarList.GetType(Index: Integer): string;
begin
  Result := FValType[Index];
end;

procedure TCnVarList.Remove(Index: Integer);
begin
  DynArrayDelete(FValues, Length(FValues), Index, 1);
  DynArrayDelete(FValType, Length(FValType), Index, 1);
end;

procedure TCnVarList.SetValues(const AValues: array of Variant);
var
  I: Integer;
begin
  SetLength(FValues, Length(AValues));
  SetLength(FValType, Length(AValues));
  for I := Low(AValues) to High(AValues) do
  begin
    FValues[I] := AValues[I];
    FValType[I] := VarTypeAsText(VarType(AValues[I]));
  end;
end;

function TCnVarList.ToString: WideString;
begin
  Result := GetString(Self);
end;

procedure CleanVarList;
var
  I: Integer;
begin
  if FVarList <> nil then
    for I := 0 to FVarList.Count - 1 do
      TCnVarList(FVarList[I]).Free;
end;

initialization
  FVarList := TList.Create;

finalization
  CleanVarList;
  FreeAndNil(FVarList);

end.
