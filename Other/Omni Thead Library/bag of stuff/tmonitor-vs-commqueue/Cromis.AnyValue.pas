(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2006-2010 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 * - Neither the name of the Iztok Kacin nor the names of its contributors may be
 *   used to endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ==================================================================================
 * Common unit for Cromis library
 * ==================================================================================
 * 08/03/2010 (1.0.0)
 *   - Initial release
 *   - IAnyValue, TAnyValue implementations
 * ==================================================================================
 * 26/07/2010 (1.0.1)
 *   - Added avtDateTime
 * ==================================================================================
 * 15/08/2010 (1.0.2)
 *   - Added Items property to enable iteration over all values
 *   - Added Count property
 * ==================================================================================
 * 08/10/2010 (1.0.3)
 *   - Use generic list instead of interface list for newer compilers
 * ==================================================================================
 * 28/12/2010 (1.0.4)
 *   - Create new interface object for each assignment to avoid interface sharing
 * ==================================================================================
*)
unit Cromis.AnyValue;

interface

uses
  SysUtils, Classes {$IF CompilerVersion >= 20}, Generics.Collections {$IFEND};

type
  TValueType = (avtNull, avtBoolean, avtInteger, avtInt64, avtCardinal, avtFloat, avtString,
                avtObject, avtPointer, avtInterface, avtWideString, avtDateTime);


  IAnyValue = Interface(IInterface)
  ['{9D866D8B-6FEC-4633-B968-AF8677AF6B40}']
    function GetName: string;
    function GetAsInt64: Int64;
    function GetAsFloat: Extended;
    function GetAsString: string;
    function GetAsObject: TObject;
    function GetAsInteger: Integer;
    function ValueType: TValueType;
    function GetAsBoolean: Boolean;
    function GetAsPointer: Pointer;
    function GetAsCardinal: Cardinal;
    function GetAsDateTime: TDateTime;
    function GetAsInterface: IInterface;
    function GetAsWideString: WideString;
    procedure SetName(const Value: string);
    procedure SetAsString(const Value: string);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsFloat(const Value: Extended);
    procedure SetAsObject(const Value: TObject);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsPointer(const Value: Pointer);
    procedure SetAsCardinal(const Value: Cardinal);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsInterface(const Value: IInterface);
    procedure SetAsWideString(const Value: WideString);
    property Name: string read GetName write SetName;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsString: string read GetAsString write SetAsString;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    property AsCardinal: Cardinal read GetAsCardinal write SetAsCardinal;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsInterface: IInterface read GetAsInterface write SetAsInterface;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
  end;

  IValueList = Interface(IInterface)
  ['{54B01683-17B0-4719-B620-48FDF31BC574}']
    function GetCount: Integer;
    function GetItems(const Index: Integer): IAnyValue;
    function Get(const Name: string): IAnyValue;
    function Exists(const Name: string): Boolean;
    function Ensure(const Name: string): IAnyValue;
    property Items[const Index: Integer]: IAnyValue read GetItems;
    property Count: Integer read GetCount;
    procedure Clear;
  end;

  TAnyValue = packed record
  private
    FData: IAnyValue;
    procedure EnsureValueObject;
    function  GetAsBoolean: Boolean;
    function  GetAsCardinal: Cardinal;
    function  GetAsFloat: Extended;
    function  GetAsInt64: Int64;
    function  GetAsInteger: Integer;
    function  GetAsInterface: IInterface;
    function  GetAsObject: TObject;
    function  GetAsString: string;
    function  GetAsDateTime: TDateTime;
    function  GetAsWideString: WideString;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsCardinal(const Value: Cardinal);
    procedure SetAsFloat(const Value: Extended);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsInterface(const Value: IInterface);
    procedure SetAsObject(const Value: TObject);
    procedure SetAsString(const Value: string);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsWideString(const Value: WideString);
  public
    procedure Clear; inline;
    function Data: IAnyValue;
    function IsNull: Boolean;
    function IsEmpty: Boolean;
    function ValueType: TValueType;
    class function Null: TAnyValue; static;
    class operator Implicit(const Value: Boolean): TAnyValue;
    class operator Implicit(const Value: Extended): TAnyValue;
    class operator Implicit(const Value: Integer): TAnyValue;
    class operator Implicit(const Value: Int64): TAnyValue;
    class operator Implicit(const Value: string): TAnyValue;
    class operator Implicit(const Value: IInterface): TAnyValue;
    class operator Implicit(const Value: WideString): TAnyValue;
    class operator Implicit(const Value: TObject): TAnyValue;
    class operator Implicit(const Value: TDateTime): TAnyValue;
    class operator Implicit(const Value: TAnyValue): Int64;
    class operator Implicit(const Value: TAnyValue): TObject;
    class operator Implicit(const Value: TAnyValue): Extended;
    class operator Implicit(const Value: TAnyValue): string;
    class operator Implicit(const Value: TAnyValue): Integer;
    class operator Implicit(const Value: TAnyValue): WideString;
    class operator Implicit(const Value: TAnyValue): Boolean;
    class operator Implicit(const Value: TAnyValue): IInterface;
    class operator Implicit(const Value: TAnyValue): TDateTime;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCardinal: Cardinal read GetAsCardinal write SetAsCardinal;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsInterface: IInterface read GetAsInterface write SetAsInterface;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsString: string read GetAsString write SetAsString;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
  end;

  // acquire function that returns the interface
  function AcquireValueList: IValueList;
  function AcquireValue: IAnyValue;

implementation

type
  TAnyValueObject = class(TInterfacedObject, IAnyValue)
  private
    FName: string;
    FValue: TVarRec;
    FValueType: TValueType;
    function GetName: string;
    function GetAsInt64: Int64;
    function GetAsFloat: Extended;
    function GetAsString: string;
    function GetAsObject: TObject;
    function GetAsInteger: Integer;
    function GetAsBoolean: Boolean;
    function GetAsPointer: Pointer;
    function GetAsCardinal: Cardinal;
    function GetAsDateTime: TDateTime;
    function GetAsInterface: IInterface;
    function GetAsWideString: WideString;
    procedure SetName(const Value: string);
    procedure SetAsString(const Value: string);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsFloat(const Value: Extended);
    procedure SetAsObject(const Value: TObject);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsPointer(const Value: Pointer);
    procedure SetAsCardinal(const Value: Cardinal);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsInterface(const Value: IInterface);
    procedure SetAsWideString(const Value: WideString);
  public
    constructor Create;
    destructor Destroy; override;
    function ValueType: TValueType;
    property Name: string read GetName write SetName;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsString: string read GetAsString write SetAsString;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    property AsCardinal: Cardinal read GetAsCardinal write SetAsCardinal;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsInterface: IInterface read GetAsInterface write SetAsInterface;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
  end;

  TValueList = class(TInterfacedObject, IValueList)
  private
  {$IF CompilerVersion >= 20}
    FValuesList: TList<IInterface>;
  {$ELSE}
    FValuesList: TInterfaceList;
  {$IFEND}
    function GetItems(const Index: Integer): IAnyValue;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Get(const Name: string): IAnyValue;
    function Ensure(const Name: string): IAnyValue;
    function Exists(const Name: string): Boolean;
    property Items[const Index: Integer]: IAnyValue read GetItems;
    property Count: Integer read GetCount;
    procedure Clear;
  end;

  function AcquireValueList: IValueList;
  begin
    Result := TValueList.Create;
  end;

  function AcquireValue: IAnyValue;
  begin
    Result := TAnyValueObject.Create;
  end;

{ TTaskValue }

constructor TAnyValueObject.Create;
begin
  FValueType := avtNull;
end;

destructor TAnyValueObject.Destroy;
begin
  case FValueType of
    avtString: FreeMem({$IFDEF UNICODE}FValue.VPWideChar{$ELSE}FValue.VPChar{$ENDIF});
    avtInterface: IUnknown(FValue.VInterface) := nil;
    avtWideString: FreeMem(FValue.VPWideChar);
    avtDateTime: FreeMem(FValue.VExtended);
    avtFloat: FreeMem(FValue.VExtended);
    avtInt64: FreeMem(FValue.VInt64);
  end;

  inherited;
end;

function TAnyValueObject.GetAsBoolean: Boolean;
begin
  Result := FValue.VBoolean;
end;

function TAnyValueObject.GetAsCardinal: Cardinal;
begin
  Result := FValue.VInt64^;
end;

function TAnyValueObject.GetAsDateTime: TDateTime;
begin
  Result := FValue.VExtended^;
end;

function TAnyValueObject.GetAsFloat: Extended;
begin
  Result := FValue.VExtended^;
end;

function TAnyValueObject.GetAsInt64: Int64;
begin
  Result := FValue.VInt64^;
end;

function TAnyValueObject.GetAsInteger: Integer;
begin
  Result := FValue.VInteger;
end;

function TAnyValueObject.GetAsInterface: IInterface;
begin
  Result := IUnknown(FValue.VInterface);
end;

function TAnyValueObject.GetAsObject: TObject;
begin
  Result := FValue.VObject;
end;

function TAnyValueObject.GetAsPointer: Pointer;
begin
  Result := FValue.VPointer;
end;

function TAnyValueObject.GetAsString: string;
begin
  case FValueType of
    avtNull:       Result := '';
    avtBoolean:    Result := BoolToStr(AsBoolean, True);
    avtInteger:    Result := IntToStr(AsInteger);
    avtFloat:      Result := FloatToStr(AsFloat);
    avtWideString: Result := AsWideString;
    avtString:
      begin
        {$IFDEF UNICODE}
          Result := FValue.VPWideChar;
        {$ELSE}
          Result := FValue.VPChar;
        {$ENDIF}
      end
    else raise Exception.Create('Value cannot be converted to string');
  end;
end;

function TAnyValueObject.GetAsWideString: WideString;
begin
  case FValueType of
    avtNull:       Result := '';
    avtBoolean:    Result := BoolToStr(AsBoolean, True);
    avtInteger:    Result := IntToStr(AsInteger);
    avtFloat:      Result := FloatToStr(AsFloat);
    avtWideString: Result := FValue.VPWideChar;
    avtString:     Result := AsString;
    else raise Exception.Create('Value cannot be converted to string');
  end;
end;

function TAnyValueObject.GetName: string;
begin
  Result := FName;
end;

procedure TAnyValueObject.SetAsBoolean(const Value: Boolean);
begin
  FValue.VBoolean := Value;
  FValueType := avtBoolean;
end;

procedure TAnyValueObject.SetAsCardinal(const Value: Cardinal);
begin
  GetMem(FValue.VInt64, SizeOf(Int64));
  FValue.VInt64^ := Value;
  FValueType := avtCardinal;
end;

procedure TAnyValueObject.SetAsDateTime(const Value: TDateTime);
begin
  GetMem(FValue.VExtended, SizeOf(Extended));
  FValue.VExtended^ := Value;
  FValueType := avtDateTime;
end;

procedure TAnyValueObject.SetAsFloat(const Value: Extended);
begin
  GetMem(FValue.VExtended, SizeOf(Extended));
  FValue.VExtended^ := Value;
  FValueType := avtFloat;
end;

procedure TAnyValueObject.SetAsInt64(const Value: Int64);
begin
  GetMem(FValue.VInt64, SizeOf(Int64));
  FValue.VInt64^ := Value;
  FValueType := avtInt64;
end;

procedure TAnyValueObject.SetAsInteger(const Value: Integer);
begin
  FValue.VInteger := Value;
  FValueType := avtInteger;
end;

procedure TAnyValueObject.SetAsInterface(const Value: IInterface);
begin
  IUnknown(FValue.VInterface) := IUnknown(Value);
  FValueType := avtInterface;
end;

procedure TAnyValueObject.SetAsObject(const Value: TObject);
begin
  FValue.VObject := Value;
  FValueType := avtObject;
end;

procedure TAnyValueObject.SetAsPointer(const Value: Pointer);
begin
  FValue.VPointer := Value;
  FValueType := avtPointer;
end;

procedure TAnyValueObject.SetAsString(const Value: string);
begin
  {$IFDEF UNICODE}
    FValueType := avtWideString;
    GetMem(FValue.VPWideChar, (Length(Value) + 1) * SizeOf(Char));
    StrCopy(FValue.VPWideChar, PChar(Value));
  {$ELSE}
    FValueType := avtString;
    GetMem(FValue.VPChar, (Length(Value) + 1) * SizeOf(Char));
    StrCopy(FValue.VPChar, PChar(Value));
  {$ENDIF}
end;

procedure TAnyValueObject.SetAsWideString(const Value: WideString);
begin
  FValueType := avtWideString;
  GetMem(FValue.VPWideChar, Length(Value) * SizeOf(WideChar));
  Move(Value[1], FValue.VPWideChar, Length(Value) * SizeOf(WideChar));
end;

procedure TAnyValueObject.SetName(const Value: string);
begin
  FName := Value;
end;

function TAnyValueObject.ValueType: TValueType;
begin
  Result := FValueType;
end;

{ TTaskValues }

procedure TValueList.Clear;
begin
  FValuesList.Clear;
end;

constructor TValueList.Create;
begin
{$IF CompilerVersion >= 20}
  FValuesList := TList<IInterface>.Create;
{$ELSE}
  FValuesList := TInterfaceList.Create;
{$IFEND}
end;

destructor TValueList.Destroy;
begin
  FreeAndNil(FValuesList);

  inherited;
end;

function TValueList.Get(const Name: string): IAnyValue;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to FValuesList.Count - 1 do
  begin
    if SameText(Name, IAnyValue(FValuesList[I]).Name) then
    begin
      Result := IAnyValue(FValuesList[I]);
      Exit;
    end;
  end;
end;

function TValueList.GetCount: Integer;
begin
  Result := FValuesList.Count;
end;

function TValueList.GetItems(const Index: Integer): IAnyValue;
begin
  Result := IAnyValue(FValuesList.Items[Index]);
end;

function TValueList.Ensure(const Name: string): IAnyValue;
begin
  Result := Get(Name);

  if Result = nil then
  begin
    Result := TAnyValueObject.Create;
    FValuesList.Add(Result);
    Result.Name := Name;
  end;
end;

function TValueList.Exists(const Name: string): Boolean;
begin
  Result := Get(Name) <> nil;
end;

{ TAnyValue }

procedure TAnyValue.Clear;
begin
  FData := nil;
end;

function TAnyValue.Data: IAnyValue;
begin
  Result := FData;
end;

procedure TAnyValue.EnsureValueObject;
begin
  FData := TAnyValueObject.Create;
end;

function TAnyValue.GetAsBoolean: Boolean;
begin
  Result := FData.AsBoolean;
end;

function TAnyValue.GetAsCardinal: Cardinal;
begin
  Result := FData.AsCardinal;
end;

function TAnyValue.GetAsDateTime: TDateTime;
begin
  Result := FData.AsDateTime;
end;

function TAnyValue.GetAsFloat: Extended;
begin
  Result := FData.AsFloat;
end;

function TAnyValue.GetAsInt64: Int64;
begin
  Result := FData.AsInt64;
end;

function TAnyValue.GetAsInteger: Integer;
begin
  Result := FData.AsInteger;
end;

function TAnyValue.GetAsInterface: IInterface;
begin
  Result := FData.AsInterface;
end;

function TAnyValue.GetAsObject: TObject;
begin
  Result := FData.AsObject;
end;

function TAnyValue.GetAsString: string;
begin
  Result := FData.AsString;
end;

function TAnyValue.GetAsWideString: WideString;
begin
  Result := FData.AsWideString;
end;

class operator TAnyValue.Implicit(const Value: string): TAnyValue;
begin
  Result.AsString := Value;
end;

class operator TAnyValue.Implicit(const Value: Int64): TAnyValue;
begin
  Result.AsInt64 := Value;
end;

class operator TAnyValue.Implicit(const Value: TObject): TAnyValue;
begin
  Result.AsObject := Value;
end;

class operator TAnyValue.Implicit(const Value: IInterface): TAnyValue;
begin
  Result.AsInterface := Value;
end;

class operator TAnyValue.Implicit(const Value: Boolean): TAnyValue;
begin
  Result.AsBoolean := Value;
end;

class operator TAnyValue.Implicit(const Value: Integer): TAnyValue;
begin
  Result.AsInteger := Value;
end;

class operator TAnyValue.Implicit(const Value: Extended): TAnyValue;
begin
  Result.AsFloat := Value;
end;

class operator TAnyValue.Implicit(const Value: TAnyValue): WideString;
begin
  Result := Value.AsWideString;
end;

class operator TAnyValue.Implicit(const Value: TAnyValue): Integer;
begin
  Result := Value.AsInteger;
end;

class operator TAnyValue.Implicit(const Value: TAnyValue): Boolean;
begin
  Result := Value.AsBoolean;
end;

class operator TAnyValue.Implicit(const Value: WideString): TAnyValue;
begin
  Result.AsWideString := Value;
end;

class operator TAnyValue.Implicit(const Value: TDateTime): TAnyValue;
begin
  Result.AsDateTime := Value;
end;

function TAnyValue.IsEmpty: Boolean;
begin
  Result := FData = nil;
end;

function TAnyValue.IsNull: Boolean;
begin
  Result := FData = nil;
end;

class function TAnyValue.Null: TAnyValue;
begin
  Result.Clear;
end;

class operator TAnyValue.Implicit(const Value: TAnyValue): IInterface;
begin
  Result := Value.AsInterface;
end;

class operator TAnyValue.Implicit(const Value: TAnyValue): TObject;
begin
  Result := Value.AsObject;
end;

class operator TAnyValue.Implicit(const Value: TAnyValue): Int64;
begin
  Result := Value.AsInt64;
end;

class operator TAnyValue.Implicit(const Value: TAnyValue): string;
begin
  Result := Value.AsString;
end;

class operator TAnyValue.Implicit(const Value: TAnyValue): Extended;
begin
  Result := Value.AsFloat;
end;

class operator TAnyValue.Implicit(const Value: TAnyValue): TDateTime;
begin
  Result := Value.AsDateTime;
end;

procedure TAnyValue.SetAsBoolean(const Value: Boolean);
begin
  EnsureValueObject;
  FData.AsBoolean := Value;
end;

procedure TAnyValue.SetAsCardinal(const Value: Cardinal);
begin
  EnsureValueObject;
  FData.AsCardinal := Value;
end;

procedure TAnyValue.SetAsDateTime(const Value: TDateTime);
begin
  EnsureValueObject;
  FData.AsDateTime := Value;
end;

procedure TAnyValue.SetAsFloat(const Value: Extended);
begin
  EnsureValueObject;
  FData.AsFloat := Value;
end;

procedure TAnyValue.SetAsInt64(const Value: Int64);
begin
  EnsureValueObject;
  FData.AsInt64 := Value;
end;

procedure TAnyValue.SetAsInteger(const Value: Integer);
begin
  EnsureValueObject;
  FData.AsInteger := Value;
end;

procedure TAnyValue.SetAsInterface(const Value: IInterface);
begin
  EnsureValueObject;
  FData.AsInterface := Value;
end;

procedure TAnyValue.SetAsObject(const Value: TObject);
begin
  EnsureValueObject;
  FData.AsObject := Value;
end;

procedure TAnyValue.SetAsString(const Value: string);
begin
  EnsureValueObject;
  FData.AsString := Value;
end;

procedure TAnyValue.SetAsWideString(const Value: WideString);
begin
  EnsureValueObject;
  FData.AsWideString := Value;
end;

function TAnyValue.ValueType: TValueType;
begin
  Result := FData.ValueType;
end;

end.
