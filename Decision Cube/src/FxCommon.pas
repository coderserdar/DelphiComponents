unit FxCommon;

interface

uses
  Variants, Dialogs, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  IniFiles, DB, DBCommon, FxArrays;

resourcestring
  SUnsupportedVarType   = 'Unsupported Data Type %d';
  sOtherValues          = 'Other Values';

type

  EFxError=class(Exception);

  TDecisionDataEvent = (xeStateChanged, xeSummaryChanged, xePivot, xeNewMetaData, xeSourceChange);

  IFxProgress=interface
    ['{B347A872-DAD5-4071-A46D-53F0ACD3EE72}']
    function GetCancel:Boolean;
    procedure SetText(const Value:WideString);
    procedure StartProgress(Max:Integer);
    procedure FinishProgress;
    procedure UpdateProgress;
    property Cancel:Boolean read GetCancel;
    property Text:WideString write SetText;
  end;

  TFxNullProgress=class(TInterfacedObject,IFxProgress)
  private
    function GetCancel:Boolean;
    procedure SetText(const Value:WideString);
  public
    procedure StartProgress(Max:Integer);
    procedure FinishProgress;
    procedure UpdateProgress;
  end;

  TExpr=class
  protected
    FLine:Integer;
    function GetActive:Boolean;virtual;abstract;
  public
    function Eval(const Off:Integer):Variant;virtual;abstract;
    function TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;virtual;abstract;
    property Active:Boolean read GetActive;
    property Line:Integer read FLine;
  end;

  TExprs=class(TList)
  private
    function GetItems(Index: Integer): TExpr;
    procedure SetItems(Index: Integer; const Value: TExpr);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification);override;
  public
    property Items[Index:Integer]:TExpr read GetItems write SetItems;default;
  end;

  TFxAbstractMapItem=class(TCollectionItem)
  protected
    function GetActive:Boolean;virtual;abstract;
  public
    function TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;virtual;abstract;
    property Active:Boolean read GetActive;
  end;

  TBinData = class(TPersistent)
  private
    FNameList: TStringList;
    FValueList: TList;
    FOtherStr: string;
    function FindName(BinName: string; var pos: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Value: TPersistent); override;
    procedure AddBinValues(BinName: string; const Values: array of const);
    function AddBinValue(BinName: string; Value: Variant): Integer;
    function BinValueCount(BinName: string): Integer;
    function GetAllBinValueCount: Integer;
    function GetBinValue(BinName: string; Index: Integer): Variant;
    function GetBinValues(BinName: string): Variant;
    function GetBinName(Index: Integer): string;
    function GetBinCount: Integer;
    function GetBinNameDataType: TFieldType;
    function GetMaxBinNameSize: Integer;
    function AddBin( BinName: string; vType: Integer ): Integer;
    function AddIBinValue( iBin: Integer; value: variant): Integer;
    function GetIBinValue( iBin: Integer; ValueIndex: Integer): variant;
    function GetIBinValueCount( iBin: Integer): Integer;
    property OtherBinName: string read FOtherStr write FOtherStr;
    property BinName[ Index:Integer ]: string read GetBinName;
    property BinCount: Integer read GetBinCount;
    property NameList: TStringList read FNameList;
    property ValueList: TList read FValueList;
  end;

  TFxLog=class
  private
    FErrorCount:Integer;
    FWarnCount :Integer;
    FMessages  :TStringList;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    procedure Error(const Value:string);
    procedure ErrorFmt(const Format:string; Params:array of const);
    procedure Warning(const Value:string);
    property ErrorCount:Integer read FErrorCount;
    property Messages:TStringList read FMessages;
  end;

  TCubeCall=function(const PID,Off:Integer; const Params:TExprs):Variant of object;

  TCommonDataStore=class(TComponent)
  private
    FExternals:TStringList;
    FMaxSummaries: Integer;
    FMaxDimensions:Integer;
    FMaxCells: Cardinal;
    FLog:TFxLog;
    FOnCall:TCubeCall;
    procedure SetMaxCells(const Value:Cardinal);
    procedure SetMaxDimensions(const Value: Integer);
    procedure SetExternals(const Value: TStringList);
  protected
    function GetDataSet:TDataSet;virtual;abstract;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure Refresh(Rebuild:Boolean);virtual;abstract;
    property DataSet:TDataSet read GetDataSet;
    property Externals:TStringList read FExternals write SetExternals;
    property Log:TFxLog read FLog;
    property MaxCells: Cardinal read FMaxCells write SetMaxCells default 2000000000;
    property MaxDimensions: Integer read FMaxDimensions write SetMaxDimensions default 5;
    property MaxSummaries: Integer read FMaxSummaries write FMaxSummaries default 10;
    property OnCall:TCubeCall read FOnCall write FOnCall;
  end;

  TIdent=PChar;

  TSymbol=class
  private
    FID:TIdent;
    FLine:Integer;
  protected
    function GetActive:Boolean;virtual;abstract;
  public
    constructor Create(const AID:TIdent; const ALine:Integer);
    function Eval(const Off:Integer):Variant;virtual;abstract;
    function TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;virtual;abstract;
    procedure Assign(const Value:Variant);virtual;
    property Active:Boolean read GetActive;
    property ID:TIdent read FID;
    property Line:Integer read FLine;
  end;

  TSymbolClass=class of TSymbol;

  TSymbols=class(TList)
  private
    FIsOwner:Boolean;
    function GetItems(const Index: Integer): TSymbol;
    procedure SetItems(const Index: Integer; const Value: TSymbol);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification);override;
  public
    constructor Create(AOwned:Boolean);
    property Items[const Index:Integer]:TSymbol read GetItems write SetItems;default;
  end;

  procedure UpdateDesigner(Sender: TComponent);
  function FormatVariant(Value: Variant; FFormat: String): string;

implementation

uses
  Math, FxStore, DbConsts;

function FormatVariant(Value: Variant; FFormat: String): string;
var
  VarData: TVarData;
begin
  VarData := TVarData(Value);
  case TVarData(Value).vType of
    varDouble   : Result := FormatFloat(FFormat, Value);
    varCurrency : Result := FormatCurr(FFormat, Value);
    varDate     : Result := FormatDateTime(FFormat, Value);
    varInteger  : Result := FormatFloat(FFormat, Value);
  else
    Result:= Value;
  end;
end;

procedure UpdateDesigner(Sender: TComponent);
var
  NextParent: TComponent;
begin
  if (csDesigning in Sender.ComponentState) and not (csUpdating in Sender.ComponentState) then
  begin
    NextParent := Sender;
    while assigned(NextParent) and not (NextParent is TCustomForm) do
      NextParent := NextParent.Owner;
    if Assigned(NextParent) and Assigned(TCustomForm(NextParent).Designer) then
    begin
      TCustomForm(NextParent).Designer.Modified;
    end;
  end;
end;

function ConvertToVariant(const Value: TVarRec): Variant;
begin
  with Value do
    case VType of
      vtInteger    : Result := VInteger;
      vtBoolean    : Result := VBoolean;
      vtChar       : Result := VChar;
      vtExtended   : Result := VExtended^;
      vtString     : Result := VString^;
      vtPChar      : Result := VPChar^;
      vtAnsiString : Result := string(VAnsiString);
      vtCurrency   : Result := VCurrency^;
      vtVariant    : if not VarIsClear(VVariant^) then
                       Result := VVariant^;
      else
        EUnsupportedTypeError.CreateFmt(sUnsupportedVarType, [Value.VType]);
    end;
end;

{ TBinData }

constructor TBinData.Create;
begin
  inherited Create;
  FNameList  := TStringList.Create;
  FValueList := TList.Create;
  FOtherStr  := sOtherValues;
end;

destructor TBinData.destroy;
var
  custAr: TCustomArray;
begin
  if Assigned(FValueList) then begin
    while (FValueList.Count > 0) do begin
      custAr := FValueList.Last;
      FValueList.Remove(custAr);
      custAr.Free;
    end;
    FValueList.Free;
    FValueList := nil;
  end;
  FNameList.Free;
  FNameList := nil;
  inherited Destroy;
end;

procedure TBinData.Assign(Value: TPersistent);
var
  custAr, newCustAr: TCustomArray;
  I: Integer;
begin
  Clear;
  FNameList.Assign(TBinData(Value).FNameList);
  FOtherStr := TBinData(Value).FOtherStr;
  for I := 0 to TBinData(Value).FValueList.Count-1 do begin
    custAr := TBinData(Value).FValueList[I];
    newCustAr := TCustomArray.Create(custAr.Count, custAr.DataType);
    newCustAr.Assign(custAr, False, False);
    FValueList.Add(newCustAr);
  end;
end;

function TBinData.AddBinValue( BinName: string; Value: Variant ): Integer;
var
  custAr: TCustomArray;
  pos: Integer;
begin
  { Add the bin name if needed, otherwise get the position of the bin name in the string list }
  if not FindName(BinName, pos) then begin
    pos := FNameList.add(BinName);
    custAr := TCustomArray.Create(1, VarType(Value));
    custAr.Duplicates := dupIgnore;
    custAr.Sorted := True;
    if custAr<>nil then
      FValueList.Add(custAr);
  end;
  { Get the value array }
  custAr := FValueList[pos];
  Result :=-1;
  {Result := }custAr.Add(Value);
end;

procedure TBinData.AddBinValues(BinName: string; const Values: array of const);
var
  I: Integer;
begin
  for I := 0 to High(Values) do
    AddBinValue(BinName, ConvertToVariant(Values[I]));
end;

function TBinData.BinValueCount(BinName: string): Integer;
var
  pos: Integer;
  custAr: TCustomArray;
begin
  Result := 0;  
  if FindName(BinName, pos) then
  begin
    custAr := FValueList[pos];
    Result := custAr.Count;
  end;
end;

function TBinData.GetAllBinValueCount: Integer;
var
  I : Integer;
begin
  Result := 0;
  for i := 0 to GetBinCount-1 do
    Result := Result + GetIBinValueCount(i);
end;

function TBinData.GetBinValue(BinName: string; Index: Integer): Variant;
var
  pos: Integer;
  custAr: TCustomArray;
begin
  if FindName(BinName, pos) then
  begin
    custAr := FValueList[pos];
    Result := custAr[Index];
  end;
end;

function TBinData.GetBinValues(BinName: string): Variant;
var
  pos: Integer;
  custAr: TCustomArray;
  I: Integer;
begin
  if FindName(BinName, pos) then
  begin
    custAr := FValueList[pos];
    Result := VarArrayCreate([0, custAr.Count-1], varVariant);
    for I := 0 to custAr.Count-1 do
      Result[I] := custAr[I];
  end;
end;

function TBinData.GetBinName(Index: Integer): string;
begin
  Result := FNameList[Index];
end;

function TBinData.GetBinCount: Integer;
begin
  Result := FNameList.Count;
end;

function TBinData.GetBinNameDataType: TFieldType;
begin
  Result := ftString;
end;

function TBinData.GetMaxBinNameSize: Integer;
var
  I : Integer;
begin
  Result := Length(FOtherStr);
  for I := 0 to FNameList.Count-1 do
    Result := Max(Result , Length(FNameList[I]));
end;

procedure TBinData.Clear;
var
  custAr: TCustomArray;
begin
  if Assigned(FValueList) then begin
    while (FValueList.Count > 0) do begin
      custAr := FValueList.Last;
      FValueList.Remove(custAr);
      custAr.Free;
    end;
  end;
  FNameList.Clear;
end;

function TBinData.AddBin(BinName: string; vType: Integer): Integer;
var
  custAr: TCustomArray;
  pos: Integer;  
begin
  { Add the bin name if needed, otherwise get the position of the bin name in the string list }
  if not FindName(BinName, pos) then
  begin
    pos := FNameList.add(BinName);
    custAr := TCustomArray.Create(0, VType);
    if (custAr <> nil) then FValueList.Add(custAr);
  end;
  Result := pos;
end;

function TBinData.FindName(BinName: string; var pos: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FNameList.count-1 do begin
    if FNameList[i]=BinName then begin
      pos:=i;
      Result:=True;
      Exit;
    end;
  end;
end;

function TBinData.GetIBinValue(iBin: Integer; ValueIndex: Integer): variant;
begin
  Result := GetBinValue(GetBinName(iBin), ValueIndex);
end;

function TBinData.GetIBinValueCount(iBin: Integer): Integer;
begin
  Result := BinValueCount(GetBinName(iBin));
end;

function TBinData.AddIBinValue(iBin: Integer; value: variant): Integer;
begin
  Result := AddBinValue(GetBinName(iBin), value);
end;

{ TFxNullProgress }

procedure TFxNullProgress.FinishProgress;
begin

end;

function TFxNullProgress.GetCancel: Boolean;
begin
  Result:=False;
end;

procedure TFxNullProgress.SetText(const Value: WideString);
begin

end;

procedure TFxNullProgress.StartProgress(Max: Integer);
begin

end;

procedure TFxNullProgress.UpdateProgress;
begin

end;

{ TCommonDataStore }

constructor TCommonDataStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExternals:=TStringList.Create;
  FLog:=TFxLog.Create;
  FMaxCells      := 2000000000; // 2'000'000'000
  FMaxDimensions := 5;
  FMaxSummaries  := 10;
end;

destructor TCommonDataStore.Destroy;
begin
  FreeAndNil(FLog);
  FreeAndNil(FExternals);
  inherited;
end;

procedure TCommonDataStore.SetExternals(const Value: TStringList);
begin
  FExternals.Assign(Value);
end;

procedure TCommonDataStore.SetMaxCells(const Value: Cardinal);
begin
  if Value=0 then
    FMaxCells:=2000000000
  else
    FMaxCells:=Value;
end;

procedure TCommonDataStore.SetMaxDimensions(const Value: Integer);
begin
  if Value <> FMaxDimensions then
    FMaxDimensions:=Min(Value,MaxBinDimensions);
end;

{ TFxLog }

procedure TFxLog.Clear;
begin
  FMessages.Clear;
  FErrorCount:=0;
  FWarnCount:=0;
end;

constructor TFxLog.Create;
begin
  inherited Create;
  FMessages:=TStringList.Create;
end;

destructor TFxLog.Destroy;
begin
  FreeAndNil(FMessages);
  inherited;
end;

procedure TFxLog.Error(const Value: string);
begin
  Inc(FErrorCount);
  FMessages.Add(Format('[Error] %s',[Value]));
end;

procedure TFxLog.ErrorFmt(const Format: string; Params: array of const);
begin
  Error(SysUtils.Format(Format,Params));
end;

procedure TFxLog.Warning(const Value: string);
begin
  Inc(FWarnCount);
  FMessages.Add(Format('[Warning] %s',[Value]));
end;

{ TSymbol }

procedure TSymbol.Assign(const Value: Variant);
begin
  Assert(False);
end;

constructor TSymbol.Create(const AID:TIdent; const ALine:Integer);
begin
  inherited Create;
  FID:=AID;
  FLine:=ALine;
end;

{ TSymbols }

constructor TSymbols.Create(AOwned: Boolean);
begin
  inherited Create;
  FIsOwner:=AOwned;
end;

function TSymbols.GetItems(const Index: Integer): TSymbol;
begin
  Result:=inherited Items[Index];
end;

procedure TSymbols.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action=lnDeleted) and FIsOwner then
    TSymbol(Ptr).Free;
end;

procedure TSymbols.SetItems(const Index: Integer; const Value: TSymbol);
begin
  inherited Items[Index]:=Value;
end;

{ TExprs }

function TExprs.GetItems(Index: Integer): TExpr;
begin
  Result:=inherited Items[Index];
end;

procedure TExprs.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action=lnDeleted then
    TExpr(Ptr).Free;
end;

procedure TExprs.SetItems(Index: Integer; const Value: TExpr);
begin
  inherited Items[Index]:=Value;
end;

end.
