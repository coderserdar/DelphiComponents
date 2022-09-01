unit ObjectDatasetClass;

interface

uses
  Classes, SysUtils, Controls, Contnrs, Graphics;

type
  TTestSubItem = class(TCollectionItem)
  private
    FId: Integer;
    FName: String;
  published
    property Id:Integer read FId write FId;
    property Name: String read FName write FName;
  end;

  TTestSubItemList = class(TCollection)
  private
    function GetItem(index: integer): TTestSubItem;
    procedure SetItem(index: integer; const Value: TTestSubItem);
  public
    property Items[index:integer]:TTestSubItem read GetItem write SetItem;
  end;

  TTestItem = class(TCollectionItem)
  private
    FAvailable: Boolean;
    FDate: tdate;
    FDateTime: tdatetime;
    FTime: ttime;
    FByte: Byte;
    FWord: Word;
    FShortInt: ShortInt;
    FInteger: Integer;
    FInt64: Int64;
    FBoolean: Boolean;
    FByteBool: ByteBool;
    FLongBool: LongBool;
    FSmallint: SmallInt;
    FWordBool: WordBool;
    FLongint: Longint;
    FCardinal: Cardinal;
    FLongWord: Longword;
    FDouble: Double;
    FExtended: Extended;
    FReal: Real;
    FPicture: TPicture;
    FSubItems: TTestSubItemList;
    FCharacter: Char;
    FMemo: TStrings;
    FWideName: WideString;
    FAnsiName: AnsiString;
    FMoney: Currency;
    FShortName: ShortString;
    procedure SetPicture(const Value: TPicture);
    procedure SetMemo(const Value: TStrings);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property Available: Boolean read FAvailable write FAvailable;

    property Boolean: Boolean read FBoolean write FBoolean;
    property ByteBool: ByteBool read FByteBool write FByteBool;
    property WordBool: WordBool read FWordBool write FWordBool;
    property LongBool: LongBool read FLongBool write FLongBool;

    property Shortint:ShortInt read FShortInt write FShortInt;
    property Byte:Byte read FByte write FByte;
    property Smallint:SmallInt read FSmallint write FSmallint;
    property Word:Word read FWord write FWord;
    property Integer:Integer read FInteger write FInteger;
    property Longint:Longint read FLongint write FLongint;
    property Cardinal:Cardinal read FCardinal write FCardinal;
    property LongWord:Longword read FLongWord write FLongWord;
    property Int64:Int64 read FInt64 write FInt64;

    property Character:Char read FCharacter write FCharacter;
    property AnsiName: AnsiString read FAnsiName write FAnsiName;
    property WideName: WideString read FWideName write FWideName;
    property ShortName: ShortString read FShortName write FShortName;

    property Date:TDate read FDate write FDate;
    property Time:TTime read FTime write FTime;
    property DateTime:TDateTime read FDateTime write FDateTime;

    //property Comp:Comp read FComp write FComp;
    property Real:Real read FReal write FReal;
    property Money:Currency read FMoney write FMoney;
    //property Single:Single read FSingle write FSingle;
    property Double:Double read FDouble write FDouble;
    property Extended:Extended read FExtended write FExtended;

    property Picture:TPicture read FPicture write SetPicture;
    property Memo: TStrings read FMemo write SetMemo;
    property SubItems: TTestSubItemList read FSubItems write fSubItems;
  end;

implementation


{ TTestItem }

procedure TTestItem.AfterConstruction;
begin
  inherited;
  FMemo := TStringList.Create;
  FPicture := TPicture.Create;
  FSubItems := TTestSubItemList.Create(TTestSubItem);
end;

procedure TTestItem.BeforeDestruction;
begin
  inherited;
  FMemo.Free;
  FPicture.Free;
  FSubItems.Free;
end;

procedure TTestItem.SetMemo(const Value: TStrings);
begin
  FMemo.Assign(Value);
end;

procedure TTestItem.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

{ TTestSubItemList }

function TTestSubItemList.GetItem(index: integer): TTestSubItem;
begin
  Result := TTestSubItem(inherited Items[Index]);
end;

procedure TTestSubItemList.SetItem(index: integer;const Value: TTestSubItem);
begin
  inherited Items[Index]:=Value;
end;

initialization
  RegisterClasses([TTestItem,TTestSubItem]);

end.
