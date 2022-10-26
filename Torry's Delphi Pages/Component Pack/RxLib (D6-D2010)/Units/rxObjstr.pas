{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit rxObjStr;

interface

{$I RX.INC}

uses
  SysUtils, Classes;

type

{ TObjectStrings }

  TDestroyEvent = procedure(Sender, AObject: TObject) of object;
  TObjectSortCompare = function (const S1, S2: string;
    Item1, Item2: TObject): Integer of object;

  TObjectStrings = class(TStringList)
  private
    FOnDestroyObject: TDestroyEvent;
  protected
    procedure DestroyObject(AObject: TObject); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure Remove(Index: Integer);
    procedure ParseStrings(const Values: string);
    procedure SortList(Compare: TObjectSortCompare);
    property OnDestroyObject: TDestroyEvent read FOnDestroyObject
      write FOnDestroyObject;
  end;

{ THugeList class }

const
  MaxHugeListSize = MaxListSize;

type
  THugeList = class(TList);

implementation

uses
  Consts,
  {$IFDEF RX_D6} RTLConsts, {$ENDIF} // Polaris
  rxStrUtils;

{ TObjectStrings }

procedure QuickSort(SortList: TStrings; L, R: Integer;
  SCompare: TObjectSortCompare);
var
  I, J: Integer;
  P: TObject;
  S: string;
begin
  repeat
    I := L;
    J := R;
    P := SortList.Objects[(L + R) shr 1];
    S := SortList[(L + R) shr 1];
    repeat
      while SCompare(SortList[I], S, SortList.Objects[I], P) < 0 do
        Inc(I);
      while SCompare(SortList[J], S, SortList.Objects[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        SortList.Exchange(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TObjectStrings.DestroyObject(AObject: TObject);
begin
  if Assigned(FOnDestroyObject) then
    FOnDestroyObject(Self, AObject)
  else
    if AObject <> nil then
      AObject.Free;
end;

procedure TObjectStrings.Clear;
var
  I: Integer;
begin
  if Count > 0 then
  begin
    Changing;
    for I := 0 to Count - 1 do
      Objects[I] := nil;
    BeginUpdate;
    try
      inherited Clear;
    finally
      EndUpdate;
    end;
    Changed;
  end;
end;

procedure TObjectStrings.Delete(Index: Integer);
begin
  Objects[Index] := nil;
  inherited Delete(Index);
end;

procedure TObjectStrings.Remove(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TObjectStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: string;
begin
  if CurIndex <> NewIndex then
  begin
    TempString := Get(CurIndex);
    TempObject := GetObject(CurIndex);
    inherited Delete(CurIndex);
    try
      InsertObject(NewIndex, TempString, TempObject);
    except
      DestroyObject(TempObject);
      raise;
    end;
  end;
end;

procedure TObjectStrings.PutObject(Index: Integer; AObject: TObject);
begin
  Changing;
  BeginUpdate;
  try
    if (Index < Self.Count) and (Index >= 0) then
      DestroyObject(Objects[Index]);
    inherited PutObject(Index, AObject);
  finally
    EndUpdate;
  end;
  Changed;
end;

procedure TObjectStrings.ParseStrings(const Values: string);
var
  Pos: Integer;
begin
  Pos := 1;
  BeginUpdate;
  try
    while Pos <= Length(Values) do
      Add(ExtractSubstr(Values, Pos, [';']));
  finally
    EndUpdate;
  end;
end;

procedure TObjectStrings.SortList(Compare: TObjectSortCompare);
begin
  if Sorted then
{$IFDEF RX_D3}
    Error(SSortedListError, 0);
{$ELSE}
    raise EListError.Create(LoadStr(SSortedListError));
{$ENDIF}
  if Count > 0 then
  begin
    BeginUpdate;
    try
      QuickSort(Self, 0, Count - 1, Compare);
    finally
      EndUpdate;
    end;
  end;
end;

end.
