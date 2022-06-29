{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: IndexContainer.pas,v 1.10 2002/12/27 15:07:32 laa Exp $}

unit IndexContainer;

interface
{$i common.inc}

uses Classes;

type
  TProcIndex = procedure(Index : Pointer) of object;
  TProcData = procedure(Index : Pointer; Data : TObject) of object;

  TIndexContainer = class(TObject)
  private
    FIndexFastSearchList, FIndexRestList: TList;
    FDataFastSearchList, FDataRestList: TList;
    FHashSize : Integer;
    function HashIndex(IndexPtr : Pointer) : Integer;
    procedure DoAdd(Index: Pointer; Data: TObject; ReplaceData : Boolean);
  public
    constructor Create(HashSize : Integer; ContainsData : Boolean);
    destructor Destroy; override;
{$ifdef D4_OR_HIGHER}
    procedure Add(Index: Pointer); overload; virtual;
    procedure Add(Index: Pointer; Data: TObject); overload; virtual;
    function Contains(Index: Pointer): Boolean; overload;
    function Contains(Index: Pointer; var Data: TObject): Boolean; overload;
    function Remove(Index: Pointer): Boolean; overload; virtual;
    function Remove(Index: Pointer; var Data: TObject): Boolean; overload; virtual;
{$else}
    procedure AddOL(Index: Pointer); virtual;
    procedure Add(Index: Pointer; Data: TObject); virtual;
    function ContainsOL(Index: Pointer): Boolean;
    function Contains(Index: Pointer; var Data: TObject): Boolean;
    function RemoveOL(Index: Pointer): Boolean; virtual;
    function Remove(Index: Pointer; var Data: TObject): Boolean; virtual;
{$endif D4_OR_HIGHER}
    procedure Clear; virtual;
    procedure ClearAndFreeData; virtual;
    function ContainsData : Boolean;
    function ItemCount : Integer;
    function Empty : Boolean;

    procedure ProcIndexContents(ProcIndex : TProcIndex);
    procedure ProcDataContents(ProcData : TProcData);
  end;

  TIndexContainerIterator = class(TObject)
  private
    FContainer : TIndexContainer;
    FUseFastSearchList : Boolean;
    FCurrentIndex : Integer;

    FIndex : Pointer;
    FData : TObject;
    FEOF : Boolean;

    function GetIndex : Pointer;
    procedure SetIndexAndData;
  public
    constructor Create(Container : TIndexContainer);
    destructor Destroy; override;

    procedure First;
    procedure Next;

    property Index : Pointer read FIndex;
    property Data : TObject read FData;
    property EOF : Boolean read FEOF;
  end;

implementation

uses SysUtils;

{ TIndexContainer }

constructor TIndexContainer.Create(HashSize : Integer; ContainsData : Boolean);
begin
  inherited Create;
  FHashSize := HashSize;

  FIndexFastSearchList := TList.Create;
  FIndexRestList := TList.Create;

  if ContainsData then
  begin
    FDataFastSearchList := TList.Create;
    FDataRestList := TList.Create;
  end;
end;

destructor TIndexContainer.Destroy;
begin
  inherited Destroy;

  FIndexFastSearchList.Free;
  FIndexRestList.Free;

  if ContainsData then
  begin
    FDataFastSearchList.Free;
    FDataRestList.Free;
  end;
end;

{$ifdef D4_OR_HIGHER}
procedure TIndexContainer.Add(Index: Pointer);
{$else}
procedure TIndexContainer.AddOL(Index: Pointer);
{$endif D4_OR_HIGHER}
begin
  DoAdd(Index, nil, False);
end;

procedure TIndexContainer.Add(Index: Pointer; Data: TObject);
begin
  DoAdd(Index, Data, True);
end;

procedure TIndexContainer.DoAdd(Index: Pointer; Data: TObject; ReplaceData : Boolean);
var
  idx : Integer;
  HasData : Boolean;
begin
  if Index = nil then
    raise Exception.Create('TIndexContainer.Add: nil is not allowed as Index!');

  idx := HashIndex(Index);

  HasData := ContainsData;
  if HasData then
    while idx >= FIndexFastSearchList.Count do
    begin
      FIndexFastSearchList.Add(nil);
      FDataFastSearchList.Add(nil);
    end
  else
    while idx >= FIndexFastSearchList.Count do
      FIndexFastSearchList.Add(nil);

  if FIndexFastSearchList[idx] = nil then
  begin
    FIndexFastSearchList[idx] := Index;
    if HasData then
      FDataFastSearchList[idx] := Data;
  end
  else if FIndexFastSearchList[idx] = Index then
  begin
    if ReplaceData and HasData then
      FDataFastSearchList[idx] := Data;
  end
  else
  begin
    idx := FIndexRestList.IndexOf(Index);
    if idx = -1 then
    begin
      FIndexRestList.Add(Index);
      if HasData then
        FDataRestList.Add(Data);
    end
    else
    begin
      if ReplaceData and HasData then
        FDataRestList[idx] := Data;
    end;
  end;
end;

procedure TIndexContainer.Clear;
begin
  FIndexFastSearchList.Clear;
  FIndexRestList.Clear;

  if ContainsData then
  begin
    FDataFastSearchList.Clear;
    FDataRestList.Clear;
  end;
end;

procedure TIndexContainer.ClearAndFreeData;
var
  i : Integer;
begin
  if ContainsData then
  begin
    for i := 0 to FDataFastSearchList.Count - 1 do
      TObject(FDataFastSearchList[i]).Free;
    for i := 0 to FDataRestList.Count - 1 do
      TObject(FDataRestList[i]).Free;
  end;

  Clear;
end;

{$ifdef D4_OR_HIGHER}
function TIndexContainer.Contains(Index: Pointer): Boolean;
{$else}
function TIndexContainer.ContainsOL(Index: Pointer): Boolean;
{$endif D4_OR_HIGHER}
var
  Data: TObject;
begin
  Result := Contains(Index, Data);
end;

function TIndexContainer.Contains(Index: Pointer; var Data: TObject): Boolean;
var
  idx : Integer;
begin
  Data := nil;

  idx := HashIndex(Index);
  if idx >= FIndexFastSearchList.Count then
    Result := False
  else if FIndexFastSearchList[idx] = nil then
    Result := False
  else if FIndexFastSearchList[idx] = Index then
  begin
    Result := True;
    if ContainsData then
      Data := FDataFastSearchList[idx];
  end
  else
  begin
    idx := FIndexRestList.IndexOf(Index);
    Result := (idx >= 0);
    if Result and ContainsData then
      Data := FDataRestList[idx];
  end;
end;

{$ifdef D4_OR_HIGHER}
function TIndexContainer.Remove(Index: Pointer): Boolean;
{$else}
function TIndexContainer.RemoveOL(Index: Pointer): Boolean;
{$endif D4_OR_HIGHER}
var
  Data: TObject;
begin
  Result := Remove(Index, Data);
end;

function TIndexContainer.Remove(Index: Pointer; var Data: TObject): Boolean;
var
  idx, i : Integer;
  HasData : Boolean;
begin
  Data := nil;

  idx := HashIndex(Index);
  if idx >= FIndexFastSearchList.Count then
    Result := False
  else if FIndexFastSearchList[idx] = nil then
    Result := False
  else if FIndexFastSearchList[idx] = Index then
  begin
    Result := True;
    HasData := ContainsData;
    if HasData then
    begin
      Data := FDataFastSearchList[idx];
      FDataFastSearchList[idx] := nil;
    end;
    FIndexFastSearchList[idx] := nil;

    for i := 0 to FIndexRestList.Count - 1 do
      if HashIndex(FIndexRestList[i]) = idx then
      begin
        FIndexFastSearchList[idx] := FIndexRestList[i];
        FIndexRestList.Delete(i);

        if HasData then
        begin
          FDataFastSearchList[idx] := FDataRestList[i];
          FDataRestList.Delete(i);
        end;

        Break;
      end;
  end
  else
  begin
    idx := FIndexRestList.IndexOf(Index);
    Result := (idx >= 0);
    if Result then
    begin
      if ContainsData then
      begin
        Data := FDataRestList[idx];
        FDataRestList.Delete(idx);
      end;
      FIndexRestList.Delete(idx);
    end;
  end;
end;

function TIndexContainer.HashIndex(IndexPtr : Pointer) : Integer;
begin
  Result := Integer(IndexPtr) mod FHashSize;
end;

function TIndexContainer.ContainsData : Boolean;
begin
  Result := (FDataFastSearchList <> nil);
end;

procedure TIndexContainer.ProcIndexContents(ProcIndex : TProcIndex);
var
  Iterator : TIndexContainerIterator;
begin
  Iterator := TIndexContainerIterator.Create(Self);
  while not Iterator.EOF do
  begin
    ProcIndex(Iterator.Index);
    Iterator.Next;
  end;
  Iterator.Free;
end;

procedure TIndexContainer.ProcDataContents(ProcData : TProcData);
var
  Iterator : TIndexContainerIterator;
begin
  Iterator := TIndexContainerIterator.Create(Self);
  while not Iterator.EOF do
  begin
    ProcData(Iterator.Index, Iterator.Data);
    Iterator.Next;
  end;
  Iterator.Free;
end;

function TIndexContainer.ItemCount : Integer;
var
  i : Integer;
begin
  Result := FIndexRestList.Count;

  for i := 0 to FIndexFastSearchList.Count - 1 do
    if FIndexFastSearchList[i] <> nil then
      Inc(Result);
end;

function TIndexContainer.Empty : Boolean;
var
  i : Integer;
begin
  Result := FIndexRestList.Count = 0;

  if Result then
    for i := 0 to FIndexFastSearchList.Count - 1 do
      if FIndexFastSearchList[i] <> nil then
      begin
        Result := False;
        Break;
      end;
end;

{ TIndexContainerIterator }

constructor TIndexContainerIterator.Create(Container : TIndexContainer);
begin
  inherited Create;
  FContainer := Container;
  FData := nil;

  First;
end;

destructor TIndexContainerIterator.Destroy;
begin
  inherited Destroy;
end;

procedure TIndexContainerIterator.First;
begin
  FUseFastSearchList := True;
  FCurrentIndex := 0;
  FEOF := False;
  FIndex := nil;

  if (FContainer.FIndexFastSearchList.Count = 0) or
     (FContainer.FIndexFastSearchList[0] = nil) then
    Next
  else
    SetIndexAndData;
end;

procedure TIndexContainerIterator.Next;
var
  NewIndex : Pointer;
begin
  if EOF then
    Exit;

  if (FIndex <> nil) then
  begin
    NewIndex := GetIndex;
    if (NewIndex <> nil) and
       (NewIndex <> FIndex) then
    begin
      SetIndexAndData;
      Exit;
    end;
  end;

  Inc(FCurrentIndex);

  if FUseFastSearchList then
  begin
    while (FCurrentIndex < FContainer.FIndexFastSearchList.Count) and
          (FContainer.FIndexFastSearchList[FCurrentIndex] = nil) do
      Inc(FCurrentIndex);

    if FCurrentIndex >= FContainer.FIndexFastSearchList.Count then
      if FContainer.FIndexRestList.Count = 0 then
        FEOF := True
      else
      begin
        FUseFastSearchList := False;
        FCurrentIndex := 0;
      end;
  end
  else if FCurrentIndex >= FContainer.FIndexRestList.Count then
    FEOF := True;

  SetIndexAndData;
end;

function TIndexContainerIterator.GetIndex : Pointer;
begin
  if FEOF then
  begin
    Result := nil;
  end
  else if FUseFastSearchList then
  begin
    if FCurrentIndex < FContainer.FIndexFastSearchList.Count then
      Result := FContainer.FIndexFastSearchList[FCurrentIndex]
    else
      Result := nil;
  end
  else
  begin
    if FCurrentIndex < FContainer.FIndexRestList.Count then
      Result := FContainer.FIndexRestList[FCurrentIndex]
    else
      Result := nil;
  end;
end;


procedure TIndexContainerIterator.SetIndexAndData;
begin
  if FEOF then
  begin
    FIndex := nil;
    FData := nil;
  end
  else if FUseFastSearchList then
  begin
    FIndex := FContainer.FIndexFastSearchList[FCurrentIndex];
    if FContainer.ContainsData then
      FData := FContainer.FDataFastSearchList[FCurrentIndex];
  end
  else
  begin
    FIndex := FContainer.FIndexRestList[FCurrentIndex];
    if FContainer.ContainsData then
      FData := FContainer.FDataRestList[FCurrentIndex];
  end;
end;

end.

