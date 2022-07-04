{**************************************************************************}
{   TxQuery DataSet                                                        }
{                                                                          }
{   The contents of this file are subject to the Mozilla Public License    }
{   Version 1.1 (the "License"); you may not use this file except in       }
{   compliance with the License. You may obtain a copy of the License at   }
{   http://www.mozilla.org/MPL/                                            }
{                                                                          }
{   Software distributed under the License is distributed on an "AS IS"    }
{   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the}
{   License for the specific language governing rights and limitations     }
{   under the License.                                                     }
{                                                                          }
{   The Original Code is SparsArr.pas                                      }
{                                                                          }
{   The Initial Developer of the Original Code is Alfonso Moreno.          }
{   Portions created by Alfonso Moreno are Copyright (C) Alfonso Moreno.   }
{   All Rights Reserved.                                                   }
{                                                                          }
{   Alfonso Moreno (Hermosillo, Sonora, Mexico)                            }
{   email: luisarvayo@yahoo.com                                            }
{     url: http://www.ezsoft.com                                           }
{          http://www.sigmap.com/txquery.htm                               }
{                                                                          }
{   Contributor(s): Chee-Yang, CHAU (Malaysia) <cychau@gmail.com>          }
{                   Sherlyn CHEW (Malaysia)                                }
{              url: http://code.google.com/p/txquery/                      }
{                   http://groups.google.com/group/txquery                 }
{                                                                          }
{**************************************************************************}

Unit SparsArr;

{$I XQ_FLAG.INC}
Interface

Uses
  Windows, SysUtils, Classes, Controls, StdCtrls
{$IFDEF LEVEL6}
  , Variants
{$ENDIF}
  ;

Type

  PPointer = ^Pointer;

  { TSparsePointerArray class}

  { Used by TSparseList.  Based on Sparse1Array, but has Pointer elements
    and Integer index, just like TPointerList/TList, and less indirection }

    { Apply function for the applicator:
          TheIndex        Index of item in array
          TheItem         Value of item (i.e pointer element) in section
          Returns: 0 if success, else error code. }
  TSPAApply = Function( TheIndex: Integer; TheItem: Pointer ): Integer;

  TSecDir = Array[0..4095] Of Pointer; { Enough for up to 12 bits of sec }
  PSecDir = ^TSecDir;
  TSPAQuantum = ( SPASmall, SPALarge ); { Section size }

  TSparsePointerArray = Class( TObject )
  Private
    secDir: PSecDir;
    slotsInDir: Word;
    indexMask, secShift: Word;
    FHighBound: Integer;
    FSectionSize: Word;
    cachedIndex: Integer;
    cachedPointer: Pointer;
    { Return item[i], nil if slot outside defined section. }
    Function GetAt( Index: Integer ): Pointer;
    { Return address of item[i], creating slot if necessary. }
    Function MakeAt( Index: Integer ): PPointer;
    { Store item at item[i], creating slot if necessary. }
    Procedure PutAt( Index: Integer; Item: Pointer );
  Public
    Constructor Create( Quantum: TSPAQuantum );
    Destructor Destroy; Override;

    { Traverse SPA, calling apply function for each defined non-nil
      item.  The traversal terminates if the apply function returns
      a value other than 0. }
    { NOTE: must be static method so that we can take its address in
      TSparseList.ForAll }
    Function ForAll( ApplyFunction: Pointer {TSPAApply} ): Integer;

    { Ratchet down HighBound after a deletion }
    Procedure ResetHighBound;

    Property HighBound: Integer Read FHighBound;
    Property SectionSize: Word Read FSectionSize;
    Property Items[Index: Integer]: Pointer Read GetAt Write PutAt; Default;
  End;

  { TSparseList class }

  TSparseList = Class( TObject )
  Private
    FList: TSparsePointerArray;
    FCount: Integer; { 1 + HighBound, adjusted for Insert/Delete }
    FQuantum: TSPAQuantum;
    Procedure NewList( Quantum: TSPAQuantum );
  Protected
    Procedure Error; Virtual;
    Function Get( Index: Integer ): Pointer;
    Procedure Put( Index: Integer; Item: Pointer );
  Public
    Constructor Create( Quantum: TSPAQuantum );
    Destructor Destroy; Override;
    Function Add( Item: Pointer ): Integer;
    Procedure Clear;
    Procedure Delete( Index: Integer );
    Procedure Exchange( Index1, Index2: Integer );
    Function First: Pointer;
    Function ForAll( ApplyFunction: Pointer {TSPAApply} ): Integer;
    Function IndexOf( Item: Pointer ): Integer;
    Procedure Insert( Index: Integer; Item: Pointer );
    Function Last: Pointer;
    Procedure Move( CurIndex, NewIndex: Integer );
    Procedure Pack;
    Function Remove( Item: Pointer ): Integer;
    Property Count: Integer Read FCount;
    Property Items[Index: Integer]: Pointer Read Get Write Put; Default;
  End;

  { TAggSparseList class }

  TAggSparseList = Class
  Private
    FList: TSparseList; { of AggItems }
  Protected
    Function Get( Index: Integer ): Variant;
    Function GetCount( Index: Integer ): Integer;
    Procedure Put( Index: Integer; Const Value: Variant );
    Procedure PutCount( Index: Integer; Value: Integer );
    Function GetSqr( Index: Integer ): Double;
    Procedure PutSqr( Index: Integer; Const Value: Double );
    Procedure Error;
  Public
    Constructor Create( Capacity: Integer );
    Destructor Destroy; Override;
    Function HasData( Index: Integer ): Boolean;
    Procedure Delete( Index: Integer );
    Procedure Exchange( Index1, Index2: Integer );
    Procedure Insert( Index: Integer; Const Value: Variant );
    Procedure Clear;

    Property Values[Index: Integer]: Variant Read Get Write Put;
    Property SqrValues[Index: Integer]: Double Read GetSqr Write PutSqr;
    Property Count[Index: Integer]: Integer Read GetCount Write PutCount;
  End;

Implementation

Type

  { AggItem management for TAggSparseList }

  PAggItem = ^TAggItem;
  TAggItem = Record
    FValue: Variant;
    FSqrValue: Double;
    FCount: Integer;
  End;

  { Exception classes }

  EAggregateSparseListError = Class( Exception );

Function NewAggItem( Const AValue: Variant; Const ASqrValue: Double; ACount: Integer ): PAggItem;
Begin
  New( Result );
  Result^.FCount := ACount;
  If VarIsNull( AValue ) Then
    Result^.FValue := 0.0
  else
    Result^.FValue := AValue;
  Result^.FSqrValue := ASqrValue;
End;

Procedure DisposeAggItem( P: PAggItem );
Begin
  If P=Nil then Exit;
  VarClear( P^.FValue );
  Dispose( P );
End;

{ TSparsePointerArray }

Const
  SPAIndexMask: Array[TSPAQuantum] Of Byte = ( 15, 255 );
  SPASecShift: Array[TSPAQuantum] Of Byte = ( 4, 8 );

  { Expand Section Directory to cover at least `newSlots' slots. Returns: Possibly
    updated pointer to the Section Directory. }

Function ExpandDir( secDir: PSecDir; Var slotsInDir: Word;
  newSlots: Word ): PSecDir;
Begin
  Result := secDir;
  ReallocMem( Result, newSlots * SizeOf( Pointer ) );
  FillChar( Result^[slotsInDir], ( newSlots - slotsInDir ) * SizeOf( Pointer ), 0 );
  slotsInDir := newSlots;
End;

{ Allocate a section and set all its items to nil. Returns: Pointer to start of
  section. }

Function MakeSec( SecIndex: Integer; SectionSize: Word ): Pointer;
Var
  SecP: Pointer;
  Size: Word;
Begin
  Size := SectionSize * SizeOf( Pointer );
  GetMem( secP, size );
  FillChar( secP^, size, 0 );
  MakeSec := SecP
End;

Constructor TSparsePointerArray.Create( Quantum: TSPAQuantum );
Begin
  SecDir := Nil;
  SlotsInDir := 0;
  FHighBound := -1;
  FSectionSize := Word( SPAIndexMask[Quantum] ) + 1;
  IndexMask := Word( SPAIndexMask[Quantum] );
  SecShift := Word( SPASecShift[Quantum] );
  CachedIndex := -1
End;

Destructor TSparsePointerArray.Destroy;
Var
  i: Integer;
  size: Word;
Begin
  { Scan section directory and free each section that exists. }
  i := 0;
  size := FSectionSize * SizeOf( Pointer );
  While i < slotsInDir Do
  Begin
    If secDir^[i] <> Nil Then
      FreeMem( secDir^[i], size );
    Inc( i )
  End;

  { Free section directory. }
  If secDir <> Nil Then
    FreeMem( secDir, slotsInDir * SizeOf( Pointer ) );
End;

Function TSparsePointerArray.GetAt( Index: Integer ): Pointer;
Var
  byteP: PAnsiChar; { patched by ccy }
  secIndex: Cardinal;
Begin
  { Index into Section Directory using high order part of
    index.  Get pointer to Section. If not null, index into
    Section using low order part of index. }
  If Index = cachedIndex Then
    Result := cachedPointer
  Else
  Begin
    secIndex := Index Shr secShift;
    If secIndex >= slotsInDir Then
      byteP := Nil
    Else
    Begin
      byteP := secDir^[secIndex];
      If byteP <> Nil Then
      Begin
        Inc( byteP, ( Index And indexMask ) * SizeOf( Pointer ) );
      End
    End;
    If byteP = Nil Then
      Result := Nil
    Else
      Result := PPointer( byteP )^;
    cachedIndex := Index;
    cachedPointer := Result
  End
End;

Function TSparsePointerArray.MakeAt( Index: Integer ): PPointer;
Var
  dirP: PSecDir;
  p: Pointer;
  byteP: PAnsiChar; { patched by ccy }
  secIndex: Word;
Begin
  { Expand Section Directory if necessary. }
  secIndex := Index Shr secShift; { Unsigned shift }
  If secIndex >= slotsInDir Then
    dirP := expandDir( secDir, slotsInDir, secIndex + 1 )
  Else
    dirP := secDir;

  { Index into Section Directory using high order part of
    index.  Get pointer to Section. If null, create new
    Section.  Index into Section using low order part of index. }
  secDir := dirP;
  p := dirP^[secIndex];
  If p = Nil Then
  Begin
    p := makeSec( secIndex, FSectionSize );
    dirP^[secIndex] := p
  End;
  byteP := p;
  Inc( byteP, ( Index And indexMask ) * SizeOf( Pointer ) );
  If Index > FHighBound Then
    FHighBound := Index;
  Result := PPointer( byteP );
  cachedIndex := -1
End;

Procedure TSparsePointerArray.PutAt( Index: Integer; Item: Pointer );
Begin
  If ( Item <> Nil ) Or ( GetAt( Index ) <> Nil ) Then
  Begin
    MakeAt( Index )^ := Item;
    If Item = Nil Then
      ResetHighBound
  End
End;

Function TSparsePointerArray.ForAll( ApplyFunction: Pointer {TSPAApply} ):
  Integer;
Var
  itemP: PAnsiChar; { Pointer to item in section } { patched by ccy }
  item: Pointer;
  i, callerBP: Cardinal;
  j, index: Integer;
Begin
  { Scan section directory and scan each section that exists,
    calling the apply function for each non-nil item.
    The apply function must be a far local function in the scope of
    the procedure P calling ForAll.  The trick of setting up the stack
    frame (taken from TurboVision's TCollection.ForEach) allows the
    apply function access to P's arguments and local variables and,
    if P is a method, the instance variables and methods of P's class }
  Result := 0;
  i := 0;
  Asm
    mov   eax,[ebp]                     { Set up stack frame for local }
    mov   callerBP,eax
  End;
  While ( i < slotsInDir ) And ( Result = 0 ) Do
  Begin
    itemP := secDir^[i];
    If itemP <> Nil Then
    Begin
      j := 0;
      index := i Shl SecShift;
      While ( j < FSectionSize ) And ( Result = 0 ) Do
      Begin
        item := PPointer( itemP )^;
        If item <> Nil Then
          { ret := ApplyFunction(index, item.Ptr); }
          Asm
            mov   eax,index
            mov   edx,item
            push  callerBP
            call  ApplyFunction
            pop   ecx
            mov   @Result,eax
          End;
        Inc( itemP, SizeOf( Pointer ) );
        Inc( j );
        Inc( index )
      End
    End;
    Inc( i )
  End;
End;

Procedure TSparsePointerArray.ResetHighBound;
Var
  NewHighBound: Integer;

  Function Detector( TheIndex: Integer; TheItem: Pointer ): Integer; Far;
  Begin
    If TheIndex > FHighBound Then
      Result := 1
    Else
    Begin
      Result := 0;
      If TheItem <> Nil Then
        NewHighBound := TheIndex
    End
  End;

Begin
  NewHighBound := -1;
  ForAll( @Detector );
  FHighBound := NewHighBound
End;

{ TSparseList }

Constructor TSparseList.Create( Quantum: TSPAQuantum );
Begin
  NewList( Quantum )
End;

Destructor TSparseList.Destroy;
Begin
  If FList <> Nil Then
    FList.Destroy
End;

Function TSparseList.Add( Item: Pointer ): Integer;
Begin
  Result := FCount;
  FList[Result] := Item;
  Inc( FCount )
End;

Procedure TSparseList.Clear;
Begin
  FList.Destroy;
  NewList( FQuantum );
  FCount := 0
End;

Procedure TSparseList.Delete( Index: Integer );
Var
  I: Integer;
Begin
  If ( Index < 0 ) Or ( Index >= FCount ) Then
    Exit;
  For I := Index To FCount - 1 Do
    FList[I] := FList[I + 1];
  FList[FCount] := Nil;
  Dec( FCount );
End;

Procedure TSparseList.Error;
Begin
  Raise EListError.Create( 'List Index Error!' )
End;

Procedure TSparseList.Exchange( Index1, Index2: Integer );
Var
  temp: Pointer;
Begin
  temp := Get( Index1 );
  Put( Index1, Get( Index2 ) );
  Put( Index2, temp );
End;

Function TSparseList.First: Pointer;
Begin
  Result := Get( 0 )
End;

{ Jump to TSparsePointerArray.ForAll so that it looks like it was called
  from our caller, so that the BP trick works. }

Function TSparseList.ForAll( ApplyFunction: Pointer {TSPAApply} ): Integer; Assembler;
Asm
        MOV     EAX,[EAX].TSparseList.FList
        JMP     TSparsePointerArray.ForAll
End;

Function TSparseList.Get( Index: Integer ): Pointer;
Begin
  If Index < 0 Then
    Error;
  Result := FList[Index]
End;

Function TSparseList.IndexOf( Item: Pointer ): Integer;
Var
  MaxIndex, Index: Integer;

  Function IsTheItem( TheIndex: Integer; TheItem: Pointer ): Integer; Far;
  Begin
    If TheIndex > MaxIndex Then
      Result := -1 { Bail out }
    Else If TheItem <> Item Then
      Result := 0
    Else
    Begin
      Result := 1; { Found it, stop traversal }
      Index := TheIndex
    End
  End;

Begin
  Index := -1;
  MaxIndex := FList.HighBound;
  FList.ForAll( @IsTheItem );
  Result := Index
End;

Procedure TSparseList.Insert( Index: Integer; Item: Pointer );
Var
  i: Integer;
Begin
  If Index < 0 Then
    Error;
  I := FCount;
  While I > Index Do
  Begin
    FList[i] := FList[i - 1];
    Dec( i )
  End;
  FList[Index] := Item;
  If Index > FCount Then
    FCount := Index;
  Inc( FCount )
End;

Function TSparseList.Last: Pointer;
Begin
  Result := Get( FCount - 1 );
End;

Procedure TSparseList.Move( CurIndex, NewIndex: Integer );
Var
  Item: Pointer;
Begin
  If CurIndex <> NewIndex Then
  Begin
    Item := Get( CurIndex );
    Delete( CurIndex );
    Insert( NewIndex, Item );
  End;
End;

Procedure TSparseList.NewList( Quantum: TSPAQuantum );
Begin
  FQuantum := Quantum;
  FList := TSparsePointerArray.Create( Quantum )
End;

Procedure TSparseList.Pack;
Var
  i: Integer;
Begin
  For i := FCount - 1 Downto 0 Do
    If Items[i] = Nil Then
      Delete( i )
End;

Procedure TSparseList.Put( Index: Integer; Item: Pointer );
Begin
  If Index < 0 Then Error;
  FList[Index] := Item;
  FCount := FList.HighBound + 1
End;

Function TSparseList.Remove( Item: Pointer ): Integer;
Begin
  Result := IndexOf( Item );
  If Result <> -1 Then
    Delete( Result )
End;

{ TAggSparseList }

Constructor TAggSparseList.Create( Capacity: Integer );
Var
  quantum: TSPAQuantum;
Begin
  If Capacity > 256 Then
    quantum := SPALarge
  Else
    quantum := SPASmall;
  FList := TSparseList.Create( Quantum );
End;

Destructor TAggSparseList.Destroy;
Begin
  If FList <> Nil Then
  Begin
    Clear;
    FList.Destroy
  End
End;

Function TAggSparseList.HasData( Index: Integer ): Boolean;
Begin
  Result:= FList[Index] <> Nil;
End;

Function TAggSparseList.Get( Index: Integer ): Variant;
Var
  p: PAggItem;
Begin
  p := PAggItem( FList[Index] );
  If p = Nil Then
  begin
    Result := 0.0;
    VarAsType(Result, varDouble);
  end Else
    Result := p^.FValue;
End;

Function TAggSparseList.GetSqr( Index: Integer ): Double;
Var
  p: PAggItem;
Begin
  p := PAggItem( FList[Index] );
  If p = Nil Then
    Result := 0
  Else
    Result := p^.FSqrValue;
End;

Function TAggSparseList.GetCount( Index: Integer ): Integer;
Var
  p: PAggItem;
Begin
  p := PAggItem( FList[Index] );
  If p = Nil Then
    Result := 0
  Else
    Result := p^.FCount
End;

Procedure TAggSparseList.Put( Index: Integer; Const Value: Variant );
Var
  p: PAggItem;
Begin
  p := PAggItem( FList[Index] );
  If p = Nil Then
    FList[Index] := NewAggItem( Value, 0, 0 )
  Else
    p^.FValue := Value;
End;

Procedure TAggSparseList.PutSqr( Index: Integer; Const Value: Double );
Var
  p: PAggItem;
Begin
  p := PAggItem( FList[Index] );
  If p = Nil Then
    FList[Index] := NewAggItem( 0, Value, 0 )
  Else
    p^.FSqrValue := Value;
End;

Procedure TAggSparseList.PutCount( Index: Integer; Value: Integer );
Var
  p: PAggItem;
Begin
  p := PAggItem( FList[Index] );
  If p = Nil Then
    FList[Index] := NewAggItem( 0, 0, Value )
  Else
    p^.FCount := Value;
End;

Procedure TAggSparseList.Error;
Begin
  Raise EAggregateSparseListError.Create( 'Put Counts Error!' )
End;

Procedure TAggSparseList.Delete( Index: Integer );
Var
  p: PAggItem;
Begin
  p := PAggItem( FList[Index] );
  If p <> Nil Then
    DisposeAggItem( p );
  FList.Delete( Index );
End;

Procedure TAggSparseList.Exchange( Index1, Index2: Integer );
Begin
  FList.Exchange( Index1, Index2 );
End;

Procedure TAggSparseList.Insert( Index: Integer; Const Value: Variant );
Begin
  FList.Insert( Index, NewAggItem( Value, 0, 0 ) );
End;

Procedure TAggSparseList.Clear;

  Function ClearItem( TheIndex: Integer; TheItem: Pointer ): Integer; Far;
  Begin
    DisposeAggItem( PAggItem( TheItem ) ); { Item guaranteed non-nil }
    Result := 0
  End;

Begin
  FList.ForAll( @ClearItem );
  FList.Clear;
End;

End.
