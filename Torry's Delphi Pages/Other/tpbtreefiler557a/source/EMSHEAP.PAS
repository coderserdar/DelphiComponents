{********************************************************************}
{* EMSHEAP.PAS - EMS Heap manager                                   *}
{********************************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower B-Tree Filer
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{--Conditional defines and compiler options}
{$I BTDefine.Inc}
{$IFDEF CanAllowOverlays}
  {$O+,F+}
{$ENDIF}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}


{--Definition of mode used for EMS heap}
  (********************************************************************)
  (****************)  { $DEFINE DebugEMSHeap}         (*****************)
  (****************)  { $DEFINE NoErrorCheckEMSHeap}  (*****************)
  (****************)  { $DEFINE ManualInitEMSHeap}    (*****************)
  (********************************************************************)
  {-Possible modes:
    no define at all: a minimum error check is done
    DebugEMSHeap: a lot of checks are performed, especially at initializition;
        use this only in test mode; this overwrites NoErrorCheckEMSHeap
    NoErrorCheckEMSHeap: no error check is done to reach maximum performance}

{--Definition of method how the EMS istalled check is done}
  (********************************************************************)
  (****************)  { $DEFINE StdEMSInstCheck}      (*****************)
  (********************************************************************)
  {-The standard test method can cause problems, so it is deactivated;
    activate this define, when EMS is available but not detected by this
    unit}

{--Definition whether to use OPEMS or TPEMS instead of EMSSUPP}
  (********************************************************************)
  (****************)  { $DEFINE UseTPEMS}             (*****************)
  (****************)  { $DEFINE UseOPEMS}             (*****************)
  (********************************************************************)

{--Correct and check defines}
  {$IFDEF DebugEMSHeap}
    {$UNDEF NoErrorCheckEMSHeap}
  {$ENDIF}
  {$IFDEF UseTPEMS}
    {$IFDEF UseOPEMS}
      ** ERROR **  Cannot define UseTPEMS and UseOPEMS at the same time
    {$ENDIF}
  {$ENDIF}


Unit EMSHeap;

Interface

Uses
{$IFDEF UseOPEMS}
  OPEMS;
{$ELSE}
  {$IFDEF UseTPEMS}
  TPEMS;
  {$ELSE}
  EMSSupp;
  {$ENDIF}
{$ENDIF}

Const
  EMSHeapErrorFuncPtr : Pointer = Nil;
  EMSHardErrorFuncPtr : Pointer = Nil;
  DoManualInitEMSHeap =
    {$IFDEF ManualInitEMSHeap}
      True;
    {$ELSE}
      False;
    {$ENDIF}

Type
  EMSPointer = Pointer;

Var
  EMSHeapInitialized : Boolean;


  Function MapEMSPtr ( EPtr : EMSPointer ) : Pointer;

  Function SaveEMSCtxt : Byte;

  Procedure RestoreEMSCtxt ( HandleInd : Byte );

  Procedure GetEMSMem ( Var EPtr : EMSPointer; Size : Word );

  Procedure FreeEMSMem ( EPtr : EMSPointer; Size : Word );

  Function EMSMemAvail : LongInt;

  Function EMSMaxAvail : Word;

  Procedure InitEMSHeap ( FreePages : Word );

  Procedure ExitEMSHeap;


Implementation

{$I EMSHeap.Cfg}

Const
  NrOfFreeListPages = 4;
  NrOfFreeListEntries = 16383;
  MaxAllocPagesForHandle = 256;
  PhysPageSize = 16384;
  MaxEMSAllocSize = 32768;
  NotMapped = $FF;

Type
  PhysPageDescriptor = Array [0..3] Of Word;

  HandleEntry = Record
    Handle : Word;
    LogPages : Byte;
    UsedForSave : Boolean;
    PhPgDescr : PhysPageDescriptor;
    LRU : Byte;
  End;

  HandleArray = Array [-1..HandlesToUseForAlloc] Of HandleEntry;

  FreeListEntry = Record
    Offset : Word;        {LogPage 15..8, OffsetInPage 7..0}
    LenM1  : Byte;        {64 Bytes : 0, 16384 Bytes : $FF}
    HandleIndex : Byte;
  End;
  FreeList = Array [1..NrOfFreeListEntries] Of FreeListEntry;

  RealEMSPointer = Record
    HandleInds,
    LogPageOfs : Word;
    {Pointers with size > 16384:
     | HandleInd2 15..8 | HandleInd1 7..0 || LogPage1 15..8 | LogPage2 4..0 |
     Pointers with size <= 16384:
     | zero 15..8       | HandleInd1 7..0 || LogPage1 15..8 | Offset1 7..0 |}
  End;

Const
  HeapErrorFuncActive : Boolean = False;

Var
  HandleInfo              : HandleArray;
  FramePtr                : Pointer;
  FramePtrSeg             : Word;
  UsedNrOfFreeListEntries : Word;
  PhysPageDescr           : PhysPageDescriptor;
    {-Word element: | LogPage 12..8 | HandleIndex 7..0 |}
  LRUMap                  : Byte;
    {-Two bits for each entry: oldest 7,6; newest 1,0}


  Function CallHardErrorFunc ( UserRoutine : Pointer; Error : Word ) : Boolean;

    Function CallUserRoutine ( Error : Word ) : Boolean; Inline
      {-Call UserRoutine}
      ( $FF / $5E / <UserRoutine ); {call far dword ptr [bp+<UserRoutine]}

  Begin
    CallHardErrorFunc := CallUserRoutine ( Error );
  End;


  Function CallHeapErrorFunc ( UserRoutine : Pointer; Size : Word ) : Integer;

    Function CallUserRoutine ( Size : Word ) : Integer; Inline
      {-Call UserRoutine}
      ( $FF / $5E / <UserRoutine ); {call far dword ptr [bp+<UserRoutine]}

  Begin
    CallHeapErrorFunc := CallUserRoutine ( Size );
  End;


  Procedure EMSHardError ( Error : Word );
  Begin
    If EMSHardErrorFuncPtr = Nil Then Begin
      Writeln;
      Writeln ( 'EMSHeap: error ', Error );
      Halt ( 240 );
    End Else Begin
      If CallHardErrorFunc ( EMSHardErrorFuncPtr, Error )
          Then Halt ( 240 );
    End;
  End;


  Function EMSHeapError ( Size : Word ) : Integer;

  Begin
    If EMSHeapErrorFuncPtr = Nil Then Begin
      EMSHeapError := 1;
    {$IFDEF DebugEMSHeap}
      Writeln;
      Writeln;
      Writeln;
      Writeln ( 'EMSHeap: not available' );
      EMSHardError ( Size );
    {$ENDIF}
    End Else Begin
      HeapErrorFuncActive := True;
      EMSHeapError := CallHeapErrorFunc ( EMSHeapErrorFuncPtr, Size );
      HeapErrorFuncActive := False;
    End;
  End;


  Procedure DeAlloc ( ToNr : Integer );

  Var
    I     : Integer;
    Dummy : Boolean;

  Begin
    For I := ToNr DownTo -1 Do Begin
      With HandleInfo [I] Do Begin
        If UsedForSave Then Begin
          Dummy := RestoreEMSContext ( Handle );
        End;
        Dummy := DeAllocateEMSHandle ( Handle );
      End;
    End;
  End;


  Function MapFreeList : Boolean;

  Begin
    If HeapErrorFuncActive Then Begin
      {-FreeList already mapped}
      MapFreeList := True;
      Exit;
    End;
    MapFreeList := False;
    If Not SaveEMSContext ( HandleInfo [-1].Handle ) Then Begin
      EMSHardError ( 100 );
      Exit;
    End;
    HandleInfo [-1].UsedForSave := True;
    If Not RestoreEMSContext ( HandleInfo [0].Handle ) Then Begin
      EMSHardError ( 101 );
      Exit;
    End;
    HandleInfo [0].UsedForSave := False;
    MapFreeList := True;
  End;


  Procedure UnmapFreeList;

  Begin
    If HeapErrorFuncActive Then Exit;
      {-FreeList already mapped, don't change that}
    If Not SaveEMSContext ( HandleInfo [0].Handle ) Then Begin
      EMSHardError ( 102 );
      Exit;
    End;
    HandleInfo [0].UsedForSave := True;
    If Not RestoreEMSContext ( HandleInfo [-1].Handle ) Then Begin
      EMSHardError ( 103 );
      Exit;
    End;
    HandleInfo [-1].UsedForSave := False;
  End;


{$IFDEF DebugEMSHeap}
  Procedure InitFail ( Error : Word );

  Begin
    Writeln;
    Writeln;
    Writeln ( 'EMSHeap init error: ', error );
    Readln;
  End;
{$ENDIF}

  Function DoInit ( FreePages : Word ) : Boolean;

  Type
    OfsSeg = Record
      O, S : Word;
    End;

  Var
    PerHandleAllocSize : LongInt;
    NrOfPages          : Integer;
    AllocNrOfHandles,
    ActPrivatHandle,
    PagesLeft,
    W                  : Word;
    Ok,
    PreInitFail        : Boolean;

  {$IFDEF UseTPEMS}
    Function EmmSigFound : Boolean;

    Type
      NameArr = Array [1..8] Of Char;

    Const
      EMMName : NameArr = 'EMMXXXX0';

    Var
      EMMIntPtr : Pointer Absolute $0000:$019C;

    Begin
      If EMMIntPtr = Nil Then Begin
        EmmSigFound := False;
      End Else Begin
        EmmSigFound := EMMName = NameArr (Ptr ( Seg (EMMIntPtr^), $0A )^);
      End;
    End;
  {$ENDIF}


  Begin
    DoInit := False;
    If FreePages < ToLetFreePages Then FreePages := ToLetFreePages;

  {$IFDEF DPMIOrWnd}
   {$IFDEF DebugEMSHeap}
    InitFail ( 21 );
   {$ENDIF}
    Exit;
  {$ELSE}
    Ok := EmmSigFound;
  {$ENDIF}

  {$IFDEF StdEMSInstCheck}
    If Not Ok Then OK := EMSInstalled;
  {$ENDIF}
    If Not OK Then Begin
    {$IFDEF DebugEMSHeap}
      InitFail ( 1 );
    {$ENDIF}
      Exit;
    End;

    If EMSVersion < $32 Then Begin
    {$IFDEF DebugEMSHeap}
      InitFail ( 2 );
    {$ENDIF}
      Exit;
    End;

    FramePtr := EMSPageFramePtr;
    If FramePtr = Nil Then Begin
    {$IFDEF DebugEMSHeap}
      InitFail ( 3 );
    {$ENDIF}
      Exit;
    End;
    FramePtrSeg := OfsSeg (FramePtr).S;
    If Ofs (FramePtr^) <> 0 Then Begin
    {$IFDEF DebugEMSHeap}
      InitFail ( 20 );
    {$ENDIF}
      Exit;
    End;

    NrOfPages := EMSPagesAvail;
    If NrOfPages = EMSErrorCode Then Begin
    {$IFDEF DebugEMSHeap}
      InitFail ( 4 );
    {$ENDIF}
      Exit;
    End;
    Dec (NrOfPages, FreePages + NrOfFreeListPages);

    If HandlesToUseForAlloc < MinEMSHeapPages Then Begin
    {$IFDEF DebugEMSHeap}
      InitFail ( 5 );
    {$ENDIF}
      Exit;
    End;

    If NrOfPages < MinEMSHeapPages Then Begin
    {$IFDEF DebugEMSHeap}
      InitFail ( 6 );
    {$ENDIF}
      Exit;
    End;
    If NrOfPages > MaxEMSHeapPages Then NrOfPages := MaxEMSHeapPages;
    UsedNrOfFreeListEntries := NrOfPages;

    PerHandleAllocSize := NrOfPages Div HandlesToUseForAlloc;
    AllocNrOfHandles := HandlesToUseForAlloc -
        ( NrOfPages - PerHandleAllocSize * HandlesToUseForAlloc );
    W := PerHandleAllocSize;
    If AllocNrOfHandles <= HandlesToUseForAlloc Then Inc (W);
    If W > MaxAllocPagesForHandle Then Begin
    {$IFDEF DebugEMSHeap}
      InitFail ( 7 );
    {$ENDIF}
      Exit;
    End;

    With HandleInfo [-1] Do Begin
      Handle := AllocateEMSPages ( 1 );
      If Handle = EMSErrorCode Then Begin
      {$IFDEF DebugEMSHeap}
        InitFail ( 8 );
      {$ENDIF}
        Exit;
      End;
      LogPages := 1;
      UsedForSave := False;
    End;
    With HandleInfo [0] Do Begin
      Handle := AllocateEMSPages ( Pred (NrOfFreeListPages) );
      If Handle = EMSErrorCode Then Begin
        DeAlloc ( -1 );
      {$IFDEF DebugEMSHeap}
        InitFail ( 8 );
      {$ENDIF}
        Exit;
      End;
      LogPages := Pred (NrOfFreeListPages);
      UsedForSave := False;
    End;

    For W := 1 To HandlesToUseForAlloc Do Begin
      With HandleInfo [W] Do Begin
        If W = Succ (AllocNrOfHandles) Then Inc (PerHandleAllocSize);
        Handle := AllocateEMSPages ( PerHandleAllocSize );
        If Handle = EMSErrorCode Then Begin
          DeAlloc ( Pred (W) );
        {$IFDEF DebugEMSHeap}
          InitFail ( 8 );
        {$ENDIF}
          Exit;
        End;
        LogPages := PerHandleAllocSize;
        UsedForSave := False;
      End;
    End;

    If Not SaveEMSContext ( HandleInfo [-1].Handle ) Then Begin
      DeAlloc ( HandlesToUseForAlloc );
    {$IFDEF DebugEMSHeap}
      InitFail ( 9 );
    {$ENDIF}
      Exit;
    End;
    HandleInfo [-1].UsedForSave := True;
    If Not MapEMSPage ( HandleInfo [-1].Handle, 0, 0 ) Then Begin
      DeAlloc ( HandlesToUseForAlloc );
    {$IFDEF DebugEMSHeap}
      InitFail ( 10 );
    {$ENDIF}
      Exit;
    End;
    For W := 1 To Pred (NrOfFreeListPages) Do Begin
      If Not MapEMSPage ( HandleInfo [0].Handle, Pred (W), W ) Then Begin
        DeAlloc ( HandlesToUseForAlloc );
      {$IFDEF DebugEMSHeap}
        InitFail ( 10 );
      {$ENDIF}
        Exit;
      End;
    End;

    ActPrivatHandle := 1;
    PagesLeft := HandleInfo [ActPrivatHandle].LogPages;
    PreInitFail := False;
    For W := 1 To NrOfFreeListEntries Do Begin
      If W <= NrOfPages Then Begin
        If PreInitFail Then Begin
          DeAlloc ( HandlesToUseForAlloc );
        {$IFDEF DebugEMSHeap}
          InitFail ( 11 );
        {$ENDIF}
          Exit;
        End;
        With FreeList (FramePtr^) [W] Do Begin
          HandleIndex := ActPrivatHandle;
          Offset :=
              Word ((HandleInfo [ActPrivatHandle].LogPages) - PagesLeft) Shl 8;
          LenM1 := $FF;
        End;
        If PagesLeft = 1 Then Begin
          If ActPrivatHandle < HandlesToUseForAlloc Then Begin
            Inc (ActPrivatHandle);
            PagesLeft := HandleInfo [ActPrivatHandle].LogPages;
          End Else Begin
            PreInitFail := True;
          End;
        End Else Begin
          Dec (PagesLeft);
        End;
      End Else Begin
        With FreeList (FramePtr^) [W] Do Begin
          HandleIndex := 0; {recognize unused entry}
          Offset := 0;
          LenM1 := 0;
        End;
      End;
    End;

    If Not SaveEMSContext ( HandleInfo [0].Handle ) Then Begin
      DeAlloc ( HandlesToUseForAlloc );
    {$IFDEF DebugEMSHeap}
      InitFail ( 12 );
    {$ENDIF}
      Exit;
    End;
    HandleInfo [0].UsedForSave := True;
    If Not RestoreEMSContext ( HandleInfo [-1].Handle ) Then Begin
      DeAlloc ( HandlesToUseForAlloc );
    {$IFDEF DebugEMSHeap}
      InitFail ( 13 );
    {$ENDIF}
      Exit;
    End;
    HandleInfo [-1].UsedForSave := False;

    LRUMap := $1B;   {bit coded: 00011011}
    For W := 0 To 3 Do Begin
      PhysPageDescr [W] := 0;
    End;

    DoInit := True;
  End;


  Function MapEMSPtr ( EPtr : EMSPointer ) : Pointer;
    {-FreeList must not be mapped on call}
  Type
    TCEMSPtr = Record
      HandleIndsLow,
      Dummy1         : Byte;
      TCLogPageOfs   : Word;
    End;

  Var
    RealEMSPtr      : RealEMSPointer Absolute EPtr;
    MapSmall,
    MapBig          : Byte;
    DescriptorSmall,
    DescriptorBig   : Word;


    Function EvenToNewest : Byte;
        Inline (
      $30 / $E4 /                                      {XOR AH, AH}
      $A0 / LRUMap /                                   {MOV AL, [LRUMap]}
      $89 / $C2 /                                      {MOV DX, AX}
      $D1 / $E0 /                                      {SHL AX, 1}
      $D1 / $E0 /                                      {SHL AX, 1}
      $F6 / $C4 / $01 /                                {TEST AH, 01}
      $75 / $11 /                                      {JNZ Cont1:}
      $88 / $D5 /                                      {MOV CH, DL}
      $88 / $E1 /                                      {MOV CL, AH}
      $81 / $E1 / $03 / $03 /                          {AND CX, 0303}
      $41 /                                            {INC CX}
      $38 / $E9 /                                      {CMP CL, CH}
      $74 / $04 /                                      {JZ Cont1:}
      $08 / $E0 /                                      {OR AL, AH}
      $EB / $3E /                                      {JMP OK:}
    {Cont1:}
      $D1 / $E0 /                                      {SHL AX, 1}
      $D1 / $E0 /                                      {SHL AX, 1}
      $F6 / $C4 / $01 /                                {TEST AH, 01}
      $75 / $1E /                                      {JNZ Cont2:}
      $88 / $D5 /                                      {MOV CH, DL}
      $88 / $E1 /                                      {MOV CL, AH}
      $81 / $E1 / $03 / $03 /                          {AND CX, 0303}
      $41 /                                            {INC CX}
      $38 / $E9 /                                      {CMP CL, CH}
      $74 / $11 /                                      {JZ Cont2:}
      $88 / $E2 /                                      {MOV DL, AH}
      $80 / $E2 / 03 /                                 {AND DL, 03}
      $D0 / $EC /                                      {SHR AH, 1}
      $D0 / $EC /                                      {SHR AH, 1}
      $D1 / $E8 /                                      {SHR AX, 1}
      $D1 / $E8 /                                      {SHR AX, 1}
      $08 / $D0 /                                      {OR AL, DL}
      $EB / $17 /                                      {JMP OK:}
    {Cont2:}
      $88 / $D4 /                                      {MOV AH, DL}
      $B1 / $04 /                                      {MOV CL, 04}
      $D3 / $E8 /                                      {SHR AX, CL}
      $D0 / $E4 /                                      {SHL AH, 1}
      $D0 / $E4 /                                      {SHL AH, 1}
      $80 / $E2 / 03 /                                 {AND DL, 03}
      $08 / $D4 /                                      {OR AH, DL}
      $D1 / $E0 /                                      {SHL AX, 1}
      $D1 / $E0 /                                      {SHL AX, 1}
      $88 / $E0 /                                      {MOV AL, AH}
      $30 / $E4 /                                      {XOR AH, AH}
    {OK:}
      $A2 / LRUMap /                                   {MOV [LRUMap], AL}
      $24 / $03                                        {AND AL, 03}
    );


    Function OldestToNewest : Byte;
        Inline (
      $A0 / LRUMap /                                   {MOV AL, [LRUMap]}
      $D0 / $C0 /                                      {ROL AL, 1}
      $D0 / $C0 /                                      {ROL AL, 1}
      $A2 / LRUMap /                                   {MOV [LRUMap], AL}
      $24 / $03                                        {AND AL, 03}
    );


    Procedure ValueToNewest ( Value : Byte );

    Begin
      Inline (
        $8A / $9E / Value /                            {MOV BL, [BP+Value]}
        $30 / $E4 /                                    {XOR AH, AH}
        $A0 / LRUMap /                                 {MOV AL, [LRUMap]}
        $89 / $C2 /                                    {MOV DX, AX}
        $24 / $03 /                                    {AND AL, 03}
        $38 / $D8 /                                    {CMP AL, BL}
        $74 / $3F /                                    {JZ DoNothing:}
        $89 / $D0 /                                    {MOV AX, DX}
        $D1 / $E0 /                                    {SHL AX, 1}
        $D1 / $E0 /                                    {SHL AX, 1}
        $38 / $DC /                                    {CMP AH, BL}
        $75 / $04 /                                    {JNZ Cont1:}
        $08 / $E0 /                                    {OR AL, AH}
        $EB / $2E /                                    {JMP OK:}
      {Cont1:}
        $D1 / $E0 /                                    {SHL AX, 1}
        $D1 / $E0 /                                    {SHL AX, 1}
        $88 / $E1 /                                    {MOV CL, AH}
        $80 / $E1 / 03 /                               {AND CL, 03}
        $38 / $D9 /                                    {CMP CL, BL}
        $75 / $0C /                                    {JNZ Cont2:}
        $D0 / $EC /                                    {SHR AH, 1}
        $D0 / $EC /                                    {SHR AH, 1}
        $D1 / $E8 /                                    {SHR AX, 1}
        $D1 / $E8 /                                    {SHR AX, 1}
        $08 / $C8 /                                    {OR AL, CL}
        $EB / $15 /                                    {JMP OK:}
      {Cont2:}
        $88 / $D4 /                                    {MOV AH, DL}
        $B1 / $04 /                                    {MOV CL, 04}
        $D3 / $E8 /                                    {SHR AX, CL}
        $D0 / $E4 /                                    {SHL AH, 1}
        $D0 / $E4 /                                    {SHL AH, 1}
        $80 / $E2 / 03 /                               {AND DL, 03}
        $08 / $D4 /                                    {OR AH, DL}
        $D1 / $E0 /                                    {SHL AX, 1}
        $D1 / $E0 /                                    {SHL AX, 1}
        $88 / $E0 /                                    {MOV AL, AH}
      {OK:}
        $A2 / LRUMap                                   {MOV [LRUMap], AL}
      {DoNothing:}
      );
    End;


    Function AlreadyMapped ( Descr : Word ) : Byte;
        Inline (
      $5B /                                     {POP BX}
      $31 / $C0 /                               {XOR AX, AX}
      $3B / $1E / PhysPageDescr /               {CMP BX, PhysPageDescr [0]}
      $74 / $17 /                               {JZ OK:}
      $40 /                                     {INC AX}
      $3B / $1E / PhysPageDescr + 2 /           {CMP BX, PhysPageDescr [1]}
      $74 / $10 /                               {JZ OK:}
      $40 /                                     {INC AX}
      $3B / $1E / PhysPageDescr + 4 /           {CMP BX, PhysPageDescr [2]}
      $74 / $09 /                               {JZ OK:}
      $40 /                                     {INC AX}
      $3B / $1E / PhysPageDescr + 6 /           {CMP BX, PhysPageDescr [3]}
      $74 / $02 /                               {JZ OK:}
      $B0 / NotMapped                           {MOV AL, NotMapped}
    {OK:}
    );


    Function SmallOffset : Word;
        Inline (
      $8A / $46 / < RealEMSPtr + 2 /            {MOV AL, [BP+RealEMSPtr]}
      $8A / $66 / < MapSmall /                  {MOV AH, [BP+MapSmall]}
      $B1 / $06 /                               {MOV CL, 06}
      $D3 / $E0                                 {SHL AX, CL}
    );


    Function BigOffset : Word;
        Inline (
      $31 / $C0 /                               {XOR AX, AX}
      $8A / $66 / < MapBig /                    {MOV AH, [BP+MapBig]}
      $D0 / $CC /                               {ROR AH, 1}
      $D0 / $CC                                 {ROR AH, 1}
    );


  Procedure FourStepChange ( BigPos, SmallPos, TempPos : Byte );

  Var
    TempDescr : Word;

  Begin
    If Not MapEMSPage ( HandleInfo [-1].Handle, 0, BigPos ) Then Begin
      EMSHardError ( 146 );
      Exit;
    End;
    If Not MapEMSPage ( HandleInfo [Byte (PhysPageDescr [BigPos])].Handle,
        PhysPageDescr [BigPos] Shr 8, SmallPos ) Then Begin
      EMSHardError ( 146 );
      Exit;
    End;
    If Not MapEMSPage ( HandleInfo [Byte (PhysPageDescr [SmallPos])].Handle,
        PhysPageDescr [SmallPos] Shr 8, TempPos ) Then Begin
      EMSHardError ( 146 );
      Exit;
    End;
    If Not MapEMSPage ( HandleInfo [Byte (PhysPageDescr [TempPos])].Handle,
        PhysPageDescr [TempPos] Shr 8, BigPos ) Then Begin
      EMSHardError ( 146 );
      Exit;
    End;
    TempDescr := PhysPageDescr [BigPos];
    PhysPageDescr [BigPos] := PhysPageDescr [TempPos];
    PhysPageDescr [TempPos] := PhysPageDescr [SmallPos];
    PhysPageDescr [SmallPos] := TempDescr;
  End;


  Procedure ThreeStepChange ( MovePos, PivotPos : Byte );

  Var
    TempDescr : Word;

  Begin
    If Not MapEMSPage ( HandleInfo [-1].Handle, 0, MovePos ) Then Begin
      EMSHardError ( 146 );
      Exit;
    End;
    If Not MapEMSPage ( HandleInfo [Byte (PhysPageDescr [MovePos])].Handle,
        PhysPageDescr [MovePos] Shr 8, PivotPos ) Then Begin
      EMSHardError ( 146 );
      Exit;
    End;
    If Not MapEMSPage ( HandleInfo [Byte (PhysPageDescr [PivotPos])].Handle,
        PhysPageDescr [PivotPos] Shr 8, MovePos ) Then Begin
      EMSHardError ( 146 );
      Exit;
    End;
    TempDescr := PhysPageDescr [MovePos];
    PhysPageDescr [MovePos] := PhysPageDescr [PivotPos];
    PhysPageDescr [PivotPos] := TempDescr;
  End;


  Begin
  {$IFNDEF NoErrorCheckEMSHeap}
    If Not EMSHeapInitialized Then Begin
      EMSHardError ( 50 );
      Exit;
    End;
    If HeapErrorFuncActive Then Begin
      EMSHardError ( 60 );
      Exit;
    End;
  {$ENDIF}
  {$IFDEF DebugEMSHeap}
    If EPtr = Nil Then Begin
      EMSHardError ( 150 );
      Exit;
    End;
  {$ENDIF}
    With RealEMSPtr Do Begin
      DescriptorSmall := (RealEMSPtr.LogPageOfs And $FF00)
          Or TCEMSPtr (RealEMSPtr).HandleIndsLow;
      MapSmall := AlreadyMapped ( DescriptorSmall );
      If MapSmall <> NotMapped Then Begin
        {-Second part (small part) already mapped to MapSmall}
        If (HandleInds And $FF00) <> 0 Then Begin
          {-Two pages}
          DescriptorBig := (HandleInds Shr 8) Or (LogPageOfs Shl 8);
          MapBig := AlreadyMapped ( DescriptorBig );
          If MapBig <> NotMapped Then Begin
            {-First part (big part) already mapped to MapBig}
            If Odd (MapBig) Then Begin
              {-Both parts mapped, but first part must be on an even
                position followed by the second}
              If MapBig = 1 Then Begin
                Case MapSmall Of
                  0 : Begin
                    ThreeStepChange ( 1, 0 );
                    MapBig := 0;
                  End;
                  2 : Begin
                    FourStepChange ( 1, 0, 2 );
                    MapBig := 0;
                  End;
                  3 : Begin
                    ThreeStepChange ( 1, 2 );
                    MapBig := 2;
                  End;
                End; {Case}
              End Else Begin
                {-MapBig is 3}
                Case MapSmall Of
                  0 : Begin
                    FourStepChange ( 3, 0, 1 );
                    MapBig := 0;
                  End;
                  1 : Begin
                    ThreeStepChange ( 3, 0 );
                    MapBig := 0;
                  End;
                  2 : Begin
                    ThreeStepChange ( 3, 2 );
                    MapBig := 2;
                  End;
                End; {Case}
              End;
              MapSmall := Succ (MapBig);
            End Else Begin
              If Succ (MapBig) <> MapSmall Then Begin
                {-Both parts mapped, but second part must follow first;
                  avoid mapping one page two times at the same time}
                If MapBig = 0 Then Begin
                  Case MapSmall Of
                    2 : Begin
                      ThreeStepChange ( 2, 1 );
                    End;
                    3 : Begin
                      ThreeStepChange ( 3, 1 );
                    End;
                  End; {Case}
                End Else Begin
                  {-MapBig is 2}
                  Case MapSmall Of
                    0 : Begin
                      FourStepChange ( 2, 0, 1 );
                      MapBig := 0;
                    End;
                    1 : Begin
                      ThreeStepChange ( 2, 0 );
                      MapBig := 0;
                    End;
                  End; {Case}
                End;
                MapSmall := Succ (MapBig);
              End;
            End;
            ValueToNewest ( MapBig );
            ValueToNewest ( MapSmall );
            MapEMSPtr := Ptr ( FramePtrSeg, BigOffset );
          End Else Begin
            {-First part (big part) not mapped yet}
            If Odd (MapSmall) Then Begin
              {-OK, first part (big part) can be mapped underneath}
              MapBig := Pred (MapSmall);
              If Not MapEMSPage ( HandleInfo [Byte (DescriptorBig)].Handle,
                  DescriptorBig Shr 8, MapBig ) Then Begin
                EMSHardError ( 140 );
                Exit;
              End;
              ValueToNewest ( MapBig );
              PhysPageDescr [MapBig] := DescriptorBig;
              ValueToNewest ( MapSmall );
              PhysPageDescr [MapSmall] := DescriptorSmall;
              MapEMSPtr := Ptr ( FramePtrSeg, BigOffset );
            End Else Begin
              {-Solve problem, and map first part (big part) to MapSmall on
                an even page number, and map second part (small part) above}
              MapBig := MapSmall;
              If Not MapEMSPage ( HandleInfo [Byte (DescriptorBig)].Handle,
                  DescriptorBig Shr 8, MapBig ) Then Begin
                EMSHardError ( 141 );
                Exit;
              End;
              ValueToNewest ( MapBig );
              PhysPageDescr [MapBig] := DescriptorBig;
              MapSmall := Succ (MapBig);
              If Not MapEMSPage ( HandleInfo [Byte (DescriptorSmall)].Handle,
                  DescriptorSmall Shr 8, MapSmall ) Then Begin
                EMSHardError ( 142 );
                Exit;
              End;
              ValueToNewest ( MapSmall );
              PhysPageDescr [MapSmall] := DescriptorSmall;
              MapEMSPtr := Ptr ( FramePtrSeg, BigOffset );
            End;
          End;
        End Else Begin
          {-Only one page, and already mapped}
          ValueToNewest ( MapSmall );
          MapEMSPtr := Ptr ( FramePtrSeg,  SmallOffset );
        End;
      End Else Begin
        {-Second part not mapped yet}
        If (HandleInds And $FF00) <> 0 Then Begin
          {-Two pages}
          DescriptorBig := (HandleInds Shr 8) Or (LogPageOfs Shl 8);
          MapBig := AlreadyMapped ( DescriptorBig );
          If MapBig <> NotMapped Then Begin
            {-First part (big part) already mapped}
            If Odd (MapBig) Then Begin
              {-First part misplaced, correct this}
              If Not MapEMSPage ( HandleInfo [-1].Handle, 0, MapBig )
                  Then Begin
                EMSHardError ( 147 );
                Exit;
              End;
              Dec (MapBig);
                {-Make it even}
              If Not MapEMSPage ( HandleInfo [Byte (DescriptorBig)].Handle,
                  DescriptorBig Shr 8, MapBig ) Then Begin
                EMSHardError ( 148 );
                Exit;
              End;
              PhysPageDescr [MapBig] := DescriptorBig;
            End;
            ValueToNewest ( MapBig );
              {-Make it the newest}
          End Else Begin
            {-Map first part (big part)}
            MapBig := EvenToNewest;
            If Not MapEMSPage ( HandleInfo [Byte (DescriptorBig)].Handle,
                DescriptorBig Shr 8, MapBig ) Then Begin
              EMSHardError ( 143 );
              Exit;
            End;
            PhysPageDescr [MapBig] := DescriptorBig;
          End;
          MapSmall := Succ (MapBig);
          If Not MapEMSPage ( HandleInfo [Byte (DescriptorSmall)].Handle,
              DescriptorSmall Shr 8, MapSmall ) Then Begin
            EMSHardError ( 144 );
            Exit;
          End;
          ValueToNewest ( MapSmall );
          PhysPageDescr [MapSmall] := DescriptorSmall;
          MapEMSPtr := Ptr ( FramePtrSeg, BigOffset );
        End Else Begin
          {-Only one page}
          MapSmall := OldestToNewest;
          If Not MapEMSPage ( HandleInfo [Byte (DescriptorSmall)].Handle,
              DescriptorSmall Shr 8, MapSmall ) Then Begin
            EMSHardError ( 145 );
            Exit;
          End;
          PhysPageDescr [MapSmall] := DescriptorSmall;
          MapEMSPtr := Ptr ( FramePtrSeg,  SmallOffset );
        End;
      End;
    End;
  End;


  Function SaveEMSCtxt : Byte;

  Var
    W : Word;

  Begin
    SaveEMSCtxt := 255;
  {$IFNDEF NoErrorCheckEMSHeap}
    If Not EMSHeapInitialized Then Begin
      EMSHardError ( 50 );
      Exit;
    End;
  {$ENDIF}
    For W := 1 To HandlesToUseForAlloc Do Begin
      With HandleInfo [W] Do Begin
        If Not UsedForSave Then Begin
          If SaveEMSContext ( Handle ) Then Begin
            SaveEMSCtxt := W;
          End Else Begin
            EMSHardError ( 110 );
          End;
          UsedForSave := True;
          PhPgDescr := PhysPageDescr;
          LRU := LRUMap;
          Exit;
        End;
      End;
    End;
    EMSHardError ( 111 );
  End;


  Procedure RestoreEMSCtxt ( HandleInd : Byte );

  Begin
  {$IFNDEF NoErrorCheckEMSHeap}
    If Not EMSHeapInitialized Then Begin
      EMSHardError ( 50 );
      Exit;
    End;
  {$ENDIF}
    With HandleInfo [HandleInd] Do Begin
      If Not RestoreEMSContext ( Handle ) Then Begin
        EMSHardError ( 112 );
        Exit;
      End;
      UsedForSave := False;
      PhysPageDescr := PhPgDescr;
      LRUMap := LRU;
    End;
  End;


  Procedure FindNextHole ( Var Size, FreeLInd : Word; OffsetZero : Boolean );
    {-FreeList must be mapped on call}
  Var
    W,
    BlockSize,
    FoundSize : Word;

  Begin
    If FreeLInd >= UsedNrOfFreeListEntries Then Begin
      FreeLInd := 0;
      Exit;
    End;
    W := FreeLInd;
    If (Size And $003F) <> 0 Then Begin
      BlockSize := ((Size + $0040) And $FFC0) Shr 6;
    End Else Begin
      BlockSize := Size Shr 6;
    End;
    If BlockSize > 0 Then Dec (BlockSize);
      {-Set BlockSize to granulity and adjust to LenM1}
    Repeat
      Inline (
        $C4 / $3E / FramePtr /                        {LES DI, [FramePtr]}
        $83 / $C7 / $02 /                             {ADD DI, 02}
        $8B / $86 / W /                               {MOV AX, [BP+W]}
        $8B / $0E / UsedNrOfFreeListEntries /         {MOV CX, [UsedNrOf..]}
        $89 / $CB /                                   {MOV BX, CX}
        $29 / $C1 /                                   {SUB CX, AX}
        $76 / $16 /                                   {JBE EndLoop:}
        $D1 / $E0 /                                   {SHL AX, 1}
        $D1 / $E0 /                                   {SHL AX, 1}
        $01 / $C7 /                                   {ADD DI, AX}
        $8A / $96 / BlockSize /                       {MOV DL, [BP+BlockSize]}
      {Scan:}
        $26 / $8B / $05 /                             {MOV AX, ES:[DI]}
        $38 / $D0 /                                   {CMP AL, DL}
        $73 / $05 /                                   {JNB EndLoop:}
        $83 / $C7 / $04 /                             {ADD DI, 04}
        $E2 / $F4 /                                   {LOOP Scan:}
      {EndLoop:}
        $29 / $CB /                                   {SUB BX, CX}
        $43 /                                         {INC BX}
        $89 / $9E / W /                               {MOV [BP+W], BX}
        $25 / $FF / $00 /                             {AND AX, 00FF}
        $40 /                                         {INC AX}
        $89 / $86 / FoundSize                         {MOV [BP+FoundSize], AX}
      );
      If W > UsedNrOfFreeListEntries Then Begin
        FreeLInd := 0;
        Exit;
      End;
      If Not OffsetZero
          Or ((FreeList (FramePtr^) [W].Offset And $00FF) = 0) Then Begin
        Size := FoundSize Shl 6;
        FreeLInd := W;
        Exit;
      End;
    Until False;
  End;


  Procedure GetEMSMem ( Var EPtr : EMSPointer; Size : Word );

  Var
    FreeLInd1,
    FreeLInd2 : Word;
    RepeatIt  : Boolean;


    Procedure BuildPointerAdjFreeList (     Size,
                                            FreeLInd1,
                                            FreeLInd2 : Word;
                                        Var EPtr      : EMSPointer );
      {-FreeList must be mapped on call}
    Var
      RealEMSPtr : RealEMSPointer Absolute EPtr;
      UsedBlks   : Word;

    Begin
      With RealEMSPtr Do Begin
        If Size > PhysPageSize Then Begin
          With FreeList (FramePtr^) [FreeLInd2] Do Begin
            HandleInds := Word (HandleIndex) Shl 8;
            LogPageOfs := Offset Shr 8;
              {-Set big part of pointer}
          End;
          If FreeLInd1 = UsedNrOfFreeListEntries Then FreeLInd1 := FreeLInd2;
            {-Correct following movement of entry FreeLInd1}
          FreeList (FramePtr^) [FreeLInd2] :=
              FreeList (FramePtr^) [UsedNrOfFreeListEntries];
          Dec (UsedNrOfFreeListEntries);
          UsedBlks := (Size - PhysPageSize) Shr 6;
          With FreeList (FramePtr^) [FreeLInd1] Do Begin
            HandleInds := HandleInds Or HandleIndex;
            LogPageOfs := LogPageOfs Or (Offset And $FF00);
              {-Set small part of pointer}
            If UsedBlks > LenM1 Then Begin
              FreeList (FramePtr^) [FreeLInd1] :=
                  FreeList (FramePtr^) [UsedNrOfFreeListEntries];
              Dec (UsedNrOfFreeListEntries);
            End Else Begin
              Dec (LenM1, UsedBlks);
              Inc (Offset, UsedBlks);
            End;
          End;
        End Else Begin
          With FreeList (FramePtr^) [FreeLInd1] Do Begin
            HandleInds := HandleIndex;
            UsedBlks := Size Shr 6;
            LogPageOfs := Offset + Succ (Word (LenM1)) - UsedBlks;
              {-Take end of hole}
            If UsedBlks > LenM1 Then Begin
              FreeList (FramePtr^) [FreeLInd1] :=
                  FreeList (FramePtr^) [UsedNrOfFreeListEntries];
              Dec (UsedNrOfFreeListEntries);
            End Else Begin
              Dec (LenM1, UsedBlks);
            End;
          End;
        End;
      End;
    End;


    Procedure FindFreeListEntries (     Size      : Word;
                                    Var FreeLInd1,
                                        FreeLInd2 : Word );
      {-FreeList must be mapped on call; Size must be <= 32768;
        FreeLInd1 = 0 on return indicates nothing found}
    Var
      SaveSize,
      FirstSize,
      RestSize  : Word;

    Begin
      FreeLInd1 := 0;
      FreeLInd2 := 0;
      If Size > PhysPageSize Then Begin
        RestSize := Size - PhysPageSize;
        FirstSize := PhysPageSize;
      End Else Begin
        RestSize := 0;
        FirstSize := Size;
      End;
      FindNextHole ( FirstSize, FreeLInd1, False );
      If FreeLInd1 = 0 Then Exit;
        {-No hole found}
      If RestSize > 0 Then Begin
        FreeLInd2 := FreeLInd1;
        FreeLInd1 := 0;
        SaveSize := RestSize;
        FindNextHole ( RestSize, FreeLInd1, True );
        If FreeLInd1 = FreeLInd2 Then Begin
          FindNextHole ( SaveSize, FreeLInd1, True );
        End;
      End;
    End;


  Begin
    EPtr := Nil;
  {$IFNDEF NoErrorCheckEMSHeap}
    If Not EMSHeapInitialized Then Begin
      EMSHardError ( 50 );
      Exit;
    End;
    If Size > MaxEMSAllocSize Then Begin
      EMSHardError ( 120 );
      Exit;
    End;
  {$ENDIF}
    If Size = 0 Then Exit;
    If (Size And $003F) <> 0 Then Begin
      Size := (Size + $0040) And $FFC0;
        {-Adjust Size to granulity}
    End;
    If Not MapFreeList Then Exit;
    Repeat
      RepeatIt := False;
      FindFreeListEntries ( Size, FreeLInd1, FreeLInd2 );
      If FreeLInd1 = 0 Then Begin
        Case EMSHeapError ( Size ) Of
          0 : Begin
            EMSHardError ( 121 );
          End;
          1 :;
          2 : Begin
            RepeatIt := True;
          End;
        End; {Case}
      End Else Begin
        BuildPointerAdjFreeList ( Size, FreeLInd1, FreeLInd2, EPtr );
      End;
    Until Not RepeatIt;
    UnmapFreeList;
  End;


  Procedure FreeEMSMem ( EPtr : EMSPointer; Size : Word );

  Var
    RealEMSPtr         : RealEMSPointer Absolute EPtr;
    UpperBindFreeLInd,
    UpperHoleStart,
    RealOfs,
    BlockSize          : Word;
    Binded             : Boolean;


    Function IncUsedNrOfFreeListEntries : Boolean;

    Begin
      If UsedNrOfFreeListEntries >= NrOfFreeListEntries Then Begin
        IncUsedNrOfFreeListEntries := False;
      End Else Begin
        IncUsedNrOfFreeListEntries := True;
        Inc (UsedNrOfFreeListEntries);
      End;
    End;


    Procedure FindFreeBinder ( Var FLBinderInd : Word;
                                   HandleInd,
                                   LogPage,
                                   StartStop   : Byte;
                                   AddLen      : Boolean );

    Var
      W : Word;

    Begin
      FLBinderInd := 0;
      If UsedNrOfFreeListEntries = 0 Then Exit;
      Inline (
        $C4 / $3E / FramePtr /                        {LES DI, [FramePtr]}
        $8B / $0E / UsedNrOfFreeListEntries /         {MOV CX, [UsedNrOf..]}
        $8A / $96 / HandleInd /                       {MOV DL, [BP+HandleInd]}
        $8A / $B6 / LogPage /                         {MOV DH, [BP+LogPage]}
        $8A / $9E / StartStop /                       {MOV BL, [BP+StartStop]}
        $8A / $BE / AddLen /                          {MOV BH, [BP+AddLen]}
      {Scan:}
        $26 / $3A / $55 / $03 /                       {CMP DL, ES:[DI+3]}
        $75 / $17 /                                   {JNZ Search:}
        $26 / $3A / $75 / $01 /                       {CMP DH, ES:[DI+1]}
        $75 / $11 /                                   {JNZ Search:}
        $26 / $8A / $05 /                             {MOV AL, ES:[DI]}
        $08 / $FF /                                   {OR BH, BH}
        $74 / $06 /                                   {JZ NoAddLen:}
        $26 / $02 / $45 / $02 /                       {ADD AL, ES:[DI+2]}
        $FE / $C0 /                                   {INC AL}
      {NoAddLen:}
        $38 / $D8 /                                   {CMP AL, BL}
        $74 / $05 /                                   {JZ EndLoop:}
        $83 / $C7 / $04 /                             {ADD DI, 04}
        $E2 / $DE /                                   {LOOP Scan:}
      {EndLoop:}
        $8B / $1E / UsedNrOfFreeListEntries /         {MOV BX, [UsedNrOf..]}
        $29 / $CB /                                   {SUB BX, CX}
        $43 /                                         {INC BX}
        $89 / $9E / W                                 {MOV [BP+W], BX}
      );
      If W <= UsedNrOfFreeListEntries Then Begin
        FLBinderInd := W;
      End;

    End;


    Function TryToBindUpperHole (     HandleInd,
                                      LogPage,
                                      Len,
                                      StartNew          : Byte;
                                  Var UpperBindFreeLInd : Word ) : Boolean;

    Var
      FLBinderInd : Word;

    Begin
      TryToBindUpperHole := False;
      UpperBindFreeLInd := 0;
      FindFreeBinder ( FLBinderInd, HandleInd, LogPage, StartNew, False );
      If FLBinderInd <> 0 Then Begin
        With FreeList (FramePtr^) [FLBinderInd] Do Begin
          Inc (LenM1, Len);
          Dec (Offset, Len);
          UpperBindFreeLInd := FLBinderInd;
          TryToBindUpperHole := True;
        End;
      End;
    End;


    Function TryToBindLowerHole ( HandleInd,
                                  LogPage,
                                  Len,
                                  Finish            : Byte;
                                  UpperBindFreeLInd : Word ) : Boolean;

    Var
      FLBinderInd : Word;

    Begin
      TryToBindLowerHole := False;
      FindFreeBinder ( FLBinderInd, HandleInd, LogPage, Finish, True );
      If FLBinderInd <> 0 Then Begin
        With FreeList (FramePtr^) [FLBinderInd] Do Begin
          If UpperBindFreeLInd = 0 Then Begin
            Inc (LenM1, Len);
          End Else Begin
            Inc (LenM1, Succ (FreeList (FramePtr^) [UpperBindFreeLInd].LenM1));
            FreeList (FramePtr^) [UpperBindFreeLInd] :=
                FreeList (FramePtr^) [UsedNrOfFreeListEntries];
            Dec (UsedNrOfFreeListEntries);
          End;
          TryToBindLowerHole := True;
        End;
      End;
    End;


  Begin
  {$IFNDEF NoErrorCheckEMSHeap}
    If Not EMSHeapInitialized Then Begin
      EMSHardError ( 50 );
      Exit;
    End;
    If EPtr = Nil Then Begin
      EMSHardError ( 130 );
      Exit;
    End;
    If Size > MaxEMSAllocSize Then Begin
      EMSHardError ( 120 );
      Exit;
    End;
  {$ENDIF}
    If Size = 0 Then Exit;
    If Not MapFreeList Then Exit;
    If Size > PhysPageSize Then Begin
      With RealEMSPtr Do Begin
      {$IFNDEF NoErrorCheckEMSHeap}
        If (HandleInds And $FF00) = 0 Then Begin
          EMSHardError ( 131 );
          UnmapFreeList;
          Exit;
        End;
      {$ENDIF}
        If IncUsedNrOfFreeListEntries Then Begin
          With FreeList (FramePtr^) [UsedNrOfFreeListEntries] Do Begin
            HandleIndex := Byte (HandleInds Shr 8);
            Offset := (LogPageOfs And $00FF) Shl 8;
            LenM1 := $FF;
          End;
        End Else Begin
          UnmapFreeList;
          Exit;
        End;
        HandleInds := HandleInds And $00FF;
        LogPageOfs := LogPageOfs And $FF00;
        Dec (Size, PhysPageSize);
          {-Simulate a size <= 16384}
      End;
    End;
    If (RealEMSPtr.HandleInds And $FF00) <> 0 Then Begin
      EMSHardError ( 132 );
      Exit;
    End;
    BlockSize := Size Shr 6;
    If (Size And $003F) <> 0 Then Inc (BlockSize);
    RealOfs := RealEMSPtr.LogPageOfs And $00FF;
    UpperHoleStart := RealOfs + BlockSize;
      {-Calculate upper hole start}
    Binded := False;
    UpperBindFreeLInd := 0;
    If UpperHoleStart < (PhysPageSize Shr 6) Then Begin
      With RealEMSPtr Do Begin
        Binded := TryToBindUpperHole ( HandleInds, Byte (LogPageOfs Shr 8),
            BlockSize, UpperHoleStart, UpperBindFreeLInd );
      End;
    End;
    If RealOfs > 0 Then Begin
      With RealEMSPtr Do Begin
        If TryToBindLowerHole ( HandleInds, Byte (LogPageOfs Shr 8),
            BlockSize, RealOfs, UpperBindFreeLInd ) Then Binded := True;
      End;
    End;
    If Not Binded Then Begin
      If IncUsedNrOfFreeListEntries Then Begin
        With FreeList (FramePtr^) [UsedNrOfFreeListEntries], RealEMSPtr
            Do Begin
          HandleIndex := Byte (HandleInds);
          Offset := LogPageOfs;
          LenM1 := Byte (Pred (BlockSize));
        End;
      End;
    End;
    UnmapFreeList;
  End;


  Function EMSMemAvail : LongInt;

  Var
    W     : Word;
    Avail : LongInt;

  Begin
    EMSMemAvail := 0;
  {$IFNDEF NoErrorCheckEMSHeap}
    If Not EMSHeapInitialized Then Begin
      EMSHardError ( 50 );
      Exit;
    End;
  {$ENDIF}
    If Not MapFreeList Then Exit;
    Avail := 0;
    For W := 1 To UsedNrOfFreeListEntries Do Begin
      Inc (Avail, (Succ (Word (FreeList (FramePtr^) [W].LenM1))) Shl 6);
    End;
    EMSMemAvail := Avail;
    UnmapFreeList;
  End;


  Function EMSMaxAvail : Word;

  Var
    OldAvail,
    Avail,
    FLInd,
    FirstPageInd : Word;

  Begin
    EMSMaxAvail := 0;
  {$IFNDEF NoErrorCheckEMSHeap}
    If Not EMSHeapInitialized Then Begin
      EMSHardError ( 50 );
      Exit;
    End;
  {$ENDIF}
    If Not MapFreeList Then Exit;
    Avail := 0;
    FLInd := 0;
    Repeat
      OldAvail := Avail;
      Inc (Avail, 64);
      FindNextHole ( Avail, FLInd, False );
    Until (Avail = PhysPageSize) Or (FLInd = 0);
    If FLInd = 0 Then Avail := OldAvail;
    If (Avail < PhysPageSize) Then Begin
      EMSMaxAvail := Avail;
      UnmapFreeList;
      Exit;
    End;
    FirstPageInd := FLInd;
    Avail := 0;
    FLInd := 0;
    Repeat
      OldAvail := Avail;
      Inc (Avail, 64);
      FindNextHole ( Avail, FLInd, True );
      If FLInd = FirstPageInd Then Avail := OldAvail;
    Until ((Avail = PhysPageSize) Or (FLInd = 0));
    If (Avail <> PhysPageSize) Or (FLInd = 0) Then Avail := OldAvail;
    EMSMaxAvail := Avail + PhysPageSize;
    UnmapFreeList;
  End;


  Var
    SaveExitProc : Pointer;

{$IFDEF XXFPlusOptXX}
  {$UNDEF XXFPlusOptXX}
{$ENDIF}
{$IFOPT F+}
  {$DEFINE XXFPlusOptXX}
{$ENDIF}
{$F+}
  Procedure EMSHeapExitProc;

  Begin
    ExitProc := SaveExitProc;
    If EMSHeapInitialized Then Begin
      DeAlloc ( HandlesToUseForAlloc );
    End;
    EMSHeapInitialized := False;
  End;
{$IFNDEF XXFPlusOptXX}
  {$F-}
{$ELSE}
  {$UNDEF XXFPlusOptXX}
{$ENDIF}


  Procedure PerformEMSHeapInit ( FreePages : Word );

  Begin
    EMSHeapInitialized := DoInit ( FreePages );
  End;


  Procedure InitEMSHeap ( FreePages : Word );

  Begin
  {$IFDEF ManualInitEMSHeap}
    If EMSHeapInitialized Then Begin
      EMSHardError ( 75 );
    End Else Begin
      PerformEMSHeapInit ( FreePages );
    End;
  {$ELSE}
    EMSHardError ( 70 );
  {$ENDIF}
  End;


  Procedure ExitEMSHeap;

  Begin
  {$IFDEF ManualInitEMSHeap}
    If EMSHeapInitialized Then Begin
      DeAlloc ( HandlesToUseForAlloc );                       {!!.41}
      EMSHeapInitialized := False;                            {!!.41}
    End;
  {$ELSE}
    EMSHardError ( 80 );
  {$ENDIF}
  End;


Begin
  EMSHeapInitialized := False;
  SaveExitProc := ExitProc;
  ExitProc := @EMSHeapExitProc;
{$IFNDEF ManualInitEMSHeap}
  PerformEMSHeapInit ( ToLetFreePages );
{$ENDIF}
End.
