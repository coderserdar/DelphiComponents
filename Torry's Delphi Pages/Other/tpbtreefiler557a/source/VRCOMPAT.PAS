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
  {$F-,V-,B-,S-,I-,R-}
  {$IFDEF CanSetOvrflowCheck}
    {$Q-}
  {$ENDIF}

Unit VRCompat;

Interface

Uses
  Filer,
  VRec;

var
  VRecBufSize : Word absolute IsamVRecBufSize;

type
  IsamAccessMode = (Normal, InSpiteOfLock, ReadOnly);

  Function CreateVariableRecBuffer ( IFBPtr : IsamFileBlockPtr ) : BOOLEAN;

  Procedure ReleaseVariableRecBuffer;

  Function SetVariableRecBuffer ( Size : Word ) : Boolean;

  Procedure AddVariableRec (     IFBPtr : IsamFileBlockPtr;
                             Var RefNr  : LongInt;
                             Var Source;
                                 Len    : Word);

  Procedure DeleteVariableRec ( IFBPtr : IsamFileBlockPtr;
                                RefNr  : LongInt );


  Procedure PutVariableRec (     IFBPtr : IsamFileBlockPtr;
                                 RefNr  : LongInt;
                             Var Source;
                                 Len    : Word;
                                 Mode : IsamAccessMode );

  Procedure GetVariableRecPart (     IFBPtr : IsamFileBlockPtr;
                                     RefNr  : LongInt;
                                 Var Dest;
                                 Var Len    : Word;
                                     Mode   : IsamAccessMode );

  Procedure GetVariableRec (     IFBPtr : IsamFileBlockPtr;
                                 RefNr  : LongInt;
                             Var Dest;
                             Var Len    : Word;
                                 Mode   : IsamAccessMode );

  Procedure GetVariableRecLength (     IFBPtr : IsamFileBlockPtr;
                                       RefNr  : LongInt;
                                   Var Len    : Word );

(*Procedure GetVariableRecRebuilt ( Var F       : IsamFile;
                                        DatSLen : Word;
                                    Var RefNr   : LongInt;
                                        OnlyLen : Boolean;
                                    Var Dest;
                                    Var Len     : Word );*)   {!!.50}

Implementation

  Function CreateVariableRecBuffer ( IFBPtr : IsamFileBlockPtr ) : Boolean;
  Begin
    CreateVariableRecBuffer := BTCreateVariableRecBuffer ( IFBPtr );
  End;

  Procedure ReleaseVariableRecBuffer;
  Begin
    BTReleaseVariableRecBuffer;
  End;

  Function SetVariableRecBuffer ( Size : Word ) : Boolean;
  Begin
    SetVariableRecBuffer := BTSetVariableRecBuffer ( Size );
  End;

  Procedure AddVariableRec (     IFBPtr : IsamFileBlockPtr;
                             Var RefNr  : LongInt;
                             Var Source;
                                 Len    : Word);
  Begin
    BTAddVariableRec ( IFBPtr, RefNr, Source, Len );
  End;

  Procedure DeleteVariableRec ( IFBPtr : IsamFileBlockPtr;
                                RefNr  : LongInt );
  Begin
    BTDeleteVariableRec ( IFBPtr, RefNr );
  End;


  Procedure PutVariableRec (     IFBPtr : IsamFileBlockPtr;
                                 RefNr  : LongInt;
                             Var Source;
                                 Len    : Word;
                                 Mode : IsamAccessMode );
  Begin
    BTPutVariableRec ( IFBPtr, RefNr, Source, Len );
  End;

  Procedure GetVariableRecPart (     IFBPtr : IsamFileBlockPtr;
                                     RefNr  : LongInt;
                                 Var Dest;
                                 Var Len    : Word;
                                     Mode   : IsamAccessMode );
  Begin
    BTGetVariableRecPart ( IFBPtr, RefNr, Dest, Len );
  End;

  Procedure GetVariableRec (     IFBPtr : IsamFileBlockPtr;
                                 RefNr  : LongInt;
                             Var Dest;
                             Var Len    : Word;
                                 Mode   : IsamAccessMode );
  Begin
    BTGetVariableRec ( IFBPtr, RefNr, Dest, Len );
  End;

  Procedure GetVariableRecLength (     IFBPtr : IsamFileBlockPtr;
                                       RefNr  : LongInt;
                                   Var Len    : Word );
  Begin
    BTGetVariableRecLength ( IFBPtr, RefNr, Len );
  End;

(*Procedure GetVariableRecRebuilt ( Var F       : IsamFile;
                                        DatSLen : Word;
                                    Var RefNr   : LongInt;
                                        OnlyLen : Boolean;
                                    Var Dest;
                                    Var Len     : Word );
  Begin
    BTGetVariableRecRebuilt ( F, DatSLen, RefNr, OnlyLen, Dest, Len, $FFFF );
  End;
*) {!!.50}

Begin
  VRecBufSize := 0;
End.
