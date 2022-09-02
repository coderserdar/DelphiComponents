{********************************************************************}
{* FIXTOVAR.PAS - Convert fixed-length to variable-length records   *}
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

Unit FixToVar;

Interface

Uses
  BTBase,
  BTIsBase,
  Filer,     {!!.TP}
  ReStruct,                                                          {!!.50}
  ReIndex,
  VReorg;

  Procedure FixToVarFileBlock ( FBlName      : IsamFileBlockName;
                                DatSLenFix   : LongInt;
                                DatSLenVar   : LongInt;
                                NumberOfKeys : Word;
                                IID          : IsamIndDescr;
                                BuildKeyFunc : FuncBuildKey );
  {-Builds a variable record length fileblock out of a fixed}


Implementation

  Function IsamChangeDatSElongateCheckZ ( Var DatSOld;
                                          Var DatSNew;
                                          Var Len     : Word ) : Boolean; far;
                                                                     {!!.50}
  Type
    AllArr = Array [0..0] Of Byte;

  Var
    Null : LongInt;

  Begin
    Null := 0;
    Move ( DatSOld, DatSNew, Len );
    Move ( Len, AllArr (DatSNew) [Len], SizeOf (Len) );
    Move ( Null, AllArr (DatSNew) [Len + SizeOf (Word)],
        SizeOf (LongInt) ) ;
    IsamChangeDatSElongateCheckZ := LongInt (DatSOld) = LongInt (0);
  End;


  Procedure FixToVarFileBlock ( FBlName      : IsamFileBlockName;
                                DatSLenFix   : LongInt;
                                DatSLenVar   : LongInt;
                                NumberOfKeys : Word;
                                IID          : IsamIndDescr;
                                BuildKeyFunc : FuncBuildKey );

  Var
    IIDDummy       : IsamIndDescr;
    DatSLenEmulVar : LongInt;
    MaxDiffBytes   : Word;

  Begin
    DatSLenEmulVar := DatSLenFix + SizeOf (Word) + SizeOf (LongInt);
    RestructFileBlock ( FBlName, DatSLenEmulVar, DatSLenFix, False,
        0, IsamChangeDatSElongateCheckZ, BTNoCharConvert, Nil );     {!!.50}
    If Not IsamOK Then Exit;
    If DatSLenVar > DatSLenEmulVar Then Begin
      MaxDiffBytes := DatSLenVar - DatSLenEmulVar;
    End Else Begin
      MaxDiffBytes := 0;
    End;
    ReorgVFileBlock ( FBlName, DatSLenVar, NumberOfKeys, IID, DatSLenEmulVar,
        MaxDiffBytes, BuildKeyFunc, ChangeDatSNoChange );           {!!.50}
  End;

{$IFDEF InitAllUnits}
Begin
{$ENDIF}
End.
