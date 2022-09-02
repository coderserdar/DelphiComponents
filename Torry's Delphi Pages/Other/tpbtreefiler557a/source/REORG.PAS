{********************************************************************}
{* REORG.PAS - reorganize fileblock                                 *}
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

Unit
  ReOrg;

Interface

Uses
  BTBase,
  BTIsBase,
  Filer,     {!!.TP}
  Restruct,
  Reindex;

  Procedure ReorgFileBlock ( FBlName        : IsamFileBlockName;
                             DatSLen        : LongInt;
                             NumberOfKeys   : Word;
                             IID            : IsamIndDescr;
                             DatSLenOld     : LongInt;
                             BuildKeyFunc   : FuncBuildKey;
                             ChangeDatSProc : FuncChangeDatS);
    {-Reorganizes a fixed record length fileblocks data and index structure}


Implementation

  Procedure ReorgFileBlock ( FBlName        : IsamFileBlockName;
                             DatSLen        : LongInt;
                             NumberOfKeys   : Word;
                             IID            : IsamIndDescr;
                             DatSLenOld     : LongInt;
                             BuildKeyFunc   : FuncBuildKey;
                             ChangeDatSProc : FuncChangeDatS);

  Var
    Dummy : Boolean;

  Begin
    RestructFileBlock ( FBlName, DatSLen, DatSLenOld, False, 0,
        ChangeDatSProc, BTNoCharConvert, Nil );
    If Not IsamOK Then Exit;
    If NumberOfKeys > 0 Then Begin
      ReIndexFileBlock ( FBlName, NumberOfKeys, IID, False,
          BuildKeyFunc, True, Dummy,
          BTNoCharConvert, Nil );
    End;
  End;

{$IFDEF InitAllUnits}
Begin
{$ENDIF}
End.
