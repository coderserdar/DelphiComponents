{*********************************************************}
{*                  OPDEFINE.INC 1.30                    *}
{*      Assorted conditional compilation directives      *}
{*********************************************************}

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
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{This compiler directive is not needed by OPro itself, but is provided for
 other programs and libraries that depend on recognizing the differences
 between the OPro 1.20 and later versions, and prior versions.  DO NOT
 CHANGE THIS DEFINE!!!}

{$DEFINE OPro12}
{$DEFINE OPro13}

{This compiler directive controls whether the Float type in OPSTRING is
 defined as an Extended or a Real. It also controls the inclusion of certain
 methods that apply only to 8087 numeric types.}

  {$N+}

{This directive forces all units to contain an initialization block, even
 those that do not have any actual initialization code. Adding these blocks
 helps to work around a bug in Turbo Debugger 1.5.}

  {.$DEFINE InitAllUnits}

{This directive determines whether or not OPCRT is compiled in such a way as
 to coexist peacefully with the standard Turbo Pascal CRT unit.}

  {.$DEFINE UseCrt}

{This directive enables mouse support in several of the units, as well as in
 some of the demo programs}

  {$DEFINE UseMouse}

{This directive enables scroll bar support in OPWINDOW and units that
 depend on it}

  {$DEFINE UseScrollBars}

{This directive enables hot spot support in OPWINDOW and units that depend
 on it}

  {$DEFINE UseHotSpots}

{This directive enables shadowing support in OPWINDOW}

  {$DEFINE UseShadows}

{This directive enables the AdjustWindow, ResizeWindow, MoveWindow, and other
 methods in OPWINDOW}

  {$DEFINE UseAdjustableWindows}

{If FourByteDates is defined, dates in OPDATE are stored as longints, giving a
range of 01/01/1600-12/31/3999. If it isn't defined, dates are stored as words,
giving a range of 01/01/1900-12/31/2078.}

  {$DEFINE FourByteDates}

{The following directive enables support for editing of dates in OPEDIT and
OPENTRY. It also enables certain options in OPDIR.}

  {$DEFINE UseDates}

{The following directive enables support for BCD reals in OPEDIT and OPENTRY}

  {.$DEFINE UseBCD}

{The following directive enables support for pick list fields in OPENTRY}

  {$DEFINE PickListFields}

{Deactivate the following define if the caller of OPSORT needs to perform
 heap allocation or deallocation while the sort is in progress, that is,
 within the user-defined procedures of the sort. For large sorts with
 element size exceeding 4 bytes, FastDispose makes a significant difference
 in the speed of heap deallocation when the sort is finished.}

  {$DEFINE FastDispose}

{If the following directive is defined, OPTSR and OPSWAP try to
 thwart SideKick}

  {$DEFINE ThwartSideKick}

{If the following directive is defined, streams support is enabled in all
 objects}

  {$DEFINE UseStreams}

{If the following directive is defined, XMS support is enabled in OPSWAP.
 IMPORTANT: You must also define the SupportXms constant in OPSWAP.ASM and
 reassemble the file. Failure to do so will cause an error when recompiling
 OPSWAP.}

  {$DEFINE SupportXms}                       {!!.02}

{If the following directive is defined, OPDRAG support will be included in all
units containing command windows.}

  {$DEFINE UseDrag}                          {!!.03}

{If the following directive is defined, the numeric editor in OPFEDIT will
 operate in calculator fashion, allowing the user to enter decimal points
 himself.}

  {.$DEFINE UseCalcEdit} {!!.13}

{The following define controls how various OPRO units react to the heap
 changes of TP6. There's no need for you to modify it.}

  {$IFDEF Ver60}
    {$DEFINE Heap6}
  {$ENDIF}

{The following define controls how various OPRO units accomodate changes to
 the BP7 compiler.  These should not be modified.}

  {$IFDEF Ver70}
    {$DEFINE Heap6}         {use TP6 style heap management in Real mode}
    {$I-,P-,T-,X-,Q-}       {set specific directives we need}
   {$IFDEF Dpmi}             {if in protected mode...}
    {$UNDEF FastDispose}      {no use of FastDispose in sorts}
   {$ENDIF}
  {$ENDIF}

