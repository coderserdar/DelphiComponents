{*********************************************************}
{*                  TPDEFINE.INC                         *}
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
 * Portions Copyright (c) Sunny Hill Software 1995, 1986
 * and used under license to TurboPower Software
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{This directive determines whether or not TPCRT is compiled in such a way as
 to coexist peacefully with the standard Turbo Pascal CRT unit.}

{.$DEFINE UseCrt}

{This directive enables mouse support in several of the units, as well as in
 some of the demo programs}

{$DEFINE UseMouse}

{The following define controls whether items within a menu system can
 be temporarily disabled and re-enabled, and whether menus can be shadowed.
 Defining Tpro5Menu will break (in a small way) menu systems defined with
 Turbo Professional 4.0. The extent of the problem is limited to the color
 arrays defined for each menu system -- the new color array requires two
 additional items.}

{$DEFINE Tpro5Menu}

{If FourByteDates is defined, dates in TPDATE are stored as longints, giving a
range of 01/01/1600-12/31/3999. If it isn't defined, dates are stored as
words, giving a range of 01/01/1900-12/31/2078. WARNING! Between version 5.08
and 5.09, we corrected a bug in TPDATE that affected date calculations when
FourByteDates was NOT defined. If you have been using word-sized dates with a
version of Turbo Professional prior to 5.09, please be sure to read the
discussion of this problem in the READ.ME file.}

{$DEFINE FourByteDates}

{Disable the following define if you never need to display directories (using
 TPDIR) with file size, date and time. Doing so reduces the final application
 size by up to 3200 bytes.}

{$DEFINE AllowDateTime}

{The following directive enables numeric (right-to-left) editor in TPENTRY}

{.$DEFINE IncludeNumeric}

{The following directive enables multiple choice fields in TPENTRY}

{.$DEFINE IncludeChoice}

{The following directive enables support for BCD reals in TPENTRY}

{.$DEFINE UseBCD}

{Deactivate the following define if the caller of TPSORT needs to perform
 heap allocation or deallocation while the sort is in progress, that is,
 within the user-defined procedures of the sort. For large sorts with
 element size exceeding 4 bytes, FastDispose makes a significant difference
 in the speed of heap deallocation when the sort is finished.}

{$DEFINE FastDispose}

{if the following directive is defined, TPTSR tries to thwart SideKick}

{$DEFINE ThwartSideKick}

{Deactivate the following define if exploding windows are not desired,
 in order to save up to 2200 bytes of code space.}

{.$DEFINE ExplodingWindows}

{Deactivate the following define if shadowed windows are not desired,
 in order to save up to 2000 bytes of code space.}

{.$DEFINE ShadowedWindows}

{Activate the following define if scrollable data entry screens are desired}

{.$DEFINE TpEntryScrolls}

{Activate the following define to allow unpickable items in TPPICK}

{.$DEFINE PickItemDisable}

{Activate the following define to allow alternate orientations in TPPICK}

{.$DEFINE EnablePickOrientations}

{The following define controls how various TPRO units react to the heap
 changes of TP6 and later. There's no need for you to modify it.}

  {$IFDEF Ver60}
    {$DEFINE Heap6}
  {$ENDIF}

  {$IFDEF Ver70}
    {$DEFINE Heap6}
    {$I-,P-,T-,Q-}
  {$ENDIF}

  {$IFDEF Ver80}
    {$DEFINE Heap6}
    {$I-,P-,T-,Q-}
  {$ENDIF}
