(**************************************************************************)
(*                                                                        *)
(* Module:   'Definition.Inc'          Copyright (c) 1998                 *)
(*                                                                        *)
(* Version: 0.0                        Dr. Dietmar Budelsky               *)
(* Sub-Version: 0.0                    budelsky@ibs.bm.eunet.de           *)
(*                                     IBS Schillings GmbH & Co KG        *)
(*                                     Ein Unternehmen der KROHNE-Gruppe  *)
(*                                     Heisenbergstr. 18                  *)
(*                                     50169 Kerpen-Türnich               *)
(*                                     Phone: (49)22 37/97 44-0           *)
(*                                                                        *)
(**************************************************************************)
(*  Changes for Delphi 4 or higher are made for Compilers higher          *)
(*  than Version 10.0.                                                     *)
(*                                                                        *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free, as long as this  *)
(* header and its copyright text is intact.                               *)
(* Dr. Dietmar Budelsky, 1998-01-07                                       *)
(**************************************************************************)
{$IFDEF VER90} // Delphi 2.0
  {$DEFINE DELPHI2}
{$ENDIF}
{$IFDEF VER93} // C++ Builder 1.0
  {$DEFINE CBUILDER1}
{$ENDIF}
{$IFDEF VER100} // Delphi 3.0
  {$DEFINE DELPHI3}
{$ENDIF}
{$IFDEF VER110} // C++ Builder 3.0
  {$DEFINE CBUILDER3}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER120} // Delphi 4.0
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI4_OR_HIGHER}
{$ENDIF}
{$IFDEF VER125} // C++ Builder 4.0
  {$DEFINE CBUILDER4}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER130} // Delphi 5.0
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
{$ENDIF}
{$IFDEF VER140} // Delphi 6.0
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
{$ENDIF}
{$IFDEF VER150} // Delphi 7.0
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
{$ENDIF}

