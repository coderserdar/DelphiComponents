(********************************************************)
(*                                                      *)
(*  Bare Library include file                           *)
(*                                                      *)
(*  Unit naming requirement:                            *)
(*                                                      *)
(* All units must be names BareXXX, where Bare is the   *)
(* name of this class library, and XXX is a descriptive *)
(* name such as Forms or Controls                       *)
(*                                                      *)
(********************************************************)

  { Delphi 2006 }
  {$IFDEF VER180}
    {$DEFINE D10}
    {$DEFINE D10_UP}
    {$DEFINE D9_UP}
    {$DEFINE D8_UP}
    {$DEFINE D7_UP}
    {$DEFINE D6_UP}
    {$DEFINE D5_UP}
    {$DEFINE D4_UP}
  {$ENDIF}

  { Delphi 2005 }
  {$IFDEF VER170}
    {$DEFINE D9}
    {$DEFINE D9_UP}
    {$DEFINE D8_UP}
    {$DEFINE D7_UP}
    {$DEFINE D6_UP}
    {$DEFINE D5_UP}
    {$DEFINE D4_UP}
  {$ENDIF}

  {$IFDEF VER160}
    {$DEFINE D8}
    {$DEFINE D8_UP}
    {$DEFINE D7_UP}
    {$DEFINE D6_UP}
    {$DEFINE D5_UP}
    {$DEFINE D4_UP}
  {$ENDIF}

  {$IFDEF VER150}
    {$DEFINE D7}
    {$DEFINE D7_UP}
    {$DEFINE D6_UP}
    {$DEFINE D5_UP}
    {$DEFINE D4_UP}
  {$ENDIF}

  {$IFDEF VER140}
    {$DEFINE D6}
    {$DEFINE D6_UP}
    {$DEFINE D5_UP}
    {$DEFINE D4_UP}
  {$ENDIF}

  {$IFDEF VER130}
    {$DEFINE D5}
    {$DEFINE D5_UP}
    {$DEFINE D4_UP}
  {$ENDIF}

  {$IFDEF VER120}
    {$DEFINE D4}
    {$DEFINE D4_UP}
  {$ENDIF}

{$IFDEF D6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN SYMBOL_LIBRARY OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNIT_DEPRECATED OFF}
  {$WARN UNIT_LIBRARY OFF}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

{$IFNDEF FREEPASCAL}
{$B-} { Complete Boolean Evaluation }
{$J+} { Writeable structured consts }
{$ENDIF}
{$R-} { Range-Checking }
{$V-} { Var-String Checking }
{$T-} { Typed @ operator }
{$X+} { Extended syntax }
{$P+} { Open string params }
{$H+} { Use long strings by default }

{.$DEFINE BARE}
{.$DEFINE DEBUGSTRINGS}
{$DEFINE DEBUGMODE}

{$IFNDEF DEBUGMODE}
  {$L-}
  {$D-}
  {$Y-}
  {$C-}
{$ELSE}
  {$C+}
{$ENDIF}
