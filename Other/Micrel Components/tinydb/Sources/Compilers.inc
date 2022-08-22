(*****************************************************************************)
(* Check the the version of Delphi or BCB.                                   *)
(*****************************************************************************)
(* Usage: Add the following line to the top of your unit file:               *)
(*        {$I Compilers.inc}                                                 *)
(*****************************************************************************)

(*

- Development environment directives

  This file defines two driectives to indicate which development environment the
  library is being compiled with. Currently this can either be Delphi or
  C++ Builder (in the near future "Kylix" will be added).

  Directive    Description
  -----------------------------------------------------------------------------
  DELPHI       Defined if compiled with Delphi
  BCB          Defined if compiled with C++ Builder

- Platform Directives

  Platform directives are not explicitly defined in this file but are defined
  by the compiler itself. They are listed here only for completeness.

  Directive    Description
  -----------------------------------------------------------------------------
  WIN32        Defined when target platform is 32 bit Windows
  LINUX        Defined when target platform is Linux

- Delphi Versions

  The following directives are direct mappings from the VERXXX directives to a
  friendly name of the associated compiler. These directives are only defined if
  the compiler is Delphi (ie DELPHI is defined).

  Directive         Description
  -----------------------------------------------------------------------------
  DELPHI_1          Defined when compiling with Delphi 1
  DELPHI_2          Defined when compiling with Delphi 2
  DELPHI_3          Defined when compiling with Delphi 3
  DELPHI_4          Defined when compiling with Delphi 4
  DELPHI_5          Defined when compiling with Delphi 5
  DELPHI_6          Defined when compiling with Delphi 6
  DELPHI_1_UP       Defined when compiling with Delphi 1 or higher
  DELPHI_2_UP       Defined when compiling with Delphi 2 or higher
  DELPHI_3_UP       Defined when compiling with Delphi 3 or higher
  DELPHI_4_UP       Defined when compiling with Delphi 4 or higher
  DELPHI_5_UP       Defined when compiling with Delphi 5 or higher
  DELPHI_6_UP       Defined when compiling with Delphi 6 or higher
  DELPHI_7          Defined when compiling with Delphi 7 or higher
  DELPHI_8          Defined when compiling with Delphi 8 or higher
  DELPHI_9          Defined when compiling with Delphi 9 or higher
  DELPHI_10_UP      Defined when compiling with Delphi 10 or higher

- C++ Builder Versions

  The following directives are direct mappings from the VERXXX directives to a
  friendly name of the associated compiler. These directives are only defined if
  the compiler is C++ Builder (ie BCB is defined).

  Directive         Description
  -----------------------------------------------------------------------------
  BCB_1             Defined when compiling with C++ Builder 1
  BCB_3             Defined when compiling with C++ Builder 3
  BCB_4             Defined when compiling with C++ Builder 4
  BCB_5             Defined when compiling with C++ Builder 5
  BCB_6             Defined when compiling with C++ Builder 6
  BCB_7             Defined when compiling with C++ Builder 7
  BCB_1_UP          Defined when compiling with C++ Builder 1 or higher
  BCB_3_UP          Defined when compiling with C++ Builder 3 or higher
  BCB_4_UP          Defined when compiling with C++ Builder 4 or higher
  BCB_5_UP          Defined when compiling with C++ Builder 5 or higher
  BCB_6_UP          Defined when compiling with C++ Builder 6 or higher
  BCB_7_UP          Defined when compiling with C++ Builder 7 or higher
  BCB_8_UP          Defined when compiling with C++ Builder 8 or higher
  BCB_9_UP          Defined when compiling with C++ Builder 9 or higher
  BCB_10_UP         Defined when compiling with C++ Builder 10 or higher

- Compiler Versions

  The following directives are direct mappings from the VERXXX directives to a
  friendly name of the associated compiler. Unlike the DELPHI_X and BCB_X
  directives, these directives are indepedent of the development environment.
  That is, they are defined regardless of whether compilation takes place using
  Delphi or C++ Builder.

  Directive         Description
  -----------------------------------------------------------------------------
  COMPILER_1        Defined when compiling with Delphi 1
  COMPILER_2        Defined when compiling with Delphi 2 or C++ Builder 1
  COMPILER_3        Defined when compiling with Delphi 3
  COMPILER_35       Defined when compiling with C++ Builder 3
  COMPILER_4        Defined when compiling with Delphi 4 or C++ Builder 4
  COMPILER_5        Defined when compiling with Delphi 5 or C++ Builder 5
  COMPILER_6        Defined when compiling with Delphi 6 or C++ Builder 6
  COMPILER_1_UP     Defined when compiling with Delphi 1 or higher
  COMPILER_2_UP     Defined when compiling with Delphi 2 or C++ Builder 1 or higher
  COMPILER_3_UP     Defined when compiling with Delphi 3 or higher
  COMPILER_35_UP    Defined when compiling with C++ Builder 3 or higher
  COMPILER_4_UP     Defined when compiling with Delphi 4 or C++ Builder 4 or higher
  COMPILER_5_UP     Defined when compiling with Delphi 5 or C++ Builder 5 or higher
  COMPILER_6_UP     Defined when compiling with Delphi 6 or C++ Builder 6 or higher
  COMPILER_7_UP     Defined when compiling with Delphi 7 or C++ Builder 7 or higher
  COMPILER_8_UP     Defined when compiling with Delphi 8 or C++ Builder 8 or higher
  COMPILER_9_UP     Defined when compiling with Delphi 9 or C++ Builder 9 or higher
  COMPILER_10_UP    Defined when compiling with Delphi 10 or C++ Builder 10 or higher

- Feature Directives

  The features directives are used to test if the compiler supports specific
  features, such as method overloading, and adjust the sources accordingly. Use
  of these directives is preferred over the use of the DELPHI and COMPILER
  directives.

  Directive              Description
  -----------------------------------------------------------------------------
  SUPPORTS_WIDESTRING    Compiler supports the WideString type (D3/BCB_3 up)
  SUPPORTS_INTERFACE     Compiler supports interfaces (D3/BCB_3)
  SUPPORTS_EXTSYM        Compiler supports the $EXTERNALSYM directive (D4/BCB_3)
  SUPPORTS_NODEFINE      Compiler supports the $NODEFINE directive (D4/BCB_3)
  SUPPORTS_INT64         Compiler supports the Int64 type (D4/BCB_4)
  SUPPORTS_DYNAMICARRAYS Compiler supports dynamic arrays (D4/BCB_4)
  SUPPORTS_DEFAULTPARAMS Compiler supports default parameters (D4/BCB_4)
  SUPPORTS_OVERLOAD      Compiler supports overloading (D4/BCB_4)

- Compiler Settings

  The compiler settings directives indicate whether a specific compiler setting
  is in effect. This facilitates changing compiler settings locally in a more
  compact and readible manner.

  Directive              Description
  -----------------------------------------------------------------------------
  ALIGN_ON               Compiling in the A+ state (no alignment)
  BOOLEVAL_ON            Compiling in the B+ state (complete boolean evaluation)
  ASSERTIONS_ON          Compiling in the C+ state (assertions on)
  DEBUGINFO_ON           Compiling in the D+ state (debug info generation on)
  IMPORTEDDATA_ON        Compiling in the G+ state (creation of imported data references)
  LONGSTRINGS_ON         Compiling in the H+ state (string defined as AnsiString)
  IOCHECKS_ON            Compiling in the I+ state (I/O checking enabled)
  WRITEABLECONST_ON      Compiling in the J+ state (typed constants can be modified)
  LOCALSYMBOLS           Compiling in the L+ state (local symbol generation)
  TYPEINFO_ON            Compiling in the M+ state (RTTI generation on)
  OPTIMIZATION_ON        Compiling in the O+ state (code optimization on)
  OPENSTRINGS_ON         Compiling in the P+ state (variable string parameters are openstrings)
  OVERFLOWCHECKS_ON      Compiling in the Q+ state (overflow checing on)
  RANGECHECKS_ON         Compiling in the R+ state (range checking on)
  TYPEDADDRESS_ON        Compiling in the T+ state (pointers obtained using the @ operator are typed)
  SAFEDIVIDE_ON          Compiling in the U+ state (save FDIV instruction through RTL emulation)
  VARSTRINGCHECKS_ON     Compiling in the V+ state (type checking of shortstrings)
  STACKFRAMES_ON         Compiling in the W+ state (generation of stack frames)
  EXTENDEDSYNTAX_ON      Compiling in the X+ state (Delphi extended syntax enabled)

*)

//-----------------------------------------------------------------------------
// Compiler settings
//-----------------------------------------------------------------------------

{$IFOPT A+} {$DEFINE ALIGN_ON} {$ENDIF}
{$IFOPT B+} {$DEFINE BOOLEVAL_ON} {$ENDIF}
{$IFOPT C+} {$DEFINE ASSERTIONS_ON} {$ENDIF}
{$IFOPT D+} {$DEFINE DEBUGINFO_ON} {$ENDIF}
{$IFOPT G+} {$DEFINE IMPORTEDDATA_ON} {$ENDIF}
{$IFOPT H+} {$DEFINE LONGSTRINGS_ON} {$ENDIF}
// HINTS
{$IFOPT I+} {$DEFINE IOCHECKS_ON} {$ENDIF}
{$IFOPT J+} {$DEFINE WRITEABLECONST_ON} {$ENDIF}
{$IFOPT L+} {$DEFINE LOCALSYMBOLS} {$ENDIF}
{$IFOPT M+} {$DEFINE TYPEINFO_ON} {$ENDIF}
{$IFOPT O+} {$DEFINE OPTIMIZATION_ON} {$ENDIF}
{$IFOPT P+} {$DEFINE OPENSTRINGS_ON} {$ENDIF}
{$IFOPT Q+} {$DEFINE OVERFLOWCHECKS_ON} {$ENDIF}
{$IFOPT R+} {$DEFINE RANGECHECKS_ON} {$ENDIF}
// REALCOMPATIBILITY
{$IFOPT T+} {$DEFINE TYPEDADDRESS_ON} {$ENDIF}
{$IFOPT U+} {$DEFINE SAFEDIVIDE_ON} {$ENDIF}
{$IFOPT V+} {$DEFINE VARSTRINGCHECKS_ON} {$ENDIF}
{$IFOPT W+} {$DEFINE STACKFRAMES_ON} {$ENDIF}
// WARNINGS
{$IFOPT X+} {$DEFINE EXTENDEDSYNTAX_ON} {$ENDIF}

//-----------------------------------------------------------------------------
// VERXXX to COMPILERX, DELPHIX and BCBX mappings
//-----------------------------------------------------------------------------

{$IFDEF VER350} //X11 Alexandria
  {$DEFINE COMPILER_28}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_28}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER340} //X10.4 Sydney
  {$DEFINE COMPILER_27}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_27}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER330} //X10.3 Rio
  {$DEFINE COMPILER_26}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_26}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER320} //X10.2 Tokyo
  {$DEFINE COMPILER_25}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_25}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER310} //X10.1 Berlin
  {$DEFINE COMPILER_24}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_24}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER300} //X10 Seattle
  {$DEFINE COMPILER_23}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_23}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER290} //XE8
  {$DEFINE COMPILER_22}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_22}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER280} //XE7
  {$DEFINE COMPILER_21}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_21}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER270} //XE6
  {$DEFINE COMPILER_20}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_20}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER260} //XE5
  {$DEFINE COMPILER_19}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_19}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER250} //XE4
  {$DEFINE COMPILER_18}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_18}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER240} //XE3
  {$DEFINE COMPILER_17}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_17}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER230} //XE2
  {$DEFINE COMPILER_16}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_16}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER220} //XE
  {$DEFINE COMPILER_15}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_15}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER210} //2010
  {$DEFINE COMPILER_14}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_14}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER200} //2009
  {$DEFINE COMPILER_12}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_12}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER185} //2007 Win32
  {$DEFINE COMPILER_11}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_11}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER180} //2006
  {$DEFINE COMPILER_10}
  {$IFDEF BCB}
    {$DEFINE BCB_10}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_10}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER170} //2005
  {$DEFINE COMPILER_9}
  {$IFDEF BCB}
    {$DEFINE BCB_9}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_9}
    {$DEFINE SUPP_INLINE}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER160}  
  {$DEFINE COMPILER_8}
  {$IFDEF BCB}
    {$DEFINE BCB_8}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_8}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE COMPILER_7}
  {$IFDEF BCB}
    {$DEFINE BCB_7}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_7}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE COMPILER_6}
  {$IFDEF BCB}
    {$DEFINE BCB_6}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_6}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE COMPILER_5}
  {$IFDEF BCB}
    {$DEFINE BCB_5}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI_5}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE COMPILER_4}
  {$DEFINE BCB_4}
  {$DEFINE BCB}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE COMPILER_4}
  {$DEFINE DELPHI_4}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE COMPILER_35}
  {$DEFINE BCB_3}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE COMPILER_3}
  {$DEFINE DELPHI_3}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER93}
  {$DEFINE COMPILER_2}
  {$DEFINE BCB_1}
  {$DEFINE BCB}
{$ENDIF}

{$IFDEF VER90}
  {$DEFINE COMPILER_2}
  {$DEFINE DELPHI_2}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER80}
  {$DEFINE COMPILER_1}
  {$DEFINE DELPHI_1}
  {$DEFINE DELPHI}
{$ENDIF}

//-----------------------------------------------------------------------------
// DELPHIn_UP from DELPHIxx mappings
//-----------------------------------------------------------------------------

{$IFDEF DELPHI_28} //Alexandria
  {$DEFINE DELPHI_28_UP}
  {$DEFINE DELPHI_27_UP}
  {$DEFINE DELPHI_26_UP}
  {$DEFINE DELPHI_25_UP}
  {$DEFINE DELPHI_24_UP}
  {$DEFINE DELPHI_23_UP}
  {$DEFINE DELPHI_22_UP}
  {$DEFINE DELPHI_21_UP}
  {$DEFINE DELPHI_20_UP}
  {$DEFINE DELPHI_19_UP}
  {$DEFINE DELPHI_18_UP}
  {$DEFINE DELPHI_17_UP}
  {$DEFINE DELPHI_16_UP}
  {$DEFINE DELPHI_15_UP}
  {$DEFINE DELPHI_14_UP}
  {$DEFINE DELPHI_12_UP}
  {$DEFINE DELPHI_11_UP}
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_27} //SYDNEY
  {$DEFINE DELPHI_27_UP}
  {$DEFINE DELPHI_26_UP}
  {$DEFINE DELPHI_25_UP}
  {$DEFINE DELPHI_24_UP}
  {$DEFINE DELPHI_23_UP}
  {$DEFINE DELPHI_22_UP}
  {$DEFINE DELPHI_21_UP}
  {$DEFINE DELPHI_20_UP}
  {$DEFINE DELPHI_19_UP}
  {$DEFINE DELPHI_18_UP}
  {$DEFINE DELPHI_17_UP}
  {$DEFINE DELPHI_16_UP}
  {$DEFINE DELPHI_15_UP}
  {$DEFINE DELPHI_14_UP}
  {$DEFINE DELPHI_12_UP}
  {$DEFINE DELPHI_11_UP}
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_26} //RIO
  {$DEFINE DELPHI_26_UP}
  {$DEFINE DELPHI_25_UP}
  {$DEFINE DELPHI_24_UP}
  {$DEFINE DELPHI_23_UP}
  {$DEFINE DELPHI_22_UP}
  {$DEFINE DELPHI_21_UP}
  {$DEFINE DELPHI_20_UP}
  {$DEFINE DELPHI_19_UP}
  {$DEFINE DELPHI_18_UP}
  {$DEFINE DELPHI_17_UP}
  {$DEFINE DELPHI_16_UP}
  {$DEFINE DELPHI_15_UP}
  {$DEFINE DELPHI_14_UP}
  {$DEFINE DELPHI_12_UP}
  {$DEFINE DELPHI_11_UP}
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_25} //TOKYO
  {$DEFINE DELPHI_25_UP}
  {$DEFINE DELPHI_24_UP}
  {$DEFINE DELPHI_23_UP}
  {$DEFINE DELPHI_22_UP}
  {$DEFINE DELPHI_21_UP}
  {$DEFINE DELPHI_20_UP}
  {$DEFINE DELPHI_19_UP}
  {$DEFINE DELPHI_18_UP}
  {$DEFINE DELPHI_17_UP}
  {$DEFINE DELPHI_16_UP}
  {$DEFINE DELPHI_15_UP}
  {$DEFINE DELPHI_14_UP}
  {$DEFINE DELPHI_12_UP}
  {$DEFINE DELPHI_11_UP}
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_24} //BERLIN
  {$DEFINE DELPHI_24_UP}
  {$DEFINE DELPHI_23_UP}
  {$DEFINE DELPHI_22_UP}
  {$DEFINE DELPHI_21_UP}
  {$DEFINE DELPHI_20_UP}
  {$DEFINE DELPHI_19_UP}
  {$DEFINE DELPHI_18_UP}
  {$DEFINE DELPHI_17_UP}
  {$DEFINE DELPHI_16_UP}
  {$DEFINE DELPHI_15_UP}
  {$DEFINE DELPHI_14_UP}
  {$DEFINE DELPHI_12_UP}
  {$DEFINE DELPHI_11_UP}
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_23} //SEATLE
  {$DEFINE DELPHI_23_UP}
  {$DEFINE DELPHI_22_UP}
  {$DEFINE DELPHI_21_UP}
  {$DEFINE DELPHI_20_UP}
  {$DEFINE DELPHI_19_UP}
  {$DEFINE DELPHI_18_UP}
  {$DEFINE DELPHI_17_UP}
  {$DEFINE DELPHI_16_UP}
  {$DEFINE DELPHI_15_UP}
  {$DEFINE DELPHI_14_UP}
  {$DEFINE DELPHI_12_UP}
  {$DEFINE DELPHI_11_UP}
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_22} //XE8
  {$DEFINE DELPHI_22_UP}
  {$DEFINE DELPHI_21_UP}
  {$DEFINE DELPHI_20_UP}
  {$DEFINE DELPHI_19_UP}
  {$DEFINE DELPHI_18_UP}
  {$DEFINE DELPHI_17_UP}
  {$DEFINE DELPHI_16_UP}
  {$DEFINE DELPHI_15_UP}
  {$DEFINE DELPHI_14_UP}
  {$DEFINE DELPHI_12_UP}
  {$DEFINE DELPHI_11_UP}
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_21} //XE7
  {$DEFINE DELPHI_21_UP}
  {$DEFINE DELPHI_20_UP}
  {$DEFINE DELPHI_19_UP}
  {$DEFINE DELPHI_18_UP}
  {$DEFINE DELPHI_17_UP}
  {$DEFINE DELPHI_16_UP}
  {$DEFINE DELPHI_15_UP}
  {$DEFINE DELPHI_14_UP}
  {$DEFINE DELPHI_12_UP}
  {$DEFINE DELPHI_11_UP}
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_20} //XE6
  {$DEFINE DELPHI_20_UP}
  {$DEFINE DELPHI_19_UP}
  {$DEFINE DELPHI_18_UP}
  {$DEFINE DELPHI_17_UP}
  {$DEFINE DELPHI_16_UP}
  {$DEFINE DELPHI_15_UP}
  {$DEFINE DELPHI_14_UP}
  {$DEFINE DELPHI_12_UP}
  {$DEFINE DELPHI_11_UP}
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_19} //XE5
  {$DEFINE DELPHI_19_UP}
  {$DEFINE DELPHI_18_UP}
  {$DEFINE DELPHI_17_UP}
  {$DEFINE DELPHI_16_UP}
  {$DEFINE DELPHI_15_UP}
  {$DEFINE DELPHI_14_UP}
  {$DEFINE DELPHI_12_UP}
  {$DEFINE DELPHI_11_UP}
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_18} //XE4
  {$DEFINE DELPHI_18_UP}
  {$DEFINE DELPHI_17_UP}
  {$DEFINE DELPHI_16_UP}
  {$DEFINE DELPHI_15_UP}
  {$DEFINE DELPHI_14_UP}
  {$DEFINE DELPHI_12_UP}
  {$DEFINE DELPHI_11_UP}
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_17} //XE3
  {$DEFINE DELPHI_17_UP}
  {$DEFINE DELPHI_16_UP}
  {$DEFINE DELPHI_15_UP}
  {$DEFINE DELPHI_14_UP}
  {$DEFINE DELPHI_12_UP}
  {$DEFINE DELPHI_11_UP}
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_16} //XE2
  {$DEFINE DELPHI_16_UP}
  {$DEFINE DELPHI_15_UP}
  {$DEFINE DELPHI_14_UP}
  {$DEFINE DELPHI_12_UP}
  {$DEFINE DELPHI_11_UP}
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_15} //XE1
  {$DEFINE DELPHI_15_UP}
  {$DEFINE DELPHI_14_UP}
  {$DEFINE DELPHI_12_UP}
  {$DEFINE DELPHI_11_UP}
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_14} //D2010
  {$DEFINE DELPHI_14_UP}
  {$DEFINE DELPHI_12_UP}
  {$DEFINE DELPHI_11_UP}
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_12} //D2009
  {$DEFINE DELPHI_12_UP}
  {$DEFINE DELPHI_11_UP}
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_11} //D2007
  {$DEFINE DELPHI_11_UP}
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_10} //D2006
  {$DEFINE DELPHI_10_UP}
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_9} //D5005
  {$DEFINE DELPHI_9_UP}
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_8} //D8 NET.
  {$DEFINE DELPHI_8_UP}
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_7} //D7
  {$DEFINE DELPHI_7_UP}
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_6} //D6
  {$DEFINE DELPHI_6_UP}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_5} //D5
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_4} //D4
  {$DEFINE DELPHI_4_UP}
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_3} //D3
  {$DEFINE DELPHI_3_UP}
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_2} //D2
  {$DEFINE DELPHI_2_UP}
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

{$IFDEF DELPHI_1} //D1 16.bit
  {$DEFINE DELPHI_1_UP}
{$ENDIF}

//-----------------------------------------------------------------------------
// BCBX_UP from BCBX mappings
//-----------------------------------------------------------------------------

{$IFDEF BCB_10}
  {$DEFINE BCB_10_UP}
  {$DEFINE BCB_9_UP}
  {$DEFINE BCB_8_UP}
  {$DEFINE BCB_7_UP}
  {$DEFINE BCB_6_UP}
  {$DEFINE BCB_5_UP}
  {$DEFINE BCB_4_UP}
  {$DEFINE BCB_3_UP}
  {$DEFINE BCB_1_UP}
{$ENDIF}

{$IFDEF BCB_9}
  {$DEFINE BCB_9_UP}
  {$DEFINE BCB_8_UP}
  {$DEFINE BCB_7_UP}
  {$DEFINE BCB_6_UP}
  {$DEFINE BCB_5_UP}
  {$DEFINE BCB_4_UP}
  {$DEFINE BCB_3_UP}
  {$DEFINE BCB_1_UP}
{$ENDIF}


{$IFDEF BCB_8}
  {$DEFINE BCB_8_UP}
  {$DEFINE BCB_7_UP}
  {$DEFINE BCB_6_UP}
  {$DEFINE BCB_5_UP}
  {$DEFINE BCB_4_UP}
  {$DEFINE BCB_3_UP}
  {$DEFINE BCB_1_UP}
{$ENDIF}

{$IFDEF BCB_7}
  {$DEFINE BCB_7_UP}
  {$DEFINE BCB_6_UP}
  {$DEFINE BCB_5_UP}
  {$DEFINE BCB_4_UP}
  {$DEFINE BCB_3_UP}
  {$DEFINE BCB_1_UP}
{$ENDIF}

{$IFDEF BCB_6}
  {$DEFINE BCB_6_UP}
  {$DEFINE BCB_5_UP}
  {$DEFINE BCB_4_UP}
  {$DEFINE BCB_3_UP}
  {$DEFINE BCB_1_UP}
{$ENDIF}

{$IFDEF BCB_5}
  {$DEFINE BCB_5_UP}
  {$DEFINE BCB_4_UP}
  {$DEFINE BCB_3_UP}
  {$DEFINE BCB_1_UP}
{$ENDIF}

{$IFDEF BCB_4}
  {$DEFINE BCB_4_UP}
  {$DEFINE BCB_3_UP}
  {$DEFINE BCB_1_UP}
{$ENDIF}

{$IFDEF BCB_3}
  {$DEFINE BCB_3_UP}
  {$DEFINE BCB_1_UP}
{$ENDIF}

{$IFDEF BCB_1}
  {$DEFINE BCB_1_UP}
{$ENDIF}

//-----------------------------------------------------------------------------
// COMPILERX_UP from COMPILERX mappings
//-----------------------------------------------------------------------------

{$IFDEF COMPILER_10}
  {$DEFINE COMPILER_10_UP}
  {$DEFINE COMPILER_9_UP}
  {$DEFINE COMPILER_8_UP}
  {$DEFINE COMPILER_7_UP}
  {$DEFINE COMPILER_6_UP}
  {$DEFINE COMPILER_5_UP}
  {$DEFINE COMPILER_4_UP}
  {$DEFINE COMPILER_35_UP}
  {$DEFINE COMPILER_3_UP}
  {$DEFINE COMPILER_2_UP}
  {$DEFINE COMPILER_1_UP}
{$ENDIF}

{$IFDEF COMPILER_9}
  {$DEFINE COMPILER_9_UP}
  {$DEFINE COMPILER_8_UP}
  {$DEFINE COMPILER_7_UP}
  {$DEFINE COMPILER_6_UP}
  {$DEFINE COMPILER_5_UP}
  {$DEFINE COMPILER_4_UP}
  {$DEFINE COMPILER_35_UP}
  {$DEFINE COMPILER_3_UP}
  {$DEFINE COMPILER_2_UP}
  {$DEFINE COMPILER_1_UP}
{$ENDIF}

{$IFDEF COMPILER_8}
  {$DEFINE COMPILER_8_UP}
  {$DEFINE COMPILER_7_UP}
  {$DEFINE COMPILER_6_UP}
  {$DEFINE COMPILER_5_UP}
  {$DEFINE COMPILER_4_UP}
  {$DEFINE COMPILER_35_UP}
  {$DEFINE COMPILER_3_UP}
  {$DEFINE COMPILER_2_UP}
  {$DEFINE COMPILER_1_UP}
{$ENDIF}

{$IFDEF COMPILER_7}
  {$DEFINE COMPILER_7_UP}
  {$DEFINE COMPILER_6_UP}
  {$DEFINE COMPILER_5_UP}
  {$DEFINE COMPILER_4_UP}
  {$DEFINE COMPILER_35_UP}
  {$DEFINE COMPILER_3_UP}
  {$DEFINE COMPILER_2_UP}
  {$DEFINE COMPILER_1_UP}
{$ENDIF}

{$IFDEF COMPILER_6}
  {$DEFINE COMPILER_6_UP}
  {$DEFINE COMPILER_5_UP}
  {$DEFINE COMPILER_4_UP}
  {$DEFINE COMPILER_35_UP}
  {$DEFINE COMPILER_3_UP}
  {$DEFINE COMPILER_2_UP}
  {$DEFINE COMPILER_1_UP}
{$ENDIF}

{$IFDEF COMPILER_5}
  {$DEFINE COMPILER_5_UP}
  {$DEFINE COMPILER_4_UP}
  {$DEFINE COMPILER_35_UP}
  {$DEFINE COMPILER_3_UP}
  {$DEFINE COMPILER_2_UP}
  {$DEFINE COMPILER_1_UP}
{$ENDIF}

{$IFDEF COMPILER_4}
  {$DEFINE COMPILER_4_UP}
  {$DEFINE COMPILER_35_UP}
  {$DEFINE COMPILER_3_UP}
  {$DEFINE COMPILER_2_UP}
  {$DEFINE COMPILER_1_UP}
{$ENDIF}

{$IFDEF COMPILER_35}
  {$DEFINE COMPILER_35_UP}
  {$DEFINE COMPILER_3_UP}
  {$DEFINE COMPILER_2_UP}
  {$DEFINE COMPILER_1_UP}
{$ENDIF}

{$IFDEF COMPILER_3}
  {$DEFINE COMPILER_3_UP}
  {$DEFINE COMPILER_2_UP}
  {$DEFINE COMPILER_1_UP}
{$ENDIF}

{$IFDEF COMPILER_2}
  {$DEFINE COMPILER_2_UP}
  {$DEFINE COMPILER_1_UP}
{$ENDIF}

{$IFDEF COMPILER_1}
  {$DEFINE COMPILER_1_UP}
{$ENDIF}

//-----------------------------------------------------------------------------
// Map COMPILERX_UP to friendly feature names
//-----------------------------------------------------------------------------

{$IFDEF COMPILER_3_UP}
  {$DEFINE SUPPORTS_WIDESTRING}
  {$DEFINE SUPPORTS_INTERFACE}
{$ENDIF}

{$IFDEF COMPILER_35_UP}
  {$DEFINE SUPPORTS_EXTSYM}
  {$DEFINE SUPPORTS_NODEFINE}
{$ENDIF}

{$IFDEF COMPILER_4_UP}
  {$DEFINE SUPPORTS_INT64}
  {$DEFINE SUPPORTS_DYNAMICARRAYS}
  {$DEFINE SUPPORTS_DEFAULTPARAMS}
  {$DEFINE SUPPORTS_OVERLOAD}
{$ENDIF}

//-----------------------------------------------------------------------------
// Cross-platform related defines
//-----------------------------------------------------------------------------

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE UNIX}
  {$DEFINE COMPLIB_CLX}
{$ENDIF}

{$IFNDEF COMPLIB_CLX}
  {$DEFINE COMPLIB_VCL}
{$ENDIF}

{$IFDEF SUPP_INLINE}
  {.$INLINE AUTO}
{$ENDIF}