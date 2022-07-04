//******************************************************************************
// COMMON PREPROCESSOR DIRECTIVES
//******************************************************************************
{**************************************************** defines for C++Builder 1 }
{$IFDEF VER93}
  {$DEFINE D1UP}
  {$DEFINE D2UP}
  {$DEFINE CBUILDER}
  {$DEFINE CBUILDER1}
  {$DEFINE CBUILDER1UP}
{$ENDIF}

{********************************************************  defines for Delphi 3 }
{$IFDEF VER100}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI3}
  {$DEFINE D1UP}
  {$DEFINE D2UP}
  {$DEFINE D3UP}
{$ENDIF}

{****************************************************  defines for C++Builder 3 }
{$IFDEF VER110}
  {$DEFINE D1UP}
  {$DEFINE D2UP}
  {$DEFINE D3UP}
  {$DEFINE CBUILDER}
  {$DEFINE CBUILDER3}
  {$DEFINE CBUILDER1UP}
  {$DEFINE CBUILDER3UP}
{$ENDIF}

{********************************************************  defines for Delphi 4 }
{$IFDEF VER120}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI4}
  {$DEFINE D1UP}
  {$DEFINE D2UP}
  {$DEFINE D3UP}
  {$DEFINE D4UP}
{$ENDIF}

{****************************************************  defines for C++Builder 4 }
{$IFDEF VER125}
  {$DEFINE D1UP}
  {$DEFINE D2UP}
  {$DEFINE D3UP}
  {$DEFINE D4UP}
  {$DEFINE CBUILDER}
  {$DEFINE CBUILDER4}
  {$DEFINE CBUILDER1UP}
  {$DEFINE CBUILDER3UP}
  {$DEFINE CBUILDER4UP}
{$ENDIF}

{************************************* defines for Delphi 5 and/or C++Builder 5 }
{$IFDEF VER130}
  {$DEFINE D1UP}
  {$DEFINE D2UP}
  {$DEFINE D3UP}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$IFNDEF BCB}
    {$DEFINE DELPHI}
    {$DEFINE DELPHI5}
  {$ELSE}
    {$DEFINE CBUILDER}
    {$DEFINE CBUILDER5}
    {$DEFINE CBUILDER1UP}
    {$DEFINE CBUILDER3UP}
    {$DEFINE CBUILDER4UP}
    {$DEFINE CBUILDER5UP}
  {$ENDIF}
{$ENDIF}

{*********************************************************  defines for Delphi 6 }
{$IFDEF VER140}
  {$IFNDEF BCB}
    {$DEFINE DELPHI}
    {$DEFINE DELPHI6}
  {$ELSE}
    {$DEFINE CBUILDER}
    {$DEFINE CBUILDER6}
    {$DEFINE CBUILDER1UP}
    {$DEFINE CBUILDER3UP}
    {$DEFINE CBUILDER4UP}
    {$DEFINE CBUILDER5UP}
    {$DEFINE CBUILDER6UP}
  {$ENDIF}
  {$DEFINE D1UP}
  {$DEFINE D2UP}
  {$DEFINE D3UP}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE D6UP}
{$ENDIF}

{*********************************************************  defines for Delphi 7 }
{$IFDEF VER150}
  {$IFNDEF BCB}
    {$DEFINE DELPHI}
    {$DEFINE DELPHI7}
  {$ELSE}
    {$DEFINE CBUILDER}
    {$DEFINE CBUILDER7}
    {$DEFINE CBUILDER1UP}
    {$DEFINE CBUILDER3UP}
    {$DEFINE CBUILDER4UP}
    {$DEFINE CBUILDER5UP}
    {$DEFINE CBUILDER6UP}
    {$DEFINE CBUILDER7UP}
  {$ENDIF}
  {$DEFINE D1UP}
  {$DEFINE D2UP}
  {$DEFINE D3UP}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE D6UP}
  {$DEFINE D7UP}
{$ENDIF}

{*********************************************************  defines for Delphi 2005 }
{$IFDEF VER170}
  {$IFNDEF BCB}
    {$DEFINE DELPHI}
    {$DEFINE DELPHI2005}
  {$ELSE}
    {$DEFINE CBUILDER}
    {$DEFINE CBUILDER7}
    {$DEFINE CBUILDER1UP}
    {$DEFINE CBUILDER3UP}
    {$DEFINE CBUILDER4UP}
    {$DEFINE CBUILDER5UP}
    {$DEFINE CBUILDER6UP}
    {$DEFINE CBUILDER7UP}
    {$DEFINE CBUILDER2005UP}
  {$ENDIF}
  {$DEFINE D1UP}
  {$DEFINE D2UP}
  {$DEFINE D3UP}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE D6UP}
  {$DEFINE D7UP}
  {$DEFINE D2005UP}
{$ENDIF}


{$IFDEF D6UP}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

{$IFDEF D7UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
  {$DEFINE 32BIT}
{$ENDIF}

{$IFDEF DELPHI}
  {$IFDEF D2UP}
    {$DEFINE DELPHI_32BIT}
  {$ENDIF}
{$ENDIF}

{$IFDEF CBUILDER}
  {$DEFINE CBUILDER_32BIT}
{$ENDIF}
//******************************************************************************
