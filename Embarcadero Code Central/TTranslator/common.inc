{===============================================================================
  common.inc Include File

  This file is included into each component unit and serves as a common
  place to add conditional defines and compiler directives to be used by all
  component units.

  Copyright © 1995-2000 by Ray Konopka
===============================================================================}

{DEFINE TRANSLATOR}
{DEFINE NO_DTI}

{$IFDEF WIN32}
  {$DEFINE D2_OR_HIGHER}
{$ENDIF}

{$IFNDEF VER80}
  {$IFNDEF VER90}
    {$IFNDEF VER93}
      {$DEFINE D3_OR_HIGHER}
      {$IFNDEF VER100}
        {$DEFINE BCB3_OR_HIGHER}
        {$IFNDEF VER110}
          {$DEFINE D4_OR_HIGHER}
          {$IFNDEF VER120}
            {$DEFINE BCB4_OR_HIGHER}
            {$IFNDEF VER125}
              {$DEFINE D5_OR_HIGHER}
                {$ifndef VER130}
                  {$define D6_OR_HIGHER}
                  {$ifndef VER140}
                    {$define D7_OR_HIGHER}
                  {$endif}
                {$endif}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

// LAA-tillsatt
{$ifndef LINUX}
{$ifdef VER130}
{$define USE_RESOURCESTRINGS}
{$endif VER130}
{$endif LINUX}
{$ifdef WEBAPPLICATION}
{$define NO_DTI}
{$endif WEBAPPLICATION}

{$IFDEF VER93}
  {$DEFINE BCB_COMPILER}
{$ENDIF}
{$IFDEF VER110}
  {$DEFINE BCB_COMPILER}
  {$OBJEXPORTALL ON}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE BCB_COMPILER}
  {$OBJEXPORTALL ON}
{$ENDIF}

