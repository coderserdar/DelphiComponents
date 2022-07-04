{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Include file                                  }
{                                                       }
{         Copyright (c) 1997, 1999                      }
{                                                       }
{*******************************************************}


{$IFNDEF VER80}
  {$IFNDEF VER90}
    {$IFNDEF VER93}
      {$DEFINE _D3_} { Delphi 3.0 or higher }

      {$IFDEF VER110} { Borland C++Builder 3.0 }
        {$DEFINE CBUILDER}
        {$ObjExportAll On}
      {$ELSE}
        {$IFNDEF VER100}
          {$DEFINE _D4_}   { Delphi 4.0 or higher }
          {$IFDEF VER130}
            {$DEFINE _D5_}   { Delphi 5.0 or higher }
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{ Comment next conditional defines if needed }
{$DEFINE RX240} { RX library 2.40 or higher }

{$IFDEF _D3_}
  {$DEFINE MIDAS} { Multi-tier part of library }
{$ENDIF}

{ Don't change next conditional defines }
{$B-} {- Complete Boolean Evaluation }
{$R-} {- Range-Checking }
{$V-} {- Var-String Checking }
{$T-} {- Typed @ operator }
{$X+} {- Extended syntax }
{$P+} {- Open string params }
{$J+} {- Writeable structured consts }
{$H+} {- Use long strings by default }
