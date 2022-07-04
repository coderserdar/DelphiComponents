{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Include file                                  }
{                                                       }
{         Copyright (c) 1997, 1999                      }
{                                                       }
{*******************************************************}


{$DEFINE _D3_}   { Delphi 5.0 or higher }
{$DEFINE _D4_}   { Delphi 5.0 or higher }
{$DEFINE _D5_}   { Delphi 5.0 or higher }

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
