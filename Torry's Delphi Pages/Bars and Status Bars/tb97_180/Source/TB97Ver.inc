{ $jrsoftware: tb97/Source/TB97Ver.inc,v 1.3 2004/02/23 22:07:45 jr Exp $ }

{ Determine Delphi/C++Builder version }
{$IFNDEF WIN32} Delphi 1 is not supported. {$ENDIF}
{$IFNDEF VER90}  { if it's not Delphi 2.0 }
  {$IFNDEF VER93}  { and it's not C++Builder 1.0 }
    {$DEFINE TB97D3}  { then it must be at least Delphi 3 or C++Builder 3 }
    {$IFNDEF VER100}  { if it's not Delphi 3.0 }
      {$IFNDEF VER120}  { Delphi 4/5's command line compiler doesn't like the ObjExportAll directive, so don't include it on Delphi 4/5 }
        {$IFNDEF VER130}
          {$ObjExportAll On}  { <- needed for compatibility with run-time packages in C++Builder 3 }
        {$ENDIF}
      {$ENDIF}
      {$IFNDEF VER110}  { and it's not C++Builder 3.0 }
        {$DEFINE TB97D4}  { then it must be at least Delphi 4 or C++Builder 4 }
        {$IFNDEF VER120} {$IFNDEF VER125}  { if it's not Delphi 4 or C++Builder 4 }
          {$DEFINE TB97D5}  { then it must be at least Delphi 5 or C++Builder 5 }
          {$IFNDEF VER130}  { if it's not Delphi 5 or C++Builder 5 }
            {$DEFINE TB97D6}  { then it must be at least Delphi 6 or C++Builder 6 }
          {$ENDIF}
        {$ENDIF} {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$ALIGN ON}
{$BOOLEVAL OFF}
{$LONGSTRINGS ON}
{$WRITEABLECONST ON}
