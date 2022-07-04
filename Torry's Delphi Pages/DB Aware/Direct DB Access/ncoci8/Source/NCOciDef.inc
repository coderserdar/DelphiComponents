{*******************************************************}
{File:      NCOciDef.inc                                }
{Revision:  0.01.05 / 13.04.2005                        }
{Comment:   NC OCI8 VCL: define names and switches      }
{Copyright: (c) 1999-2005, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{*******************************************************}

{$ifdef VER100}      // Delphi 3
    {$define OCI_D3}
    {$define OCI_DELPHI}
{$endif}

{$ifdef VER110}      // C++ Builder 3
    {$define OCI_D3}
    {$define OCI_D35}
    {$define OCI_BCB}
{$endif}

{$ifdef VER120}      // Delphi 4
    {$define OCI_D3}
    {$define OCI_D35}
    {$define OCI_D4}
    {$define OCI_DELPHI}
{$endif}

{$ifdef VER125}      // C++ Builder 4
    {$define OCI_D3}
    {$define OCI_D35}
    {$define OCI_D4}
    {$define OCI_BCB}
{$endif}

{$ifdef VER130}      // C++ Builder 5 & Delphi 5
    {$define OCI_D3}
    {$define OCI_D35}
    {$define OCI_D4}
    {$define OCI_D5}
    {$IFDEF BCB}
        {$define OCI_BCB}    // C++ Builder 5
    {$ELSE}
        {$define OCI_DELPHI} // Delphi 5
    {$ENDIF}
{$endif}

{$ifdef VER140}      // Delphi 6 and C++ Builder 6
    {$define OCI_D3}
    {$define OCI_D35}
    {$define OCI_D4}
    {$define OCI_D5}
    {$define OCI_D6}
    {$define OCI_DELPHI}
    {$IFDEF BCB}
        {$define OCI_BCB}    // C++ Builder 6
    {$ELSE}
        {$define OCI_DELPHI} // Delphi 6
    {$ENDIF}
{$endif}

{$ifdef VER150} // Delphi 7
    {$define OCI_D3}
    {$define OCI_D35}
    {$define OCI_D4}
    {$define OCI_D5}
    {$define OCI_D6}
    {$define OCI_D7}
    {$define OCI_DELPHI}
{$endif}

{$ifdef VER170} // Delphi 2005
    {$define OCI_D3}
    {$define OCI_D35}
    {$define OCI_D4}
    {$define OCI_D5}
    {$define OCI_D6}
    {$define OCI_D7}
    {$define OCI_D9}
    {$define OCI_DELPHI}
{$endif}

{$ifdef VER180} // Delphi 2006
    {$define OCI_D3}
    {$define OCI_D35}
    {$define OCI_D4}
    {$define OCI_D5}
    {$define OCI_D6}
    {$define OCI_D7}
    {$define OCI_D9}
    {$define OCI_D10}
    {$IFDEF BCB}
        {$define OCI_BCB}    // C++ Builder
    {$ELSE}
        {$define OCI_DELPHI} // Delphi
    {$ENDIF}
{$endif}

{$ifdef OCI_D7}
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CODE off}
  {$warn UNSAFE_CAST off}
{$endif}

{$ifdef OCI_BCB}
    {$ObjExportAll On}
{$endif}

{$B-} // Complete bool eval off
{$A+} // Align records on
{$R-} // Range checking off
{$X+} // Extended syntax on
{$T-} // Type @ off
{$H+} // Huge strings on

{$UNDEF OCI_USE_BINDDYNAMIC}        // define when dynamic bind will be stable
{$UNDEF OCI_DEBUG}                  // define for some debug stuffs
{$UNDEF OCI_EXPRESSION_STANDALONE}  // define when TOCIExpression will used in
                                    // standalone App, without other NCOCI8 units
{$UNDEF OCI_BDE}                    // define to use BDE compatibility
