{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995 AO ROSNO                   }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RXCConst;

{ RX controls and components constants }
{
  Reserved diapasone
  from MaxExtStrID - 36
  to   MaxExtStrID - 84
}

interface

const
{ The minimal VCL's used string ID is 61440. The custom IDs must be
  less that above. }
  MaxExtStrID = 61300;

const

{ ToolEdit }

  SBrowse                = MaxExtStrID - 36;
  SDefaultFilter         = MaxExtStrID - 37;

{ PickDate }

  SDateDlgTitle          = MaxExtStrID - 38;
  SNextYear              = MaxExtStrID - 39;
  SNextMonth             = MaxExtStrID - 40;
  SPrevYear              = MaxExtStrID - 41;
  SPrevMonth             = MaxExtStrID - 42;

{ VCLUtils }

  SNotImplemented        = MaxExtStrID - 43;
  SFileNotExec           = MaxExtStrID - 44;
  SLoadLibError          = MaxExtStrID - 45;
  SDetails               = MaxExtStrID - 46;

{ Polaris }

  SDateOutOfRange       = MaxExtStrID - 65;
  SDateOutOfMin         = MaxExtStrID - 66;
  SDateOutOfMax         = MaxExtStrID - 67;

  SDateMinLimit	        = MaxExtStrID - 64;
  SDateMaxLimit         = MaxExtStrID - 63;

implementation

{$IFDEF WIN32}
 {$R *.R32}
{$ELSE}
 {$R *.R16}
{$ENDIF}

end.