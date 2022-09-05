// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //
//                                                                            //
// For further information / comments, visit our WEB site at                  //
//   www.mcm-design.com                                                       //
// or e-mail to                                                               //
//   CustomerCare@mcm-design.dk                                               //
//----------------------------------------------------------------------------//
//
// $Log:  21513: InstmcmOCR.pas 
//
//    Rev 1.7    01-03-2011 20:39:54  mcm    Version: IMG 3.4
//
//    Rev 1.6    25-10-2009 17:28:18  mcm
// Support for Delphi 2010
//
//    Rev 1.5    10-08-2009 00:48:30  mcm    Version: IMG 3.3
// Delphi 2009 support
//
//    Rev 1.4    27-08-2007 18:54:00  mcm    Version: IMG 3.1
// Added support for Delphi 2007
//
//    Rev 1.3    22-12-2005 20:52:12  mcm    Version: IMG 2.12
// Delphi 2006 support.
//
//   Rev 1.2    03-01-2005 18:26:32  mcm    Version: IMG 2.7
// Added support for Delphi 2005.

//
//   Rev 1.1    16-10-2003 11:30:18  mcm    Version: OCR 1.0

//
//   Rev 1.0    25-09-2003 23:26:18  mcm    Version: IMG 1.5

//
//   Rev 1.5    15-06-2003 13:08:00  mcm    Version: IMG 1.3.4
// Added components TmcmMultiImageCtrl and TmcmImageMorph.

//
//   Rev 1.4    11-03-2003 00:08:00  mcm    Version: IMG 1.3.2
// Added TmcmImageDualView component.

//
//   Rev 1.3    27-01-2003 14:00:04  mcm
// Added mcmImageMath unit.

//
//   Rev 1.2    08-10-2002 13:01:32  mcm    Version: IMG 1.2

//
//   Rev 1.0    27-05-2002 16:21:52  mcm

unit InstmcmOCR;

interface

{$Include 'mcmDefines.pas'}

{$IFDEF DCB2_5}
  uses Dsgnintf;
{$ELSE}
  uses DesignIntf, DesignEditors;
{$ENDIF}

type
  TmcmOCRPropMenu = class(TComponentEditor)
  public
    function GetVerbCount : Integer; override;
    function GetVerb(Index : Integer): string; override;
    procedure ExecuteVerb(Index : Integer); override;
  end;

procedure Register;

implementation

uses {$IFNDEF GE_DXE2}
       Windows, Classes, SysUtils, Dialogs,
     {$ELSE}
       System.Classes, WinApi.Windows, System.SysUtils, System.Types, Vcl.Dialogs,
     {$ENDIF}
     mcmOCR, uOcrAbout;

procedure Register;
begin
  RegisterComponents('Imaging Toolbox', [TmcmOCR]);
  RegisterComponentEditor(TmcmOCR, TmcmOCRPropMenu);
end; // Register.


function TmcmOcrPropMenu.GetVerbCount : Integer;
begin
  Result := 1;
end; // TmcmOcrPropMenu.GetVerbCount.


function TmcmOcrPropMenu.GetVerb(Index : Integer): string;
begin
  case Index of
  0 : Result := '&About';
//  1 : Result := '&Preferrences';
  end;
end; // TmcmOcrPropMenu.GetVerb.


procedure TmcmOcrPropMenu.ExecuteVerb(Index : Integer);
begin
  case Index of
  0 : begin
        mcmOcrAboutBox := TmcmOcrAboutBox.Create(Nil);
        {$IFDEF MCMOCRDEMO}
          mcmOcrAboutBox.DemoVersion := True;
        {$ENDIF}
        mcmOcrAboutBox.ShowModal;
        mcmOcrAboutBox.Free;
      end;
  1 : begin
      end;
  end;
end; // TmcmOcrPropMenu.ExecuteVerb.

end.
