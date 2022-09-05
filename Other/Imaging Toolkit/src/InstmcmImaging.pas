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
// $Log:  17523: InstmcmImaging.pas
//
//    Rev 1.22    2014-02-02 21:09:52  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.21    01-03-2011 20:39:54  mcm    Version: IMG 3.4
//
//    Rev 1.20    25-10-2009 21:32:10  mcm
//
//    Rev 1.19    25-10-2009 17:23:48  mcm
// Support for Delphi 2010
//
//    Rev 1.18    10-08-2009 00:47:18  mcm
// Delphi 2009 support
//
//    Rev 1.17    01-01-2009 16:00:34  mcm    Version: IMG 3.3
// Delphi 2009 support
//
//    Rev 1.16    20-08-2007 20:28:36  mcm
// Added support for Delphi 2007
//
//    Rev 1.15    21-05-2006 13:11:18  mcm    Version: IMG 3.0
// Added mcmTrace component.
//
//    Rev 1.14    22-12-2005 20:52:12  mcm    Version: IMG 2.12
// Delphi 2006 support.
//
//   Rev 1.13    24-07-2005 18:59:48  mcm    Version: IMG 2.9
// Added TmcmDropSource and TmcmDropTarget controls.

//
//   Rev 1.12    03-01-2005 18:26:32  mcm    Version: IMG 2.7
// Added support for Delphi 2005.

//
//   Rev 1.11    01-09-2004 18:29:18  mcm    Version: IMG 2.6
// Moved the TmcmImageDB control to it's own package to support the toolkit in
// Delphi/c++Builder standard edition.

//
//   Rev 1.10    21-07-2004 21:02:00  mcm    Version: IMG 2.5

//
//   Rev 1.9    30-01-2004 19:38:18  mcm    Version: IMG 2.3
// Added mcmGraphics unit, TmcmProfile.

//
//   Rev 1.8    10-12-2003 15:33:40  mcm
// Added TmcmColorGrid.

//
//   Rev 1.7    24-11-2003 20:20:32  mcm
// Added component TmcmImageDB.

//
//   Rev 1.6    16-10-2003 11:30:08  mcm    Version: IMG 1.6

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

unit InstmcmImaging;

interface

{$Include 'mcmDefines.pas'}

{$IFDEF DCB2_5}
  uses Dsgnintf;
{$ELSE}
  uses DesignIntf, DesignEditors;
{$ENDIF}

type
  TmcmPropMenu = class(TComponentEditor)
  public
    function GetVerbCount : Integer; override;
    function GetVerb(Index : Integer): string; override;
    procedure ExecuteVerb(Index : Integer); override;
  end;

procedure Register;

implementation

uses {$IFNDEF GE_DXE2}
      Windows, SysUtils, Classes, Dialogs, TypInfo,
     {$ELSE}
      WinApi.Windows, System.SysUtils, System.Classes, Vcl.Dialogs, System.TypInfo,
     {$ENDIF}
     mcmImage,
     mcmControls,
     mcmFileDialogues,
     mcmImageColor,
     mcmImageFilter,
     mcmImageMath,
     mcmImageMorph,
     mcmImageTransform,
     mcmImageDualView,
     mcmPrinter,
     mcmThumbnail,
     mcmTrace,
     mcmColorGrid,
     mcmGraphics,
     mcmRegion,
     mcmShape,
     mcmDragDrop,
     uImgAbout;

procedure Register;
begin
  // RegisterNoIcon([TmcmImage]);
  // RegisterNoIcon([TmcmTIFFFile]);
  RegisterComponents('Imaging Toolbox', [TmcmImageColor]);
  RegisterComponents('Imaging Toolbox', [TmcmImageCtrl]);
//  RegisterComponents('Imaging Toolbox', [TmcmImageDB]);
  RegisterComponents('Imaging Toolbox', [TmcmMultiImageCtrl]);
  RegisterComponents('Imaging Toolbox', [TmcmImageDualView]);
  RegisterComponents('Imaging Toolbox', [TmcmImageFilter]);
  RegisterComponents('Imaging Toolbox', [TmcmImageMath]);
  RegisterComponents('Imaging Toolbox', [TmcmImageMorph]);
  RegisterComponents('Imaging Toolbox', [TmcmImageTransform]);
  RegisterComponents('Imaging Toolbox', [TmcmPrinter]);
  RegisterComponents('Imaging Toolbox', [TmcmPrintPreview]);
  RegisterComponents('Imaging Toolbox', [TmcmRegion]);
  RegisterComponents('Imaging Toolbox', [TmcmThumbView]);
  RegisterComponents('Imaging Toolbox', [TmcmTrace]);
  RegisterComponents('Imaging Toolbox', [TmcmColorGrid]);
  RegisterComponents('Imaging Toolbox', [TmcmProfile]);
  RegisterComponents('Imaging Toolbox', [TmcmShape]);
  RegisterComponents('Imaging Toolbox', [TmcmShellComboBox]);
  RegisterComponents('Imaging Toolbox', [TmcmShellListView]);
  RegisterComponents('Imaging Toolbox', [TmcmOpenDialog]);
  RegisterComponents('Imaging Toolbox', [TmcmSaveDialog]);
  RegisterComponents('Imaging Toolbox', [TmcmDropSource]);
  RegisterComponents('Imaging Toolbox', [TmcmDropTarget]);

  RegisterComponentEditor(TmcmImageColor, TmcmPropMenu);
  RegisterComponentEditor(TmcmImageCtrl, TmcmPropMenu);
//  RegisterComponentEditor(TmcmImageDB, TmcmPropMenu);
  RegisterComponentEditor(TmcmMultiImageCtrl, TmcmPropMenu);
  RegisterComponentEditor(TmcmImageFilter, TmcmPropMenu);
  RegisterComponentEditor(TmcmImageDualView, TmcmPropMenu);
  RegisterComponentEditor(TmcmImageMath, TmcmPropMenu);
  RegisterComponentEditor(TmcmImageMorph, TmcmPropMenu);
  RegisterComponentEditor(TmcmImageTransform, TmcmPropMenu);
  RegisterComponentEditor(TmcmPrinter, TmcmPropMenu);
  RegisterComponentEditor(TmcmPrintPreview, TmcmPropMenu);
  RegisterComponentEditor(TmcmColorGrid, TmcmPropMenu);
  RegisterComponentEditor(TmcmProfile, TmcmPropMenu);
  RegisterComponentEditor(TmcmRegion, TmcmPropMenu);
  RegisterComponentEditor(TmcmThumbView, TmcmPropMenu);
  RegisterComponentEditor(TmcmTrace, TmcmPropMenu);
  RegisterComponentEditor(TmcmShellComboBox, TmcmPropMenu);
  RegisterComponentEditor(TmcmShellListView, TmcmPropMenu);
  RegisterComponentEditor(TmcmOpenDialog, TmcmPropMenu);
  RegisterComponentEditor(TmcmSaveDialog, TmcmPropMenu);
  RegisterComponentEditor(TmcmDropSource, TmcmPropMenu);
  RegisterComponentEditor(TmcmDropTarget, TmcmPropMenu);
  //  RegisterPropertyEditor(TypeInfo(TStrings), TmcmSaveDialog, 'Filter', TStringProperty);
  //  RegisterPropertyEditor(TypeInfo(TFileName), TmcmOpenDialog, 'FileName', TStringProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TmcmOpenDialog, 'Filter', TStringProperty);
  //  RegisterPropertyEditor(TypeInfo(TStrings), TmcmOpenDialog, 'Filter', TStringProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TmcmSaveDialog, 'Filter', TStringProperty);
end; // Register.


function TmcmPropMenu.GetVerbCount : Integer;
begin
  Result := 1;
end; // TmcmPropMenu.GetVerbCount.


function TmcmPropMenu.GetVerb(Index : Integer): string;
begin
  case Index of
  0 : Result := '&About';
//  1 : Result := '&Preferrences';
  end;
end; // TmcmPropMenu.GetVerb.


procedure TmcmPropMenu.ExecuteVerb(Index : Integer);
begin
  case Index of
  0 : begin
        mcmImgAboutBox := TmcmImgAboutBox.Create(Nil);
        {$IFDEF MCMDEMO}
          mcmImgAboutBox.DemoVersion := True;
        {$ENDIF}
        mcmImgAboutBox.ShowModal;
        mcmImgAboutBox.Free;
      end;
  1 : begin
      end;
  end;
end; // TmcmPropMenu.ExecuteVerb.

end.
