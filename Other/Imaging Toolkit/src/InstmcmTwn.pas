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
// $Log:  15882: InstmcmTwn.pas
//
//    Rev 1.14    2014-02-02 16:46:10  mcm
// Change company name
//
//    Rev 1.13    2014-01-15 13:41:56  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.12    2013-12-04 23:16:10  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.11    2013-11-25 21:32:36  mcm
// Added XE2 About dialogue.
//
//    Rev 1.10    01-03-2011 20:39:54  mcm    Version: IMG 3.4
//
//    Rev 1.9    25-10-2009 16:44:30  mcm    Version: DT 3.10
// Support for Delphi 2010
//
//    Rev 1.8    27-12-2008 15:03:00  mcm    Version: DT 3.8
// Delphi 2009 support
//
//    Rev 1.7    11-08-2007 10:59:14  mcm    Version: DT 3.7
// Added support for Delphi 2007
//
//    Rev 1.6    22-12-2005 18:08:34  mcm    Version: DT 3.6
// Added support for Delphi 2006
//
//   Rev 1.5    03-01-2005 18:35:20  mcm    Version: DT 3.3
// Added support for Delphi 2005.

//
//   Rev 1.4    15-04-2003 10:30:20  mcm    Version: DT 2.3
// Added support for BCB 6

//
//   Rev 1.3    06-03-2003 11:02:56  mcm    Version: DT 2.2
// Included the mcmSTI component.

//
//   Rev 1.2    14-10-02 00:21:42  mcm

//
//   Rev 1.1    08-10-2002 14:28:32  mcm    Version: DT2.1

//
//   Rev 1.0    04-12-2001 16:49:06  mcm    Version: DT 2.0

unit InstmcmTwn;

{$INCLUDE mcmDefines.pas}

interface

{$IFDEF CLR} // VCL .NET
//       System.Drawing,
//       System.ComponentModel,
//       Borland.Vcl.Types,
  uses  //Borland.Vcl.Design.DesignIntf,
       Borland.Vcl.Design.DesignEditors;
{$ELSE} // VCL Win32
  {$IFDEF DCB2_5}
    uses Dsgnintf;
  {$ELSE}
    uses DesignIntf, DesignEditors;
  {$ENDIF}
{$ENDIF}


type
  TmcmPropMenu = class(TComponentEditor)
  public
    function GetVerbCount : Integer; override;
    function GetVerb(Index : Integer): string; override;
    procedure ExecuteVerb(Index : Integer); override;
  end;

  TFilenameProperty = class(TStringProperty)
  public
    { Public Declarations }
    procedure Edit; override;
    function  GetAttributes : TPropertyAttributes; override;
  end;

  TFileFormatsProperty = class(TEnumProperty)
  public
    function  GetAttributes : TPropertyAttributes; override;
  end;


procedure Register;


implementation

uses {$IFDEF GE_DXE2}
     WinApi.Windows, System.SysUtils, System.Classes, Vcl.Dialogs,
     {$ELSE}
     Classes, Windows, SysUtils, Dialogs,
     {$ENDIF}
     twain,
     mcmSTI,
     mcmTWAIN,
     mcmTWAINContainer,
     uTwnAbout;

procedure Register;
begin
  RegisterNoIcon([TTwnContainer]);
  RegisterNoIcon([TTwnContainerList]);
  RegisterNoIcon([TTwnFrame]);
  RegisterNoIcon([TTwnSourceInfo]);
//  RegisterComponents('Imaging Toolbox', [TTwnSourceInfo]);
  RegisterComponents('Imaging Toolbox', [TmcmSTI]);
  RegisterComponents('Imaging Toolbox', [TmcmTWAIN]);
  RegisterPropertyEditor(TypeInfo(TFileName),
                         TmcmTWAIN,
                         'FileName',
                         TFilenameProperty);
  RegisterPropertyEditor(TypeInfo(TTwnFileFmt),
                         TmcmTWAIN,
                         'FileFormats',
                         TFileFormatsProperty);
  RegisterComponentEditor(TmcmSTI, TmcmPropMenu);
  RegisterComponentEditor(TmcmTWAIN, TmcmPropMenu);
end; // End Register.


function TmcmPropMenu.GetVerbCount : Integer;
begin
  Result := 1;
end; // End TmcmPropMenu.GetVerbCount.


function TmcmPropMenu.GetVerb(Index : Integer): string;
begin
  case Index of
  0 : Result := '&About';
  1 : Result := '&Preferrences';
  end;
end; // End TmcmPropMenu.GetVerb.


procedure TmcmPropMenu.ExecuteVerb(Index : Integer);
begin
  case Index of
  0 : begin
        mcmAboutBox := TmcmAboutBox.Create(Nil);
        mcmAboutBox.ShowModal;
        mcmAboutBox.Free;
      end;
  1 : begin
      end;
  end;
end; // End TmcmPropMenu.ExecuteVerb.


procedure TFilenameProperty.Edit;
var SaveDialog : TSaveDialog;
    SaveName   : string;
begin
  SaveDialog := TSaveDialog.create(Nil);
  try
    SaveName := lowercase(GetValue);
    SaveDialog.FilterIndex := 2;
    if (pos('.tif', SaveName) = (Length(SaveName) - 3))
    then SaveDialog.FilterIndex := 1
    else if (pos('.bmp', Value) = (Length(Value) - 3))
         then SaveDialog.FilterIndex := 2
         else if (pos('.jpg', Value) = (Length(Value) - 3))
              then SaveDialog.FilterIndex := 3
              else if (pos('.fpx', Value) = (Length(Value) - 3))
                   then SaveDialog.FilterIndex := 4
                   else if (pos('.png', Value) = (Length(Value) - 3))
                        then SaveDialog.FilterIndex := 5;

    SaveDialog.DefaultExt := 'bmp';
    // SaveDialog.filename := RemoveExt(SaveName);
    SaveDialog.filter := 'TIFF (*.tif)|*.tif|Bitmap (*.bmp)|*.bmp|JPEG (*.jpg)|*.jpg|FPX (*.fpx)|*.fpx|PNG (*.png)|*.png';
    SaveDialog.options := [ofHideReadOnly,ofPathMustExist];
    if SaveDialog.execute
    then SetValue(SaveDialog.Filename);
  finally
    SaveDialog.free;
  end;
end; // End TFilenameProperty.Edit.


function TFilenameProperty.GetAttributes : TPropertyAttributes;
begin
  Result := [paDialog];
end; // End TFilenameProperty.GetAttributes.


function TFileFormatsProperty.GetAttributes : TPropertyAttributes;
begin
  Result := [paSubProperties];
end; // End TFileFormatsProperty.GetAttributes.                                 

{$UNDEF DELBCB_V2_V5}

end.
