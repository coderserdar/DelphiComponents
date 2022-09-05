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
// $Log:  15922: uTwnDlgAppInfo.pas 
//
//    Rev 1.3    2014-01-15 13:42:06  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.2    2013-12-04 23:16:18  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.1    25-10-2009 16:44:30  mcm    Version: DT 3.10
// Support for Delphi 2010
//
//   Rev 1.0    04-12-2001 16:49:12  mcm    Version: DT 2.0

unit uTwnDlgAppInfo;

{$INCLUDE mcmDefines.pas}

interface

uses {$IFDEF GE_DXE2}
     WinApi.Windows, System.SysUtils, WinApi.Messages, System.Classes,
     Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
     {$ELSE}
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls,
     {$ENDIF}
     twain;

type
  TFormAppInfo        = class(TForm)
    gbInformation     : TGroupBox;
    btnOK             : TButton;
    lTWAINID          : TLabel;
    lAppVersion       : TLabel;
    lAppLanguage      : TLabel;
    lAppCountry       : TLabel;
    lAppInfo          : TLabel;
    lAppProtocol      : TLabel;
    lAppGroups        : TLabel;
    lAppManufacturer  : TLabel;
    lAppProductFamily : TLabel;
    lAppProductName   : TLabel;
    lValTWAINID       : TLabel;
    lValVersion       : TLabel;
    lValLanguage      : TLabel;
    lValCountry       : TLabel;
    lValInfo          : TLabel;
    lValProtocol      : TLabel;
    lValGroups        : TLabel;
    lValManufacturer  : TLabel;
    lValProductFamily : TLabel;
    lValProductName   : TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetAppID(Identity : pTW_IDENTITY);
  end;

var FormAppInfo : TFormAppInfo;

implementation

{$R *.DFM}

procedure TFormAppInfo.SetAppID(Identity : pTW_IDENTITY);
var TmpStr : string;
begin
  if (Identity <> Nil)
  then begin
       with Identity^
       do begin
          lValTWAINID.Caption       := IntToStr(Id);
          lValVersion.Caption       := IntToStr(Version.MajorNum) + '.' + IntToStr(Version.MinorNum);
          lValLanguage.Caption      := IntToStr(Version.Language);
          lValCountry.Caption       := IntToStr(Version.Country);
          lValInfo.Caption          := String(StrPas(Version.Info));
          lValProtocol.Caption      := IntToStr(ProtocolMajor) + '.' + IntToStr(ProtocolMinor);
          TmpStr := '';
          if ((SupportedGroups and DG_CONTROL) <> 0)
          then TmpStr := TmpStr + 'DG_CONTROL, ';
          if ((SupportedGroups and DG_IMAGE) <> 0)
          then TmpStr := TmpStr + 'DG_IMAGE, ';
          if ((SupportedGroups and DG_AUDIO) <> 0)
          then TmpStr := TmpStr + 'DG_AUDIO, ';
          lValGroups.Caption        := Copy(TmpStr, 1, Length(TmpStr) - 2);
          lValManufacturer.Caption  := String(StrPas(Manufacturer));
          lValProductFamily.Caption := String(StrPas(ProductFamily));
          lValProductName.Caption   := String(StrPas(ProductName));
       end;
  end;
end; { End TFormAppInfo.SetAppID.                                              }

end.
