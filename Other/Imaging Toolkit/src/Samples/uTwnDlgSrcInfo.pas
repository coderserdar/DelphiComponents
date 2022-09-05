{ $HDR$}
{------------------------------------------------------------------------------}
{ MCM DESIGN                                                                   }
{                                                                              }
{ For further information / comments, visit our WEB site at                    }
{   www.mcm-design.com                                                         }
{ or e-mail to                                                                 }
{   CustomerCare@mcm-design.dk                                                 }
{------------------------------------------------------------------------------}
{}
{ $Log:  15932: uTwnDlgSrcInfo.pas 
//
//    Rev 1.3    2014-01-15 13:42:06  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.2    2013-12-05 21:43:38  mcm
//
//    Rev 1.1    2013-12-04 23:16:20  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//   Rev 1.0    04-12-2001 16:49:14  mcm    Version: DT 2.0

{
{   Rev 1.0    23-10-00 22:06:59  mcm
{ TWAIN Data Source info dialogue.
}
{}
unit uTwnDlgSrcInfo;

{$INCLUDE mcmDefines.pas}

interface

uses {$IFNDEF GE_DXE2}
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls,
     twain, mcmTWAIN;
    {$ELSE}
     WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
     Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.Controls, Vcl.StdCtrls,
     twain, mcmTWAIN;
    {$ENDIF}

type
  TFormSrcInfo        = class(TForm)
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
    procedure SetSourceInfo(Value : TTwnSourceInfo);
  public
    { Public declarations }
    property SourceInfo : TTwnSourceInfo
      write SetSourceInfo;
  end;

var FormSrcInfo : TFormSrcInfo;

implementation

{$R *.DFM}

procedure TFormSrcInfo.SetSourceInfo(Value : TTwnSourceInfo);
var TmpStr : string;
begin
  if Assigned(Value)
  then begin
       lValTWAINID.Caption       := IntToHex(Value.Id, 8);
       lValVersion.Caption       := Value.Version;
       lValLanguage.Caption      := IntToStr(longint(Value.Language));
       lValCountry.Caption       := IntToStr(longint(Value.Country));
       lValInfo.Caption          := Value.Info;
       lValProtocol.Caption      := Value.Protocol;
       TmpStr := '';

       if ((Value.SupportedGroups and DG_CONTROL) <> 0)
       then TmpStr := TmpStr + 'DG_CONTROL, ';
       if ((Value.SupportedGroups and DG_IMAGE) <> 0)
       then TmpStr := TmpStr + 'DG_IMAGE, ';
       if ((Value.SupportedGroups and DG_AUDIO) <> 0)
       then TmpStr := TmpStr + 'DG_AUDIO, ';
       lValGroups.Caption        := Copy(TmpStr, 1, Length(TmpStr) - 2);
       lValManufacturer.Caption  := Value.Manufacturer;
       lValProductFamily.Caption := Value.ProductFamily;
       lValProductName.Caption   := Value.ProductName;
  end;
end; { End TFormSrcInfo.SetSourceInfo.                                         }

end.
