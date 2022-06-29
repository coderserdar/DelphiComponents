{*******************************************************}
{                                                       }
{            Windows Dialogs interface unit             }
{                   version 2.2                         }
{ Author:                                               }
{ Serhiy Perevoznyk                                     }
{ serge_perevoznyk@hotmail.com                          }
{                                                       }
{     Use, modification and distribution is allowed     }
{without limitation, warranty, or liability of any kind.}
{                                                       }
{*******************************************************}

unit psvdreg;

interface

uses
  Windows,
  Classes,
  Controls,
  psvDialogs,
  {$IFDEF VER130}
  DsgnIntf,
  {$ELSE}
  DesignIntf,
  DesignEditors,
  {$ENDIF}
  Dialogs,
  Forms;

type
  TAppletFileProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;


procedure Register;

implementation

{$R psvDialogs.dcr}

procedure Register;
begin
  RegisterComponents('Dialogs',
  [TpsvOrganizeFavoritesDialog,
   TpsvFormatDialog,
   TpsvComputerNameDialog,
   TpsvBrowseFolderDialog,
   TpsvControlPanelDialog,
   TpsvAppletDialog,
   TpsvChangeIconDialog,
   TpsvShellAboutDialog,
   TpsvRunDialog,
   TpsvObjectPropertiesDialog,
   TpsvOpenDialog,
   TpsvSaveDialog,
   TpsvNewLinkDialog,
   TpsvAddHardwareDialog,
   TpsvOpenWidthDialog,
   TpsvDiskFullDialog,
   TpsvExitWindowsDialog,
   TpsvOutOfMemoryDialog]);
   RegisterPropertyEditor(TypeInfo(String), TpsvAppletDialog, 'AppletName', TAppletFileProperty);
end;



{property editor for TpsvAppletDialog component}
procedure TAppletFileProperty.Edit;
var
  APFileOpen: TOpenDialog;
begin
  APFileOpen := TOpenDialog.Create(Application);
  APFileOpen.Filename := GetValue;
  APFileOpen.Filter := 'Applet File (*.cpl)|*.cpl';
  APFileOpen.Options := APFileOpen.Options + [ofPathMustExist,
    ofFileMustExist];
  try
    if APFileOpen.Execute then SetValue(APFileOpen.Filename);
  finally
    APFileOpen.Free;
  end;
end;

function TAppletFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

end.
