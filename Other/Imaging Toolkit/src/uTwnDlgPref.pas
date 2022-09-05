{------------------------------------------------------------------------------}
{ MCM DESIGN                                                                   }
{                                                                              }
{ For further information / comments, visit our WEB site at                    }
{   www.mcm-design.com                                                         }
{ or e-mail to                                                                 }
{   CustomerCare@mcm-design.dk                                                 }
{------------------------------------------------------------------------------}
{}
{ $Log:  15926: uTwnDlgPref.pas 
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
//   Rev 1.1    06-03-2003 11:06:00  mcm    Version: DT 2.2
// Added conditional define to disable warnings on "Unsafe Type, Cast and Code"
// for Delphi 7.

//
//   Rev 1.0    04-12-2001 16:49:14  mcm    Version: DT 2.0

{
{   Rev 1.5    04-06-00 16:37:56  mcm    Version: 1.9.0
{ Modified to use FileFormat property instead of 
{ Get/SetFileFormat.
}
{
{   Rev 1.4    01-06-00 23:39:01  mcm    Version: 1.9.0
{ Added support for FPX, TIFF(Multi-page) and PNG file 
{ format.
}
{
{   Rev 1.3    18-02-00 16:43:32  mcm    Version: 1.8.3
{ Updated use of TtwnXferType according to change in 
{ twain.pas.
}
{
{   Rev 1.2    16-02-00 13:14:02  mcm
{ Replaced TSpinEdit with mcmRealSpinEdit. Solvs 
{ include problem in C++Builder 3.
}
{
{   Rev 1.1    15-02-00 16:54:26  mcm
}
{
{   Rev 1.0    30-12-99 00:29:19  Marc Martin
{ Initial Revision
}
{}
unit uTwnDlgPref;

{------------------------------------------------------------------------------}
{ This unit demonstrates negotiation, using the functions GetCapabilityMsg &   }
{ SetCapabilityMsg and the TtwnContainer.                                      }
{------------------------------------------------------------------------------}

{$INCLUDE mcmDefines.pas}

interface

uses {$IFDEF GE_DXE2}
     WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
     Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
     Vcl.ComCtrls, System.Win.Registry, Vcl.Buttons,
     {$ELSE}
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls, ComCtrls, Registry, Buttons,
     {$ENDIF}
     umcmIntE, mcmTWAIN;

type
  TDlgTwainPref     = class(TForm)
    btnScan         : TButton;
    btnApply        : TButton;
    btnClose        : TButton;

    SaveDialogTwn   : TSaveDialog;

    PageControl     : TPageControl;

    tsSelect        : TTabSheet;
    gbSource        : TGroupBox;
    lProductVer     : TLabel;
    lProdVerValue   : TLabel;
    lManufacturer   : TLabel;
    lManufactValue  : TLabel;
    lProtocolVer    : TLabel;
    lProtVerValue   : TLabel;
    cbSource        : TComboBox;

    gbSettings      : TGroupBox;
    cbDriverSetting : TCheckBox;
    lPredefine      : TLabel;
    cbPredefName    : TComboBox;
    btnAdd          : TButton;
    btnDelete       : TButton;

    tsTransfer      : TTabSheet;
    gbTransfer      : TGroupBox;
    lMechanism      : TLabel;
    cbTransfer      : TComboBox;
    cbDisableAfter  : TCheckBox;
    cbIndicators    : TCheckBox;
    gbFile          : TGroupBox;
    lFileName       : TLabel;
    lFileFormat     : TLabel;
    eFileName       : TEdit;
    btnPathSelect   : TButton;
    cbFileFormat    : TComboBox;

    tsLayout        : TTabSheet;
    gbSpatialRes    : TGroupBox;
    lUnit           : TLabel;
    lHoriz          : TLabel;
    lVert           : TLabel;
    cbUnit          : TComboBox;
    cbAspect        : TCheckBox;
    rsHoriz         : TmcmRealSpin;
    rsVert          : TmcmRealSpin;
    cbHoriz         : TComboBox;
    cbVert          : TComboBox;

    tsOrientation   : TTabSheet;
    gbOrientation   : TGroupBox;
    sb0degree       : TSpeedButton;
    sb90degree      : TSpeedButton;
    sb180degree     : TSpeedButton;
    sb270degree     : TSpeedButton;
    l0degree        : TLabel;
    l90degree       : TLabel;
    l180degree      : TLabel;
    l270degree      : TLabel;

    gbRotation      : TGroupBox;
    lDegree         : TLabel;
    cbRotation      : TComboBox;

    tsColor         : TTabSheet;
    gbColor         : TGroupBox;
    lColorModel     : TLabel;
    cbPixelType     : TComboBox;

    gbAOI           : TGroupBox;
    lLeft           : TLabel;
    rsLeft          : TmcmRealSpin;
    lTop            : TLabel;
    rsTop           : TmcmRealSpin;
    lRight          : TLabel;
    rsRight         : TmcmRealSpin;
    lBottom         : TLabel;
    rsBottom        : TmcmRealSpin;
    seRotation      : TmcmRealSpin;

    procedure FormCreate          (Sender : TObject);
    procedure FormDestroy         (Sender : TObject);
    procedure FormActivate        (Sender : TObject);

    procedure btnScanClick        (Sender : TObject);
    procedure btnApplyClick       (Sender : TObject);
    procedure btnCloseClick       (Sender : TObject);

    procedure PageControlChange   (Sender : TObject);

    procedure cbSourceChange      (Sender : TObject);
    procedure cbSourceDropDown    (Sender : TObject);
    procedure cbDriverSettingClick(Sender : TObject);

    procedure btnAddClick         (Sender : TObject);
    procedure btnDeleteClick      (Sender : TObject);

    procedure cbTransferChange    (Sender : TObject);
    procedure cbDisableAfterClick (Sender : TObject);
    procedure cbIndicatorsClick   (Sender : TObject);

    procedure btnPathSelectClick  (Sender : TObject);
    procedure cbFileFormatChange  (Sender : TObject);
    procedure eFileNameChange     (Sender : TObject);

    procedure cbPredefNameChange  (Sender : TObject);
    procedure cbUnitChange        (Sender : TObject);
    procedure rsHorizChange       (Sender : TObject);
    procedure rsVertChange        (Sender : TObject);
    procedure cbHorizChange       (Sender : TObject);
    procedure cbVertChange        (Sender : TObject);
    procedure cbAspectClick       (Sender : TObject);

    procedure seRotationChange    (Sender : TObject);
    procedure cbRotationChange    (Sender : TObject);
    procedure sbDegreeClick       (Sender : TObject);

  private
    { Private declarations }
    mcmTWAIN             : TmcmTWAIN;
    FDSMwasOpen          : boolean;
    FKeyStr              : string;
    FReg                 : TRegIniFile;
    FPage                : integer;
    FSource              : string;
    FNewUnit             : integer;
    FOldUnit             : integer;
    FDegree              : integer;
    FSelDevice           : string;
    FUseDriverSet        : boolean;
    FDisableAfterAcquire : boolean;
    FShowIndicator       : boolean;

    procedure StoreDefaults;
    procedure LoadDefaults;
    procedure LoadSourceDefineName;
    procedure StoreDefines        (Section    : string);
    procedure LoadDefines         (Section    : string);
    procedure EnablePage          (Control    : TWinControl;
                                   Value      : boolean);
    function  GetCBObjectValue    (ComboBox   : TComboBox) : integer;
    function  RemoveExt           (Value      : string) : string;
    function  ConvertUnitValue    (Value      : Double;
                                   Resolution : double) : double;
  public
    { Public declarations }
    procedure TWAINResolution;
    procedure TWAINNegotiation    (Sender     : TObject);
    procedure TWAINUserValues;
    property  SelectedDevice : string
      read    FSelDevice;
    property  UseDriverSetting : boolean
      read    FUseDriverSet;
    property  DisableAfterAcquire : boolean
      read    FDisableAfterAcquire;
    property  ShowIndicator : boolean
      read    FShowIndicator;
  end;

var DlgTwainPref : TDlgTwainPref;

implementation

{$R *.DFM}
{$IFOPT X-} {$DEFINE EXTENDED_SYNTAX} {$X+} {$ENDIF}

uses Twain, mcmTWAINContainer;


procedure TDlgTwainPref.FormCreate(Sender : TObject);
var OldCursor : TCursor;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  mcmTWAIN := TmcmTWAIN(Owner);
  try
    FSelDevice := '';
    FNewUnit := -1;
    FOldUnit := -1;
    FDegree  := 0;
    FUseDriverSet := False;

    btnScan.Enabled  := False;
    btnApply.Enabled := False;

    eFileName.Text := mcmTWAIN.Filename;

    FKeyStr := '\SOFTWARE\mcmTwain\';
    FReg := TRegIniFile.Create('');
    FReg.RootKey := HKEY_LOCAL_MACHINE;
    LoadDefaults;
    LoadSourceDefineName;

  finally
    Screen.Cursor := OldCursor;
  end;
end; { End TDlgTwainPref.FormCreate.                                           }


procedure TDlgTwainPref.FormDestroy(Sender : TObject);
begin
  FReg.Free;
end; { End TDlgTwainPref.FormDestroy.                                          }


procedure TDlgTwainPref.FormActivate(Sender : TObject);
var OldCursor : TCursor;
    i         : integer;
begin
  OnActivate := Nil; // Only run this event once.
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Update;
    Application.ProcessMessages;

    FDSMwasOpen := mcmTWAIN.IsDSMOpen;
    if Not(FDSMwasOpen)
    then mcmTWAIN.OpenSourceMgr;

    // Get available sources.
    if (cbSource.Items.Count = 0)
    then begin
         cbSourceDropDown(Self);
         i := cbSource.Items.IndexOf(FSource);
         cbSource.ItemIndex := i;
         FSource := '';
         cbSourceChange(Self);
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end; { End TDlgTwainPref.FormActivate.                                         }


procedure TDlgTwainPref.StoreDefaults;
begin
  if FReg.OpenKey(FKeyStr, True)
  then begin
       FReg.WriteInteger('Default', 'Page', FPage);
       FReg.WriteString('Default', 'Source', FSource);
       FReg.WriteString('Sources', FSource, cbPredefName.Items[cbPredefName.ItemIndex]);
       FReg.CloseKey;
  end;

  if FReg.OpenKey(FKeyStr + 'Defines\', False)
  then begin
       if (cbPredefName.Items.Count > 0)
       then FReg.WriteInteger('', 'Item', cbPredefName.ItemIndex);
       FReg.CloseKey;
  end;
end; { End TDlgTwainPref.StoreDefaults.                                        }


procedure TDlgTwainPref.LoadDefaults;
begin
  if FReg.OpenKey(FKeyStr + 'Defines\', False)
  then begin
       FReg.ReadSections(cbPredefName.Items);
       FReg.CloseKey;
  end;

  if FReg.OpenKey(FKeyStr, True)
  then begin
       FPage   := FReg.ReadInteger('Default', 'Page', 0);
       FSource := FReg.ReadString('Default', 'Source', '');
       FReg.CloseKey;
  end;

  cbSource.Text := FSource;
  PageControl.ActivePage := PageControl.Pages[FPage];
end; { End TDlgTwainPref.LoadDefaults.                                         }


procedure TDlgTwainPref.LoadSourceDefineName;
var PreStr : string;
begin
  if FReg.OpenKey(FKeyStr, True)
  then begin
       PreStr  := FReg.ReadString('Sources', FSource, '');
       if (Length(PreStr) > 0)
       then cbPredefName.ItemIndex := cbPredefName.Items.IndexOf(PreStr)
       else cbPredefName.ItemIndex := -1;
       FReg.CloseKey;
  end;
end; { End TDlgTwainPref.LoadSourceDefineName.                                 }


procedure TDlgTwainPref.StoreDefines(Section : string);
var i : integer;
begin
  if (Section <> '')
  then begin
       if FReg.OpenKey(FKeyStr + 'Defines\', True)
       then begin
            // Transfer.
            i := integer(cbTransfer.Items.Objects[cbTransfer.ItemIndex]);
            FReg.WriteInteger(Section, 'Xfer',   i);
            FReg.WriteBool(Section,    'DAA',    cbDisableAfter.Checked);
            FReg.WriteBool(Section,    'SSI',    cbIndicators.Checked);
            if (cbFileFormat.ItemIndex >= 0)
            then begin
                 i := integer(cbFileFormat.Items.Objects[cbFileFormat.ItemIndex]);
                 FReg.WriteInteger(Section, 'Format',   i);
            end;

            // Layout.
            if (cbUnit.ItemIndex >= 0)
            then begin
                 i := integer(cbUnit.Items.Objects[cbUnit.ItemIndex]);
                 FReg.WriteInteger(Section, 'Unit',   i);
            end;
            FReg.WriteString(Section,  'xRes',   FloatToStr(rsHoriz.Value));
            FReg.WriteString(Section,  'yRes',   FloatToStr(rsVert.Value));
            FReg.WriteBool(Section,    'Aspect', cbAspect.Checked);

            FReg.WriteString(Section,  'Left',   FloatToStr(rsLeft.Value));
            FReg.WriteString(Section,  'Top',    FloatToStr(rsTop.Value));
            FReg.WriteString(Section,  'Right',  FloatToStr(rsRight.Value));
            FReg.WriteString(Section,  'Bottom', FloatToStr(rsBottom.Value));

            // Orientation.
            FReg.WriteInteger(Section, 'Degree', FDegree);
            FReg.WriteInteger(Section, 'Rotate', Round(seRotation.Value));

            // Color.
            if (cbPixelType.ItemIndex >= 0)
            then begin
                 i := integer(cbPixelType.Items.Objects[cbPixelType.ItemIndex]);
                 FReg.WriteInteger(Section, 'PixelType', i);
            end;

            FReg.CloseKey;
       end;
  end;
end; { End TDlgTwainPref.StoreDefines.                                         }


procedure TDlgTwainPref.LoadDefines(Section : string);
var i, j : integer;
    rStr : string;
begin
  if (Section <> '')
  then begin
       if FReg.OpenKey(FKeyStr + 'Defines\', True)
       then begin
            // Transfer.
            j := integer(cbTransfer.Items.Objects[cbTransfer.ItemIndex]);
            i := FReg.ReadInteger(Section, 'Xfer', j);
            j := cbTransfer.Items.IndexOfObject(pointer(i));
            if (j >= 0)
            then cbTransfer.ItemIndex := j;
            cbDisableAfter.Checked := FReg.ReadBool(Section, 'DAA', True);
            cbIndicators.Checked   := FReg.ReadBool(Section, 'SSI', True);
            i := FReg.ReadInteger(Section, 'Format', -1);
            j := cbFileFormat.Items.IndexOfObject(pointer(i));
            if (j >= 0)
            then cbFileFormat.ItemIndex := j;
            cbTransferChange(Self);
            cbFileFormatChange(Self);

            // Layout.
            i :=  FReg.ReadInteger(Section, 'Unit',   i);
            j := cbUnit.Items.IndexOfObject(pointer(i));
            if (j >= 0)
            then cbUnit.ItemIndex := j;
            FNewUnit := GetCBObjectValue(cbUnit);
            FOldUnit := FNewUnit;
            rStr := FReg.ReadString(Section, 'xRes', FloatToStr(rsHoriz.Value));
            if (rStr <> '')
            then begin
                 if cbHoriz.Visible
                 then begin
                      i := cbHoriz.Items.IndexOf(rStr);
                      if (i >= 0)
                      then cbHoriz.ItemIndex := i;
                      rStr := cbHoriz.Items[cbHoriz.ItemIndex];
                 end;
                 rsHoriz.Value := StrToFloat(rStr);
            end;

            rStr := FReg.ReadString(Section, 'yRes', FloatToStr(rsVert.Value));
            if (rStr <> '')
            then begin
                 if cbVert.Visible
                 then begin
                      i := cbVert.Items.IndexOf(rStr);
                      if (i >= 0)
                      then cbVert.ItemIndex := i;
                      rStr := cbVert.Items[cbVert.ItemIndex];
                 end;
                 rsVert.Value := StrToFloat(rStr);
            end;

            cbAspect.Checked := FReg.ReadBool(Section, 'Aspect', True);

            rStr := FReg.ReadString(Section,  'Left',   FloatToStr(rsLeft.Value));
            if (rStr <> '')
            then rsLeft.Value := StrToFloat(rStr);
            rStr := FReg.ReadString(Section,  'Top',    FloatToStr(rsTop.Value));
            if (rStr <> '')
            then rsTop.Value := StrToFloat(rStr);
            rStr := FReg.ReadString(Section,  'Right',  FloatToStr(rsRight.Value));
            if (rStr <> '')
            then rsRight.Value := StrToFloat(rStr);
            rStr := FReg.ReadString(Section,  'Bottom', FloatToStr(rsBottom.Value));
            if (rStr <> '')
            then rsBottom.Value := StrToFloat(rStr);

            // Orientation.
            FDegree := FReg.ReadInteger(Section, 'Degree', FDegree);
            case FDegree of
            0   : sb0degree.Click;
            90  : sb90degree.Click;
            180 : sb180degree.Click;
            270 : sb270degree.Click;
            end;

            j := FReg.ReadInteger(Section, 'Rotate', Round(seRotation.Value));
            if cbRotation.Visible
            then begin
                 rStr := IntToStr(j);
                 i := cbRotation.Items.IndexOf(rStr);
                 if (i >= 0)
                 then cbRotation.ItemIndex := i;
                 rStr := cbRotation.Items[cbRotation.ItemIndex];
                 j := StrToInt(rStr);
            end;
            seRotation.Value := j;

            // Color.
            i := FReg.ReadInteger(Section, 'PixelType', -1);
            j := cbPixelType.Items.IndexOfObject(pointer(i));
            if (j >= 0)
            then cbPixelType.ItemIndex := j;

            FReg.CloseKey;
       end;
  end;
end; { End TDlgTwainPref.LoadDefines.                                          }


procedure TDlgTwainPref.EnablePage(Control : TWinControl; Value : boolean);

  procedure EnableGroupBox(GroupBox : TGroupBox);
  var j : integer;
  begin
    for j := 0 to (GroupBox.ControlCount - 1)
    do GroupBox.Controls[j].Enabled := Value;
  end; { End EnableGroupBox.                                                   }

var i, j : integer;
begin
  if (Control is TGroupBox)
  then EnableGroupBox(TGroupBox(Control))
  else begin
       for i := 0 to (Control.ControlCount - 1)
       do begin
          if (Control.Controls[i] is TGroupBox)
          then begin
               for j := 0 to ((Control.Controls[i] as TGroupBox).ControlCount - 1)
               do (Control.Controls[i] as TGroupBox).Controls[j].Enabled := Value;
          end;
          if (Control.Controls[i] is TControl)
          then (Control.Controls[i] as TControl).Enabled := Value;
       end;
  end;
end; { End TDlgTwainPref.EnablePage.                                           }


{------------------------------------------------------------------------------}
{ Basic routines.                                                              }
{------------------------------------------------------------------------------}

procedure TDlgTwainPref.btnScanClick(Sender : TObject);
begin
  ModalResult := mrOK;
  btnApply.Enabled := False;
  StoreDefaults;

  if Not(cbDriverSetting.Checked)
  then TWAINUserValues;

  FUseDriverSet := Not(cbDriverSetting.Checked);
  FDisableAfterAcquire := cbDisableAfter.Checked;
  FShowIndicator := cbIndicators.Checked;
  FSelDevice := cbSource.Text;

  // Always close source.
  mcmTWAIN.CloseSource;

  ModalResult := mrOK;
end; { End TDlgTwainPref.btnScanClick.                                         }


procedure TDlgTwainPref.btnApplyClick(Sender : TObject);
begin
  StoreDefaults;
  btnAddClick(Self);
  btnApply.Enabled := False;
end; { End TDlgTwainPref.btnApplyClick.                                        }


procedure TDlgTwainPref.btnCloseClick(Sender : TObject);
begin
  if btnApply.Enabled
  then begin
       // Ask if changes should be saved ?

  end;
  // Always close source.
  mcmTWAIN.CloseSource;

  // Close source manager if "I" opened it.
  if Not(FDSMwasOpen)
  then mcmTWAIN.CloseSourceMgr;
end; { End TDlgTwainPref.btnCancelClick.                                       }


function TDlgTwainPref.GetCBObjectValue(ComboBox : TComboBox) : integer;
begin
  Result := integer(ComboBox.Items.Objects[ComboBox.ItemIndex]);
end; { End TDlgTwainPref.GetCBObjectValue.                                     }


procedure TDlgTwainPref.PageControlChange(Sender : TObject);
begin
  FPage := PageControl.ActivePage.PageIndex;
  case FPage of
  0 : begin // Default Source.
        cbSourceDropDown(Self);
      end;
  end;
end; { End TDlgTwainPref.PageControlChange.                                    }


{------------------------------------------------------------------------------}
{ Source Page.                                                                 }
{------------------------------------------------------------------------------}

procedure TDlgTwainPref.cbSourceChange(Sender : TObject);
var OldCursor : TCursor;
begin
  OldCursor := Screen.Cursor;
  Update;
  try
    Screen.Cursor := crHourGlass;
    if (cbSource.ItemIndex >= 0)
    then begin
         if (FSource <> cbSource.Items[cbSource.ItemIndex])
         then begin
              mcmTWAIN.CloseSource;
              FSource := cbSource.Items[cbSource.ItemIndex];
              LoadSourceDefineName;

              btnApply.Enabled := True;
              if mcmTWAIN.OpenSource(FSource)
              then begin
                   with mcmTWAIN.SourceInfo
                   do begin
                      lManufactValue.Caption := Manufacturer;
                      lProdVerValue.Caption  := Version;
                      lProtVerValue.Caption  := Protocol;
                   end;
              end;
         end;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end; { End TDlgTwainPref.cbSourceChange.                                       }


procedure TDlgTwainPref.cbSourceDropDown(Sender : TObject);
var i : integer;
begin
  if (cbSource.Items.Count = 0)
  then begin
       mcmTWAIN.GetSourceList(cbSource.Items);
       if (cbSource.Items.Count > 0)
       then begin
            i := cbSource.Items.IndexOf(FSource);
            cbSource.ItemIndex := i;
            btnScan.Enabled  := True;
       end;
  end;
end; { End TDlgTwainPref.cbSourceDropDown.                                     }


procedure TDlgTwainPref.cbDriverSettingClick(Sender : TObject);
var Drvs : boolean;
begin
  Drvs := Not(cbDriverSetting.Checked);
  // tsTransfer.TabVisible    := Drvs;
  tsLayout.TabVisible      := Drvs;
  tsColor.TabVisible       := Drvs;
  tsOrientation.TabVisible := Drvs;
  cbPredefName.Enabled     := Drvs;
  btnDelete.Enabled        := Drvs and (cbPredefName.Items.Count > 0);
  FUseDriverSet            := Drvs;
end; { End TDlgTwainPref.cbDriverSettingClick.                                 }


procedure TDlgTwainPref.cbPredefNameChange(Sender : TObject);
begin
  if (cbPredefName.Text <> '')
  then begin
       if FReg.OpenKey(FKeyStr + 'Defines\', False)
       then begin
            if FReg.KeyExists(cbPredefName.Text)
            then begin
                 btnAdd.Enabled    := False;
                 btnDelete.Enabled := True;
                 LoadDefines(cbPredefName.Text);
            end
            else begin
                 btnAdd.Enabled    := True;
                 btnDelete.Enabled := False;
            end;
            FReg.CloseKey;
       end
       else begin
            btnAdd.Enabled := True;
            btnDelete.Enabled := False;
       end;
  end;
end; { End TDlgTwainPref.cbPredefName.                                         }


procedure TDlgTwainPref.btnAddClick(Sender : TObject);
var Section : string;
    i       : integer;
begin
  if (cbPredefName.Text <> '')
  then begin
       i := cbPredefName.Items.IndexOf(cbPredefName.Text);
       if (i = -1)
       then begin
            cbPredefName.Items.Add(cbPredefName.Text);
            Section := cbPredefName.Text;
       end
       else Section := cbPredefName.Items[i];
       StoreDefines(Section);
  end;
end; { End TDlgTwainPref.btnAddClick.                                          }


procedure TDlgTwainPref.btnDeleteClick(Sender : TObject);
var SectionName : string;
begin
  SectionName := cbPredefName.Items[cbPredefName.ItemIndex];
  if (SectionName <> '')
  then begin
       cbPredefName.Text := '';
       cbPredefName.Items.Delete(cbPredefName.ItemIndex);
       TRegistry(FReg).DeleteKey(FKeyStr + 'Defines\' + SectionName);
  end;
  btnDelete.Enabled := (cbPredefName.Items.Count > 0);
end; { End TDlgTwainPref.btnDeleteClick.                                       }


{------------------------------------------------------------------------------}
{ Transfer Page.                                                               }
{------------------------------------------------------------------------------}

procedure TDlgTwainPref.cbTransferChange(Sender : TObject);
var EnFile : boolean;
begin
  if (cbTransfer.ItemIndex < 0)
  then cbTransfer.ItemIndex := 0;

  with mcmTWAIN
  do begin
     // The Transfer mechanism is assgined directly to TmcmTWAIN component.
     // You do therefore not need to negotiate this capability.
     XferMech := TtwnXferType(GetCBObjectValue(cbTransfer));
     EnFile := (XferMech = TWFX_Files);
     lFileName.Enabled     := EnFile;
     eFileName.Enabled     := EnFile;
     lFileFormat.Enabled   := EnFile;
     cbFileFormat.Enabled  := EnFile;
     btnPathSelect.Enabled := EnFile;
  end;
end; { End TDlgTwainPref.cbTransferChange.                                     }


procedure TDlgTwainPref.cbDisableAfterClick(Sender : TObject);
begin
  // Disable After Acquire is assgined directly to TmcmTWAIN component.
  // You do therefore not need to negotiate this capability.
  with mcmTWAIN
  do DisableAfterAcquire := cbDisableAfter.Checked;
end; { End TDlgTwainPref.cbDisableAfterClick.                                  }


procedure TDlgTwainPref.cbIndicatorsClick(Sender : TObject);
begin
  // Show Indicators is assgined directly to TmcmTWAIN component.
  // You do therefore not need to negotiate this capability.
  with mcmTWAIN
  do ShowIndicators := cbIndicators.Checked;
end; { End TDlgTwainPref.cbIndicatorsClick.                                    }


function TDlgTwainPref.RemoveExt(Value : string) : string;
var TempName : String;
begin
  TempName := lowercase(Value);
  if (pos('.tif', TempName) = (Length(TempName) - 3))
  then TempName := Copy(Value, 1, Length(TempName) - 4)
  else if (pos('.bmp', TempName) = (Length(TempName) - 3))
       then TempName := Copy(Value, 1, Length(TempName) - 4)
       else if (pos('.jpg', TempName) = (Length(TempName) - 3))
            then TempName := Copy(Value, 1, Length(TempName) - 4)
            else TempName := Value;
  Result := TempName;
end; { End TDlgTwainPref.RemoveExt.                                            }


procedure TDlgTwainPref.eFileNameChange(Sender : TObject);
var i        : integer;
begin
  // The file name is assgined directly to TmcmTWAIN component.
  // You do therefore not need to negotiate this capability.
  mcmTWAIN.FileName := eFileName.Text;
  i := cbFileFormat.ItemIndex;
  if (i >= 0)
  then begin
       with cbFileFormat.Items
       do begin
          if (TtwnFileFmt(Objects[i]) <> mcmTWAIN.FileFormat)
          then begin
               i := 0;
               while (i < Count) and
                     (TtwnFileFmt(Objects[i]) <> mcmTWAIN.FileFormat)
               do inc(i);
               if (i < Count)
               then cbFileFormat.ItemIndex := i;
          end;
       end;
  end;
end; { End TDlgTwainPref.eFileNameChange.                                      }


procedure TDlgTwainPref.btnPathSelectClick(Sender : TObject);
var i : integer;
begin
  SaveDialogTwn.Filter := '';
  for i := 0 to (cbFileFormat.Items.Count - 1)
  do begin
     with SaveDialogTwn
     do case TtwnFileFmt(cbFileFormat.Items.Objects[i]) of
        TWFF_TIFF      : Filter := Filter + 'TIFF (*.tif)|*.tif|';
        TWFF_BMP       : Filter := Filter + 'Bitmap (*.bmp)|*.bmp|';
        TWFF_JFIF      : Filter := Filter + 'JPEG (*.jpg)|*.jpg|';
        TWFF_FPX       : Filter := Filter + 'FPX (*.fpx)|*.fpx|';
        TWFF_TIFFMULTI : Filter := Filter + 'TIFF-Multi (*.tif)|*.tif|';
        TWFF_PNG       : Filter := Filter + 'PNG (*.png)|*.png|';
        // TWFF_SPIFF    :
        // TWFF_EXIF     :
        end;
     if (i = cbFileFormat.ItemIndex)
     then SaveDialogTwn.FilterIndex := i + 1;
  end;
  SaveDialogTwn.Filename := RemoveExt(eFileName.Text);
  SaveDialogTwn.options := [ofHideReadOnly,ofPathMustExist];
  if SaveDialogTwn.Execute
  then begin
       eFileName.Text := RemoveExt(SaveDialogTwn.FileName);
       cbFileFormat.ItemIndex := SaveDialogTwn.FilterIndex - 1;
       cbFileFormatChange(Sender);
  end;
end; { End TDlgTwainPref.btnPathSelectClick.                                   }


procedure TDlgTwainPref.cbFileFormatChange(Sender : TObject);
var i : integer;
begin
  // The file extension is assgined directly to TmcmTWAIN component.
  // You do therefore not need to negotiate this capability.
  i := cbFileFormat.ItemIndex;
  if (i >= 0)
  then begin
       mcmTWAIN.FileFormat := TtwnFileFmt(cbFileFormat.Items.Objects[i]);
       eFileName.Text := mcmTWAIN.Filename;
  end;
end; { End TDlgTwainPref.cbFileFormatChange.                                   }


{------------------------------------------------------------------------------}
{ Resolution Page.                                                             }
{------------------------------------------------------------------------------}

function TDlgTwainPref.ConvertUnitValue(Value      : Double;
                                        Resolution : double) : double;
var Pixel : Extended;
    Inch  : Extended;
    AUnit : integer;
begin
  // Converts a (AOI) position value from one unit to another.
  if (FOldUnit <> FNewUnit) and (Resolution > 0)
  then begin
       // Convert Value to pixels.
       if (FOldUnit <> TWUN_PIXELS)
       then begin
            Pixel := Value * Resolution;
            AUnit := FOldUnit;
       end
       else begin
            Pixel := Value;
            AUnit := FNewUnit;
       end;

       // Convert Resolution to Inch resolution.
       case AUnit of
       TWUN_INCHES      : Inch  := Resolution;
       TWUN_CENTIMETERS : Inch  := Resolution * 2.54;
       TWUN_PICAS       : Inch  := Resolution * 6.0;
       TWUN_POINTS      : Inch  := Resolution * 72.0;
       TWUN_TWIPS       : Inch  := Resolution * 1440.0;
       TWUN_PIXELS      : Inch  := Resolution;
       else Inch  := 1.0;
       end;

       // Calculate new value.
       case FNewUnit of
       TWUN_INCHES      : Result := Pixel / Inch;
       TWUN_CENTIMETERS : Result := Pixel / (Inch / 2.54);
       TWUN_PICAS       : Result := Pixel / (Inch / 6.0);
       TWUN_POINTS      : Result := Pixel / (Inch / 72.0);
       TWUN_TWIPS       : Result := Pixel / (Inch / 1440.0);
       TWUN_PIXELS      : Result := Round(Pixel);
       else Result := Value;
       end;
  end
  else Result := Value;
end; { End TDlgTwainPref.ConvertUnitValue.                                     }


procedure TDlgTwainPref.cbUnitChange(Sender : TObject);
var OldLeft   : double;
    OldTop    : double;
    OldRight  : double;
    OldBottom : double;
    HorizRes  : double;
    VertRes   : double;
    NotPixel  : boolean;
begin
  // Save AOI selection.
  OldLeft   := rsLeft.Value;
  OldTop    := rsTop.Value;
  OldRight  := rsRight.Value;
  OldBottom := rsBottom.Value;
  HorizRes  := rsHoriz.Value;
  VertRes   := rsVert.Value;

  NotPixel := (GetCBObjectValue(cbUnit) <> TWUN_PIXELS);
  lHoriz.Enabled   := NotPixel;
  lVert.Enabled    := NotPixel;
  rsHoriz.Enabled  := NotPixel;
  rsVert.Enabled   := NotPixel;
  cbHoriz.Enabled  := NotPixel;
  cbVert.Enabled   := NotPixel;
  cbAspect.Enabled := NotPixel;

  if (GetCBObjectValue(cbUnit) <> TWUN_PIXELS)
  then begin
       rsLeft.Decimals   := 2;
       rsTop.Decimals    := 2;
       rsRight.Decimals  := 2;
       rsBottom.Decimals := 2;
  end
  else begin
       rsLeft.Decimals   := 0;
       rsTop.Decimals    := 0;
       rsRight.Decimals  := 0;
       rsBottom.Decimals := 0;
  end;

  // Set selected unit in current source.
  with mcmTWAIN
  do begin
     if (Containers.Items[ICAP_UNITS] <> Nil)
     then begin
          FNewUnit := GetCBObjectValue(cbUnit);
          Containers.Items[ICAP_UNITS].CurrentValue := FNewUnit;
          if (SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_UNITS]) = TWRC_SUCCESS)
          then begin
               // Get Resolution information corresponding to the selected unit.
               TWAINResolution;

               if (FOldUnit = TWUN_PIXELS)
               then begin
                    HorizRes  := rsHoriz.Value;
                    VertRes   := rsVert.Value;
               end;

               // Calculate AOI based on new unit selection.
               rsLeft.Value   := ConvertUnitValue(OldLeft, HorizRes);
               rsTop.Value    := ConvertUnitValue(OldTop, VertRes);
               rsRight.Value  := ConvertUnitValue(OldRight, HorizRes);
               rsBottom.Value := ConvertUnitValue(OldBottom, VertRes);
               FOldUnit := Containers.Items[ICAP_UNITS].CurrentValue;
               btnApply.Enabled := True;
          end;
     end;
  end;
end; { End TDlgTwainPref.cbUnitChange.                                         }


procedure TDlgTwainPref.rsHorizChange(Sender : TObject);
begin
  if Assigned(rsHoriz)
  then if cbAspect.Checked
       then if Assigned(rsVert)
            then rsVert.Value := rsHoriz.Value;
  btnApply.Enabled := True;
end; { End TDlgTwainPref.rsHorizChange.                                        }


procedure TDlgTwainPref.rsVertChange(Sender : TObject);
begin
  btnApply.Enabled := True;
end; { End TDlgTwainPref.rsVertChange.                                         }


procedure TDlgTwainPref.cbHorizChange(Sender : TObject);
var Value : double;
begin
  try
    Value := StrToFloat(cbHoriz.Items[cbHoriz.ItemIndex]);
    rsHoriz.Value := Value;
    if cbAspect.Checked
    then begin
         if Assigned(rsVert)
         then rsVert.Value := rsHoriz.Value;
         if Assigned(cbVert)
         then cbVert.ItemIndex := cbVert.Items.IndexOf(cbHoriz.Items[cbHoriz.ItemIndex]);
    end;
    btnApply.Enabled := True;
  except
  end;
end; { End TDlgTwainPref.cbHorizChange.                                        }


procedure TDlgTwainPref.cbVertChange(Sender : TObject);
var Value : double;
begin
  try
    Value := StrToFloat(cbVert.Items[cbVert.ItemIndex]);
    rsVert.Value := Value;
    btnApply.Enabled := True;
  except
  end;
end; { End TDlgTwainPref.cbVertChange.                                         }


procedure TDlgTwainPref.cbAspectClick(Sender : TObject);
begin
  if cbAspect.Checked
  then begin
       lVert.Enabled  := False;
       rsVert.Enabled := False;
       cbVert.Enabled := False;
  end
  else begin
       lVert.Enabled  := True;
       rsVert.Enabled := True;
       cbVert.Enabled := True;
  end;
  btnApply.Enabled := True;
end; { End TDlgTwainPref.cbAspectClick.                                        }


{------------------------------------------------------------------------------}
{ Orientation Page.                                                            }
{------------------------------------------------------------------------------}

procedure TDlgTwainPref.sbDegreeClick(Sender : TObject);
begin
  FDegree := (Sender as TSpeedButton).Tag;
  (Sender as TSpeedButton).Down := True;
end; { End TDlgTwainPref.sbDegreeClick.                                        }


procedure TDlgTwainPref.seRotationChange(Sender : TObject);
begin
  btnApply.Enabled := True;
end; { End TDlgTwainPref.seRotationChange.                                     }


procedure TDlgTwainPref.cbRotationChange(Sender : TObject);
var Value : integer;
begin
  try
    Value := StrToInt(cbRotation.Items[cbRotation.ItemIndex]);
    seRotation.Value := Value;
    btnApply.Enabled := True;
  except
  end;
end; { End TDlgTwainPref.cbRotationChange.                                     }


{------------------------------------------------------------------------------}
{ Color Page.                                                                  }
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{ TmcmTWAIN interface functions.                                               }
{------------------------------------------------------------------------------}

procedure TDlgTwainPref.TWAINResolution;
var Container : TTwnContainer;
    i         : integer;
begin
  with mcmTWAIN
  do begin
     if IsCapSupported(ICAP_PHYSICALWIDTH)
     then begin
          rsLeft.MaxValue  := PhysicalHeight;
          rsRight.MaxValue := rsLeft.MaxValue;
          if (rsRight.Value = 0) or (rsRight.Value > rsRight.MaxValue)
          then rsRight.Value := rsRight.MaxValue;
          rsLeft.Hint      := 'Min .. Max = ' +
                              FloatToStrF(rsLeft.MinValue, ffFixed, 7, 2) + ' .. ' +
                              FloatToStrF(rsLeft.MaxValue, ffFixed, 7, 2);
          rsRight.Hint     := rsLeft.Hint;
     end;

     if IsCapSupported(ICAP_PHYSICALHEIGHT)
     then begin
          rsTop.MaxValue    := PhysicalWidth;
          rsBottom.MaxValue := rsTop.MaxValue;
          if (rsBottom.Value = 0) or (rsBottom.Value > rsBottom.MaxValue)
          then rsBottom.Value := rsBottom.MaxValue;
          rsTop.Hint       := 'Min .. Max = ' +
                              FloatToStrF(rsTop.MinValue, ffFixed, 7, 2) + ' .. ' +
                              FloatToStrF(rsTop.MaxValue, ffFixed, 7, 2);
          rsBottom.Hint    := rsTop.Hint;
     end;

     if (GetCBObjectValue(cbUnit) <> TWUN_PIXELS)
     then begin
          if IsCapSupported(ICAP_XRESOLUTION)
          then begin
               rsHoriz.Enabled := True;
               cbHoriz.Enabled := True;
               if (GetCapabilityMsg(ICAP_XRESOLUTION, MSG_GET, Container) = TWRC_SUCCESS)
               then begin
                    case Container.ContainerType of
                    TWON_ENUMERATION,
                    TWON_ONEVALUE    : begin
                                         rsHoriz.Visible   := False;
                                         cbHoriz.Visible   := True;
                                         cbHoriz.Clear;
                                         for i := 0 to (Container.NumItems - 1)
                                         do begin
                                            with cbHoriz
                                            do Items.Add(IntToStr(Container.Items[i]));
                                            if (Container.Items[i] = Container.CurrentValue)
                                            then begin
                                                 cbHoriz.ItemIndex := i;
                                                 rsHoriz.Value := Container.CurrentValue;
                                            end;
                                         end;
                                       end;
                    TWON_RANGE       : begin
                                         cbHoriz.Visible   := False;
                                         rsHoriz.Visible   := True;
                                         rsHoriz.MinValue  := Container.MinValue;
                                         rsHoriz.MaxValue  := Container.MaxValue;
                                         rsHoriz.Increment := Container.StepValue;
                                         rsHoriz.Value     := Container.CurrentValue;
                                         rsHoriz.Hint      := 'Min .. Max = ' +
                                                              FloatToStrF(rsHoriz.MinValue, ffFixed, 7, 2) + ' .. ' +
                                                              FloatToStrF(rsHoriz.MaxValue, ffFixed, 7, 2);
                                       end;
                    end;
               end;
               Container := Nil;
          end
          else begin
               rsHoriz.Enabled := False;
               cbHoriz.Enabled := False;
          end;

          if IsCapSupported(ICAP_YRESOLUTION)
          then begin
               rsVert.Enabled := True;
               cbVert.Enabled := True;
               if (GetCapabilityMsg(ICAP_XRESOLUTION, MSG_GET, Container) = TWRC_SUCCESS)
               then begin
                    case Container.ContainerType of
                    TWON_ENUMERATION,
                    TWON_ONEVALUE    : begin
                                         rsVert.Visible   := False;
                                         cbVert.Visible   := True;
                                         cbVert.Clear;
                                         for i := 0 to (Container.NumItems - 1)
                                         do begin
                                            with cbVert
                                            do Items.Add(IntToStr(Container.Items[i]));
                                            if (Container.Items[i] = Container.CurrentValue)
                                            then begin
                                                 cbVert.ItemIndex := i;
                                                 rsVert.Value := Container.CurrentValue;
                                            end;
                                         end;
                                       end;
                    TWON_RANGE       : begin
                                         cbVert.Visible   := False;
                                         rsVert.Visible   := True;
                                         rsVert.MinValue  := Container.MinValue;
                                         rsVert.MaxValue  := Container.MaxValue;
                                         rsVert.Increment := Container.StepValue;
                                         rsVert.Value     := Container.CurrentValue;
                                         rsVert.Hint      := 'Min .. Max = ' +
                                                              FloatToStrF(rsVert.MinValue, ffFixed, 7, 2) + ' .. ' +
                                                              FloatToStrF(rsVert.MaxValue, ffFixed, 7, 2);
                                       end;
                    end;
               end;
               Container := Nil;
          end
          else begin
               rsVert.Enabled := False;
               cbVert.Enabled := False;
          end;
     end;
  end;
end; { End TDlgTwainPref.TWAINResolution.                                      }


procedure TDlgTwainPref.TWAINNegotiation(Sender : TObject);
var i         : integer;
    Container : TtwnContainer;
begin
  with mcmTWAIN
  do begin
     //-------------------------------------------------------------------------
     // Get supported units.
     //-------------------------------------------------------------------------
     if IsCapSupported(ICAP_UNITS)
     then begin
          tsLayout.Enabled := True;
          cbUnit.Clear;
          if (GetCapabilityMsg(ICAP_UNITS, MSG_GET, Container) = TWRC_SUCCESS)
          then begin
               cbUnit.Enabled := (Container.NumItems > 0);
               for i := 0 to (Container.NumItems - 1)
               do begin
                  case Containers.Items[ICAP_UNITS].Items[i] of
                  TWUN_INCHES      : cbUnit.Items.AddObject('Inch',       pointer(TWUN_INCHES));
                  TWUN_CENTIMETERS : cbUnit.Items.AddObject('Centimeter', pointer(TWUN_CENTIMETERS));
                  TWUN_PICAS       : cbUnit.Items.AddObject('Picas',      pointer(TWUN_PICAS));
                  TWUN_POINTS      : cbUnit.Items.AddObject('Points',     pointer(TWUN_POINTS));
                  TWUN_TWIPS       : cbUnit.Items.AddObject('Twips',      pointer(TWUN_TWIPS));
                  TWUN_PIXELS      : cbUnit.Items.AddObject('Pixel',      pointer(TWUN_PIXELS));
                  end;
                  if (Container.Items[i] = Container.CurrentValue)
                  then begin
                       FOldUnit := Container.CurrentValue;
                       FNewUnit := FOldUnit;
                       cbUnit.ItemIndex := i;
                  end;
               end;
          end;
          Container := Nil;
     end
     else tsLayout.Enabled := False;

     //-------------------------------------------------------------------------
     // Get resolution information.
     //-------------------------------------------------------------------------
     TWAINResolution;

     //-------------------------------------------------------------------------
     // Get scaling information.
     //-------------------------------------------------------------------------
     if IsCapSupported(ICAP_XSCALING)
     then begin
          Container := Nil;
     end;

     if IsCapSupported(ICAP_YSCALING)
     then begin
          Container := Nil;
     end;

     //-------------------------------------------------------------------------
     // Get transfer mechanism's.
     //-------------------------------------------------------------------------
     if IsCapSupported(ICAP_XFERMECH)
     then begin
          cbTransfer.Clear;
          if (GetCapabilityMsg(ICAP_XFERMECH, MSG_GET, Container) = TWRC_SUCCESS)
          then begin
               cbTransfer.Enabled := (Container.NumItems > 0);
               for i := 0 to (Container.NumItems - 1)
               do begin
                  case Container.Items[i] of
                  TWSX_NATIVE : cbTransfer.Items.AddObject('Native', pointer(TWSX_NATIVE));
                  TWSX_FILE   : cbTransfer.Items.AddObject('File',   pointer(TWSX_FILE));
                  TWSX_MEMORY : cbTransfer.Items.AddObject('Memory', pointer(TWSX_MEMORY));
                  end;
                  if (Container.Items[i] = Container.CurrentValue)
                  then begin
                       cbTransfer.ItemIndex := i;
                       cbTransferChange(Self);
                  end;
               end;
          end;
          Container := Nil;
     end;

     //-------------------------------------------------------------------------
     // Get supported file formats.
     //-------------------------------------------------------------------------
     cbFileFormat.Clear;
     if IsCapSupported(ICAP_IMAGEFILEFORMAT)
     then begin
          if (GetCapabilityMsg(ICAP_IMAGEFILEFORMAT, MSG_GET, Container) = TWRC_SUCCESS)
          then begin
               cbFileFormat.Enabled := (Container.NumItems > 0) and
                                       (XferMech = TWFX_Files);
               for i := 0 to (Container.NumItems - 1)
               do begin
                  if (TtwnFileFmt(Container.Items[i]) in FileFormats)
                  then begin
                       case TtwnFileFmt(Container.Items[i]) of
                       // Tag Image File Format.
                       TWFF_TIFF      : cbFileFormat.Items.AddObject('TIFF', pointer(TWFF_TIFF));
                       // Macintosh PICT format.
                       // TWFF_PICT     : cbFileFormat.Items.AddObject('Macintosh PICT', pointer(TWFF_PICT)); }
                       // Windows format.
                       TWFF_BMP       : cbFileFormat.Items.AddObject('BMP', pointer(TWFF_BMP));
                       // X-Windows Bitmap.
                       // TWFF_XBM      : cbFileFormat.Items.AddObject('X-Windows BMP', pointer(TWFF_XBM)); }
                       // JPEG File Format.
                       TWFF_JFIF      : cbFileFormat.Items.AddObject('JPEG', pointer(TWFF_JFIF));
                       TWFF_FPX       : cbFileFormat.Items.AddObject('FPX', pointer(TWFF_FPX));
                       TWFF_TIFFMULTI : cbFileFormat.Items.AddObject('TIFF-Multi page', pointer(TWFF_TIFFMULTI));
                       TWFF_PNG       : cbFileFormat.Items.AddObject('PNG', pointer(TWFF_PNG));
                       // TWFF_SPIFF  :
                       // TWFF_EXIF   :
                       end;
                       if (Container.Items[i] = Container.CurrentValue)
                       then cbFileFormat.ItemIndex := i;
                  end;
               end;
          end;
          Container := Nil;
     end
     else cbFileFormat.Enabled := False;

     // Check if file formats (Src vs. App) coinsided. If not remove File
     // transfer.
     if (cbFileFormat.Items.Count = 0)
     then begin
          i := cbTransfer.Items.IndexOf('File');
          if (i >= 0)
          then begin
               cbTransfer.Items.Delete(i);
               cbTransferChange(Sender);
          end;
     end;
     cbFileFormatChange(Sender);

     // If Transfer item count is "0", App and Source cannot transfer an image.
     if (cbTransfer.Items.Count = 0)
     then btnScan.Enabled := False;

     //-------------------------------------------------------------------------
     // Get current orientation.
     //-------------------------------------------------------------------------
     EnablePage(gbOrientation, False);
     FDegree := 0;
     if IsCapSupported(ICAP_ORIENTATION)
     then begin
          if (GetCapabilityMsg(ICAP_ORIENTATION, MSG_GET, Container) = TWRC_SUCCESS)
          then begin
               case Container.CurrentValue of
               TWOR_ROT0   : sb0degree.Click;
               TWOR_ROT90  : sb90degree.Click;
               TWOR_ROT180 : sb180degree.Click;
               TWOR_ROT270 : sb270degree.Click;
               end;
               if (Container.ContainerType = TWON_ENUMERATION)
               then begin
                    for i := 0 to (Container.NumItems - 1)
                    do begin
                       case Container.Items[i] of
                       TWOR_ROT0   : begin
                                       sb0degree.Enabled := True;
                                       l0degree.Enabled  := True;
                                     end;
                       TWOR_ROT90  : begin
                                       sb90degree.Enabled := True;
                                       l90degree.Enabled  := True;
                                     end;
                       TWOR_ROT180 : begin
                                       sb180degree.Enabled := True;
                                       l180degree.Enabled  := True;
                                     end;
                       TWOR_ROT270 : begin
                                       sb270degree.Enabled := True;
                                       l270degree.Enabled  := True;
                                     end;
                       end;
                    end;
               end
               else EnablePage(gbOrientation, True);
          end;
          Container := Nil;
     end;

     //-------------------------------------------------------------------------
     // Get rotation.
     //-------------------------------------------------------------------------
     if IsCapSupported(ICAP_ROTATION)
     then begin
          EnablePage(gbRotation, True);
          if (GetCapabilityMsg(ICAP_ROTATION, MSG_GET, Container) = TWRC_SUCCESS)
          then begin
               seRotation.Value    := Container.CurrentValue;
               case Container.ContainerType of
               TWON_ENUMERATION : begin
                                    cbRotation.Clear;
                                    seRotation.Visible := False;
                                    cbRotation.Visible := True;
                                    for i := 0 to (Container.NumItems - 1)
                                    do begin
                                       with cbRotation
                                       do Items.Add(IntToStr(Container.Items[i]));
                                    end;
                                  end;
               TWON_RANGE       : begin
                                    cbRotation.Visible := False;
                                    seRotation.Visible := True;
                                    seRotation.MaxValue := Container.MaxValue;
                                    seRotation.MinValue := Container.MinValue;
                                  end;
               TWON_ONEVALUE    : begin
                                    cbRotation.Visible := False;
                                    seRotation.Visible := True;
                                    seRotation.MaxValue := 359;
                                    seRotation.MinValue := 0;
                                  end;
               end;
               seRotation.Hint := 'Min .. Max = ' +
                                   FloatToStrF(seRotation.MinValue, ffFixed, 7, 2) + ' .. ' +
                                   FloatToStrF(seRotation.MaxValue, ffFixed, 7, 2);
          end;
          Container := Nil;
     end
     else EnablePage(gbRotation, False);

     //-------------------------------------------------------------------------
     // Get Color information.
     //-------------------------------------------------------------------------
     if IsCapSupported(ICAP_PIXELTYPE)
     then begin
          EnablePage(gbColor, True);
          cbPixelType.Clear;
          if (GetCapabilityMsg(ICAP_PIXELTYPE, MSG_GET, Container) = TWRC_SUCCESS)
          then begin
               for i := (Container.NumItems - 1) downto 0
               do begin
                  if True// (TmcmPixelFmt(Container.Items[i]) in PixelFormats)
                  then begin
                       case Container.Items[i] of
                       TWPT_BW      : cbPixelType.Items.AddObject('Black & White', pointer(TWPT_BW));
                       TWPT_GRAY    : cbPixelType.Items.AddObject('Gray', pointer(TWPT_GRAY));
                       TWPT_RGB     : cbPixelType.Items.AddObject('RGB True color', pointer(TWPT_RGB));
                       TWPT_PALETTE : cbPixelType.Items.AddObject('Color', pointer(TWPT_PALETTE));

                       // The following pixel types have been excluded, since
                       // we haven't added suport for these formats.
                       {
                        TWPT_CMY     : cbPixelType.Items.AddObject('CMY', pointer(TWPT_CMY));
                        TWPT_CMYK    : cbPixelType.Items.AddObject('CMYK', pointer(TWPT_CMYK));
                        TWPT_YUV     : cbPixelType.Items.AddObject('YUV', pointer(TWPT_YUV));
                        TWPT_YUVK    : cbPixelType.Items.AddObject('YUVK', pointer(TWPT_YUVK));
                        TWPT_CIEXYZ  : cbPixelType.Items.AddObject('CIE', pointer(TWPT_CIEXYZ));
                       }
                       // Instead these items are deleted from the Container.
                       // When returning this container to the source, the
                       // source will confine the user choices to the items in
                       // the container.
                       TWPT_CMY     : Container.DeleteItem(TWPT_CMY);
                       TWPT_CMYK    : Container.DeleteItem(TWPT_CMYK);
                       TWPT_YUV     : Container.DeleteItem(TWPT_YUV);
                       TWPT_YUVK    : Container.DeleteItem(TWPT_YUVK);
                       TWPT_CIEXYZ  : Container.DeleteItem(TWPT_CIEXYZ);
                       end;
                  end;
                  if (i < Container.NumItems)
                  then if (Container.Items[i] = Container.CurrentValue)
                       then cbPixelType.ItemIndex := i;
               end;
          end;
          Container := Nil;
     end;
  end;

  // If the selected source is linked to a definition, load that.
  LoadDefines(cbPredefName.Text);
end; { End TDlgTwainPref.TWAINNegotiation.                                     }


procedure TDlgTwainPref.TWAINUserValues;
var ImageLayout : TImageLayout;
begin
  with mcmTWAIN
  do begin
     //-------------------------------------------------------------------------
     // Set Layout information.
     //-------------------------------------------------------------------------
     if (Containers.Items[ICAP_UNITS] <> Nil)
     then begin
          Containers.Items[ICAP_UNITS].CurrentValue := GetCBObjectValue(cbUnit);
          if (SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_UNITS]) = TWRC_SUCCESS)
          then begin
               // If Unit <> TWUN_PIXELS then set the Resolutions.
               // Otherwise resolutions are by default = 1 (Dimensionless).
               if (Containers.Items[ICAP_UNITS].CurrentValue <> TWUN_PIXELS)
               then begin
                    if (Containers.Items[ICAP_XRESOLUTION] <> Nil)
                    then begin
                         Containers.Items[ICAP_XRESOLUTION].CurrentValue := rsHoriz.Value;
                         if (SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_XRESOLUTION]) <> TWRC_SUCCESS)
                         then ;
                    end;
                    if (Containers.Items[ICAP_YRESOLUTION] <> Nil)
                    then begin
                         Containers.Items[ICAP_YRESOLUTION].CurrentValue := rsVert.Value;
                         if (SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_YRESOLUTION]) <> TWRC_SUCCESS)
                         then ;
                    end;
               end;

               ImageLayout := GetImageLayout;
               with ImageLayout
               do begin
                  Frame.Left   := rsLeft.Value;
                  Frame.Top    := rsTop.Value;
                  Frame.Right  := rsRight.Value;
                  Frame.Bottom := rsBottom.Value;
               end;
               SetImageLayout(ImageLayout);
          end
          else { Error, Could not set Unit. };
     end;

     //-------------------------------------------------------------------------
     // Set Orientation information.
     //-------------------------------------------------------------------------

     if (Containers.Items[ICAP_ORIENTATION] <> Nil)
     then begin
          Containers.Items[ICAP_ORIENTATION].CurrentValue := FDegree;
          if (SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_ORIENTATION]) = TWRC_SUCCESS)
          then ;
     end;

     if (Containers.Items[ICAP_ROTATION] <> Nil)
     then begin
          Containers.Items[ICAP_ROTATION].CurrentValue := seRotation.Value;
          if (SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_ROTATION]) = TWRC_SUCCESS)
          then ;
     end;

     //-------------------------------------------------------------------------
     // Set Color information.
     //-------------------------------------------------------------------------
     if (Containers.Items[ICAP_PIXELTYPE] <> Nil)
     then begin
          Containers.Items[ICAP_PIXELTYPE].CurrentValue := GetCBObjectValue(cbPixelType);
          if (SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_PIXELTYPE]) = TWRC_SUCCESS)
          then ;
     end;
  end;
end; { End TDlgTwainPref.TWAINUserValues.                                      }

{$IFDEF EXTENDED_SYNTAX} {$X-} {$UNDEF EXTENDED_SYNTAX} {$ENDIF}

end.
