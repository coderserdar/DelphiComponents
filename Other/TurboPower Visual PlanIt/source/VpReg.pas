{*********************************************************}
{*                  VPREG.PAS 1.03                       *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I Vp.INC}    { Compiler Version Defines }
{$R VpREG.RES} { Palette Glyphs           }

unit VpReg;
  {Registration unit for the Visual PlanIt design-time interface}

interface

uses
  DIALOGS,
  Windows,
  {$IFDEF VERSION6}
    DesignIntf,
    DesignEditors,
    VCLEditors,
  {$ELSE}
    DsgnIntf,
  {$ENDIF}
  Classes,
  Controls,
  TypInfo,
  Forms,
  SysUtils,
  VpDatePropEdit;

type
  {TDBStringProperty}
  TDBStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  {TAliasNameProperty}
  TAliasNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {TDriverNameProperty}
  TDriverNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  { TDataStoreProperty }
  TDataStoreProperty = class(TComponentProperty)
  private
    FCheckProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TVpDateProperty = class (TFloatProperty)
    public
      procedure Edit; override;
      function  GetAttributes : TPropertyAttributes; override;
      function  GetValue : string; override;
      procedure SetValue (const Value : string); override;
  end;

  TVpGenericFileNameProperty = class (TStringProperty)
    protected
    public
      function GetAttributes: TPropertyAttributes; override;
      procedure Edit; override;
  end;

  TVpLocalizeFileNameProperty = class (TVpGenericFileNameProperty)
  end;

procedure Register;

implementation

uses
  DbTables,                   { VCL - BDE runtime unit                       }
  VpWavPE,                    { Wav File Finder - Property Editor            }

  { Component Units                                                          }
  VpBase,                     { Base classes for Vp                          }
  VpClock,                    { Clock Component                              }
  VpDlg,                      { Dialog components ancestor                   }
  VpLEDLabel,                 { LEDLabel Component                           }
  VpCalendar,                 { Calendar Component                           }
  VpNavBar,                   { Navigation Bar Component                     }
  VpBaseDS,                   { Base DataStore Classes                       }
  VpBDEDS,                    { DataStore Component                          }
  VpDayView,                  { Day View Component                           }
  VpWeekView,                 { Week View Component                          }
  VpMonthView,                { Month View Component                         }
  VpContactGrid,              { ContactGrid Component                        }
  VpDateEdit,                 { DateEdit Component                           }
  VpTaskList,                 { Task List Component                          }
  VpFlxDS,                    { Flexible DataStore                           }
  VpContactEditDlg,           { Contact Edit Dialog Component                }
  VpTaskEditDlg,              { Task Edit Dialog Component                   }
  VpEvntEditDlg,              { Event Edit Dialog Component                  }
  VpAlarmDlg,                 { Alarm Notification Dialog                    }
  VpResEditDlg,               { Resource Edit Dialog                         }
  VpPrtPrv,                   { Print Preview Component                      }
  VpPrtFmtCBox,               { Print Format Combo Box Component             }
  VpPrtPrvDlg,                { Print Preview Dialog                         }
  VpPrtFmtDlg,                { Print Format Dialog                          }
  VpPrtFmtEd,                 { Print Format Property editor                 }
  VpContactButtons,           { - New contact grid button bar component }    
  { Designtime Interfaces (Property and Component Editors)                   }
  VpAbout,                    { About form for the About property editor     }
  VpNabEd,                    { component editor for the VpNavBar            }
  VpFlxDSEd1;                 { Field mapper component editor for the FlexDS }


(*****************************************************************************)
{ TAliasNameProperty }

procedure TAliasNameProperty.GetValueList(List: TStrings);
begin
  (GetComponent(0) as TVpBDEDataStore).Database.Session.GetAliasNames(List);
end;

(*****************************************************************************)
{ TDBStringProperty }

function TDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;
{=====}

procedure TDBStringProperty.GetValueList(List: TStrings);
begin
end;
{=====}

procedure TDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;
{=====}

(*****************************************************************************)
{ TDriverNameProperty }

procedure TDriverNameProperty.GetValueList(List: TStrings);
begin
  (GetComponent(0) as TVpBDEDataStore).Database.Session.GetDriverNames(List);
end;
{=====}

(*****************************************************************************)
{ TDataStoreProperty }
procedure TDataStoreProperty.CheckComponent(const Value: string);
var
  J: Integer;
  DataStore: TVpCustomDataStore;
begin
  DataStore := TVpCustomDataStore(Designer.GetComponent(Value));
  for J := 0 to PropCount - 1 do
    if TVpDayView(GetComponent(J)).DataStore = DataStore then
      Exit;
  FCheckProc(Value);
end;

procedure TDataStoreProperty.GetValues(Proc: TGetStrProc);
begin
  FCheckProc := Proc;
  inherited GetValues(CheckComponent);
end;
{=====}

(*****************************************************************************)
{ TDataStoreProperty }
procedure TVpDateProperty.Edit;

var
  frmDatePropertyEditor : TfrmDatePropertyEditor;

begin
  frmDatePropertyEditor := TfrmDatePropertyEditor.Create (Application);
  try
    frmDatePropertyEditor.VpCalendar1.Date := GetFloatValue;
    if frmDatePropertyEditor.Execute then
      SetFloatValue (Trunc (frmDatePropertyEditor.VpCalendar1.Date));
  finally
    frmDatePropertyEditor.Free;
  end;
end;
{=====}

(*****************************************************************************)
{ TVpDateProperty }
function TVpDateProperty.GetAttributes : TPropertyAttributes;
begin
  Result := [paDialog, paMultiSelect];
end;
{=====}

function TVpDateProperty.GetValue : string;
begin
  Result := FormatDateTime ('ddddd', GetFloatValue);
end;
{=====}

procedure TVpDateProperty.SetValue (const Value : string);
begin
  SetFloatValue (StrToDate (Value));
end;
{=====}

(*****************************************************************************)
{ TVpGenericFileNameProperty }
function TVpGenericFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TVpGenericFileNameProperty.Edit;
const
  VpRegLocalizeFilter = 'Localization Files (*.XML)|*.XML';
  VpRegDefFilter      = 'All Files (*.*)|*.*';

var
  Dlg    : TOpenDialog;
  Filter : string;

begin
  Filter := '';
  if Self is TVpLocalizeFileNameProperty then
    Filter := VpRegLocalizeFilter;

  if Filter = '' then
    Filter := VpRegDefFilter
  else
    Filter := Filter + '|' + VpRegDefFilter;

  Dlg := TOpenDialog.Create (Application);
  try
    Dlg.DefaultExt := '*.*';
    Dlg.Filter := Filter;
    Dlg.FilterIndex := 0;
    Dlg.Options := [ofHideReadOnly];
    Dlg.FileName := Value;
    if Dlg.Execute then
      Value := Dlg.FileName;
  finally
    Dlg.Free;
  end;
end;

{*** component registration ***}
  procedure Register;
  begin
    { register component editors }
    RegisterComponentEditor(TVpNavBar, TVpNavBarEditor);
    RegisterComponentEditor(TVpControlLink, TVpPrtFmtPropertyEditor);
    RegisterComponentEditor(TVpFlexDataStore, TVpFlexDSEditor);

    { register the About Box property editor for the Version properties        }
    RegisterPropertyEditor(TypeInfo(string), TVpCollectionItem,
      'Version', TVpAboutProperty);
    RegisterPropertyEditor(TypeInfo(string), TVpComponent,
      'Version', TVpAboutProperty);
    RegisterPropertyEditor(TypeInfo(string), TVpNavBar,
      'Version', TVpAboutProperty);
    RegisterPropertyEditor(TypeInfo(string), TVpCalendar,
      'Version', TVpAboutProperty);
    RegisterPropertyEditor(TypeInfo(string), TVpLEDLabel,
      'Version', TVpAboutProperty);
    RegisterPropertyEditor(TypeInfo(string), TVpClock,
      'Version', TVpAboutProperty);
    RegisterPropertyEditor(TypeInfo(string), TVpResourceCombo,
      'Version', TVpAboutProperty);
    RegisterPropertyEditor(TypeInfo(string), TVpCustomControl,
      'Version', TVpAboutProperty);
    RegisterPropertyEditor(TypeInfo(string), TVpControlLink,
      'Version', TVpAboutProperty);
    RegisterPropertyEditor(TypeInfo(string), TVpBDEDataStore,
      'Version', TVpAboutProperty);
    RegisterPropertyEditor(TypeInfo(string), TVpFlexDataStore,
      'Version', TVpAboutProperty);
    RegisterPropertyEditor(TypeInfo(string), TVpDateEdit,
      'Version', TVpAboutProperty);
    RegisterPropertyEditor(TypeInfo(string), TVpBaseDialog,
      'Version', TVpAboutProperty);
    RegisterPropertyEditor(TypeInfo(string), TVpPrintFormatComboBox,
      'Version', TVpAboutProperty);

    {register the BDE Alias and Driver properties                             }
    RegisterPropertyEditor(TypeInfo(string), TVpBDEDataStore,
      'AliasName', TAliasNameProperty);
    RegisterPropertyEditor(TypeInfo(string), TVpBDEDataStore,
      'DriverName', TDriverNameProperty);

    {register the DayView properties                                          }
    RegisterPropertyEditor(TypeInfo(TVpCustomDataStore), TVpDayView,
      'DataStore', TDataStoreProperty);

    {register the property editor for the DataStore's DefaultAlarmWav         }
    RegisterPropertyEditor(TypeInfo(string), TVpCustomDataStore,
      'DefaultEventSound', TWavFileProperty);

    RegisterPropertyEditor (TypeInfo (TDateTime),
                            TVpPrintPreview,
                            'StartDate',
                            TVpDateProperty);
    RegisterPropertyEditor (TypeInfo (TDateTime),
                            TVpPrintPreview,
                            'EndDate',
                            TVpDateProperty);
    RegisterPropertyEditor (TypeInfo (TDateTime),
                            TVpPrintPreviewDialog,
                            'StartDate',
                            TVpDateProperty);
    RegisterPropertyEditor (TypeInfo (TDateTime),
                            TVpPrintPreviewDialog,
                            'EndDate',
                            TVpDateProperty);

    RegisterPropertyEditor (TypeInfo (string), TVpControlLink,
                            'LocalizationFile', TVpLocalizeFileNameProperty);

    {register Visual PlanIt components with the IDE}
    RegisterComponents('Visual PlanIt', [
      TVpLEDLabel,
      TVpClock,
      TVpCalendar,
      TVpNavBar,
      TVpFlexDataStore,
      TVpBDEDataStore,
      TVpControlLink,
      TVpPrintPreview,
      TVpPrintFormatComboBox,
      TVpDateEdit,
      TVpResourceCombo,
      TVpDayView,
      TVpWeekView,
      TVpMonthView,
      TVpContactGrid,
      TVpContactButtonBar,                                               
      TVpTaskList,
      TVpNotificationDialog,
      TVpResourceEditDialog,
      TVpEventEditDialog,
      TVpContactEditDialog,
      TVpTaskEditDialog,
      TVpPrintFormatEditDialog,
      TVpPrintPreviewDialog]);
  end;
end.
