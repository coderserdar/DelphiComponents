{*********************************************************}
{*                 VPFLXDSED1.PAS 1.03                   *}
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

{.$DEFINE RUNTIMETEST}
{$I Vp.INC}

unit VpFlxDsEd1;
  { Flexible DataStore ComponentEditor }
  { Introduced in version 1.01         }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFNDEF RUNTIMETEST}
  {$IFDEF VERSION6} DesignIntf, DesignEditors, {$ELSE} DsgnIntf, {$ENDIF}
  {$ENDIF} {RUNTIMETEST}
  ExtCtrls, StdCtrls, Db, DBTables, VpData, VpFlxDS, ComCtrls, Buttons;

type
  TfrmFieldMapper = class(TForm)
    Panel1: TPanel;
    Button2: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Bevel1: TBevel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label1: TLabel;
    DatasetFieldLB: TListBox;
    VPFieldLB: TListBox;
    FieldMappingsLB: TListBox;
    btnDeleteMapping: TButton;
    DatasetCombo: TComboBox;
    btnAddMapping: TBitBtn;
    btnClearMappings: TButton;
    procedure FormShow(Sender: TObject);
    procedure DatasetComboChange(Sender: TObject);
    procedure DBFieldSelected(Sender: TObject);
    procedure DatasetFieldLBKeyPress(Sender: TObject; var Key: Char);
    procedure VpFieldSelected(Sender: TObject);
    procedure VPFieldLBKeyPress(Sender: TObject; var Key: Char);
    procedure btnAddMappingClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure btnDeleteMappingClick(Sender: TObject);
    procedure btnClearMappingsClick(Sender: TObject);
    procedure FieldMappingsLBClick(Sender: TObject);
    procedure FieldMappingsLBKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    DSResActive    : Boolean;
    DSEventActive  : Boolean;
    DSContactActive: Boolean;
    DSTaskActive   : Boolean;
    procedure SyncObjects;
    procedure OpenDatasets;
  public
    FlexDS: TVpFlexDataStore;
    ResDS: TDataset;
    EventsDS: TDataset;
    ContactsDS: TDataset;
    TasksDS: TDataset;
  end;

{$IFDEF RUNTIMETEST}
procedure RuntimeTest(FlexDS: TVpFlexDataStore);
{$ELSE}
  TVpFlexDSEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer) : string; override;
    function GetVerbCount : Integer; override;
  end;
{$ENDIF}

implementation
{$R *.DFM}
uses
  vpConst;

{$IFDEF RUNTIMETEST}
{ Runtime test }

procedure RuntimeTest(FlexDS: TVpFlexDataStore);
var
  frmFieldMapper: TfrmFieldMapper;
begin
  if FlexDS = nil then
    Exit;

  Application.CreateForm(TfrmFieldMapper, frmFieldMapper);

  try
    frmFieldMapper.FlexDS := FlexDS;

    if FlexDS.ResourceDataSource <> nil then
      frmFieldMapper.ResDS      := FlexDS.ResourceDataSource.DataSet;

    if FlexDS.EventsDataSource <> nil then
      frmFieldMapper.EventsDS   := FlexDS.EventsDataSource.DataSet;

    if FlexDS.ContactsDataSource <> nil then
      frmFieldMapper.ContactsDS := FlexDS.ContactsDataSource.DataSet;

    if FlexDS.TasksDataSource <> nil then
      frmFieldMapper.TasksDS    := FlexDS.TasksDataSource.DataSet;

    frmFieldMapper.ShowModal;

  finally
    frmFieldMapper.release;
  end;
end;
{=====}

{$ELSE} {RUNTIMETEST}

{$IFDEF VERSION6}
procedure MapDatabaseFields(Designer : IDesigner;
  FlexDS : TVpFlexDataStore);
{$ELSE}
procedure MapDatabaseFields(Designer : IFormDesigner;
  FlexDS : TVpFlexDataStore);
{$ENDIF}
var
  frmFieldMapper: TfrmFieldMapper;
begin
  if FlexDS = nil then
    Exit;
  Application.CreateForm(TfrmFieldMapper, frmFieldMapper);
  try
    frmFieldMapper.FlexDS := FlexDS;
    if FlexDS.ResourceDataSource <> nil then
      frmFieldMapper.ResDS      := FlexDS.ResourceDataSource.DataSet;
    if FlexDS.EventsDataSource <> nil then
      frmFieldMapper.EventsDS   := FlexDS.EventsDataSource.DataSet;
    if FlexDS.ContactsDataSource <> nil then
      frmFieldMapper.ContactsDS := FlexDS.ContactsDataSource.DataSet;
    if FlexDS.TasksDataSource <> nil then
      frmFieldMapper.TasksDS    := FlexDS.TasksDataSource.DataSet;
    frmFieldMapper.ShowModal;
  finally
    frmFieldMapper.release;
  end;
  Designer.Modified;
end;
{=====}

{*** TVpNavBarEditor ***}

procedure TVpFlexDSEditor.ExecuteVerb(Index : Integer);
begin
  if Index = 0 then
    MapDatabaseFields(Designer, (Component as TVpFlexDataStore));
end;

function TVpFlexDSEditor.GetVerb(Index : Integer) : string;
begin
  if Index = 0 then
    Result := 'Map Database Fields...';
end;

function TVpFlexDSEditor.GetVerbCount : Integer;
begin
  Result := 1;
end;
{$ENDIF} {RuntimeTest}


procedure TfrmFieldMapper.FormShow(Sender: TObject);
begin
  DatasetCombo.Items.Clear;
  DatasetCombo.Text := '';

  try
    OpenDatasets;
  finally
    {Load DatasetCombo}
    if (ResDS <> nil) and (ResDS.Active) then
      DatasetCombo.Items.Add(ResourceTableName)
    else
      FlexDS.ResourceMappings.Clear;

    if (EventsDS <> nil) and (EventsDS.Active) then
      DatasetCombo.Items.Add(EventsTableName)
    else
      FlexDS.EventMappings.Clear;

    if (ContactsDS <> nil) and (ContactsDS.Active) then
      DatasetCombo.Items.Add(ContactsTableName)
    else
      FlexDS.ContactMappings.Clear;

    if (TasksDS <> nil) and (TasksDS.Active) then
      DatasetCombo.Items.Add(TasksTableName)
    else
      FlexDS.TaskMappings.Clear;
  end;
end;
{=====}

procedure TfrmFieldMapper.OpenDatasets;
var
  ErrorStr: string;
begin
  ErrorStr := '';
  btnAddMapping.Caption := 'Add Field'#13#10'Mapping';

  DSResActive := false;
  DSEventActive := false;

  {Open the Resources Dataset}
  if ResDS <> nil then begin
    DSResActive := ResDS.Active;
    if not ResDS.Active then
    try
      ResDS.Open;
    except
      ErrorStr := '     Resources (Failed to open)'#13#10;
    end;
  end else
    ErrorStr := '     Resources (Datasource not assigned)'#13#10;

  {Open the Events Dataset}
  if EventsDS <> nil then begin
    DSEventActive := EventsDS.Active;
    if not EventsDS.Active then
    try
      EventsDS.Open;
    except
      ErrorStr := ErrorStr + '     Events (Failed to open)'#13#10;
    end;
  end else
    ErrorStr := ErrorStr + '     Events (Datasource not assigned)'#13#10;

  {Open the Contacts Dataset}
  if ContactsDS <> nil then begin
    DSContactActive := ContactsDS.Active;
    try
      ContactsDS.Open;
    except
      ErrorStr := ErrorStr + '     Contacts (Failed to open)'#13#10;
    end;
  end else
    ErrorStr := ErrorStr + '     Contacts (Datasource not assigned)'#13#10;

  {Open the Tasks Dataset}
  if TasksDS <> nil then begin
    DSTaskActive := TasksDS.Active;
    try
      TasksDS.Open;
    except
      ErrorStr := ErrorStr + '     Tasks (Failed to open)'#13#10;
    end;
  end else
    ErrorStr := ErrorStr + '     Tasks (Datasource not assigned)'#13#10;

  { let the user know if there was a prolen opening any of the datasets. }
  if (ErrorStr <> '') then
    Application.MessageBox(PChar('There was an error opening the following '
      + 'datasets'#13#10#10 + ErrorStr + #10
      + 'Field mapping for these tables will not be available until the '
      + 'errors are corrected.'), 'Error Opening Dataset(s)', 0);
end;
{=====}

procedure TfrmFieldMapper.DatasetComboChange(Sender: TObject);
begin
  SyncObjects;
end;
{=====}

procedure TfrmFieldMapper.SyncObjects;
var
  I: integer;
  FM: TVpFieldMapping;
  MC: TCollection;
begin
  MC := nil;
  FieldMappingsLB.Items.Clear;
  DatasetFieldLB.Items.Clear;
  VpFieldLB.Clear;

  if DatasetCombo.Text = ResourceTableName then begin
    MC := FlexDS.ResourceMappings;
    if not ResDS.Active then
      ResDS.Open;
    ResDS.FieldDefs.GetItemNames(DataSetFieldLB.Items);
    VpFieldLB.Items.Add('ResourceID');
    VpFieldLB.Items.Add('Description');
    VpFieldLB.Items.Add('Notes');
    VpFieldLB.Items.Add('ImageIndex');
    VpFieldLB.Items.Add('Active');
    VpFieldLB.Items.Add('UserField0');
    VpFieldLB.Items.Add('UserField1');
    VpFieldLB.Items.Add('UserField2');
    VpFieldLB.Items.Add('UserField3');
    VpFieldLB.Items.Add('UserField4');
    VpFieldLB.Items.Add('UserField5');
    VpFieldLB.Items.Add('UserField6');
    VpFieldLB.Items.Add('UserField7');
    VpFieldLB.Items.Add('UserField8');
    VpFieldLB.Items.Add('UserField9');
  end

  else if DatasetCombo.Text = EventsTableName then begin
    MC := FlexDS.EventMappings;
    EventsDS.FieldDefs.GetItemNames(DataSetFieldLB.Items);
    VpFieldLB.Items.Add('RecordID');
    VpFieldLB.Items.Add('StartTime');
    VpFieldLB.Items.Add('EndTime');
    VpFieldLB.Items.Add('ResourceID');
    VpFieldLB.Items.Add('Description');
    VpFieldLB.Items.Add('Note');
    VpFieldLB.Items.Add('Category');
    VpFieldLB.Items.Add('AllDayEvent');
    VpFieldLB.Items.Add('AlarmWavPath');
    VpFieldLB.Items.Add('AlarmSet');
    VpFieldLB.Items.Add('AlarmAdv');
    VpFieldLB.Items.Add('AlarmAdvType');
    VpFieldLB.Items.Add('SnoozeTime');
    VpFieldLB.Items.Add('RepeatCode');
    VpFieldLB.Items.Add('RepeatRangeEnd');
    VpFieldLB.Items.Add('CustInterval');
    VpFieldLB.Items.Add('UserField0');
    VpFieldLB.Items.Add('UserField1');
    VpFieldLB.Items.Add('UserField2');
    VpFieldLB.Items.Add('UserField3');
    VpFieldLB.Items.Add('UserField4');
    VpFieldLB.Items.Add('UserField5');
    VpFieldLB.Items.Add('UserField6');
    VpFieldLB.Items.Add('UserField7');
    VpFieldLB.Items.Add('UserField8');
    VpFieldLB.Items.Add('UserField9');
  end

  else if DatasetCombo.Text = ContactsTableName then begin
    MC := FlexDS.ContactMappings;
    ContactsDS.FieldDefs.GetItemNames(DataSetFieldLB.Items);
    VpFieldLB.Items.Add('ResourceID');
    VpFieldLB.Items.Add('RecordID');
    VpFieldLB.Items.Add('FirstName');
    VpFieldLB.Items.Add('LastName');
    VpFieldLB.Items.Add('Birthdate');
    VpFieldLB.Items.Add('Anniversary');
    VpFieldLB.Items.Add('Title');
    VpFieldLB.Items.Add('Company');
    VpFieldLB.Items.Add('Position');
    VpFieldLB.Items.Add('EMail');
    VpFieldLB.Items.Add('Address');
    VpFieldLB.Items.Add('City');
    VpFieldLB.Items.Add('State');
    VpFieldLB.Items.Add('Zip');
    VpFieldLB.Items.Add('Country');
    VpFieldLB.Items.Add('Note');
    VpFieldLB.Items.Add('Phone1');
    VpFieldLB.Items.Add('Phone2');
    VpFieldLB.Items.Add('Phone3');
    VpFieldLB.Items.Add('Phone4');
    VpFieldLB.Items.Add('Phone5');
    VpFieldLB.Items.Add('PhoneType1');
    VpFieldLB.Items.Add('PhoneType2');
    VpFieldLB.Items.Add('PhoneType3');
    VpFieldLB.Items.Add('PhoneType4');
    VpFieldLB.Items.Add('PhoneType5');
    VpFieldLB.Items.Add('Category');
    VpFieldLB.Items.Add('Custom1');
    VpFieldLB.Items.Add('Custom2');
    VpFieldLB.Items.Add('Custom3');
    VpFieldLB.Items.Add('Custom4');
    VpFieldLB.Items.Add('UserField0');
    VpFieldLB.Items.Add('UserField1');
    VpFieldLB.Items.Add('UserField2');
    VpFieldLB.Items.Add('UserField3');
    VpFieldLB.Items.Add('UserField4');
    VpFieldLB.Items.Add('UserField5');
    VpFieldLB.Items.Add('UserField6');
    VpFieldLB.Items.Add('UserField7');
    VpFieldLB.Items.Add('UserField8');
    VpFieldLB.Items.Add('UserField9');
  end

  else if DatasetCombo.Text = TasksTableName then begin
    MC := FlexDS.TaskMappings;
    TasksDS.FieldDefs.GetItemNames(DataSetFieldLB.Items);
    VpFieldLB.Items.Add('ResourceID');
    VpFieldLB.Items.Add('RecordID');
    VpFieldLB.Items.Add('Complete');
    VpFieldLB.Items.Add('Description');
    VpFieldLB.Items.Add('Details');
    VpFieldLB.Items.Add('CreatedOn');
    VpFieldLB.Items.Add('CompletedOn');
    VpFieldLB.Items.Add('Priority');
    VpFieldLB.Items.Add('Category');
    VpFieldLB.Items.Add('DueDate');
    VpFieldLB.Items.Add('UserField0');
    VpFieldLB.Items.Add('UserField1');
    VpFieldLB.Items.Add('UserField2');
    VpFieldLB.Items.Add('UserField3');
    VpFieldLB.Items.Add('UserField4');
    VpFieldLB.Items.Add('UserField5');
    VpFieldLB.Items.Add('UserField6');
    VpFieldLB.Items.Add('UserField7');
    VpFieldLB.Items.Add('UserField8');
    VpFieldLB.Items.Add('UserField9');
  end;

  if MC <> nil then
    for I := 0 to pred(MC.Count) do begin
      FM := TVpFieldMapping(MC.Items[I]);
      {Delete mapped selection from the DatabaseFields list}
      if (DatasetFieldLB.Items.IndexOf(FM.DBField) > -1) then
        DatasetFieldLB.Items.Delete(DatasetFieldLB.Items.IndexOf(FM.DBField));
      {Delete mapped selection from the VPFields List}
      if (VPFieldLB.Items.IndexOf(FM.VPField) > -1) then
        VPFieldLB.Items.Delete(VPFieldLB.Items.IndexOf(FM.VPField));

      {Add the field mapping to the Field Mappings Listbox}
      FieldMappingsLB.Items.Add(FM.DBField + ' -> ' + FM.VPField);
    end;

  {enable/disable buttons}
  btnDeleteMapping.Enabled := false;
  btnClearMappings.Enabled := FieldMappingsLB.Items.Count > 0;
  btnAddMapping.Enabled := false;
  
  VpFieldLB.ItemIndex := -1;
  DatasetFieldLB.ItemIndex := -1;
end;
{=====}

procedure TfrmFieldMapper.DBFieldSelected(Sender: TObject);
begin
  btnAddMapping.Enabled := (VpFieldLB.ItemIndex > -1);
end;
{=====}

procedure TfrmFieldMapper.DatasetFieldLBKeyPress(Sender: TObject;
  var Key: Char);
begin
  DbFieldSelected(sender);
end;
{=====}

procedure TfrmFieldMapper.VpFieldSelected(Sender: TObject);
begin
  btnAddMapping.Enabled := (DatasetFieldLB.ItemIndex > -1);
end;
{=====}

procedure TfrmFieldMapper.VPFieldLBKeyPress(Sender: TObject;
  var Key: Char);
begin
  VpFieldSelected(Sender);
end;
{=====}

procedure TfrmFieldMapper.btnAddMappingClick(Sender: TObject);
var
  FM: TVpFieldMapping;
  MC: TCollection;
begin
  MC := nil;
  if DataSetCombo.Text = ResourceTableName then
    MC := FlexDS.ResourceMappings

  else if DataSetCombo.Text = EventsTableName then
    MC := FlexDS.EventMappings

  else if DataSetCombo.Text = ContactsTableName then
    MC := FlexDS.ContactMappings

  else if DataSetCombo.Text = TasksTableName then
    MC := FlexDS.TaskMappings;

  if MC <> nil then begin
    FM := TVpFieldMapping(MC.Add);

    FM.DBField := DatasetFieldLB.Items[DatasetFieldLB.ItemIndex];
    FM.VPField := VPFieldLB.Items[VPFieldLB.ItemIndex];

    SyncObjects;
  end;
end;
{=====}

procedure TfrmFieldMapper.Button2Click(Sender: TObject);
begin
  Close;
end;
{=====}

procedure TfrmFieldMapper.Button5Click(Sender: TObject);
begin
//  Help;
end;
{=====}

procedure TfrmFieldMapper.btnDeleteMappingClick(Sender: TObject);
begin
  if FieldMappingsLB.ItemIndex > -1 then begin

    if DataSetCombo.Text = ResourceTableName then
      FlexDS.ResourceMappings.Items[FieldMappingsLB.ItemIndex].Free

    else if DataSetCombo.Text = EventsTableName then
      FlexDS.EventMappings.Items[FieldMappingsLB.ItemIndex].Free

    else if DataSetCombo.Text = ContactsTableName then
      FlexDS.ContactMappings.Items[FieldMappingsLB.ItemIndex].Free

    else if DataSetCombo.Text = TasksTableName then
      FlexDS.TaskMappings.Items[FieldMappingsLB.ItemIndex].Free;

    SyncObjects;
  end;
end;
{=====}

procedure TfrmFieldMapper.btnClearMappingsClick(Sender: TObject);
var
  MC: TCollection;
begin
  MC := nil;
  if FieldMappingsLB.Items.Count > 0 then begin

    if DataSetCombo.Text = ResourceTableName then
      MC := FlexDS.ResourceMappings

    else if DataSetCombo.Text = EventsTableName then
      MC := FlexDS.EventMappings

    else if DataSetCombo.Text = ContactsTableName then
      MC := FlexDS.ContactMappings

    else if DataSetCombo.Text = TasksTableName then
      MC := FlexDS.TaskMappings;

    if MC <> nil then begin
      while (MC.Count > 0) do
        MC.Items[0].Free;
      SyncObjects;
    end;
  end;
end;
{=====}

procedure TfrmFieldMapper.FieldMappingsLBClick(Sender: TObject);
begin
  btnDeleteMapping.Enabled := FieldMappingsLB.ItemIndex > -1;
end;
{=====}

procedure TfrmFieldMapper.FieldMappingsLBKeyPress(Sender: TObject;
  var Key: Char);
begin
  FieldMappingsLBClick(Sender);
end;
{=====}

procedure TfrmFieldMapper.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  {reset all datasets to their original active status}
  if ResDS <> nil then
    ResDS.Active := DSResActive;
  if EventsDS <> nil then
    EventsDS.Active := DSEventActive;
  if ContactsDS <> nil then
    ContactsDS.Active := DSContactActive;
  if TasksDS <> nil then
    TasksDS.Active := DSTaskActive;
end;
{=====}

end.


 
