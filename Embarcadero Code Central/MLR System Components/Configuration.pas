unit Configuration;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	Registry, checklst, StdCtrls, Grids, ComCtrls, Buttons, Extctrls;

type
	TConfigMode = (cmUser, cmComputer);
	TConfiguration = class(TComponent)
	private
    procedure SetApplicationName(const Value: string);
		{ Private declarations }
	protected
		{ Protected declarations }
		Reg								:TRegistry;
		FApplicationName	:string;
		FSoftwareHouse		:string;
		FMode							:TConfigMode;
		FOpenedKey				:Boolean;
		FBase							:string;
		procedure Check;
		procedure SetMode(Value :TConfigMode);
		function	CreateControlIdentifier(Control :TControl): string;
	public
		{ Public declarations }
		constructor Create(AOwner :TComponent); override;
		destructor Destroy; override;
		procedure	WriteString(Identifier, Value :string);
		function	ReadString(Identifier, Default :string): string;
		procedure	WriteStrings(const Identifier :string; Strings :TStrings);
		procedure	ReadStrings(const Identifier :string; Strings :TStrings);
		procedure	WriteBool(const Identifier :string; const Value :Boolean);
		function	ReadBoolWithDefault(const Identifier :string; const Value :Boolean): Boolean;
		procedure	WriteInteger(const Identifier :string; const Value :Integer);
		function	ReadInteger(const Identifier :string): Integer;
		function	ReadIntegerWithDefault(const Identifier :string;const Default :Integer): Integer;
		{ Automatically write controls. }
		procedure	WriteCheckListbox(CheckListbox :TCheckListbox);
		procedure	ReadCheckListbox(CheckListbox :TCheckListbox);
		procedure	WriteCheckListboxValues(CheckListbox :TCheckListbox);
		procedure	ReadCheckListboxValues(CheckListbox :TCheckListbox);
		procedure WriteCheckbox(Checkbox :TCheckbox);
		procedure	ReadCheckbox(Checkbox :TCheckbox; Default :Boolean);
		procedure	WriteEdit(Edit :TEdit);
		procedure ReadEdit(Edit :TEdit);
		procedure ReadEditWithDefault(Edit :TEdit; Default :string);
		procedure	WriteStringGrid(StringGrid :TStringGrid);
		procedure	ReadStringGrid(StringGrid :TStringGrid);
		procedure ReadStringGridWithCSVTitles(StringGrid :TStringGrid; const Titles :string);
		procedure	ReadPageControl(PageControl :TPageControl);
		procedure WritePageControl(PageControl :TPageControl);
		procedure	ReadRadioGroup(RadioGroup :TRadioGroup);
		procedure	WriteRadioGroup(RadioGroup :TRadioGroup);
		procedure	ReadWindowPosition(Window :TWinControl);
		procedure	WriteWindowPosition(window :TWinControl);
		procedure ReadListBox(ListBox :TListBox);
		procedure WriteListBox(ListBox :TListBox);
		procedure ReadListBoxContents(ListBox :TListBox);
		procedure WriteListBoxContents(ListBox :TListBox);
    procedure ReadUpDown(UpDown :TUpDown; const DefaultValue :Integer);
    procedure WriteUpDown(UpDown :TUpDown);
    procedure ReadDateTimePicker(DateTimePicker :TDateTimePicker; const DefaultValue :TDateTime);
    procedure WriteDateTimePicker(DateTimePicker :TDateTimePicker);
    procedure ReadComboBox(ComboBox :TComboBox);
    procedure WriteComboBox(ComboBox :TComboBox);
	published
		{ Published declarations }
		property	ApplicationName	:string read FApplicationName write SetApplicationName;
		property	SoftwareHouse		:string read FSoftwareHouse write FSoftwareHouse;
		property	Mode						:TConfigMode read FMode write SetMode default cmUser;
	end;

procedure Register;

implementation

procedure Register;
begin
	RegisterComponents('MLR Sysco', [TConfiguration]);
end;

procedure TConfiguration.WriteString(Identifier, Value :string);
begin
	Check;
	Reg.WriteString(Identifier, Value);
end;

function TConfiguration.ReadString(Identifier, Default :string):string;
begin
	Check;
	if Reg.ValueExists(Identifier) then
		Result	:= Reg.ReadString(Identifier)
	else
		Result	:= Default;
end;

constructor TConfiguration.Create(AOwner :TComponent);
begin
	inherited Create(AOwner);
	Reg					:= TRegistry.Create;
	FMode				:= cmUser;
	Reg.RootKey	:= HKEY_CURRENT_USER;
	FOpenedKey	:= False;
end;

destructor TConfiguration.Destroy;
begin
	Reg.Free;
	inherited Destroy;
end;

procedure TConfiguration.Check;
begin
	if FApplicationName = '' then
		raise Exception.Create('You must specify a valid application name.');
	if FSoftwareHouse = '' then
		raise Exception.Create('You must specify a valid software developer house.');
	if not FOpenedKey then begin
		FBase	:= 'Software\' + FSoftwareHouse + '\' + FApplicationName;
		Reg.OpenKey(FBase, True);
		FOpenedKey	:= True;
	end;
end;

procedure TConfiguration.SetMode(Value :TConfigMode);
begin
	FMode	:= Value;
	case FMode of
		cmUser:			Reg.RootKey	:= HKEY_CURRENT_USER;
		cmComputer: Reg.RootKey := HKEY_LOCAL_MACHINE;
	end;
end;

procedure TConfiguration.WriteCheckListbox(CheckListbox :TCheckListbox);
var
	i	:Integer;
begin
	Check;
	try
		Reg.DeleteKey(CheckListbox.Parent.Name + CheckListbox.Name);
	except
		{ Ignore all errors. }
	end;
	Reg.OpenKey(CheckListbox.Parent.Name + CheckListbox.Name, True);
	for i := 0 to CheckListbox.Items.Count - 1 do
		Reg.WriteBool(CheckListbox.Items[i], CheckListbox.Checked[i]);
	Reg.CloseKey;
	Reg.OpenKey(FBase, False);
end;

procedure TConfiguration.ReadCheckListbox(CheckListbox :TCheckListbox);
var
	i	:Integer;
begin
	Check;
	if Reg.KeyExists(CheckListbox.Parent.Name + CheckListbox.Name) then begin
		Reg.OpenKey(CheckListbox.Parent.Name + CheckListbox.Name, False);
		Reg.GetValueNames(CheckListbox.Items);
		for i := 0 to CheckListbox.Items.Count - 1 do
			CheckListbox.Checked[i]	:= Reg.ReadBool(CheckListbox.Items[i]);
		Reg.CloseKey;
		Reg.OpenKey(FBase, False);
	end else begin
		CheckListbox.Items.Clear;
	end;
end;

procedure TConfiguration.WriteCheckbox(Checkbox :TCheckbox);
begin
	Check;
	Reg.WriteBool(Checkbox.Parent.Name + CheckBox.Name, Checkbox.Checked);
end;

procedure TConfiguration.ReadCheckbox(Checkbox :TCheckbox; Default :Boolean);
begin
	Check;
	if Reg.ValueExists(Checkbox.Parent.Name + CheckBox.Name) then
		Checkbox.Checked	:= Reg.ReadBool(Checkbox.Parent.Name + CheckBox.Name)
	else
		Checkbox.Checked	:= Default;
end;

procedure	TConfiguration.WriteBool(const Identifier :string; const Value :Boolean);
begin
	Check;
	Reg.WriteBool(Identifier, Value);
end;

function TConfiguration.ReadBoolWithDefault(const Identifier :string; const Value :Boolean) :Boolean;
begin
	Check;
	if Reg.ValueExists(Identifier) then
		Result	:= Reg.ReadBool(Identifier)
	else
		Result	:= Value;
end;

procedure	TConfiguration.WriteEdit(Edit :TEdit);
begin
	Check;
	Reg.WriteString(CreateControlIdentifier(Edit), Edit.Text);
end;

procedure	TConfiguration.ReadEdit(Edit :TEdit);
begin
	Check;
	if Reg.ValueExists(CreateControlIdentifier(Edit)) then
		Edit.Text	:= Reg.ReadString(CreateControlIdentifier(Edit))
	else
		Edit.Text	:= '';
end;

procedure	TConfiguration.ReadEditWithDefault(Edit :TEdit; Default :string);
begin
	Check;
	if Reg.ValueExists(CreateControlIdentifier(Edit)) then
		Edit.Text	:= Reg.ReadString(CreateControlIdentifier(Edit))
	else
		Edit.Text	:= Default;
end;

function TConfiguration.CreateControlIdentifier(Control :TControl): string;
begin
	if Control.Parent <> nil then
		Result	:= Control.Parent.Name + Control.Name
	else
		Result	:= Control.Name;
end;

procedure TConfiguration.WriteStrings(const Identifier :string; Strings :TStrings);
var
	i	:Integer;
begin
	Check;
	try
		Reg.DeleteKey(Identifier);
	except
		{ Ignore all errors. }
	end;
	Reg.OpenKey(Identifier, True);
	for i := 0 to Strings.Count - 1 do
		Reg.WriteString(Strings[i], '-');
	Reg.CloseKey;
	Reg.OpenKey(FBase, False);
end;

procedure TConfiguration.ReadStrings(const Identifier :string; Strings :TStrings);
begin
	Check;
	if Reg.KeyExists(Identifier) then begin
		Reg.OpenKey(Identifier, False);
		Reg.GetValueNames(Strings);
		Reg.CloseKey;
		Reg.OpenKey(FBase, False);
	end else begin
		Strings.Clear;
	end;
end;

procedure TConfiguration.WriteStringGrid(StringGrid :TStringGrid);
var
	i						:Integer;
	Identifier	:string;
begin
	Check;
	Identifier	:= CreateControlIdentifier(StringGrid);
	for i := 0 to StringGrid.ColCount - 1 do
		WriteString(Identifier + IntToStr(i), StringGrid.Cols[i].CommaText);
	WriteInteger(Identifier + 'ColCount', StringGrid.ColCount);
	WriteInteger(Identifier + 'RowCount', StringGrid.RowCount);
end;

procedure TConfiguration.ReadStringGrid(StringGrid :TStringGrid);
var
	i						:Integer;
	Identifier	:string;
	j						:Integer;
	Strings			:TStringList;
begin
	Check;
	Identifier					:= CreateControlIdentifier(StringGrid);
	StringGrid.ColCount	:= ReadIntegerWithDefault(Identifier + 'ColCount', StringGrid.ColCount);
	StringGrid.RowCount	:= ReadIntegerWithDefault(Identifier + 'RowCount', StringGrid.RowCount);
	Strings							:= TStringList.Create;
	try
		for i := 0 to StringGrid.ColCount - 1 do begin
			Strings.CommaText	:= ReadString(Identifier + IntToStr(i), '');
			for j := 0 to Strings.Count - 1 do begin
				StringGrid.Cells[i, j]	:= Strings[j];
			end;
		end;
	finally
		Strings.Free;
	end;
end;

procedure TConfiguration.ReadStringGridWithCSVTitles(StringGrid :TStringGrid; const Titles :string);
begin
	ReadStringGrid(StringGrid);
	if StringGrid.RowCount = 0 then StringGrid.RowCount := 1;
	StringGrid.Rows[0].CommaText	:= Titles;
end;

procedure TConfiguration.WriteInteger(const Identifier :string; const Value :Integer);
begin
	Check;
	Reg.WriteInteger(Identifier, Value);
end;

function TConfiguration.ReadInteger(const Identifier :string): Integer;
begin
	Check;
	Result	:= Reg.ReadInteger(Identifier);
end;

function TConfiguration.ReadIntegerWithDefault(const Identifier :string; const Default :Integer): Integer;
begin
	Check;
	if Reg.ValueExists(Identifier) then
		Result	:= Reg.ReadInteger(Identifier)
	else
		Result	:= Default;
end;

procedure TConfiguration.ReadPageControl(PageControl :TPageControl);
var
	Identifier	:string;
	ActiveID		:string;
	I						:Integer;
begin
	Check;
	Identifier	:= CreateControlIdentifier(PageControl);
	if Reg.ValueExists(Identifier) then begin
		ActiveID	:= ReadString(Identifier, '');
		for i := 0 to PageControl.PageCount - 1 do
			if PageControl.Pages[i].Name = ActiveID then begin
				PageControl.ActivePage	:= PageControl.Pages[i];
				exit;
			end;
	end;
end;

procedure TConfiguration.WritePageControl(PageControl :TPageControl);
begin
	Check;
	Reg.WriteString(CreateControlIdentifier(PageControl),
		PageControl.ActivePage.Name);
end;

procedure TConfiguration.ReadRadioGroup(RadioGroup :TRadioGroup);
begin
	Check;
	RadioGroup.ItemIndex	:=
		ReadIntegerWithDefault(CreateControlIdentifier(RadioGroup), RadioGroup.ItemIndex);
end;

procedure TConfiguration.WriteRadioGroup(RadioGroup :TRadioGroup);
begin
	Check;
	Reg.WriteInteger(CreateControlIdentifier(RadioGroup),
		RadioGroup.ItemIndex);
end;

procedure TConfiguration.WriteWindowPosition(Window :TWinControl);
var
	ControlIdentifier	:string;
begin
	Check;
	ControlIdentifier	:= CreateControlIdentifier(Window);
	Reg.WriteInteger(ControlIdentifier + 'Left', Window.Left);
	Reg.WriteInteger(ControlIdentifier + 'Top', Window.Top);
	Reg.WriteInteger(ControlIdentifier + 'Height', Window.Height);
	Reg.WriteInteger(ControlIdentifier + 'Width', Window.Width);
	if Window is TCustomForm then
		Reg.WriteInteger(ControlIdentifier + 'State', Integer(TCustomForm(Window).WindowState));
end;

procedure TConfiguration.ReadWindowPosition(window :TWinControl);
var
	ControlIdentifier	:string;
begin
	Check;
	ControlIdentifier	:= CreateControlIdentifier(Window);
	Window.Left				:= ReadIntegerWithDefault(ControlIdentifier + 'Left', Window.Left);
	Window.Top				:= ReadIntegerWithDefault(ControlIdentifier + 'Top', Window.Top);
	Window.Width			:= ReadIntegerWithDefault(ControlIdentifier + 'Width', Window.Width);
	Window.Height			:= ReadIntegerWithDefault(ControlIdentifier + 'Height', Window.Height);
	if Window is TCustomForm then
		TCustomForm(Window).WindowState	:= TWindowState(ReadIntegerWithDefault(
			ControlIdentifier + 'State', Integer(TCustomForm(Window).WindowState)));
end;

procedure TConfiguration.ReadListBox(ListBox :TListBox);
begin
	Check;
	ListBox.ItemIndex	:= ReadIntegerWithDefault(CreateControlIdentifier(ListBox), ListBox.ItemIndex);
end;

procedure TConfiguration.WriteListBox(ListBox :TListBox);
begin
	Check;
	WriteInteger(CreateControlIdentifier(ListBox), ListBox.ItemIndex);
end;

procedure TConfiguration.ReadUpDown(UpDown :TUpDown; const DefaultValue :Integer);
begin
	Check;
  UpDown.Position := ReadIntegerWithDefault(CreateControlIdentifier(UpDown), DefaultValue);
end;

procedure TConfiguration.WriteUpDown(UpDown :TUpDown);
begin
	Check;
  WriteInteger(CreateControlIdentifier(UpDown), UpDown.Position);
end;

procedure TConfiguration.ReadDateTimePicker(DateTimePicker :TDateTimePicker; const DefaultValue :TDateTime);
begin
	Check;
  DateTimePicker.DateTime := StrToDateTime(
  	ReadString(CreateControlIdentifier(DateTimePicker), DateTimeToStr(DefaultValue)));
end;

procedure TConfiguration.WriteDateTimePicker(DateTimePicker :TDateTimePicker);
begin
	Check;
  WriteString(CreateControlIdentifier(DateTimePicker),
  	DateTimeToStr(DateTimePicker.DateTime));
end;

procedure	TConfiguration.WriteCheckListboxValues(CheckListbox :TCheckListbox);
var	i	:Integer; ID :string;
begin
	Check;
  ID	:= CreateControlIdentifier(CheckListbox);
  Reg.OpenKey(ID, True);
  for i := 0 to CheckListbox.Items.Count - 1 do
  	Reg.WriteBool(CheckListbox.Items[i], CheckListbox.Checked[i]);
  Reg.CloseKey;
  Reg.OpenKey(FBase, False);
end;

procedure	TConfiguration.ReadCheckListboxValues(CheckListbox :TCheckListbox);
var	i	:Integer; ID :string;
begin
	Check;
  ID	:= CreateControlIdentifier(CheckListbox);
  if Reg.KeyExists(ID) then begin
  	Reg.OpenKey(ID, False);
    for i := 0 to CheckListbox.Items.Count - 1 do
    	try
	    	CheckListbox.Checked[i] := Reg.ReadBool(CheckListbox.Items[i]);
	    except
        CheckListbox.Checked[i] := False;
	    end;
    Reg.CloseKey;
    Reg.OpenKey(FBase, False);
  end else begin
    for i := 0 to CheckListbox.Items.Count - 1 do
      CheckListbox.Checked[i] := False;
  end;
end;

procedure TConfiguration.ReadComboBox(ComboBox: TComboBox);
begin
	Check;
	if Reg.ValueExists(CreateControlIdentifier(ComboBox)) then begin
    if ComboBox.Style = csDropDownList then
      ComboBox.ItemIndex := Reg.ReadInteger(CreateControlIdentifier(ComboBox))
    else
      ComboBox.Text := Reg.ReadString(CreateControlIdentifier(ComboBox));
    if Assigned(ComboBox.OnChange) then
    	ComboBox.OnChange(ComboBox);
  end;
end;

procedure TConfiguration.WriteComboBox(ComboBox: TComboBox);
begin
	Check;
  if ComboBox.Style = csDropDownList then
    Reg.WriteInteger(CreateControlIdentifier(ComboBox), ComboBox.ItemIndex)
  else
  	Reg.WriteString(CreateControlIdentifier(ComboBox), ComboBox.Text);
end;

procedure TConfiguration.SetApplicationName(const Value: string);
begin
  if FOpenedKey then begin
    Reg.CloseKey;
    FOpenedKey := False;
  end;
  FApplicationName := Value;
end;

procedure TConfiguration.ReadListBoxContents(ListBox: TListBox);
begin
	Check;
  ReadStrings(CreateControlIdentifier(ListBox), ListBox.Items);
  ReadListBox(ListBox);
end;

procedure TConfiguration.WriteListBoxContents(ListBox: TListBox);
begin
	Check;
  WriteStrings(CreateControlIdentifier(ListBox), ListBox.Items);
  WriteListBox(ListBox);
end;

end.
