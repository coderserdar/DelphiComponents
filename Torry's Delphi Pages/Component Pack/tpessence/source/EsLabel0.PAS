
(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Essentials Vol I
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ES.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{$W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

{$IFNDEF Win32}
  {$G+} {286 Instructions}
  {$N+} {Numeric Coprocessor}
  {$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

unit EsLabel0;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, EsLabel, ExtCtrls,
{$IFDEF AboveD6}
  DesignIntf,                                                        {!!.09}
  DesignEditors,                                                     {!!.09}
{$ELSE}
  dsgnintf,
{$ENDIF}
  Menus, IniFiles,
  EsBase, EsData, EsMnuBtn, EsClrCbx, EsDir, EsWebPE, TypInfo;

type
  TEsLabelFrm = class(TForm)
    Panel1: TPanel;
    EsLabel: TEsLabel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    SchemeCb: TComboBox;
    SaveAsBtn: TButton;
    DeleteBtn: TButton;
    Panel3: TPanel;
    GraduateRg: TRadioGroup;
    ShadowRg: TRadioGroup;
    HighlightRg: TRadioGroup;
    FromColorCcb: TEsColorComboBox;
    HighlightColorCcb: TEsColorComboBox;
    ShadowColorCcb: TEsColorComboBox;
    HighlightDirectionLbl: TLabel;
    ShadowDirectionLbl: TLabel;
    FontColorCcb: TEsColorComboBox;
    Panel4: TPanel;
    FontSizeSb: TScrollBar;
    FontSizeLbl: TLabel;
    HighlightDepthLbl: TLabel;
    ShadowDepthLbl: TLabel;
    ShadowDepthSb: TScrollBar;
    HighlightDepthSb: TScrollBar;
    AppearanceCb: TComboBox;
    ColorSchemeCb: TComboBox;
    procedure FontSizeSbChange(Sender: TObject);
    procedure HighlightDepthSbChange(Sender: TObject);
    procedure ShadowDepthSbChange(Sender: TObject);
    procedure GraduateRgClick(Sender: TObject);
    procedure HighlightRgClick(Sender: TObject);
    procedure ShadowRgClick(Sender: TObject);
    procedure FromColorCcbChange(Sender: TObject);
    procedure HighlightColorCcbChange(Sender: TObject);
    procedure ShadowColorCcbChange(Sender: TObject);
    procedure FontColorCcbChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveAsBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure SchemeCbChange(Sender: TObject);
    procedure AppearanceCbChange(Sender: TObject);
    procedure ColorSchemeCbChange(Sender: TObject);
  private
  public
    HighlightDirectionDp : TEsDirectionPicker;
    ShadowDirectionDp : TEsDirectionPicker;
    SettingScheme : Boolean;
    SettingCb : Boolean;

    procedure DeleteScheme(const S : string);
    procedure HighlightDirectionChange(Sender: TObject);
    procedure ShadowDirectionChange(Sender: TObject);
    procedure SchemeChange;
  end;

type
  {component editor for the notebook pages}
  TEsLabelEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index : Integer);
      override;
    function GetVerb(Index : Integer) : AnsiString;
      override;
    function GetVerbCount : Integer;
      override;
  end;

  {property editor for the special settings class}
  TEsCustomSettingsProperty = class(TClassProperty)
  public
    procedure Edit;
      override;
    function GetAttributes : TPropertyAttributes;
      override;
  end;

function EditEsLabel(L : TEsLabel) : Boolean;


implementation

uses EsLabel1;

{$R *.DFM}

const
  IniFileName = 'ESSENCE.INI';

function EditEsLabel(L : TEsLabel) : Boolean;
var
  D : TEsLabelFrm;
begin
  Result := False;
  D := TEsLabelFrm.Create(Application);
  try
    D.EsLabel.Font.Assign(L.Font);
    D.EsLabel.CustomSettings.Assign(L.CustomSettings);

    D.FontSizeSb.Position := L.Font.Size;
    D.HighlightDepthSB.Position := L.CustomSettings.HighlightDepth;
    D.ShadowDepthSB.Position := L.CustomSettings.ShadowDepth;

    D.GraduateRg.ItemIndex := Ord(L.CustomSettings.GraduateStyle);
    D.HighlightRg.ItemIndex := Ord(L.CustomSettings.HighlightStyle);
    D.ShadowRg.ItemIndex := Ord(L.CustomSettings.ShadowStyle);

    D.FontColorCcb.SelectedColor := L.Font.Color;
    D.FromColorCcb.SelectedColor := L.CustomSettings.GraduateFromColor;
    D.HighlightColorCcb.SelectedColor := L.CustomSettings.HighlightColor;
    D.ShadowColorCcb.SelectedColor := L.CustomSettings.ShadowColor;

    D.HighlightDirectionDp.Direction := Ord(L.CustomSettings.HighlightDirection)-1;
    D.ShadowDirectionDp.Direction := Ord(L.CustomSettings.ShadowDirection)-1;

    if D.ShowModal = mrOK then begin
      L.CustomSettings.BeginUpdate;
      try
        L.Font.Assign(D.EsLabel.Font);
        L.CustomSettings.Assign(D.EsLabel.CustomSettings);
      finally
        L.CustomSettings.EndUpdate;
      end;
      Result := True;
    end;
  finally
    D.Free;
  end;
end;


{*** TEsLabelEditor ***}

procedure TEsLabelEditor.ExecuteVerb(Index : Integer);
begin
  case Index of
    0 : if EditEsLabel(TEsLabel(Component)) then
          Designer.Modified;
    1 : ;
    2 : ShellWebCall1;
    3 : ShellWebCall2;
    4 : ShellMailCall;
  end;
end;

function TEsLabelEditor.GetVerb(Index : Integer) : AnsiString;
begin
  case Index of
    0 : Result := 'Style Manager';
    1 : Result := '-';
    2 : Result := WebText1;
    3 : Result := WebText2;
    4 : Result := MailText;
  end;
end;

function TEsLabelEditor.GetVerbCount : Integer;
begin
  Result := 5;
end;


{*** TEsCustomSettingsProperty ***}

procedure TEsCustomSettingsProperty.Edit;
var
  I : Integer;
  C : TComponent;
  L : TEsLabel;
  M : TEsLabel;
begin
  C := TComponent(GetComponent(0));
  if C is TEsCustomLabel then begin
    L := TEsLabel(C);
    if EditEsLabel(L) then begin
      {if more than one component selected, apply changes to others}
      for I := 2 to PropCount do begin
        M := TEsLabel(GetComponent(Pred(I)));
        M.CustomSettings.BeginUpdate;
        try
          M.Font.Assign(L.Font);
          M.CustomSettings.Assign(L.CustomSettings);
        finally
          M.CustomSettings.EndUpdate;
        end;
        M.Invalidate;
      end;
      Designer.Modified;
    end;
  end;
end;

function TEsCustomSettingsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paMultiSelect, paDialog, paReadOnly]
end;


{*** TEsLabelFrm ***}

procedure TEsLabelFrm.FontSizeSbChange(Sender: TObject);
begin
  EsLabel.Font.Size := FontSizeSb.Position;
  FontSizeLbl.Caption := IntToStr(EsLabel.Font.Size);
  SchemeChange;
end;

procedure TEsLabelFrm.HighlightDepthSbChange(Sender: TObject);
begin
  EsLabel.CustomSettings.HighlightDepth := HighlightDepthSb.Position;
  HighlightDepthLbl.Caption := IntToStr(EsLabel.CustomSettings.HighlightDepth);
  SchemeChange;
end;

procedure TEsLabelFrm.ShadowDepthSbChange(Sender: TObject);
begin
  EsLabel.CustomSettings.ShadowDepth := ShadowDepthSb.Position;
  ShadowDepthLbl.Caption := IntToStr(EsLabel.CustomSettings.ShadowDepth);
  SchemeChange;
end;

procedure TEsLabelFrm.GraduateRgClick(Sender: TObject);
begin
  case GraduateRg.ItemIndex of
    0 : EsLabel.CustomSettings.GraduateStyle := gsNone;
    1 : EsLabel.CustomSettings.GraduateStyle := gsHorizontal;
    2 : EsLabel.CustomSettings.GraduateStyle := gsVertical;
  end;
  SchemeChange;
end;

procedure TEsLabelFrm.HighlightRgClick(Sender: TObject);
begin
  case HighlightRg.ItemIndex of
    0 : EsLabel.CustomSettings.HighlightStyle := ssPlain;
    1 : EsLabel.CustomSettings.HighlightStyle := ssExtrude;
    2 : EsLabel.CustomSettings.HighlightStyle := ssGraduated;
  end;
  SchemeChange;
end;

procedure TEsLabelFrm.ShadowRgClick(Sender: TObject);
begin
  case ShadowRg.ItemIndex of
    0 : EsLabel.CustomSettings.ShadowStyle := ssPlain;
    1 : EsLabel.CustomSettings.ShadowStyle := ssExtrude;
    2 : EsLabel.CustomSettings.ShadowStyle := ssGraduated;
  end;
  SchemeChange;
end;

procedure TEsLabelFrm.FromColorCcbChange(Sender: TObject);
begin
  EsLabel.CustomSettings.GraduateFromColor := FromColorCcb.SelectedColor;
  SchemeChange;
end;

procedure TEsLabelFrm.HighlightColorCcbChange(Sender: TObject);
begin
  EsLabel.CustomSettings.HighlightColor := HighlightColorCcb.SelectedColor;
  SchemeChange;
end;

procedure TEsLabelFrm.ShadowColorCcbChange(Sender: TObject);
begin
  EsLabel.CustomSettings.ShadowColor := ShadowColorCcb.SelectedColor;
  SchemeChange;
end;

procedure TEsLabelFrm.FontColorCcbChange(Sender: TObject);
begin
  EsLabel.Font.Color := FontColorCcb.SelectedColor;
  SchemeChange;
end;

procedure TEsLabelFrm.FormCreate(Sender: TObject);
var
  Ini : TIniFile;
  A   : TEsAppearance;
  C   : TEsColorScheme;
begin
  Top := (Screen.Height - Height) div 3;
  Left := (Screen.Width - Width) div 2;

  {load scheme names into combo box}
  Ini := TIniFile.Create('ESSENCE.INI');
  try
    SchemeCb.Items.Clear;
    Ini.ReadSection('Schemes', SchemeCb.Items);
  finally
    Ini.Free;
  end;

  {create direction pickers}
  HighlightDirectionDp := TEsDirectionPicker.Create(Self);
  HighlightDirectionDp.Top := HighlightDirectionLbl.Top;
  HighlightDirectionDp.Left := HighlightDirectionLbl.Left + HighlightDirectionLbl.Width;
  HighlightDirectionDp.Width := 50;
  HighlightDirectionDp.Height := 50;
  HighlightDirectionDp.NumDirections := 8;
  HighlightDirectionDp.OnChange := HighlightDirectionChange;
  HighlightDirectionDp.Parent := HighlightDirectionLbl.Parent;

  ShadowDirectionDp := TEsDirectionPicker.Create(Self);
  ShadowDirectionDp.Top := ShadowDirectionLbl.Top;
  ShadowDirectionDp.Left := ShadowDirectionLbl.Left + ShadowDirectionLbl.Width;
  ShadowDirectionDp.Width := 50;
  ShadowDirectionDp.Height := 50;
  ShadowDirectionDp.NumDirections := 8;
  ShadowDirectionDp.OnChange := ShadowDirectionChange;
  ShadowDirectionDp.Parent := ShadowDirectionLbl.Parent;

  {initialize appearance and color scheme comboboxes using rtti}
  {$IFDEF WIN32}
  for A := Low(TEsAppearance) to High(TEsAppearance) do
    AppearanceCb.Items.Add(GetEnumName(TypeInfo(TEsAppearance), Ord(A)));
  for C := Low(TEsColorScheme) to High(TEsColorScheme) do
    ColorSchemeCb.Items.Add(GetEnumName(TypeInfo(TEsColorScheme), Ord(C)));
  {$ELSE}
  for A := Low(TEsAppearance) to High(TEsAppearance) do
    AppearanceCb.Items.Add(GetEnumName(TypeInfo(TEsAppearance), Ord(A))^);
  for C := Low(TEsColorScheme) to High(TEsColorScheme) do
    ColorSchemeCb.Items.Add(GetEnumName(TypeInfo(TEsColorScheme), Ord(C))^);
  {$ENDIF WIN32}
end;

procedure TEsLabelFrm.SaveAsBtnClick(Sender: TObject);
var
  Ini : TIniFile;
  S   : string;
begin
  with TSaveSchemeFrm.Create(Self) do begin
    if (ShowModal = mrOK) and (SchemeNameEd.Text > '') then begin
      S := SchemeNameEd.Text;
      Ini := TIniFile.Create('ESSENCE.INI');
      try
        {delete scheme}
        DeleteScheme(S);

        {add scheme name to list of schemes}
        Ini.WriteInteger('Schemes', S, 0);

        {create new scheme section and add values}
        Ini.WriteInteger(S, 'GraduateStyle', Ord(EsLabel.CustomSettings.GraduateStyle));
        Ini.WriteInteger(S, 'HighlightStyle', Ord(EsLabel.CustomSettings.HighlightStyle));
        Ini.WriteInteger(S, 'ShadowStyle', Ord(EsLabel.CustomSettings.ShadowStyle));
        Ini.WriteString(S, 'GraduateFromColor', ColorToString(EsLabel.CustomSettings.GraduateFromColor));
        Ini.WriteString(S, 'HighlightColor', ColorToString(EsLabel.CustomSettings.HighlightColor));
        Ini.WriteString(S, 'ShadowColor', ColorToString(EsLabel.CustomSettings.ShadowColor));
        Ini.WriteInteger(S, 'HighlightDirection', Ord(EsLabel.CustomSettings.HighlightDirection));
        Ini.WriteInteger(S, 'ShadowDirection', Ord(EsLabel.CustomSettings.ShadowDirection));
        Ini.WriteString(S, 'FontColor', ColorToString(EsLabel.Font.Color));
        Ini.WriteString(S, 'FontName', EsLabel.Font.Name);
        Ini.WriteInteger(S, 'FontPitch', Ord(EsLabel.Font.Pitch));
        Ini.WriteInteger(S, 'FontSize', EsLabel.Font.Size);
        Ini.WriteBool(S, 'FontBold', fsBold in EsLabel.Font.Style);
        Ini.WriteBool(S, 'FontItalic', fsItalic in EsLabel.Font.Style);
        Ini.WriteBool(S, 'FontUnderline', fsUnderline in EsLabel.Font.Style);
        Ini.WriteBool(S, 'FontStrikeOut', fsStrikeOut in EsLabel.Font.Style);
        Ini.WriteInteger(S, 'HighlightDepth', EsLabel.CustomSettings.HighlightDepth);
        Ini.WriteInteger(S, 'ShadowDepth', EsLabel.CustomSettings.ShadowDepth);
      finally
        Ini.Free;
      end;

      {add item to the combobox, if its not there already}
      if SchemeCb.Items.IndexOf(S) < 0 then
        SchemeCb.Items.Add(S);
    end;
    Free;
  end;
end;

procedure TEsLabelFrm.DeleteBtnClick(Sender: TObject);
var
  I   : Integer;
begin
  I := SchemeCb.ItemIndex;
  if I > -1 then begin
    DeleteScheme(SchemeCb.Items[I]);
    {delete the entry from the combo box}
    SchemeCb.Items.Delete(I);
  end;
end;

procedure TEsLabelFrm.DeleteScheme(const S : string);
var
  Ini : TIniFile;
  {$IFNDEF Win32}
  Buf : array[0..255] of Char;
  {$ENDIF}
begin
  {delete the scheme entry from the ini file}
  Ini := TIniFile.Create(IniFileName);
  try
    {delete the section}
    Ini.EraseSection(S);
    {delete the scheme name}
    {$IFDEF Win32}
    Ini.DeleteKey('Schemes', S);
    {$ELSE}
    StrPLCopy(Buf, S, SizeOf(Buf)-1);
    WritePrivateProfileString('Schemes', Buf, nil, IniFileName);
    {$ENDIF}
  finally
    Ini.Free;
  end;
end;

procedure TEsLabelFrm.SchemeCbChange(Sender: TObject);
var
  I   : Integer;
  Ini : TIniFile;
  S   : string;
begin
  I := SchemeCb.ItemIndex;
  if I > -1 then begin
    S := SchemeCb.Items[I];
    Ini := TIniFile.Create('ESSENCE.INI');
    SettingScheme := True;
    try
      EsLabel.CustomSettings.GraduateStyle := TEsGraduateStyle(Ini.ReadInteger(S, 'GraduateStyle', 0));
      EsLabel.CustomSettings.HighlightStyle := TEsShadeStyle(Ini.ReadInteger(S, 'HighlightStyle', 0));
      EsLabel.CustomSettings.ShadowStyle := TEsShadeStyle(Ini.ReadInteger(S, 'ShadowStyle', 0));
      EsLabel.CustomSettings.GraduateFromColor := StringToColor(Ini.ReadString(S, 'GraduateFromColor', '0'));
      EsLabel.CustomSettings.HighlightColor := StringToColor(Ini.ReadString(S, 'HighlightColor', '0'));
      EsLabel.CustomSettings.ShadowColor := StringToColor(Ini.ReadString(S, 'ShadowColor', '0'));
      EsLabel.CustomSettings.HighlightDirection := TEsShadeDirection(Ini.ReadInteger(S, 'HighlightDirection', 0));
      EsLabel.CustomSettings.ShadowDirection := TEsShadeDirection(Ini.ReadInteger(S, 'ShadowDirection', 0));
      EsLabel.CustomSettings.HighlightDepth := Ini.ReadInteger(S, 'HighlightDepth', 1);
      EsLabel.CustomSettings.ShadowDepth := Ini.ReadInteger(S, 'ShadowDepth', 1);

      EsLabel.Font.Color := StringToColor(Ini.ReadString(S, 'FontColor', '0'));
      EsLabel.Font.Name := Ini.ReadString(S, 'FontName', 'Times New Roman');
      EsLabel.Font.Pitch := TFontPitch(Ini.ReadInteger(S, 'FontPitch', 0));
      EsLabel.Font.Size := Ini.ReadInteger(S, 'FontSize', 10);
      EsLabel.Font.Style := [];
      if Ini.ReadBool(S, 'FontBold', False) then
        EsLabel.Font.Style := EsLabel.Font.Style + [fsBold];
      if Ini.ReadBool(S, 'FontItalic', False) then
        EsLabel.Font.Style := EsLabel.Font.Style + [fsItalic];
      if Ini.ReadBool(S, 'FontUnderline', False) then
        EsLabel.Font.Style := EsLabel.Font.Style + [fsUnderline];
      if Ini.ReadBool(S, 'FontStrikeOut', False) then
        EsLabel.Font.Style := EsLabel.Font.Style + [fsStrikeOut];

      FontSizeSb.Position := EsLabel.Font.Size;
      HighlightDepthSB.Position := EsLabel.CustomSettings.HighlightDepth;
      ShadowDepthSB.Position := EsLabel.CustomSettings.ShadowDepth;
      GraduateRg.ItemIndex := Ord(EsLabel.CustomSettings.GraduateStyle);
      HighlightRg.ItemIndex := Ord(EsLabel.CustomSettings.HighlightStyle);
      ShadowRg.ItemIndex := Ord(EsLabel.CustomSettings.ShadowStyle);
      FontColorCcb.SelectedColor := EsLabel.Font.Color;
      FromColorCcb.SelectedColor := EsLabel.CustomSettings.GraduateFromColor;
      HighlightColorCcb.SelectedColor := EsLabel.CustomSettings.HighlightColor;
      ShadowColorCcb.SelectedColor := EsLabel.CustomSettings.ShadowColor;
      HighlightDirectionDp.Direction := Ord(EsLabel.CustomSettings.HighlightDirection)-1;
      ShadowDirectionDp.Direction := Ord(EsLabel.CustomSettings.ShadowDirection)-1;
    finally
      SettingScheme := False;
      Ini.Free;
    end;
  end;
end;

procedure TEsLabelFrm.HighlightDirectionChange(Sender: TObject);
begin
  EsLabel.CustomSettings.HighlightDirection :=
    TEsShadeDirection(HighlightDirectionDp.Direction+1);
  SchemeChange;
end;

procedure TEsLabelFrm.ShadowDirectionChange(Sender: TObject);
begin
  EsLabel.CustomSettings.ShadowDirection :=
    TEsShadeDirection(ShadowDirectionDp.Direction+1);
  SchemeChange;
end;

procedure TEsLabelFrm.SchemeChange;
begin
  if not SettingScheme then
    SchemeCb.ItemIndex := -1;

  if not SettingCb then begin
    AppearanceCb.ItemIndex := -1;
    ColorSchemeCb.ItemIndex := -1;
  end;
end;


procedure TEsLabelFrm.AppearanceCbChange(Sender: TObject);
begin
  if AppearanceCb.ItemIndex > -1 then begin
    SettingScheme := True;
    SettingCb := True;
    try
      EsLabel.Appearance := TEsAppearance(AppearanceCb.ItemIndex);

      FontSizeSb.Position := EsLabel.Font.Size;
      HighlightDepthSB.Position := EsLabel.CustomSettings.HighlightDepth;
      ShadowDepthSB.Position := EsLabel.CustomSettings.ShadowDepth;
      GraduateRg.ItemIndex := Ord(EsLabel.CustomSettings.GraduateStyle);
      HighlightRg.ItemIndex := Ord(EsLabel.CustomSettings.HighlightStyle);
      ShadowRg.ItemIndex := Ord(EsLabel.CustomSettings.ShadowStyle);
      FontColorCcb.SelectedColor := EsLabel.Font.Color;
      FromColorCcb.SelectedColor := EsLabel.CustomSettings.GraduateFromColor;
      HighlightColorCcb.SelectedColor := EsLabel.CustomSettings.HighlightColor;
      ShadowColorCcb.SelectedColor := EsLabel.CustomSettings.ShadowColor;
      HighlightDirectionDp.Direction := Ord(EsLabel.CustomSettings.HighlightDirection)-1;
      ShadowDirectionDp.Direction := Ord(EsLabel.CustomSettings.ShadowDirection)-1;
    finally
      SettingCb := False;
      SettingScheme := False;
    end;
  end;
end;

procedure TEsLabelFrm.ColorSchemeCbChange(Sender: TObject);
begin
  if ColorSchemeCb.ItemIndex > -1 then begin
    SettingScheme := True;
    SettingCb := True;
    try
      EsLabel.ColorScheme := TEsColorScheme(ColorSchemeCb.ItemIndex);

      FontSizeSb.Position := EsLabel.Font.Size;
      HighlightDepthSB.Position := EsLabel.CustomSettings.HighlightDepth;
      ShadowDepthSB.Position := EsLabel.CustomSettings.ShadowDepth;
      GraduateRg.ItemIndex := Ord(EsLabel.CustomSettings.GraduateStyle);
      HighlightRg.ItemIndex := Ord(EsLabel.CustomSettings.HighlightStyle);
      ShadowRg.ItemIndex := Ord(EsLabel.CustomSettings.ShadowStyle);
      FontColorCcb.SelectedColor := EsLabel.Font.Color;
      FromColorCcb.SelectedColor := EsLabel.CustomSettings.GraduateFromColor;
      HighlightColorCcb.SelectedColor := EsLabel.CustomSettings.HighlightColor;
      ShadowColorCcb.SelectedColor := EsLabel.CustomSettings.ShadowColor;
      HighlightDirectionDp.Direction := Ord(EsLabel.CustomSettings.HighlightDirection)-1;
      ShadowDirectionDp.Direction := Ord(EsLabel.CustomSettings.ShadowDirection)-1;
    finally
      SettingCb := False;
      SettingScheme := False;
    end;
  end;
end;

end.
