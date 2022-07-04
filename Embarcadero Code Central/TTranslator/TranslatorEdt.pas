{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: TranslatorEdt.pas,v 1.21 2003/04/16 15:19:30 laa Exp $ }

unit TranslatorEdt;

interface
{$i common.inc}

uses
{$ifdef D6_OR_HIGHER}
  DesignIntf, DesignEditors, DesignMenus,
{$else}
  DsgnIntf,
{$endif D6_OR_HIGHER}
  Classes, Menus, Translator;

type
  TTranslatorLanguageProperty = class(TStringProperty)
  private
    function GetTranslatorClient : TTranslatorClient;
  public
    property TranslatorClient : TTranslatorClient read GetTranslatorClient;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TStringsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TAboutProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TServerProperty = class(TComponentProperty)
  private
    function GetTranslatorClient : TTranslatorClient;
  public
    property TranslatorClient : TTranslatorClient read GetTranslatorClient;
    procedure SetValue(const Value: string); override;
  end;

  TTranslatorClientEditor = class(TDefaultEditor)
  private
    procedure ShowAbout(Sender : TObject);
    procedure SetLanguage(Sender : TObject);
    procedure EditStrings(Sender : TObject);
  protected
{$ifdef D6_OR_HIGHER}
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
{$else}
    procedure EditProperty(PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean); override;
{$endif D6_OR_HIGHER}
  public
{$ifdef D4_OR_HIGHER}
{$ifdef D6_OR_HIGHER}
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
{$else}
    procedure PrepareItem(Index: Integer; const AItem: TMenuItem); override; // FIXA!!!
{$endif D6_OR_HIGHER}
{$endif D4_OR_HIGHER}
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  procedure ShowTranslatorEditor(Client : TTranslatorClient);
  procedure Register;

implementation

uses
{$ifdef D6_OR_HIGHER}
  SysUtils,
{$endif D6_OR_HIGHER}
  Dialogs, TranslatorEditor, AboutTranslator, Forms;

procedure ShowAboutDlg;
var
  Dlg : TdlgAboutTranslator;
begin
  Application.CreateForm(TdlgAboutTranslator, Dlg);
  Dlg.Show(TTranslatorClient.TranslatorVersion);
end;

function GetEditorWindow(Strings : IEditableTranslatedStrings) : TdlgStringsEditor;
begin
  Result := TdlgStringsEditor(Strings.EditorWindow);
end;

procedure ShowTranslatorEditor(Client : TTranslatorClient);
var
  Editor: TdlgStringsEditor;
  FCaption : String;
begin
  if Client.Server = nil then
  begin
    ShowMessage('Select Server first!');
    Exit;
  end;

  Editor := GetEditorWindow(Client.Strings);
  if Editor = nil then
  begin
    Application.CreateForm(TdlgStringsEditor, Editor);

    Editor.Strings := Client.Strings;
    FCaption := 'Editing ';
    if Client.Server.Owner <> nil then
      FCaption := FCaption + Client.Server.Owner.Name + '.';
    FCaption := FCaption + Client.Server.Name;
    Editor.Caption := FCaption;
  end;

  Editor.ShowClient(Client);
end;

procedure RefreshTranslatorEditor(Client : TTranslatorClient);
var
  Editor: TdlgStringsEditor;
begin
  if Assigned(Client.Server) then
  begin
    Editor := GetEditorWindow(Client.Server.Strings);
    if Assigned(Editor) then
      Editor.RefreshEditor;
  end;
end;

// ------------------------- TTranslatorLanguageProperty -----------------------

function TTranslatorLanguageProperty.GetTranslatorClient: TTranslatorClient;
begin
  Result := TTranslatorClient(GetComponent(0));
end;

function TTranslatorLanguageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TTranslatorLanguageProperty.GetValues(Proc: TGetStrProc);
var
  FTranslator : TTranslatorClient;
  i : Integer;
begin
  FTranslator := TranslatorClient;
  if FTranslator.Server <> nil then
    for i := 0 to FTranslator.Server.Strings.LanguageCount - 1 do
      Proc(FTranslator.Server.Strings.Languages[i]);
end;

function TTranslatorLanguageProperty.GetValue: string;
begin
  Result := TranslatorClient.Language;
end;

procedure TTranslatorLanguageProperty.SetValue(const Value: string);
begin
  TranslatorClient.Language := Value;
end;

{ TStringsProperty }

procedure TStringsProperty.Edit;
var
  Client : TTranslatorClient;
begin
  Client := TTranslatorClient(GetComponent(0));
  ShowTranslatorEditor(Client);
end;

function TStringsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{ TAboutProperty }

procedure TAboutProperty.Edit;
begin
  ShowAboutDlg;
end;

function TAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{ TServerProperty }

function TServerProperty.GetTranslatorClient : TTranslatorClient;
begin
  Result := TTranslatorClient(GetComponent(0));
end;

procedure TServerProperty.SetValue(const Value: string);
begin
  inherited SetValue(Value);
  RefreshTranslatorEditor(TranslatorClient);
end;

{ TTranslatorClientEditor }

procedure TTranslatorClientEditor.ShowAbout(Sender : TObject);
begin
  ShowAboutDlg;
end;

procedure TTranslatorClientEditor.SetLanguage(Sender : TObject);
var
  NewIndex : integer;
begin
  with Self.Component as TTranslatorClient do
  begin
    if Server = nil then
      Exit;

{$ifdef D6_OR_HIGHER}
    NewIndex := StrToInt( System.Copy(TMenuItem(Sender).Name,2,Length(TMenuItem(Sender).Name)) );
{$else}
    NewIndex := TMenuItem(Sender).Tag;
{$endif D6_OR_HIGHER}
    if Language <> Server.Strings.Languages[NewIndex] then
    begin
      Language := Server.Strings.Languages[NewIndex];
      Designer.Modified;
    end;
  end;
end;

function TTranslatorClientEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Edit Strings...'
  else if Index = 1 then
    Result := 'Language'
  else if Index = 2 then
    Result := 'About Translator...'
  else
    Result := '';
end;

{$ifdef D4_OR_HIGHER}
// FIXA!!! -- denna hittades inte under Delphi3
{$ifdef D6_OR_HIGHER}
procedure TTranslatorClientEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
{$else}
procedure TTranslatorClientEditor.PrepareItem(Index: Integer; const AItem: TMenuItem);
{$endif D6_OR_HIGHER}
var
  i : Integer;
{$ifndef D6_OR_HIGHER}
  FMenuItem : TMenuItem;
{$endif D6_OR_HIGHER}
begin
  if Index = 1 then
  begin
    with Self.Component as TTranslatorClient do
    begin
      if Server <> nil then
        for i := 0 to Server.Strings.LanguageCount - 1 do
        begin
    {$ifdef D6_OR_HIGHER}
          AItem.AddItem( Server.Strings.Languages[i], scNone, (Server.Strings.Languages[i] = Language), True, SetLanguage, 0, 'T' + IntToStr(i) );
    {$else}
          FMenuItem := TMenuItem.Create(AItem);
          FMenuItem.Caption := Server.Strings.Languages[i];
          FMenuItem.Tag := i;
          FMenuItem.RadioItem := True;
          FMenuItem.Checked := FMenuItem.Caption = Language;
          FMenuItem.OnClick := SetLanguage;
          AItem.Add(FMenuItem);
    {$endif D6_OR_HIGHER}
        end;
    end;
  end;
end;
{$endif D4_OR_HIGHER}

{$ifdef D6_OR_HIGHER}
procedure TTranslatorClientEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
{$else}
procedure TTranslatorClientEditor.EditProperty(PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean);
{$endif D6_OR_HIGHER}
begin
{$ifndef D6_OR_HIGHER}
  FreeEditor := True;
{$endif D6_OR_HIGHER}
  Continue := False;

{$ifndef MAB}
  ShowAbout(nil);
{$else MAB}
  ShowTranslatorEditor(Self.Component as TTranslatorClient);
{$endif MAB}
end;

function TTranslatorClientEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

procedure TTranslatorClientEditor.ExecuteVerb(Index: Integer);
{var
  AOut : IDesignerNotify;
  ATrans : TTranslator;
  i : Integer; }
begin
  case Index of
    0: EditStrings(nil);
    1:; // Sub Menu
    2: ShowAbout(nil);
  end;
end;

procedure TTranslatorClientEditor.EditStrings(Sender : TObject);
begin
  if (Component is TTranslatorClient) then
    ShowTranslatorEditor(TTranslatorClient(Component));
end;

procedure Register;
begin
  {Property Editors}
  RegisterPropertyEditor(TypeInfo(TLanguage), TTranslatorClient, 'Language', TTranslatorLanguageProperty);
  RegisterPropertyEditor(TypeInfo(TTranslatedStrings), TTranslatorClient, '', TStringsProperty);
  RegisterPropertyEditor(TypeInfo(TAboutTranslator), TTranslatorClient, '', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(TTranslator), TTranslatorClient, 'Server', TServerProperty);

  {Component Editors}
  RegisterComponentEditor(TTranslatorClient, TTranslatorClientEditor);
end;

end.

