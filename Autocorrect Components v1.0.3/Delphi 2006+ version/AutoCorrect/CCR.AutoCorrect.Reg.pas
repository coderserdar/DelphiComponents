{**************************************************************************************}
{                                                                                      }
{ AutoCorrect components                                                               }
{ Version 1.0.3 (2009-09-23)                                                           }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is CCR.AutoCorrect.Reg.pas.                                        }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009 Chris Rolliston. All Rights Reserved.         }
{                                                                                      }
{**************************************************************************************}

unit CCR.AutoCorrect.Reg;

interface

procedure Register;

implementation

uses SysUtils, Classes, Graphics, DesignIntf, DesignEditors, VCLEditors,
  CCR.AutoCorrect, CCR.AutoCorrect.Controls;

{$R TAutoCorrectEngine.dcr}

type
  TFontNameToSetProperty = class(TFontNameProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TFontSizeToSetProperty = class(TIntegerProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TAutoCorrectEngineEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;
begin
  RegisterComponents('CCR AutoCorrect', [TAutoCorrectEngine, TComboBoxWithAutoCorrect,
    TEditWithAutoCorrect, TMemoWithAutoCorrect, TRichEditWithAutoCorrect]);
  RegisterPropertyEditor(TypeInfo(TFontName), TAutoCorrectEntry, 'FontNameToSet', TFontNameToSetProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TAutoCorrectEntry, 'FontSizeToSet', TFontSizeToSetProperty);
  RegisterComponentEditor(TAutoCorrectEngine, TAutoCorrectEngineEditor);
end;

const
  SNone = '(None)';

{ TFontNameToSetProperty }

function TFontNameToSetProperty.GetValue: string;
begin
  Result := inherited GetValue;
  if Result = '' then Result := SNone;
end;

procedure TFontNameToSetProperty.SetValue(const Value: string);
begin
  if SameText(Value, SNone) then
    inherited SetValue('')
  else
    inherited;
end;

{ TFontSizeToSetProperty }

function TFontSizeToSetProperty.GetValue: string;
begin
  Result := inherited GetValue;
  if Result = '0' then Result := SNone;
end;

procedure TFontSizeToSetProperty.SetValue(const Value: string);
begin
  if SameText(Value, SNone) or (Value = '') then
    inherited SetValue('0')
  else
    inherited;
end;

{ TAutoCorrectEngineEditor }

procedure TAutoCorrectEngineEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
begin
  Continue := Prop.GetName <> 'CustomEntries';
  if not Continue then Prop.Edit;
end;

procedure TAutoCorrectEngineEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

function TAutoCorrectEngineEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Custom Entries...';
end;

function TAutoCorrectEngineEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
