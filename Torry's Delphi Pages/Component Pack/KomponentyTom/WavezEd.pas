{
Copyright © 1998 by Delphi 4 Developer's Guide - Xavier Pacheco and Steve Teixeira
}
unit WavezEd;

interface

uses DesignEditors, DesignIntf;

type
  { Property editor for TWaveFile's WaveName property }
  TWaveFileStringProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { Component editor for TddgWaveFile.  Allows user to play and stop }
  { WAV sounds from local menu in IDE. }
  TWaveEditor = class(TComponentEditor)
  private
    procedure EditProp(PropertyEditor: TPropertyEditor);
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses TypInfo, Wavez, Classes, Controls, Dialogs;

const
  VerbCount = 2;
  VerbArray: array[0..VerbCount - 1] of string[7] = ('Play', 'Stop');

{ TWaveFileStringProperty }

procedure TWaveFileStringProperty.Edit;
{ Executed when user clicks the ellipses button on the WavName   }
{ property in the Object Inspector.  This method allows the user }
{ to pick a file from an OpenDialog and sets the property value. }
begin
  with TOpenDialog.Create(nil) do
    try
      { Set up properties for dialog }
      Filter := 'Wav files|*.wav|All files|*.*';
      DefaultExt := '*.wav';
      { Put current value in the FileName property of dialog }
      FileName := GetStrValue;
      { Execute dialog and set property value if dialog is OK }
      if Execute then
        SetStrValue(FileName);
    finally
      Free;
    end;
end;

function TWaveFileStringProperty.GetAttributes: TPropertyAttributes;
{ Indicates the property editor will invoke a dialog. }
begin
  Result := [paDialog];
end;

{ TWaveEditor }

procedure TWaveEditor.Edit;
{ Called when user double-clicks on the component at design time. }
{ This method calls the GetComponentProperties method in order to }
{ invoke the Edit method of the WaveName property editor. }
var
  Components: TComponentList;
begin
  Components := TComponentList.Create;
  try
    Components.Add(Component);
    GetComponentProperties(Components, tkAny, Designer, EditProp);
  finally
    Components.Free;
  end;
end;

procedure TWaveEditor.EditProp(PropertyEditor: TPropertyEditor);
{ Called once per property in response to GetComponentProperties }
{ call.  This method looks for the WaveName property editor and  }
{ calls its Edit method. }
begin
  if PropertyEditor is TWaveFileStringProperty then begin
    TWaveFileStringProperty(PropertyEditor).Edit;
    Designer.Modified;    // alert Designer to modification
  end;
end;

procedure TWaveEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: TddgWaveFile(Component).Play;
    1: TddgWaveFile(Component).Stop;
  end;
end;

function TWaveEditor.GetVerb(Index: Integer): string;
begin
  Result := VerbArray[Index];
end;

function TWaveEditor.GetVerbCount: Integer;
begin
  Result := VerbCount;
end;

end.
