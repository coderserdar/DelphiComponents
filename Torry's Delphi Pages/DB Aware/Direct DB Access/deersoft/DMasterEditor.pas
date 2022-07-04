unit DMasterEditor;

interface

uses
  Windows, Classes, Messages, Dialogs,
  {$IFDEF MSWINDOWS}
  DesignIntf,
  DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  DMaster;

type
  TDMasterConnectionProperty = class (TStringProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure Edit; override;

  end;

type
  TDMasterEditor = class(TComponentEditor)
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): String; override;
    procedure ExecuteVerb(Index: Integer); override;

  end;

procedure Register;

implementation

procedure Register;
begin
     RegisterPropertyEditor(TypeInfo(String), TDMaster, 'Connection', TDMasterConnectionProperty);
     RegisterComponentEditor(TDMaster, TDMasterEditor);
end;

function TDMasterConnectionProperty.GetAttributes:TPropertyAttributes;
begin
     Result := [paDialog];
end;


procedure TDMasterConnectionProperty.Edit;
begin
     SetStrValue(TDMaster(GetComponent(0)).Editor);
     Modified;
end;


function TDMasterEditor.GetVerbCount: Integer;
begin
     Result := 1;
end;


function TDMasterEditor.GetVerb(Index: Integer): String;
begin
     Result := 'Editor';
end;


procedure TDMasterEditor.ExecuteVerb(Index: Integer);
begin
     TDMaster(Component).Connection := TDMaster(Component).Editor;
     Designer.Modified;
end;

end.
