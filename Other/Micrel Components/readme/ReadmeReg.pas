unit ReadmeReg;

{$I jbSupp.inc}

interface

procedure Register;

implementation

uses Classes, SysUtils, Controls, TypInfo, Consts, Readme,
  {$IFDEF VER6UP} RTLConsts, DesignIntf, DesignEditors {$ELSE} DsgnIntf {$ENDIF};

type
  TReadmeEditor = class(TDefaultEditor)
  public
{$IFDEF VER6UP}
    procedure EditProperty(const PropertyEditor: IProperty;
      var Continue: Boolean); override;
{$ELSE}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
{$ENDIF}
  end;
  
{ TStringsEditor }
  
{$IFDEF VER6UP}   
procedure TReadmeEditor.EditProperty(const PropertyEditor: IProperty;
  var Continue: Boolean);
{$ELSE}
procedure TReadmeEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'Readme') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

{ Register }

procedure Register;
begin
  { Register component }
  RegisterComponents('Support',[TReadme]);
  RegisterComponentEditor(TReadme, TReadmeEditor);
end;

end.