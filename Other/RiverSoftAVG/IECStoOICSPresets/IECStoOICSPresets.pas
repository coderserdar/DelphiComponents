unit IECStoOICSPresets;
//=== File Prolog ============================================================
//	This code was developed by RiverSoftAVG.
//
//--- Notes ------------------------------------------------------------------
//
//--- Development History  ---------------------------------------------------
//
//      10/2003 T. Grubb
//		          Initial version.
//
//      File Contents:
//              Presets for TInferenceEngine properties
//
//
//--- Warning ----------------------------------------------------------------
//  This software is property of RiverSoftAVG. Unauthorized use or
//  duplication of this software is strictly prohibited. Authorized users
//  are subject to the following restrictions:
//	*	RiverSoftAVG is not responsible for
//		any consequence of the use of this software.
//	*	The origin of this software must not be misrepresented either by
//		explicit claim or by omission.
//	*	Altered versions of this software must be plainly marked as such.
//	*	This notice may not be removed or altered.
//
//      (c) 2003, Thomas G. Grubb
//
//=== End File Prolog ========================================================

interface

uses
  SysUtils, Classes, Controls, RSObjectInspector, RSPropertyPresets;

type
  TIECStoOICSPresets = class(TComponent)
  { Purpose: to ensure that the Object Inspector Component Suite presets for
    working with the Inference Engine Component Suite are properly registered
    for using them at design-time.  By dropping this component on your form,
    you ensure the initialization section is executed and the presets are
    available at design-time.  This component is not needed if you just
    need the presets at run-time. }
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end; { TIECStoOICSPresets }

  TRSIECSDlgPropertyPreset = class(TRSPropertyPreset)
  { Purpose: To provide a base preset class for IECS properties that require
    one of the dialogs }
  private
    { Private declarations }
    FCaption: TCaption;
  protected
    { Protected declarations }
    procedure EditButtonClick( Sender: TObject ); virtual; abstract;
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
  published
    { Published declarations }
    property Caption: TCaption read FCaption write FCaption;
  end; { TRSIECSDlgPropertyPreset }

  TRSTIEDefinedTypesDlgPropertyPreset = class(TRSIECSDlgPropertyPreset)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure EditButtonClick( Sender: TObject ); override;
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
  published
    { Published declarations }
  end; { TRSTIEDefinedTypesDlgPropertyPreset }

  TRSTFactsDlgPropertyPreset = class(TRSIECSDlgPropertyPreset)
  private
    { Private declarations }
    FEnableFactTemplateEditor: Boolean;
    FEnableTypeEditor: Boolean;
  protected
    { Protected declarations }
    procedure EditButtonClick( Sender: TObject ); override;
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
  published
    { Published declarations }
    property EnableFactTemplateEditor: Boolean read FEnableFactTemplateEditor write FEnableFactTemplateEditor default True;
    property EnableTypeEditor: Boolean read FEnableTypeEditor write FEnableTypeEditor default True;
  end; { TRSTFactsDlgPropertyPreset }

  TRSTFactSetFactsDlgPropertyPreset = class(TRSTFactsDlgPropertyPreset)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure EditButtonClick( Sender: TObject ); override;
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
  published
    { Published declarations }
  end; { TRSTFactSetFactsDlgPropertyPreset }

  TRSTFactTemplatesDlgPropertyPreset = class(TRSIECSDlgPropertyPreset)
  private
    { Private declarations }
    FEnableTypeEditor: Boolean;
  protected
    { Protected declarations }
    procedure EditButtonClick( Sender: TObject ); override;
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
  published
    { Published declarations }
    property EnableTypeEditor: Boolean read FEnableTypeEditor write FEnableTypeEditor default True;
  end; { TRSTFactTemplatesDlgPropertyPreset }

  TRSTUserFunctionListDlgPropertyPreset = class(TRSIECSDlgPropertyPreset)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure EditButtonClick( Sender: TObject ); override;
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
  published
    { Published declarations }
  end; { TRSTUserFunctionListDlgPropertyPreset }

  TRSEngineDDPropertyPreset = class(TRSTComponentPropertyPreset)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure GetPickList(Sender: TObject; const PropName: string; Values: TStrings);
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
  published
    { Published declarations }
  end; { TRSEngineDDPropertyPreset }

  TRSTRulesDlgPropertyPreset = class(TRSIECSDlgPropertyPreset)
  private
    { Private declarations }
    FEnableTypeEditor: Boolean;
    FEnableFactTemplateEditor: Boolean;
  protected
    { Protected declarations }
    procedure EditButtonClick( Sender: TObject ); override;
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
  published
    { Published declarations }
    property EnableFactTemplateEditor: Boolean read FEnableFactTemplateEditor write FEnableFactTemplateEditor default True;
    property EnableTypeEditor: Boolean read FEnableTypeEditor write FEnableTypeEditor default True;
  end; { TRSTRulesDlgPropertyPreset }

procedure Register;

implementation

uses
    IEDialogs, InferenceEngine, Grids, TypInfo, Forms, IEFacts, IEPackages;

procedure Register;
begin
  RegisterComponents('AI', [TIECStoOICSPresets]);
end;

{ TRSIECSDlgPropertyPreset }

constructor TRSIECSDlgPropertyPreset.Create(Collection: TCollection);
begin
     inherited Create(Collection);
     EditStyle := esEllipsis;
     TypeKinds := [tkClass];
     OnEditButtonClick := EditButtonClick;
end;

{ TRSTIEDefinedTypesDlgPropertyPreset }

constructor TRSTIEDefinedTypesDlgPropertyPreset.Create(
  Collection: TCollection);
begin
     inherited Create(Collection);
     Caption := 'Edit Defined Types...';
     Name := 'TIEDefinedTypes Dialog Property Editor';
     TypeNames.Add( 'TIEDefinedTypes' );
end;

procedure TRSTIEDefinedTypesDlgPropertyPreset.EditButtonClick(
  Sender: TObject);
var
   DefTypes: TIEDefinedTypes;
   Dialog: TDefinedTypesDialog;
begin
     // Get the TIEDefinedTypes class
     DefTypes := (Sender as TRSItemProperty).GetObjectProperty as TIEDefinedTypes;
     // Create the DefinedTypes dialog
     Dialog := TDefinedTypesDialog.Create(Application);
     try
        // set the engine to the TIEDefinedTypes.Engine property as there may be
        // more than one inference engine on the form
        Dialog.Engine := DefTypes.Engine;
        Dialog.Caption := Caption;
        // execute the dialog... The defined types dialog ensures that the DefinedTypes
        // property is updated if the user clicks ok
        Dialog.Execute;
     finally
        Dialog.Free;
     end;
end;

{ TRSTFactsDlgPropertyPreset }

constructor TRSTFactsDlgPropertyPreset.Create(Collection: TCollection);
begin
     inherited Create(Collection);
     Name := 'TFacts Dialog Property Editor';
     Caption := 'Edit Facts...';
     TypeNames.Add( 'TFacts' );
     EnableFactTemplateEditor := True;
     EnableTypeEditor := True;
end;

procedure TRSTFactsDlgPropertyPreset.EditButtonClick(Sender: TObject);
var
   Facts: TFacts;
   Dialog: TFactsDialog;
begin
     // Get the TFacts class
     Facts := (Sender as TRSItemProperty).GetObjectProperty as TFacts;
     // Create the Facts dialog
     Dialog := TFactsDialog.Create(Application);
     try
        // set the engine to the TFacts.Engine property as there may be
        // more than one inference engine on the form
        Dialog.Engine := Facts.Engine;
        Dialog.Caption := Caption;
        Dialog.EnableFactTemplateEditor := EnableFactTemplateEditor;
        Dialog.EnableTypeEditor := EnableTypeEditor;
        // execute the dialog... The Facts dialog ensures that the Facts
        // property is updated if the user clicks ok
        Dialog.Execute;
     finally
        Dialog.Free;
     end;
end;

{ TRSTFactTemplatesDlgPropertyPreset }

constructor TRSTFactTemplatesDlgPropertyPreset.Create(
  Collection: TCollection);
begin
     inherited Create(Collection);
     Name := 'TFactTemplates Dialog Property Editor';
     Caption := 'Edit Fact Templates...';
     TypeNames.Add( 'TFactTemplates' );
     EnableTypeEditor := True;
end;

procedure TRSTFactTemplatesDlgPropertyPreset.EditButtonClick(
  Sender: TObject);
var
   FTs: TFactTemplates;
   Dialog: TFactTemplatesDialog;
begin
     // Get the TFacts class
     FTs := (Sender as TRSItemProperty).GetObjectProperty as TFactTemplates;
     // Create the TFactTemplates dialog
     Dialog := TFactTemplatesDialog.Create(Application);
     try
        // set the engine to the TFactTemplates.Engine property as there may be
        // more than one inference engine on the form
        Dialog.Engine := FTs.Engine;
        Dialog.Caption := Caption;
        Dialog.EnableTypeEditor := EnableTypeEditor;
        // execute the dialog... The TFactTemplates dialog ensures that the TFactTemplates
        // property is updated if the user clicks ok
        Dialog.Execute;
     finally
        Dialog.Free;
     end;
end;

{ TRSTRulesDlgPropertyPreset }

constructor TRSTRulesDlgPropertyPreset.Create(Collection: TCollection);
begin
     inherited Create(Collection);
     Name := 'TRules Dialog Property Editor';
     Caption := 'Edit Rules...';
     TypeNames.Add( 'TRules' );
     EnableFactTemplateEditor := True;
     EnableTypeEditor := True;
end;

procedure TRSTRulesDlgPropertyPreset.EditButtonClick(Sender: TObject);
var
   Rules: TRules;
   Dialog: TRulesDialog;
begin
     // Get the TRules class
     Rules := (Sender as TRSItemProperty).GetObjectProperty as TRules;
     // Create the Rules dialog
     Dialog := TRulesDialog.Create(Application);
     try
        // set the engine to the TRules.Engine property as there may be
        // more than one inference engine on the form
        Dialog.Engine := Rules.Engine;
        Dialog.Caption := Caption;
        Dialog.EnableFactTemplateEditor := EnableFactTemplateEditor;
        Dialog.EnableTypeEditor := EnableTypeEditor;
        // execute the dialog... The Rules dialog ensures that the Rules
        // property is updated if the user clicks ok
        Dialog.Execute;
     finally
        Dialog.Free;
     end;
end;

{ TRSTFactSetFactsDlgPropertyPreset }

constructor TRSTFactSetFactsDlgPropertyPreset.Create(
  Collection: TCollection);
begin
     inherited Create(Collection);
     Name := 'TFactSetFacts Dialog Property Editor';
     Caption := 'Edit FactSet Facts...';
     MatchTypes := [];
     ParentClassNames.Add('TFactSet');
     PropNames.Add('Facts');
end;

procedure TRSTFactSetFactsDlgPropertyPreset.EditButtonClick(
  Sender: TObject);
var
   Facts: TFactSetFacts;
   Dialog: TFactSetDialog;
begin
     // Get the TFactSetFacts class
     Facts := (Sender as TRSItemProperty).GetObjectProperty as TFactSetFacts;
     // Create the Facts dialog
     Dialog := TFactSetDialog.Create(Application);
     try
        // set the FactSet to the TFactSetFacts.FactSet property
        Dialog.FactSet := Facts.FactSet;
        Dialog.Caption := Caption;
        Dialog.EnableFactTemplateEditor := EnableFactTemplateEditor;
        Dialog.EnableTypeEditor := EnableTypeEditor;
        // execute the dialog... The TFactSetFacts dialog ensures that the TFactSetFacts
        // property is updated if the user clicks ok
        Dialog.Execute;
     finally
        Dialog.Free;
     end;
end;

{ TRSEngineDDPropertyPreset }

constructor TRSEngineDDPropertyPreset.Create(Collection: TCollection);
begin
     inherited Create(Collection);
     Name := 'TInferenceEngine Property Drop Down Property Preset';
     PropNames.Add('Engine');
     ParentClassNames.Add('TFactSet');
     ParentClassNames.Add('TRule');
     OnGetPickList := GetPickList;
end;

procedure TRSEngineDDPropertyPreset.GetPickList(Sender: TObject;
  const PropName: string; Values: TStrings);
var
   i: Integer;
begin
     // this method fills the pick list with all components that match the RealValue
     // property of the item
     Values.BeginUpdate;
     try
        if (Inspector <> nil) and (Inspector.Owner <> nil) then
        for i := 0 to Inspector.Owner.ComponentCount - 1 do
            if Inspector.Owner.Components[i] is TInferenceEngine then
               Values.Add(Inspector.Owner.Components[i].Name);
        if Values.Count = 0 then
           Values.Add('');
     finally
        Values.EndUpdate;
     end;
end;

{ TRSTUserFunctionListDlgPropertyPreset }

constructor TRSTUserFunctionListDlgPropertyPreset.Create(
  Collection: TCollection);
begin
     inherited Create(Collection);
     Name := 'TUserFunctionList Dialog Property Editor';
     Caption := 'Edit Functions...';
     TypeNames.Add( 'TUserFunctionList' );
end;

procedure TRSTUserFunctionListDlgPropertyPreset.EditButtonClick(
  Sender: TObject);
var
   UFL: TUserFunctionList;
   Dialog: TFunctionsDialog;
begin
     // Get the TUserFunctionList class
     UFL := (Sender as TRSItemProperty).GetObjectProperty as TUserFunctionList;
     // Create the TUserFunctionList dialog
     Dialog := TFunctionsDialog.Create(Application);
     try
        // set the engine to the TUserFunctionList.Package property as there may be
        // more than one inference engine on the form
        Dialog.Package := UFL.Package;
        Dialog.Caption := Caption;
        // execute the dialog... The TUserFunctionList dialog ensures that the TUserFunctionList
        // property is updated if the user clicks ok
        Dialog.Execute;
     finally
        Dialog.Free;
     end;
end;

initialization
  RegisterPresets([ TRSTIEDefinedTypesDlgPropertyPreset,
                    TRSTFactsDlgPropertyPreset,
                    TRSTFactTemplatesDlgPropertyPreset,
                    TRSTRulesDlgPropertyPreset,
                    TRSTFactSetFactsDlgPropertyPreset,
                    TRSEngineDDPropertyPreset,
                    TRSTUserFunctionListDlgPropertyPreset ]);

finalization
  UnregisterPresets([ TRSTIEDefinedTypesDlgPropertyPreset,
                    TRSTFactsDlgPropertyPreset,
                    TRSTFactTemplatesDlgPropertyPreset,
                    TRSTRulesDlgPropertyPreset,
                    TRSTFactSetFactsDlgPropertyPreset,
                    TRSEngineDDPropertyPreset,
                    TRSTUserFunctionListDlgPropertyPreset ]);

end.
