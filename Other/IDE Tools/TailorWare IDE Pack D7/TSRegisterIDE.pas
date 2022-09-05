unit TSRegisterIDE;

interface

procedure Register;

implementation

uses
	Controls,
	Classes,
	Graphics,
	DesignIntf,

	TSForms,
	TSPropEdit,
	TSRegFrmWiz;

procedure Register;
begin
	RegisterPropertyEditor(TypeInfo(TColor), TPersistent, '', TTSColorProperty);
	RegisterPropertyEditor(TypeInfo(TColor), nil, '', TTSColorProperty);

	//Register TFrmStrPropDlg as Property Editor for string classes.
	RegisterPropertyEditor(TypeInfo(string), nil, '', TTSStringProperty);
	RegisterPropertyEditor(TypeInfo(TCaption), nil, '', TTSStringProperty);
	RegisterPropertyEditor(TypeInfo(WideString), nil, '', TTSStringProperty);
	RegisterPropertyEditor (TypeInfo(TStrings), nil, '', TTSStringListProperty);

	//"Reregister" TFrmStrPropDlg as Property Editor for those string Properties that
	// have other property editors assigned via Class/Property rather than type.
	RegisterPropertyEditor(TypeInfo(string), TControl, 'Hint', TTSStringProperty);

	RegisterNewFormClass(TTSForm, 'TSForms');
	RegisterNewFormClass(TTSCommonCaptionForm, 'TSForms');
	RegisterNewFormClass(TTailorAppForm, 'TSForms', 'GREENBALL', 'Another Form Class', 'TailorSoft');
end;



end.
