unit TSForms;

interface

uses
	Windows,
	Messages,
	Classes,
	Forms,
	Contnrs,
	Controls;

const
	MsgWaitForShown = WM_User + 999;
	MsgWaitForActivated  = WM_User + 1000;

type
	NTSCaptionStyle = (tscsCommonFirst, tscsFormCaptionFirst, tscsCommonOnly, tscsFormCaptionOnly);
	NTSSaverActive = (tssvInactive, tssvActive, tssvInherit);
	MGotoIDEvent = procedure(Sender: TObject; ID: Integer) of object;

	TWinControlCracker = class(TWinControl);

	//#####################################################################################
	//		Klasse TTSForm = class(TForm)
	//#####################################################################################
	TTSForm = class(TForm)
	private
		fFirstShow: Boolean;
		fMinApp: Boolean;
		fAfterShow: TNotifyEvent;
		fAfterActivate: TNotifyEvent;
		fRestorePosition: TNotifyEvent;
		fSavePos: TNotifyEvent;
		fSaverActive: NTSSaverActive;
		procedure SetMinApp(Value: Boolean);
		procedure WMSysCommand(var Msg: TMessage); message WM_SysCommand;  // = messages "Maximize/Minimize/Restore"
		procedure WaitForActivated(var Msg: TMessage); Message MsgWaitForActivated;
		procedure WaitForShown(var Msg: TMessage); message MsgWaitForShown;
	protected
		procedure Loaded; override;
	public
		constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
		procedure AfterConstruction; override;
		procedure BeforeDestruction; override;
		procedure Activate; override;
		procedure DoShow; override;
		function CheckSaverActive: Boolean; virtual;
		procedure SavePosition;
		procedure RestorePosition;
	published
		property MinimizesApp: Boolean read fMinApp write SetMinApp;
		property SaverActive: NTSSaverActive read fSaverActive write fSaverActive;
		property AfterShow: TNotifyEvent read fAfterShow write fAfterShow;
		property AfterActivate: TNotifyEvent read fAfterActivate write fAfterActivate;
		property OnSavePosition: TNotifyEvent read fSavePos write fSavePos;
		property OnRestorePosition: TNotifyEvent read fRestorePosition write fRestorePosition;
	end;

	//#####################################################################################
	//		Klasse TTSCommonCaptionForm = class(TTSForm)
	//#####################################################################################
	TTSCommonCaptionForm = class(TTSForm)
	private
		fFormCaption: string;
		fCommonCaption: string;
		fCaptionSeparator: string;
		fOnCommonCaptionChange: TNotifyEvent;
		fCaptionStyle: NTSCaptionStyle;
		fSettingCaption: Boolean;
		function GetCaption: string;
		procedure SetCaptionSeparator(const Value: string);
		procedure SetFormCaption(const Value: string);
		procedure SetCommonCaption(const Value: string);
		procedure SetCaption(const Value: string);
		procedure InternalSetCaption;
		procedure SetCaptionStyle(const Value: NTSCaptionStyle);
		procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
		function GetCommonMaster: Boolean;
		procedure SetCommonMaster(const Value: Boolean);
	public
		constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
		procedure DoCreate; override;
		destructor Destroy; override;
		procedure SetCommonCaptionForAll; overload;
		procedure SetCommonCaptionForAll(const Value: string); overload;
	published
		property CaptionSeparator: string read fCaptionSeparator write SetCaptionSeparator;
		property Caption: string read GetCaption write SetCaption stored False;
		property CaptionStyle: NTSCaptionStyle read fCaptionStyle write SetCaptionStyle default tscsFormCaptionOnly;
		property FormCaption: string read fFormCaption write SetFormCaption;
		property CommonCaption: string read fCommonCaption write SetCommonCaption stored False;
		property CommonMaster: Boolean read GetCommonMaster write SetCommonMaster default False;
		property OnCommonCaptionChange: TNotifyEvent read fOnCommonCaptionChange write fOnCommonCaptionChange;
	end;

	//#####################################################################################
	//		Klasse TTailorAppForm = class(TTSCommonCaptionForm)
	//#####################################################################################
	TTailorAppForm = class(TTSCommonCaptionForm)
	private
		fOnReadOnlyChange: TNotifyEvent;
		fReadOnly: Boolean;
		fOnGotoID: MGotoIDEvent;
		procedure SetReadOnly(const Value: Boolean);
	public
		constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
		property ReadOnly: Boolean read fReadOnly write SetReadOnly;
		procedure GotoID(IDNr: Integer);
	published
		property OnReadOnlyChange: TNotifyEvent read fOnReadOnlyChange write fOnReadOnlyChange;
		property OnGotoID: MGotoIDEvent read fOnGotoID write fOnGotoID;
	end;

function TSCommonFormCaption: string; overload;
function TSCommonFormCaption(const NewCaption: string): string; overload;

implementation

uses StrUtils;

var
	CommonFormCaption: string = '';
	TSCaptionForms: TObjectList = nil;
	CommonCaptionMaster: TTSCommonCaptionForm = nil;

function TSCommonFormCaption: string; overload;
begin
	Result := CommonFormCaption;
end;

function TSCommonFormCaption(const NewCaption: string): string; overload;
var
	I: Integer;
begin
	CommonFormCaption := NewCaption;
	if Assigned(TSCaptionForms) then
		for I := 0 to pred(TSCaptionForms.Count) do
			if TSCaptionForms[I] is TTSCommonCaptionForm then
				TTSCommonCaptionForm(TSCaptionForms[I]).CommonCaption := NewCaption;
	Result := CommonFormCaption;
end;

//***************************************************************************************
//***************************************************************************************
{ TTSForm}
//***************************************************************************************
//***************************************************************************************

//***************************************************************************************
function TTSForm.CheckSaverActive: Boolean;
begin
	Result := fSaverActive = tssvActive;
end;

constructor TTSForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
	inherited CreateNew(AOwner, Dummy);
	fSaverActive := tssvActive;
end;

//***************************************************************************************
procedure TTSForm.AfterConstruction;
begin
	inherited AfterConstruction;
end;

//***************************************************************************************
procedure TTSForm.BeforeDestruction;
begin
	if CheckSaverActive then
		SavePosition;
	inherited BeforeDestruction;
end;

//***************************************************************************************
procedure TTSForm.DoShow;
begin
	inherited DoShow;
	if fFirstShow
		and CheckSaverActive
		and not (csDesigning in ComponentState)
	then
		RestorePosition;
	fFirstShow := False;
	if Assigned(fAfterShow) then
		PostMessage(Handle, MsgWaitForShown, 0, 0);
end;

//***************************************************************************************
procedure TTSForm.Loaded;
begin
	inherited Loaded;
	fFirstShow := True;
end;

//***************************************************************************************
procedure TTSForm.WaitForShown(var Msg: TMessage);
begin
	if Assigned(fAfterShow) then
		fAfterShow(Self);
end;

//***************************************************************************************
procedure TTSForm.Activate;
begin
	inherited Activate;
	if Assigned(fAfterActivate) then
		PostMessage(Handle, MsgWaitForActivated, 0, 0);
end;

//***************************************************************************************
procedure TTSForm.WaitForActivated(var Msg: TMessage);
begin
	if Assigned(fAfterActivate) then
		fAfterActivate(Self);
end;

//***************************************************************************************
procedure TTSForm.RestorePosition;
begin
	if Assigned(fRestorePosition) then
		fRestorePosition(Self);
end;

//***************************************************************************************
procedure TTSForm.SavePosition;
begin
	if Assigned(fSavePos) then
		fSavePos(Self);
end;

//***************************************************************************************
procedure TTSForm.SetMinApp(Value: Boolean);
begin
	fMinApp := Value;
end;

//***************************************************************************************
procedure TTSForm.WMSysCommand(var Msg: TMessage);
begin
	if fMinApp
	and ((Msg.wParam = SC_MINIMIZE) or (Msg.wParam = SC_ICON))
	and not (csDesigning in ComponentState) then
	begin
		Application.Minimize;
		Msg.Result := 1;
	end
	else
		inherited;
end;


//***************************************************************************************
//***************************************************************************************
{ TTSCommonCaptionForm }
//***************************************************************************************
//***************************************************************************************

//***************************************************************************************
constructor TTSCommonCaptionForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
	inherited CreateNew(AOwner, Dummy);
	fCommonCaption := CommonFormCaption;
	fCaptionSeparator := ': ';
	fCaptionStyle := tscsFormCaptionOnly;
	InternalSetCaption;
end;

//***************************************************************************************
procedure TTSCommonCaptionForm.DoCreate;
begin
//	fCommonCaption := CommonCaption;
//	fCaptionSeparator := ': ';
//	SetCaption(inherited Caption);
//	InternalSetCaption;
//	if not (csDesigning in ComponentState) then begin
		if not Assigned(TSCaptionForms) then
			TSCaptionForms := TObjectList.Create(False);
		TSCaptionForms.Add(Self);
//	end;
	inherited DoCreate;
end;

//***************************************************************************************
destructor TTSCommonCaptionForm.Destroy;
begin
	if Assigned(TSCaptionForms) then
		TSCaptionForms.Remove(Self);
	SetCommonMaster(False);
	inherited Destroy;
end;

//***************************************************************************************
//Die Methode übergibt den eigenen Datenbank-Anzeigenamen (DBCaption) an die
// globale Funktion "StandardDBName", und übergibt den Namen so an alle anderen TTSCommonCaptionForms.
procedure TTSCommonCaptionForm.SetCommonCaptionForAll(const Value: string);
begin
	TSCommonFormCaption(Value);
end;

//***************************************************************************************
procedure TTSCommonCaptionForm.SetCommonCaptionForAll;
begin
	TSCommonFormCaption(fCommonCaption);
end;

//***************************************************************************************
function TTSCommonCaptionForm.GetCaption: string;
begin
	Result := inherited Caption;
end;

//***************************************************************************************
procedure TTSCommonCaptionForm.SetCaptionSeparator(const Value: string);
begin
	fCaptionSeparator := Value;
	InternalSetCaption;
end;

//***************************************************************************************
procedure TTSCommonCaptionForm.InternalSetCaption;
begin
	fSettingCaption := True;
	try
		case fCaptionStyle of
		tscsCommonFirst: begin
			if fFormCaption = '' then
				inherited Caption := fCommonCaption
			else if fCommonCaption = '' then
				inherited Caption := fFormCaption
			else
				inherited Caption := fCommonCaption + fCaptionSeparator + fFormCaption;
		end;
		tscsFormCaptionFirst: begin
			if fFormCaption = '' then
				inherited Caption := fCommonCaption
			else if fCommonCaption = '' then
				inherited Caption := fFormCaption
			else
				inherited Caption := fFormCaption + fCaptionSeparator + fCommonCaption;
		end;
		tscsCommonOnly:
			inherited Caption := fCommonCaption;
		tscsFormCaptionOnly:
			inherited Caption := fFormCaption;
		end;

		if (fCommonCaption <> CommonFormCaption)
		and CommonMaster then
			TSCommonFormCaption(fCommonCaption);

	finally
		fSettingCaption := False;
	end;
end;

//***************************************************************************************
procedure TTSCommonCaptionForm.SetCaption(const Value: string);
var
	P: Integer;
begin
	P := Pos(CaptionSeparator, Value);
	case fCaptionStyle of
	tscsCommonFirst:
		if P > 0 then begin
			fCommonCaption := Copy(Value, 1, P-1);
			fFormCaption := Copy(Value, P+Length(CaptionSeparator), Length(Value));
		end
		else begin
			fCommonCaption := Value;
			fFormCaption := '';
		end;
	tscsFormCaptionFirst:
		if P > 0 then begin
			fCommonCaption := Copy(Value, P+Length(CaptionSeparator), Length(Value));
			fFormCaption := Copy(Value, 1, P-1);
		end
		else begin
			fCommonCaption := '';
			fFormCaption := Value;
		end;
	tscsCommonOnly: begin
		fCommonCaption := Value;
		fFormCaption := '';
	end;
	tscsFormCaptionOnly: begin
		fCommonCaption := '';
		fFormCaption := Value;
	end;
	end;
	InternalSetCaption;
end;

//***************************************************************************************
procedure TTSCommonCaptionForm.CMTextChanged(var Message: TMessage);
begin
	if not fSettingCaption then begin
		SetCaption(inherited Caption);
	end;
end;

//***************************************************************************************
procedure TTSCommonCaptionForm.SetCommonCaption(const Value: string);
begin
	fCommonCaption := Value;
	InternalSetCaption;
	if not (csDesigning in ComponentState) and Assigned(fOnCommonCaptionChange) then
		fOnCommonCaptionChange(Self);
end;

//***************************************************************************************
procedure TTSCommonCaptionForm.SetFormCaption(const Value: string);
begin
	fFormCaption := Value;
	InternalSetCaption;
end;

//***************************************************************************************
procedure TTSCommonCaptionForm.SetCaptionStyle(const Value: NTSCaptionStyle);
begin
	fCaptionStyle := Value;
	InternalSetCaption;
end;


//***************************************************************************************
function TTSCommonCaptionForm.GetCommonMaster: Boolean;
begin
	Result := Assigned(CommonCaptionMaster) and (Self = CommonCaptionMaster);
end;

//***************************************************************************************
procedure TTSCommonCaptionForm.SetCommonMaster(const Value: Boolean);
begin
	if Value then begin
		CommonCaptionMaster := Self;
		TSCommonFormCaption(fCommonCaption);
	end
	else if GetCommonMaster then
		CommonCaptionMaster := nil;
end;

//***************************************************************************************
//***************************************************************************************
{ TTailorAppForm }
//***************************************************************************************
//***************************************************************************************

constructor TTailorAppForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
	inherited CreateNew(AOwner, Dummy);
	fOnReadOnlyChange := nil;
end;

procedure TTailorAppForm.GotoID(IDNr: Integer);
begin
	if Assigned(fOnGotoID) then
		fOnGotoID(Self, IDNr);
end;

procedure TTailorAppForm.SetReadOnly(const Value: Boolean);
begin
	fReadOnly := Value;
	if Assigned(fOnReadOnlyChange) then
		fOnReadOnlyChange(Self);
end;


//***************************************************************************************
//***************************************************************************************

initialization
	;

finalization
	TSCaptionForms.Free;

end.



