{------------------------------------------------------------------------------}
{                                                                              }
{  WaveMixer - Audio Mixer Controls                                            }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

{$I DELPHIAREA.INC}

unit scWaveMixer;

interface

uses
  Windows, Messages, Classes, MMSystem;

type

  TAudioMixer = class;
  TAudioMixerLine = class;

  TMixerControlType = (mcVolume, mcMute, mcSelect, mcMix);
  TMixerControlTypes = set of TMixerControlType;

  TMixerLineTargetType = (tgUndefined, tgWaveOut, tgWaveIn, tgMidiOut,
    tgMidiIn, tgAux);

  TMixerLineComponentType = (cmDstUndefined, cmDstDigital, cmDstLine,
    cmDstMonitor, cmDstSpeakers, cmDstHeadphones, cmDstTelephone, cmDstWaveIn,
    cmDstVoiceIn, cmSrcUndefined, cmSrcDigital, cmSrcLine, cmSrcMicrophone,
    cmSrcSynthesizer, cmCompactDisc, cmSrcTelephone, cmSrcPCSpeaker,
    cmSrcWaveOut, cmSrcAuxiliary, cmSrcAnalog);

  TMixerLineFlags = set of (lfActive, lfDisconnected, lfSource);

  TMixerLineNotifyEvent = procedure(Sender: TObject;
    MixerLine: TAudioMixerLine) of object;
  TMixerControlNotifyEvent = procedure(Sender: TObject;
    MixerLine: TAudioMixerLine; ControlType: TMixerControlType) of object;

  TAudioMixerLine = class(TObject)
  private
    fID: Integer;
    fMixer: TAudioMixer;
    fLineInfo: TMixerLine;
    fLastError: MMRESULT;
    fAvailableControls: TMixerControlTypes;
    fControls: array[TMixerControlType] of TMixerControl;
    function GetName: String;
    function GetTargetType: TMixerLineTargetType;
    function GetComponentType: TMixerLineComponentType;
    function GetFlags: TMixerLineFlags;
    function GetVolume: Integer;
    procedure SetVolume(Value: Integer);
    function GetMute: Boolean;
    procedure SetMute(Value: Boolean);
    function GetSelectedLine: Integer;
    procedure SetSelectedLine(Value: Integer);
  protected
    constructor Create(AMixer: TAudioMixer; AID: Integer);
    function AssignMixerControl(var MixerControl: TMixerControl;
      ControlType: TMixerControlType): Boolean;
    function FindControlByID(ControlID: DWORD;
      var ControlType: TMixerControlType): Boolean;
    function FindLineIDByChannelIndex(Index: DWORD;
      ControlType: TMixerControlType): Integer;
    function FindChannelIndexByLineID(LineID: DWORD;
      ControlType: TMixerControlType): Integer;
    procedure RefreshDetails(AID: Integer);
    property LineInfo: TMixerLine read fLineInfo;
  public
    property Mixer: TAudioMixer read fMixer;
    property ID: Integer read fID;
    property Name: String read GetName;
    property LastError: MMRESULT read fLastError;
    property TargetType: TMixerLineTargetType read GetTargetType;
    property ComponentType: TMixerLineComponentType read GetComponentType;
    property Flags: TMixerLineFlags read GetFlags;
    property AvailableControls: TMixerControlTypes read fAvailableControls;
    property Volume: Integer read GetVolume write SetVolume;
    property Mute: Boolean read GetMute write SetMute;
    property SelectedLine: Integer read GetSelectedLine write SetSelectedLine;
  end;

  TAudioMixer = class(TComponent)
  private
    fHandle: THandle;
    fMixerID: DWORD;
    fMixerCaps: TMixerCaps;
    fDestinationID: DWORD;
    fLines: TList;
    fMaster: TAudioMixerLine;
    fLastError: MMRESULT;
    fOnLineChange: TMixerLineNotifyEvent;
    fOnControlChange: TMixerControlNotifyEvent;
    CallbackHandle: HWND;
    function GetMixerCount: WORD;
    procedure SetMixerID(Value: DWORD);
    function GetMixerName: String;
    procedure SetMixerName(const Value: String);
    function GetDestinationCount: WORD;
    procedure SetDestinationID(Value: DWORD);
    function GetDestinationName: String;
    procedure SetDestinationName(const Value: String);
    function GetLineCount: WORD;
    function GetLines(LineID: DWORD): TAudioMixerLine;
    procedure CallbackProc(var Message: TMessage); virtual;
    procedure DeleteMixerLines;
    procedure CreateMixerLines;
  protected
    procedure RefreshDetails(DestinationOnly: Boolean);
    procedure DoLineChange(MixerLine: TAudioMixerLine);
    procedure DoControlChange(MixerLine: TAudioMixerLine; ControlType: TMixerControlType);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FetchMixerNames(const Names: TStrings): DWORD;
    function FetchDestinationNames(const Names: TStrings): DWORD;
    function FindMixerLine(ComponentType: TMixerLineComponentType;
      var ADestinationID, ALineID: Integer): Boolean;
    property Handle: THandle read fHandle;
    property LastError: MMRESULT read fLastError;
    property MixerCount: WORD read GetMixerCount;
    property MixerCaps: TMixerCaps read fMixerCaps;
    property DestinationCount: WORD read GetDestinationCount;
    property Master: TAudioMixerLine read fMaster;
    property LineCount: WORD read GetLineCount;
    property Lines[LineID: DWORD]: TAudioMixerLine read GetLines;
  published
    property MixerID: DWORD read fMixerID write SetMixerID default 0;
    property MixerName: String read GetMixerName write SetMixerName stored False;
    property DestinationID: DWORD read fDestinationID write SetDestinationID default 0;
    property DestinationName: String read GetDestinationName write SetDestinationName stored False;
    property OnLineChange: TMixerLineNotifyEvent read fOnLineChange write fOnLineChange;
    property OnControlChange: TMixerControlNotifyEvent read fOnControlChange write fOnControlChange;
  end;

implementation

{$IFNDEF COMPILER6_UP}
uses Forms;
{$ENDIF}

const
  // Mixer Line Target Types
  MixerLineTargetTypes: array [TMixerLineTargetType] of DWORD = (
    MIXERLINE_TARGETTYPE_UNDEFINED,
    MIXERLINE_TARGETTYPE_WAVEOUT,
    MIXERLINE_TARGETTYPE_WAVEIN,
    MIXERLINE_TARGETTYPE_MIDIOUT,
    MIXERLINE_TARGETTYPE_MIDIIN,
    MIXERLINE_TARGETTYPE_AUX);
  // Mixer Line Component Types
  MixerLineComponentTypes: array [TMixerLineComponentType] of DWORD = (
    MIXERLINE_COMPONENTTYPE_DST_UNDEFINED,
    MIXERLINE_COMPONENTTYPE_DST_DIGITAL,
    MIXERLINE_COMPONENTTYPE_DST_LINE,
    MIXERLINE_COMPONENTTYPE_DST_MONITOR,
    MIXERLINE_COMPONENTTYPE_DST_SPEAKERS,
    MIXERLINE_COMPONENTTYPE_DST_HEADPHONES,
    MIXERLINE_COMPONENTTYPE_DST_TELEPHONE,
    MIXERLINE_COMPONENTTYPE_DST_WAVEIN,
    MIXERLINE_COMPONENTTYPE_DST_VOICEIN,
    MIXERLINE_COMPONENTTYPE_SRC_UNDEFINED,
    MIXERLINE_COMPONENTTYPE_SRC_DIGITAL,
    MIXERLINE_COMPONENTTYPE_SRC_LINE,
    MIXERLINE_COMPONENTTYPE_SRC_MICROPHONE,
    MIXERLINE_COMPONENTTYPE_SRC_SYNTHESIZER,
    MIXERLINE_COMPONENTTYPE_SRC_COMPACTDISC,
    MIXERLINE_COMPONENTTYPE_SRC_TELEPHONE,
    MIXERLINE_COMPONENTTYPE_SRC_PCSPEAKER,
    MIXERLINE_COMPONENTTYPE_SRC_WAVEOUT,
    MIXERLINE_COMPONENTTYPE_SRC_AUXILIARY,
    MIXERLINE_COMPONENTTYPE_SRC_ANALOG);
  // Mixer Control Types
  MixerControlTypes: array [TMixerControlType] of DWORD = (
    MIXERCONTROL_CONTROLTYPE_VOLUME,
    MIXERCONTROL_CONTROLTYPE_MUTE,
    MIXERCONTROL_CONTROLTYPE_MUX,
    MIXERCONTROL_CONTROLTYPE_MIXER);

{ TAudioMixerLine }

type
  TMixerControlDetailsBooleanArray = array[0..255] of TMixerControlDetailsBoolean;
  PMixerControlDetailsBooleanArray = ^TMixerControlDetailsBooleanArray;
  TMixerControlDetailsListTextArray = array[0..255] of TMixerControlDetailsListText;
  PMixerControlDetailsListTextArray = ^TMixerControlDetailsListTextArray;

constructor TAudioMixerLine.Create(AMixer: TAudioMixer; AID: Integer);
begin
  fMixer := AMixer;
  RefreshDetails(AID);
end;

procedure TAudioMixerLine.RefreshDetails(AID: Integer);
var
  ControlType: TMixerControlType;
begin
  fID := AID;
  FillChar(fLineInfo, SizeOf(fLineInfo), 0);
  fLineInfo.cbStruct := SizeOf(fLineInfo);
  fLineInfo.dwDestination := fMixer.DestinationID;
  if fID >= 0 then
  begin
    fLineInfo.dwSource := fID;
    fLastError := mixerGetLineInfo(fMixer.Handle, @fLineInfo,
      MIXER_GETLINEINFOF_SOURCE or MIXER_OBJECTF_HMIXER)
  end
  else
    fLastError := mixerGetLineInfo(fMixer.Handle, @fLineInfo,
      MIXER_GETLINEINFOF_DESTINATION or MIXER_OBJECTF_HMIXER);
  fAvailableControls := [];
  if fLastError = MMSYSERR_NOERROR then
    for ControlType := Low(TMixerControlType) to High(TMixerControlType) do
      if AssignMixerControl(fControls[ControlType], ControlType) then
        Include(fAvailableControls, ControlType);
end;

function TAudioMixerLine.AssignMixerControl(var MixerControl: TMixerControl;
  ControlType: TMixerControlType): Boolean;
var
  mxLineControls: TMixerLineControls;
begin
  FillChar(mxLineControls, SizeOf(TMixerLineControls), 0);
  mxLineControls.cbStruct := SizeOf(TMixerLineControls);
  mxLineControls.dwLineID := fLineInfo.dwLineID;
  mxLineControls.cControls := 1;
  mxLineControls.cbmxctrl := SizeOf(TMixerControl);
  FillChar(MixerControl, SizeOf(TMixerControl), 0);
  mxLineControls.dwControlType := MixerControlTypes[ControlType];
  mxLineControls.pamxctrl := @MixerControl;
  Result := (mixerGetLineControls(fMixer.Handle, @mxLineControls,
    MIXER_GETLINECONTROLSF_ONEBYTYPE or MIXER_OBJECTF_HMIXER) = MMSYSERR_NOERROR);
end;

function TAudioMixerLine.FindControlByID(ControlID: DWORD;
  var ControlType: TMixerControlType): Boolean;
var
  CT: TMixerControlType;
begin
  Result := False;
  for CT := Low(TMixerControlType) to High(TMixerControlType) do
    if (CT in fAvailableControls) and (fControls[CT].dwControlID = ControlID) then
    begin
      ControlType := CT;
      Result := True;
    end;
end;

function TAudioMixerLine.FindLineIDByChannelIndex(Index: DWORD;
  ControlType: TMixerControlType): Integer;
var
  Details: TMixerControlDetails;
  Val: PMixerControlDetailsListTextArray;
  ValByteCount: Integer;
  I: Integer;
begin
  Result := -1;
  if ControlType in fAvailableControls then
  begin
    if Index < fControls[ControlType].cMultipleItems then
    begin
      ValByteCount := fControls[ControlType].cMultipleItems *
        SizeOf(TMixerControlDetailsListText);
      GetMem(Val, ValByteCount);
      try
        FillChar(Val^, ValByteCount, 0);
        FillChar(Details, SizeOf(Details), 0);
        Details.cbStruct := SizeOf(Details);
        Details.dwControlID := fControls[ControlType].dwControlID;
        Details.cChannels := 1;
        Details.cMultipleItems := fControls[ControlType].cMultipleItems;
        Details.cbDetails := SizeOf(TMixerControlDetailsListText);
        Details.paDetails := Val;
        fLastError := mixerGetControlDetails(fMixer.Handle, @Details,
          MIXER_GETCONTROLDETAILSF_LISTTEXT or MIXER_OBJECTF_HMIXER);
        if fLastError = MMSYSERR_NOERROR then
        begin
          fLastError := MMSYSERR_INVALPARAM;
          for I := 0 to Mixer.LineCount - 1 do
            if Mixer.Lines[I].Name = Val[Index].szName then
            begin
              Result := Mixer.Lines[I].ID;
              Break;
            end;
        end;
      finally
        FreeMem(Val);
      end;
    end
    else
      fLastError := MMSYSERR_INVALPARAM;
  end
  else
    fLastError := MIXERR_INVALCONTROL;
end;

function TAudioMixerLine.FindChannelIndexByLineID(LineID: DWORD;
  ControlType: TMixerControlType): Integer;
var
  Details: TMixerControlDetails;
  Val: PMixerControlDetailsListTextArray;
  ValByteCount: Integer;
  I: Integer;
begin
  Result := -1;
  if ControlType in fAvailableControls then
  begin
    ValByteCount := fControls[ControlType].cMultipleItems *
      SizeOf(TMixerControlDetailsListText);
    GetMem(Val, ValByteCount);
    try
      FillChar(Val^, ValByteCount, 0);
      FillChar(Details, SizeOf(Details), 0);
      Details.cbStruct := SizeOf(Details);
      Details.dwControlID := fControls[ControlType].dwControlID;
      Details.cChannels := 1;
      Details.cMultipleItems := fControls[ControlType].cMultipleItems;
      Details.cbDetails := SizeOf(TMixerControlDetailsListText);
      Details.paDetails := Val;
      fLastError := mixerGetControlDetails(fMixer.Handle, @Details,
        MIXER_GETCONTROLDETAILSF_LISTTEXT or MIXER_OBJECTF_HMIXER);
      if fLastError = MMSYSERR_NOERROR then
      begin
        fLastError := MMSYSERR_INVALPARAM;
        for I := 0 to fControls[ControlType].cMultipleItems - 1 do
          if Mixer.Lines[LineID].Name = Val[I].szName then
          begin
            Result := I;
            Break;
          end;
      end;
    finally
      FreeMem(Val);
    end;
  end
  else
    fLastError := MIXERR_INVALCONTROL;
end;

function TAudioMixerLine.GetName: String;
begin
  Result := fLineInfo.szName;
end;

function TAudioMixerLine.GetTargetType: TMixerLineTargetType;
var
  TT: TMixerLineTargetType;
begin
  Result := tgUndefined;
  for TT := Low(TMixerLineTargetType) to High(TMixerLineTargetType) do
    if fLineInfo.Target.dwType = MixerLineTargetTypes[TT] then
    begin
      Result := TT;
      Exit;
    end;
end;

function TAudioMixerLine.GetComponentType: TMixerLineComponentType;
var
  CT: TMixerLineComponentType;
begin
  Result := cmDstUndefined;
  for CT := Low(TMixerLineComponentType) to High(TMixerLineComponentType) do
    if fLineInfo.dwComponentType = MixerLineComponentTypes[CT] then
    begin
      Result := CT;
      Exit;
    end;
end;

function TAudioMixerLine.GetFlags: TMixerLineFlags;
begin
  Result := [];
  if LongBool(fLineInfo.fdwLine and MIXERLINE_LINEF_ACTIVE) then
    Include(Result, lfActive);
  if LongBool(fLineInfo.fdwLine and MIXERLINE_LINEF_DISCONNECTED) then
    Include(Result, lfDisconnected);
  if LongBool(fLineInfo.fdwLine and MIXERLINE_LINEF_SOURCE) then
    Include(Result, lfSource);
end;

function TAudioMixerLine.GetVolume: Integer;
var
  Details: TMixerControlDetails;
  Val: TMixerControlDetailsUnsigned;
begin
  Result := -1;
  fLastError := MIXERR_INVALCONTROL;
  if mcVolume in fAvailableControls then
  begin
    FillChar(Details, SizeOf(Details), 0);
    Details.cbStruct := SizeOf(Details);
    Details.dwControlID := fControls[mcVolume].dwControlID;
    Details.cChannels := 1;
    Details.cMultipleItems := 0;
    Details.cbDetails := SizeOf(Val);
    Details.paDetails := @Val;
    fLastError := mixerGetControlDetails(fMixer.Handle, @Details,
      MIXER_GETCONTROLDETAILSF_VALUE or MIXER_OBJECTF_HMIXER);
    if fLastError = MMSYSERR_NOERROR then
      Result := MulDiv(Val.dwValue, 100, fControls[mcVolume].Bounds.dwMaximum);
  end;
end;

procedure TAudioMixerLine.SetVolume(Value: Integer);
var
  Details: TMixerControlDetails;
  Val: TMixerControlDetailsUnsigned;
begin
  fLastError := MIXERR_INVALCONTROL;
  if mcVolume in fAvailableControls then
  begin
    if Value < 0 then Value := 0;
    if Value > 100 then Value := 100;
    Val.dwValue := MulDiv(fControls[mcVolume].Bounds.dwMaximum, Value, 100);
    FillChar(Details, SizeOf(Details), 0);
    Details.cbStruct := SizeOf(Details);
    Details.dwControlID := fControls[mcVolume].dwControlID;
    Details.cChannels := 1;
    Details.cMultipleItems := 0;
    Details.cbDetails := SizeOf(Val);
    Details.paDetails := @Val;
    fLastError := mixerSetControlDetails(fMixer.Handle, @Details,
      MIXER_SETCONTROLDETAILSF_VALUE or MIXER_OBJECTF_HMIXER);
  end;
end;

function TAudioMixerLine.GetMute: Boolean;
var
  Details: TMixerControlDetails;
  Val: TMixerControlDetailsBoolean;
begin
  Result := True;
  fLastError := MIXERR_INVALCONTROL;
  if mcMute in fAvailableControls then
  begin
    FillChar(Details, SizeOf(Details), 0);
    Details.cbStruct := SizeOf(Details);
    Details.dwControlID := fControls[mcMute].dwControlID;
    Details.cChannels := 1;
    Details.cMultipleItems := 0;
    Details.cbDetails := SizeOf(Val);
    Details.paDetails := @Val;
    fLastError := mixerGetControlDetails(fMixer.Handle, @Details,
      MIXER_GETCONTROLDETAILSF_VALUE or MIXER_OBJECTF_HMIXER);
    if fLastError = MMSYSERR_NOERROR then
      Result := Boolean(Val.fValue);
  end;
end;

procedure TAudioMixerLine.SetMute(Value: Boolean);
var
  Details: TMixerControlDetails;
  Val: TMixerControlDetailsBoolean;
begin
  fLastError := MIXERR_INVALCONTROL;
  if mcMute in fAvailableControls then
  begin
    Val.fValue := Ord(Value);
    FillChar(Details, SizeOf(Details), 0);
    Details.cbStruct := SizeOf(Details);
    Details.dwControlID := fControls[mcMute].dwControlID;
    Details.cChannels := 1;
    Details.cMultipleItems := 0;
    Details.cbDetails := SizeOf(Val);
    Details.paDetails := @Val;
    fLastError := mixerSetControlDetails(fMixer.Handle, @Details,
      MIXER_SETCONTROLDETAILSF_VALUE or MIXER_OBJECTF_HMIXER);
  end;
end;

function TAudioMixerLine.GetSelectedLine: Integer;
var
  ControlType: TMixerControlType;
  Details: TMixerControlDetails;
  Val: PMixerControlDetailsBooleanArray;
  ValByteCount: Integer;
  ValIndex: Integer;
begin
  Result := -1;
  if mcMix in fAvailableControls then
    ControlType := mcMix
  else if mcSelect in fAvailableControls then
    ControlType := mcSelect
  else
  begin
    fLastError := MIXERR_INVALCONTROL;
    Exit;
  end;
  if ControlType in fAvailableControls then
  begin
    ValByteCount := fControls[ControlType].cMultipleItems *
      SizeOf(TMixerControlDetailsBoolean);
    GetMem(Val, ValByteCount);
    try
      FillChar(Val^, ValByteCount, 0);
      FillChar(Details, SizeOf(Details), 0);
      Details.cbStruct := SizeOf(Details);
      Details.dwControlID := fControls[ControlType].dwControlID;
      Details.cChannels := 1;
      Details.cMultipleItems := fControls[ControlType].cMultipleItems;
      Details.cbDetails := SizeOf(TMixerControlDetailsBoolean);
      Details.paDetails := Val;
      fLastError := mixerGetControlDetails(fMixer.Handle, @Details,
        MIXER_GETCONTROLDETAILSF_VALUE or MIXER_OBJECTF_HMIXER);
      if fLastError = MMSYSERR_NOERROR then
      begin
        for ValIndex := 0 to fControls[ControlType].cMultipleItems - 1 do
          if Boolean(Val[ValIndex].fValue) then
          begin
            Result := FindLineIDByChannelIndex(ValIndex, ControlType);
            Break;
          end;
      end;
    finally
      FreeMem(Val);
    end;
  end;
end;

procedure TAudioMixerLine.SetSelectedLine(Value: Integer);
var
  ControlType: TMixerControlType;
  Details: TMixerControlDetails;
  Val: PMixerControlDetailsBooleanArray;
  ValByteCount: Integer;
  ValIndex: Integer;
begin
  if mcMix in fAvailableControls then
    ControlType := mcMix
  else if mcSelect in fAvailableControls then
    ControlType := mcSelect
  else
  begin
    fLastError := MIXERR_INVALCONTROL;
    Exit;
  end;
  ValIndex := FindChannelIndexByLineID(Value, ControlType);
  if ValIndex >= 0 then
  begin
    ValByteCount := fControls[ControlType].cMultipleItems *
      SizeOf(TMixerControlDetailsBoolean);
    GetMem(Val, ValByteCount);
    try
      FillChar(Val^, ValByteCount, 0);
      Val[ValIndex].fValue := Ord(True);
      FillChar(Details, SizeOf(Details), 0);
      Details.cbStruct := SizeOf(Details);
      Details.dwControlID := fControls[ControlType].dwControlID;
      Details.cChannels := 1;
      Details.cMultipleItems := fControls[ControlType].cMultipleItems;
      Details.cbDetails := SizeOf(Val);
      Details.paDetails := Val;
      fLastError := mixerSetControlDetails(fMixer.Handle, @Details,
        MIXER_SETCONTROLDETAILSF_VALUE or MIXER_OBJECTF_HMIXER);
    finally
      FreeMem(Val);
    end;
  end
  else
    fLastError := MMSYSERR_INVALPARAM
end;

{ TAudioMixer }

constructor TAudioMixer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CallbackHandle := AllocateHWnd(CallbackProc);
  fMaster := TAudioMixerLine.Create(Self, -1);
  fLines := TList.Create;
  RefreshDetails(False);
end;

destructor TAudioMixer.Destroy;
begin
  DeleteMixerLines;
  fLines.Free;
  fMaster.Free;
  if fHandle <> 0 then
    mixerClose(fHandle);
  if CallbackHandle <> 0 then
    DeallocateHWnd(CallbackHandle);
  inherited Destroy;
end;

procedure TAudioMixer.CallbackProc(var Message: TMessage);
var
  I: Integer;
  ControlType: TMixerControlType;
begin
  with Message do
    case msg of
      MM_MIXM_LINE_CHANGE:
      begin
        if THandle(wParam) = fHandle then
          if fMaster.LineInfo.dwLineID = DWORD(lParam) then
            DoLineChange(fMaster)
          else
            for I := 0 to fLines.Count - 1 do
              if TAudioMixerLine(fLines[I]).LineInfo.dwLineID = DWORD(lParam) then
              begin
                DoLineChange(TAudioMixerLine(fLines[I]));
                Break;
              end;
        Result := 0;
      end;
      MM_MIXM_CONTROL_CHANGE:
      begin
        if THandle(wParam) = fHandle then
          if fMaster.FindControlByID(DWORD(lParam), ControlType) then
            DoControlChange(fMaster, ControlType)
          else
            for I := 0 to fLines.Count - 1 do
              if TAudioMixerLine(fLines[I]).FindControlByID(DWORD(lParam), ControlType) then
              begin
                DoControlChange(TAudioMixerLine(fLines[I]), ControlType);
                Break;
              end;
        Result := 0;
      end;
    else
      Result := DefWindowProc(CallbackHandle, Msg, wParam, lParam);
    end;
end;

function TAudioMixer.FetchMixerNames(const Names: TStrings): DWORD;
var
  Index: Integer;
  Caps: TMixerCaps;
begin
  Result := 0;
  for Index := 0 to mixerGetNumDevs - 1 do
  begin
    mixerGetDevCaps(Index, @Caps, SizeOf(Caps));
    Names.Append(Caps.szPname);
    Inc(Result);
  end;
end;

function TAudioMixer.FetchDestinationNames(const Names: TStrings): DWORD;
var
  Index: Integer;
  LineInfo: TMixerLine;
begin
  Result := 0;
  for Index := 0 to fMixerCaps.cDestinations - 1 do
  begin
    LineInfo.cbStruct := SizeOf(LineInfo);
    LineInfo.dwDestination := Index;
    mixerGetLineInfo(fHandle, @LineInfo,
      MIXER_GETLINEINFOF_DESTINATION or MIXER_OBJECTF_HMIXER);
    Names.Append(LineInfo.szName);
    Inc(Result);
  end;
end;

function TAudioMixer.FindMixerLine(ComponentType: TMixerLineComponentType;
  var ADestinationID, ALineID: Integer): Boolean;
var
  LineInfo: TMixerLine;
begin
  Result := False;
  LineInfo.cbStruct := SizeOf(LineInfo);
  LineInfo.dwComponentType := MixerLineComponentTypes[ComponentType];
  if mixerGetLineInfo(fHandle, @LineInfo, MIXER_GETLINEINFOF_COMPONENTTYPE or
    MIXER_OBJECTF_HMIXER) = MMSYSERR_NOERROR then
  begin
    ADestinationID := LineInfo.dwDestination;
    ALineID := LineInfo.dwSource;
    Result := (ALineID >= 0);
  end;
end;

procedure TAudioMixer.DoLineChange(MixerLine: TAudioMixerLine);
begin
  if Assigned(fOnLineChange) then
    fOnLineChange(Self, MixerLine);
end;

procedure TAudioMixer.DoControlChange(MixerLine: TAudioMixerLine;
  ControlType: TMixerControlType);
begin
  if Assigned(fOnControlChange) then
    fOnControlChange(Self, MixerLine, ControlType);
end;

function TAudioMixer.GetMixerCount: WORD;
begin
  Result := mixerGetNumDevs;
end;

function TAudioMixer.GetMixerName: String;
begin
  Result := fMixerCaps.szPname;
end;

procedure TAudioMixer.SetMixerName(const Value: String);
var
  Names: TStringList;
  Index: Integer;
begin
  if MixerName <> Value then
  begin
    Names := TStringList.Create;
    try
      FetchMixerNames(Names);
      Index := Names.IndexOf(Value);
      if Index <> -1 then MixerID := Index;
    finally
      Names.Free;
    end;
  end;
end;

procedure TAudioMixer.SetMixerID(Value: DWORD);
begin
  if (fMixerID <> Value) and (Value < MixerCount) then
  begin
    fMixerID := Value;
    RefreshDetails(False);
  end;
end;

procedure TAudioMixer.SetDestinationID(Value: DWORD);
begin
  if fDestinationID <> Value then
  begin
    fDestinationID := Value;
    RefreshDetails(True);
  end;
end;

function TAudioMixer.GetDestinationName: String;
begin
  Result := fMaster.Name;
end;

procedure TAudioMixer.SetDestinationName(const Value: String);
var
  Names: TStringList;
  Index: Integer;
begin
  if DestinationName <> Value then
  begin
    Names := TStringList.Create;
    try
      FetchDestinationNames(Names);
      Index := Names.IndexOf(Value);
      if Index <> -1 then DestinationID := Index;
    finally
      Names.Free;
    end;
  end;
end;

function TAudioMixer.GetDestinationCount: WORD;
begin
  Result := fMixerCaps.cDestinations;
end;

function TAudioMixer.GetLineCount: WORD;
begin
  Result := fMaster.LineInfo.cConnections;
end;

function TAudioMixer.GetLines(LineID: DWORD): TAudioMixerLine;
begin
  Result := TAudioMixerLine(fLines[LineID]);
end;

procedure TAudioMixer.CreateMixerLines;
var
  I: Integer;
begin
  for I := 0 to LineCount - 1 do
    fLines.Add(TAudioMixerLine.Create(Self, I));
end;

procedure TAudioMixer.DeleteMixerLines;
var
  I: Integer;
begin
  for I := fLines.Count - 1 downto 0 do
  begin
    TAudioMixerLine(fLines[I]).Free;
    fLines.Delete(I);
  end;
end;

procedure TAudioMixer.RefreshDetails(DestinationOnly: Boolean);
begin
  DeleteMixerLines;
  if not DestinationOnly then
  begin
    if fHandle <> 0 then
    begin
      mixerClose(fHandle);
      fHandle := 0;
    end;
    FillChar(fMixerCaps, SizeOf(fMixerCaps), 0);
    mixerGetDevCaps(fMixerID, @fMixerCaps, SizeOf(fMixerCaps));
    fLastError := mixerOpen(@fHandle, fMixerID, CallBackHandle, 0,
      MIXER_OBJECTF_MIXER or CALLBACK_WINDOW);
  end;
  if fDestinationID >= DestinationCount then
    fDestinationID := 0;
  fMaster.RefreshDetails(-1);
  CreateMixerLines;
end;

end.
