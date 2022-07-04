///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

const
  dmuaEnabled=1;   // update state flags
  dmuaChecked=2;
  dmuaVisible=4;
  dmuaUndefined=8;
  CATID_DMActionProvider: TGUID = '{673A9FB4-9A0B-11D4-B2A4-FD6847C75367}'; 

type
  IDMActionProvider=interface(IUnknown)
    ['{673A9FB5-9A0B-11D4-B2A4-FD6847C75367}']
    function InitializeProvider(HWnd: THandle; 
      const DMApp: IDispatch): HResult; stdcall;
    function ConfigureProvider: HResult; stdcall;
    function ExecuteAction(Action: variant): HResult; stdcall;
    function UpdateAction(Action: variant; var State: integer): HResult; stdcall;
    function GetActionProperties(Action: variant; 
      out Caption, Hint, HelpKeyword: WideString): HResult; stdcall;
    function GetActionID(Action: variant; out ID: WideString): HResult; stdcall;
    function ActionCount(out Count: integer): HResult; stdcall;
    function CloseProvider: HResult; stdcall;
  end;

