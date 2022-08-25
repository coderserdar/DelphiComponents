{$IFDEF FPC}
   {$MODE DELPHI}{$H+}
   {$APPTYPE GUI}
{$ENDIF}
{$I cef.inc}

program cefclient;

uses
  Classes,
  Windows,
  Messages,
  SysUtils,
  ceflib,
  ceffilescheme in '..\filescheme\ceffilescheme.pas';

type
  TCustomClient = class(TCefClientOwn)
  private
    FLifeSpan: ICefBase;
    FLoad: ICefBase;
    FDisplay: ICefBase;
  protected
    function GetLifeSpanHandler: ICefBase; override;
    function GetLoadHandler: ICefBase; override;
    function GetDisplayHandler: ICefBase; override;
  public
    constructor Create; override;
  end;

  TCustomLifeSpan = class(TCefLifeSpanHandlerOwn)
  protected
    procedure OnAfterCreated(const browser: ICefBrowser); override;
  end;

  TCustomLoad = class(TCefLoadHandlerOwn)
  protected
    procedure OnLoadStart(const browser: ICefBrowser; const frame: ICefFrame); override;
    procedure OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame;
      httpStatusCode: Integer); override;
  end;

  TCustomDisplay = class(TCefDisplayHandlerOwn)
  protected
    procedure OnAddressChange(const browser: ICefBrowser;
      const frame: ICefFrame; const url: ustring); override;
    procedure OnTitleChange(const browser: ICefBrowser; const title: ustring); override;
  end;


  TScheme = class(TCefSchemeHandlerOwn)
  private
    FResponse: TMemoryStream;
    procedure Output(const str: ustring);
  protected
    function ProcessRequest(const Request: ICefRequest; var redirectUrl: ustring;
      const callback: ICefSchemeHandlerCallback): Boolean; override;
    procedure GetResponseHeaders(const response: ICefResponse; var responseLength: Int64); override;
    function ReadResponse(DataOut: Pointer; BytesToRead: Integer;
      var BytesRead: Integer; const callback: ICefSchemeHandlerCallback): Boolean; override;
  public
    constructor Create(SyncMainThread: Boolean;
      const scheme: ustring; const browser: ICefBrowser; const request: ICefRequest); override;
    destructor Destroy; override;
  end;

  TExtension = class(TCefv8HandlerOwn)
  private
    FTestParam: ustring;
  protected
    function Execute(const name: ustring; const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean; override;
  end;

type
{$IFDEF FPC}
  TWindowProc = LongInt;
{$ELSE}
  TWindowProc = Pointer;
  WNDPROC = Pointer;
{$ENDIF}

var
  Window : HWND;
  handl: ICefBase = nil;
  brows: ICefBrowser = nil;
  browsrHwnd: HWND = INVALID_HANDLE_VALUE;
  navigateto: ustring = 'http://www.google.com';

  backWnd, forwardWnd, reloadWnd, stopWnd, editWnd: HWND;
  editWndOldProc: TWindowProc;
  isLoading, canGoBack, canGoForward: Boolean;

const
  MAX_LOADSTRING = 100;
  MAX_URL_LENGTH = 255;
  BUTTON_WIDTH = 72;
  URLBAR_HEIGHT = 24;

  IDC_NAV_BACK = 200;
  IDC_NAV_FORWARD = 201;
  IDC_NAV_RELOAD = 202;
  IDC_NAV_STOP = 203;

function CefWndProc(Wnd: HWND; message: UINT; wParam: Integer; lParam: Integer): Integer; stdcall;
var
  ps: PAINTSTRUCT;
  info: TCefWindowInfo;
  rect: TRect;
  hdwp: THandle;
  x: Integer;
  strPtr: array[0..MAX_URL_LENGTH-1] of WideChar;
  strLen, urloffset: Integer;
begin
  if Wnd = editWnd then
    case message of
    WM_CHAR:
      if (wParam = VK_RETURN) then
      begin
        // When the user hits the enter key load the URL
        FillChar(strPtr, SizeOf(strPtr), 0);
        PDWORD(@strPtr)^ := MAX_URL_LENGTH;
        strLen := SendMessageW(Wnd, EM_GETLINE, 0, Integer(@strPtr));
        if (strLen > 0) then
        begin
          strPtr[strLen] := #0;
          brows.MainFrame.LoadUrl(strPtr);
        end;
        Result := 0;
      end else
        Result := CallWindowProc(WNDPROC(editWndOldProc), Wnd, message, wParam, lParam);
    else
      Result := CallWindowProc(WNDPROC(editWndOldProc), Wnd, message, wParam, lParam);
    end else
    case message of
      WM_PAINT:
        begin
          BeginPaint(Wnd, ps);
          EndPaint(Wnd, ps);
          result := 0;
        end;
      WM_CREATE:
        begin
          handl := TCustomClient.Create;
          x := 0;
          GetClientRect(Wnd, rect);

          backWnd := CreateWindowW('BUTTON', 'Back',
                                 WS_CHILD or WS_VISIBLE or BS_PUSHBUTTON
                                 or WS_DISABLED, x, 0, BUTTON_WIDTH, URLBAR_HEIGHT,
                                 Wnd, IDC_NAV_BACK, HInstance, nil);
          Inc(x, BUTTON_WIDTH);

          forwardWnd := CreateWindowW('BUTTON', 'Forward',
                                    WS_CHILD or WS_VISIBLE or BS_PUSHBUTTON
                                    or WS_DISABLED, x, 0, BUTTON_WIDTH,
                                    URLBAR_HEIGHT, Wnd, IDC_NAV_FORWARD,
                                    HInstance, nil);
          Inc(x, BUTTON_WIDTH);

          reloadWnd := CreateWindowW('BUTTON', 'Reload',
                                   WS_CHILD or WS_VISIBLE or BS_PUSHBUTTON
                                   or WS_DISABLED, x, 0, BUTTON_WIDTH,
                                   URLBAR_HEIGHT, Wnd, IDC_NAV_RELOAD,
                                   HInstance, nil);
          Inc(x, BUTTON_WIDTH);

          stopWnd := CreateWindowW('BUTTON', 'Stop',
                                 WS_CHILD or WS_VISIBLE or BS_PUSHBUTTON
                                 or WS_DISABLED, x, 0, BUTTON_WIDTH, URLBAR_HEIGHT,
                                 Wnd, IDC_NAV_STOP, HInstance, nil);
          Inc(x, BUTTON_WIDTH);

          editWnd := CreateWindowW('EDIT', nil,
                                 WS_CHILD or WS_VISIBLE or WS_BORDER or ES_LEFT or
                                 ES_AUTOVSCROLL or ES_AUTOHSCROLL or WS_DISABLED,
                                 x, 0, rect.right - BUTTON_WIDTH * 4,
                                 URLBAR_HEIGHT, Wnd, 0, HInstance, nil);

          // Assign the edit window's WNDPROC to this function so that we can
          // capture the enter key
          editWndOldProc := TWindowProc(GetWindowLong(editWnd, GWL_WNDPROC));
          SetWindowLong(editWnd, GWL_WNDPROC, LongInt(@CefWndProc));

          FillChar(info, SizeOf(info), 0);
          Inc(rect.top, URLBAR_HEIGHT);
          info.Style := WS_CHILD or WS_VISIBLE or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or WS_TABSTOP;
          info.WndParent := Wnd;
          info.x := rect.left;
          info.y := rect.top;
          info.Width := rect.right - rect.left;
          info.Height := rect.bottom - rect.top;
          CefBrowserCreate(@info, handl.Wrap, navigateto, nil);
          isLoading := False;
          canGoBack := False;
          canGoForward := False;
          SetTimer(Wnd, 1, 100, nil);
          result := 0;
        end;
      WM_TIMER:
        begin
          // Update the status of child windows
          EnableWindow(editWnd, True);
          EnableWindow(backWnd, canGoBack);
          EnableWindow(forwardWnd, canGoForward);
          EnableWindow(reloadWnd, not isLoading);
          EnableWindow(stopWnd, isLoading);
          Result := 0;
        end;
      WM_COMMAND:
        case LOWORD(wParam) of
          IDC_NAV_BACK:
            begin
              brows.GoBack;
              Result := 0;
            end;
          IDC_NAV_FORWARD:
            begin
              brows.GoForward;
              Result := 0;
            end;
          IDC_NAV_RELOAD:
            begin
              brows.Reload;
              Result := 0;
            end;
          IDC_NAV_STOP:
            begin
              brows.StopLoad;
              Result := 0;
            end;
        else
          result := DefWindowProc(Wnd, message, wParam, lParam);
        end;
      WM_DESTROY:
        begin
          brows := nil;
          PostQuitMessage(0);
          result := 0;
        end;
      WM_SETFOCUS:
        begin
          if browsrHwnd <> INVALID_HANDLE_VALUE then
            PostMessage(browsrHwnd, WM_SETFOCUS, wParam, 0);
          Result := 0;
        end;
      WM_SIZE:
        begin
          if(browsrHwnd <> INVALID_HANDLE_VALUE) then
          begin
            // Resize the browser window and address bar to match the new frame
            // window size
            GetClientRect(Wnd, rect);
            Inc(rect.top, URLBAR_HEIGHT);
            urloffset := rect.left + BUTTON_WIDTH * 4;
            hdwp := BeginDeferWindowPos(1);
         		hdwp := DeferWindowPos(hdwp, editWnd, 0, urloffset, 0, rect.right - urloffset, URLBAR_HEIGHT, SWP_NOZORDER);
            hdwp := DeferWindowPos(hdwp, browsrHwnd, 0, rect.left, rect.top,
              rect.right - rect.left, rect.bottom - rect.top, SWP_NOZORDER);
            EndDeferWindowPos(hdwp);
          end;
          result := DefWindowProc(Wnd, message, wParam, lParam);
        end;
      WM_CLOSE:
        begin
          if brows <> nil then
            brows.ParentWindowWillClose;
          result := DefWindowProc(Wnd, message, wParam, lParam);
        end
     else
       result := DefWindowProc(Wnd, message, wParam, lParam);
     end;
end;


{ TCustomClient }

constructor TCustomClient.Create;
begin
  inherited;
  FLifeSpan := TCustomLifeSpan.Create;
  FLoad := TCustomLoad.Create;
  FDisplay := TCustomDisplay.Create;
end;

function TCustomClient.GetDisplayHandler: ICefBase;
begin
  Result := FDisplay;
end;

function TCustomClient.GetLifeSpanHandler: ICefBase;
begin
  Result := FLifeSpan;
end;

function TCustomClient.GetLoadHandler: ICefBase;
begin
  Result := FLoad;
end;

{ TCustomLifeSpan }

procedure TCustomLifeSpan.OnAfterCreated(const browser: ICefBrowser);
begin
  if not browser.IsPopup then
  begin
    // get the first browser
    brows := browser;
    browsrHwnd := brows.GetWindowHandle;
  end;
end;

{ TCustomLoad }

procedure TCustomLoad.OnLoadEnd(const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
begin
  if browser.GetWindowHandle = browsrHwnd then
    isLoading := False;
end;

procedure TCustomLoad.OnLoadStart(const browser: ICefBrowser;
  const frame: ICefFrame);
begin
  if browser.GetWindowHandle = browsrHwnd then
  begin
    isLoading := True;
    canGoBack := browser.CanGoBack;
    canGoForward := browser.CanGoForward;
  end;
end;

{ TCustomDisplay }

procedure TCustomDisplay.OnAddressChange(const browser: ICefBrowser;
  const frame: ICefFrame; const url: ustring);
begin
  if (browser.GetWindowHandle = browsrHwnd) and frame.IsMain then
    SetWindowTextW(editWnd, PWideChar(url));
end;

procedure TCustomDisplay.OnTitleChange(const browser: ICefBrowser;
  const title: ustring);
begin
  if browser.GetWindowHandle = browsrHwnd then
    SetWindowTextW(Window, PWideChar(title));
end;

{ TScheme }

constructor TScheme.Create(SyncMainThread: Boolean;
  const scheme: ustring; const browser: ICefBrowser; const request: ICefRequest);
begin
  inherited;
  FResponse := TMemoryStream.Create;
end;

destructor TScheme.Destroy;
begin
  FResponse.Free;
  inherited;
end;

function TScheme.ProcessRequest(const Request: ICefRequest; var redirectUrl: ustring;
  const callback: ICefSchemeHandlerCallback): Boolean;
begin
  OutPut('<html>');
  OutPut('  <body>ClientV8ExtensionHandler says:<br><pre>');
  OutPut('<script language="javascript">');
  OutPut('  cef.test.test_param =''Assign and retrieve a value succeeded the first time.'';');
  OutPut('  document.writeln(cef.test.test_param);');
  OutPut('  cef.test.test_param = ''Assign and retrieve a value succeeded the second time.'';');
  OutPut('  document.writeln(cef.test.test_param);');
  OutPut('  var obj = cef.test.test_object();');
  OutPut('  document.writeln(obj.param);');
  OutPut('  document.writeln(obj.GetMessage());');
  OutPut('</script>');
  OutPut('</pre></body>');
  OutPut('</html>');
  FResponse.Seek(0, soFromBeginning);
  callback.HeadersAvailable;
  callback.BytesAvailable;
  Result := True;
end;

procedure TScheme.GetResponseHeaders(const response: ICefResponse;
  var responseLength: Int64);
begin
  response.Status := 200;
  response.StatusText := 'OK';
  response.MimeType := 'text/html';
  ResponseLength := FResponse.Size;
end;

function TScheme.ReadResponse(DataOut: Pointer; BytesToRead: Integer;
      var BytesRead: Integer; const callback: ICefSchemeHandlerCallback): Boolean;
begin
  BytesRead := FResponse.Read(DataOut^, BytesToRead);
  Result := True;
end;

procedure TScheme.Output(const str: ustring);
var
  u: UTF8String;
begin
{$IFDEF UNICODE}
  u := UTF8String(str);
{$ELSE}
  u := UTF8Encode(str);
{$ENDIF}
  FResponse.Write(PAnsiChar(u)^, Length(u));
end;

function TExtension.Execute(const name: ustring; const obj: ICefv8Value;
  const arguments: TCefv8ValueArray; var retval: ICefv8Value;
  var exception: ustring): Boolean;
begin
  if(name = 'SetTestParam') then
  begin
    // Handle the SetTestParam native function by saving the string argument
    // into the local member.
    if (Length(arguments) <> 1) or (not arguments[0].IsString) then
    begin
      Result := false;
      Exit;
    end;
    FTestParam := arguments[0].GetStringValue;
    Result := true;
  end
  else if(name = 'GetTestParam') then
  begin
    // Handle the GetTestParam native function by returning the local member
    // value.
    retval := TCefv8ValueRef.CreateString(Ftestparam);
    Result := true;
  end
  else if (name = 'GetTestObject') then
  begin
    // Handle the GetTestObject native function by creating and returning a
    // new V8 object.
    retval := TCefv8ValueRef.CreateObject(nil);
    // Add a string parameter to the new V8 object.
    retval.SetValueByKey('param', TCefv8ValueRef.CreateString(
        'Retrieving a parameter on a native object succeeded.'));
    // Add a function to the new V8 object.
    retval.SetValueByKey('GetMessage',
        TCefv8ValueRef.CreateFunction('GetMessage', Self));
    Result := true;
  end
  else if(name = 'GetMessage') then
  begin
    // Handle the GetMessage object function by returning a string.
    retval := TCefv8ValueRef.CreateString(
        'Calling a function on a native object succeeded.');
    Result := true;
  end else
    Result := false;
end;

const
  code =
   'var cef;'+
   'if (!cef)'+
   '  cef = {};'+
   'if (!cef.test)'+
   '  cef.test = {};'+
   '(function() {'+
   '  cef.test.__defineGetter__(''test_param'', function() {'+
   '    native function GetTestParam();'+
   '    return GetTestParam();'+
   '  });'+
   '  cef.test.__defineSetter__(''test_param'', function(b) {'+
   '    native function SetTestParam();'+
   '    if(b) SetTestParam(b);'+
   '  });'+
   '  cef.test.test_object = function() {'+
   '    native function GetTestObject();'+
   '    return GetTestObject();'+
   '  };'+
   '})();';

var
{$IFDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
  Msg      : TMsg;
{$ENDIF}
  wndClass : TWndClass;
begin
  CefCache := 'cache';
  CefLoadLibDefault;

  CefRegisterCustomScheme('client', True, False, False);
  CefRegisterCustomScheme('file', True, False, False);

  CefRegisterSchemeHandlerFactory('client', 'test', False, TScheme);
  CefRegisterSchemeHandlerFactory('file', '', False, TFileScheme);

  CefRegisterExtension('v8/test', code, TExtension.Create as ICefV8Handler);
  //navigateto := 'client://test/';
  //navigateto := 'file://c:\';
  try
    wndClass.style          := CS_HREDRAW or CS_VREDRAW;
    wndClass.lpfnWndProc    := @CefWndProc;
    wndClass.cbClsExtra     := 0;
    wndClass.cbWndExtra     := 0;
    wndClass.hInstance      := hInstance;
    wndClass.hIcon          := LoadIcon(0, IDI_APPLICATION);
    wndClass.hCursor        := LoadCursor(0, IDC_ARROW);
    wndClass.hbrBackground  := 0;
    wndClass.lpszMenuName   := nil;
    wndClass.lpszClassName  := 'chromium';

    RegisterClass(wndClass);

    Window := CreateWindow(
      'chromium',             // window class name
      'Chromium browser',     // window caption
      WS_OVERLAPPEDWINDOW or WS_CLIPCHILDREN,    // window style
      Integer(CW_USEDEFAULT), // initial x position
      Integer(CW_USEDEFAULT), // initial y position
      Integer(CW_USEDEFAULT), // initial x size
      Integer(CW_USEDEFAULT), // initial y size
      0,                      // parent window handle
      0,                      // window menu handle
      hInstance,              // program instance handle
      nil);                   // creation parameters

    ShowWindow(Window, SW_SHOW);
    UpdateWindow(Window);

{$IFNDEF CEF_MULTI_THREADED_MESSAGE_LOOP}
    CefRunMessageLoop;
{$ELSE}
    while(GetMessageW(msg, 0, 0, 0)) do
    begin
      TranslateMessage(msg);
      DispatchMessageW(msg);
    end;
{$ENDIF}
  finally
    handl := nil;
  end;
end.
