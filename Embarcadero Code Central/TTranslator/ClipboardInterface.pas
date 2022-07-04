{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: ClipboardInterface.pas,v 1.6 2003/01/02 09:03:19 laa Exp $}

unit ClipboardInterface;

interface

uses
{$ifndef LINUX}
  Windows, Messages, 
{$else LINUX}
  Qt, QClipBrd,
{$endif LINUX}
  Classes;

type
{$ifdef LINUX}
  UINT = Integer;
  HWND = QClipboardH;
{$endif LINUX}

  TAbstractFormatObject = class
  private
    FFormat : UINT;
    FRendered : Boolean;
  protected
    constructor Create(AFormat : UINT);
    procedure SetRendered(Value : Boolean);
  public
    destructor Destroy; override;
    procedure SetData(RenderNow : Boolean); virtual; abstract;

    property Format : UINT read FFormat;
    property Rendered : Boolean read FRendered;
  end;

  TBufferFormatObject = class(TAbstractFormatObject)
  private
    FBuffer : PChar;
    FSize : Integer;
    procedure SetBuffer(var Buffer; Size: Integer);
  protected
    procedure SetPCharBuffer(Buffer: PChar; Size : Integer);
  public
    constructor Create(AFormat : UINT; ABuffer : PChar; BufferSize : Integer);
    destructor Destroy; override;
    procedure SetData(RenderNow: Boolean); override;
    class function GetBufferFromClipboard(AFormat: Word; Buffer: PChar; BufSize: Integer): Integer;
  end;

  TStreamFormatObject = class(TBufferFormatObject)
  private
    FStream : TMemoryStream;
  protected
    procedure SetStream(AStream : TMemoryStream);
  public
    constructor Create(AFormat : UINT; AStream : TMemoryStream);
    destructor Destroy; override;
    procedure SetData(RenderNow: Boolean); override;
  end;

  TFormatObjList = class(TList)
  private
    function GetFormat(idx : Integer) : UINT;
    function GetFormatObject(idx : Integer) : TAbstractFormatObject;
    procedure FreeFormatObjects;
  public
    destructor Destroy; override;
    function ObjectByFormat(AFormat : UINT) : TAbstractFormatObject;

    function Add(Item: TAbstractFormatObject): Integer; //reintroduce;
    property Format[idx : Integer] : UINT read GetFormat;
    property FormatObject[idx : Integer] : TAbstractFormatObject read GetFormatObject;
    function ContainsFormat(AFormat : UINT) : Boolean;
    procedure RemoveFormat(AFormat : UINT);
  end;

  TClipboardInterface = class(TPersistent)
  private
    FFormatObjects : TFormatObjList;
    FOpenRefCount: Integer;
    FHandle: HWND;

{$ifndef LINUX}
    procedure WMRenderFormat(var Msg: TWMRenderFormat); message WM_RENDERFORMAT;
{$else LINUX}
    FOwnsData : Boolean;

    // Fixa Kylix MVJ
    procedure ClipboardChangedNotification; cdecl;
{$endif LINUX}
{
    procedure WMDestroyClipboard(var Msg: TWMDestroyClipboard); message WM_DESTROYCLIPBOARD;
    procedure WMRenderAllFormats(var Msg: TWMRenderAllFormats); message WM_RENDERALLFORMATS;
}
    procedure WndProc(var TheMessage: TMessage);

    procedure _DoRenderFormat(AFormat : UINT; RenderNow : Boolean);
    procedure _DoForceRenderFormat(AFormat : UINT; RenderNow : Boolean);
    procedure _DoRenderAll(RenderNow : Boolean);
    procedure _DoForceRenderAll(RenderNow : Boolean);

    function GetAsText : String;
    procedure SetAsText(AString : String);
    procedure SetAsMemoryStream(AStream : TMemoryStream);
  protected
    procedure AddFormatObject(AObj : TAbstractFormatObject);
    constructor Create; //reintroduce;
    property FormatObjects : TFormatObjList read FFormatObjects;
  public
    destructor Destroy; override;
    procedure CheckForUnrendered;

    function ContainsFormat(AFormat : UINT) : Boolean;
    function ContainsUnrendered : Boolean;
    function GetAsFormatText( AFormat : UINT ) : String;

    procedure Open;
    procedure Close;
    procedure Clear;
    procedure RenderAll;
    procedure RenderFormat( AFormat : UINT );

    property AsText : String read GetAsText write SetAsText;
    property AsMemoryStream : TMemoryStream write SetAsMemoryStream;
    property Handle : HWND read FHandle;
  end;

{$ifdef LINUX}
  function OpenClipboard(Handle : HWND) : Boolean;
  procedure EmptyClipboard;
  procedure CloseClipboard;
  function IsClipboardFormatAvailable( AFormat : UINT ) : Boolean;

  const
    CF_TEXT = 0;
    NO_HANDLE = nil;
{$else LINUX}
  const
    NO_HANDLE = 0;
{$endif LINUX}

var
  ClipbrdInterface : TClipboardInterface;

implementation

uses
  SysUtils,
{$ifndef LINUX}
  Dialogs, Forms, Controls,
{$else LINUX}
  QConsts, QDialogs, QForms, QControls,
{$endif LINUX}
  DataEditorConstants;

const
  MSGE_CantAddToClosedClipbrdInteface = 'Can not add objects to Clipboard Interface when it is closed!';
  MSGE_CantEmptyClosedClipbrdInteface = 'Can not empty Clipboard Interface when it is closed!';
//  MSGE_CreateIntefaceShouldBeUsed = 'CreateInterface should be used when creating a Clipboard Interface!';

{var
  WindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @DefWindowProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TClipbrdInterfaceWindow');
}

{$ifdef LINUX}
var
  HWND_ClipBrd : HWND = nil;

function OpenClipboard(Handle : HWND) : Boolean;
begin
  Result := True;
  if HWND_ClipBrd = nil then
    HWND_ClipBrd := Handle
  else
    Result := False;
end;

procedure EmptyClipboard;
begin
  if HWND_ClipBrd <> nil then
    QClipboard_clear(HWND_ClipBrd)
  else
    raise Exception.Create('Can not empty clipboard! No handle to clipboard!');
end;

procedure CloseClipboard;
begin
  HWND_ClipBrd := nil;
end;

function IsClipboardFormatAvailable( AFormat : UINT ) : Boolean;
begin
  Result := False;
end;
{$endif LINUX}

constructor TClipboardInterface.Create;
begin
  inherited Create;

  FOpenRefCount := 0;
  FFormatObjects := TFormatObjList.Create;

{$ifndef LINUX}
  FHandle := AllocateHWnd( Self.WndProc );
{$else LINUX}
  // FIXA Kylix MVJ
  FHandle := nil;
{$endif LINUX}
end;

destructor TClipboardInterface.Destroy;
begin
  inherited Destroy;

{$ifndef LINUX}
  DeallocateHWnd(Handle);
  FHandle := 0;
{$else LINUX}
  FHandle := nil;
{$endif LINUX}

  FormatObjects.Free;
  FFormatObjects := nil;
end;

procedure TClipboardInterface.CheckForUnrendered;
begin
{$ifndef LINUX}
  if (GetClipboardOwner = ClipbrdInterface.Handle) and
     ClipbrdInterface.ContainsUnrendered then
{$else LINUX}
  if FOwnsData and
     ClipbrdInterface.ContainsUnrendered then
{$endif LINUX}
  begin
    case MessageDlg('There is data on the Clipboard.' + #13#10 +
                    'Do you want to be able to use it in other applications?',
                    mtConfirmation, [mbYes, mbNo], 0) of
      mrYes : ClipbrdInterface.RenderAll;
      mrNo :;
    end;
  end;
end;

procedure TClipboardInterface.AddFormatObject(AObj: TAbstractFormatObject);
begin
  if FOpenRefCount = 0 then
    raise EInvalidOperation.Create(Self.ClassName + '.AddFormatObject:' +
          ' ' + MSGE_CantAddToClosedClipbrdInteface);

  FormatObjects.Add(AObj);
end;

function TClipboardInterface.ContainsFormat(AFormat : UINT) : Boolean;
begin
  Result := FormatObjects.ContainsFormat(AFormat);
end;

function TClipboardInterface.ContainsUnrendered : Boolean;
var
  iItem : Integer;
begin
  Result := False;
  for iItem := 0 to FormatObjects.Count -1 do
    if not FormatObjects.FormatObject[iItem].Rendered then
    begin
      Result := True;
      Break;
    end;
end;

procedure TClipboardInterface.Clear;
begin
  FormatObjects.FreeFormatObjects;
end;

procedure TClipboardInterface.Open;
begin
  Inc(FOpenRefCount);
end;

procedure TClipboardInterface.Close;
begin
  if FOpenRefCount = 0 then
    Exit;
  Dec(FOpenRefCount);
  if FOpenRefCount = 0 then
  begin
    if not OpenClipboard(Handle) then
{      raise Exception.CreateRes(@SCannotOpenClipboard)};

    try
      EmptyClipboard;

      _DoRenderAll(False);
    finally
      CloseClipboard;
    end;
  end;
end;

function TClipboardInterface.GetAsText : String;
begin
  GetAsFormatText( CF_Text );
end;

function TClipboardInterface.GetAsFormatText(AFormat: UINT): String;
var
  Data: THandle;
begin
  OpenClipboard(Handle);
{$ifndef LINUX}
  Data := GetClipboardData(AFormat);
  try
    if Data <> 0 then
      Result := PChar(GlobalLock(Data))
    else
      Result := '';
  finally
    if Data <> 0 then GlobalUnlock(Data);
      CloseClipboard;
  end;
{$else LINUX}
  CloseClipboard;
{$endif LINUX}
end;

procedure TClipboardInterface.SetAsText(AString : String);
var
  AStream : TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  if AString <> '' then
    AStream.Write( AString[1], Length(AString) );
  AStream.Write( KEY_NULL, 1 );
  SetAsMemoryStream(AStream);
end;

procedure TClipboardInterface.SetAsMemoryStream(AStream : TMemoryStream);
begin
  TStreamFormatObject.Create(CF_TEXT, AStream);
end;

procedure TClipboardInterface.RenderFormat(AFormat: UINT);
begin
  if OpenClipboard(NO_HANDLE) then
    _DoRenderFormat( AFormat, True );
  CloseClipboard;
end;

procedure TClipboardInterface.RenderAll;
begin
  if OpenClipboard(NO_HANDLE) then
{    raise Exception.CreateRes(@SCannotOpenClipboard)};

  try
    EmptyClipboard;

    _DoForceRenderAll(True);
  finally
    CloseClipboard;
  end;
end;

procedure TClipboardInterface._DoRenderFormat(AFormat : UINT; RenderNow : Boolean);
var
  AObj : TAbstractFormatObject;
begin
  AObj := FormatObjects.ObjectByFormat(AFormat);
  if AObj = nil then
    raise Exception.Create(Self.ClassName + '._DoRenderFormat:' + ' ' +
                         'Can''t render format!');
  if not AObj.Rendered or
     not RenderNow then
  begin
    AObj.SetData(RenderNow);
{$ifdef LINUX}
    FOwnsData := not RenderNow or FOwnsData;
{$endif LINUX}
  end;
end;

procedure TClipboardInterface._DoForceRenderFormat(AFormat : UINT; RenderNow : Boolean);
begin
  FormatObjects.ObjectByFormat(AFormat).SetRendered(False);
  _DoRenderFormat(AFormat, RenderNow);
end;

procedure TClipboardInterface._DoRenderAll(RenderNow : Boolean);
var
  iObj : Integer;
begin
  for iObj := 0 to FormatObjects.Count -1 do
    _DoRenderFormat(FormatObjects.Format[iObj], RenderNow);
end;

procedure TClipboardInterface._DoForceRenderAll(RenderNow : Boolean);
var
  iObj : Integer;
begin
  for iObj := 0 to FormatObjects.Count -1 do
    _DoForceRenderFormat(FormatObjects.Format[iObj], RenderNow);
end;

procedure TClipboardInterface.WndProc(var TheMessage: TMessage);
begin
{$ifndef LINUX}
  with TheMessage do
  begin
    case Msg of
//      WM_RENDERALLFORMATS : WMRenderAllFormats(TWMRenderAllFormats(TheMessage));
      WM_RENDERFORMAT     : WMRenderFormat(TWMRenderFormat(TheMessage));
//      WM_DESTROYCLIPBOARD : WMDestroyClipboard(TWMDestroyClipboard(TheMessage));
      else
       {call the default window procedure for all unhandled messages}
       Result := DefWindowProc(Application.Handle, Msg, WParam, LParam);
    end;
  end;
{$else LINUX}
  inherited;
{$endif LINUX}
end;

{$ifndef LINUX}
procedure TClipboardInterface.WMRenderFormat(var Msg: TWMRenderFormat);
begin
  if ContainsFormat(Msg.Format) then
  begin
    _DoRenderFormat(Msg.Format, True);
    Msg.Result := 1;
  end
  else
    inherited;
end;

{$else LINUX}

procedure TClipboardInterface.ClipboardChangedNotification;
begin
  FOwnsData := False;
end;

{$endif LINUX}

{
procedure TClipboardInterface.WMRenderAllFormats(var Msg: TWMRenderAllFormats);
begin
  Msg.Result := 1;
end;

procedure TClipboardInterface.WMDestroyClipboard(var Msg: TWMDestroyClipboard);
begin
  inherited;
end;
}
{ TFormatObjList }

destructor TFormatObjList.Destroy;
begin
  FreeFormatObjects;

  inherited Destroy;
end;

procedure TFormatObjList.FreeFormatObjects;
var
  iItem : Integer;
begin
  for iItem := Count -1 downto 0 do
    FormatObject[iItem].Free;
  Clear;
end;

function TFormatObjList.Add(Item: TAbstractFormatObject): Integer;
begin
  RemoveFormat(Item.Format);

  Result := Inherited Add(Item);
end;

function TFormatObjList.ContainsFormat(AFormat : UINT) : Boolean;
var
  iItem : Integer;
begin
  Result := False;
  for iItem := 0 to Count -1 do
  begin
    if Format[iItem] = AFormat then
    begin
      Result := True;
      Break
    end;
  end;
end;

procedure TFormatObjList.RemoveFormat(AFormat : UINT);
var
  AObj : TAbstractFormatObject;
begin
  AObj := ObjectByFormat(AFormat);
  while AObj <> nil do
  begin
    Remove(AObj);
    AObj := ObjectByFormat(AFormat);
  end;
end;

function TFormatObjList.GetFormat(idx: Integer): UINT;
begin
  Result := FormatObject[idx].Format;
end;

function TFormatObjList.GetFormatObject(idx: Integer): TAbstractFormatObject;
begin
  Result := TAbstractFormatObject(Items[idx]);
end;

function TFormatObjList.ObjectByFormat(AFormat: UINT): TAbstractFormatObject;
var
  iItems : Integer;
begin
  Result := nil;
  for iItems := 0 to Count -1 do
    if Format[iItems] = AFormat then
    begin
      Result := FormatObject[iItems];
      Break;
    end;
end;

{ TAbstractFormatObject }

constructor TAbstractFormatObject.Create(AFormat: UINT);
begin
  inherited Create;

  FRendered := False;
  FFormat := AFormat;
  ClipbrdInterface.AddFormatObject(Self);
end;

destructor TAbstractFormatObject.Destroy;
begin
  ClipbrdInterface.FormatObjects.Remove(Self);
  inherited Destroy;
end;

procedure TAbstractFormatObject.SetRendered(Value : Boolean);
begin
  FRendered := Value;
end;

{ TBufferFormatObject }

constructor TBufferFormatObject.Create(AFormat: UINT; ABuffer: PChar; BufferSize : Integer);
begin
  Inherited Create(AFormat);
  FBuffer := ABuffer;
  FSize := BufferSize;
end;

destructor TBufferFormatObject.Destroy;
begin
  inherited;
end;

procedure TBufferFormatObject.SetData(RenderNow: Boolean);
begin
{$ifndef LINUX}
  if not RenderNow then
    SetClipboardData(Format, 0)
  else
    SetPCharBuffer(FBuffer, FSize);
  SetRendered(RenderNow);
{$endif LINUX}
end;

procedure TBufferFormatObject.SetBuffer(var Buffer; Size: Integer);
var
  Data: THandle;
  DataPtr: Pointer;
begin
{$ifndef LINUX}
  try
    Data := GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE, Size);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(Buffer, DataPtr^, Size);
        SetClipboardData(Format, Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
  end;
{$endif LINUX}
end;

class function TBufferFormatObject.GetBufferFromClipboard(AFormat: Word; Buffer: PChar;
               BufSize: Integer): Integer;
var
  Data: THandle;
begin
{$ifndef LINUX}
  OpenClipboard(ClipbrdInterface.Handle);
  Data := GetClipboardData(AFormat);
  if Data = 0 then
    Result := 0
  else
  begin
    Result := StrLen(StrLCopy(Buffer, GlobalLock(Data), BufSize - 1));
    GlobalUnlock(Data);
  end;
  CloseClipboard;
{$endif LINUX}
end;

procedure TBufferFormatObject.SetPCharBuffer(Buffer: PChar; Size : Integer);
begin
  SetBuffer(Buffer^, Size);
end;

{ TStreamFormatObject }

constructor TStreamFormatObject.Create(AFormat: UINT;
  AStream: TMemoryStream);
begin
  inherited Create(AFormat, nil, 0);

  FStream := AStream;
end;

destructor TStreamFormatObject.Destroy;
begin
  inherited;

  FStream.Free;
end;

procedure TStreamFormatObject.SetStream(AStream : TMemoryStream);
begin
  if FStream <> nil then
    FStream.Free;
  FStream := AStream;
  SetRendered(False);
end;

procedure TStreamFormatObject.SetData(RenderNow: Boolean);
begin
  if RenderNow then
    SetPCharBuffer(FStream.Memory, FStream.Size)
  else
    Inherited SetData(RenderNow);
  SetRendered(RenderNow);
end;

initialization
  ClipbrdInterface := TClipboardInterface.Create;

finalization
  ClipbrdInterface.Destroy;

end.


