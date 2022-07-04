{**************************************************************************************}
{                                                                                      }
{ AutoCorrect components for Delphi 7                                                  }
{ Version 1.0.1 (2009-08-26)                                                           }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is CCR.AutoCorrect.Controls.pas.                                   }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009 Chris Rolliston. All Rights Reserved.         }
{                                                                                      }
{**************************************************************************************}

unit CCR.AutoCorrect.Controls;
{
  Defines simple descendents of the standard TComboBox, TEdit, TMemo and TRichEdit
  classes that add an AutoCorrectEngine property to their immediate ancestor, allowing
  you to hook up to a TAutoCorrectEngine instance at design time. Beyond that,
  TRichEditWithAutoCorrect also works around streaming bugs in the underlying control -
  specifically, v1 to v3 fail to load em- and en-dashes correctly, and v1 additionally
  fails to load smart quotes correctly. My fixes work on the understanding that if the
  source RTF uses the \'xx syntax for these characters, then they are loaded correctly.
}
interface

uses
  Windows, Messages, RichEdit, SysUtils, Classes, Controls, StdCtrls, ComCtrls,
  CCR.AutoCorrect;

type
  TComboBoxWithAutoCorrect = class(TComboBox)
  private
    FAutoCorrectEngine: IAutoCorrectEngine;
    procedure SetAutoCorrectEngine(const Value: IAutoCorrectEngine);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
  published
    property AutoCorrectEngine: IAutoCorrectEngine read FAutoCorrectEngine
      write SetAutoCorrectEngine;
  end;

  TEditWithAutoCorrect = class(TEdit)
  private
    FAutoCorrectEngine: IAutoCorrectEngine;
    procedure SetAutoCorrectEngine(const Value: IAutoCorrectEngine);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
  published
    property AutoCorrectEngine: IAutoCorrectEngine read FAutoCorrectEngine
      write SetAutoCorrectEngine;
  end;

  TMemoWithAutoCorrect = class(TMemo)
  private
    FAutoCorrectEngine: IAutoCorrectEngine;
    procedure SetAutoCorrectEngine(const Value: IAutoCorrectEngine);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
  published
    property AutoCorrectEngine: IAutoCorrectEngine read FAutoCorrectEngine
      write SetAutoCorrectEngine;
  end;

  TEMStreamIn = packed record
    Msg: UINT;
    Options: WPARAM;
    EditStream: ^TEditStream;
    Result: LRESULT;
  end;

  TRichEditWithAutoCorrect = class(TRichEdit)
  private
    FAutoCorrectEngine: IAutoCorrectEngine;
    procedure SetAutoCorrectEngine(const Value: IAutoCorrectEngine);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure EMStreamIn(var Message: TEMStreamIn); message EM_STREAMIN;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
  published
    property AutoCorrectEngine: IAutoCorrectEngine read FAutoCorrectEngine
      write SetAutoCorrectEngine;
  end;

implementation

uses ClipBrd;

var
  CF_RTF: Word;

function IsRTFHeader(const Buffer: PAnsiChar; BufferSize: Integer): Boolean;
const
  RTFHeader: PAnsiChar = '{\rtf';
  RTFHeaderSize = 5;
begin
  Result := (BufferSize >= RTFHeaderSize) and CompareMem(Buffer, RTFHeader, RTFHeaderSize);
end;

procedure FixBrokenRTFCodes(const StartPos, EndPos: PAnsiChar); //EndPos is inclusive
const
  DashOrSingleQuoteLen = 7;
  OldEmDash: PAnsiChar = '\emdash';
  NewEmDash: PAnsiChar = '\''97\xx'; 
  OldEnDash: PAnsiChar = '\endash';
  NewEnDash: PAnsiChar = '\''96\xx';
  OldSingleLQuote: PAnsiChar = '\lquote';
  NewSingleLQuote: PAnsiChar = '\''91\xx';
  OldSingleRQuote: PAnsiChar = '\rquote';
  NewSingleRQuote: PAnsiChar = '\''92\xx';
  DoubleQuoteLen = 10;
  OldDoubleLQuote: PAnsiChar = '\ldblquote';
  NewDoubleLQuote: PAnsiChar = '\''93\xxxxx';
  OldDoubleRQuote: PAnsiChar = '\rdblquote';
  NewDoubleRQuote: PAnsiChar = '\''94\xxxxx';
var
  SeekPtr: PAnsiChar;
begin
  SeekPtr := StartPos;
  repeat
    repeat
      if SeekPtr + DashOrSingleQuoteLen >= EndPos then Exit;
      Inc(SeekPtr);
      if SeekPtr^ = '\' then
        case SeekPtr[1] of
          'e', 'l', 'r': Break;
          '\': Inc(SeekPtr); //a double backslash means 'display a backslash character'
        end;
    until False;
    if CompareMem(SeekPtr, OldEmDash, DashOrSingleQuoteLen) then
      System.Move(NewEmDash^, SeekPtr^, DashOrSingleQuoteLen)
    else if CompareMem(SeekPtr, OldEnDash, DashOrSingleQuoteLen) then
      System.Move(NewEnDash^, SeekPtr^, DashOrSingleQuoteLen)
    else if CompareMem(SeekPtr, OldSingleLQuote, DashOrSingleQuoteLen) then
      System.Move(NewSingleLQuote^, SeekPtr^, DashOrSingleQuoteLen)
    else if CompareMem(SeekPtr, OldSingleRQuote, DashOrSingleQuoteLen) then
      System.Move(NewSingleRQuote^, SeekPtr^, DashOrSingleQuoteLen)
    else if (SeekPtr + DoubleQuoteLen > EndPos) then
      Break
    else if CompareMem(SeekPtr, OldDoubleLQuote, DoubleQuoteLen) then
      System.Move(NewDoubleLQuote^, SeekPtr^, DoubleQuoteLen)
    else if CompareMem(SeekPtr, OldDoubleRQuote, DoubleQuoteLen) then
      System.Move(NewDoubleRQuote^, SeekPtr^, DoubleQuoteLen)
    else
      Continue;
    Inc(SeekPtr, DashOrSingleQuoteLen - 1);
  until False;
end;

{ TComboBoxWithAutoCorrect }

procedure TComboBoxWithAutoCorrect.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if FAutoCorrectEngine <> nil then FAutoCorrectEngine.KeyDownOccurred(Self, Key, Shift);
end;

procedure TComboBoxWithAutoCorrect.KeyPress(var Key: Char);
begin
  inherited;
  if FAutoCorrectEngine <> nil then FAutoCorrectEngine.KeyPressOccurred(Self, Key);
end;

procedure TComboBoxWithAutoCorrect.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and AComponent.IsImplementorOf(FAutoCorrectEngine) then
    SetAutoCorrectEngine(nil);
end;

procedure TComboBoxWithAutoCorrect.SetAutoCorrectEngine(const Value: IAutoCorrectEngine);
begin
  ReferenceInterface(FAutoCorrectEngine, opRemove);
  FAutoCorrectEngine := Value;
  ReferenceInterface(FAutoCorrectEngine, opInsert);
end;

procedure TComboBoxWithAutoCorrect.WMUndo(var Message: TMessage);
begin
  inherited;
  if FAutoCorrectEngine <> nil then FAutoCorrectEngine.UndoOccurred(Self);
end;

{ TEditWithAutoCorrect }

procedure TEditWithAutoCorrect.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if FAutoCorrectEngine <> nil then FAutoCorrectEngine.KeyDownOccurred(Self, Key, Shift);
end;

procedure TEditWithAutoCorrect.KeyPress(var Key: Char);
begin
  inherited;
  if FAutoCorrectEngine <> nil then FAutoCorrectEngine.KeyPressOccurred(Self, Key);
end;

procedure TEditWithAutoCorrect.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and AComponent.IsImplementorOf(FAutoCorrectEngine) then
    SetAutoCorrectEngine(nil);
end;

procedure TEditWithAutoCorrect.SetAutoCorrectEngine(const Value: IAutoCorrectEngine);
begin
  ReferenceInterface(FAutoCorrectEngine, opRemove);
  FAutoCorrectEngine := Value;
  ReferenceInterface(FAutoCorrectEngine, opInsert);
end;

procedure TEditWithAutoCorrect.WMUndo(var Message: TMessage);
begin
  inherited;
  if FAutoCorrectEngine <> nil then FAutoCorrectEngine.UndoOccurred(Self);
end;

{ TMemoWithAutoCorrect }

procedure TMemoWithAutoCorrect.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if FAutoCorrectEngine <> nil then FAutoCorrectEngine.KeyDownOccurred(Self, Key, Shift);
end;

procedure TMemoWithAutoCorrect.KeyPress(var Key: Char);
begin
  inherited;
  if FAutoCorrectEngine <> nil then FAutoCorrectEngine.KeyPressOccurred(Self, Key);
end;

procedure TMemoWithAutoCorrect.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and AComponent.IsImplementorOf(FAutoCorrectEngine) then
    SetAutoCorrectEngine(nil);
end;

procedure TMemoWithAutoCorrect.SetAutoCorrectEngine(const Value: IAutoCorrectEngine);
begin
  ReferenceInterface(FAutoCorrectEngine, opRemove);
  FAutoCorrectEngine := Value;
  ReferenceInterface(FAutoCorrectEngine, opInsert);
end;

procedure TMemoWithAutoCorrect.WMUndo(var Message: TMessage);
begin
  inherited;
  if FAutoCorrectEngine <> nil then FAutoCorrectEngine.UndoOccurred(Self);
end;

{ TRichEditWithAutoCorrect }

procedure TRichEditWithAutoCorrect.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if FAutoCorrectEngine <> nil then FAutoCorrectEngine.KeyDownOccurred(Self, Key, Shift);
  if (Shift = [ssCtrl]) and (Key = Ord('V')) and not ReadOnly then
  begin                        //the rich edit control handles this directly,
    Key := 0;                  //not sending a WM_PASTE message to itself
    Perform(WM_PASTE, 0, 0);
  end;
end;

procedure TRichEditWithAutoCorrect.KeyPress(var Key: Char);
begin
  inherited;
  if FAutoCorrectEngine <> nil then FAutoCorrectEngine.KeyPressOccurred(Self, Key);
end;

procedure TRichEditWithAutoCorrect.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and AComponent.IsImplementorOf(FAutoCorrectEngine) then
    SetAutoCorrectEngine(nil);
end;

procedure TRichEditWithAutoCorrect.SetAutoCorrectEngine(const Value: IAutoCorrectEngine);
begin
  ReferenceInterface(FAutoCorrectEngine, opRemove);
  FAutoCorrectEngine := Value;
  ReferenceInterface(FAutoCorrectEngine, opInsert);
end;

type
  TStreamWithFix = record
    DoneFirst, PassStraightOn: Boolean;
    SourceCookie: Pointer;
    SourceCallBack: function (Cookie: Pointer; Buffer: PAnsiChar;//RichEdit.pas doesn't declare dwCookie correctly with 64 bit in mind - should be DWORD_PTR
      BufferSize: Longint; var BytesWritten: Longint): Longint; stdcall;
    Remainder: array of AnsiChar;
  end;

  TStreamFromClipboard = record
    SeekPtr: PAnsiChar;
    BytesLeft: Integer;
  end;

function StreamLoadFromClipboard(var Cookie: TStreamFromClipboard;
  Buffer: PAnsiChar; BufferSize: Longint; var BytesWritten: Longint): Longint; stdcall;
begin
  Result := 0;
  if Cookie.BytesLeft >= BufferSize then
    BytesWritten := BufferSize
  else
    BytesWritten := Cookie.BytesLeft;
  Move(Cookie.SeekPtr^, Buffer^, BytesWritten);
  Inc(Cookie.SeekPtr, BytesWritten);
  Dec(Cookie.BytesLeft, BytesWritten);
end;

function StreamLoadWithFix(var Cookie: TStreamWithFix; Buffer: PAnsiChar;
  BufferSize: Longint; var BytesWritten: Longint): Longint; stdcall;
var
  NewBytesWritten: Longint;
  EndPos, LastCodeBegin: PAnsiChar;
begin
  BytesWritten := Length(Cookie.Remainder);
  if BytesWritten <> 0 then
  begin
    Move(Cookie.Remainder[0], Buffer^, BytesWritten);
    Cookie.Remainder := nil;
  end;
  Result := Cookie.SourceCallBack(Cookie.SourceCookie, @Buffer[BytesWritten],
    BufferSize - BytesWritten, NewBytesWritten);
  Inc(BytesWritten, NewBytesWritten);
  if (Result <> 0) or (NewBytesWritten = 0) then Exit;
  if BytesWritten < BufferSize then
  begin
    Result := Cookie.SourceCallBack(Cookie.SourceCookie, @Buffer[BytesWritten],
      BufferSize - BytesWritten, NewBytesWritten);
    if Result <> 0 then Exit;
    Inc(BytesWritten, NewBytesWritten);
  end;
  if not Cookie.DoneFirst then
  begin
    Cookie.DoneFirst := True;
    Cookie.PassStraightOn := (@Cookie.SourceCallBack = @StreamLoadFromClipboard) or
      not IsRTFHeader(Buffer, BufferSize);
  end;
  if Cookie.PassStraightOn then Exit;
  { A relevant RTF code may straddle two calls to the source callback, hence the
    'remainder' business. }
  EndPos := @Buffer[BytesWritten - 1];
  LastCodeBegin := EndPos;
  repeat
    if LastCodeBegin^ = '\' then
      if LastCodeBegin[-1] = '\' then
        Dec(LastCodeBegin)
      else
        Break;
    Dec(LastCodeBegin);
  until (LastCodeBegin <= Buffer);
  Dec(BytesWritten, EndPos - LastCodeBegin + 1);
  FixBrokenRTFCodes(Buffer, @LastCodeBegin[-1]); //second param is inclusive
  SetLength(Cookie.Remainder, EndPos - LastCodeBegin + 1);
  Move(LastCodeBegin^, Cookie.Remainder[0], Length(Cookie.Remainder));
end;

procedure TRichEditWithAutoCorrect.EMStreamIn(var Message: TEMStreamIn);
var
  Cookie: TStreamWithFix;
begin
  if (Message.Options and SF_RTF <> 0) and (Message.EditStream <> nil) and
    Assigned(Message.EditStream.pfnCallback) then
  begin
    Cookie.DoneFirst := False;
    Cookie.SourceCookie := Pointer(Message.EditStream.dwCookie);
    Cookie.SourceCallBack := Pointer(@Message.EditStream.pfnCallback);
    Pointer(Message.EditStream.dwCookie) := @Cookie;
    Pointer(@Message.EditStream.pfnCallback) := @StreamLoadWithFix;
  end;
  inherited;
end;

procedure TRichEditWithAutoCorrect.WMPaste(var Message: TWMPaste);
var
  Cookie: TStreamFromClipboard;
  DataHandle: THandle;
  EditStream: TEditStream;
  DataStr: AnsiString;
begin
  if PlainText or not Clipboard.HasFormat(CF_RTF) then
  begin
    inherited;
    Exit;
  end;
  DataHandle := 0;
  Clipboard.Open;
  try
    DataHandle := Clipboard.GetAsHandle(CF_RTF);
    Cookie.BytesLeft := GlobalSize(DataHandle);
    SetString(DataStr, PAnsiChar(GlobalLock(DataHandle)), Cookie.BytesLeft);
    Cookie.SeekPtr := PAnsiChar(DataStr);
    FixBrokenRTFCodes(Cookie.SeekPtr, @Cookie.SeekPtr[Cookie.BytesLeft - 1]);
    Pointer(EditStream.dwCookie) := @Cookie;
    EditStream.dwError := 0;
    EditStream.pfnCallback := @StreamLoadFromClipboard;
    Perform(EM_STREAMIN, SF_RTF or SFF_SELECTION, LPARAM(@EditStream));
  finally
    if DataHandle <> 0 then GlobalUnlock(DataHandle);
    Clipboard.Close;
  end;
end;

procedure TRichEditWithAutoCorrect.WMUndo(var Message: TMessage);
begin
  inherited;
  if FAutoCorrectEngine <> nil then FAutoCorrectEngine.UndoOccurred(Self);
end;

initialization
  CF_RTF := RegisterClipboardFormat(RichEdit.CF_RTF);

end.
