unit API_base;

//------------------------------------------------------------------------------
// API Base
// created to save some writing on the components, this unit will hold all the
// base types of the components used in this package -> meaning, every compoennt
// has version string commmon and also there is some small constants etc. that
// many of the component shares.
//------------------------------------------------------------------------------
//
// 14102009, ari pikivirta
//  * added BinToInt function
//
// 09062009, ari pikivirta
//  * added Floor function
//  * added TickDiff(StartTick, EndTick) function
//
// 20052009, ari pikivirta
//  * added constant DTMONESECOND
//
// 16062008, ari pikivirta
//  * added functions to work with bits
//
// 13082007, ari pikivirta
//  * created and all components modified on the main package
//

interface

uses
  Windows, Classes, Controls, Messages, StdCtrls, ExtCtrls;

{$include 'inc\CompilerVersions.INC'}

const
  DTMONESECOND = 1/(24*60*60); // of tdatetime (windows)

  // bit definitions
  BIT0 = 1;
  BIT1 = 2;
  BIT2 = 4;
  BIT3 = 8;
  BIT4 = 16;
  BIT5 = 32;
  BIT6 = 64;
  BIT7 = 128;
  BIT8 = 256;
  BIT9 = 512;
  BIT10 = 1024;
  BIT11 = 2048;
  BIT12 = 4096;
  BIT13 = 8192;
  BIT14 = 16384;
  BIT15 = 32768;

type
  // custom TComponent - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  TAPI_Custom_Component = class(TComponent)
  private
    fversion: string;
  published
    property Version: string read fversion write fversion stored false;
  end;

  // custom TPaintBox - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  TAPI_Custom_PaintBox = class(TPaintbox)
  private
    fversion: string;
    fonmouseenter: tnotifyevent;
    fonmouseleave: tnotifyevent;
  protected
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  published
    property Version: string read fversion write fversion stored false;
    property OnMouseEnter: tnotifyevent read fonmouseenter write fonmouseenter;
    property OnMouseLeave: tnotifyevent read fonmouseleave write fonmouseleave;
  end;

  // custom TPanel - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  TAPI_Custom_Panel = class(TPanel)
  private
    fversion: string;
    fonmouseenter: tnotifyevent;
    fonmouseleave: tnotifyevent;
  protected
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  published
    property Version: string read fversion write fversion stored false;
    property OnMouseEnter: tnotifyevent read fonmouseenter write fonmouseenter;
    property OnMouseLeave: tnotifyevent read fonmouseleave write fonmouseleave;
  end;

  // custom TListbox - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  TAPI_Custom_ListBox = class(TListBox)
  private
    fversion: string;
    fonmouseenter: tnotifyevent;
    fonmouseleave: tnotifyevent;
  protected
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  published
    property Version: string read fversion write fversion stored false;
    property OnMouseEnter: tnotifyevent read fonmouseenter write fonmouseenter;
    property OnMouseLeave: tnotifyevent read fonmouseleave write fonmouseleave;
  end;

  TAPI_Custom_Edit = class(TEdit)
  private
    fversion: string;
    fonmouseenter: tnotifyevent;
    fonmouseleave: tnotifyevent;
  protected
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  published
    property Version: string read fversion write fversion stored false;
    property OnMouseEnter: tnotifyevent read fonmouseenter write fonmouseenter;
    property OnMouseLeave: tnotifyevent read fonmouseleave write fonmouseleave;
  end;

  TAPI_Custom_Label = class(TLabel)
  private
    fversion: string;
    fonmouseenter: tnotifyevent;
    fonmouseleave: tnotifyevent;
  protected
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  published
    property Version: string read fversion write Fversion stored false;
    property OnMouseEnter: tnotifyevent read fonmouseenter write fonmouseenter;
    property OnMouseLeave: tnotifyevent read fonmouseleave write fonmouseleave;
  end;

// FUNCTIONS AND PROCEDURES AVAILABLE
//------------------------------------------------------------------------------
// some of these are here just because they don't have better place
// in the package yet - some for because they're just the basics that
// is needed more than once and does not fit into API_strings for example.

procedure SetBit (var OfByte: Integer; Const BitIndex: integer; Const State: Boolean); overload;
procedure SetBit (var OfByte: Byte; Const BitIndex: byte; Const State: Boolean); overload;
function  BitIsSet (Const OfByte, BitIndex: Integer): Boolean; //overload;
function  BinToInt(Const ABinStr: AnsiString): Integer;
function  Ceil(Const value: double): integer;
function  Floor(Const value: double): integer;
function  TickDiff(const StartTick, EndTick: LongWord): LongWord;

// loading DLL files
function  LoadAndRunDLLProcedure(Const DLLfile, FunctionName: AnsiString; Const ShowMessages: Boolean = FALSE ): boolean;

implementation

//------------------------------------------------------------------------------
function BinToInt(Const ABinStr: AnsiString): Integer;
var
  tmpS: AnsiString;
  p, c: Integer;
begin
  result:= 0; // assume we'll fail !=)
  tmpS:= ansistring(ABinStr); // convert to ansi string
  if (length(tmpS)<1) or (length(tmpS)>32) then exit; // range check binary
  //
  // start conversion to numeric
  c:= 0;
  for p:=length(ABinStr) downto 1 do
  begin
    SetBit(result, c, tmpS[p]='1');
    inc(c, 1);
  end;
end;

//------------------------------------------------------------------------------
function TickDiff(const StartTick, EndTick : LongWord): LongWord;
begin
  if EndTick >= StartTick then Result := EndTick - StartTick
    else Result := High(LongWord) - StartTick + EndTick;
end;

//------------------------------------------------------------------------------
function Ceil (Const value: double): integer;
begin
  if frac(value)>0 then result:= trunc(value)+1
    else result:= trunc(value);
end;

//------------------------------------------------------------------------------
function Floor(Const value: double): integer;
begin
  result:= trunc(value); // cut all decimals
end;

//------------------------------------------------------------------------------
procedure SetBit (var OfByte: integer; Const BitIndex: integer; Const State: boolean);
begin
  if state then ofbyte:=ofbyte or (1 shl Bitindex)
    else ofbyte:=ofbyte and ($FFFFFFFF xor (1 shl Bitindex));
end;

procedure SetBit (var OfByte: byte; Const BitIndex:byte; Const State: boolean);
begin
  if state then ofbyte:=ofbyte or (1 shl Bitindex)
    else ofbyte:=ofbyte and ($FF xor (1 shl Bitindex));
end;

function BitIsSet (Const OfByte, BitIndex: integer): boolean;
begin
  result:=(ofbyte and (1 shl Bitindex))<>0;
end;

//------------------------------------------------------------------------------
function LoadAndRunDLLProcedure(Const DLLfile, FunctionName: AnsiString; Const ShowMessages: Boolean = FALSE): boolean;
type
  // define the type of "function" we're calling
  // this is only procedure without any paramters..
  TFunc_Start = procedure;
var
  Func_Start: TFunc_Start;
  hDll: THandle;
  FuncPtr: TFarProc;
  sMsg: AnsiString;
begin
  Result:= False;
  hDll:= LoadLibrary( PChar( DLLfile ) );
  if (hDll>32) then
  begin
    FuncPtr:=
      GetProcAddress(
        hDll, PChar( FunctionName ) );
    @Func_Start:= FuncPtr;
    if (nil<>@Func_Start) then
    begin
      Func_Start;
      Result:= True;
    end else
    begin
      if ShowMessages then
      begin
        sMsg:= 'DLL entry point '+ FunctionName + ' not found';
        MessageBox(0, PAnsiChar(sMsg), 'Error', MB_OK);
      end;
    end;
    FreeLibrary( hDll );
  end else
  begin
    if ShowMessages then
    begin
      sMsg := 'File ' + DLLfile + ' not found';
      MessageBox(0, PAnsiChar( sMsg ), 'Error', MB_OK );
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_Custom_PaintBox.MouseEnter(var Message: TMessage);
begin
  if assigned(fonmouseenter) then
    fonmouseenter(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_Custom_PaintBox.MouseLeave(var Message: TMessage);
begin
  if assigned(fonmouseleave) then
    fonmouseleave(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_Custom_Panel.MouseEnter(var Message: TMessage);
begin
  if assigned(fonmouseenter) then
    fonmouseenter(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_Custom_Panel.MouseLeave(var Message: TMessage);
begin
  if assigned(fonmouseleave) then
    fonmouseleave(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_Custom_Listbox.MouseEnter(var Message: TMessage);
begin
  if assigned(fonmouseenter) then
    fonmouseenter(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_Custom_Listbox.MouseLeave(var Message: TMessage);
begin
  if assigned(fonmouseleave) then
    fonmouseleave(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_Custom_Edit.MouseEnter(var Message: TMessage);
begin
  if assigned(fonmouseenter) then
    fonmouseenter(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_Custom_Edit.MouseLeave(var Message: TMessage);
begin
  if assigned(fonmouseleave) then
    fonmouseleave(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_Custom_Label.MouseEnter(var Message: TMessage);
begin
  if assigned(fonmouseenter) then
    fonmouseenter(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_Custom_Label.MouseLeave(var Message: TMessage);
begin
  if assigned(fonmouseleave) then
    fonmouseleave(self);
end;

end.

