unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_listbox, API_keylog, ExtCtrls, API_base;

type
  TForm1 = class(TForm)
    API_keylog1: TAPI_keylog;
    API_listbox1: TAPI_listbox;
    Timer1: TTimer;
    Memo1: TMemo;
    Label3: TLabel;
    Memo2: TMemo;
    Label4: TLabel;
    procedure API_keylog1Key(sender: TObject; key: Integer);
    procedure FormActivate(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
procedure TForm1.API_keylog1Key(sender: TObject; key: Integer);
begin
  // show key as value
  api_listbox1.items.add(inttostr(key));

  // send key to our 2nd memo
  memo2.Perform(WM_IME_CHAR, ord(key), 0);
end;

//------------------------------------------------------------------------------
// set keylogger active
procedure TForm1.FormActivate(Sender: TObject);
begin
  api_keylog1.Active:= true;
end;

//------------------------------------------------------------------------------
// we're trapping all the goodies from the memo to the notepad via this
// funny nice keydown event - it has all we need!
procedure TForm1.Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  h: hwnd;
begin

  (*
  ssShift     The Shift key is held down.
  ssAlt       The Alt key is held down.
  ssCtrl      The Ctrl key is held down.
  ssLeft      The left mouse button is held down.
  *)

  // send virtual key code to the notepad's edit field if such is foúnd
  h:= FindWindow('NotePad', nil);
  if h<>0 then
  begin

    (*
    combo:=     findwindowex(h,0,'ComboBox',nil);
    edit:=      findwindowex(h,0,'Edit',nil);
    richedit:=  findwindowex(h,0,'RichEdit',nil);
    *)

    h:= FindWindowEx(h, 0, 'Edit', nil);
    if h<>0 then
    begin
      api_keylog1.SendKey(h, key, ssShift in shift, ssCtrl in shift, ssAlt in shift, 1);
      label4.caption:= 'key send succesfully to notepad.';
    end else
      label4.caption:= 'Edit missing from notepad!?';

  end else
    label4.Caption:= 'Didn''t find notepad''s handle.';
end;

end.
