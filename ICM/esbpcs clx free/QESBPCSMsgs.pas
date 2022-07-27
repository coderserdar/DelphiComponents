{: Contains the routines for easy use of Windows Message Dialogs within
 ESBPCS for CLX.

 This is designed to work in Borland Delphi 6 CLX and above, Borland
 C++ Builder 6 CLX and above, and Borland Kylix 2 and above.
 Most if not all features will work in Kylix 1 but it is not currently supported.<p>

 These routines are simply wrappers around MessageDlg and InputQuery,
 designed to make them easier to use.<p>

 Copyright © 1999-2002 ESB Consultancy<p>

 v2.3 - 14 September 2002
}

unit QESBPCSMsgs;

{$I esbpcs.inc}

interface

uses
     QESBPCSGlobals;

{: Displays a Confirmation (ie Yes/No) Message Dialog. If Yes is pressed then
 the Result is True. Ensures Default Cursor is displayed, preserves
 state of cursor.
 @param Msg Message to Display.
 @returns True if Yes is Clicked, False if No is Clicked
 @cat Dialogs
}
function ConfirmMsg (const Msg: string): Boolean;

{: Displays a Confirmation (ie Yes/No/Cancel) Message Dialog. If Cancel is pressed
 then	the Result is False, Yes or No give a Result of True, you then
 check AnsYes to see if the Yes button was pressed. Ensures Default
 Cursor is displayed, preserves state of cursor.
 @param Msg Message to Display.
 @param AnsYes Returns True if Yes is Clicked, False if No is Clicked
 @returns True if Yes or No are Clicked, False if Cancel is Clicked
 @cat Dialogs
}
function ConfirmCancelMsg (const Msg: string; var AnsYes: Boolean): Boolean;

{: Displays an Information Message Dialog. Ensures Default Cursor is displayed,
 preserves state of cursor.
 @param Msg Message to Display.
 @cat Dialogs
}
procedure InfoMsg (const Msg: string);

{: Displays a Warning Message Dialog. Ensures Default Cursor is displayed,
 preserves state of cursor.
 @param Msg Message to Display.
 @cat Dialogs
}
procedure WarningMsg (const Msg: string);

{: Displays an Error Message Dialog. Ensures Default Cursor is displayed,
 preserves state of cursor.
 @param Msg Message to Display.
 @cat Dialogs
}
procedure ErrorMsg (const Msg: string);

{: Displays a Validation (Error) Message Dialog. Ensures
 Default Cursor is displayed, preserves state of cursor.
 @param Field Name of the Field that had the problem.
 @param Msg Message to Display.
 @cat Dialogs
}
procedure ValidationMsg (const Field, Msg: string);

{: Allows Input of an Integer in a Dialog with given Caption and Prompt.
 If the user Cancels, then the function returns False. Invalid input
 is returned as 0.Ensures Default Cursor is displayed, preserves
 state of cursor.
 @param Caption Info to Display at the top of the Dialog.
 @param Prompt Info to Display to the left of the Input Field.
 @param L Input & Output Value to be displayed & edited.
 @returns True if Ok is Clicked, False if Cancel is Clicked
 @cat Dialogs
}
function InputInt (const Caption, Prompt: string;
     var L: Int64): Boolean; overload;
function InputInt (const Caption, Prompt: string;
     var L: LongInt): Boolean; overload;
function InputInt (const Caption, Prompt: string;
     var L: LongWord): Boolean; overload;
function InputInt (const Caption, Prompt: string;
     var L: SmallInt): Boolean; overload;
function InputInt (const Caption, Prompt: string;
     var L: Word): Boolean; overload;
function InputInt (const Caption, Prompt: string;
     var L: ShortInt): Boolean; overload;
function InputInt (const Caption, Prompt: string;
     var L: Byte): Boolean; overload;

{: Allows Input of a String in a Dialog with given Caption and Prompt.
 If the user Cancels, then the function returns False. Ensures
 Default Cursor is displayed, preserves state of cursor.
 @param Caption Info to Display at the top of the Dialog.
 @param Prompt Info to Display to the left of the Input Field.
 @param S Input & Output Value to be displayed & edited.
 @returns True if Ok is Clicked, False if Cancel is Clicked
 @cat Dialogs
}
function InputStr (const Caption, Prompt: string;
     var S: string): Boolean;

implementation

uses
     QForms, QDialogs, QControls,
     QESBPCS_RS_Globals,
     QESBPCSConvert;

function ConfirmMsg (const Msg: string): Boolean;
var
     Hold: TCursor;
begin
     Hold := Screen.Cursor;
     Screen.Cursor := crDefault;
     try
          Result := MessageDlg (Msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
     finally
          Screen.Cursor := Hold;
     end;
end;

function ConfirmCancelMsg (const Msg: string; var AnsYes: Boolean): Boolean;
var
     Hold: TCursor;
     Ans: Word;
begin
     Hold := Screen.Cursor;
     Screen.Cursor := crDefault;
     try
          Ans := MessageDlg (Msg, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
          Result := Ans <> mrCancel;
          AnsYes := Ans = mrYes;
     finally
          Screen.Cursor := Hold;
     end;
end;

procedure InfoMsg (const Msg: string);
var
     Hold: TCursor;
begin
     Hold := Screen.Cursor;
     Screen.Cursor := crDefault;
     try
          MessageDlg (Msg, mtInformation, [mbOK], 0);
     finally
          Screen.Cursor := Hold;
     end;
end;

procedure WarningMsg (const Msg: string);
var
     Hold: TCursor;
begin
     Hold := Screen.Cursor;
     Screen.Cursor := crDefault;
     try
          MessageDlg (Msg, mtWarning, [mbOK], 0);
     finally
          Screen.Cursor := Hold;
     end;
end;

procedure ErrorMsg (const Msg: string);
var
     Hold: TCursor;
begin
     Hold := Screen.Cursor;
     Screen.Cursor := crDefault;
     try
          MessageDlg (Msg, mtError, [mbOK], 0);
     finally
          Screen.Cursor := Hold;
     end;
end;

procedure ValidationMsg (const Field, Msg: string);
var
     Hold: TCursor;
begin
     Hold := Screen.Cursor;
     Screen.Cursor := crDefault;
     try
          MessageDlg (rsField + ': ' + Field + #13 + RsReason + ': ' + Msg,
               mtError, [mbOK], 0);
     finally
          Screen.Cursor := Hold;
     end;
end;

function InputStr (const Caption, Prompt: string;
     var S: string): Boolean;
var
     Hold: TCursor;
begin
     Hold := Screen.Cursor;
     Screen.Cursor := crDefault;
     try
          Result := InputQuery (Caption, Prompt, S);
     finally
          Screen.Cursor := Hold;
     end;
end;

function InputInt (const Caption, Prompt: string;
     var L: Int64): Boolean;
var
     Hold: TCursor;
     TempS: string;
begin
     Hold := Screen.Cursor;
     Screen.Cursor := crDefault;
     try
          if L <> 0 then
               TempS := Int2EStr (L)
          else
               TempS := '';
          Result := InputQuery (Caption, Prompt, TempS);
          if Result then
          begin
               if TempS <> '' then
                    L := Str2Int (TempS)
               else
                    Result := False;
          end;
     finally
          Screen.Cursor := Hold;
     end;
end;

function InputInt (const Caption, Prompt: string;
     var L: LongWord): Boolean;
var
     Hold: TCursor;
     TempS: string;
begin
     Hold := Screen.Cursor;
     Screen.Cursor := crDefault;
     try
          if L <> 0 then
               TempS := Int2EStr (L)
          else
               TempS := '';
          Result := InputQuery (Caption, Prompt, TempS);
          if Result then
          begin
               if TempS <> '' then
                    L := Str2Int (TempS)
               else
                    Result := False;
          end;
     finally
          Screen.Cursor := Hold;
     end;
end;

function InputInt (const Caption, Prompt: string;
     var L: LongInt): Boolean;
var
     Hold: TCursor;
     TempS: string;
begin
     Hold := Screen.Cursor;
     Screen.Cursor := crDefault;
     try
          if L <> 0 then
               TempS := Int2EStr (L)
          else
               TempS := '';
          Result := InputQuery (Caption, Prompt, TempS);
          if Result then
          begin
               if TempS <> '' then
                    L := Str2Int (TempS)
               else
                    Result := False;
          end;
     finally
          Screen.Cursor := Hold;
     end;
end;

function InputInt (const Caption, Prompt: string;
     var L: Word): Boolean;
var
     Hold: TCursor;
     TempS: string;
begin
     Hold := Screen.Cursor;
     Screen.Cursor := crDefault;
     try
          if L <> 0 then
               TempS := Int2EStr (L)
          else
               TempS := '';
          Result := InputQuery (Caption, Prompt, TempS);
          if Result then
          begin
               if TempS <> '' then
                    L := Str2Int (TempS)
               else
                    Result := False;
          end;
     finally
          Screen.Cursor := Hold;
     end;
end;

function InputInt (const Caption, Prompt: string;
     var L: SmallInt): Boolean;
var
     Hold: TCursor;
     TempS: string;
begin
     Hold := Screen.Cursor;
     Screen.Cursor := crDefault;
     try
          if L <> 0 then
               TempS := Int2EStr (L)
          else
               TempS := '';
          Result := InputQuery (Caption, Prompt, TempS);
          if Result then
          begin
               if TempS <> '' then
                    L := Str2Int (TempS)
               else
                    Result := False;
          end;
     finally
          Screen.Cursor := Hold;
     end;
end;

function InputInt (const Caption, Prompt: string;
     var L: Byte): Boolean;
var
     Hold: TCursor;
     TempS: string;
begin
     Hold := Screen.Cursor;
     Screen.Cursor := crDefault;
     try
          if L <> 0 then
               TempS := Int2EStr (L)
          else
               TempS := '';
          Result := InputQuery (Caption, Prompt, TempS);
          if Result then
          begin
               if TempS <> '' then
                    L := Str2Int (TempS)
               else
                    Result := False;
          end;
     finally
          Screen.Cursor := Hold;
     end;
end;

function InputInt (const Caption, Prompt: string;
     var L: ShortInt): Boolean;
var
     Hold: TCursor;
     TempS: string;
begin
     Hold := Screen.Cursor;
     Screen.Cursor := crDefault;
     try
          if L <> 0 then
               TempS := Int2EStr (L)
          else
               TempS := '';
          Result := InputQuery (Caption, Prompt, TempS);
          if Result then
          begin
               if TempS <> '' then
                    L := Str2Int (TempS)
               else
                    Result := False;
          end;
     finally
          Screen.Cursor := Hold;
     end;
end;

end.
