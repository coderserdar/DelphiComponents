unit OOPReg;

{$IFDEF VER140} {$DEFINE D6New} {$ENDIF}
{$IFDEF VER150} {$DEFINE D6New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D6New} {$ENDIF}
{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D7New} {$ENDIF}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}


interface

procedure Register;
implementation

uses Classes, Controls, OOPHilit, PlusMemo, PMSupport, PMemoReg, ExtHilit, PlusKeys, Dialogs,
    {$IFDEF D6New} DesignEditors, DesignIntf; {$ELSE} DsgnIntf; {$ENDIF}

type TOOPKeywordsProperty = class(TKeywordsProperty)
                                 public
                                     procedure Edit; override;
                                 end;


procedure TOOPKeywordsProperty.Edit;
var
  MyHighlight: TOOPHighlighter;
  MyList     : TExtKeywordList;
  tmplist    : TKeywordList;
  i          : Integer;
begin
  MyHighlight:= GetComponent(0) as TOOPHighlighter;
  MyList:= TExtHighlighter(MyHighlight).Keywords;
  tmplist:= TKeywordList.Create;
  tmplist.Assign(MyList);
  if EditKeywordList(tmplist, False, False) then
    begin
      for i:= 0 to tmplist.Count-1 do
        if i<MyList.Count then
            pKeyInfoLen(MyList.KeyList[i])^.BasicPart:= pKeyInfoLen(tmplist.KeyList[i])^.BasicPart;
      Modified;  
      MyHighlight.ReApplyKeys
    end;
  tmplist.Free
end;


procedure Register;
begin
  RegisterComponents({UCONVERT}'PlusMemo'{/UCONVERT}, [TOOPHighlighter]);
  RegisterPropertyEditor(TypeInfo(TOOPKeywordList), TOOPHighlighter, 'KeywordsOvr', TOOPKeywordsProperty);
end;

function Confirm(Msg: AnsiString): Boolean;
begin
  Result:= MessageDlg(Msg, mtConfirmation,  [mbOk, mbCancel], 0)=mrOK
end;

begin
  OOPHilit.ConfirmFunc:= Confirm;

end.

