unit CPPReg;

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

uses Classes, Controls, CPPHilit, PlusMemo, PMSupport, PMemoReg, ExtHilit, PlusKeys, Dialogs, 
    {$IFDEF D6New} DesignEditors, DesignIntf; {$ELSE} DsgnIntf; {$ENDIF}

type TCPPKeywordsProperty = class(TKeywordsProperty)
                                 public
                                     procedure Edit; override;
                                 end;


procedure TCPPKeywordsProperty.Edit;
var
  MyHighlight: TCPPHighlighter;
  MyList     : TExtKeywordList;
  tmplist    : TKeywordList;
  i          : Integer;
begin
  MyHighlight:= GetComponent(0) as TCPPHighlighter;
  MyList:= TExtHighlighter(MyHighlight).Keywords;
  tmplist:= TKeywordList.Create;
  tmplist.Assign(MyList);
  if EditKeywordList(tmplist, False, False) then
    begin
      for i:= 0 to tmplist.Count-1 do
        if i<MyList.Count then pKeyInfoLen(MyList.KeyList[i])^.BasicPart:= pKeyInfoLen(tmplist.KeyList[i])^.BasicPart;
      Modified;  
      MyHighlight.ReApplyKeys
    end;
  tmplist.Free
end;


procedure Register;
begin
  RegisterComponents({UCONVERT}'PlusMemo'{/UCONVERT}, [TCPPHighlighter]);
  RegisterPropertyEditor(TypeInfo(TCPPKeywordList), TCPPHighlighter, 'KeywordsOvr', TCPPKeywordsProperty);
end;

function Confirm(Msg: AnsiString): Boolean;
begin
  Result:= MessageDlg(Msg, mtConfirmation,  [mbOk, mbCancel], 0)=mrOK
end;

begin
  CPPHilit.ConfirmFunc:= Confirm;

end.

