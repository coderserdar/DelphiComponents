unit pmActions;

{ © Electro-Concept Mauricie, 2003 }
{ Implements standard edit actions that works with TPlusMemo *and* TCustomEdit components (where applicable)
  Once this file is in a package, PlusMemo standard edit actions become available at
  design time, under the category 'Edit' and 'Search':
    TpmEditCut, TpmEditCopy, TpmEditPaste, TpmEditDelete, TpmEditSelectAll, TpmEditUndo, TpmEditRedo;
    TpmSearchFind, TpmSearchFindFirst, TpmSearchFindNext, TpmSearchReplace;

 }

{$IFDEF VER130} {$DEFINE D5New} {$ENDIF}    
{$IFDEF VER140} {$DEFINE D5New} {$ENDIF}
{$IFDEF VER150} {$DEFINE D5New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D5New} {$ENDIF}

{$IFDEF VER140} {$DEFINE D6New} {$ENDIF}
{$IFDEF VER150} {$DEFINE D6New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D6New} {$ENDIF}

{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D7New} {$ENDIF}

{$B-}  { not complete boolean evaluation }
{$H+}  { long strings }
{$J+}  { writeable typed constants }

{$DEFINE pmActions}

{UCONVERT}
  {$IFDEF pmActionsClx}
    {$DEFINE pmClx}
  {$ENDIF}
{/UCONVERT}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

interface

{$IFDEF pmClx}
uses QStdActns, Classes;
{$ELSE}
uses StdActns, Classes;
{$ENDIF}

type
  TpmEditCut = class(TEditCut)
    public
      constructor Create(AOwner: TComponent); override;
      function HandlesTarget(Target: TObject): Boolean; override;
      procedure ExecuteTarget(Target: TObject); override;
      procedure UpdateTarget(Target: TObject); override;
    end;

  TpmEditCopy = class(TEditCopy)
    public
      constructor Create(AOwner: TComponent); override;
      function HandlesTarget(Target: TObject): Boolean; override;
      procedure ExecuteTarget(Target: TObject); override;
      procedure UpdateTarget(Target: TObject); override;
    end;

  TpmEditPaste = class(TEditPaste)
    public
      constructor Create(AOwner: TComponent); override;
      function HandlesTarget(Target: TObject): Boolean; override;
      procedure ExecuteTarget(Target: TObject); override;
      procedure UpdateTarget(Target: TObject); override;
    end;

  TpmEditSelectAll = class({$IFDEF D5New} TEditSelectAll {$ELSE} TEditAction {$ENDIF})
    public
      constructor Create(AOwner: TComponent); override;
      function HandlesTarget(Target: TObject): Boolean; override;
      procedure ExecuteTarget(Target: TObject); override;
      procedure UpdateTarget(Target: TObject); override;
    end;

  TpmEditDelete = class({$IFDEF D5New} TEditDelete {$ELSE} TEditAction {$ENDIF})
    public
      constructor Create(AOwner: TComponent); override;
      function HandlesTarget(Target: TObject): Boolean; override;
      procedure ExecuteTarget(Target: TObject); override;
      procedure UpdateTarget(Target: TObject); override;
    end;

  {$IFDEF pmClx}
    {$IFNDEF D7New}
      TEditUndo = TEditAction;
    {$ENDIF}
  {$ENDIF}
  
  TpmEditUndo = class({$IFDEF D5New} TEditUndo {$ELSE} TEditAction {$ENDIF})
    public
      constructor Create(AOwner: TComponent); override;
      function HandlesTarget(Target: TObject): Boolean; override;
      procedure ExecuteTarget(Target: TObject); override;
      procedure UpdateTarget(Target: TObject); override;
    end;

  TpmEditRedo = class(TEditAction)
    public
      constructor Create(AOwner: TComponent); override;
      function HandlesTarget(Target: TObject): Boolean; override;
      procedure ExecuteTarget(Target: TObject); override;
      procedure UpdateTarget(Target: TObject); override;
    end;

  {$IFNDEF pmClx}
  {$IFDEF D6New}
  TpmSearchFind = class(TSearchFind)
    public
      constructor Create(AOwner: TComponent); override;
      function HandlesTarget(Target: TObject): Boolean; override;
      procedure Search(Sender: TObject); override;
      procedure UpdateTarget(Target: TObject); override;
    end;

  TpmSearchFindFirst = class(TSearchFindFirst)
    public
      constructor Create(AOwner: TComponent); override;
      function HandlesTarget(Target: TObject): Boolean; override;
      procedure Search(Sender: TObject); override;
      procedure UpdateTarget(Target: TObject); override;
    end;

  TpmSearchFindNext = class(TSearchFindNext)
    public
      constructor Create(AOwner: TComponent); override;
      procedure ExecuteTarget(Target: TObject); override;
    end;

  TpmSearchReplace = class(TSearchReplace)
    private
      procedure Replace(Sender: TObject);
    public
      constructor Create(AOwner: TComponent); override;
      procedure ExecuteTarget(Target: TObject); override;
      function HandlesTarget(Target: TObject): Boolean; override;
      procedure Search(Sender: TObject); override;
      procedure UpdateTarget(Target: TObject); override;
    end;
  {$ENDIF}
  {$ENDIF}

procedure Register;

implementation

{$IFDEF pmClx}
uses SysUtils, QControls, QForms, QDialogs, QActnList, PlusMemoClx, PMSupportClx;
{$ELSE}
uses SysUtils, Consts, Controls, Forms, Dialogs, ActnList, PlusMemo, PMSupport;
{$ENDIF}


procedure Register;
begin
  RegisterActions('Edit', [TpmEditCopy, TpmEditCut, TpmEditDelete, TpmEditPaste, TpmEditRedo,
                           TpmEditSelectAll, TpmEditUndo], nil);
  {$IFNDEF pmClx}
  {$IFDEF D6New}
  RegisterActions('Search', [TpmSearchFind, TpmSearchFindFirst, TpmSearchFindNext, TpmSearchReplace], nil)
  {$ENDIF}
  {$ENDIF}
end;
{ TpmEditCut }

constructor TpmEditCut.Create(AOwner: TComponent);
begin
  inherited;
  Caption:= 'Cu&t';
  Category:= 'Edit';
end;

procedure TpmEditCut.ExecuteTarget(Target: TObject);
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then
    begin
      sied.CutToClipboard;
      sied.ScrollInView
    end
  else inherited
end;

function TpmEditCut.HandlesTarget(Target: TObject): Boolean;
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then Result:= TWinControl(Target).Focused
                                              else Result:= inherited HandlesTarget(Target)
end;

procedure TpmEditCut.UpdateTarget(Target: TObject);
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then Enabled:= sied.CanCut
                                              else inherited
end;

{ TpmEditCopy }

constructor TpmEditCopy.Create(AOwner: TComponent);
begin
  inherited;
  Caption:= '&Copy';
  Category:= 'Edit'
end;

procedure TpmEditCopy.ExecuteTarget(Target: TObject);
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then sied.CopyToClipboard
                                              else inherited
end;

function TpmEditCopy.HandlesTarget(Target: TObject): Boolean;
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then Result:= TWinControl(Target).Focused
                                              else Result:= inherited HandlesTarget(Target)
end;

procedure TpmEditCopy.UpdateTarget(Target: TObject);
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then Enabled:= sied.CanCopy
                                              else inherited
end;

{ TpmEditPaste }

constructor TpmEditPaste.Create(AOwner: TComponent);
begin
  inherited;
  Caption:= '&Paste';
  Category:= 'Edit'
end;

procedure TpmEditPaste.ExecuteTarget(Target: TObject);
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then
    begin
      sied.PasteFromClipboard;
      sied.ScrollInView
    end
  else inherited
end;

function TpmEditPaste.HandlesTarget(Target: TObject): Boolean;
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then Result:= TWinControl(Target).Focused
                                              else Result:= inherited HandlesTarget(Target)
end;

procedure TpmEditPaste.UpdateTarget(Target: TObject);
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then Enabled:= sied.CanPaste
                                              else inherited
end;

{ TpmEditSelectAll }

constructor TpmEditSelectAll.Create(AOwner: TComponent);
begin
  inherited;
  Caption:= 'Select &all';
  Category:= 'Edit'
end;

procedure TpmEditSelectAll.ExecuteTarget(Target: TObject);
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then sied.SelectAll
                                              else inherited
end;

function TpmEditSelectAll.HandlesTarget(Target: TObject): Boolean;
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then Result:= TWinControl(Target).Focused
                                              else Result:= inherited HandlesTarget(Target)
end;

procedure TpmEditSelectAll.UpdateTarget(Target: TObject);
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then Enabled:= sied.CanSelectAll
                                              else inherited
end;

{ TpmEditDelete }

constructor TpmEditDelete.Create(AOwner: TComponent);
begin
  inherited;
  Caption:= '&Delete';
  Category:= 'Edit'
end;

procedure TpmEditDelete.ExecuteTarget(Target: TObject);
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then
    begin
      sied.ClearSelection;
      sied.ScrollInView
    end
  else inherited
end;

function TpmEditDelete.HandlesTarget(Target: TObject): Boolean;
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then Result:= TWinControl(Target).Focused
                                              else Result:= inherited HandlesTarget(Target)
end;

procedure TpmEditDelete.UpdateTarget(Target: TObject);
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then Enabled:= sied.CanDelete
                                              else inherited
end;

{ TpmEditUndo }

constructor TpmEditUndo.Create(AOwner: TComponent);
begin
  inherited;
  Caption:= '&Undo';
  Category:= 'Edit'
end;

procedure TpmEditUndo.ExecuteTarget(Target: TObject);
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then
    begin
      sied.Undo;
      sied.ScrollInView
    end
  else inherited
end;

function TpmEditUndo.HandlesTarget(Target: TObject): Boolean;
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then Result:= TWinControl(Target).Focused
                                              else Result:= inherited HandlesTarget(Target)
end;

procedure TpmEditUndo.UpdateTarget(Target: TObject);
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then Enabled:= sied.CanUndo
                                              else inherited
end;

{ TpmEditRedo }

constructor TpmEditRedo.Create(AOwner: TComponent);
begin
  inherited;
  Caption:= '&Redo';
  Category:= 'Edit'
end;

procedure TpmEditRedo.ExecuteTarget(Target: TObject);
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then
    begin
      sied.Redo;
      sied.ScrollInView
    end
  else inherited
end;

function TpmEditRedo.HandlesTarget(Target: TObject): Boolean;
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then Result:= TWinControl(Target).Focused
                                              else Result:= False //inherited HandlesTarget(Target)
end;

procedure TpmEditRedo.UpdateTarget(Target: TObject);
var sied: IpmEditAction;
begin
  if Target.GetInterface(IpmEditAction, sied) then Enabled:= sied.CanRedo
                                              else inherited
end;

{$IFNDEF pmClx}
{$IFDEF D6New}
{ TpmSearchFind }

constructor TpmSearchFind.Create(AOwner: TComponent);
begin
  inherited;
  Caption:= '&Find';
  Category:= 'Search'
end;

function TpmSearchFind.HandlesTarget(Target: TObject): Boolean;
begin
  if Screen.ActiveControl is TPlusMemo then
    begin
      Result:= True;
      //Enabled:= True
    end
  else Result:= inherited HandlesTarget(Target)
end;

procedure TpmSearchFind.Search;
var sf: TFindDialog;
begin
  if TControl(FControl) is TPlusMemo then
    begin
      sf:= TFindDialog(FDialog);
      if TPlusMemo(FControl).FindTxt(sf.FindText, frDown in sf.Options, frMatchCase in sf.Options,
                   frWholeWord in sf.Options, False) then
          TPlusMemo(FControl).ScrollInView
      else
          ShowMessage(Format(STextNotFound, [TFindDialog(FDialog).FindText]))
    end

  else
      inherited;
end;

procedure TpmSearchFind.UpdateTarget(Target: TObject);
begin
  if Target is TPlusMemo then Enabled:= TPlusMemo(Target).CharCount>0
                         else inherited;
end;

{ TpmSearchFindFirst }

constructor TpmSearchFindFirst.Create(AOwner: TComponent);
begin
  inherited;
  Caption:= 'Find f&irst';
  Category:= 'Search'
end;

function TpmSearchFindFirst.HandlesTarget(Target: TObject): Boolean;
begin
  if Screen.ActiveControl is TPlusMemo then
    begin
      Result:= True;
      Enabled:= True
    end
  else Result:= inherited HandlesTarget(Target)
end;

procedure TpmSearchFindFirst.Search(Sender: TObject);
var sf: TFindDialog;
begin
  if TControl(FControl) is TPlusMemo then
    begin
      sf:= TFindDialog(FDialog);
      if TPlusMemo(FControl).FindTxt(sf.FindText, frDown in sf.Options, frMatchCase in sf.Options,
                   frWholeWord in sf.Options, True) then
          TPlusMemo(FControl).ScrollInView
      else
          ShowMessage(Format(STextNotFound, [TFindDialog(FDialog).FindText]))
    end

  else
      inherited;
end;

procedure TpmSearchFindFirst.UpdateTarget(Target: TObject);
begin
  if Target is TPlusMemo then Enabled:= TPlusMemo(Target).CharCount>0
                         else inherited;
end;

{ TpmSearchFindNext }

constructor TpmSearchFindNext.Create(AOwner: TComponent);
begin
  inherited;
  Caption:= 'Find &Next';
  Category:= 'Search'
end;

procedure TpmSearchFindNext.ExecuteTarget(Target: TObject);
begin
  if not Assigned(SearchFind) then Exit
  else inherited;
end;


{ TpmSearchReplace }

constructor TpmSearchReplace.Create(AOwner: TComponent);
begin
  inherited;
  Caption:= '&Replace';
  Category:= 'Search'
end;

procedure TpmSearchReplace.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  if Target is TPlusMemo then
    begin
      TReplaceDialog(FDialog).OnReplace := Replace;
      TReplaceDialog(FDialog).OnFind := Search
    end;
end;

procedure TpmSearchReplace.Search;
var sf: TFindDialog;
begin
  if TControl(FControl) is TPlusMemo then
    begin
      sf:= TFindDialog(FDialog);
      if TPlusMemo(FControl).FindTxt(sf.FindText, frDown in sf.Options, frMatchCase in sf.Options,
                   frWholeWord in sf.Options, False) then
          TPlusMemo(FControl).ScrollInView
      else
          ShowMessage(Format(STextNotFound, [TFindDialog(FDialog).FindText]))
    end

  else
      inherited;
end;

function TpmSearchReplace.HandlesTarget(Target: TObject): Boolean;
begin
  if Screen.ActiveControl is TPlusMemo then
    begin
      Result:= True;
      Enabled:= True
    end
  else Result:= inherited HandlesTarget(Target)
end;

procedure TpmSearchReplace.Replace(Sender: TObject);
var
  Found: Boolean;
  FoundCount: Integer;
  srd: TReplaceDialog;
  smemo: TPlusMemo;
begin
  // FControl gets set in ExecuteTarget
  if not (TControl(FControl) is TPlusMemo) then Exit;
  smemo:= TPlusMemo(FControl);

  FoundCount := 0;
  srd:= Sender as TReplaceDialog;
  if (smemo.SelLength<>0) and
     (not (frReplaceAll in Dialog.Options) and
     (AnsiCompareText(smemo.SelText, srd.FindText) = 0) or
     (frReplaceAll in Dialog.Options) and (smemo.SelText = srd.FindText)) then
    begin
      smemo.SelText := srd.ReplaceText;
      if smemo.FindTxt(Dialog.FindText, frDown in Dialog.Options, frMatchCase in Dialog.Options,
                       frWholeWord in Dialog.Options, False) then
          smemo.ScrollInView;
      if not (frReplaceAll in Dialog.Options) then Exit;
    end;

  repeat
    Found := smemo.FindTxt(Dialog.FindText, frDown in Dialog.Options, frMatchCase in Dialog.Options,
                     frWholeWord in Dialog.Options, FFindFirst);
    if Found then
      begin
        smemo.SelText := srd.ReplaceText;
        Inc(FoundCount);
      end;
  until not Found or not (frReplaceAll in Dialog.Options);

  if not Found and (FoundCount = 0) then
    ShowMessage(Format(STextNotFound, [Dialog.FindText]));
end;

procedure TpmSearchReplace.UpdateTarget(Target: TObject);
begin
  if Target is TPlusMemo then Enabled:= TPlusMemo(Target).CharCount>0
                         else inherited;
end;
{$ENDIF}
{$ENDIF}

end.
