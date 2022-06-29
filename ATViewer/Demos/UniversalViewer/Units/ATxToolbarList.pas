unit ATxToolbarList;

interface

uses
  Controls, Classes, StdCtrls, ComCtrls, Menus;

const
  cToolbarButtonsMax = 100;

type
  PToolbarButtonRec = ^TToolbarButtonRec;
  TToolbarButtonRec = record
    FMenuItem: TMenuItem;
    FOldShortcut: TShortCut;
    FCheck: boolean;
    FDropdownMenu: TPopupMenu;
    FToolButton: TToolButton;
  end;

type
  TToolbarList = class
  private
    FButtonsNum: integer;
    FCurrentsNum: integer;
    FButtons: array[1..cToolbarButtonsMax] of TToolbarButtonRec;
    FCurrents: array[1..cToolbarButtonsMax] of integer;
    FCurrentsDefaults: string;
    FImageList: TImageList;
    //function IsValidAvailIndex(N: integer): boolean;
    function IsValidCurrentIndex(N: integer): boolean;
    function MenuIndex(const AName: string): integer;
    procedure SetCurrentString(const AValue: string);
    function GetCurrentString: string;
  public
    constructor Create(AImageList: TImageList = nil; const ACurrentsDefault: string = '');
    destructor Destroy; override;
    procedure Clear;
    procedure ClearCurrents;
    function AddAvail(AMenuItem: TMenuItem; APopupMenu: TPopupMenu = nil): boolean;
    function AddCurrent(AMenuItem: TMenuItem): boolean; overload;
    function AddCurrent(AIndex: integer): boolean; overload;
    function AddCurrentAfter(AAvailNum, ACurrentNum: integer): boolean;
    function RemoveCurrent(ACurrentNum: integer): boolean;
    function IsAvailCurrent(AAvailNum: integer): boolean;
    function IndexOf(AMenuItem: TMenuItem): integer;
    function GetAvail(N: integer; var ARec: PToolbarButtonRec): boolean;
    function GetCurrent(N: integer; var AAvailNum: integer): boolean;
    function GetToolButton(AMenuItem: TMenuItem): TToolButton;
    procedure CopyTo(ADest: TToolbarList);
    procedure ApplyTo(AToolbar: TToolbar);
    procedure Update(AMenuItem: TMenuItem; AEnabled: integer; AChecked: integer = -1; AVisible: integer = -1);
    property ImageList: TImageList read FImageList write FImageList;
    function MoveCurrentUp(N: integer): boolean;
    function MoveCurrentDown(N: integer): boolean;
    procedure ResetCurrents;
    function UpdateCaptions: string;
    property CurrentString: string read GetCurrentString write SetCurrentString;
    procedure PrepareShortcuts;
    procedure RestoreShortcuts;
    procedure UpdateImageIndex(AMenuItem: TMenuItem; AIndex: Integer);
  end;

var
  sMsgButtonSeparator: string = '(Separator)';

function GetToolbarButtonId(const Rec: TToolbarButtonRec): string;


implementation

uses
  Windows, SysUtils, ATxMsgProc, Dialogs, Consts;

{ Helper functions }

function GetToolbarButtonId(const Rec: TToolbarButtonRec): string;
begin
  with Rec do
    Result:= Copy(FMenuItem.Name, 4, MaxInt);
end;

{ TToolbarList }

{
function TToolbarList.IsValidAvailIndex(N: integer): boolean;
begin
  Result:= (N>=1) and (N<=FButtonsNum);
end;
}

function TToolbarList.IsValidCurrentIndex(N: integer): boolean;
begin
  Result:= (N>=1) and (N<=FCurrentsNum);
end;

constructor TToolbarList.Create(AImageList: TImageList = nil; const ACurrentsDefault: string = '');
begin
  Clear;
  FImageList:= AImageList;
  FCurrentsDefaults:= ACurrentsDefault;
end;

destructor TToolbarList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TToolbarList.Clear;
begin
  FButtonsNum:= 0;
  ClearCurrents;
end;

procedure TToolbarList.ClearCurrents;
begin
  FCurrentsNum:= 0;
end;

function TToolbarList.AddAvail(AMenuItem: TMenuItem; APopupMenu: TPopupMenu = nil): boolean;
begin
  ClearCurrents;
  Result:= FButtonsNum<cToolbarButtonsMax;
  if Result then
    begin
    Inc(FButtonsNum);
    with FButtons[FButtonsNum] do
      begin
      FMenuItem:= AMenuItem;
      FOldShortcut:= 0;
      FCheck:= (AMenuItem.Checked) or (AMenuItem.RadioItem);
      FDropdownMenu:= APopupMenu;
      FToolButton:= nil;
      end;
    AMenuItem.Tag:= FButtonsNum;
    end;
end;

function TToolbarList.AddCurrent(AMenuItem: TMenuItem): boolean;
begin
  Result:= AddCurrent(IndexOf(AMenuItem));
end;

function TToolbarList.AddCurrent(AIndex: integer): boolean;
begin
  Result:= FCurrentsNum<cToolbarButtonsMax;
  if Result then
    begin
    Inc(FCurrentsNum);
    FCurrents[FCurrentsNum]:= AIndex;
    end;
end;

procedure TToolbarList.CopyTo(ADest: TToolbarList);
var
  i: integer;
begin
  ADest.FImageList:= FImageList;
  ADest.FCurrentsDefaults:= FCurrentsDefaults;
  ADest.Clear;
  ADest.FButtonsNum:= FButtonsNum;
  ADest.FCurrentsNum:= FCurrentsNum;

  for i:= 1 to FButtonsNum do
    begin
    ADest.FButtons[i].FMenuItem:= FButtons[i].FMenuItem;
    ADest.FButtons[i].FOldShortcut:= FButtons[i].FOldShortcut;
    ADest.FButtons[i].FCheck:= FButtons[i].FCheck;
    ADest.FButtons[i].FDropdownMenu:= FButtons[i].FDropdownMenu;
    ADest.FButtons[i].FToolButton:= FButtons[i].FToolButton;
    end;

  for i:= 1 to FCurrentsNum do
    ADest.FCurrents[i]:= FCurrents[i];
end;

function TToolbarList.GetAvail(N: integer; var ARec: PToolbarButtonRec): boolean;
begin
  Result:= (N>0) and (N<=FButtonsNum);
  if Result then
    ARec:= @FButtons[N];
end;

function TToolbarList.GetCurrent(N: integer; var AAvailNum: integer): boolean;
begin
  Result:= IsValidCurrentIndex(N);
  if Result then
    AAvailNum:= FCurrents[N];
end;

function TToolbarList.IsAvailCurrent(AAvailNum: integer): boolean;
var
  i: integer;
begin
  Result:= false;
  for i:= 1 to FCurrentsNum do
    if FCurrents[i]=AAvailNum then
      begin Result:= true; Break end;
end;

procedure TToolbarList.ApplyTo(AToolbar: TToolbar);
var
  i: integer;
begin
  for i:= Low(FButtons) to High(FButtons) do
    with FButtons[i] do
      FToolButton:= nil;

  with AToolbar do
    for i:= ButtonCount-1 downto 0 do
      Buttons[i].Free;

  AToolbar.Images:= FImageList;
  AToolbar.ButtonWidth:= FImageList.Width;
  AToolbar.ButtonHeight:= FImageList.Height;

  for i:= FCurrentsNum downto 1 do
    with FButtons[FCurrents[i]] do
      begin
      FToolButton:= TToolButton.Create(AToolbar);
      with FToolButton do
        begin
        Parent:= AToolbar;
        if FMenuItem.Caption = '-' then
          begin
          Style:= tbsSeparator;
          Hint:= '';
          Width:= 6;
          end
        else
          begin
          Style:= tbsButton;
          Hint:= MsgStrip(FMenuItem.Caption);
          ImageIndex:= FMenuItem.ImageIndex;

          if FCheck then
            Style:= tbsCheck;

          if Assigned(FDropdownMenu) then
            begin
            Style:= tbsDropDown;
            DropdownMenu:= FDropdownMenu;
            end;

          OnClick:= FMenuItem.OnClick;
          end;
        end;
      end;
end;


procedure TToolbarList.Update(AMenuItem: TMenuItem; AEnabled: integer; AChecked: integer = -1; AVisible: integer = -1);
var
  Btn: TToolButton;  
begin
  with AMenuItem do
    begin
    if Tag>0 then
      Btn:= FButtons[Tag].FToolButton
    else
      Btn:= nil;
    if AEnabled>=0 then
      begin
      Enabled:= (AEnabled<>0);
      if Assigned(Btn) then Btn.Enabled:= Enabled;
      end;
    if AChecked>=0 then
      begin
      Checked:= (AChecked<>0);
      if Assigned(Btn) then Btn.Down:= Checked;
      end;
    if AVisible>=0 then
      begin
      //Visible:= (AVisible<>0);
      if Assigned(Btn) then Btn.Visible:= (AVisible<>0);
      end;
    end;
end;


function TToolbarList.IndexOf(AMenuItem: TMenuItem): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 1 to FButtonsNum do
    if FButtons[i].FMenuItem=AMenuItem then
      begin Result:= i; Break end;
end;  

function TToolbarList.AddCurrentAfter(AAvailNum, ACurrentNum: integer): boolean;
var
  i: integer;
begin
  if not IsValidCurrentIndex(ACurrentNum) then
    Result:= AddCurrent(AAvailNum)
  else
    begin
    Result:= FCurrentsNum<cToolbarButtonsMax;
    if Result then
      begin
      Inc(FCurrentsNum);
      for i:= FCurrentsNum downto ACurrentNum+2 do
        FCurrents[i]:= FCurrents[i-1];
      FCurrents[ACurrentNum+1]:= AAvailNum;
      end;
    end;  
end;

function TToolbarList.RemoveCurrent(ACurrentNum: integer): boolean;
var
  i: integer;
begin
  Result:= IsValidCurrentIndex(ACurrentNum);
  if Result then
    begin
    Dec(FCurrentsNum);
    for i:= ACurrentNum to FCurrentsNum do
      FCurrents[i]:= FCurrents[i+1];
    end;
end;

{
procedure TToolbarList.ShowCurrents;
var
  i: integer;
  s: string;
begin
  s:= '';
  for i:= 1 to FCurrentsNum do
    s:=
      s +
      Format('%d: #%d: %s',
        [i, FCurrents[i],
         FButtons[FCurrents[i]].FMenuItem.Name]) +
      #13;
  ShowMessage(s);
end;
}


function TToolbarList.MoveCurrentUp(N: integer): boolean;
var
  i: integer;
begin
  Result:= IsValidCurrentIndex(N) and (N>1);
  if Result then
    begin
    i:= FCurrents[N-1];
    FCurrents[N-1]:= FCurrents[N];
    FCurrents[N]:= i;
    end;
end;

function TToolbarList.MoveCurrentDown(N: integer): boolean;
var
  i: integer;
begin
  Result:= IsValidCurrentIndex(N) and (N<FCurrentsNum);
  if Result then
    begin
    i:= FCurrents[N+1];
    FCurrents[N+1]:= FCurrents[N];
    FCurrents[N]:= i;
    end;
end;


function SGetItem(var SList: string): string;
var
  k: integer;
begin
  k:= Pos(' ', SList);
  if k=0 then k:= MaxInt;
  Result:= Copy(SList, 1, k-1);
  Delete(SList, 1, k);
end;

procedure TToolbarList.SetCurrentString(const AValue: string);
var
  Items, Item: string;
  N: integer;
begin
  ClearCurrents;

  Items:= AValue;
  repeat
    Item:= SGetItem(Items);
    if Item='' then Break;
    Insert('mnu', Item, 1);

    N:= MenuIndex(Item);
    if N=0 then Continue;

    AddCurrent(N);
  until false;
end;

function TToolbarList.MenuIndex(const AName: string): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 1 to FButtonsNum do
    if FButtons[i].FMenuItem.Name=AName then
      begin Result:= i; Break end;
end;

function TToolbarList.GetCurrentString: string;
var
  i: integer;
begin
  Result:= '';
  for i:= 1 to FCurrentsNum do
    Result:= Result+GetToolbarButtonId(FButtons[FCurrents[i]])+' ';
end;

procedure TToolbarList.ResetCurrents;
begin
  SetCurrentString(FCurrentsDefaults);
end;

function TToolbarList.UpdateCaptions: string;
var
  i: integer;
begin
  for i:= 1 to FButtonsNum do
    with FButtons[i] do
      begin
      if Assigned(FToolButton) then
        FToolButton.Hint:= MsgStrip(FMenuItem.Caption);
      end;
end;

//Note: PrepareShortcuts double call shoud not break already saved shortcuts!
procedure TToolbarList.PrepareShortcuts;
var
  i: integer;
  S: string;
  Key: Word;
  Shift: TShiftState;
begin
  for i:= 1 to FButtonsNum do
    with FButtons[i] do
      if Assigned(FMenuItem) and (FOldShortcut = 0) then
        begin
        S:= ShortcutToText(FMenuItem.ShortCut);
        ShortcutToKey(FMenuItem.ShortCut, Key, Shift);
        if (Length(S) = 1) or ((Key = VK_DELETE) and (Shift = [])) then
          begin
          FOldShortcut:= FMenuItem.ShortCut;
          FMenuItem.ShortCut:= TextToShortCut(
            {$ifdef MENX} MenuKeyCaps[mkcAlt] {$else} smkcAlt {$endif} {'Alt+'}
            + S);
          end;
        end;
end;

procedure TToolbarList.RestoreShortcuts;
var
  i: integer;
begin
  for i:= 1 to FButtonsNum do
    with FButtons[i] do
      if Assigned(FMenuItem) and (FOldShortcut <> 0) then
        begin
        FMenuItem.ShortCut:= FOldShortcut;
        FOldShortcut:= 0;
        end;
end;

function TToolbarList.GetToolButton(AMenuItem: TMenuItem): TToolButton;
var
  N: Integer;
begin
  N:= IndexOf(AMenuItem);
  if N > 0 then
    Result:= FButtons[N].FToolButton
  else
    Result:= nil;
end;

procedure TToolbarList.UpdateImageIndex(AMenuItem: TMenuItem; AIndex: Integer);
var
  B: TToolButton;
begin
  B:= GetToolButton(AMenuItem);
  if Assigned(B) then
    B.ImageIndex:= AIndex;
end;


end.
