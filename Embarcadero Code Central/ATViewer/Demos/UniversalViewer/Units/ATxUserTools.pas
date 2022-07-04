unit ATxUserTools;

interface

uses
  Classes, Controls;

type
  TATUserTool = record
    FCaption: string;
    FCommand: WideString;
    FParams: WideString;
    FActions: string;
  end;

  TATUserTools = array[1..8] of TATUserTool;

procedure ClearUserTool(var Tool: TATUserTool);
procedure ClearUserTools(var Tools: TATUserTools);
procedure CopyUserTools(const Src: TATUserTools; var Dest: TATUserTools);
procedure DeleteUserTool(var Tools: TATUserTools; N: integer);
procedure SwapUserTools(var Src, Dest: TATUserTool);
function NumOfUserTools(const Tools: TATUserTools): integer;
procedure AddUserTool(var Tools: TATUserTools; const Tool: TATUserTool);
function ConfigureUserTool(var Tool: TATUserTool; Owner: TComponent): boolean;
function UserToolHasAction(const Tool: TATUserTool; const Action: string): boolean;
procedure UserToolAddAction(var Tool: TATUserTool; const Action: string);

function AddCommandIcon(const Command: WideString; ImageList: TImageList): integer;


implementation

uses
  Windows, ShellAPI, SysUtils, Graphics,
  Consts, CommCtrl,
  ATxFProc, UFormViewToolParams;

const
  cActionSep = ',';


procedure ClearUserTool(var Tool: TATUserTool);
begin
  Tool.FCaption:= '';
  Tool.FCommand:= '';
  Tool.FParams:= '';
  Tool.FActions:= '';
end;

procedure ClearUserTools(var Tools: TATUserTools);
var
  i: integer;
begin
  for i:= Low(TATUserTools) to High(TATUserTools) do
    ClearUserTool(Tools[i]);
end;

procedure CopyUserTool(const Src: TATUserTool; var Dest: TATUserTool);
begin
  Dest.FCaption:= Src.FCaption;
  Dest.FCommand:= Src.FCommand;
  Dest.FParams:= Src.FParams;
  Dest.FActions:= Src.FActions;
end;

procedure CopyUserTools(const Src: TATUserTools; var Dest: TATUserTools);
var
  i: integer;
begin
  for i:= Low(TATUserTools) to High(TATUserTools) do
    CopyUserTool(Src[i], Dest[i]);
end;

procedure SwapUserTools(var Src, Dest: TATUserTool);
var
  Tool: TATUserTool;
begin
  CopyUserTool(Src, Tool);
  CopyUserTool(Dest, Src);
  CopyUserTool(Tool, Dest);
end;


procedure DeleteUserTool(var Tools: TATUserTools; N: integer);
var
  i: integer;
begin
  if (N>=Low(TATUserTools)) and (N<=High(TATUserTools)) then
    begin
    for i:= N to High(TATUserTools)-1 do
      CopyUserTool(Tools[i+1], Tools[i]);
    ClearUserTool(Tools[High(TATUserTools)]);
    end;
end;

function NumOfUserTools(const Tools: TATUserTools): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= High(TATUserTools) downto Low(TATUserTools) do
    if Tools[i].FCaption<>'' then
      begin Result:= i; Break end;
end;

procedure AddUserTool(var Tools: TATUserTools; const Tool: TATUserTool);
var
  N: integer;
begin
  N:= NumOfUserTools(Tools);
  if N<High(TATUserTools) then
    CopyUserTool(Tool, Tools[N+1]);
end;

function ConfigureUserTool(var Tool: TATUserTool; Owner: TComponent): boolean;
const
  cSelectAll = 'SelectAll';
  cCopy      = 'Copy';
  cExit      = 'Exit';
begin
  with TFormViewToolParams.Create(Owner) do
    try
      edCaption.Text:= Tool.FCaption;
      edCommand.Text:= Tool.FCommand;
      edParams.Text:= Tool.FParams;

      chkSelectAll.Checked:= UserToolHasAction(Tool, cSelectAll);
      chkCopy.Checked:= UserToolHasAction(Tool, cCopy);
      chkExit.Checked:= UserToolHasAction(Tool, cExit);

      Result:= (ShowModal=mrOk) and (edCaption.Text<>'');
      if Result then
        begin
        Tool.FCaption:= edCaption.Text;
        Tool.FCommand:= edCommand.Text;
        Tool.FParams:= edParams.Text;

        Tool.FActions:= '';
        if chkSelectAll.Checked then
          UserToolAddAction(Tool, cSelectAll);
        if chkCopy.Checked then
          UserToolAddAction(Tool, cCopy);
        if chkExit.Checked then
          UserToolAddAction(Tool, cExit);
        end;
    finally
      Release;
    end;
end;

function UserToolHasAction(const Tool: TATUserTool; const Action: string): boolean;
begin
  Result:= Pos(
    cActionSep + Action + cActionSep,
    cActionSep + Tool.FActions + cActionSep) > 0;
end;

procedure UserToolAddAction(var Tool: TATUserTool; const Action: string);
begin
  with Tool do
    FActions:= FActions + Action + cActionSep;
end;


var
  _TempIcon: TIcon = nil;

function AddCommandIcon(const Command: WideString; ImageList: TImageList): integer;
var
  hdIcon: HICON;
  Icons1, Icons2: HICON;
begin
  Result:= -1;
  if not IsFileExist(Command) then Exit;
  //AddIcon exception on 29x35:
  if (ImageList.Height > 32) or (ImageList.Width > 32) then Exit;

  if Win32Platform = VER_PLATFORM_WIN32_NT
    then hdIcon:= ExtractIconExW(PWChar(Command), 0, Icons1, Icons2, 1)
    else hdIcon:= ExtractIconExA(PAnsiChar(AnsiString(Command)), 0, Icons1, Icons2, 1);

  if (hdIcon <> 0) and (hdIcon <> 1) then
    try
      //if ImageList has size>=32, get large icon, otherwise get small:
      if (ImageList.Height >= 32) or (ImageList.Width >= 32)
        then _TempIcon.Handle:= Icons1
        else _TempIcon.Handle:= Icons2;
      if _TempIcon.Handle > 0 then
        Result:= ImageList.AddIcon(_TempIcon);
    except    
    end;
end;


initialization
  _TempIcon:= TIcon.Create;

finalization
  FreeAndNil(_TempIcon);

end.
