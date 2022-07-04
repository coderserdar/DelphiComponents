///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit DMMemo;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Messages, Windows;

{This component is just a workaround. It was established that when some form with various
controls on it is docked to the MDI form, all components receive parasitic WM_SETTEXT
message with nil text. Notice that this wrong behavior may take place only if no MDI children
are opened - otherwise all works fine.}

type
  TDMMemo = class(TMemo)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure Hook(var Msg: TWMSETTEXT); message WM_SETTEXT;
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('DM2003', [TDMMemo]);
end;

{ TDMMemo }

procedure TDMMemo.Hook(var Msg: TWMSETTEXT);
const 
  S: string='';  // bufferize text and repair it when cleared
begin           
  if Msg.Text<>nil then 
  begin 
    inherited; 
    S:=Msg.Text; 
  end
  else 
    SetWindowText(Handle, PChar(S));
end;

end.
