{$INCLUDE ..\cDefines.inc}
unit cThreads;

interface

uses
  { Delphi }
  Classes;



{                                                                              }
{ TThreadEx                                                                    }
{   Extended base class for thread implementations.                            }
{                                                                              }
type
  TThreadEx = class(TThread)
  public
    // Make TThread's Synchronize method public
    procedure Synchronize(Method: TThreadMethod);

    // Make TThread's Terminate method virtual
    procedure Terminate; virtual;

    // Make Terminated property public
    property  Terminated;
  end;



implementation



{                                                                              }
{ TThreadEx                                                                    }
{                                                                              }
procedure TThreadEx.Synchronize(Method: TThreadMethod);
begin
  inherited Synchronize(Method);
end;

procedure TThreadEx.Terminate;
begin
  inherited Terminate;
end;



end.

