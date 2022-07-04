unit OraError;

interface

uses Classes, SysUtils, DB, OraDefines;

type
  EOraError = class(EDatabaseError)
  private
   FErrorType:sword;
   FErrorCode:sb4;
   FErrorText:string;
  public
   constructor Create(EType:sword;ECode:sb4;EText:string);
   destructor Destroy;override;
   property ErrorType:sword read FErrorType;
   property ErrorCode:sb4 read FErrorCode;
   property Text:string read FErrorText;
  end;

procedure ADatabaseError(const Message: string; Component: TComponent = nil);
procedure ADatabaseErrorFmt(const Message: string; const Args: array of const;Component: TComponent = nil);

implementation

procedure ADatabaseError(const Message: string; Component: TComponent = nil);
begin
  if Assigned(Component) and (Component.Name <> '')
   then raise EDatabaseError.Create(Format('%s: %s', [Component.Name, Message]))
   else raise EDatabaseError.Create(Message);
end;

procedure ADatabaseErrorFmt(const Message: string; const Args: array of const;
  Component: TComponent = nil);
begin
  ADatabaseError(Format(Message, Args), Component);
end;


constructor EOraError.Create(EType:sword;ECode:sb4;EText:string);
begin
// Errors with numbers 20010-20050 are reserved for user information messages (not errors) from database.
//  Cut all debug info (such as error line,name of procedure) from error message text.
//  Leaves only user information message.
//  User information message should have the following format for example
//  Example PL/SQL code how to raise user information message:
//    raise_application_error(-20010,'{You cannot do this operation because ....}');

 FErrorText:=EText; // full error text
 FErrorType:=EType;
 FErrorCode:=ECode;
 if (ECode>=20010) and (ECode<=20050) then
  if Pos('{', EText) <> 0 then EText := Copy(EText, Pos('{', EText)+1, Pos('}', EText)-Pos('{', EText)-1);
 inherited Create(EText);
end;

destructor EOraError.Destroy;
begin
 inherited Destroy;
end;

end.
