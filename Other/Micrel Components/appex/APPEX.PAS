{
 Designer: Craig Ward, 100554.2072@compuserve.com
 Date:     3/1/96
 Version:  1.2

 Function: Writes application error messages to text-file.

 Fields:   There are several properties that the developer can set:

            [1] ErrorLog - a string that stores the full path and filename of
                           the text-file (this must be set)
            [2] UserName - a string that stores the application's current
                           user name (this does not need to be set)
            [3] ShowExcep- this bool determines whether to show application exceptions,
                           or not (default is true).

 Calling:   There is only one method to call, and this must be passed the
            information that an exception will generate.

            For instance, in your application you will need to add the following
            line (it is probably best to add this line to the main form's unit,
            such as tied to the OnCreate event):

              Application.OnException := AppException1.WriteException;

            This will write the error message to the text file specified in the
            TextFile field, adding user and dateTime details.

 Try... :   It is likely that there is try..except blocks within your application.
            If these blocks contain default handling of exceptions (for instance,
            you may abort or exit from a routine that attemtps to wrongly divide
            by zero) then your handler will execute, and the application's
            AppException component will be bypassed.

 Thanks:    Special thanks to Chris Newham (100027.16@compuserve.com) for his
            improvements to the component.
*******************************************************************************}
unit Appex;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs;


type
  TAppException = class(TComponent)
  private
    { Private declarations }
    FTextFile: string;
    FUserName: string;
    FShowExcep: Boolean;
    procedure SetTextFile(value: string);
    procedure SetUserName(value: string);
    procedure SetShowExcep(value: Boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure WriteException(Sender: TObject; E: Exception);
    procedure ErrorException(Typ:Char; Const Msg: String);
    constructor Create(aOwner: TComponent); override;
  published
    { Published declarations }
    property ErrorLog: string read FTextFile write SetTextFile;
    property UserName: string read FUserName write SetUserName;
    property ShowExcep: Boolean read FShowExcep write SetShowExcep default True;
  end;

procedure Register;

{*******************************************************************************}
implementation

{register component}
procedure Register;
begin
  RegisterComponents('System', [TAppException]);
end;

{create}
constructor TAppException.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
  ShowExcep := true;
end;

{set text file}
procedure TAppException.SetTextFile(value: string);
begin
 if FTextFile <> value then
  FTextFile := value;
end;

{set user name}
procedure TAppException.SetUserName(value: string);
begin
 if FUserName <> value then
  FUserName := value;
end;

procedure TAppException.SetShowExcep(value: boolean);
begin
 if FShowExcep <> value then
  FShowExcep := value;
end;

{write exception message to text file}
procedure TAppException.WriteException(Sender: TObject; E: Exception);
var
 tF: TextFile;
begin
 If FTextFile = '' Then Exit;
 AssignFile(tF,FTextFile);                        {assign field to Text-File}
 if FileExists( FTextFile ) then Append(tF)       {prepare to append to file}
 else Rewrite( tF );


 WriteLn(tF,'** Výjimka');
 Writeln(tF);
 if FUserName <> '' then
  WriteLn(tF,'Uživatel: '+FUserName);                 {write user name - optional}

 WriteLn(tF,DateTimeToStr(Now));                  {write time}

 WriteLn(tF,E.Message);                           {write error message}
 Writeln(tF);                                  {add blank line}
 Flush(tF);
 CloseFile(tF);                                   {close file}

 if FShowExcep then application.ShowException(E); {show exception if reqd}

end;

procedure TAppException.ErrorException(Typ:Char; Const Msg: String);
var
   tF: Text;
begin
   if FTextFile= '' then Exit;
   AssignFile(tF, FTextFile);
   if FileExists(FTextFile) then Append(tF) else Rewrite(tF);

  Case UpCase(Typ) Of
    'W':WriteLn(tF,'** Varování');
    'E':WriteLn(tF,'** Chyba');
  Else
    WriteLn(tF,'** Neznámá chyba');
  End;
  Writeln(tF);
  if FUserName <> '' then
   WriteLn(tF,'Uživatel: '+FUserName); {write user name - optional}

  WriteLn(tF,DateTimeToStr(Now));      {write time}

  WriteLn(tF,Msg);                     {write error message}
  Writeln(tF);                         {add blank line}
  Flush(tF);
  CloseFile(tF);                       {close file}
end;

end.
