unit KADaoConnectionCheck;

interface

uses
  Windows, Messages, SysUtils, Classes, DB, Extctrls, Dialogs, KDaoDatabase;


type
  TErrAction=(CloseDatabase,RaiseException);
  TErrActionSet=Set of TErrAction;
  TKADaoConnectionCheck = class(TComponent)
  private
    { Private declarations }
    Timer              : TTimer;
    InTimer            : Boolean;
    Procedure            TimerProcedure(Sender: TObject);
  protected
    { Protected declarations }
    F_Active           : Boolean;
    F_CheckInterval    : Integer;
    F_Database         : TKADaoDatabase;
    F_ErrorCode        : Integer;
    F_ExceptionText    : String;
    F_OnErrorAction    : TErrActionSet;
    F_OnNoConnection   : TNotifyEvent;
    F_OnConnectionAgain: TNotifyEvent;

    Procedure            F_Set_Active(Value:Boolean);
    Procedure            F_Set_CheckInterval(Value:Integer);
    Procedure            F_Set_Database(Value:TKADaoDatabase);
    Procedure            F_Set_ErrorCode(Value:Integer);
    Procedure            F_Set_ExceptionText(Value: String);
    Procedure            F_Set_OnErrorAction(Value:TErrActionSet);
    Procedure            Activate(Value:Boolean);
    Procedure            Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure            Loaded; override;
  public
    { Public declarations }

     Constructor         Create(AOwner : TComponent); override;
     Destructor          Destroy; override;
  published
    { Published declarations }
    Property Active                : Boolean         Read F_Active            Write F_Set_Active;
    Property Interval              : Integer         Read F_CheckInterval     Write F_Set_CheckInterval;
    Property Database              : TKADaoDatabase  Read F_Database          Write F_Set_Database;
    Property ErrorCode             : Integer         Read F_ErrorCode         Write F_Set_ErrorCode;
    Property ExceptionText         : String          Read F_ExceptionText     Write F_Set_ExceptionText;
    Property ErrorAction           : TErrActionSet   Read F_OnErrorAction     Write F_Set_OnErrorAction;
    Property OnNoConnection        : TNotifyEvent    Read F_OnNoConnection    Write F_OnNoConnection;
    Property OnConnectionRestored  : TNotifyEvent    Read F_OnConnectionAgain Write F_OnConnectionAgain;
  end;

procedure Register;

implementation


Constructor TKADaoConnectionCheck.Create(AOwner : TComponent);
Begin
 F_Active           := False;
 F_CheckInterval    := 1000;
 F_Database         := Nil;
 F_ExceptionText    := 'Connection to Database %s is broken!';
 F_OnErrorAction    := [];
 F_ErrorCode        := 0;
 InTimer            := False;
 Timer              := TTimer.Create(Self);
 Timer.Enabled      := False;
 Timer.Interval     := F_CheckInterval;
 Timer.OnTimer      := TimerProcedure;
 Inherited Create(AOwner);
End;

Destructor TKADaoConnectionCheck.Destroy;
Begin
 if F_Active Then Active := False;
 Timer.OnTimer:=Nil;
 Timer.Free;
 Inherited Destroy;
End;

Procedure TKADaoConnectionCheck.TimerProcedure(Sender: TObject);
Var
 Res : Integer;
 Dir : TSearchRec;
 S   : String;
Begin
 if InTimer Then Exit;
 if Not Assigned(F_Database) Then Exit;
 InTimer := True;
 Try
  Res:=FindFirst(F_Database.Database,faAnyFile,Dir);
  FindClose(Dir);
  if (Res <> 0) And (F_ErrorCode=0) Then
     Begin
       F_ErrorCode := Res;
       if F_Database.Connected Then
          Begin
           if Assigned(F_OnNoConnection) Then F_OnNoConnection(Self);
           if CloseDatabase  in F_OnErrorAction Then F_Database.Close;
           if RaiseException in F_OnErrorAction Then
              Begin
               if Pos('%s',AnsiLowerCase(F_ExceptionText)) <> 0  Then
                  S := Format(F_ExceptionText,[F_Database.Database])
               Else
                 S := F_ExceptionText;
              DatabaseError(S);
             End;
          End;
     End
  Else
     Begin
       F_ErrorCode:=Res;
       if (F_ErrorCode=0) And Assigned(F_OnConnectionAgain) Then F_OnConnectionAgain(Self);
     End;
 Finally
  InTimer := False;
 End;
 if Not F_Database.Connected Then Exit;
End;

Procedure TKADaoConnectionCheck.Activate(Value:Boolean);
Begin
 If Value Then
    Begin
      Timer.Enabled := True;
    End
 Else
    Begin
     Timer.Enabled := False;
    End;
End;

Procedure TKADaoConnectionCheck.Loaded;
begin
  Try
    inherited Loaded;
    if F_Active Then Activate(F_Active);
  Except
  End;
end;

Procedure TKADaoConnectionCheck.F_Set_Active(Value:Boolean);
Begin
 if F_Active=Value Then Exit;
 F_Active := Value;
 if csLoading in ComponentState Then Exit;
 if (F_Active) And (Not Assigned(F_Database)) Then
     Begin
       F_Active:=False;
       DatabaseError('Database property is not set!');
     End;
 if (F_Active) And (Not F_Database.Connected) Then
    Begin
       F_Active:=False;
       F_Active:=False;DatabaseError('Database is not connected!');
    End;
 Activate(F_Active);
End;

Procedure TKADaoConnectionCheck.F_Set_CheckInterval(Value:Integer);
Begin
 if csLoading in ComponentState Then
    Begin
      F_CheckInterval := Value;
      Exit;
    End
 Else
    Begin
     if F_Active Then DatabaseError('Cannot set CheckInterval while Active property is true!');
     F_CheckInterval := Value;
    End;
End;

Procedure TKADaoConnectionCheck.F_Set_Database(Value:TKADaoDatabase);
Begin
 if csLoading in ComponentState Then
    Begin
      F_Database := Value;
      Exit;
    End
 Else
    Begin
     if F_Active Then DatabaseError('Cannot set Database while Active property is true!');
     F_Database := Value;
    End;
End;

Procedure TKADaoConnectionCheck.F_Set_ErrorCode(Value:Integer);
Begin
 //*************************************************** ReadOnly
End;

Procedure TKADaoConnectionCheck.F_Set_ExceptionText(Value: String);
Begin
  if csLoading in ComponentState Then
    Begin
      F_ExceptionText := Value;
      Exit;
    End
 Else
    Begin
     if F_Active Then DatabaseError('Cannot set ExceptionText while Active property is true!');
     F_ExceptionText := Value;
    End;
End;

Procedure TKADaoConnectionCheck.F_Set_OnErrorAction(Value:TErrActionSet);
Begin
   if csLoading in ComponentState Then
    Begin
      F_OnErrorAction := Value;
      Exit;
    End
 Else
    Begin
     if F_Active Then DatabaseError('Cannot set OnErrorAction while Active property is true!');
     F_OnErrorAction:=Value;
    End;
End;

Procedure TKADaoConnectionCheck.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (F_Database <> nil) and (AComponent = F_Database) then F_Database := nil;
end;

procedure Register;
begin
  RegisterComponents('KA Dao', [TKADaoConnectionCheck]);
end;

end.
