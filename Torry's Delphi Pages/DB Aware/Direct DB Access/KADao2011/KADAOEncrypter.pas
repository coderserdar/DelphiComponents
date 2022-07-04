unit KADAOEncrypter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TKADAOEncrypter = class(TComponent)
  private
    { Private declarations }
    F_EncodedString : String;
    F_DecodedString : String;
  protected
    { Protected declarations }
    Function  F_Get_EncodedString:String;
    Procedure F_Set_EncodedString(Value:String);
    Function  F_Get_DecodedString:String;
    Procedure F_Set_DecodedString(Value:String);
  public
    { Public declarations }
    Function EncodeString(Value:String):String;
    Function DecodeString(Value:String):String;
    Constructor  Create(AOwner: TComponent); override;
    Destructor    Destroy; override;
  published
    { Published declarations }
    Property EncodedString : String Read F_Get_EncodedString Write F_Set_EncodedString;
    Property DecodedString : String Read F_Get_DecodedString Write F_Set_DecodedString;
  end;

procedure Register;

implementation

Constructor TKADAOEncrypter.Create(AOwner: TComponent);
Begin
 Inherited Create(AOwner);
 F_EncodedString := '';
 F_DecodedString := '';
End;

Destructor TKADAOEncrypter.Destroy;
Begin

 Inherited Destroy;
End;

Function  TKADAOEncrypter.EncodeString(Value:String):String;
Begin                                                                     
 Result := Value;
End;

Function  TKADAOEncrypter.DecodeString(Value:String):String;
Begin
 Result := Value;
End;

Function  TKADAOEncrypter.F_Get_EncodedString:String;
Begin
 Result := F_EncodedString;
End;

Function  TKADAOEncrypter.F_Get_DecodedString:String;
Begin
  Result := F_DecodedString;
End;

Procedure TKADAOEncrypter.F_Set_EncodedString(Value:String);
Begin
 F_DecodedString := DecodeString(Value);
End;

Procedure TKADAOEncrypter.F_Set_DecodedString(Value:String);
Begin
 F_EncodedString := EncodeString(Value);
End;


procedure Register;
begin
  RegisterComponents('KA Dao', [TKADAOEncrypter]);
end;

end.
