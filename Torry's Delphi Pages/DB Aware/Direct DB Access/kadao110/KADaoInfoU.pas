(*****************************************************************************)
(* MODULE: ACCINFOU                                                          *)
(* AUTHOR: Peter Blair                                                       *)
(* RIGHTS: Reggatta Systems, Inc. - COPYRIGHT 1995 - 1998                    *)
(* DESCR.: This module contains classes for InfoPower compatibility          *)
(*****************************************************************************)
unit KADaoInfoU;
//$I BSE.INC}
interface
uses
    DAOApi, 
    {$IFDEF DAO35}
    DAO35Api,
    {$ENDIF}
    {$IFDEF DAO36}
    DAO36Api,
    {$ENDIF}
    {$IFDEF DAO120}
    DAO120Api,
    {$ENDIF}
    DBTables, Windows, SysUtils, Classes, Db, DBCommon, KDaoDataBase, ActiveX, Forms, KDaoTable, wwTable, wwTypes;

type
    TwwKADaoTable =
    class( TKADaoTable )
    private
        FControlType    : TwwTableDisplayType;
        FPictureMasks   : TStrings;
        FUsePictureMask : boolean;
        FOnInvalidValue : TwwInvalidValueEvent;

        function GetControlType : TStrings;
        procedure SetControlType( sel : TStrings );
        function GetPictureMasks : TStrings;
        procedure SetPictureMasks( sel : TStrings );

    protected
        procedure DoBeforePost; override; { For picture support }

    public
        constructor Create( AOwner : TComponent ); override;
        destructor Destroy; override;

    published
        property ControlType : TStrings
        read  GetControlType
        write setControltype stored TRUE;
        property PictureMasks: TStrings
        read GetPictureMasks
        write SetPictureMasks;
        property ValidateWithMask : boolean
        read FUsePictureMask
        write FUsePictureMask;
        property OnInvalidValue: TwwInvalidValueEvent
        read FOnInvalidValue
        write FOnInvalidValue;
    end;

procedure register;

implementation

uses DaoUtils, Dialogs, SortByDialog, QueryDefDialogUnit, MasterDetailFormUnit,  wwCommon;


constructor TwwKADaoTable.create( AOwner : TComponent );
begin
    inherited Create( AOwner );
    FControlType    := TStringList.create;
    FPictureMasks   := TStringList.create;
    FUsePictureMask := True;
end;


destructor TwwKADaoTable.Destroy;
begin
    FControlType.Free;
    FPictureMasks.Free;
    FPictureMasks:= nil;
    inherited Destroy;
end;


function TwwKADaoTable.GetControltype : TStrings;
begin
    Result := FControlType;
end;


procedure TwwKADaoTable.SetControlType( sel : TStrings );
begin
    FControlType.Assign( sel );
end;


function TwwKADaoTable.GetPictureMasks : TStrings;
begin
    Result:= FPictureMasks
end;


procedure TwwKADaoTable.SetPictureMasks( sel : TStrings );
begin
    FPictureMasks.Assign( sel );
end;


procedure TwwKADaoTable.DoBeforePost;
begin
    inherited DoBeforePost;
    if FUsePictureMask then
        wwValidatePictureFields( self, FOnInvalidValue );
end;


procedure register;
begin
    RegisterComponents('KA Dao', [ TwwKADaoTable] );
end;


end.

