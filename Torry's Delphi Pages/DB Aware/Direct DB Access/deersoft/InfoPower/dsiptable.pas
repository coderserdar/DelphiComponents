(*****************************************************************************)
(* MODULE: IPTemplate                                                        *)
(* AUTHOR:                                                                   *)
(* RIGHTS:                                                                   *)
(* DESCR.: This module contains classes for InfoPower compatibility          *)
(*****************************************************************************)

Unit DSIPTable;

interface

uses
  SysUtils,
  WinTypes,
  WinProcs,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  DB,        {You may need to replace these units with your engines versions}
  DBTables,  {You may need to replace these units with your engines versions}
  DTables,
  dialogs,
  wwfilter,
  wwStr,
  wwSystem,
  wwTable,
  wwtypes;

Type
  TwwDTable =
  Class( TDTAble )    {Change to derive from your dataset type}
    Private
      FControlType    : TStrings;
      FPictureMasks   : TStrings;
      FUsePictureMask : boolean;
      FOnInvalidValue : TwwInvalidValueEvent;

      Function GetControlType : TStrings;
      Procedure SetControlType( sel : TStrings );
      Function GetPictureMasks : TStrings;
      Procedure SetPictureMasks( sel : TStrings );

    Protected
      Procedure DoBeforePost; Override; { For picture support }

    Public
      Constructor Create( AOwner : TComponent ); Override;
      Destructor Destroy; Override;

    Published
				  {$ifdef ver100}
      //Property IndexDefs;  // Only Publish this if your component defines IndexDefs
						{$endif}
      Property ControlType : TStrings
        Read  GetControlType
        Write setControltype;
      Property PictureMasks: TStrings
        Read GetPictureMasks
        Write SetPictureMasks;
      Property ValidateWithMask : boolean
        Read FUsePictureMask
        Write FUsePictureMask;
      Property OnInvalidValue: TwwInvalidValueEvent
        Read FOnInvalidValue
        Write FOnInvalidValue;
  end;

Procedure Register;

implementation
uses
  wwcommon,
  dbconsts;


Constructor TwwDTable.create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  FControlType    := TStringList.create;
  FPictureMasks   := TStringList.create;
  FUsePictureMask := True;
end;


Destructor TwwDTable.Destroy;
begin
  FControlType.Free;
  FPictureMasks.Free;
  FPictureMasks:= NIL;
  Inherited Destroy;
end;


Function TwwDTable.GetControltype : TStrings;
begin
  Result := FControlType;
end;


Procedure TwwDTable.SetControlType( sel : TStrings );
begin
  FControlType.Assign( sel );
end;


Function TwwDTable.GetPictureMasks : TStrings;
begin
  Result:= FPictureMasks
end;


Procedure TwwDTable.SetPictureMasks( sel : TStrings );
begin
  FPictureMasks.Assign( sel );
end;

Procedure TwwDTable.DoBeforePost;
begin
  Inherited DoBeforePost;
  if FUsePictureMask then
    wwValidatePictureFields( self, FOnInvalidValue );
end;

Procedure Register;
begin
  RegisterComponents('IP Access', [TwwDTable] );
end;

end.
