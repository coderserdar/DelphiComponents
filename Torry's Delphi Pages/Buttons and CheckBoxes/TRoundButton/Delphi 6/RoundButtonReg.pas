{$J+}
{$R-}
{$B-}

{$IFDEF VER90}      { Borland Delphi 2.0 }
{$DEFINE DELPHI2_UP}
{$DEFINE ISDELPHI}
{$ENDIF}

{$IFDEF VER93}      { Borland Delphi 2.0 }
{$DEFINE DELPHI2_UP}
{$ENDIF}

{$IFDEF VER100}     { Borland Delphi 3.0 }
{$DEFINE DELPHI2_UP}
{$DEFINE DELPHI3_UP}
{$DEFINE ISDELPHI}
{$ENDIF}

{$IFDEF VER110}     { Borland C++Builder 3.0 }
{$DEFINE DELPHI2_UP}
{$DEFINE DELPHI3_UP}
{$ENDIF}

{$IFDEF VER120}     { Borland Delphi 4.0 }
{$DEFINE DELPHI2_UP}
{$DEFINE DELPHI3_UP}
{$DEFINE DELPHI4_UP}
{$DEFINE ISDELPHI}
{$ENDIF}

{$IFDEF VER125}     { Borland C++Builder 4.0 }
{$DEFINE DELPHI2_UP}
{$DEFINE DELPHI3_UP}
{$DEFINE DELPHI4_UP}
{$ENDIF}

{$IFDEF VER130}     { Borland Delphi 5.0 } { Borland C++Builder 5.0 }
{$DEFINE DELPHI2_UP}
{$DEFINE DELPHI3_UP}
{$DEFINE DELPHI4_UP}
{$DEFINE DELPHI5_UP}
{$ENDIF}

{$IFDEF VER140}     { Borland Delphi 6.0 } { Borland C++Builder 6.0 }
{$DEFINE DELPHI2_UP}
{$DEFINE DELPHI3_UP}
{$DEFINE DELPHI4_UP}
{$DEFINE DELPHI5_UP}
{$DEFINE DELPHI6_UP}
 {$IFNDEF BCB}
  {$DEFINE ISDELPHI}
 {$ENDIF}
{$ENDIF}

{$IFDEF VER150}     { Borland Delphi 7.0 } { Borland C++Builder 7.0 }
{$DEFINE DELPHI2_UP}
{$DEFINE DELPHI3_UP}
{$DEFINE DELPHI4_UP}
{$DEFINE DELPHI5_UP}
{$DEFINE DELPHI6_UP}
{$DEFINE DELPHI7_UP}
 {$IFNDEF BCB}
  {$DEFINE ISDELPHI}
 {$ENDIF}
{$ENDIF}

{$IFDEF VER160}     { Borland Delphi 8.0 }
{$DEFINE DELPHI2_UP}
{$DEFINE DELPHI3_UP}
{$DEFINE DELPHI4_UP}
{$DEFINE DELPHI5_UP}
{$DEFINE DELPHI6_UP}
{$DEFINE DELPHI7_UP}
{$DEFINE DELPHI8_UP}
 {$IFNDEF BCB}
  {$DEFINE ISDELPHI}
 {$ENDIF}
{$ENDIF}

{$IFDEF VER170}     { Borland Delphi 9.0, Borland Delphi 2005 }
{$DEFINE DELPHI2_UP}
{$DEFINE DELPHI3_UP}
{$DEFINE DELPHI4_UP}
{$DEFINE DELPHI5_UP}
{$DEFINE DELPHI6_UP}
{$DEFINE DELPHI7_UP}
{$DEFINE DELPHI8_UP}
{$DEFINE DELPHI9_UP}
 {$IFNDEF BCB}
  {$DEFINE ISDELPHI}
 {$ENDIF}
{$ENDIF}

{$IFDEF VER180}     { Borland Delphi 10.0, Borland Developer Studio 2006 }
{$DEFINE DELPHI2_UP}
{$DEFINE DELPHI3_UP}
{$DEFINE DELPHI4_UP}
{$DEFINE DELPHI5_UP}
{$DEFINE DELPHI6_UP}
{$DEFINE DELPHI7_UP}
{$DEFINE DELPHI8_UP}
{$DEFINE DELPHI9_UP}
{$DEFINE DELPHI10_UP}
 {$IFNDEF BCB}
  {$DEFINE ISDELPHI}
 {$ENDIF}
{$ENDIF}

{$IFDEF VER110}
 {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER125}
 {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER130}
 {$IFDEF BCB}
  {$ObjExportAll On}
 {$ELSE}
  {$DEFINE ISDELPHI}
 {$ENDIF}
{$ENDIF}

{$IFDEF ISDELPHI}
 {$IFDEF DELPHI3_UP}
  {$DEFINE DELPHI_ONLY_UP3}
 {$ENDIF}
{$ENDIF}

unit RoundButtonReg;

interface

procedure Register;

implementation

uses SysUtils, Classes,
  {$IFDEF Delphi6_UP}
     DesignIntf, DesignEditors,
  {$ELSE}
     Dsgnintf,
  {$ENDIF}
     Forms,
     Dialogs,
     RoundButton;

type
  TRoundButtonEditor = class( TComponentEditor )
     function GetVerbCount: integer; override;
     function GetVerb( Index: integer ): String; override;
     procedure ExecuteVerb( Index: integer ); override;
  end;

function TRoundButtonEditor.GetVerbCount : integer ;
begin
  Result := 1;
end;

function TRoundButtonEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := '&About';
  end;
end;

procedure TRoundButtonEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0 : MessageDlg('TRoundButton v1.2' + #13 + #13 +
                   'Copyright (c) 2005-2007 by Ngo Quoc Anh' + #13 +
                    #13 +
                   '-----------------' + #13 +
                    #13 +
                   'All your questions, comments, suggestions and bug' + #13 +
                   'reports are welcomed. Don''t hesitate to contact us.' + #13 +
                    #13 +
                   'E-mail:  bookworm_vn@yahoo.com',
                   mtInformation,
                  [mbOk], 0);
  end;
end;

procedure Register;
begin
  RegisterComponents('NQA - Additional',[TRoundButton]);
  RegisterComponentEditor(TRoundButton, TRoundButtonEditor);
end;

end.
