unit iPaperModelReg;

interface

{$i iformedit.inc}

uses Classes,
        {$ifdef D6+}
                designeditors,designintf, 
        {$else}
                DsgnIntf,
        {$endif}
        ipsConst, iPaperModel;

type

   TpsPaperModelEditor =class(TDefaultEditor)
   public
        procedure Edit; override;
   	procedure ExecuteVerb(Index: Integer); override;
   	function GetVerb(Index: Integer): string; override;
   	function GetVerbCount: Integer; override;
   end;


procedure Register;

implementation

uses Controls, iPaperModelProp;

procedure Register;
begin
        RegisterComponentEditor(TpsPaperModel,   TpsPaperModelEditor);
end;

{ TPaperModelEditor }

procedure TpsPaperModelEditor.Edit;
begin
  inherited;
  ExecuteVerb(0);
end;

procedure TpsPaperModelEditor.ExecuteVerb(Index: Integer);
begin
        if Index in [0,1] then
                EditPaperModelProps(TpsPaperModel(Component),Index=1);
end;

function TpsPaperModelEditor.GetVerb(Index: Integer): string;
begin
	case Index of
		0 : Result := 'PaperModel component - PSOFT company © 2000';
                1 : Result := 'PaperModel component - version : '+PSOFT_PaperModel_Version;
	end;
end;

function TpsPaperModelEditor.GetVerbCount: Integer;
begin
        Result := 2;
end;

end.
