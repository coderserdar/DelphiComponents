unit EanAceReg;

interface

{$I ean.inc}

uses
        {$ifdef MSWINDOWS}
                Windows, ShellAPI,
                {$ifndef PSOFT_D6}DsgnIntf
                {$else} DesignEditors,DesignIntf
                {$endif};

        {$endif}
        {$ifdef LINUX}
                DesignEditors,DesignIntf;
        {$endif}


const PageEan = 'Barcode components';

type
   TAceEanEditor =class(TDefaultEditor)
   public
   	procedure ExecuteVerb(Index: Integer); override;
   	function GetVerb(Index: Integer): string; override;
   	function GetVerbCount: Integer; override;
   end;

procedure Register;

implementation

uses EanKod, EanAce, EanAceDB, Classes;

{$R EanAce.res}

procedure TAceEanEditor.ExecuteVerb(Index: Integer);
var E:TCustomEan;
begin
   if Component is TAceCustomEan then E:=TAceCustomEan(Component).Ean
   else                               E:=nil;

   if E<>nil then
	case Index of
		0 : E.Copyright;
                1 : E.ActiveSetupWindow('');
            {$ifdef WINDOWS}
                2 : ShellExecute(0, 'open', PChar(BarcodeLibraryHomePage), nil, nil, SW_SHOWNORMAL);
                3 : ShellExecute(0, 'open', PChar('mailto:'+BarcodeLibraryEmail), nil, nil, SW_SHOWNORMAL);
                4 : ShellExecute(0, 'open', PChar(BarcodeLibraryRegisterString), nil, nil, SW_SHOWNORMAL);
            {$endif}
	end;
end;

function TAceEanEditor.GetVerb(Index: Integer): String;
begin
	case Index of
		0 : Result := 'Barcode library - PSOFT company ©1998-2002';
                1 : Result := 'Barcode properties editor';
            {$ifdef WINDOWS}
                2 : Result := 'Barcode library homepage';
                3 : Result := 'Barcode library - send email to authors';
                4 : Result := 'Online registration of Barcode library';
            {$endif}
	end;
end;

function TAceEanEditor.GetVerbCount: Integer;
begin
     {$ifdef WINDOWS}
            Result := 5;
     {$else}
	    Result:= 2;
     {$endif}
end;


procedure Register;
begin
     RegisterComponentEditor(TAceEAN,        TAceEanEditor);
     RegisterComponentEditor(TAceDBEAN,      TAceEanEditor);
     RegisterComponents(PageEan,[TAceEan,TAceDBEan]);
end;

end.
