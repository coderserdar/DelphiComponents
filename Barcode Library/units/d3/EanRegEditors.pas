unit EanRegEditors;

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

type
   TEanEditor =class(TDefaultEditor)
   public
        procedure Edit; override;
   	procedure ExecuteVerb(Index: Integer); override;
   	function GetVerb(Index: Integer): string; override;
   	function GetVerbCount: Integer; override;
   end;

   TPDF417PropertyEditor=class(TClassProperty)
   public
         function  GetAttributes:TPropertyAttributes; override;
         procedure Edit; override;
   end;

procedure Register;


implementation

uses EanKod,EanDB,EanQR,EanDBQr,Classes;

procedure TEanEditor.Edit;
begin
        ExecuteVerb(1);
end;

procedure TEanEditor.ExecuteVerb(Index: Integer);
var E:TCustomEan;
begin
   E:=nil;
   if Component is TCustomEan then E:=TCustomEan(Component);
   if Component is TQrCustomEan then E:=TQrCustomEan(Component).Ean;

   if E<>nil then
	case Index of
		0 : E.Copyright;
                1 : E.ActiveSetupWindow('');
            {$ifdef MSWINDOWS}
                2 : ShellExecute(0, 'open', PChar(BarcodeLibraryHomePage), nil, nil, SW_SHOWNORMAL);
                3 : ShellExecute(0, 'open', PChar('mailto:'+BarcodeLibraryEmail), nil, nil, SW_SHOWNORMAL);
                4 : ShellExecute(0, 'open', PChar(BarcodeLibraryRegisterString), nil, nil, SW_SHOWNORMAL);
            {$endif}
	end;
end;

function TEanEditor.GetVerb(Index: Integer): String;
begin
	case Index of
		0 : Result := 'Barcode library - PSOFT company ©1998-2002';
                1 : Result := 'Barcode properties editor';
            {$ifdef MSWINDOWS}
                2 : Result := 'Barcode library homepage';
                3 : Result := 'Barcode library - send email to authors';
                4 : Result := 'Online registration of Barcode library';
            {$endif}
	end;
end;

function TEanEditor.GetVerbCount: Integer;
begin
     {$ifdef MSWINDOWS}
	Result:= 5;
     {$endif}
     {$ifdef LINUX}
	Result:= 2;
     {$endif}
end;

function  TPDF417PropertyEditor.GetAttributes:TPropertyAttributes;
begin
     Result := [paDialog, paSubProperties];
end;


procedure TPDF417PropertyEditor.Edit;
var E:TCustomEan;
begin
     E:=TCustomEan(TpsPDF417(GetOrdValue).Ean);
     E.ActiveSetupWindow('SH_PDF417');
end;

procedure Register;
begin
     RegisterComponentEditor(TCustomEAN,   TEanEditor);
     RegisterComponentEditor(TQRCustomEAN, TEanEditor);
     RegisterPropertyEditor(TypeInfo(TpsPDF417), nil, '',TPDF417PropertyEditor);
end;

end.
