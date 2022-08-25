unit MagSignProg;

{
Updated by Angus Robertson, Magenta Systems Ltd, England, 23rd November 2018
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

SignProg functions are designed for Code Signing, aka Microsoft
Authenticode. 

Currently just two functions that check if an EXE, DLL or CAB file has
a valid code signing certificate, that the certificate is trusted and
that the program is not corrupted.  These functions were written to
support an remote program updater tool, to ensure the new program was
not corrupted, but may also be used to self test a Delphi application
for image corruption.

ProgVerifyTrust - simple function to check code signing certificate exists
and is valid and not expired, and the program image is not corrupted.
This should work with Windows 2000 and later (which have wintrust.dll).

ProgVerifyCert - similar to ProgVerifyTrust, but also extracts the
certificate information, names, dates, etc.  But this function needs
capicom.dll COM object to be installed (from which the CAPICOM_TLB type
library is created), which is a free redistributable file, included with
this code.

Microsoft claims Capicom is deprecated with Windows 7, but the DLL still works.
You need to register the COM object by running 'regsvr32.exe capicom.dll'

Note the API used in CapiCom to sign a program is SignerSign but is not
yet supported here since signprog.exe works fine to actually sign code.


}

interface

uses Windows, sysutils, ActiveX, ComObj, CAPICOM_TLB, wintrust, wcrypt2,
  magsubs1 ;

type

    TCertInfo = record
        Res: integer ;
        Response: string ;
        SignerName: string ;
        SignerIssuer: string ;
        SignerExpireDT: TDateTime ;
        DescName: string ;
        DescURL: string ;
        StamperName: string ;
        SigningDT: TDateTime ;
    end ;

function ProgVerifyTrust (const Fname: string ; const HashOnly,
                        Expired: boolean; var Response: string): integer ;
function ProgVerifyCert (const Fname: string ; var CertInfo: TCertInfo): integer ;

implementation

function ProgVerifyTrust (const Fname: string ; const HashOnly,
                        Expired: boolean; var Response: string): integer ;
var
    ActionID: TGUID ;
    WinTrustData: TWinTrustData ;
    WinTrustFileInfo: TWinTrustFileInfo ;
    WFname: WideString ;
begin
    result := -1 ;
    if Win32Platform < VER_PLATFORM_WIN32_NT then
    begin
        response := 'WinVerifyTrust Not Available on Win9x' ;
        exit ;
    end ;
    if (@WinVerifyTrust = nil) then
    begin
        response := 'WinVerifyTrust Not Loaded' ;
        exit ;
    end ;
    if NOT FileExists (Fname) then
    begin
        Response := 'Program File Not Found' ;
        exit ;
    end ;
    WinTrustFileInfo.cbStruct := SizeOf (TWinTrustFileInfo) ;
    WFname := Fname ;
    WinTrustFileInfo.pcwszFilePath := @WFname [1] ;
    WinTrustFileInfo.hFile := 0 ;
    WinTrustFileInfo.pgKnownSubject := Nil ;
    WinTrustData.cbStruct := SizeOf (TWinTrustData) ;
    WinTrustData.pPolicyCallbackData := Nil ;
    WinTrustData.pSIPClientData := Nil ;
    WinTrustData.dwUIChoice := WTD_UI_NONE ;
    WinTrustData.fdwRevocationChecks := WTD_REVOKE_NONE ;
    WinTrustData.dwUnionChoice := WTD_CHOICE_FILE ;
    WinTrustData.Info.pFile := @WinTrustFileInfo ;
    WinTrustData.dwStateAction := 0 ;
    WinTrustData.hWVTStateData := 0 ;
    WinTrustData.pwszURLReference := Nil ;
    WinTrustData.dwProvFlags := WTD_REVOCATION_CHECK_NONE ;
    if HashOnly then WinTrustData.dwProvFlags :=
                            WinTrustData.dwProvFlags OR WTD_HASH_ONLY_FLAG ;      // ignore certificate 
    if Expired then WinTrustData.dwProvFlags :=
                        WinTrustData.dwProvFlags OR WTD_LIFETIME_SIGNING_FLAG ;   // check expired date
    WinTrustData.dwUIContext := WTD_UICONTEXT_EXECUTE ;
    ActionID := WINTRUST_ACTION_GENERIC_VERIFY_V2 ;
    Result := WinVerifyTrust (INVALID_HANDLE_VALUE, ActionID, @WinTrustData) ;
    case Result of
      ERROR_SUCCESS:
         response := 'Trusted Code';
      TRUST_E_SUBJECT_NOT_TRUSTED:
         response := 'Not Trusted Code';
      TRUST_E_PROVIDER_UNKNOWN:
         response := 'Trust Provider Unknown';
      TRUST_E_ACTION_UNKNOWN:
         response := 'Trust Provider Action Unknown';
      TRUST_E_SUBJECT_FORM_UNKNOWN:
         response := 'Trust Provider Form Unknown';
      TRUST_E_NOSIGNATURE:
         response := 'Unsigned Code';
      TRUST_E_EXPLICIT_DISTRUST:
         response := 'Certificate Marked as Untrusted by the User';
      TRUST_E_BAD_DIGEST:
         response := 'Code has been Modified' ;
      CERT_E_EXPIRED:
        response := 'Signed Code But Certificate Expired' ;
      CERT_E_CHAINING:
        response := 'Signed Code But Certificate Chain Not Trusted' ;
      CERT_E_UNTRUSTEDROOT:
        response := 'Signed Code But Certificate Root Not Trusted' ;
      CERT_E_UNTRUSTEDTESTROOT:
        response := 'Signed Code But With Untrusted Test Certificate' ;
      CRYPT_E_SECURITY_SETTINGS:
         response := 'Local Security Options Prevent Verification';
      else
         response := 'Trust Error: ' + FormatLastError ;
    end ;
end ;

function ProgVerifyCert (const Fname: string ; var CertInfo: TCertInfo): integer ;
var
    SignedCode: ISignedCode ;
    AuthAttr: IAttribute ;
    Enum: IEnumVariant ;
    ovVar: OleVariant;
    lwValue: LongWord;
//    Certificate: ICertificate2 ;
//    S: string ;
begin
    result := -1 ;
    if NOT FileExists (Fname) then
    begin
        CertInfo.Res := result ;
        CertInfo.Response := 'Program File Not Found' ;
        exit ;
    end ;
    try
        SignedCode := CoSignedCode.Create ;
        SignedCode.FileName := Fname ;
        SignedCode.Verify (false);
        result := ERROR_SUCCESS ;   // otherwise exception
        CertInfo.Response := 'Trusted Code' ;
    except
        result := EOleSysError (ExceptObject).ErrorCode ;
        case Result of
          ERROR_SUCCESS:
             CertInfo.Response := 'Trusted Code';
          TRUST_E_SUBJECT_NOT_TRUSTED:
             CertInfo.Response := 'Not Trusted Code';
          TRUST_E_PROVIDER_UNKNOWN:
             CertInfo.Response := 'Trust Provider Unknown';
          TRUST_E_ACTION_UNKNOWN:
             CertInfo.Response := 'Trust Provider Action Unknown';
          TRUST_E_SUBJECT_FORM_UNKNOWN:
             CertInfo.Response := 'Trust Provider Form Unknown';
          TRUST_E_NOSIGNATURE:
             CertInfo.Response := 'Unsigned Code';
          TRUST_E_EXPLICIT_DISTRUST:
             CertInfo.Response := 'Certificate Marked as Untrusted by the User';
          TRUST_E_BAD_DIGEST:
             CertInfo.Response := 'Code has been Modified' ;
          CERT_E_EXPIRED:
            CertInfo.Response := 'Signed Code But Certificate Expired' ;
          CERT_E_CHAINING:
            CertInfo.Response := 'Signed Code But Certificate Chain Not Trusted' ;
          CERT_E_UNTRUSTEDROOT:
            CertInfo.Response := 'Signed Code But Certificate Root Not Trusted' ;
          CERT_E_UNTRUSTEDTESTROOT:
            CertInfo.Response := 'Signed Code But With Untrusted Test Certificate' ;
          CRYPT_E_SECURITY_SETTINGS:
             CertInfo.Response := 'Local Security Options Prevent Verification';
          REGDB_E_CLASSNOTREG:
             CertInfo.Response := 'capicom.dll Missing or Not Registered';
          else
             CertInfo.Response := 'Error: ' + GetExceptMess (ExceptObject) ;
        end ;
    end ;
    CertInfo.Res := result ;
    if (result = ERROR_SUCCESS) or
       (result = TRUST_E_EXPLICIT_DISTRUST) or
       (result = TRUST_E_BAD_DIGEST) or
       (result = CERT_E_EXPIRED) or
       (result = CERT_E_CHAINING) or
       (result = CERT_E_UNTRUSTEDROOT) or
       (result = CERT_E_UNTRUSTEDTESTROOT) then
    begin
        try
            CertInfo.DescName := SignedCode.Description ;
            CertInfo.DescURL := SignedCode.DescriptionURL ;
            CertInfo.SignerName := SignedCode.Signer.Certificate.GetInfo
                                        (CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME)  ;
            CertInfo.SignerIssuer := SignedCode.Signer.Certificate.GetInfo
                                         (CAPICOM_CERT_INFO_ISSUER_SIMPLE_NAME) ;
            CertInfo.SignerExpireDT := SignedCode.Signer.Certificate.ValidToDate ;
            if Assigned (SignedCode.TimeStamper) then
            begin
                CertInfo.StamperName := SignedCode.TimeStamper.Certificate.GetInfo
                                        (CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME)  ;
                if SignedCode.TimeStamper.AuthenticatedAttributes.Count > 0 then
                begin
                    Enum := (SignedCode.TimeStamper.AuthenticatedAttributes._NewEnum) as IEnumVariant;
                    while (Enum.Next (1, ovVar, lwValue) = S_OK) do
                    begin
                        AuthAttr := IUnknown(ovVar) as IAttribute;
                        if AuthAttr.Name = CAPICOM_AUTHENTICATED_ATTRIBUTE_SIGNING_TIME then
                                            CertInfo.SigningDT  := AuthAttr.Value ;  // UTC time
                    end ;
                end ;
            end ;
          // could get SignedCode.Signer.Chain

         // could display all certificates
         {   if SignedCode.Certificates.Count > 0 then
            begin
                Enum := (SignedCode.Certificates._NewEnum) as IEnumVariant;
                while (Enum.Next (1, ovVar, lwValue) = S_OK) do
                begin
                    Certificate := IUnknown(ovVar) as ICertificate2;
                    S := '' ;
                    if Certificate.BasicConstraints.IsPresent then
                    begin
                        if Certificate.BasicConstraints.IsCertificateAuthority then
                                                    S := S + 'Only Cert Auth; ' ;
                        if Certificate.BasicConstraints.IsCritical then
                                                    S := S + 'Critical; ' ;
                    end ;
                    if Certificate.ExtendedProperties.
                    S := S + '; Name= ' + Certificate.GetInfo (CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME) ;
                    S := S + '; Issuer= ' + Certificate.GetInfo (CAPICOM_CERT_INFO_ISSUER_SIMPLE_NAME) ;
                    S := S + '; ValidTo= ' + DateToStr (Certificate.ValidToDate) ;
                    S := S + '; Subject=' + Certificate.SubjectName ;
                    Response := Response + CRLF + S ;
                end ;
            end ;   }
        except
        end ;
    end ;
end ;

end.
