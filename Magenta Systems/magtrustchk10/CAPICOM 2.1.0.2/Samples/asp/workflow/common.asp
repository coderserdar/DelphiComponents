<%
Option Explicit
Const CAPICOM_INFO_SUBJECT_SIMPLE_NAME = 0
Const CAPICOM_INFO_SUBJECT_EMAIL_NAME = 2
Const CAPICOM_CHECK_TRUSTED_ROOT = 1
Const CAPICOM_CHECK_TIME_VALIDITY = 2
Const CAPICOM_CHECK_SIGNATURE_VALIDITY = 4
Const CAPICOM_CHECK_ONLINE_REVOCATION_STATUS = 8
const CAPICOM_CHECK_ONLINE_ALL = 495 
Const CAPICOM_TRUST_IS_REVOKED = 4 
Const CAPICOM_TRUST_REVOCATION_STATUS_UNKNOWN = 64
Const DATASTORE_LASTTRANS = "CAPICOM.LastTransaction"
Const DATASTORE_TRANSPREFIX = "CAPICOM.Trans." 
Const DATASTORE_TRANSSUFFIX = ".approved" 
Const CAPICOM_INIT = "CAPICOM.Initialized"
Const DATASTORE_MAXTRANS = "CAPICOM.MaxTrans"
Const SELF_APPROVAL = "CAPICOM.SelfApproval"
Const CHECK_REVOCATION = "CAPICOM.CheckRevocation"
''''''''''''''''''''''''''''''
'' Set the SELF_APPROVAL constant to False to enable the AD integration
''''''''''''''''''''''''''''''
Dim bInitialized

Sub PrintErrorMessage( strErrorMsg )
  Response.Write "<FONT size=5 color=""red""><STRONG>"
  Response.Write strErrorMsg
  Response.Write "</STRONG></FONT></P>"
  err.clear
End Sub


Function FormatStrong ( msg ) 
  FormatStrong = "<FONT color=""red""><STRONG>" & msg & "</STRONG></FONT>"

End Function

Sub Initialize
    Application.Lock
    if Not CBool( Application(CAPICOM_INIT) ) Then
        Application(SELF_APPROVAL) = True
        Application(CHECK_REVOCATION) = False
        Application(DATASTORE_MAXTRANS) = 10
        Application(CAPICOM_INIT) = True
    end If
    Application.Unlock
End Sub
'''''''''''''''''''''''''''''''''''''''
'' Verification functions
''
'''''''''''''''''''''''''''''''''''''''
Function IsRevocationEnabled
    IsRevocationEnabled = CBool (Application (CHECK_REVOCATION))
End Function

Function SetRevocation ( setting ) 
    Application (CHECK_REVOCATION) = setting
End Function

Function VerifyTheSignedString( strSignedData, oSignedData )
  On Error Resume Next
  Dim oKeyUsage
  Dim oSigner
  Dim chain
  Dim cert
  Err.clear
  oSignedData.Verify strSignedData
  if Err.Number = 0 Then
    For each oSigner in oSignedData.Signers
      Set oKeyUsage = oSigner.Certificate.KeyUsage
      if Not oKeyUsage Is nothing And oKeyUsage.IsPresent = True And oKeyUsage.IsDigitalSignatureEnabled = False Then
        PrintErrorMessage "Verification Error.  The key is not valid for digital signature."
        Response.Write "<p><table border=1>"
        Response.Write "<tr><td> Subject Name </td><td>" &  oSigner.Certificate.SubjectName & "</td></tr>"
        Response.Write "<tr><td> Issuer Name </td><td>" &  oSigner.Certificate.IssuerName & "</td></tr>"
        Response.Write "<tr><td> Valid To </td><td>" &  oSigner.Certificate.ValidToDate & "</td></tr>"
        Response.Write "<tr><td> Thumbprint </td><td>" &  oSigner.Certificate.Thumbprint & "</td></tr>"
        Response.Write "<tr><td> <b>IsDigitalSignatureEnabled </b></td><td><b> " & oKeyUsage.IsDigitalSignatureEnabled  & "</b></td></tr>"
        Response.Write "<tr><td> IsKeyEnciphermentEnabled </td><td> " & oKeyUsage.IsKeyEnciphermentEnabled  & "</td></tr>"
        Response.Write "<tr><td> IsDataEnciphermentEnabled </td><td> " & oKeyUsage.IsDataEnciphermentEnabled  & "</td></tr>"
        Response.Write "</table><p>"
        VerifyTheSignedString = False
        Exit function
      end if
      if IsRevocationEnabled Then
        for each cert in oSignedData.Certificates
             cert.IsValid.CheckFlag = CAPICOM_CHECK_ONLINE_ALL 
          If cert.IsValid.Result Then
             'CERTIFICATE IS VALID
          Else
			 Set chain = CreateObject("CAPICOM.Chain")
             chain.Build cert
             If CAPICOM_TRUST_IS_REVOKED and chain.Status Then
                VerifyTheSignedString = False
                PrintErrorMessage "At least one certificate in the chain has been revoked for " & cert.SubjectName
                Exit function
             End If
             If CAPICOM_TRUST_REVOCATION_STATUS_UNKNOWN and chain.Status Then
                VerifyTheSignedString = False
                PrintErrorMessage "The revocation status could not be determined for " & cert.SubjectName
                Exit function
             End If
          End If
        Next
      End If
    Next 
    VerifyTheSignedString = True
  Else
    VerifyTheSignedString = False
  End if
End Function


'''''''''''''''''''''''''''''''''''''''
'' Persistence functions
''
'' You should modify these functions to persist the transactions
'' to the data store of your choosing.  In this sample, they are
'' stored using the Application global objects.  The values in the
'' Application object are lost when the server is restarted.
''
'' AssignNextTransaction 
'' GetNextTransaction
'' SaveSignedDocument
'' GetTransData
'' IsTransactionApproved
'' ApproveTransaction
'' GetTransactionApprovalTime
''
'''''''''''''''''''''''''''''''''''''''

Function IsTransactionApproved ( intTransactionID )
   If Len ( Application(DATASTORE_TRANSPREFIX & intTransactionID & DATASTORE_TRANSSUFFIX ) ) > 0 Then
     IsTransactionApproved = True
   Else
     IsTransactionApproved = False
   End If
end Function

Sub ApproveTransaction ( intTransactionID )
   Application(DATASTORE_TRANSPREFIX & intTransactionID & DATASTORE_TRANSSUFFIX ) = Now
End Sub

Function GetTransactionApprovalTime ( intTransactionID )
   GetTransactionApprovalTime = Application(DATASTORE_TRANSPREFIX & intTransactionID & DATASTORE_TRANSSUFFIX )
End Function

Function AssignNextTransaction 
  if CInt( Application( DATASTORE_LASTTRANS ) )  >= GetMaxTransaction Then
    ClearTrans
  end if
  Application.Lock
  Application( DATASTORE_LASTTRANS ) = Application( DATASTORE_LASTTRANS ) + 1
  AssignNextTransaction = Application( DATASTORE_LASTTRANS ) 
  Application.Unlock
End Function

Function GetNextTransaction
  GetNextTransaction = Application( DATASTORE_LASTTRANS )
End Function

sub SaveSignedDocument ( intNextIdNumber, strSignedData )
  Application(DATASTORE_TRANSPREFIX & intNextIdNumber) =  strSignedData
end sub

Function GetTransData ( intTransactionID ) 
  GetTransData = Application(DATASTORE_TRANSPREFIX & intTransactionID )
End Function

Sub ClearTrans
  Application.Lock
  Dim count 
  Dim i 
  count = GetNextTransaction
  for i = 1 to count
    Application(DATASTORE_TRANSPREFIX & i & DATASTORE_TRANSSUFFIX ) = ""
    Application(DATASTORE_TRANSPREFIX & i ) = ""
    Application( DATASTORE_LASTTRANS ) = 0
  next 
  Application.Unlock
End Sub

Function GetMaxTransaction
  GetMaxTransaction = Application(DATASTORE_MAXTRANS)
End Function

Function SetMaxTransaction ( max )
  Application(DATASTORE_MAXTRANS) = CInt ( max )
End Function
''''''''''''''''''''''''''''''''''''''''
''
''  Approver Lookup function
''
''''''''''''''''''''''''''''''''''''''''
Function GetSelfApproval
    GetSelfApproval = CBool (Application (SELF_APPROVAL))
End Function

Function SetApproval ( setting ) 
    Application (SELF_APPROVAL) = setting
End Function

Function LookupMgrinAD( strEmailAddress ) 
on error resume next
  Dim strQuery, strNamingContext
  Dim oConnection
  Dim oCmd
  Dim oRecordset
  Dim strADOQuery
  Dim oUser
  Dim oRootDSE
  Dim strOutput
  Dim vProp
  Dim strProp
  if GetSelfApproval Then
     LookupMgrinAD = strEmailAddress
     exit function
  end if
  Set oRootDSE = GetObject("LDAP://RootDSE")
  strNamingContext = oRootDSE.Get("defaultNamingContext")
  
  strQuery = "<LDAP://" & strNamingContext & ">;(&(objectCategory=person)(objectClass=user)(mail=" &   strEmailAddress & "));cn,adspath;subtree"

  ' -- Set up the connection ---
  Set oConnection = CreateObject("ADODB.Connection")
  Set oCmd = CreateObject("ADODB.Command")

  oConnection.Provider = "ADsDSOObject"
  oConnection.Open "ADs Provider"
  Set oCmd.ActiveConnection = oConnection
  'oCmd.Properties("Chase referrals") = ADS_CHASE_REFERRALS_EXTERNAL ‘64
  oCmd.CommandText = strQuery
  Set oRecordset = oCmd.Execute
  If oRecordset.RecordCount = 0 Then
    PrintErrorMessage "The user could not be found in the directory<p>e-mail address:<tt>"& strEmailAddress & "</tt>"
    Exit function
  End If
  If oRecordset.RecordCount <> 1 Then
    PrintErrorMessage "Did not find exactly one " & strUserCommonName & vbLf & "<P>"
    Exit function
  End If
  oRecordset.MoveFirst
  Set oUser = GetObject(oRecordset.Fields("ADsPath"))  'Use the returned ADsPath to bind to the user
  Set oUser = GetObject("LDAP://" & oUser.Manager) 'Get the manager's object
  LookupMgrinAD = oUser.EmailAddress
End function


%>

