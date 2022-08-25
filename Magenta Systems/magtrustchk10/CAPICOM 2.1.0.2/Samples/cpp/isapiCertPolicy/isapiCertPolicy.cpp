//*****************************************************************************
//
//  THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
//  EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
//
//  isapiCertPolicy by Jim Harkins
//
//  This sample code demonstrates how the Capicom Certificate and Chain objects
//  can be used to implement a simple ISAPI filter to enforce additional
//  restrictions on Client Certificates used for an SSL session.
//
//  When using SSL and Client Certificates, IIS supports Certificate Trust
//  Lists to limit the Root Certificates used to authenticate Client
//  Certificates.  In some applications it's desirable to have finer grained
//  control over which Client Certificates are accepted.
//
//  This ISAPI filter uses capicom to simplify the implementation of the
//  following additional Client Certificate checks:
//
//      A set of required Extended Key Usage OIDs are matched in the
//      Client Certificate.  In this example we check for "Smart Card
//      Logon", but the same technique can be used to require any
//      EKU. Following accepted practice for X.509 certs, if no EKUs
//      are present, the certificate is assumed good for all purposes,
//      and this check is passed.
//
//      Issuing Certificate's Thumbprint matches an entry in a short list
//      of accepted Thumbprints.  This is an effective and recommended
//      way to limit accepted Client Certificates to a specific set of
//      Intermediate issuing authorities.
//
//  If no Client Certificate is available, this filter allows the connection.
//  A certificate should be available when IIS is configured to Require Client
//  Certificates.  But even secure sites can allow some public content, for
//  example a start page containing information and instructions.  In this case
//  there may be no requirement for Client Certificates or even SLL, and it's
//  appropriate to bypass the additional checks this filter implements.
//
//  This structure and capicom make it easy to implement additional checks.
//
//  Copyright (C) 2002.  Microsoft Corporation.  All rights reserved.
//
//*****************************************************************************

// ISAPICERTPOLICY.CPP - Implementation file for your Internet Server
//    isapiCertPolicy Filter

#include "stdafx.h"
#include "isapiCertPolicy.h"
#include "ErrorTxt.h"

#define ARRAY_SIZE(a) (sizeof(a)/sizeof(*(a)))

const LPWSTR CAPICOM_SMART_CARD_LOGON_OID_W = L"1.3.6.1.4.1.311.20.2.2";
const LPWSTR CORP_HIGH_ASSUR_OID_W = L"1.3.6.1.4.1.311.42.2.1";

const LPWSTR MS_INTRA_L2_USER_1_THUMBPRINT =
    L"15B1691A542A99D7BC3724ABA68FBA7C01B2DEE1";

const LPWSTR MS_INTRA_L2_USER_2_THUMBPRINT =
    L"FA744DC332740D6BD34A708453B79026BBD92DC4";

const LPWSTR MS_INTRA_L2_USER_3_THUMBPRINT =
    L"528567A45B2D41617BC3EC6BA4D275A1B41B05E0";

const char g_szHttpHeader[] = "HTTP/1.0 200 OK\r\n"
    "Content-Type: text/html\r\n"
    "\r\n";

// List of Oids we want to match
static WCHAR const *gc_apszMatchOid[] = {
    CAPICOM_SMART_CARD_LOGON_OID_W,
    CORP_HIGH_ASSUR_OID_W};

static const int gc_iMatchOidCnt = ARRAY_SIZE(gc_apszMatchOid);

// List of acceptable issuer thumbprints
static WCHAR const *gc_apszThumbprint[] = {
    MS_INTRA_L2_USER_1_THUMBPRINT,
    MS_INTRA_L2_USER_2_THUMBPRINT,
    MS_INTRA_L2_USER_3_THUMBPRINT };

static const int gc_iThumbPrintCnt = ARRAY_SIZE(gc_apszThumbprint);


///////////////////////////////////////////////////////////////////////
// The one and only CWinApp object
// NOTE: You may remove this object if you alter your project to no
// longer use MFC in a DLL.

CWinApp theApp;


///////////////////////////////////////////////////////////////////////
// The one and only CIsapiCertPolicyFilter object

CIsapiCertPolicyFilter theFilter;


///////////////////////////////////////////////////////////////////////
// CIsapiCertPolicyFilter implementation

CIsapiCertPolicyFilter::CIsapiCertPolicyFilter()
{
}


CIsapiCertPolicyFilter::~CIsapiCertPolicyFilter()
{
}


BOOL CIsapiCertPolicyFilter::GetFilterVersion(PHTTP_FILTER_VERSION pVer)
{
    // DebugBreak() can be useful for debugging startup code
    // uncomment these lines as needed for testing

//  #if defined(_DEBUG) || defined(DEBUG) /////////////////////
//      DebugBreak();
//  #endif // defined(_DEBUG) || defined(DEBUG) ///////////////

    // Call default implementation for initialization
    CHttpFilter::GetFilterVersion(pVer);

    // Clear the flags set by base class
    pVer->dwFlags &= ~SF_NOTIFY_ORDER_MASK;

    // Set the flags we are interested in
    //
    // Note: Using SF_NOTIFY_AUTHENTICATION and OnAuthentication
    // allows early termination of the connecton.  However, if you
    // need access to the authenticated user name (for exampe to
    // enforce a match with the certificate) then
    // SF_NOTIFY_AUTH_COMPLETE can be used with an implementation
    // of OnAuthComplete. See the ISAPI Filter documentation in
    // the IISSDK for additional information.

    pVer->dwFlags |= SF_NOTIFY_SECURE_PORT | SF_NOTIFY_ORDER_DEFAULT |
        SF_NOTIFY_AUTHENTICATION;

    // Load description string
    TCHAR sz[SF_MAX_FILTER_DESC_LEN+1];
    ISAPIVERIFY(::LoadString(AfxGetResourceHandle(),
            IDS_FILTER, sz, SF_MAX_FILTER_DESC_LEN));
    _tcscpy(pVer->lpszFilterDesc, sz);
    return TRUE;
}


// Ensure Parent Cert's Thumbprint is on our list

BOOL CIsapiCertPolicyFilter::MatchIssuerThumbprint(
    CAPICOM::ICertificate2Ptr Cert)
{
    CAPICOM::IChainPtr Chain(__uuidof(CAPICOM::Chain));
    CAPICOM::ICertificatesPtr Certs(NULL);

    BOOL fOk = Chain->Build(Cert);

    if (fOk)
    {
        Certs = Chain->Certificates;
        fOk = (2 <= Certs->Count);
    }

    if (fOk)
    {
        CAPICOM::ICertificate2Ptr ParentCert(Certs->Item[2]);
        _bstr_t bstrThumbprint(ParentCert->Thumbprint, FALSE);

        fOk = !!bstrThumbprint;

        if (fOk)
        {
            BOOL fFound = FALSE;

            for (
                int iThumbprint = 0;
                iThumbprint < gc_iThumbPrintCnt && !fFound;
                ++iThumbprint)
            {
                fFound = (0 == lstrcmpiW(
                    bstrThumbprint,
                    gc_apszThumbprint[iThumbprint]));
            }
            fOk = fOk && fFound;
        }
    }

    return fOk;
}


// Try to find the given EKU on our list of EKU's

BOOL CIsapiCertPolicyFilter::MatchExtendedKeyUsage(
    CAPICOM::IEKUPtr EKU, BOOL *pfMatchOid)
{
    bool fMatch = FALSE;
    _bstr_t bstrOid(EKU->OID, FALSE);

    // For each Match Oid, while not matched

    for (
        int iOid = 0;
        iOid < gc_iMatchOidCnt && !fMatch;
        ++iOid)
    {
        // Skips OIDs we've already matched
        if (!pfMatchOid[iOid])
        {
            fMatch = (
                !!bstrOid &&
                0 == lstrcmpW(bstrOid, gc_apszMatchOid[iOid]));

            if (fMatch)
            {
                // This OID matched, don't check for it any more
                pfMatchOid[iOid] = TRUE;
            }
        }
    }
    return fMatch;
}

BOOL CIsapiCertPolicyFilter::MatchExtendedKeyUsage(CAPICOM::ICertificate2Ptr Cert)
{
    CAPICOM::IExtendedKeyUsagePtr ExUsage(Cert->ExtendedKeyUsage());
    BOOL fMatch = FALSE;
    BOOL fOk = TRUE;

    // Extended Key Usage represents restrictions if present

    if (0 != ExUsage->IsPresent)
    {
        CAPICOM::IEKUsPtr EKUs(ExUsage->EKUs);
        int iEkuCnt = EKUs->Count;
        int iMatchCnt = 0;

        // If the certificate doesn't have at least as many
        // EKUs as we have OIDs to match, then don't even try.

        if (gc_iMatchOidCnt <= iEkuCnt)
        {
            // BOOL array to track our matches,
            // start with none matched
            BOOL afMatchOid[gc_iMatchOidCnt] = {FALSE};

            for (
                int iEku = 1;
                fOk && iEku <= iEkuCnt && iMatchCnt < gc_iMatchOidCnt;
                ++iEku)
            {
                // For Each EKU in EKUs, while more OIDs to match

                fMatch = MatchExtendedKeyUsage(EKUs->Item[iEku], afMatchOid);

                if (fMatch)
                {
                    // Count this match
                    ++iMatchCnt;
                }
            }
        }
        fOk = fOk && (gc_iMatchOidCnt == iMatchCnt);
    }

    return fOk;
}

BOOL CIsapiCertPolicyFilter::CheckCertPolicy(const _bstr_t &bstrCert)
{
    CAPICOM::ICertificate2Ptr Cert(__uuidof(CAPICOM::Certificate));

    Cert->Import(bstrCert);

    BOOL fOk = MatchIssuerThumbprint(Cert);

    if (fOk)
    {
        fOk =  MatchExtendedKeyUsage(Cert);
    }

    return fOk;
}


DWORD CIsapiCertPolicyFilter::OnAuthentication(
    CHttpFilterContext* pCtxt,
    PHTTP_FILTER_AUTHENT pAuthent)
{
    DWORD dwRet = SF_STATUS_REQ_FINISHED;

    try {
        BOOL fOk = TRUE;
        BOOL fHaveCert = FALSE;

        char CertificateBuf[8192];
        CERT_CONTEXT_EX ccex;
        ccex.cbAllocated = sizeof(CertificateBuf);
        ccex.CertContext.pbCertEncoded = (BYTE*)CertificateBuf;
        ccex.dwCertificateFlags = 0;
        DWORD dwSize = sizeof(ccex);

        fOk = pCtxt->ServerSupportFunction(
            (enum SF_REQ_TYPE)HSE_REQ_GET_CERT_INFO_EX,
            (LPVOID)&ccex,
            &dwSize,
            NULL);

        if (fOk)
        {
            // Do we have a cert?
            fHaveCert = (1 == (1 & ccex.dwCertificateFlags));
        }

        if (fHaveCert)
        {
            // convert ccex's Cert to BSTR

            _bstr_t bstrCert(
                SysAllocStringLen(
                    (OLECHAR * )ccex.CertContext.pbCertEncoded,
                    (ccex.CertContext.cbCertEncoded+1)/2),
                FALSE);

            fOk = CheckCertPolicy(bstrCert);
        }

        if (fOk)
        {
            // Passed checks
            dwRet = SF_STATUS_REQ_NEXT_NOTIFICATION;
        }
        else
        {
            // Error or failed to validate show:
            // HTTP 403.16 - Forbidden: Client certificate untrusted or invalid

            // Write HTTP Header
            DWORD dwBytes = sizeof(g_szHttpHeader) - 1;
            pCtxt->WriteClient((LPVOID)g_szHttpHeader, &dwBytes, 0);

            // Write Error Message

            dwBytes = sizeof(g_szHttpError403_16) - 1;
            pCtxt->WriteClient((LPVOID)g_szHttpError403_16, &dwBytes, 0);
        }
    }
    catch(...)
    {
        // Unexpected Exception, show HTTP 500 - Internal server error

        // Write HTTP Header
        DWORD dwBytes = sizeof(g_szHttpHeader) - 1;
        pCtxt->WriteClient((LPVOID)g_szHttpHeader, &dwBytes, 0);

        // Write Error Message

        dwBytes = sizeof(g_szHttpError500) - 1;
        pCtxt->WriteClient((LPVOID)g_szHttpError500, &dwBytes, 0);
    }

    return dwRet;
}


// Do not edit the following lines, which are needed by ClassWizard.
#if 0
BEGIN_MESSAGE_MAP(CIsapiCertPolicyFilter, CHttpFilter)
    //{{AFX_MSG_MAP(CIsapiCertPolicyFilter)
    //}}AFX_MSG_MAP
END_MESSAGE_MAP()
#endif  // 0

///////////////////////////////////////////////////////////////////////
// If your extension will not use MFC, you'll need this code to make
// sure the extension objects can find the resource handle for the
// module.  If you convert your extension to not be dependent on MFC,
// remove the comments arounn the following AfxGetResourceHandle()
// and DllMain() functions, as well as the g_hInstance global.

/****

static HINSTANCE g_hInstance;

HINSTANCE AFXISAPI AfxGetResourceHandle()
{
    return g_hInstance;
}

BOOL WINAPI DllMain(HINSTANCE hInst, ULONG ulReason,
                    LPVOID lpReserved)
{
    if (ulReason == DLL_PROCESS_ATTACH)
    {
        g_hInstance = hInst;
    }

    return TRUE;
}

****/
