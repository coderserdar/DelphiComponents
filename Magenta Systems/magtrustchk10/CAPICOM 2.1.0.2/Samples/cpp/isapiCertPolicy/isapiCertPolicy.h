#if !defined(AFX_ISAPICERTPOLICY_H__1E90AFFB_FEEA_4CE0_A3CA_5DDB665CA990__INCLUDED_)
#define AFX_ISAPICERTPOLICY_H__1E90AFFB_FEEA_4CE0_A3CA_5DDB665CA990__INCLUDED_

// ISAPICERTPOLICY.H - Header file for your Internet Server
//    isapiCertPolicy Filter

#include "resource.h"


class CIsapiCertPolicyFilter : public CHttpFilter
{
public:
    CIsapiCertPolicyFilter();
    ~CIsapiCertPolicyFilter();

// Overrides
    // ClassWizard generated virtual function overrides
        // NOTE - the ClassWizard will add and remove member functions here.
        //    DO NOT EDIT what you see in these blocks of generated code !
    //{{AFX_VIRTUAL(CIsapiCertPolicyFilter)
public:
    virtual BOOL GetFilterVersion(PHTTP_FILTER_VERSION pVer);
    virtual DWORD OnAuthentication(CHttpFilterContext* pCtxt, PHTTP_FILTER_AUTHENT pAuthent);

private:
    static BOOL MatchIssuerThumbprint(
        CAPICOM::ICertificate2Ptr Cert);

    static BOOL MatchExtendedKeyUsage(
        CAPICOM::IEKUPtr EKU, BOOL *pfMatchOid);

    static BOOL MatchExtendedKeyUsage(CAPICOM::ICertificate2Ptr Cert);
    static BOOL CheckCertPolicy(const _bstr_t &bstrCert);

    //}}AFX_VIRTUAL

    //{{AFX_MSG(CIsapiCertPolicyFilter)
    //}}AFX_MSG
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_ISAPICERTPOLICY_H__1E90AFFB_FEEA_4CE0_A3CA_5DDB665CA990__INCLUDED)
