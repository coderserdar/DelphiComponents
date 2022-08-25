/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


 THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
 EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.

 Copyright (C) 1999 - 2000.  Microsoft Corporation.  All rights reserved.

 Module:      Store.cpp

 Abstract:    Main program of CAPICOM Store sample. See ReadMe.txt for more 
              detail information about this sample.

 Environment: Win32 console, UNICODE ready.

------------------------------------------------------------------------------*/

#include <tchar.h>
#include <stdio.h>
#include <atlbase.h>
#include <windows.h>

#pragma warning (disable : 4192)

//
// Import TLB from DLL 
//
// Note: Make sure either you have the DLL in the current directory, or point 
//       it to the correct directory on you drive.
//
#import "capicom.dll"

//
// Use CAPICOM namespace.
//
using namespace CAPICOM;

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  Module: main()

  Remark: Entry point of CAPICOM Store C++ sample.

-----------------------------------------------------------------------------*/

int __cdecl _tmain (int argc, _TCHAR  * argv[])
{
    HRESULT hr = S_OK;

    //
    // Initialize COM library.
    //
    CoInitialize(0);

    try
    {
        //
        // Open current user My store.
        //
        _bstr_t   bstrName = _T("My");
        IStorePtr pIStore(__uuidof(Store));

        if (FAILED(hr = pIStore->Open(CAPICOM_CURRENT_USER_STORE,
                                      bstrName, 
                                      CAPICOM_STORE_OPEN_READ_ONLY)))
        {
            ATLTRACE(_T("Error [%#x]: pIStore->Open() failed at line %d.\n"), hr, __LINE__);
            throw hr;
        }

        //
        // Display all certificate in the store.
        //
        IUnknownPtr     pIUnknown;
        IEnumVARIANTPtr pIEnum;
        _variant_t      pDisp;
        ULONG           ulFetched;

        //
        // Get _NewEnum of Certificates collection.
        //
        if (FAILED(hr = pIStore->Certificates->get__NewEnum(&pIUnknown)))
        {
            ATLTRACE(_T("Error [%#x]: pIStore->Certificates->get__NewEnum() failed at line %d.\n"), hr, __LINE__);
            throw hr;
        }

        //
        // Get IEnumVARIANT interface of _NewEnum.
        //
        if (FAILED(hr = pIUnknown->QueryInterface(IID_IEnumVARIANT, (void **) &pIEnum)))
        {
            ATLTRACE(_T("Error [%#x]: pIUnknown->QueryInterface() failed at line %d.\n"), hr, __LINE__);
            throw hr;
        }

        //
        // Now loop through all items in the collection.
        //
        while (pIEnum->Next(1, &pDisp, &ulFetched) == S_OK)
        {
            //
            // Display the certificate.
            //
            if (FAILED(hr = ((ICertificatePtr) pDisp.pdispVal)->Display()))
            {
                ATLTRACE(_T("Error [%#x]: ((ICertificatePtr) pDisp.pdispVal)->Display() failed at line %d.\n"), hr, __LINE__);
                throw hr;
            }

            pDisp.Clear();
        }
    }

    catch (_com_error e) 
    {
        hr = e.Error();
        ATLTRACE(_T("Error [%#x]: %s.\n"), hr, e.ErrorMessage());
    }

    catch (HRESULT hr)
    {
        ATLTRACE(_T("Error [%#x]: CAPICOM error.\n"), hr);
    }

    catch(...)
    {
        hr = CAPICOM_E_UNKNOWN;
        ATLTRACE(_T("Unknown error.\n"));
    }

    CoUninitialize();

    return (int) hr;
}