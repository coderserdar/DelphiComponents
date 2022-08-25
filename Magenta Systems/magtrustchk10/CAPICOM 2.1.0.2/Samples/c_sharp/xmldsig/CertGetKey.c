#include <stdio.h>
#include <windows.h>
#include <wincrypt.h>

typedef struct  {
    BLOBHEADER    blob;
    RSAPUBKEY     rsa;
} KEY_HEADER;

void memrev(LPBYTE pb, DWORD cb)
{
    BYTE    b;
    DWORD   i;
    LPBYTE  pbEnd = pb+cb-1;
    
    for (i=0; i<cb/2; i++, pb++, pbEnd--) {
        b = *pb;
        *pb = *pbEnd;
        *pbEnd = b;
    }
}

// Free the allocated blob array
__declspec( dllexport )
void UnmanagedArrayFree (BYTE * ppbKeyBlob) // [in] memory address to free
{
	free(ppbKeyBlob);
}

// Export public-key information given a certificate file
__declspec( dllexport )
BOOL PublicKeyBlobFromCertificateRawData (BYTE * rgbRawData, 
									   // [in] parameter holding the raw data array size										
									   int cbRawData,
 									   // [out] parameter holding the blob byte array
									   BYTE ** ppbKeyBlob, 
									   // [out] parameter holding the blob array size
									   UINT * pKeyBlobSize) 
{
	PCCERT_CONTEXT     pCert = NULL;
    DWORD              dwEncodingType = X509_ASN_ENCODING|PKCS_7_ASN_ENCODING;

	HCRYPTPROV hCryptProv = 0;
	HCRYPTKEY hKey = 0;

	DWORD dwBufferLen = 0;
    DWORD               dwBlobLen = 0;

	BOOL retValue = TRUE;

    // Create certificate context
    pCert = CertCreateCertificateContext(
                            dwEncodingType,
							(unsigned const char *) rgbRawData,
							cbRawData);
	
	if (pCert == NULL) {
		retValue = FALSE;
		goto Cleanup;
	}

	// Acquire a cryptographic context
	if (!CryptAcquireContext(&hCryptProv, 
				NULL,
				NULL, 
				PROV_RSA_FULL, 
				0)) { 
		if (!CryptAcquireContext(&hCryptProv, 
				NULL,
				NULL, 
				PROV_RSA_FULL, 
				CRYPT_NEWKEYSET)) { 
			retValue = FALSE;
			goto Cleanup;
		}
	}

	// Verify the date validity
	if (CertVerifyTimeValidity(
			NULL,
			pCert->pCertInfo) != 0) {
		retValue = FALSE;
		goto Cleanup;
	}

	// Maybe should have some validation of the certificate in case we can do it ...

	// Import the public key information
	if (!CryptImportPublicKeyInfoEx(
			hCryptProv,
			dwEncodingType,
			&(pCert->pCertInfo->SubjectPublicKeyInfo),
			0,
			0,
			NULL,
			&hKey)) {
		HRESULT hr = GetLastError();
		retValue = FALSE;
		goto Cleanup;
	}

	// Export the key into a key blob
	if (!CryptExportKey(
			hKey,
			0,
			PUBLICKEYBLOB,
			0,
			NULL,
			&dwBlobLen)) {
		retValue = FALSE;
		goto Cleanup;
    }

	*pKeyBlobSize = dwBlobLen;
    *ppbKeyBlob = (LPBYTE) malloc(dwBlobLen * sizeof(BYTE));
    if (*ppbKeyBlob == NULL) {
		retValue = FALSE;
		goto Cleanup;
    }
    
    if (!CryptExportKey(
			hKey,
			0,
			PUBLICKEYBLOB,
			0,
			*ppbKeyBlob,
			&dwBlobLen)) {
		retValue = FALSE;
		goto Cleanup;
    }
	
Cleanup:
	if (hKey)
		CryptDestroyKey(hKey);

	if (pCert)
		CertFreeCertificateContext(pCert);

	if (hCryptProv)
		CryptReleaseContext(hCryptProv, 0);

	return retValue;
}

// Export public-key information given a certificate file
__declspec( dllexport )
void ImportPublicKeyBlob (
 					   // [in] parameter holding the blob byte array
					   BYTE * pbKeyBlob, 
					   // [in] parameter holding the blob array size
					   int KeyBlobSize,
					   // [out] parameter holding the exponent array size
					   UINT * pExponent,
  					   // [out] parameter holding the modulus byte array
					   BYTE ** ppbModulus, 
					   // [out] parameter holding the modulus array size
					   UINT * pModulusSize) 
{
    KEY_HEADER * pKeyInfo = (KEY_HEADER *) pbKeyBlob;
    DWORD cb = (pKeyInfo->rsa.bitlen/8);
    LPBYTE pbX = pbKeyBlob + sizeof(BLOBHEADER) + sizeof(RSAPUBKEY);

    //  Exponent
    *pExponent = pKeyInfo->rsa.pubexp;

    // Modulus
	*pModulusSize = cb;
	*ppbModulus = (LPBYTE) malloc(cb * sizeof(BYTE));
    memcpy(*ppbModulus, pbX, cb);
	// We need to go back to big endian
	memrev(*ppbModulus, cb);
}
