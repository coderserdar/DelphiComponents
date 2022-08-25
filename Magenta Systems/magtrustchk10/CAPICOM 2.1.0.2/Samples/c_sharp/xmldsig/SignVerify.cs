//******************************************************************************
//
// THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
// EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
//
// Copyright (C) 1999- 2002.  Microsoft Corporation.  All rights reserved.
//
//******************************************************************************
//
// SignVerify.cs
//
// This is a C# sample that illustrates how to use features introduced in
// CAPICOM's bridging support to interop between CAPI and System.Security.xml.
//
// Note: for simplicity, this code does not handle exceptions.
//
//******************************************************************************

using System;
using System.Drawing;
using System.ComponentModel;
using System.Windows.Forms;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography.Xml;
using System.Xml;
using System.Collections;
using Interop.CAPICOM;
using System.Runtime.InteropServices;


namespace Sample {
    public class MySignedXml : SignedXml {
        private IEnumerator m_keyInfoEnum = null;
	    public MySignedXml() : base() {}
	    public MySignedXml(XmlDocument document) : base(document) {}

        [DllImport("CertGetKey.dll", ExactSpelling=true)]
        private static extern void UnmanagedArrayFree (IntPtr ppbKeyBlob);

        [DllImport("CertGetKey.dll", ExactSpelling=true)]
        private static extern int PublicKeyBlobFromCertificateRawData (byte[] rgbRawData, int cbRawData, 
                                                        out IntPtr /* ref byte[] */ ppbKeyBlob, out uint pKeyBlobSize);

        [DllImport("CertGetKey.dll", ExactSpelling=true)]
        private static extern void ImportPublicKeyBlob (byte[] rgbPubKeyBlob, int cbPubKeyBlob, out uint pExponent, 
													   out IntPtr /* ref byte[] */ ppbModulus, out uint pModulusSize); 

        private static byte[] ConvertIntToByteArray(uint dwInput) {
            // output of this routine is always big endian
            byte[] rgbTemp = new byte[4]; 
            uint t1, t2;

            if (dwInput == 0) return new byte[1]; 
            t1 = dwInput; 
            int i = 0;
            while (t1 > 0) {
                t2 = t1 % 256;
                rgbTemp[i] = (byte) t2;
                t1 = (t1 - t2)/256;
                i++;
            }
            // Now, copy only the non-zero part of rgbTemp and reverse
            byte[] rgbOutput = new byte[i];
            // copy and reverse in one pass
            for (int j = 0; j < i; j++) {
                rgbOutput[j] = rgbTemp[i-j-1];
            }
            return(rgbOutput);
        }

	    protected override AsymmetricAlgorithm GetPublicKey() {
            RSAKeyValue tempRSA;
            DSAKeyValue tempDSA;
		    KeyInfoX509Data tempCert;

            if (m_keyInfoEnum == null)
                m_keyInfoEnum = KeyInfo.GetEnumerator();

            while (m_keyInfoEnum.MoveNext()) {
                tempRSA = m_keyInfoEnum.Current as RSAKeyValue;
                if (tempRSA != null) return(tempRSA.Key);

                tempDSA = m_keyInfoEnum.Current as DSAKeyValue;
                if (tempDSA != null) return(tempDSA.Key);

			    tempCert = m_keyInfoEnum.Current as KeyInfoX509Data;
			    if (tempCert != null) {
				    if (tempCert.Certificates != null) {
					    // The code here doesn't look in all the certificates and doesn't check if they belong 
                        // to the same certificate chain, or whether they are valid
                        if (tempCert.Certificates != null) {
                            foreach (X509Certificate x509certificate in tempCert.Certificates) {
                                IntPtr KeyBlob = IntPtr.Zero;
                                uint KeyBlobSize = 0;
                                int result = PublicKeyBlobFromCertificateRawData(x509certificate.GetRawCertData(), x509certificate.GetRawCertData().Length, out KeyBlob, out KeyBlobSize);
                                byte[] blob = new byte[KeyBlobSize];
                                Marshal.Copy(KeyBlob, blob, 0, (int)KeyBlobSize);
                                UnmanagedArrayFree(KeyBlob);
		                        RSACryptoServiceProvider rsa = new RSACryptoServiceProvider();
							    RSAParameters parameters = new RSAParameters();
							    IntPtr ExponentBlob = IntPtr.Zero, ModulusBlob = IntPtr.Zero;
							    uint PubExponent = 0, ModulusSize = 0;
                                ImportPublicKeyBlob(blob, blob.Length, out PubExponent, out ModulusBlob, out ModulusSize);
                                parameters.Exponent = ConvertIntToByteArray(PubExponent);
                                parameters.Modulus = new byte[ModulusSize];
                                Marshal.Copy(ModulusBlob, parameters.Modulus, 0, (int)ModulusSize);
                                UnmanagedArrayFree(ModulusBlob);
							    rsa.ImportParameters(parameters);                                
                                return rsa; 
					        }
                        }
                    }
                }
            }
            return(null);
	    }
    }

    internal class SignVerify {
        private static void GetSignatureKey (out RSACryptoServiceProvider rsa, out X509Certificate x509Cert) {
            rsa = null;
            x509Cert = null;
            try {
                Store st = new Store();
                st.Open(CAPICOM_STORE_LOCATION.CAPICOM_CURRENT_USER_STORE,
                        "MY", // Store Name
                        CAPICOM_STORE_OPEN_MODE.CAPICOM_STORE_OPEN_MAXIMUM_ALLOWED);
                Certificates selectedCerts = ((ICertificates2)st.Certificates).Select(
                                        Messages.DemoTitle,
                                        Messages.ChooseCertMessage,
                                        false);

                ICertificate2 selectedCert = ((ICertificate2)selectedCerts[1]);
                if (selectedCert.HasPrivateKey() == false) {
                    MessageBox.Show(Messages.ChooseCertNoPrivateKey, Messages.ExceptionTitle, MessageBoxButtons.OK, MessageBoxIcon.Stop);            
                    return;
                }
                
                // This only works with RSA keys
                if (selectedCert.PublicKey().Algorithm.FriendlyName != "RSA") {
                    MessageBox.Show(Messages.ChooseCertKeyAlgorithm, Messages.ExceptionTitle, MessageBoxButtons.OK, MessageBoxIcon.Stop);            
                    return;
                }

                // Construct the public key
                CspParameters csp = new CspParameters();
                csp.KeyContainerName = selectedCert.PrivateKey.ContainerName;
                csp.ProviderName = selectedCert.PrivateKey.ProviderName;
                csp.ProviderType = Convert.ToInt32(selectedCert.PrivateKey.ProviderType);
                switch (selectedCert.PrivateKey.KeySpec) {
                    case CAPICOM_KEY_SPEC.CAPICOM_KEY_SPEC_KEYEXCHANGE:
                        csp.KeyNumber = 1;
                        break;
                    case CAPICOM_KEY_SPEC.CAPICOM_KEY_SPEC_SIGNATURE:
                        csp.KeyNumber = 2;
                        break;
                }
                if (selectedCert.PrivateKey.IsMachineKeyset())
                    csp.Flags = CspProviderFlags.UseMachineKeyStore;
                rsa = new RSACryptoServiceProvider(csp);

                x509Cert = GetX509Certificate(selectedCert);
            } catch (Exception) {
                MessageBox.Show(Messages.ChooseCertUnableToConstructKey, Messages.ExceptionTitle, MessageBoxButtons.OK, MessageBoxIcon.Stop);
            }
        }

        private static X509Certificate GetX509Certificate (ICertificate2 cert) {
            String certAsString = cert.Export(CAPICOM_ENCODING_TYPE.CAPICOM_ENCODE_BASE64);
		    char[] charArray = {'\r', '\n', '\0'};
		    certAsString = certAsString.TrimEnd(charArray);

		    byte[] buffer = Convert.FromBase64String(certAsString);
		    return new X509Certificate(buffer);
        }

        public static string SignXML (string strXML) {
            string strResult = "";
            try {
                SignedXml signedXml = new SignedXml();

                // Get the signature key. For this demo, we look for keys associated with a certificate in the "MY" store            
                RSACryptoServiceProvider rsa = null;
                X509Certificate x509Cert = null;
                GetSignatureKey(out rsa, out x509Cert); 
                if (rsa == null) return "";
                signedXml.SigningKey = rsa;

                Reference reference = new Reference();
                reference.Uri = "#object-1";
		        reference.Type = "http://www.w3.org/2000/09/xmldsig#Object";

		        // Add an object
		        System.Security.Cryptography.Xml.DataObject obj = new System.Security.Cryptography.Xml.DataObject();
		        XmlDocument doc = new XmlDocument();
                doc.PreserveWhitespace = true;
                doc.LoadXml(strXML);
		        obj.Data = doc.ChildNodes;
		        obj.Id = "object-1";
		        signedXml.AddObject(obj);

                signedXml.AddReference(reference);
                KeyInfo keyInfo = new KeyInfo();

                keyInfo.AddClause(new RSAKeyValue(rsa));
                signedXml.KeyInfo = keyInfo;

                // compute the signature
                signedXml.ComputeSignature();
    
                strResult = signedXml.GetXml().OuterXml;
            } catch (Exception exc) {
                MessageBox.Show(exc.ToString(), Messages.ExceptionTitle, MessageBoxButtons.OK, MessageBoxIcon.Stop);
                return strResult;
            }

            return strResult;
        }
 
        public static string SignXMLX509Data (string strXML) {
            string strResult = "";
            try {
                SignedXml signedXml = new SignedXml();

                // Get the signature key. For this demo, we look for keys associated with a certificate in the "MY" store            
                RSACryptoServiceProvider rsa = null;
                X509Certificate x509Cert = null;
                GetSignatureKey(out rsa, out x509Cert); 
                if (rsa == null) return "";
                signedXml.SigningKey = rsa;

                Reference reference = new Reference();
                reference.Uri = "#object-1";
		        reference.Type = "http://www.w3.org/2000/09/xmldsig#Object";

		        // Add an object
		        System.Security.Cryptography.Xml.DataObject obj = new System.Security.Cryptography.Xml.DataObject();
		        XmlDocument doc = new XmlDocument();
                doc.PreserveWhitespace = true;
                doc.LoadXml(strXML);
		        obj.Data = doc.ChildNodes;
		        obj.Id = "object-1";
		        signedXml.AddObject(obj);

                signedXml.AddReference(reference);
                KeyInfo keyInfo = new KeyInfo();

                // Include the certificate raw data with the signed file
                keyInfo.AddClause(new KeyInfoX509Data(x509Cert));
                signedXml.KeyInfo = keyInfo;

                // compute the signature
                signedXml.ComputeSignature();
    
                strResult = signedXml.GetXml().OuterXml;
            } catch (Exception exc) {
                MessageBox.Show(exc.ToString(), Messages.ExceptionTitle, MessageBoxButtons.OK, MessageBoxIcon.Stop);
                return strResult;
            }

            return strResult;
        }
        
        public static bool VerifyXML (string strXML) {
            bool bResult = false;
        
            try {
                XmlDocument xmlDocument = new XmlDocument();
                xmlDocument.PreserveWhitespace = true;
                xmlDocument.LoadXml(strXML);

                MySignedXml signedXml = new MySignedXml(xmlDocument);

                XmlNodeList nodeList = xmlDocument.GetElementsByTagName("Signature");
                signedXml.LoadXml((XmlElement)nodeList[0]);

                AsymmetricAlgorithm key = null;
                if (signedXml.CheckSignatureReturningKey(out key)) {
                    bResult = true;
                } 
            } catch (Exception exc) {
                MessageBox.Show(exc.ToString(), Messages.ExceptionTitle, MessageBoxButtons.OK, MessageBoxIcon.Stop);
            }
        
            return bResult;
        }
    }
}
