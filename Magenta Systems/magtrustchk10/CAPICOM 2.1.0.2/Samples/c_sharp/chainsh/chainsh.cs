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
// chainsh.cs
//
// This is a C# sample that illustrates how to use features introduced in
// CAPICOM's bridging support to interop between CAPI and CAPICOM.
//
// Note: for simplicity, this script does not handle exceptions.
//
//******************************************************************************

using System;
using CAPICOM;
using System.Collections;
using System.Runtime.InteropServices;


namespace Sample
{
	public class Win32 
	{
			
		[DllImport("crypt32.dll",CharSet=CharSet.Unicode)]
		public static extern uint CertOpenSystemStore(
			uint hProv, 
			String szSubsystemProtocol 
			);

		[DllImport("crypt32.dll",CharSet=CharSet.Unicode)]
		public static extern uint  CertFindChainInStore(
			uint hCertStore,
			uint dwCertEncodingType,
			uint dwFindFlags,
			uint dwFindType,
			ref CERT_CHAIN_FIND_BY_ISSUER_PARA pvFindData,
			uint pPrevChainContext
			);

		public struct CERT_CHAIN_FIND_BY_ISSUER_PARA 
		{
			public int  cbSize;
			public String pszUsageIdentifier;  
			public uint dwKeySpec;
			public uint dwAcquirePrivateKeyFlags;
			public uint cIssuer;
			public uint rgIssuer;
			public uint pfnFindCallback;
			public uint pvFindArg;
			public uint pdwIssuerChainIndex;
			public uint pdwIssuerElementIndex;
		}
	}
	
	class Sample
	{
		[STAThread]
		public static void Main(string[] args)
		{	
			uint pStoreContext  = 0;
			uint pChainContext  = 0;
			String [] input     = new String[2];
			bool done           = false;
			
			displayHelp();

			while (!done)
			{
				Console.Write(">>");
				input = Console.ReadLine().Split(' ');
				
				//parse command line
				if (input[0].ToLower() == "q" || input[0].ToLower() == "quit")
				{
					done = true;
				}
				else if(input[0].ToLower() == "help" || input[0].ToLower() == "h")
				{
					displayHelp();
				}
				else if (input[0].ToLower() == "open"  || input[0].ToLower() == "o")
				{
					if (input.Length == 2)
					{
						pStoreContext = openStore(input[1]);
					}
					else
					{
						displayHelp();
					}
				}
				else if (input[0].ToLower() == "chains" || input[0].ToLower() == "c")
				{
					showChains(pStoreContext);	
				}
				else if (input[0].ToLower() == "display"  || input[0].ToLower() == "d")
				{
					if (input.Length == 2)
					{
						pChainContext = getChainFromStore(pStoreContext, System.Convert.ToInt32(input[1]));	
					
						if (pChainContext == 0)
						{
							Console.WriteLine("Couldn't display chain with indexnvalid chain index.");
						}
						else				
						{
							displayChain(pChainContext);
						}
					}
					else
					{
						displayHelp();
					}
					
				}
				else if (input[0].ToLower() == "verify"  || input[0].ToLower() == "v")
				{
					if (input.Length == 2)
					{
						pChainContext = getChainFromStore(pStoreContext, System.Convert.ToInt32(input[1]));	
						
						if (pChainContext == 0)
						{
							Console.WriteLine("Invalid chain index.");
						}
						else
						{				
							checkValidity(pChainContext);						
						}
					}
					else
					{
						displayHelp();				
					}		
				}
			}
		}
		
		public static uint openStore(String szStoreName)
		{
			
			uint  pStoreContext = 0;     // system store handle
			ICertStore iStore;			
			Store oStore;				 // CAPICOM object
			
			//verify that valid system store name was passed in
			if (szStoreName.ToLower() != "my" & 
				szStoreName.ToLower() != "ca" &
				szStoreName.ToLower() != "root")
			{	
				displayHelp();
				return 0;
			}
						
			try 
			{
				//open system store, use default CSP
				pStoreContext = Win32.CertOpenSystemStore(
					0,
					szStoreName);
				
				// API bridging
				oStore = new StoreClass();
				iStore = (ICertStore)oStore;
				iStore.StoreHandle = (int)pStoreContext;

				Console.WriteLine ("The store has " + oStore.Certificates.Count + " certificates");
				
				return pStoreContext;
			}
			catch (Exception e)
			{
				Console.WriteLine("Error in " + e.Source + ": " + e.Message);
				return 0;
			}
		}
		public static void showChains(uint pStoreContext)
		{
			const uint X509_ASN_ENCODING						= 0x00000001;
			const uint PKCS_7_ASN_ENCODING						= 0x00010000;
			const uint CERT_CHAIN_FIND_BY_ISSUER				= 1;
			const uint CERT_CHAIN_FIND_BY_ISSUER_NO_KEY_FLAG	=  0x4000;

			uint  pChainContext = 0;
					
			if (pStoreContext == 0)
			{	
				Console.WriteLine("No store is currently open.");
				return;
			}
			
			Win32.CERT_CHAIN_FIND_BY_ISSUER_PARA pvFindPara =
								new Win32.CERT_CHAIN_FIND_BY_ISSUER_PARA();
			
			//get size of CERT_CHAIN_FIND_BY_ISSUER_PARA struct
			pvFindPara.cbSize = Marshal.SizeOf(pvFindPara);
			try 
			{
				//get first chain
				pChainContext = Win32.CertFindChainInStore(
					pStoreContext,
					X509_ASN_ENCODING | PKCS_7_ASN_ENCODING,
					CERT_CHAIN_FIND_BY_ISSUER_NO_KEY_FLAG,
					CERT_CHAIN_FIND_BY_ISSUER,
					ref pvFindPara,
					0
					);

				//get the rest 		
				while (pChainContext != 0)
				{
					displayChain(pChainContext);  // display chains
					pChainContext = Win32.CertFindChainInStore(
						pStoreContext,
						X509_ASN_ENCODING | PKCS_7_ASN_ENCODING,
						CERT_CHAIN_FIND_BY_ISSUER_NO_KEY_FLAG,
						CERT_CHAIN_FIND_BY_ISSUER,
						ref pvFindPara,
						pChainContext);
					
				}
				return;
			}
			catch (Exception e)
			{
				Console.WriteLine("Error in " + e.Source + ": " + e.Message);
				return;
			}
		}
		public static uint getChainFromStore(uint pStoreContext, int index)
		{
			const uint X509_ASN_ENCODING						= 0x00000001;
			const uint PKCS_7_ASN_ENCODING						= 0x00010000;
			const uint CERT_CHAIN_FIND_BY_ISSUER				= 1;
			const uint CERT_CHAIN_FIND_BY_ISSUER_NO_KEY_FLAG	=  0x4000;

			uint  pChainContext = 0;
					
			if (pStoreContext == 0)
			{	
				Console.WriteLine("No store is currently open.");
				return 0;
			}
			
			Win32.CERT_CHAIN_FIND_BY_ISSUER_PARA pvFindPara =
				new Win32.CERT_CHAIN_FIND_BY_ISSUER_PARA();
			
			//get size of CERT_CHAIN_FIND_BY_ISSUER_PARA struct
			pvFindPara.cbSize = Marshal.SizeOf(pvFindPara);
			try 
			{
				//get the chain
				while (index > 0)
				{
					pChainContext = Win32.CertFindChainInStore(
						pStoreContext,
						X509_ASN_ENCODING | PKCS_7_ASN_ENCODING,
						CERT_CHAIN_FIND_BY_ISSUER_NO_KEY_FLAG,
						CERT_CHAIN_FIND_BY_ISSUER,
						ref pvFindPara,
						pChainContext);

					if (pChainContext == 0)
					{
						Console.WriteLine(" Couldn't find chain with index " + index);
						return 0;
					}					
					index--;					
				}
				return pChainContext;
			}
			catch (Exception e)
			{
				Console.WriteLine("Error in " + e.Source + ": " + e.Message);
				return 0;
			}
		}
		public static void checkValidity(uint pChainContext)
		{	
			IChainContext iChain;
			CAPICOM.Chain oChain;	
			int status;
				
			//API bridging
			oChain = new ChainClass();
			iChain = (IChainContext)oChain;
			iChain.ChainContext = (int)pChainContext;

			//get status of the entire chain
			status = oChain.get_Status(0);
			
			if (status == 0)
			{
				Console.Write("The chain appears trustworthy.");
				Console.WriteLine();
				return;
			}
			
			Console.WriteLine("The chain is invalid.");

			// display specific chain status
			// note: for simplicity, only SOME possible status 
			// values are presented below
			if ((status & Constants.CAPICOM_TRUST_IS_NOT_TIME_VALID) == Constants.CAPICOM_TRUST_IS_NOT_TIME_VALID)
				Console.WriteLine("Chain status: not time valid");
				
			if ((status & Constants.CAPICOM_TRUST_IS_NOT_TIME_NESTED) == Constants.CAPICOM_TRUST_IS_NOT_TIME_NESTED)
				Console.WriteLine("Chain status: time nested.");
				
			if ((status & Constants.CAPICOM_TRUST_IS_REVOKED) == Constants.CAPICOM_TRUST_IS_REVOKED)
				Console.WriteLine("Chain status: is revoked");
				
			if ((status & Constants.CAPICOM_TRUST_IS_NOT_SIGNATURE_VALID) == Constants.CAPICOM_TRUST_IS_REVOKED)
				Console.WriteLine("Chain status:  invalid signature");
				
			if ((status & Constants.CAPICOM_TRUST_IS_UNTRUSTED_ROOT) == Constants.CAPICOM_TRUST_IS_UNTRUSTED_ROOT)
					Console.WriteLine("Chain status: untrusted root");
				
			if ((status & Constants.CAPICOM_TRUST_REVOCATION_STATUS_UNKNOWN) == Constants.CAPICOM_TRUST_REVOCATION_STATUS_UNKNOWN)
					Console.WriteLine("Chain status: revocation status unknown");
				
			if ((status & Constants.CAPICOM_TRUST_INVALID_EXTENSION) == Constants.CAPICOM_TRUST_INVALID_EXTENSION)
					Console.WriteLine("Chain status: invalid extension");
				
			if ((status & Constants.CAPICOM_TRUST_INVALID_POLICY_CONSTRAINTS) == Constants.CAPICOM_TRUST_INVALID_POLICY_CONSTRAINTS)
					Console.WriteLine("Chain status: invalid policy constraints");
				
			if ((status & Constants.CAPICOM_TRUST_INVALID_BASIC_CONSTRAINTS) == Constants.CAPICOM_TRUST_INVALID_BASIC_CONSTRAINTS)
					Console.WriteLine("Chain status: invalid basic constraints");
				
			if ((status & Constants.CAPICOM_TRUST_INVALID_NAME_CONSTRAINTS) == Constants.CAPICOM_TRUST_INVALID_NAME_CONSTRAINTS)
					Console.WriteLine("Chain status:  invalid name constraints");
				
			if ((status & Constants.CAPICOM_TRUST_IS_OFFLINE_REVOCATION) == Constants.CAPICOM_TRUST_IS_OFFLINE_REVOCATION)
					Console.WriteLine("Chain status: offline revocation");
			
			if ((status & Constants.CAPICOM_TRUST_IS_PARTIAL_CHAIN) == Constants.CAPICOM_TRUST_IS_PARTIAL_CHAIN)
					Console.WriteLine("Chain status: partial chain");
				
			Console.WriteLine();
						
		}
		public static void displayChain(uint pChainContext)
		{
			Certificate cert;
			IChainContext iChain;
			CAPICOM.Chain oChain;
				
			IEnumerator eEnum;
			int i = 0;
					
			//API bridging
			oChain = new ChainClass();
			iChain = (IChainContext)oChain;
			iChain.ChainContext = (int)pChainContext;

			//get the number of certificates in chain
			eEnum = oChain.Certificates.GetEnumerator();
			
			Console.WriteLine();
			Console.WriteLine("Chain: ");

			//display properties of each certificate in the chain
			while (eEnum.MoveNext() == true)
			{
				i++;
				cert = (Certificate) eEnum.Current;
				Console.WriteLine("Certificate: " + i);
				Console.WriteLine("Subject: " + cert.SubjectName);
				Console.WriteLine("Issuer: " + cert.IssuerName);
				Console.WriteLine("Hash: " + cert.Thumbprint);
				Console.WriteLine("Valid From: " + cert.ValidFromDate);
				Console.WriteLine("Valid To: " + cert.ValidToDate);
				Console.WriteLine();
			}
		}	
		
		public static void displayHelp()
		{
			const int HELP_PAD = 30;

			Console.WriteLine("Usage:");
			Console.WriteLine();
			Console.Write("[h]elp".PadRight(HELP_PAD, ' '));
			Console.WriteLine("This screen");

			Console.WriteLine();
			Console.Write("[o]pen <store name>".PadRight(HELP_PAD, ' '));
			Console.WriteLine("Open 'my', 'ca', or 'root' system store");
								
			Console.WriteLine();
			Console.Write("[c]hains".PadRight(HELP_PAD, ' '));
			Console.WriteLine("List all chains found in store");
				
			Console.WriteLine();
			Console.Write("[d]isplay <idx>".PadRight(HELP_PAD, ' '));
			Console.WriteLine("<idx> is the number of the chain to display");

			Console.WriteLine();
			Console.Write("[v]erify <idx>".PadRight(HELP_PAD, ' '));
			Console.WriteLine("<idx> is the number of the chain to verify");

			Console.WriteLine();
			Console.Write("[q]uit".PadRight(HELP_PAD, ' '));
			Console.WriteLine("Exit this program.");

			Console.WriteLine();
		}

	}
}
