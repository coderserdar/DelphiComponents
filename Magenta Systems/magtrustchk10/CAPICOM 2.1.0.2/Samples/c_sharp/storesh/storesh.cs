//******************************************************************************
//
// THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND, 
// EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED 
// WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
//
// Copyright (C) 1999 - 2002.  Microsoft Corporation.  All rights reserved.
//
//******************************************************************************
//
// storesh.cs
//
// This is a sample script to illustrate how to use CAPICOM and C#. It creates a
// pseudo shell to navigate certificate stores.
//
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
// To run this sample you need to have Microsoft Visual C# .NET
// To start: open the storesh.sln file in Visual Studio
//
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//
//  Try these commands:
//
//	help
//	dir
//	cd ..
//	cd ..\addressbook
//	dir -l
//	dir -subject John
//	cd \LocalMachine
//	cd \lm
//	display 3
//	view 2
//	import c:\home\temp\pfx1.pfx
//	export 7 c:\home\temp\itm.cer
//  pfxexport 11 c:\home\temp\foo.pfx secretpassword
//	rem 2
//  dir -issuer "Thawte"
//  dir -eku "Code Signing"
//  dir -eku 1.3.6.1.5.5.7.3.4
//
// Note: For simplicity, this script does not handle exceptions.
//
//******************************************************************************

using System;
using System.Collections;
using System.Runtime.InteropServices;
using CAPICOM;
using System.Text;

namespace storesh
{

	
	public class Win32 
	{

		[DllImport("crypt32.dll",CharSet=CharSet.Unicode )]
		public static extern uint CertEnumSystemStore(
			uint dwFlags, 
			uint pvSystemStoreLocationPara, 
			String pvArg, 
			CertEnumSystemStoreCallback pfnEnum
			);

	}

	public delegate bool CertEnumSystemStoreCallback(
		[In, MarshalAs( UnmanagedType.LPWStr) ] String pvSystemStore, 
		uint dwFlags, 
		ref CERT_SYSTEM_STORE_INFO pStoreInfo, 
		uint pvReserved, 
		[In, MarshalAs( UnmanagedType.LPWStr) ] String pvArg
	);

	[StructLayout(LayoutKind.Sequential)]
	public struct CERT_SYSTEM_STORE_INFO
	{
		uint cbSize;
	}



	class Storesh 
	{
		const uint CERT_SYSTEM_STORE_CURRENT_USER  = 0x10000;
		const uint CERT_SYSTEM_STORE_LOCAL_MACHINE  = 0x20000;
		const int VALIDITY_LENGTH = 23;
		const int CERTNAME_LENGTH = 20;
		const int THUMBPRINT_LENGTH = 42;
		const int FRIENDLYNAME_LENGTH = 20;
		const int CAPICOM_ENCODED_DATA_FORMAT_MULTI_LINES =1 ;
		const int HELP_PAD = 30;
		const String CurrentUser = "CurrentUser";
		const String LocalMachine = "LocalMachine";

		static private String _currStoreName = "My";
		static private String _currStoreLocation = CurrentUser;
		static IEnumerator _currFilter;
		static StoreClass _oCurrStore;

		enum Listing {Brief, Long};
		
		
		/// The main entry point for the application.
		[STAThread]
		static void Main(string[] args)
		{
			
			//We take one argument, a starting store name
			if (args.Length > 0)
			{
				_currStoreName = args[0];
			}
			_oCurrStore = new StoreClass();
			_oCurrStore.Open(
				CAPICOM_STORE_LOCATION.CAPICOM_CURRENT_USER_STORE,
				_currStoreName,
				CAPICOM_STORE_OPEN_MODE.CAPICOM_STORE_OPEN_EXISTING_ONLY | 
				CAPICOM_STORE_OPEN_MODE.CAPICOM_STORE_OPEN_MAXIMUM_ALLOWED);
			waitForInput();
		}

		static void waitForInput()
		{
			bool done = false;
			String input;
			while (!done) 
			{
				Console.Write(_currStoreLocation + "\\" + _currStoreName + ">");
				input = Console.ReadLine();
				done = parseCommandLine(input);
			}
		}

		static bool parseCommandLine ( String input)
		{
			
			ArrayList alArgs = ParseArgs(input);
			Certificate oCert = null;
			
			switch ((String)alArgs[0])
			{
				case "cd":
				{
					//This is the 'change directory' command
					String storename;
					if (alArgs.Count > 1)
					{
						storename = (String)alArgs[1];
					}
					else 
					{
						storename = _currStoreName; //reset store name
					}

					if (storename.Equals("..") && _currStoreName.Length > 0 )
					{
						_oCurrStore = new StoreClass();
						storename = null;
						_currFilter = null;
						_currStoreName = "";
					} 
					else if (storename.StartsWith("..") && _currStoreName.Length > 0 )
					{
						_oCurrStore = new StoreClass();
						_currFilter = null;
						storename = storename.Substring(3,storename.Length - 3);
						_currStoreName = "";
					} 
					else if (storename.Equals(".."))
					{
						storename = null;
					} 
					else if (storename.Equals("\\" + LocalMachine)  || storename.Equals("\\lm" ))
					{
						_oCurrStore = new StoreClass();
						_currStoreName = "";
						storename = null;
						_currStoreLocation = LocalMachine;
						_currFilter = null;
					}
					else if (storename.Equals("\\" + CurrentUser)  || storename.Equals("\\cu" ))
					{
						_oCurrStore = new StoreClass();
						_currStoreName = "";
						storename = null;
						_currStoreLocation = CurrentUser;
						_currFilter = null;
					}
					if (storename != null && _currStoreName.Equals("")) 
					{

						try 
						{
							CAPICOM_STORE_LOCATION OpenMode = CAPICOM_STORE_LOCATION.CAPICOM_CURRENT_USER_STORE;
							if (_currStoreLocation.Equals(LocalMachine))
							{
								OpenMode = CAPICOM_STORE_LOCATION.CAPICOM_LOCAL_MACHINE_STORE;
							}

							//Open the store MAX_ALLOWED in case the user wants to import/rem/export
							//They may not have permission to modify HKLM stores
							_oCurrStore.Open(	OpenMode,
								storename,
								CAPICOM_STORE_OPEN_MODE.CAPICOM_STORE_OPEN_EXISTING_ONLY | 
								CAPICOM_STORE_OPEN_MODE.CAPICOM_STORE_OPEN_MAXIMUM_ALLOWED);
							_currStoreName = storename;

						} 
						catch (Exception e)
						{
							Console.WriteLine (e.Message);
						}
					}
					return false;
				}
				case "q":
				case "quit":
				{
					return true;
				}
				case "h":
				case "help":
				{
					DisplayHelp();
				
					return false;
				} 
				case "v":
				case "view":
					try 
					{
						oCert = GetCertByIndex(Convert.ToInt32(alArgs[1]));
						if (oCert != null ) 
						{
							DisplayCertificate(oCert, "");
						}
						else
						{
							Console.WriteLine ("No certificate with that index (" + alArgs[1] + ") could be found.");
						}
					} 
					catch (Exception e)
					{
						Console.WriteLine ("An error was encountered processing the " + alArgs[0] + " command: " + e.Message);
					}
					break;
				case "y":
				case "display":
					try 
					{
						oCert = GetCertByIndex(Convert.ToInt32(alArgs[1]));
						if (oCert != null ) 
						{
							oCert.Display();
						}
						else
						{
							Console.WriteLine ("No certificate with that index (" + alArgs[1] + ") could be found.");
						}
					} 
					catch (Exception e)
					{
						Console.WriteLine ("An error was encountered processing the " + alArgs[0] + " command: " + e.Message);
					}
					break;
				case "rem":
					try 
					{
						oCert = GetCertByIndex(Convert.ToInt32(alArgs[1]));
						if (oCert != null ) 
						{
							String answer = "n";
							Console.WriteLine(	ToColumn (oCert.GetInfo(CAPICOM_CERT_INFO_TYPE.CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME), CERTNAME_LENGTH) + 
								" " + 
								ToColumn (oCert.Thumbprint, THUMBPRINT_LENGTH ));
							Console.WriteLine ("Issuer: " + oCert.GetInfo(CAPICOM_CERT_INFO_TYPE.CAPICOM_CERT_INFO_ISSUER_SIMPLE_NAME));
							Console.WriteLine ("Validity Period: " + oCert.ValidFromDate + " - " + oCert.ValidToDate);
							DisplayEKU(oCert.ExtendedKeyUsage().EKUs);
							Console.WriteLine();
							Console.Write("Are you sure you want to remove this certificate (y/n)? ");
							answer = Console.ReadLine();
							if (!answer.Equals("y"))
							{
								break;
							}

							if (oCert.HasPrivateKey() && !oCert.PrivateKey.IsHardwareDevice() )
							{
								oCert.PrivateKey.Delete();
								Console.WriteLine ("The private key was deleted.");
							}
							try 
							{
								_oCurrStore.Remove(oCert);
								Console.WriteLine("The certificate was removed.");
							} 
							catch 
							{
								Console.WriteLine("The certificate could not be removed.");
							}
						}
						else
						{
							Console.WriteLine ("No certificate with that index (" + alArgs[1] + ") could be found.");
						}
					} 
					catch (Exception e)
					{
						Console.WriteLine ("An error was encountered processing the " + alArgs[0] + " command: " + e.Message);
					}
					break;
				case "e":
				case "export":
					try 
					{
						oCert = GetCertByIndex(Convert.ToInt32(alArgs[1]));
						if (oCert != null ) 
						{
							String filename =  (String)alArgs[2];
							if (filename != null)
							{
								oCert.Save(filename,
									"",
									CAPICOM_CERTIFICATE_SAVE_AS_TYPE.CAPICOM_CERTIFICATE_SAVE_AS_CER,
									CAPICOM_CERTIFICATE_INCLUDE_OPTION.CAPICOM_CERTIFICATE_INCLUDE_END_ENTITY_ONLY);

							} 
							else
							{
								Console.WriteLine("No filename specified.");
							}
						}
						else
						{
							Console.WriteLine ("No certificate with that index (" + alArgs[1] + ") could be found.");
						}
					} 
					catch (Exception e)
					{
						Console.WriteLine ("An error was encountered processing the " + alArgs[0] + " command: " + e.Message);
					}
					break;
				case "i":
				case "import":
					try 
					{
						String filename = null;
						String password = null;
						CAPICOM_KEY_STORAGE_FLAG iFlag = CAPICOM_KEY_STORAGE_FLAG.CAPICOM_KEY_STORAGE_DEFAULT;
						
						if (alArgs.Count >= 2)
						{
							filename = (String)alArgs[1];
						}
						if (alArgs.Count >= 3)
						{
							password = (String)alArgs[2];
						}
						if (alArgs.Count >= 4)
						{
							String flag = null;
							if (alArgs.Count == 4)
							{
								password = null;
								flag = (String)alArgs[3];
							}
							else if (alArgs.Count == 5)
							{
								flag = (String)alArgs[4];
							}
							if (flag.Equals ("e"))
							{
								iFlag = CAPICOM_KEY_STORAGE_FLAG.CAPICOM_KEY_STORAGE_EXPORTABLE;
							} 
							else if (flag.Equals ("p"))
							{
								iFlag = CAPICOM_KEY_STORAGE_FLAG.CAPICOM_KEY_STORAGE_USER_PROTECTED;
							}
							else if (flag.Equals ("ep") || flag.Equals ("pe"))
							{
								iFlag = CAPICOM_KEY_STORAGE_FLAG.CAPICOM_KEY_STORAGE_USER_PROTECTED | CAPICOM_KEY_STORAGE_FLAG.CAPICOM_KEY_STORAGE_EXPORTABLE;
							}
						}
						_oCurrStore.Load(filename, password, iFlag);
					}
					catch (Exception e)
					{
						Console.WriteLine ("An error was encountered processing the " + alArgs[0] + " command: " + e.Message);
					}
					break;
				case "p":
				case "pfxexport":
					try 
					{
						oCert = GetCertByIndex(Convert.ToInt32(alArgs[1]));
						if (oCert != null ) 
						{
							String filename =  (String)alArgs[2];
							String password =  (String)alArgs[3];
										
							if (filename != null)
							{
								oCert.Save(filename,
									password,
									CAPICOM_CERTIFICATE_SAVE_AS_TYPE.CAPICOM_CERTIFICATE_SAVE_AS_PFX,
									CAPICOM_CERTIFICATE_INCLUDE_OPTION.CAPICOM_CERTIFICATE_INCLUDE_WHOLE_CHAIN);
							} 
							else
							{
								Console.WriteLine("No filename specified.");
							}
						}
						else
						{
							Console.WriteLine ("No certificate with that index (" + alArgs[1] + ") could be found.");
						}
					} 
					catch (Exception e)
					{
						Console.WriteLine ("An error was encountered processing the " + alArgs[0] + " command: " + e.Message);
					}
					break;
				case "c":
                                case "create":
                                case "del":
                                        try
                                        {
                                                // Opening it is the same whether we're creating or deleting
                                                if (alArgs.Count == 1) {
                                                        Console.WriteLine ("Please enter a name for the store");
                                                        break;
                                                }
                                                String name = (String) alArgs[1];
                                                for (int i = 2; i < alArgs.Count; i++) {
                                                        name += " " + alArgs[i];
                                                }
                                                Store newStore = new Store();
                                                CAPICOM_STORE_LOCATION storeLoc = CAPICOM_STORE_LOCATION.CAPICOM_CURRENT_USER_STORE;
                                                if (_currStoreLocation.Equals(LocalMachine)) {
                                                        storeLoc = CAPICOM_STORE_LOCATION.CAPICOM_LOCAL_MACHINE_STORE;
                                                }
                                                newStore.Open(storeLoc,
                                                              name,
                                                              CAPICOM_STORE_OPEN_MODE.CAPICOM_STORE_OPEN_READ_WRITE);

                                                // Now delete if that is what was chosen
                                                if (alArgs[0].Equals("del")) {
                                                        if (_currStoreName.Equals(name)) {
                                                                Console.WriteLine("You cannot delete the store you are currently viewing");
                                                                Console.WriteLine("Please use the <cd> command to change stores first");
                                                                break;
                                                        }
													    newStore.Delete();
                                                }
                                        }
                                        catch (Exception e)
                                        {
                                                Console.WriteLine ("An error was encountered processing the " + alArgs[0] + " command: " + e.Message);
                                        }
                                        break;
				case "d":
				case "dir":
				{
					Certificates oCerts;
					Listing listing = Listing.Brief;

					if ((_currStoreLocation.Equals(CurrentUser) || _currStoreLocation.Equals(LocalMachine))
						&& _currStoreName.Length == 0)
					{
						uint retval = 0;
						uint dwFlags = CERT_SYSTEM_STORE_CURRENT_USER;

						CertEnumSystemStoreCallback StoreCallback = new CertEnumSystemStoreCallback(Storesh.CertEnumSystemStoreCallback);
						if (_currStoreLocation.Equals(LocalMachine)) 
						{
							dwFlags = CERT_SYSTEM_STORE_LOCAL_MACHINE;
						}
						retval= Win32.CertEnumSystemStore(
							dwFlags, 
							0, 
							_currStoreName, 
							StoreCallback 
							);
					}
					else if (alArgs.Count >= 1)
					{
						int i =0 ;
						try 
						{
							_currFilter = _oCurrStore.Certificates.GetEnumerator();
							for (i = 1; i < alArgs.Count; i++)
							{
								String param = ((String)alArgs[i]).ToLower();
								if (param.Equals("/l") || param.Equals("-l"))
								{
									listing = Listing.Long;
								} 
								else if (param.Equals("/subject") || param.Equals("-subject"))
								{
									Object filter = (System.String)alArgs[++i]; 
									oCerts = (Certificates)_oCurrStore.Certificates;
									oCerts = (Certificates)oCerts.Find(CAPICOM_CERTIFICATE_FIND_TYPE.CAPICOM_CERTIFICATE_FIND_SUBJECT_NAME,
										filter,
										false);
									_currFilter = oCerts.GetEnumerator();

								}
								else if (param.Equals("/eku") || param.Equals("-eku"))
								{
									Object filter = (System.String)alArgs[++i]; 
									oCerts = (Certificates)_oCurrStore.Certificates;
									oCerts = (Certificates)oCerts.Find(CAPICOM_CERTIFICATE_FIND_TYPE.CAPICOM_CERTIFICATE_FIND_APPLICATION_POLICY,
										filter,
										false);
									_currFilter = oCerts.GetEnumerator();

								}
								else if (param.Equals("/issuer") || param.Equals("-issuer"))
								{
									Object filter = (System.String)alArgs[++i]; 
									oCerts = (Certificates)_oCurrStore.Certificates;
									oCerts = (Certificates)oCerts.Find(CAPICOM_CERTIFICATE_FIND_TYPE.CAPICOM_CERTIFICATE_FIND_ISSUER_NAME,
										filter,
										false);
									_currFilter = oCerts.GetEnumerator();

								}
								else if (param.Equals("/sha1") || param.Equals("-sha1"))
								{
									String filter = (String)alArgs[++i]; 
									oCerts = (Certificates)_oCurrStore.Certificates;
									oCerts = (Certificates)oCerts.Find(CAPICOM_CERTIFICATE_FIND_TYPE.CAPICOM_CERTIFICATE_FIND_SHA1_HASH,
										filter,
										false);
									_currFilter = oCerts.GetEnumerator();
								}
							}
							i = 1;
							while (_currFilter.MoveNext() == true)
							{
								oCert = (Certificate) _currFilter.Current;
								Console.Write ( (i++ + ". ").PadRight(4,' '));
								switch (listing)
								{
									case Listing.Brief:
										Console.WriteLine(	ToColumn (oCert.GetInfo(CAPICOM_CERT_INFO_TYPE.CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME), CERTNAME_LENGTH) +
											" " + 
											ToColumn (oCert.ValidToDate.ToString(), VALIDITY_LENGTH) + 
											ToColumn (oCert.GetInfo(CAPICOM_CERT_INFO_TYPE.CAPICOM_CERT_INFO_ISSUER_SIMPLE_NAME), CERTNAME_LENGTH) );
										break;
									case Listing.Long:
									{
										Console.WriteLine(	ToColumn (oCert.GetInfo(CAPICOM_CERT_INFO_TYPE.CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME), CERTNAME_LENGTH) + 
											" " + 
											ToColumn (oCert.Thumbprint, THUMBPRINT_LENGTH ));
										Console.WriteLine ("Issuer: " + oCert.GetInfo(CAPICOM_CERT_INFO_TYPE.CAPICOM_CERT_INFO_ISSUER_SIMPLE_NAME));
										Console.WriteLine ("Validity Period: " + oCert.ValidFromDate + " - " + oCert.ValidToDate);
										DisplayEKU(oCert.ExtendedKeyUsage().EKUs);
										Console.WriteLine();
										break;
									}
								}
							}
						}
						catch (Exception e)
						{
							Console.WriteLine("An error was encountered: " + e.Message);
						}
					}
					break;
				}
				default:
					DisplayHelp();
					break;

			}

			return false;
		}

		public static bool CertEnumSystemStoreCallback(
			String pvSystemStore, 
			uint dwFlags, 
			ref CERT_SYSTEM_STORE_INFO pStoreInfo, 
			uint pvReserved, 
			String pvArg
			)
		{
			Console.WriteLine("[ " +pvSystemStore + " ]");
			return true;
		}

		public static Certificate GetCertByIndex( int idx )
		{
			Certificate oCert;
			int i = 0;

			if (_oCurrStore != null) 
			{
				if (_currFilter == null) 
				{
					_currFilter = _oCurrStore.Certificates.GetEnumerator();
				}
				else 
				{
					_currFilter.Reset();
				}

				while (_currFilter.MoveNext() == true)
				{
					oCert  = (Certificate)_currFilter.Current;
					i++;
					if (i == idx)
					{
						return oCert;
					}
				}
			}
			return null;
		}

		private static ArrayList ParseArgs(String input)
		{
			char[] delims = new char[] {' '};
			ArrayList argsarray = new ArrayList();
			String multiString = null;

			String[] argSplit = input.Split(delims);
			for (int i = 0; i < argSplit.Length; i++)
			{
				String curr = argSplit[i];
				if (curr.StartsWith("\"") && curr.EndsWith("\""))
				{
					argsarray.Add(curr.Substring(1,curr.Length-2));
				}
				else if (curr.StartsWith("\""))
				{
					multiString = curr;
				}
				else if (argSplit[i].EndsWith("\""))
				{
					multiString = multiString + " " + curr;
					argsarray.Add(multiString.Substring(1,multiString.Length-2));
					multiString = null;
				}
				else
				{
					if (multiString != null)
					{
						multiString = multiString + " " + curr;
					}
					else 
					{
						argsarray.Add(curr);
					}
				}
			}
			argSplit = null;
			return argsarray;
		}
		
		public static void DisplayHelp()
		{
			Console.WriteLine();
			Console.Write("[h]elp".PadRight(HELP_PAD, ' '));
			Console.WriteLine("This screen");

			Console.WriteLine();
			Console.Write("cd <location>".PadRight(HELP_PAD, ' '));
			Console.WriteLine("<location> is a store name or ..");
			Console.WriteLine("".PadRight(HELP_PAD, ' ')+ "Can also be \\LocalMachine (\\lm) or \\CurrentUser (\\cu)");
				
			Console.WriteLine();
			Console.Write("[d]ir <options>".PadRight(HELP_PAD, ' '));
			Console.WriteLine("List the contents of a certificate store");
			Console.WriteLine("".PadRight(HELP_PAD, ' ')+ "-l	Long listing");
			Console.WriteLine("".PadRight(HELP_PAD, ' ')+ "-subject <name>	Match subject name.");
			Console.WriteLine("".PadRight(HELP_PAD, ' ')+ "-issuer <name>	Match issuer name.");
			Console.WriteLine("".PadRight(HELP_PAD, ' ')+ "-eku <EKU>	Must have specified EKU.");
			Console.WriteLine("".PadRight(HELP_PAD, ' ')+ "-sha1 <sha1>	Match thumbprint.");

			Console.WriteLine();
			Console.Write("[v]iew <idx>".PadRight(HELP_PAD, ' '));
			Console.WriteLine("<idx> is the number of the certificate to view");

			Console.WriteLine();
			Console.Write("rem <idx>".PadRight(HELP_PAD, ' '));
			Console.WriteLine("<idx> is the number of the certificate to remove");

			Console.WriteLine();
			Console.Write("displa[y] <idx>".PadRight(HELP_PAD, ' '));
			Console.WriteLine("<idx> is the number of the certificate to display");

                        Console.WriteLine();
                        Console.Write("[c]reate <store>".PadRight(HELP_PAD, ' '));
                        Console.WriteLine("<store> is the name of the store to create");

                        Console.WriteLine();
                        Console.Write("del <store>".PadRight(HELP_PAD, ' '));
                        Console.WriteLine("<store> is the name of the store to delete");
                        Console.WriteLine("".PadRight(HELP_PAD, ' ')+ "This cannot be a system store");

			Console.WriteLine();
			Console.Write("[e]xport <idx> <file>".PadRight(HELP_PAD, ' '));
			Console.WriteLine("<idx> is the number of the certificate to display");
			Console.WriteLine("".PadRight(HELP_PAD, ' ')+ "<file> is the destination filename");

			Console.WriteLine();
			Console.Write("[p]fxexport <idx> <file> <pw>".PadRight(HELP_PAD, ' '));
			Console.WriteLine("<idx> is the number of the certificate to display");
			Console.WriteLine("".PadRight(HELP_PAD, ' ')+ "<file> is the destination filename");
			Console.WriteLine("".PadRight(HELP_PAD, ' ')+ "<pw> is the password");

			Console.WriteLine();
			Console.Write("[i]mport <file> <pw> [-key e|p]".PadRight(HELP_PAD, ' '));
			Console.WriteLine("<file> is the input filename");
			Console.WriteLine("".PadRight(HELP_PAD, ' ')+ "<pw> is the filename password");
			Console.WriteLine("".PadRight(HELP_PAD, ' ')+ "-key: e is exportable");
			Console.WriteLine("".PadRight(HELP_PAD, ' ')+ "-key: p is user protected. Both may be specified");

		}
		
		public static String ToColumn (String s, int i)
		{
			String retval;
			if (s == null || i > s.Length )
			{
				retval = s.PadRight(i);
			}
			else
			{
				retval = s.Substring(0,i).PadRight(i,' ');
			}
			return retval;
		}

		public static void DisplayEKU (EKUs EKUs)
		{
			if (EKUs.Count > 0)
			{
				OID OID;
				OID = new OIDClass();
				foreach (EKU EKU in EKUs)
				{
					OID.Value = EKU.OID;
					Console.WriteLine( "  " + OID.FriendlyName + " (" + OID.Value + ")" );
				}
				OID = null;
			}
		}

		public static void DisplayCertificate ( Certificate Certificate , String Title)
		{
			String[] KeySpecStrings = {"Unknown", "Exchange","Signature" };
			String[] ProviderTypes = {	"Unknown", 
										 "PROV_RSA_FULL",
										 "PROV_RSA_SIG",
										 "PROV_DSS",
										 "PROV_FORTEZZA",
										 "PROV_MS_EXCHANGE",
										 "PROV_SSL",
										 "PROV_STT_MER",
										 "PROV_STT_ACQ",
										 "PROV_STT_BRND",
										 "PROV_STT_ROOT",
										 "PROV_STT_ISS",
										 "PROV_RSA_SCHANNEL",
										 "PROV_DSS_DH",
										 "PROV_EC_ECDSA_SIG",
										 "PROV_EC_ECNRA_SIG",
										 "PROV_EC_ECDSA_FULL",
										 "PROV_EC_ECNRA_FULL",
										 "PROV_DH_SCHANNEL",
										 "PROV_SPYRUS_LYNKS",
										 "PROV_RNG",
										 "PROV_INTEL_SEC",
										 "PROV_REPLACE_OWF",
										 "PROV_RSA_AES" };
   
			//int iIndex = 0;
			
			Console.WriteLine( Title );
			Console.WriteLine();
			Console.WriteLine( "Subject Name:");
			Console.WriteLine( "  Simple name = " + Certificate.SubjectName);
			Console.WriteLine( "  Email name  = " + Certificate.GetInfo(CAPICOM_CERT_INFO_TYPE.CAPICOM_CERT_INFO_SUBJECT_EMAIL_NAME));
			Console.WriteLine( "  UPN name    = " + Certificate.GetInfo(CAPICOM_CERT_INFO_TYPE.CAPICOM_CERT_INFO_SUBJECT_UPN));
			Console.WriteLine( "  DNS name    = " + Certificate.GetInfo(CAPICOM_CERT_INFO_TYPE.CAPICOM_CERT_INFO_SUBJECT_DNS_NAME));
			Console.WriteLine();
			Console.WriteLine( "Issuer Name: " + Certificate.IssuerName);
			Console.WriteLine();
			Console.WriteLine( "Serial Number: " + Certificate.SerialNumber);
			Console.WriteLine();
			Console.WriteLine( "Not Before: " + Certificate.ValidFromDate);
			Console.WriteLine();
			Console.WriteLine( "Not After: " + Certificate.ValidToDate);
			Console.WriteLine();
			Console.WriteLine( "SHA1 Hash: " + Certificate.Thumbprint);
			Console.WriteLine();
			Console.WriteLine( "IsValid: " + Certificate.IsValid().Result);
			Console.WriteLine();
			Console.WriteLine( "Archived: " + Certificate.Archived);
			Console.WriteLine();
       

			if (Certificate.BasicConstraints().IsPresent) 
			{
				Console.WriteLine( "Basic Constraints:" );
				Console.WriteLine( "  Critical          = " + Certificate.BasicConstraints().IsCritical );
				Console.WriteLine( "  CA                = " + Certificate.BasicConstraints().IsCertificateAuthority );
				Console.WriteLine( "  PathLenConstraint = ");
				if (Certificate.BasicConstraints().IsPathLenConstraintPresent) 
				{
					Console.WriteLine( Certificate.BasicConstraints().PathLenConstraint);
				}
				else
				{
					Console.WriteLine( "Not present.");
				}
			}	
			else
			{
				Console.WriteLine( "Basic Constraints: Not present." );
			}          
			Console.WriteLine();
   
			if (Certificate.KeyUsage().IsPresent) 
			{
				Console.WriteLine( "Key Usage:");
				Console.WriteLine( "  Critical                  = " + Certificate.KeyUsage().IsCritical);
				Console.WriteLine( "  IsDigitalSignatureEnabled = " + Certificate.KeyUsage().IsDigitalSignatureEnabled );
				Console.WriteLine( "  IsNonRepudiationEnabled   = " + Certificate.KeyUsage().IsNonRepudiationEnabled);
				Console.WriteLine( "  IsKeyEnciphermentEnabled  = " + Certificate.KeyUsage().IsKeyEnciphermentEnabled);
				Console.WriteLine( "  IsDataEnciphermentEnabled = " + Certificate.KeyUsage().IsDataEnciphermentEnabled);
				Console.WriteLine( "  IsKeyAgreementEnabled     = " + Certificate.KeyUsage().IsKeyAgreementEnabled);
				Console.WriteLine( "  IsKeyCertSignEnabled      = " + Certificate.KeyUsage().IsKeyCertSignEnabled);
				Console.WriteLine( "  IsCRLSignEnabled          = " + Certificate.KeyUsage().IsCRLSignEnabled);
				Console.WriteLine( "  IsEncipherOnlyEnabled     = " + Certificate.KeyUsage().IsEncipherOnlyEnabled);
				Console.WriteLine( "  IsDecipherOnlyEnabled     = " + Certificate.KeyUsage().IsDecipherOnlyEnabled);
			}
			else
			{
				Console.WriteLine( "Key Usage: Not present.");

			}
			Console.WriteLine();
   
			if (Certificate.ExtendedKeyUsage().IsPresent) 
			{
				if (Certificate.ExtendedKeyUsage().EKUs.Count > 0)
				{
					OID OID;
					OID = new OIDClass();
					Console.WriteLine( "Extended Key Usage:");
					Console.WriteLine( "  Critical = " + Certificate.ExtendedKeyUsage().IsCritical);
					foreach (EKU EKU in Certificate.ExtendedKeyUsage().EKUs)
					{
						OID.Value = EKU.OID;
						Console.WriteLine( "  " + OID.FriendlyName + " (" + OID.Value + ")" );
					}
					OID = null;
				}
				else
				{
					Console.WriteLine( "Extended Key Usage: Not valid for any usage.");
					Console.WriteLine( "  Critical = " + Certificate.ExtendedKeyUsage().IsCritical );
				}
			}
			else
			{
				Console.WriteLine( "Extended Key Usage: Not present (valid for all usages).");
			}
			Console.WriteLine();
   
			if (Certificate.Template().IsPresent) 
			{
				Console.WriteLine( "Template:");
				Console.WriteLine( "  Critical = " + Certificate.Template().IsCritical);
				Console.WriteLine( "  Name     = " + Certificate.Template().Name);
				Console.WriteLine( "  OID      = " + Certificate.Template().OID.FriendlyName + "(" + Certificate.Template().OID.Value + ")");
				Console.WriteLine( "  Major    = " + Certificate.Template().MajorVersion);
				Console.WriteLine( "  Minor    = " + Certificate.Template().MinorVersion);
			}
			else
			{
				Console.WriteLine( "Template: Not present.");
			}
			Console.WriteLine();
   
			Console.WriteLine( "Public Key:");
			Console.WriteLine( "  Algorithm  = " + Certificate.PublicKey().Algorithm.FriendlyName + "(" + Certificate.PublicKey().Algorithm.Value + ")");
			Console.WriteLine( "  Length     = " + Certificate.PublicKey().Length + " bits");
			Console.WriteLine( "  Key blob   = " + Certificate.PublicKey().EncodedKey.Format(true));
			Console.WriteLine( "  Parameters = " + Certificate.PublicKey().EncodedParameters.Format(true));
   
			if (Certificate.HasPrivateKey()) 
			{
				Console.WriteLine( "Private Key:");
				Console.WriteLine( "  Container name   = " + Certificate.PrivateKey.ContainerName);
				//Don't display unique container name for hardware token because it may cause UI to be displayed.
				if (! Certificate.PrivateKey.IsHardwareDevice()) 
				{
					Console.WriteLine( "  Unique name      = " + Certificate.PrivateKey.UniqueContainerName);
				}
				Console.WriteLine( "  Provider name    = " + Certificate.PrivateKey.ProviderName);
				Console.WriteLine(     "  Provider type    = " );
				if (Convert.ToInt32(Certificate.PrivateKey.ProviderType) > ProviderTypes.GetUpperBound(0))
				{
					Console.WriteLine( ProviderTypes[0] + " (" + Convert.ToString(Certificate.PrivateKey.ProviderType) + ")");
				}
				else
				{
					Console.WriteLine( ProviderTypes[Convert.ToInt32(Certificate.PrivateKey.ProviderType)] + " (" + Convert.ToString(Certificate.PrivateKey.ProviderType) + ")");
				}
				Console.WriteLine(     "  Key spec         = " );
				if (Convert.ToInt32(Certificate.PrivateKey.KeySpec) > KeySpecStrings.GetUpperBound(0)) 
				{
					Console.WriteLine( KeySpecStrings[0] + " (" + Convert.ToString(Certificate.PrivateKey.KeySpec) + ")");
				}
				else
				{
					Console.WriteLine( KeySpecStrings[Convert.ToInt32(Certificate.PrivateKey.KeySpec)] + " (" + Convert.ToString(Certificate.PrivateKey.KeySpec) + ")");
				}
				Console.WriteLine( "  Accessible       = " + Certificate.PrivateKey.IsAccessible());
				Console.WriteLine( "  Protected        = " + Certificate.PrivateKey.IsProtected());
				Console.WriteLine( "  Exportable       = " + Certificate.PrivateKey.IsExportable());
				Console.WriteLine( "  Removable        = " + Certificate.PrivateKey.IsRemovable());
				Console.WriteLine( "  Machine keyset   = " + Certificate.PrivateKey.IsMachineKeyset());
				Console.WriteLine( "  Hardware storage = " + Certificate.PrivateKey.IsHardwareDevice());
			}
			else
			{
				Console.WriteLine( "Private Key: Not found." );
			}
			Console.WriteLine();
   
			if (true) 
			{
				int iIndex = 0;
		
				foreach (Extension Extension in Certificate.Extensions())
				{
					iIndex++;
					Console.WriteLine( "Extension #" + Convert.ToString(iIndex) + ": " + Extension.OID.FriendlyName + "(" + Extension.OID.Value + ")");
					Console.WriteLine( "  " + Extension.EncodedData.Format(true));
         
					if (Extension.EncodedData.Decoder() != null)
					{
						switch (Extension.OID.Value)
						{
							case Constants.CAPICOM_CERT_POLICIES_OID:
							{
								CertificatePolicies CertPolicies;
								int pIndex = 0;

								CertPolicies = (CertificatePolicies)Extension.EncodedData.Decoder();
								Console.WriteLine( "Decoded Certificate Policies: " + Convert.ToString(CertPolicies.Count) + " PolicyInformation(s)");
               
								foreach  (PolicyInformation PolicyInformation in CertPolicies)
								{
									int qIndex = 0;
									pIndex++;
									Console.WriteLine( "  PolicyInformation #" + Convert.ToString(pIndex) + ": " + Convert.ToString(PolicyInformation.Qualifiers.Count) + " Qualifier(s)");
									Console.WriteLine( "    OID = " + PolicyInformation.OID.FriendlyName + "(" + PolicyInformation.OID.Value + ")");
                  
                  
									foreach (Qualifier Qualifier  in PolicyInformation.Qualifiers)
									{
										qIndex++;
										Console.WriteLine( "    Qualifier #" + Convert.ToString(qIndex) + ":");
										Console.WriteLine( "      OID               = " + Qualifier.OID.FriendlyName + "(" + Qualifier.OID.Value + ")");
										Console.WriteLine( "      CPS URI           = " + Qualifier.CPSPointer);
										Console.WriteLine( "      Organization name = " + Qualifier.OrganizationName);
										Console.WriteLine( "      Notice number(s)  = ");
										if (Qualifier.NoticeNumbers != null) 
										{
											foreach (NoticeNumbers NoticeNumber in Qualifier.NoticeNumbers)
											{
												Console.WriteLine( Convert.ToString(NoticeNumber));
											}
										}
										Console.WriteLine();
										Console.WriteLine( "      Explicit text     = " + Qualifier.ExplicitText);
									}
									Console.WriteLine();
								}
								break;
							}
						}
					}
				}
				if (iIndex == 0) 
				{
					Console.WriteLine( "Extension: None.");
					Console.WriteLine();
				}
      
				iIndex = 0;
				foreach (ExtendedProperty ExtendedProperty in Certificate.ExtendedProperties())
				{
					iIndex ++;
					Console.WriteLine( "Property #" + Convert.ToString(iIndex) + " (ID = " + ExtendedProperty.PropID + "):" );
					Console.WriteLine( "  " + ExtendedProperty.get_Value(CAPICOM_ENCODING_TYPE.CAPICOM_ENCODE_BASE64));
				}
				if (iIndex == 0) 
				{
					Console.WriteLine( "Property: None.");
					Console.WriteLine();
				}
				
			}
		}
	}

	
}
