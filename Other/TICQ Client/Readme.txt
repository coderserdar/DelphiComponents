//
// TICQClient
// (C) Alex Demchenko(alex@ritlabs.com)
//     Gene Reeves(notgiven2k@lycos.com)
//
// Web: http://www.cobans.net
// Latest updates: http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/ticqlib
//
//
//
// TICQClient v1.20/v1.20f/v1.21 by
//   Dalibor Držík (eraser@senior.cz)
//   Yegor Derevenet (yegor@box.vsi.ru)
//-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

[           Legend:          ]
[ + Added feature            ]
[ * Improved/changed feature ]
[ - Bug fixed (I hope)       ]

30-Jun-2004, version 1.21
[+] Added idle time support (set/get idle time)
[+] Added procedure CreateCLI_SETIDLETIME
[+] Added German translation (Thx to Peter from Austria)
[*] Using CLI_ADDCONTACT_multi by login
[*] Improved the code of an example to compile in Delphi 5 and 6 (Thx to sas)
[-] Fixed wrong reference to a function (Component/ICQDb.pas)
[-] Fixed bug in renaming algorithm (Example/Main.pas)
[-] Fixed CreateCLI_SENDADVMSG_CUSTOM handling (current status missing)

23-April-2004, version 1.20f
[+] Added some variables and improved TLV's handling in SNAC(4,7)
[*] Some mirror changes and comments in source code
[-] Fixed reading of away-msgs

18-April-2004, version 1.20
[+] Added contact list entries authorization status reading (TUINEntry.Authorized).
[+] Reading time of offline messages and urls
[+] Added online since feature
[+] Added procedure to send user you've added him in your contact list (SendYouWereAdded)
[+] Added procedure to send authorization (SendAuthResponse)
[+] Added function StatusToInt()
[+] Added feature to detection ICQ clones and theirs version
[+] New status constants
[*] Completely improved the example application (Example/)
[*] Conversion functions were changed to use 64-bit integer
[*] Changed event OnAuthRequest
[*] Improved function StatusToStr()
[*] Improved reading of away-msgs.
[*] Added RequestOfflineMessages method. Now request for offline messages is not sending automatically on login.
[-] Removed some hints in ICQSock.pas
[-] Fixed StrToInt convertation error in HSnac1306. (Thx! Jeka)

18-April-2003, Version 1.19 beta
[+] Added Hungarian translation (Thx! to Mickey)
[*] Added connection idle timeouts to direct connections.
[*] Rewrote Direct Connection code, now supports connection on demand and send and recv of files is working. (nightrader)
[+] Added AddContactsMulti for adding a list of contacts in one bacth (thanks ?? from forums)
[!] Introducing multi-language support
[*] TICQDb: IdxFile property has been removed (component tries to find it in the dir with dat file), DbType is read-only; TICQDb guesses the DbType itself
[-] TICQDb: If .dat or .idx file were not exist a new (empty) file was created
[-] TICQDb: Fixed miranda-icq database reading
[-] More UTF-8 encodings, which make TICQClient capable with Chinese language (Thx! to Kevin)
[-] Fixed RTF->Plaintext converting function (Thx! to Alex Petrov)
[-] Fixed VerySimpleExample main form tab order and uses clause error
[-] Delphi5 warnings fix caused connection error in Delphi6
[-] Fixed Delphi 6 warnings in ICQSock.pas


26-January-2003, Version 1.19 alpha-2
[+] TICQDb: OnProgress event now works in Mirdanda-icq databases
[+] Added support for birthday, see Login procedure
[-] Fixed Delphi5 compatability
[-] Fixed some Delphi5 warnings
[-] Removed .manifest file, it caused errors while debug in WinXP


22-January-2003, Version 1.19 alpha (please test HTTP, HTTPS proxies and send reports to alex@ritlabs.com)
[!] Added HTTP, HTTPS, SOCKS4a proxy support
[+] New sockets idealogy
[+] Added LogOff procedure and OnLogOff event (nightrader)
[+] Added OnAuthRequest Event. When a user request your authorization. (nightrader)
[+] Added AddContactVisible and AddContactInvisible to compliment the RemoveContactVisible and RemoveContactInVisible.(nightrader)
[+] Added default ICQServer and ICQPort in TICQClient properties (they are not empty now in a newly created component)
[-] Handling data too much time caused 'Malformed packets' error (network packets were lost)
[-] Fixed SOCKS4 authorization bug (possible it wasn't worked before) (Thx! to Kevin)
[-] Messages sent from real ICQ client in away statuses were not received
[-] Fixed SOCKS5 authorization bug (Thx! to Vladimir Kochelev)
[-] Fixed TICQCLient.HSnac0407, Added URL Msg Fix (as described in the forums) (nightrader)
[-] Fixed integer overflow in ICQWorks.EncryptPak (nightrader)


16-September-2002, Version 1.18f (fix release)
[+] Added .manifest file in Example project for better WinXP GUI compatability
[-] Fixed SMS reply receiving (Thx! to Nick Anikin)
[-] Fixed OnConnectionRefused event, it wasn't called when connection with ICQ server was lost (Thx! to Mironov Serghei)
[-] Application containing TTimer on the same form as TICQClient generated Access Violation error (Thx! to Simon de James)


07-September-2002, Version 1.18
[!] Sorry that HTTP proxy support wasn't added in this release, it's impossible due to blocking sockets architecture, but ICQ HTTP PROXY protocol has been discovered and parsed (for developers: see MySocket.pas)
[!] Added file receiving see 'File Transfers' in Help.chm
[+] Added RequestInfoShort procedure & OnUserInfoShort event for requesting the short info about user
[+] Added LastError: String property in TICQClient
[+] Added ICQClientVer_Major, ICQClientVer_Minor constants in ICQWorks.pas
[*] Some work-speed improvements
[*] Improved info request/response procedures & events
[*] Improved white pages (more compatible with ICQ2002a)
[*] Improved sockets: data sending should work better now (especially with badly internet connection)
[*] Improved TICQClient timeout timer
[*] Contacts & contacts requests can be sent/received also through server now, see SendContacts, RequestContacts functions
[*] OnPasswordChanged events moved to OnInfoChanged event.
[-] Finally fixed the bug which doesn't allow to close the application having TICQClient while Windows restarting/shutting down
[-] Fixed bug: users where not added to invisible list while receiving server side lists in Example (bug in Example) (Thx! to Attila)
[-] Fixed age field in OnUserFound event when searching white pages
[-] Fixed disconnection bug in direct connection sessions (server doesn't handled disconnection events)
[-] Fixed access violation on receiving OnInfoBackGround events with different count in Pasts & Affiliations lists (bug in Example)
[-] Fixed UTF8ToStrSmart function, now it should convert UNICODE strings from ICQ server nearly perfectly ;) (Thx! to Alexander Vaga)
[-] Fixed TMySock access violation when destroying the object after socket reported OnConnectError
[-] Lots of access violation fixes, which appear while using heavily TICQClient as a non-visual object


17-August-2002, Version 1.17
[!] Added server side list upload support (you can even add users into SSL without authorization!), see 'Server Side Lists' in Help.chm
[!] Added Miranda-icq database support in TICQDb component, see DbType property in Help.chm
[+] Added ignore list support, see Server Side Lists in Help.chm
[+] Added OnURLFound event in TICQDb
[+] Added OnChangePasswordError event, called when password cannot be changed
[+] Added new ERR_LOGIN type in OnError event, for more info see Help.chm file - OnError event
[+] Added SetAuthorization procedure setting 'authorization required' and 'webaware flag', see SetAuthorization procedure, OnAuthorizationChangedOk event
[+] ICQ databases can be open now while ICQ is running
[*] Added authorize parameter in OnUserFound event
[-] Fixed internet address resolving, which caused connection errors on some Win98 systems
[-] Received SMS messages were not converted from UTF-8 format
[-] Fixed devision by zero in TICQDb component


10-August-2002, Version 1.16
[+] Added SMS reply event, now you can receive messages sent through cellular, see OnSMSReply event (Thx! to Olivier)
[+] Added connection timeout support, see ConnectionTimeout property
[+] Added OnError event, now you can understand why you've been disconnected :)
[+] Answers on info changing, see OnInfoChanged event
[+] An option disabling direct connections, see property DisableDirectConnections
[*] Direct connections are estabilished now through proxy (if any)
[*] Removed thread layer while resolving IPs in SOCKS proxies
[*] MySocket.pas improvements, fixed potential Access Violation bugs
[*] TProxyType moved to ICQWorks.pas
[-] Fixed SetSelfInfoMore procedure: languges were broken
[-] Fixed message ACKs
[-] Proxy packets were dumped in OnPktParse event
[-] SendSMS procedure didn't convert Text to UTF-8 format
[-] Client should disconnect now on receiving malformed packets


06-August-2002, Version 1.15
[!] SOCKS4, SOCKS5 proxy support, only experimental now, please test it
[+] Added keep alive packets support, see procedure SendKeepAlive
[+] Added OnOnlineInfo event providing advanced info about users going online: Internal & External IPs, Port, Protocol version.
[-] Messages from Mac clients were not received (Thx! to AV(T))
[-] Fixed range checking errors in TICQClient component (Thx! to Klimashev I.A.)
[-] Fixed unicode strings in server side contact list group names
[-] Fixed icon displaying in UserSearch form of the Example project
[-] Some help fixes and modifications


16-July-2002, Version 1.11
[!] Added TICQDb example. See DbConverter.
[+] Impoved SMS support, see OnSMSAck & OnSMSRefused events.
[-] TICQDb: .idx and .dat files were not closed after importing
[-] Offline messages were not received (Thx to Sergey Sokolov)


15-July-2002, Version 1.1
[!] Importing messages, urls, contacts(with their info) & owner's info (including password!) from ICQ2000x, ICQ2001x, ICQ2002a databases. See TICQDb component.
[!] SMS messages are working :) (Thx! to Nick Barrett)
[*] property Pasword changed to property Password (sorry for spelling)
[*] New help file! (.chm format)
[*] Removed AddContactVisible, AddContactInvisible functions, use VisibleList.Add, InvisibleList.Add instead.
[-] Fixed bug with Visible/Invisible lists: after adding a user to your vis./inv. lists you was disconnected from server. (Thx! to Karloz R.)
[-] Sorry that new Russian help file isn't included in this release, I have't got time for it bacause of going in the summer hollidays :)


12-July-2002, Version 1.0 (Release!)
[+] Added contacts request, see RequestContacts function
[+] Added acks on every direct packet, see OnDirectPacketAck event.
[-] Russian help didn't contain some functions & events
[-] Main example should work now in Delphi5 & other compatability issues


10-July-2002, Version 0.9
[!] Direct connections are supported now!
[!] OnMessageRecv & OnOfflineMsgRecv are divided now into OnMessageRecv/OnURLRecv & OnOfflineMsgRecv/OnOfflineURLRecv, so the MsgType param isn't used now
[+] Sending/Receiving messages directly to client
[+] Sending/Receiving contacts
[+] Receiving contacts requests
[+] Improved MySocket.pas, now it works safier and doesn't use any threads


05-July-2002, Version 0.8
[+] Added Russian translation of Help.html. See HelpRus.html.
[+] Added password changing procedure. See ChangePassword procedure & OnChangePasswordOk event.
[+] Added unregistering the existing UIN. See UnregisterUIN procedure and OnUnregisterBadPassword, OnUnregisterOk events.
[-] UNICODE(UTF-8) names in the server side contact lists were not converted to ASCII.
[-] Packet dumper now parses more packets on channels 1 & 4


05-July-2002, Version 0.7
[+] Now you can register new UIN from TICQClient! See RegisterNewUIN procedure and OnNewUINRegistered, OnNewUINRefused events.
[+] Auto-away messages support. See RequestAwayMsg procedure, OnAutoMsgResponse event and AutoAwayMessage property.
[-] Since v0.6 'White Pages' did not work properly (Thx! to Ozan Kulahci)
[-] Login event was called too early, so search & some other functions were not working immideatly after login (Thx! to Ozan Kulahci)


04-July-2002, Version 0.6
[!] Removed Connecting property & OnConnect, OnConnectError, OnDisconnect events. I think there's no way of using them. OnConnectionFailed now called when you loose connection with server or you cannot connect to it.
[+] Added advanced message support(SendMessageAdvanced procedure, OnAdvancedMsgAck event). Now you can receive confirmations on messages you've sent!
[+] Added SendSMS(const Destination, Text: String) procedure for sending sms messages, please, test it and mail me(email above) about results, country where I live isn't supported by the ICQ sms gateway :)
[+] Added new items to languages constant array
[-] Example wasn't showing messages received in RTF format


02-July-2002, Version 0.5
[+] Added visible/invisible list support: look at AddContactVisible, AddContactInvisible functions and VisibleList, InvisibleList TStringLists in Help.html file
[+] Updated main Example, now it receives server lists, saves contact list to disk and adds users from the search dialog
[+] Added new VerySimple example with just logging and sending/receiving messages. This example shows easy of use of the TICQClient component.
[+] Changed AddContact(UIN: LongWord) procedute to AddContact(UIN: LongWord): Boolean function, it returns True when user is added to the list and False if it's in the list already
[+] Added RemoveContact procedure, which removes UIN from user's contact list while you are online
[-] Fix in AddContact procedure it was sending the entire contact list, instead of new UIN
[-] User's info StringLists(Interests, Affiliations, etc) in Example having the same group names caused errors: the first group and value were repeated a few times instead of showing the different values with the same groups


01-July-2002, Version 0.4
[!] Server side contact list is supported now!
[+] Added SendURL(UIN: LongWord; const URL, Description: String) procedure for sending URLs
[+] Added SetSelfInfoGeneral, SetSelfInfoMore, SetSelfInfoAbout procedures for uploading the self info
[-] Fixed SearchByName procedure, now it works correctly


30-June-2002, Version 0.3
[+] Added ICQClient.dcr icon, thx to Quique!
[+] Added OnUserInfoBackground event
[-] Fix with user's gender in 'White Pages'
[-] A few other small fixes


28-June-2002, Version 0.2
[+] Added white pages & random search
[+] Update to PacketDump viewer (PktDump.pas), not it parses CLI_META and SRV_META packets
[-] Fix in SRV_METAINTEREST, interests were badly parsed
[-] Some fixes with LNTS strings


26-June-2002, Version 0.1
[!] First public release