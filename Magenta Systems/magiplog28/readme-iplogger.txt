Magenta Systems IP Log Streaming Component v2.8
===============================================

Updated by Angus Robertson, Magenta Systems Ltd, England, 14th December 2018
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd


TMagIpLog is designed for IP stream logging, using TCP Client, TCP Server,
UDP Client or UDP Server protocols, sending simple text lines across a
network so they may be displayed or written to disk remotely. The component
allows two way communication with TCP and UDP, so may also be used for
simple protocols such as communication between two applications. The
component supports multiple client sockets so may be used to send data to
two or more different remote servers at the same time. For TCP and UDP
clients, the component will optionally ping the remote computer first
before opening an IP connection to allow faster failure retries and some
confirmation that UDP may work. TCP client provides repeated connection
retry attempts, including re-establishing a lost connection. UDP client
will optionally keep pinging the remote during a connection to ensure it's
still there. UDP server sends data to the IP address and port from which
it last received data. TCP server supports multiple remote clients
connecting. Received data is parsed for various line endings optionally
removing control characters and triggering an event for a received line.
The only other two events are optional, one for state changed when starting
and stopping, the second offering progress information and errors. The
component supports both IPv4 and IPv6, host name lookup for TCP and UDP
Client, and SSL connections for TCP Client and TCP Server, including
remote server certificate checking using either a local PEM bundle root
 file or the Windows Certificate Store.

A demo application testiplog.exe illustrates use of TMagIpLog as a TCP or
UDP client or server, and both in the same program sending data locally.

The same component may be used in a client or server application, to send
or receive.

Note: applications using this component also need OverbyteIcsLogger in
application  uses to satisfy the event type TLogOption.

The Magenta Systems ComCap application may also be used to capture IP
streams to files or a database.

Requires Internet Component Suite (ICS) V8.55 dated 20 June 2018 or later and
OpenSSL 1.1.0 or later, both of which may be downloaded from:
http://wiki.overbyte.eu/wiki/index.php/ICS_Download
The latest ICS version in the nightly zip includes the latest OpenSSL 1.1.1.

Compatible with Delphi 7, 2007, 2009, 2010, XE, XE2, XE3, XE4, XE5,
XE6, XE7, XE8, 10 Seattle, 10.1 Berlin, 10.2 Tokyo and 10.3 Rio.
Compatible with Windows Vista, 2008, 7, 8, 2012, 10, 2016 and 2019.


Installation
------------

It is recommended the TMagIpLog files are copied into a new sub-directory
within the ICS directories, specifically (ics)\Source\Magenta.  It is not
necessary to add TMagIpLog to a package unless you want to drop it on a
form, if add it to an iCS package.  Also add this new directory to the
browsing path.


Using TMagIpLog
---------------

1 - Drop the component onto a form (or create it in code, see testiplog.exe).

2 - Specify LogProtocol as one of logprotUdpClient, logprotUdpServer,
logprotTcpServer, logprotTcpClient.

3 - For client protocols, specify RemoteHost (name or IP address) and
RemoteIpPort, CheckPing true if ping to be used, RetryAttempts to non-zero
if continual retries not needed, RetryWaitSecs for delay between retries .

4 - For server protocols, LocalIpAddress is 0.0.0.0 to listen on all local
addresses, LocalIpPort must be non-zero.

5 - For sending data, AddCRLF to false if line already have terminating
characters, UdpNoCRLF to false if UDP should send CRLF.

6 - For receiving data, LineEndType to one of lineendCR, lineendLF,
lineendCustom (set in hex in CustomLineEnd) or lineendPacket (for UDP),
then MaxLineLen if a line should be returned before lineend is found,
normally non-ASCII characters are removed, set StripControls to false if
they should be replaced by spaces, RawData to true if CR, LF, FF and
control characters should not be removed.

7 - Assign onLogRecvEvent if data is to be received, onLogChangeEvent if
tracking of start and stop is needed, onLogProgEvent if progress information
is needed for logging.

8 - Call StartLogging.  The LogChangeEvent and LogProgEvent will trigger
when LogState changes to logstateOK when data may be sent.

9 - To send a line, if function GetAnyStateOK is true, call SendLogLine.
MaxSendBuffer specifies the amount of data that can be buffered otherwise
SendLogLine will fail.

10 - Received data will trigger LogRecvEvent once per line.

11 - Call StopLogging to stop.  Buffered data may continue to be sent
after close, keep calling CheckStopped until true when it's really finished
and component may be destroyed.

12 - To send an unlimited size stream, create a stream in the application
with TBufferedFileStream or TFileStream, and pass it to SendStream.
LogState changes to logstateOKStream while it's being sent, then back to
logstateOK as it finishes, the application should then free the stream.

13 - There is no specific handling for receiving a stream, textual data
will be handled according to the normal line end properties, and can be
saved to another stream in LogRecvEvent.  Binary data is more problematic,
set RawData to true and MaxLineLen to get a buffer load at a time, but
the last buffer load will need to be extracted with GetPartialLine using
a timeout, this is called automatically when the connection is closed.

14 - To send to multiple clients, set MaxSockets to the number needed,
then use the function SetRemotes to specify the remote host and port for
each socket number, base 0.  The events all return Socnr to indicate which
socket. MaxSockets also specifies how many remote clients can connect to
TCP Server, but note that Socnr is dynamic and changes as remote clients
come and go.

15 - To support SSL on TCP/IP client or server, drop an TSslContext
component on the form, assign it to the LogSslContext property and set
the ForceSsl property to true. For better performance, set LogSslSessCache
to a TSslAvlSessionCache component.

16 - For SSL TCP Server, the SslContext component must have the SslCertFile
and SslPrivKeyFile properties set to the file names of an SSL certificate
and Private Key PEM files respectively, and SslCipherList set to
sCipherMozillaSrvBack for strong but backward compatiblle cipher support.
The component includes sample self signed certificate and password files
iplog-cert.pem and iplog-prvkey.pem, and you can create your own with the
ICS SSL sample application Pemtool, or buy commercial PEM certificates.

17 - For SSL TCP Client, the SslContext component must have the SslCAFile
property set to the file name of a PEM root certification authority file
containing trusted root certificates.  Such a file is supplied with the
component RootCaCertsBundle.pem containing various root certificates covering
most major registries. SslContext  SslCipherList can be left as the default
to allow connection to any server. The LogSslVerMethod property can be
logSslVerNone to skip certificate verification, logSslVerBundle to check
using the CA bundle file or logSslVerWinStore to check using the Windows
certificate store (a little slower, bur maybe more certificates). To check
if certificates have been revoked set LogSslRevocation to true, beware this
needs public internet access and can be very slow or fail. LogSslReportChain
set to true reports certificate details checked.


Release Notes
-------------

18th August 2007 - 1.1 - using OverbyteIcsFtpSrvT instead of OverbyteIcsLibrary,
UDP receive packets may be from multiple hosts, always keep IP.

5th August 2008 - 1.2 - made compatible with ICS V7 and Delphi 2009.  Note only
supports ANSI with Delphi 2009 and later.

20th August 2009 - 1.3 - fixed problem with MaxSockets being reported as closed
in the event when only one was open, tested with Delphi 2010

9th August 2010 - 1.4 - removed cast warnings with Delphi 2009 and later

22nd Sept 2011 - 1.5 - added SndBufSize and RcvBufSize to increase buffer sizes and speed

11th Sept 2012 - 1.6 - better error for too many clients with server
added CurSockets property for current number of server sockets

7th July 2014 - 2.0 - now only ICS 8 and later, using new ICS ping.
Added IPv6 and SSL support, including server certificate checking.
Added host name support for UDP and TCP client with DNS lookup.
Added LogProtocols suffixed 6 for IPv6.
Cleaned up some progress messages, identify error progress events.
Removed line length limit of 1024 that was not checked.
Added send a stream of unlimited length.
Get buffered partial received line during close.
Default line end is LF instead of CR so UNIX files are processed.

13th July 2015 - 2.2 - requires ICS V8.18 June 2015 or later.
Added better SSL handshake error reporting.
Added lineendCRLF, only support FF as lineend if using CR.
Added Debug Info button for ICS info level logging.
Added SSL Server DH Params, set ECDHCurves, both for ECDH ciphers.
Note OpenSSL no longer support dhparam512, minimum is 768 bits.

23rd Oct 2015  - 2.3 - requires ICS V8.19 October 2015 or later.
Better SSL client and server certificate reporting.

8th July 2016  - 2.4 - requires ICS V8.30 July 2016 or later.
Fixed certificate reporting typo.
Removed TBufferedFileStream, not needed.
Added SrvTimeoutSecs to close idle server sessions, note needs ICS V8.30
or later to fix a SSL bug that stopped SrvTimeoutSecs working.
Added Socket property to get current socket, mainly for statistics
Report session length and data xmit/recv before closing

23rd Nov 2016  - 2.5 - requires ICS V8.39 November 2016 or later.
Added GetSendWaiting to check how many bytes of send data not yet sent.
Increased default MaxSendBuffer size to 64K.
Added property TotRecvData total data received since connection, or
  when method ResetRecvData was called.
Added property MaxRecvData which causes onLogRecvEvent to be called
  when that length has been received.  May be used for fixed length
  binary packets or where received data contains a content length
  such as a HTTP response header followed by binary data.
Server takes exclusive access of addr/port.
Fixed bug with multiple clients not using correct port.
Added SSL Server Name Indication support.
Check multiple client SSL host names correctly.
Removed USE_SSL so SSL is always supported.
Removed TX509Ex now using TX509Base.
Using OpenSSL certificate verification host checking.
Server now supports LogSslReportChain to report server certificates,
  checks expired and reports chain.

7th March 2017 - 2.6 - requires ICS V8.43 March 2017 or later.
set IcsLogger for context so it logs more stuff.
Simplified reporting SSL certs in client handshake.
Improved validation of server certificates.
Use threaded DNS lookup.

22nd June 2018 - 2.7 - requires ICS V8.55 20 June 2018 or later.
Support TLSv1.3, no real changes.
Don't start SSL handshake twice.
Cleaned up SSL error handling.
Added SslCliSecurity to set client security.

14th December 2018 - 2.8 - tested with ICS 8.58
Added final OpenSSL 1.1.1a DLLs, recompiled.
Removed madexcept.

Pending major changes to use IcsHosts in 3.0.



Copyright Information
---------------------

Magenta Systems IP Log Streaming Component is freeware, but is
still copyrighted by Magenta Systems Ltd who may change the status or
withdraw it at any time, without notice.

Magenta Systems IP Log Streaming Component may be freely distributed via
web pages, FTP sites, BBS and conferencing systems or on CD-ROM in
unaltered zip format, but no charge may be made other than reasonable
media or bandwidth cost.


Magenta Systems Ltd
9 Vincent Road
Croydon
CR0 6ED
United Kingdom

Phone 020 8656 3636, International Phone +44 20 8656 3636

Email: delphi@magsys.co.uk
Web: https://www.magsys.co.uk/delphi/



