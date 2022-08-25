Magenta Systems Mail Queue Component v2.5
=========================================

Updated by Angus Robertson, Magenta Systems Ltd, England, 26th November 2018
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

Magenta Systems Mail Queue Component is designed to add robust email
capability to applications that need to report alerts, errors, etc. The
component uses a disk queue to allow repeated mail attempts of multiple
emails using multiple mail servers over many hours or days, to ensure
mail is always delivered. The component needs François Piette's internet
component suite (ICS) versions 7 and 8 and supports SSL and HTML mail.

The TMagMailQueue component has two main benefits over a simple ICS
TSslSmtpCli component: it supports extended retries over many hours or days,
and supports multiple SMTP relay servers or looks-up MX servers, while
alleviating the need for the application to handle retries.  Mail is queued
to disk, so retries will continue if the application is restarted.

Preparing the email is done using the ICS THtmlSmtpCli component so it may
be plain text or HTML email with one or more file attachments. Once the mail
properties in QuHtmlSmtp have been specified, it is queued using the
QueueMail method which saves it to an EML spool file.

The component runs a thread which checks the queue for new EML spool files, and
attempts to forward them to one or more SMTP Mail Servers using TSslSmtpCli,
optionally with SSL. If mail delivery succeeds, the spool file may be deleted or
moved to an archive folder.  If mail delivery fails, the spool file remains in
the queue and further attempts are made separated by the times in minutes
listed in the RetryList list.  If all delivery attempts fail, the spool file
may be deleted or moved to a badmail folder.

Note that some email servers support grey listing and reject the first email
attempt from a new sender but allow a retry 10 or 15 minutes later, something
that is very effective in blocking spam emails (since they don't usually retry).

If multiple mail servers are specified, delivery is attempted once using
each server, for each retry attempt.  Each mail server is specified as
TMailServer and there is no limit to the total.

Each time the queue is updated or a delivery attempt made, the queue is saved to
file in the control folder, so the component may be stopped and restarted with
failed attempts continuing.

The EML spool files are compatible with those created by many Microsoft email
applications such as CDO, and the AddtoQueue method can also be used to queue
existing EML files with the queue details specified in MailQuItem.

Note, this component is intended for sending low volume email from individual
Delphi applications, with more flexibility than a simple TSslSmtpCli component.
For use as a heavy duty SMTP server, queue processing could be improved to
avoid moving records around as much or saving them to disk as often, and mail
bodies could be read as required from disk instead of being read entirely to
memory first.  A mail pickup folder could be added which is scanned for new EML
files.

Requires Internet Component Suite (ICS) V8.55 dated 20 June 2018 or later and
OpenSSL 1.1.0 or later, both of which may be downloaded from:
http://wiki.overbyte.eu/wiki/index.php/ICS_Download
The latest ICS version in the nightly zip includes the latest OpenSSL.

Compatible with Delphi 7/2006/2007/2009/2010/XE-XE8/D10/D10.1/D10.2
Tested with Windows Vista, 2008, 7, 8, 2012, 10, 2016


Files and Folders Used
----------------------

The TMagMailQueue component heavily uses disk files, in different sub-directories
within the mail root directory specified in property MailQuDir, these are:

- control - contains MailQuItems.Ctl a single row file with the next message item
            number, and MailQuItems.Hdr which is a CSV file containing one row for
            each mail item still in the queue.
- spool   - contains any queued email files, named in the format item00000001.eml
            with the number increasing, taken from MailQuItems.Ctl
- archive - if ArchiveSent property is true, once an email has been successfully
            sent it is moved into the archive directory
- badmail - if DeleteFailed property is false, once an email has exceeded all the
            retry attempts it is moved into the badmail directory, from where it
            may be manually requeued if necessary

If logging of sent email is specified, the default file name FileQuSent property
is MailQuSent-yyyymmdd.log inb CSV format similar to MailQuItems.Hdr.

A demo application mailqudemo.exe illustrates simple email queuing. The zip
contains the EXE demo and required SSL files



Release Notes
-------------

18th January 2011 - 1.0 - first public release.  Not yet tested with Delphi
2009 or later.

2nd March 2011 - 1.2 - automatically create mailqueue directory in demo
application, removed missing uses statement.  Support queuing mail with
OwnHeaders bypassing htmlmail.  Log event definition changed.

11th August 2011 - 1.2 - updates subroutines for Win64 support, removed one
unneeded unit from uses.

5th Oct 2011  - 1.3 - Debug logging works properly
Don't retry emails that fail too large for server (error 552)

11th Sept 2012 - 1.4 - ICS V8, IPv6

23rd March 2013 - 1.5 - Added Mail Server SocketFamily and LocalAddr6 for IPv6

10th Dec 2014 - 1.6 - Better SSL handshake reporting

27th Oct 2015  - 2.0 - requires ICS V8.19 October 2015 or later.
check and report SSL certificates using PEM file or Windows Cert Store
Allow three SMTP servers to be specified for each email in queue
Lookup DNS MX records and send to those SMTP servers
Queue keeps last response or error in queue
Mail completed log (same CSV format as queue)
Queue changed event to tell client something is happening
QueueMail method now returns item number (not boolean)
New UnQueueMail method to remove item number from queue
Demo save settings in INI file
Demo new View Mail Queue window to see what's waiting
Added SMTP Send Method, relay, specific or lookup MX mail servers
Added HELO Sending Host Name may be needed if using MX mail servers

Warning - if using MX DNS servers and multiple recipients, need to queue mail
multiple times !!!! This will be fixed for the next release.

7th July 2016  - 2.1 - requires ICS V8.30 July 2016 or later.
Support SSL enhancements in ICS for OpenSSL 1.1.0
Don't change SSL directory, let application control it
Use default SSL root bundle if none specified

1st December 2016 - 2.2 - requires ICS V8.39 November 2016 or later.
Better error handling.
Use OpenSSL host checking.
Fixed bug that meant failed email was not deleted from queue.
Don't queue email without recipients.
Use timer to update windows to avoid problems with mass email performance.

6th March 2017 - 2.3 - requires ICS V8.43 March 2017 or later.
Simplified SSL certificate reporting.

11 Mar 2017 - 2-4 - Added WaitSend to wait until everything sent.

22th Jun 2018 - 2.5 - requires ICS V8.55 20 June 2018 or later.
Added RetryWithoutSsl which retries an SSL failure without SSL.
Added SslCliSecurity to set client security level.
Using IcsWndControl for threaded message handling.
SendSmtpClient now created new for each attempt in case of prior faillure
causing terminal corruption.
If SSL certificate verify fails, next attempt is another server.
Supports TLSv1.3 with OpenSSL 1.1.1 beta.

26th November 2018 - 2.5 - tested with ICS 8.58
Added final OpenSSL 1.1.1a DLLs, recompiled.



Copyright Information
---------------------

Magenta Systems Mail Queue Component is freeware, but is still copyrighted
by Magenta Systems Ltd who may change the status or withdraw it at any
time, without notice.

Magenta Systems Mail Queue Component may be freely distributed via web
pages, FTP sites, BBS and conferencing systems or on CD-ROM in unaltered
zip format, but no charge may be made other than reasonable media or
bandwidth cost.


Magenta Systems Ltd
9 Vincent Road
Croydon
CR0 6ED
United Kingdom

Phone 020 8656 3636, International Phone +44 20 8656 3636

Email: delphi@magsys.co.uk
Web: https://www.magsys.co.uk/delphi/



