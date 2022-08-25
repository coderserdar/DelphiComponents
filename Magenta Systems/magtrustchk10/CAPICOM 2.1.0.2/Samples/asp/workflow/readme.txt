CAPICOM2 sample
-----------------------
Date:  April 2, 2002

Questions about the sample should be posted on:
http://discuss.microsoft.com/archives/capicom.html

These demonstration scripts run on Windows 9x with IE 5.0 or later, Windows 
NT 4.0 with SP4 or later, Windows 2000 and later.  It uses CAPICOM.dll V2, an 
ActiveX component, that must be registered.

In this sample, employees submit order requests to a webserver.  These order requests are digitally signed.  An approver views the transaction and approves it by cosigning.  The server then validates both signatures.  

Requirements
------------
Workflow Option 1:
In this simplified version, employees can approve their own order requests.  This method is meant for those who do not have an active directory set up and just wish to examine the workflow components.  This is the default.

Workflow Option 2:
This method requires Active Directory.  The employee/manager relationship and emailAddress attribute should be set in the Active Directory.  Use the "Active Directory Users and Computers" MMC snap-in to configure user objects in Active Directory.  Visit the options page in the sample to change to this method.

To turn on revocation checking visit the options page in the sample.

This method requires certain security settings on your web site.  The ASP applications need to be able to query the directory.  You have several options:
1.  Use anonymous access and specify a user account/password for IIS.
2.  Use Authenticated access and select "Integrated Windows Authentication".  Turn off anonymous access if you select this option.
3.  Use authenticated access with Basic Auth and server side SSL.  Do not use Basic Auth without SSL. Turn off anonymous access if you select this option.


The certificates used in this application assume there is a UPN component in the Subject Alt Name extension. 


1.  Create a directory called "capicom" to host the pages in your webserver directory (e.g. C:\Inetpub\wwwroot\capicom) and copy the ASP pages there.
2.  Access the start page: http://<servername>/capicom/submitter.asp.
