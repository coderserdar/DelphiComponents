============================
FlashFiler 2 in an ISAPI DLL
============================

The example in this folder demonstrates how to access a FlashFiler
database via an ISAPI DLL using a remote FlashFiler server or an
embedded FlashFiler server engine. It simulates a web-based inventory
reporting system for a fictional company named Mythic Proportions.

This example was created and tested under the following environment:

  - Windows 2000 Professional, Service Pack 1

  - IIS 5

  - Delphi 5.0

  - Remote server accessed using Single User Protocol and TCP/IP

  - Embedded server engine

The following FlashFiler 2 tables are used by this example:

  - The DistribC table contains the distribution centers for
    the fictional company.

  - The Product table contains the products held in the company's
    inventory.

  - The Invent table identifies the quantity of each product at
    each distribution center.

This file tells you how to set up the example program.  Please do the
following:

1. Create the FlashFiler alias MythicDB for the
   Examples\Mythic subfolder of your FlashFiler 2 installation.

2. Compile the FFWebInv.dpr project file, creating FFWebInv.dll.

3. Copy FFWebInv.dll to the Scripts subdirectory of your InetPub
   directory. The default is C:\InetPub\Scripts.

4. Start your web browser and type in the following URL:

   http://MachineName/Scripts/FFWebInv.dll

   For example, if your machine is named larry-dev then you would type:

   http://larry-dev/Scripts/FFWebInv.dll

   The initial web page asks you to select a product.  The drop down
   combobox lists the OEM number of each product handled by the company.

5. Select a product and click the Submit button.  The resulting web page
   lists the inventory of the product at each distribution center.


Notes
-----

1. This example can switch between using a remote FlashFiler Server and
   an embedded FlashFiler server engine. The latter provides the best
   performance as database calls are made directly into the server
   engine without having to travel through a transport layer. To have
   the example use an embedded server engine, comment out the following
   line in unit ffServer.pas:

     {$DEFINE UseRemoteServer}

   For example, to use an embedded server engine then the line would
   look like the following:

     {.$DEFINE UseRemoteServer}

   To use a remote FlashFiler Server, leave the previously-mentioned
   DEFINE as is. In addition, specify the communications transport and
   server name via the aProtocol and aServerName constants in the unit
   ffServer.pas.


2. This example was designed so that you could reuse the base connection
   data module FFConn and the server data module FFServer. To re-use the
   connection module, create an inherited data module that contains
   instances of TffTable and TffQuery as needed by your application.
   Data module FFConn will automatically connect the tables and queries
   to its TffDatabase and TffSession components.

   Your inherited data module must override the GetAliasName and
   GetAliasPath functions in order to provide the appropriate alias name
   and alias directory on the FlashFiler Server.


3. Once the ISAPI DLL has been accessed via a web browser, the DLL will
   be in use by the operating system until the computer hosting the DLL
   is rebooted or the World Wide Web Publishing Service is stopped.


4. If you want the ISAPI DLL to use an external FlashFiler Server
   located on the same computer as the ISAPI DLL and you want the
   communication to be performed using Single User Protocol (SUP) then
   you must enable the World Wide Web Publishing Service's "Allow
   service to interact with desktop" property. Use the following steps
   to enable this property in Windows 2000:

   a. On the Windows Start menu, select menu item Programs->
      Administrative Tools->Services. The Services window displays.

   b. Select the World Wide Web Publishing Service and click the right
      mouse button. A popup menu appears.

   c. Select the Properties menu item from the popup menu. The
      properties window displays.

   d. Go to the Log On page. Select "Local System account" as the "Log
      on as:" option.

   e. Under "Local System account", check the "Allow service to interact
      with desktop" property.
