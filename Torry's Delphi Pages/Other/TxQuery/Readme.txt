Introduction
TxQuery is created and owned by Alfonso Moreno.  He has stopped the development of TxQuery for years.  However, there are many Delphi developers still using TxQuery.  A barrier for TxQuery migrating to Delphi 2009 and 2010 is the introduction of Unicode.

I contact Alfonso Moreno on Nov 17, 2009 to ask if he may consider make TxQuery open source and let this great product continue enhanced by the Delphi community.  He finally agree and I wish to say big "Thank You" for his contribution.

I have attempted to patch the source code to make it compile and work with Delphi Unicode.  Test cases has been created to make sure it works as expected.  I know there are other cases that I didn't cover yet, just alert me promptly.  You are also welcome to join the maintenance and enhancement for this project.

TxQuery component is a TDataSet descendant component that can be used to query one or more TDataSet descendant components using SQL statements. It is implemented in Delphi 100% source code, no DLL required, because it implements its own SQL syntax parser and SQL engine.

It can be very useful for TDataSet descendants components (including TClientDataSet) that do not use the BDE and that do not implement the SQL language or to mix tables types (dbase, paradox, access).

Source Code
As mention, TxQuery is now open source.  The license of the software is Mozilla Public License 1.1.

You may download the latest source code from the Alfonso Moreno from TxQuery Version 1.86.2.7z .

I have patched the source code to make it work for Delphi 2007, 2009 and 2010.  You may always check out from SVN repository: https://code.google.com/p/txquery/ .

Support
You may post all your question in TxQuery group  (http://groups.google.com/group/txquery ).  I will try my best to answer your question.

Join the TxQuery Development
You are always welcome to join the TxQuery development and enhancement: https://code.google.com/p/txquery/
