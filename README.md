google-oauth2.0
=================

Small simple FPC library for implementing Google OAuth 2.0

This can be used to do XOAUTH2 for sending mail via GMail


Requirements
============

* FPC 2.6.4+ (others untested)
* Synapse 40+
* libeay32.dll
* libssl32.dll
* ssleay32.dll

How does it work?
=================

First of all, for your own project you'll need to create your own credentials.
The ones provides only works for my own test-address/account.
After you have tested your app you need to go through a pubisch/review process.

https://console.developers.google.com/apis/credentials
* Create a "New Project"
* Give the project a name and click "Create"
* Select the project and click upper left 3 lines button
* Wait for the creation of the project until the button "Select Project" is visible
* Click "Select Project"
* Click "Enable Apis and Services"
* If you want GMail/SMTP access search gmail and Enable "Gmail API"
* If you want Calendar access search calendar and Enable "Google Calendar API"
* If you want Drive access search drive and Enable "Google Drive API"
* If you want Contacts access search contacts and Enable "Contacts API"
* Click "OAuth consent screen"
* Choose "External" (Internal is only available for Google Workspace users)
* Fill out the consent screen
* Click "Save and continue"
* At the scopes click "Add or remove scopes"
* Check ".../auth/userinfo.email" and ".../auth/userinfo.profile" (first 2)
* If you want GMail/SMTP access seach gmail (in the filter) and add "https://mail.google.com/"
* If you want Calendar access search calendar and add https://www.googleapis.com/auth/calendar
* If you want Drive access search drive and add https://www.googleapis.com/auth/drive
* If you want Contact access search contacts and add https://www.google.com/m8/feeds
* Press the "Update" button
* Click "Save and continue"
* Click "Add Users" at the "Test users" screen
* Enter your own e-mail adres as test-user and click "Add"
* Click "Save and continue"
* Click "Credentials"
* Click "Create Credentials"
* Choose "OAuth client ID"
* Choose "Desktop app" and name it
* Click "Create"
* Click the "Download JSON" and save the .json file
* Click "Ok"

Now copy the client_secret_xxx.json to client.json in your app directory
Run the program, click "Remove token.dat" and click "Get Access"
Choose your test-address account and click continue
Make sure to check the All mail GMail checkbox in the authentication screen
and click "Continue"

The program should now give the message "We now have access" in the memo-screen
You can click the "Send mail" to send a test message

~~Currently the provided credentials do work for the test-application.~~  
The provided credentials do not work for other users. You need to register your own with the prcedure described above.

Note: The sope for composing mail `https://www.googleapis.com/auth/gmail.compose` is not
enhough to mail via smtp. Smtp access requires full mail-access (https://mail.google.com/).

Further Resources
=================
* https://developers.google.com/accounts/docs/OAuth2InstalledApp
* https://developers.google.com/oauthplayground/
* https://developers.google.com/accounts/docs/OAuth2
* https://developers.google.com/google-apps/gmail/oauth_overview
* https://developers.google.com/google-apps/calendar/
* http://masashi-k.blogspot.nl/2013/06/sending-mail-with-gmail-using-xoauth2.html

Synapse requirement
=================
For communication Synapse from Ararat is used. You can download the latest version from https://sourceforge.net/p/synalist/code/HEAD/tree/trunk/
(at the top-right is a "Download snapshot"-button. Put it somewhere and in Lazarus you can choose "Package" > "Open package file".
Browse to the folder for synapse and choose laz_synapse.lpk.
Extra step is to add the ssl_openssl.pas before compiling for HTTPS access.
You don't need to install anything. laz_synapse will now be available as package.
You also need the openssl DLLs in your project directory (or search-path). libeay32.dll, libssl32.dll and ssleay32.dll.

Todo
====

* Improve the documentation and comments
* Create a TDataset-like descendant for Calendar and EMail access

