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
(the ones provides here show its own title-header during authentication)

https://developers.google.com/identity/protocols/OpenIDConnect#getcredentials
* Create a project
* Enable the GMail and Google+ Api
* Set the details in "Consent screen"
* Create credentials for a "Installed application"
* And use the Client ID and Client secret in the example application

~~Currently the provided credentials do work for the test-application.~~  
The provided credentials do not work anymore. You need to register your own.

Note: The sope for composing mail `https://www.googleapis.com/auth/gmail.compose` is not
enhough to mail via smtp. Smtp access requires full mail-access.

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

