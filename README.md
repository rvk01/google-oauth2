google-oauth2.0
=================

Small simple FPC library for implementing Google OAuth 2.0

This can be used to do XOAUTH2 for sending mail via GMail


Requirements
============

This module / script collection use the following modules (available on CPAN):
* FPC 2.6.4+ (others untested)
* Synapse
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

Currently the provided credentials do work for the test-application.

Further Resources
=================
* https://developers.google.com/accounts/docs/OAuth2InstalledApp
* https://developers.google.com/oauthplayground/
* https://developers.google.com/accounts/docs/OAuth2
* https://developers.google.com/google-apps/gmail/oauth_overview
* https://developers.google.com/google-apps/calendar/
* http://masashi-k.blogspot.nl/2013/06/sending-mail-with-gmail-using-xoauth2.html

Todo
====

*
