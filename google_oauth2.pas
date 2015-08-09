{ google_oauth2

  Copyright (C) 2015-2015 Rik van Kekem (rvk)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{
  Purpose:
  With this unit (class TGoogleOAuth2) you can get access to the Google apis

  https://developers.google.com/identity/protocols/OpenIDConnect#getcredentials
  https://developers.google.com/accounts/docs/OAuth2InstalledApp
  https://developers.google.com/oauthplayground/
  https://developers.google.com/google-apps/calendar/
  http://masashi-k.blogspot.nl/2013/06/sending-mail-with-gmail-using-xoauth2.html

  2015-07-08 usebrowsertitle to get the auth.code from the title-bar (which sometimes didn't work)
  2015-07-08 implemented focemanualauth so you always need to enter auth.code manually
  2015-07-07 some extra debug information
  2015-07-07 initial getinformation only done when access_token is not empty
  2015-07-07 extra variable Tokens_refreshed to indicate you need to save the tokens
  2015-07-04 initial release

}
// todo: convert for delphi compatibility
// todo: maybe make token_filename changable
// todo: improve the documentation and comments

unit google_oauth2;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils,
  fpjson, jsonparser;

type
  GoogleScope = (goMail, goContacts, goCalendar, goDrive);
  GoogleScopeSet = set of GoogleScope;

type
  TGoogleOAuth2 = class
  private
    { private declarations }
    FClient_id: string;
    FClient_secret: string;
    FAuthorize_token: string;
    FAuthorize_token_html: string;
    FRefresh_token: string;
    FAccess_token: string;
    FTokens_refreshed: boolean;
    FForceManualAuth: boolean;
    FUseBrowserTitle: boolean;
    FScopes: TStringList;
    FLastErrorCode: string;
    FLastErrorMessage: string;
    FFullname: string;
    FEMail: string;
    FLogMemo: TMemo;
    FDebugMemo: TMemo;
  private
    procedure LoadAccessRefreshTokens;
    procedure SaveAccessRefreshTokens;
    procedure GetAuthorize_token_interactive;
    procedure GetRefresh_token;
    procedure GetAccess_token;
  public
    { public declarations }
    constructor Create(client_id, client_secret: string); virtual;
    destructor Destroy; override;
    function RetrieveJSONValue(JSON: TJSONData; Value: string): string;

    procedure GetAccess(Scopes: GoogleScopeSet = []; UseTokenFile: boolean = False);
    function GetXOAuth2Base64: string;
    procedure LogLine(Value: string);
    procedure DebugLine(Value: string);

    property Tokens_refreshed: boolean read FTokens_refreshed write FTokens_refreshed;
    property Authorize_token: string read FAuthorize_token write FAuthorize_token;
    property Authorize_token_html: string read FAuthorize_token_html
      write FAuthorize_token_html;
    property Refresh_token: string read FRefresh_token write FRefresh_token;
    property Access_token: string read FAccess_token write FAccess_token;
    property ForceManualAuth: boolean read FForceManualAuth write FForceManualAuth;
    property UseBrowserTitle: boolean read FUseBrowserTitle write FUseBrowserTitle;
    property LastErrorCode: string read FLastErrorCode write FLastErrorCode;
    property LastErrorMessage: string read FLastErrorMessage write FLastErrorMessage;
    property Fullname: string read FFullname write FFullname;
    property EMail: string read FEMail write FEMail;
    property LogMemo: TMemo read FLogMemo write FLogMemo;
    property DebugMemo: TMemo read FDebugMemo write FDebugMemo;
  published

  end;

implementation

uses
  synacode, synautil, httpsend, // for communication
  ssl_openssl, // you need to include this one in your requirements
  comobj, // for ceating Browser-object
  base64, // for the XOAuth2 token
  Dialogs, // for inputbox
  Forms; // for Screen.Width/Height

const
  token_filename = 'tokens.dat';
  GetTokenUrl = 'https://accounts.google.com/o/oauth2/token';
  AuthorizationUrl = 'https://accounts.google.com/o/oauth2/auth';
  RedirectUri = 'urn:ietf:wg:oauth:2.0:oob';

function TGoogleOAuth2.RetrieveJSONValue(JSON: TJSONData; Value: string): string;
var
  D: TJSONData;
begin
  Result := '';
  if Assigned(JSON) then
  begin
    D := JSON.FindPath(Value);
    if assigned(D) then
      Result := D.AsString;
  end;
end;

constructor TGoogleOAuth2.Create(client_id, client_secret: string);
begin
  inherited Create;
  FClient_id := client_id;
  FClient_secret := client_secret;
  FAuthorize_token := '';
  FAuthorize_token_html := '';
  FRefresh_token := '';
  FAccess_token := '';
  FTokens_refreshed := False;
  FForceManualAuth := False;
  FUseBrowserTitle := True;
  FLastErrorCode := '';
  FLastErrorMessage := '';
  FFullname := '';
  FEMail := '';
  FLogMemo := nil;
  FDebugMemo := nil;
  FScopes := TStringList.Create;
  FScopes.Delimiter := ' ';
  FScopes.QuoteChar := ' ';
end;

destructor TGoogleOAuth2.Destroy;
begin
  FScopes.Free;
  inherited Destroy;
end;

procedure TGoogleOAuth2.LogLine(Value: string);
begin
  if LogMemo <> nil then
    LogMemo.Lines.Add(Value);
  DebugLine(Value);
end;

procedure TGoogleOAuth2.DebugLine(Value: string);
begin
  if DebugMemo <> nil then
    DebugMemo.Lines.Add(Value);
end;

procedure TGoogleOAuth2.GetAccess(Scopes: GoogleScopeSet = [];
  UseTokenFile: boolean = False);

  procedure GetInformation;
  var
    URL: string;
    Params: string;
    Response: TStringList;
    P: TJSONParser;
    J, D: TJSONData;
  begin
    Url := 'https://www.googleapis.com/plus/v1/people/me';
    Params := 'access_token=' + access_token;
    Response := TStringList.Create;
    try
      if HttpGetText(URL + '?' + Params, Response) then
      begin
        P := TJSONParser.Create(Response.Text);
        try
          J := P.Parse;
          if Assigned(J) then
          begin
            D := J.FindPath('error');
            if assigned(D) then
            begin
              LastErrorCode := RetrieveJSONValue(D, 'code');
              LastErrorMessage := RetrieveJSONValue(D, 'message');
            end;
            Fullname := RetrieveJSONValue(J, 'displayName');
            D := J.FindPath('emails');
            if assigned(D) and (D.Count > 0) then
              D := D.Items[0];
            EMail := RetrieveJSONValue(D, 'value');
          end;
        finally
          if assigned(J) then
            J.Free;
          P.Free;
        end;
      end;
    finally
      Response.Free;
    end;
  end;

begin
  if Scopes = [] then
  begin
    LogLine('No scope specified in GetAccess');
  end;

  FScopes.Add('profile');
  FScopes.Add('email');
  // always use profile/email to find the full name and email

  if goCalendar in Scopes then
  begin
    FScopes.Add('https://www.googleapis.com/auth/calendar');
  end;

  if goMail in Scopes then
    FScopes.Add('https://mail.google.com/');

  if goContacts in Scopes then
    FScopes.Add('https://www.google.com/m8/feeds/');

  if goDrive in Scopes then
    FScopes.Add('https://www.googleapis.com/auth/drive');

  if UseTokenFile then
    LoadAccessRefreshTokens
  else
  begin
    // LogLine('If you had an access_token please set it before calling GetAccess');
  end;

  Fullname := '';
  EMail := '';
  if access_token <> '' then
  begin
    LogLine('Getting account information');
    GetInformation;
  end;
  if (LastErrorCode <> '') or (access_token = '') then
  begin
    if access_token <> '' then
    begin
      LogLine(Format('Error: %s - %s', [LastErrorCode, LastErrorMessage]));
      LogLine(Format('Invalid access_token %s', [access_token]));
      Access_token := ''; // <- invalidate
    end;
    GetAccess_token;
    if access_token <> '' then
    begin
      LogLine('Getting account information');
      GetInformation;
      if (EMail <> '') then
      begin
        Tokens_refreshed := True; // and correct
        if UseTokenFile then
          SaveAccessRefreshTokens
        else
          LogLine('Please save the access_token');
      end;
    end;
  end;

  if EMail <> '' then
    LogLine(Format('%s <%s>', [Fullname, EMail]));
  if LastErrorCode <> '' then
    LogLine(Format('Error: %s - %s', [LastErrorCode, LastErrorMessage]));
  if EMail <> '' then
    LogLine('We now have access')
  else
    LogLine('We don''t have access');

end;

// this is used for encoding the access_token for XOAUTH2 in gmail
function TGoogleOAuth2.GetXOAuth2Base64: string;
begin
  Result := 'user=%s' + #1 + 'auth=Bearer %s' + #1 + #1;
  Result := format(Result, [EMail, Access_token]);
  Result := EncodeStringBase64(Result);
end;

procedure TGoogleOAuth2.LoadAccessRefreshTokens;
var
  FS: TFileStream;
  P: TJSONParser;
  J: TJSONData;
begin
  if FileExists(token_filename) then
  begin
    FS := TFileStream.Create(token_filename, fmOpenRead);
    P := TJSONParser.Create(FS);
    try
      J := P.Parse;
      refresh_token := RetrieveJSONValue(J, 'refresh_token');
      access_token := RetrieveJSONValue(J, 'access_token');
      LogLine('Tokens restored from ' + token_filename);
    finally
      if assigned(J) then
        J.Free;
      P.Free;
      FS.Free;
    end;
  end;
end;

procedure TGoogleOAuth2.SaveAccessRefreshTokens;
var
  J: TJSONData;
begin
  J := TJSONObject.Create(['refresh_token', refresh_token, 'access_token',
    access_token]);
  try
    with TStringList.Create do
      try
        Add(J.AsJSON);
        SaveToFile(token_filename);
        LogLine('Tokens saved to ' + token_filename);
      finally
        Free;
      end;
  finally
    J.Free;
  end;
end;

procedure TGoogleOAuth2.GetAuthorize_token_interactive;
var
  URL: string;
  Params: string;
  GoUrl: variant;
  Browser: olevariant;
  Document: olevariant;
  Body: olevariant;
  SearchFor: string;
  Found: string;
  Scope: string;
begin
  try

    Scope := FScopes.DelimitedText;
    if Scope = '' then
    begin
      LogLine('No scope specified in GetAccess');
    end;

    URL := AuthorizationUrl;
    Params := '';
    Params := Params + 'response_type=' + EncodeURLElement('code');
    Params := Params + '&client_id=' + EncodeURLElement(FClient_id);
    Params := Params + '&redirect_uri=' + EncodeURLElement(RedirectUri);
    Params := Params + '&scope=' + EncodeURLElement(Scope);

    LogLine('Authorizing...');
    GoUrl := Url + '?' + Params;
    Browser := CreateOleObject('InternetExplorer.Application');
    try
      Browser.Visible := True;
      Browser.AddressBar := False;
      Browser.Menubar := False;
      Browser.ToolBar := False;
      Browser.StatusBar := False;
      Browser.Left := Screen.Width div 2 - 300;
      Browser.Top := Screen.Height div 2 - 300;
      Browser.Width := 600;
      Browser.Height := 600;
      Browser.Navigate(GoUrl);

      if ForceManualAuth then
      begin

        Authorize_token := InputBox('Authentication code',
          'Please enter the authorization code', '');

        if Authorize_token <> '' then
          DebugLine('Authorization: We have a manual Authorize_token');

      end
      else
      begin

        Sleep(500);
        Application.ProcessMessages;
        SearchFor := 'Success code=';
        while (Pos(SearchFor, Browser.LocationName) <> 1) do
        begin
          Sleep(500);
          Application.ProcessMessages;
        end;
        DebugLine('Browser.LocationName: ' + Browser.LocationName);

        // https://developers.google.com/youtube/2.0/developers_guide_protocol_oauth2#OAuth2_Installed_Applications_Flow
        Found := Browser.LocationName;
        Authorize_token := Copy(Found, Length(SearchFor) + 1, 1000);
        DebugLine('Authorization: We have an Authorize_token from the browser-title');
        Authorize_token := 'abd';

        // always do the html stuff.
        if (Pos(SearchFor, Browser.LocationName) = 1) then // 'Success code='
        begin
          Document := Browser.Document;
          Body := Document.Body;
          Found := Body.InnerHtml;

          DebugLine('Browser catched HTML:');
          DebugLine('---------------------------------------');
          DebugLine(Found);
          DebugLine('---------------------------------------');

          // the Success code in the Browsers-title is not always complete
          // sometimes it is cut off.
          // todo: check if this is still the case

          // could have been done with RegExp
          // but this is the only place we need it
          SearchFor := 'readonly="readonly" value="';
          if Pos(SearchFor, Found) > 0 then
          begin
            System.Delete(Found, 1, Pos(SearchFor, Found) + Length(SearchFor) - 1);
            if Pos('">', Found) > 0 then
              Authorize_token_html := Copy(Found, 1, Pos('">', Found) - 1);
          end;
          if Authorize_token_html <> '' then
            DebugLine('Authorization: We have the browser-HTML text');
        end;

        if (Authorize_token = '') and (Authorize_token_html <> '') then
        begin  // Make it the main token
          Authorize_token := Authorize_token_html;
          Authorize_token_html := '';
        end;

      end;

      Browser.Quit;

    finally
      Browser := Unassigned;
    end;

  except
    // on E: EOleSysError do ;
    on E: Exception do
    begin
      DebugLine('Browser closed without confirmation.');
      DebugLine('Exception: ' + E.Message);
    end;
  end;

end;

procedure TGoogleOAuth2.GetRefresh_token;
var
  URL: string;
  Params: string;
  Response: TMemoryStream;
  P: TJSONParser;
  J, D: TJSONData;
begin
  LastErrorCode := '';
  LastErrorMessage := '';

  // If we haven't got a Authentication token we need to ask permission
  if Authorize_token = '' then
    GetAuthorize_token_interactive;
  if (Authorize_token = '') then
    exit;

  LogLine('Getting new Refresh_token');
  URL := GetTokenUrl;
  Params := '';
  Params := Params + 'code=' + EncodeURLElement(authorize_token);
  Params := Params + '&client_id=' + EncodeURLElement(FClient_id);
  Params := Params + '&client_secret=' + EncodeURLElement(FClient_secret);
  Params := Params + '&redirect_uri=' + EncodeURLElement(RedirectUri);
  Params := Params + '&grant_type=' + EncodeURLElement('authorization_code');
  Response := TMemoryStream.Create;
  try
    if HttpPostURL(URL, Params, Response) then
    begin
      Response.Position := 0;
      P := TJSONParser.Create(Response);
      try
        J := P.Parse;
        D := J.FindPath('error');
        if assigned(D) then
        begin
          LastErrorCode := RetrieveJSONValue(D, 'code');
          LastErrorMessage := RetrieveJSONValue(D, 'message');
          LogLine(Format('Error in GetRefresh_token: %s - %s',
            [LastErrorCode, LastErrorMessage]));
        end;
        refresh_token := RetrieveJSONValue(J, 'refresh_token');
        access_token := RetrieveJSONValue(J, 'access_token');
        if access_token <> '' then
          LogLine('New refresh- & access_token received');
      finally
        if assigned(J) then
          J.Free;
        P.Free;
      end;
    end;
  finally
    Response.Free;
  end;

  if (access_token = '') and (Authorize_token_html <> '') then
  begin
    LogLine('Using backup Authentication token (html)');
    Authorize_token := Authorize_token_html;
    Authorize_token_html := '';
    GetRefresh_token;
  end;

end;


procedure TGoogleOAuth2.GetAccess_token;
var
  URL: string;
  Params: string;
  Response: TMemoryStream;
  P: TJSONParser;
  J, D: TJSONData;
begin
  LastErrorCode := '';
  LastErrorMessage := '';

  // If we haven't got a refresh token we need to get one or possibly re-authenticate
  if Refresh_token = '' then
    GetRefresh_token;
  if Refresh_token = '' then
    exit;
  if Access_token <> '' then
    exit; // we already received via getrefresh_token, so we can exit

  LogLine('Getting new Access_token');
  URL := GetTokenUrl;
  Params := '';
  Params := Params + 'client_id=' + EncodeURLElement(FClient_id);
  Params := Params + '&client_secret=' + EncodeURLElement(FClient_secret);
  Params := Params + '&refresh_token=' + EncodeURLElement(refresh_token);
  Params := Params + '&grant_type=' + EncodeURLElement('refresh_token');
  Response := TMemoryStream.Create;
  try
    if HttpPostURL(URL, Params, Response) then
    begin
      Response.Position := 0;
      P := TJSONParser.Create(Response);
      try
        J := P.Parse;
        D := J.FindPath('error');
        if assigned(D) then
        begin
          LastErrorCode := RetrieveJSONValue(D, 'code');
          LastErrorMessage := RetrieveJSONValue(D, 'message');
          if LastErrorMessage = '' then
            LastErrorMessage := D.AsString;
          LogLine(Format('Error in GetAccess_token: %s - %s',
            [LastErrorCode, LastErrorMessage]));
          // haal nieuwe refresh_token
        end;
        access_token := RetrieveJSONValue(J, 'access_token');
        if access_token <> '' then
          LogLine('New access_token received');
      finally
        if assigned(J) then
          J.Free;
        P.Free;
      end;
    end;
  finally
    Response.Free;
  end;
end;

end.
