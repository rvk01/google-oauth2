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
  https://masashi-k.blogspot.nl/2013/06/sending-mail-with-gmail-using-xoauth2.html
  https://www.limilabs.com/blog/oauth2-gmail-imap-service-account


  2022-01-11 use a local server instance to retrieve authorization code instead of IE browser
  2022-01-11 UseBrowserTitle and ForceManualAuth removed
  2016-12-12 delphi compatible
  2016-12-12 using superobject for json
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
// http://calebb.net/

unit google_oauth2;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}


interface

uses
  Classes, StdCtrls, SysUtils {, fpjson, jsonparser};

type
  GoogleScope = (goMail, goContacts, goCalendar, goDrive);
  GoogleScopeSet = set of GoogleScope;

type
  TGoogleOAuth2 = class(TObject)
  private
    { private declarations }
    FClient_id: string;
    FClient_secret: string;
    FAuthorize_token: string;
    FRefresh_token: string;
    FAccess_token: string;
    FTokens_refreshed: boolean;
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
    // function RetrieveJSONValue(JSON: TJSONData; Value: string): string;

    procedure GetAccess(Scopes: GoogleScopeSet = []; UseTokenFile: boolean = False);
    function GetXOAuth2Base64: string;
    procedure LogLine(Value: string);
    procedure DebugLine(Value: string);

    property Tokens_refreshed: boolean read FTokens_refreshed write FTokens_refreshed;
    property Authorize_token: string read FAuthorize_token write FAuthorize_token;
    property Refresh_token: string read FRefresh_token write FRefresh_token;
    property Access_token: string read FAccess_token write FAccess_token;
    property LastErrorCode: string read FLastErrorCode write FLastErrorCode;
    property LastErrorMessage: string read FLastErrorMessage write FLastErrorMessage;
    property Fullname: string read FFullname write FFullname;
    property EMail: string read FEMail write FEMail;
    property LogMemo: TMemo read FLogMemo write FLogMemo;
    property DebugMemo: TMemo read FDebugMemo write FDebugMemo;

  end;

implementation

{$IFNDEF FPC}
{$DEFINE USE_SUPEROBJECT}
{$ENDIF}


uses
  synacode, synautil, httpsend, // for communication
{$WARN UNIT_DEPRECATED OFF}
  ssl_openssl, // you need to include this one in your requirements
{$WARN UNIT_DEPRECATED ON}
  comobj, // for ceating Browser-object
  // ActiveX, // CoInitialize
  Variants,
  Dialogs, // for inputbox
  Forms, // for Screen.Width/Height
  blcksock,
  // base64, // for the XOAuth2 token, we use synapse now
{$IFDEF USE_SUPEROBJECT} superobject {$ELSE} fpjson, jsonparser {$ENDIF},
  Shellapi,
  Windows;

const
  token_filename = 'tokens.dat';
  GetTokenUrl = 'https://accounts.google.com/o/oauth2/token';
  AuthorizationUrl = 'https://accounts.google.com/o/oauth2/auth';
  // RedirectUri = 'urn:ietf:wg:oauth:2.0:oob';
  RedirectUri = 'http://localhost:1500';

{$IFNDEF FPC}

  // we re-declare this one with string so Delphi doesn't give hints about string-conversion
function EncodeURLElement(const Value: String): String;
begin
  Result := string(EncodeTriplet(ansistring(Value), '%', URLSpecialChar + URLFullSpecialChar));
end;
{$ENDIF}

{$IFDEF USE_SUPEROBJECT}


function RetrieveJSONValue(JSonString, Key: string; FromArray: String = ''; Index: Integer = 0): string;
var
  obj: ISuperObject;
begin
  Result := '';
  obj := SO(JSonString);
  if FromArray = '' then
  begin
    // if obj.AsObject.Exists(Key) then Result := obj.S[Key];
    Result := obj.S[Key];
  end
  else
  begin
    if obj.AsObject.Exists(FromArray) then
      if obj.A[FromArray].O[Index].AsObject.Exists(Key) then
          Result := obj.A[FromArray].O[Index].S[Key];
  end;
  Result := AnsiDequotedStr(Result, '"');
end;
{$ELSE}

function RetrieveJSONValue(JSonString, Key: string; FromArray: string = ''; Index: integer = 0): string;
var
  P: TJSONParser;
  J, D, L: TJSONData;
  Key1, Key2: string;
begin
  Result := '';
  Key1 := Key;
  Key2 := '';
  if Pos('.', Key1) > 0 then
  begin
    Key2 := Copy(Key1, Pos('.', Key1) + 1);
    Key1 := Copy(Key1, 1, Pos('.', Key1) - 1);
  end;
  P := TJSONParser.Create(JSonString);
  try
    J := P.Parse;
    if Assigned(J) then
    begin
      if FromArray <> '' then
      begin
        D := J.FindPath(FromArray);
        if Assigned(D) and (D.Count > 0) then
        begin
          D := D.Items[Index];
          L := D.FindPath(Key1);
          if assigned(L) then
            Result := L.AsString;
        end;
      end
      else
      begin
        if Key2 <> '' then
        begin
          D := J.FindPath(Key1);
          if Assigned(D) then
          begin
            L := D.FindPath(Key2);
            if assigned(L) then
              Result := L.AsString;
          end;
        end
        else
        begin
          D := J.FindPath(Key1);
          if Assigned(D) then
            Result := D.AsString;
        end;
      end;
    end;
  finally
    //if Assigned(L) then L.Free;
    //if Assigned(D) then D.Free;
    if Assigned(J) then J.Free;
    P.Free;
  end;
end;

{$ENDIF}


constructor TGoogleOAuth2.Create(client_id, client_secret: string);
begin
  inherited Create;
  FClient_id := client_id;
  FClient_secret := client_secret;
  FAuthorize_token := '';
  FRefresh_token := '';
  FAccess_token := '';
  FTokens_refreshed := False;
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
{$IFDEF DEBUG}
  // Showmessage(Value);
{$ENDIF}
  if DebugMemo <> nil then
      DebugMemo.Lines.Add(Value);
end;

procedure TGoogleOAuth2.GetAccess(Scopes: GoogleScopeSet = []; UseTokenFile: boolean = False);

// tr '._-' '\n/+' | sed '2s|$|===|p;d' | base64 -D

  procedure GetInformation;
  var
    URL: string;
    Params: string;
    Response: TStringList;
    JSonStr: String;
  begin
    URL := 'https://www.googleapis.com/oauth2/v3/userinfo';
    Params := 'access_token=' + Access_token;
    Response := TStringList.Create;
    try
      if HttpGetText(URL + '?' + Params, Response) then
      begin
        JSonStr := Response.Text;
        DebugLine(JSonStr);
        LastErrorCode := RetrieveJSONValue(JSonStr, 'error.code');
        LastErrorMessage := RetrieveJSONValue(JSonStr, 'error.message');
        if LastErrorCode <> '' then
            LogLine(Format('Error in GetRefresh_token: %s - %s', [LastErrorCode, LastErrorMessage]));

        Fullname := RetrieveJSONValue(JSonStr, 'name');
        EMail := RetrieveJSONValue(JSonStr, 'email');

      end;

    finally
        Response.Free;
    end;
  end;

  procedure GetInformation_oud_viaGooglePlus_not_used_anymore;
  var
    URL: string;
    Params: string;
    Response: TStringList;
    JSonStr: String;
  begin
    URL := 'https://www.googleapis.com/plus/v1/people/me';
    Params := 'access_token=' + Access_token;
    Response := TStringList.Create;
    try
      if HttpGetText(URL + '?' + Params, Response) then
      begin
        JSonStr := Response.Text;
        LastErrorCode := RetrieveJSONValue(JSonStr, 'error.code');
        LastErrorMessage := RetrieveJSONValue(JSonStr, 'error.message');
        if LastErrorCode <> '' then
            LogLine(Format('Error in GetRefresh_token: %s - %s', [LastErrorCode, LastErrorMessage]));

        Fullname := RetrieveJSONValue(JSonStr, 'displayName');
        EMail := RetrieveJSONValue(JSonStr, 'value', 'emails', 0);

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

  FScopes.Add('profile'); // https://www.googleapis.com/auth/userinfo.profile
  FScopes.Add('email'); // https://www.googleapis.com/auth/userinfo.email
  // always use profile/email to find the full name and email

  // https://mail.google.com/
  // https://www.googleapis.com/auth/gmail.modify
  // https://www.googleapis.com/auth/gmail.compose

  // The scope for IMAP and SMTP access is https://mail.google.com/.
  // https://stackoverflow.com/questions/40881026/reduced-scope-for-gmail-imap-access

  if goMail in Scopes then FScopes.Add('https://mail.google.com/');
  // if goMail in Scopes then FScopes.Add('https://www.googleapis.com/auth/gmail.compose');
  if goContacts in Scopes then FScopes.Add('https://www.google.com/m8/feeds/');
  if goCalendar in Scopes then FScopes.Add('https://www.googleapis.com/auth/calendar');
  if goDrive in Scopes then FScopes.Add('https://www.googleapis.com/auth/drive');

  if UseTokenFile then LoadAccessRefreshTokens
  else
  begin
    // LogLine('If you had an access_token please set it before calling GetAccess');
  end;

  Fullname := '';
  EMail := '';
  if Access_token <> '' then
  begin
    LogLine('Getting account information');
    GetInformation;
  end;
  if (EMail = '') or (LastErrorCode <> '') or (Access_token = '') then
  begin
    if Access_token <> '' then
    begin
      LogLine(Format('Error: %s - %s', [LastErrorCode, LastErrorMessage]));
      LogLine(Format('Invalid access_token %s', [Access_token]));
      Access_token := ''; // <- invalidate
    end;
    GetAccess_token;
    if Access_token <> '' then
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
  Result := Format(Result, [EMail, Access_token]);
  // Result := SynaCode.EncodeStringBase64(Result);
  Result := string(synacode.EncodeBase64(ansistring(Result)));
end;

{$IFDEF USE_SUPEROBJECT}


procedure TGoogleOAuth2.LoadAccessRefreshTokens;
var
  JSON: ISuperObject;
begin
  try
    JSON := TSuperObject.ParseFile(token_filename, True);
    Refresh_token := JSON.S['refresh_token'];
    Access_token := JSON.S['access_token'];
  finally
      JSON := nil;
  end;
end;

procedure TGoogleOAuth2.SaveAccessRefreshTokens;
var
  JSON: ISuperObject;
begin
  try
    JSON := SO;
    JSON.S['refresh_token'] := Refresh_token;
    JSON.S['access_token'] := Access_token;
    JSON.SaveTo(token_filename);
  finally
      JSON := nil;
  end;
end;

{$ELSE}


procedure TGoogleOAuth2.LoadAccessRefreshTokens;
var
  FS: TFileStream;
  P: TJSONParser;
  J, D: TJSONData;
begin
  if FileExists(token_filename) then
  begin
    FS := TFileStream.Create(token_filename, fmOpenRead);
    P := TJSONParser.Create(FS);
    try
      J := P.Parse;
      D := J.FindPath('refresh_token');
      if assigned(D) then refresh_token := D.AsString;
      D := J.FindPath('access_token');
      if assigned(D) then access_token := D.AsString;
      LogLine('Tokens restored from ' + token_filename);
    finally
      // if assigned(D) then D.Free;
      if assigned(J) then J.Free;
      P.Free;
      FS.Free;
    end;
  end;
end;

procedure TGoogleOAuth2.SaveAccessRefreshTokens;
var
  J: TJSONData;
begin
  J := TJSONObject.Create(['refresh_token', Refresh_token, 'access_token', Access_token]);
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
{$ENDIF}


type
  THTTPServerThread = class(TThread)
  private
    ListenerSocket: TTCPBlockSocket;
    ConnectionSocket: TTCPBlockSocket;
  public
    Authorize_token: String;
    procedure Execute; override;
    procedure CancelThread(Sender: TObject; var CanClose: boolean);
  end;

procedure THTTPServerThread.CancelThread(Sender: TObject; var CanClose: boolean);
begin
  Terminate;
end;

procedure THTTPServerThread.Execute;
var
  S: string;
  method, uri, protocol: string;
  OutputDataString: string;
  SendDataString: string;
begin
  Authorize_token := '';
  FreeOnTerminate := False;
  ListenerSocket := TTCPBlockSocket.Create;
  ConnectionSocket := TTCPBlockSocket.Create;

  try
    ListenerSocket.CreateSocket;
    ListenerSocket.setLinger(True, 10);
    ListenerSocket.Bind('localhost', '1500');
    ListenerSocket.Listen;
    while not terminated do
    begin
      Sleep(1000);
      // Application.ProcessMessages;
      if ListenerSocket.CanRead(1000) and not terminated then
      begin
        ConnectionSocket.Socket := ListenerSocket.Accept;

        // read request line
        S := string(ConnectionSocket.RecvString(1000));
        method := fetch(S, ' ');
        uri := fetch(S, ' ');
        protocol := fetch(S, ' ');

        // read request headers
        repeat
            S := string(ConnectionSocket.RecvString(1000));
        until S = '';

        // /?code=4/fegArZQDUJqFdoCw-1DU16ohYsoA5116feRuCW0LiuQ
        // /?error=access_denied
        Authorize_token := '';
        if Pos('code=', uri) > 0 then
        begin
          Authorize_token := Copy(uri, Pos('code=', uri) + 5);
        end;

        if Authorize_token = '' then
        begin
          SendDataString :=
            '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'
            + ' "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">' + CRLF
            + '<html><center><h1>Something went wrong.<br><br>Application does not have access.<br><br>You can close this page.</h1></center></html>' + CRLF;
        end
        else
        begin
          SendDataString :=
            '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'
            + ' "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">' + CRLF
            + '<html><center><h1>Application now has access.<br><br>You can close this page.</h1></center></html>' + CRLF;
        end;

        OutputDataString := 'HTTP/1.0 200' + CRLF;
        OutputDataString := OutputDataString + 'Content-type: Text/Html' + CRLF;
        OutputDataString := OutputDataString + 'Content-length: ' + IntTostr(Length(SendDataString)) + CRLF;
        OutputDataString := OutputDataString + 'Connection: close' + CRLF;
        OutputDataString := OutputDataString + 'Date: ' + Rfc822DateTime(now) + CRLF;
        OutputDataString := OutputDataString + 'Server: Synapse' + CRLF;
        OutputDataString := OutputDataString + '' + CRLF;
        ConnectionSocket.SendString(ansistring(OutputDataString));
        ConnectionSocket.SendString(ansistring(SendDataString));

        ConnectionSocket.CloseSocket;

        Terminate;
      end;
    end;

  finally
    ConnectionSocket.Free;
    ListenerSocket.Free;
  end;

end;

procedure TGoogleOAuth2.GetAuthorize_token_interactive;
var
  URL: string;
  Params: string;
  GoUrl: variant;
  Scope: string;
  ServerThread: THTTPServerThread;
  dl: TForm;

  function StartBrowser(const FileName: string): boolean;
  begin
    Result := Shellapi.ShellExecute(0, nil, pchar(FileName), nil, nil, 5 { SW_SHOW } ) > 32;
  end;

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
    GoUrl := URL + '?' + Params;

    ServerThread := THTTPServerThread.Create(False);
    try
      StartBrowser(GoUrl); // open website

      dl := CreateMessageDialog('Waiting for permission', mtInformation, []);
      try

        dl.Height := Round(80 * (dl.PixelsPerInch / 96));
        dl.OnCloseQuery := ServerThread.CancelThread;
        dl.Top := 38;
        dl.Left := 5;
        dl.Show;
        dl.Repaint;

        while not ServerThread.terminated do
        begin
          Sleep(1);
          Application.ProcessMessages;
        end;

        ServerThread.WaitFor; // blocking met dialog

      finally
          dl.Free;
      end;

      Authorize_token := ServerThread.Authorize_token;
    finally
        ServerThread.Free;
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
  JSonStr: String;
begin
  LastErrorCode := '';
  LastErrorMessage := '';

  // If we haven't got a Authentication token we need to ask permission
  if Authorize_token = '' then GetAuthorize_token_interactive;
  if (Authorize_token = '') then exit;

  LogLine('Getting new Refresh_token');
  URL := GetTokenUrl;
  Params := '';
  Params := Params + 'code=' + EncodeURLElement(Authorize_token);
  Params := Params + '&client_id=' + EncodeURLElement(FClient_id);
  Params := Params + '&client_secret=' + EncodeURLElement(FClient_secret);
  Params := Params + '&redirect_uri=' + EncodeURLElement(RedirectUri);
  Params := Params + '&grant_type=' + EncodeURLElement('authorization_code');
  Response := TMemoryStream.Create;
  try
    if HttpPostURL(URL, Params, Response) then
    begin
      Response.Position := 0;
      JSonStr := string(PansiChar(Response.Memory));
      DebugLine(JSonStr);
      LastErrorCode := RetrieveJSONValue(JSonStr, 'error.code');
      LastErrorMessage := RetrieveJSONValue(JSonStr, 'error.message');
      if LastErrorCode <> '' then
          LogLine(Format('Error in GetRefresh_token: %s - %s', [LastErrorCode, LastErrorMessage]));
      Refresh_token := RetrieveJSONValue(JSonStr, 'refresh_token');
      Access_token := RetrieveJSONValue(JSonStr, 'access_token');
      if Access_token <> '' then
          LogLine(Format('New refresh- & access_token received (%s, %s)', [Refresh_token, Access_token]));
    end;
  finally
      Response.Free;
  end;

end;

procedure TGoogleOAuth2.GetAccess_token;
var
  URL: string;
  Params: string;
  Response: TMemoryStream;
  JSonStr: String;
begin
  LastErrorCode := '';
  LastErrorMessage := '';

  // If we haven't got a refresh token we need to get one or possibly re-authenticate
  if Refresh_token = '' then GetRefresh_token;
  if Refresh_token = '' then exit;
  if Access_token <> '' then exit; // we already received via getrefresh_token, so we can exit

  LogLine('Getting new Access_token');
  URL := GetTokenUrl;
  Params := '';
  Params := Params + 'client_id=' + EncodeURLElement(FClient_id);
  Params := Params + '&client_secret=' + EncodeURLElement(FClient_secret);
  Params := Params + '&refresh_token=' + EncodeURLElement(Refresh_token);
  Params := Params + '&grant_type=' + EncodeURLElement('refresh_token');
  Response := TMemoryStream.Create;
  try
    if HttpPostURL(URL, string(Params), Response) then
    begin
      Response.Position := 0;
      JSonStr := string(PansiChar(Response.Memory));
      DebugLine(JSonStr);
      LastErrorCode := RetrieveJSONValue(JSonStr, 'error.code');
      LastErrorMessage := RetrieveJSONValue(JSonStr, 'error.message');
      if LastErrorCode <> '' then
          LogLine(Format('Error in GetRefresh_token: %s - %s', [LastErrorCode, LastErrorMessage]));
      Access_token := RetrieveJSONValue(JSonStr, 'access_token');
      if Access_token <> '' then
          LogLine(Format('New access_token received (%s)', [Access_token]));
    end;
  finally
      Response.Free;
  end;
end;

end.
