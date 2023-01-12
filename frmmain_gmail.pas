unit frmMain_GMail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, blcksock, fpjson, jsonConf;

type

  { TMainform }

  TMainform = class(TForm)
    btGetAccess: TButton;
    btSendMail: TButton;
    btRemoveTokens: TButton;
    btClearLog: TButton;
    btClearDebug: TButton;
    CheckGroup1: TCheckGroup;

    edBody: TMemo;
    edRecipient: TEdit;
    edSender: TEdit;
    edSubject: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PageControl6: TPageControl;
    Memo1: TMemo;
    Memo2: TMemo;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Panel1: TPanel;
    TabSheet1: TTabSheet;
    TabSheet14: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet8: TTabSheet;
    procedure btGetAccessClick(Sender: TObject);
    procedure btSendMailClick(Sender: TObject);
    procedure btRemoveTokensClick(Sender: TObject);
    procedure btClearLogClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  protected
  public
    { public declarations }
    procedure AddToLog(Str: string);
    procedure CheckTokenFile;
  end;

var
  Mainform: TMainform;


implementation

uses
  google_oauth2,
  smtpsend,
  synautil;

{$R *.lfm}

{ TMainform }

var
  client_id: string = '504681931309-gc0n3bqtr0dgp6se1d7ee6pcean7heho.apps.googleusercontent.com';
  client_secret: string = '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'; // only valid for my own test-user ( 2023-01-12 )

procedure TMainform.AddToLog(Str: string);
begin
  Memo1.Lines.Add(Str);
end;

procedure TMainform.CheckTokenFile;
begin

  if FileExists('tokens.dat') then // already tokens
  begin
    CheckGroup1.Enabled := False;
    CheckGroup1.Caption := 'Access (scope)             remove tokens.dat first to get new access';
    btGetAccess.Caption := 'Check access';
  end
  else
  begin
    CheckGroup1.Enabled := True;
    CheckGroup1.Caption := 'Access (scope)';
    btGetAccess.Caption := 'Get access';
  end;

end;

procedure TMainform.FormCreate(Sender: TObject);
var
  Cfg: TJSONConfig;
begin

  Memo1.Clear;
  Memo2.Clear;

  Cfg := TJSONConfig.Create(nil);
  try
    cfg.Filename:= 'client.json';
    client_id := cfg.GetValue('installed/client_id', client_id);
    client_secret := cfg.GetValue('installed/client_secret', client_secret);
  finally
    Cfg.Free;
  end;

  if Pos('504681931309', client_id) = 1 then // default client_id
  begin
    AddToLog('Using client_id from sourcecode (' + client_id + ')');
    AddToLog('You need to create your own project and download the client.json');
    AddToLog('See README.md for information');
  end
  else
  begin
    AddToLog('Using client_id from file client.json (' + client_id + ')');
  end;

  Width := round(Screen.Width * 0.6);
  Height := round(Screen.Height * 0.9) - 100;
  Top := 100;

  CheckGroup1.Checked[0] := True;
  CheckGroup1.Checked[1] := True;
  CheckGroup1.Checked[2] := True;
  CheckGroup1.CheckEnabled[0] := False;
  CheckGroup1.CheckEnabled[1] := False;

  PageControl1.ActivePageIndex := 0;

  CheckTokenFile;

end;

procedure TMainform.FormDestroy(Sender: TObject);
begin
end;

procedure TMainform.btGetAccessClick(Sender: TObject);
var
  gOAuth2: TGoogleOAuth2;
  Scopes: GoogleScopeSet;
begin
  // Onetime authentication
  // Save tokens to tokens.dat
  gOAuth2 := TGoogleOAuth2.Create(client_id, client_secret);
  try

    Scopes := [];
    if CheckGroup1.Checked[2] then Include(Scopes, goMail);
    if CheckGroup1.Checked[3] then Include(Scopes, goContacts);
    if CheckGroup1.Checked[4] then Include(Scopes, goCalendar);
    if CheckGroup1.Checked[5] then Include(Scopes, goDrive);

    gOAuth2.LogMemo := Memo1;
    gOAuth2.DebugMemo := Memo2;
    gOAuth2.GetAccess(Scopes, True); // <- get from file

    if gOAuth2.EMail <> '' then
    begin
      edSender.Text := format('%s <%s>', [gOAuth2.FullName, gOAuth2.EMail]);
      if (edRecipient.Text = '') or (edRecipient.Text = 'recipient@valid_domain.com') then
        edRecipient.Text := format('%s', [gOAuth2.EMail]);
    end;

    CheckTokenFile;

  finally
    gOAuth2.Free;
  end;

end;


procedure TMainform.btRemoveTokensClick(Sender: TObject);
begin
  if not FileExists('tokens.dat') then
  begin
    AddToLog('tokens.dat does not exist');
    exit;
  end;

  Deletefile('tokens.dat');

  if not FileExists('tokens.dat') then
    AddToLog('tokens.dat deleted')
  else
    AddToLog('error while removing tokens.dat');

  CheckTokenFile;

end;

// -----------------------------------------------------
// Little hack for TSMTPSend to give the command XOAUTH2
// -----------------------------------------------------

type
  TmySMTPSend = class helper for TSMTPSend
  public
    function DoXOAuth2(const Value: string): boolean;
    function ChallengeError(): string;
  end;


function TmySMTPSend.DoXOAuth2(const Value: string): boolean;
var
  x: integer;
  s: string;
begin
  Sock.SendString('AUTH XOAUTH2 ' + Value + CRLF);
  repeat
    s := Sock.RecvString(FTimeout);
    if Sock.LastError <> 0 then
      Break;
  until Pos('-', s) <> 4;
  x := StrToIntDef(Copy(s, 1, 3), 0);
  Result := (x = 235);
end;

function TmySMTPSend.ChallengeError(): string;
var
  s: string;
begin
  Result := '';
  Sock.SendString('' + CRLF);
  repeat
    s := Sock.RecvString(FTimeout);
    if Sock.LastError <> 0 then
      Break;
    if Result <> '' then
      Result := Result + CRLF;
    Result := Result + s;
  until Pos('-', s) <> 4;
end;

// -----------------------------------------------------
// -----------------------------------------------------

procedure TMainform.btSendMailClick(Sender: TObject);
var
  gOAuth2: TGoogleOAuth2;
  smtp: TSMTPSend;
  msg_lines: TStringList;
begin
  if (edRecipient.Text = '') or (edRecipient.Text = 'recipient@valid_domain.com') then
  begin
    Memo1.Lines.Add('Please change the recipient');
    exit;
  end;

  if not FileExists('tokens.dat') then
  begin
    // first get all access clicked on Groupbox
    btGetAccess.Click;
  end;

  gOAuth2 := TGoogleOAuth2.Create(client_id, client_secret);
  smtp := TSMTPSend.Create;
  msg_lines := TStringList.Create;
  try
    btSendMail.Enabled := False;

    // first get oauthToken
    gOAuth2.LogMemo := Memo1;
    gOAuth2.DebugMemo := Memo2;
    gOAuth2.GetAccess([], True); // <- get from file
    // no need for scope because we should already have access
    // via the btGetAccess for all the scopes in Groupbox
    if gOAuth2.EMail = '' then
      exit;

    CheckTokenFile;

    edSender.Text := format('%s <%s>', [gOAuth2.FullName, gOAuth2.EMail]);

    msg_lines.Add('From: ' + edSender.Text);
    msg_lines.Add('To: ' + edRecipient.Text);
    msg_lines.Add('Subject: ' + edSubject.Text);
    msg_lines.Add('');
    msg_lines.Add(edBody.Text);

    smtp.TargetHost := 'smtp.gmail.com';
    smtp.TargetPort := '587';

    AddToLog('SMTP Login');
    if not smtp.Login() then
    begin
      AddToLog('SMTP ERROR: Login:' + smtp.EnhCodeString);
      exit;
    end;
    if not smtp.StartTLS() then
    begin
      AddToLog('SMTP ERROR: StartTLS:' + smtp.EnhCodeString);
      exit;
    end;

    AddToLog('XOAUTH2');
    if not smtp.DoXOAuth2(gOAuth2.GetXOAuth2Base64) then
    begin
      AddToLog('XOAUTH2 ERROR: ' + CRLF + smtp.ChallengeError());
      exit;
    end;

    AddToLog('SMTP Mail');
    if not smtp.MailFrom(gOAuth2.EMail, Length(gOAuth2.EMail)) then
    begin
      AddToLog('SMTP ERROR: MailFrom:' + smtp.EnhCodeString);
      exit;
    end;
    if not smtp.MailTo(edRecipient.Text) then
    begin
      AddToLog('SMTP ERROR: MailTo:' + smtp.EnhCodeString);
      exit;
    end;
    if not smtp.MailData(msg_lines) then
    begin
      AddToLog('SMTP ERROR: MailData:' + smtp.EnhCodeString);
      exit;
    end;

    AddToLog('SMTP Logout');
    if not smtp.Logout() then
    begin
      AddToLog('SMTP ERROR: Logout:' + smtp.EnhCodeString);
      exit;
    end;

    AddToLog('OK !');

  finally
    gOAuth2.Free;
    smtp.Free;
    msg_lines.Free;
    btSendMail.Enabled := True;
  end;

end;

procedure TMainform.btClearLogClick(Sender: TObject);
begin
  Memo1.Clear;
end;

end.
