unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckGroup1: TCheckGroup;
    edBody: TMemo;
    edRecipient: TEdit;
    edSender: TEdit;
    edSubject: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses google_oauth2,
  blcksock, smtpsend;

{$R *.lfm}

{ TForm1 }

const
  client_id = '896304839415-nnl5e0smrtakhr9r2l3bno0tes2mrtgk.apps.googleusercontent.com';
  client_secret = 'dUahHDn3IMyhCIk3qD4tf8E_';

procedure TForm1.FormCreate(Sender: TObject);
begin
  if CheckGroup1.Items.Count > 2 then
  begin
    CheckGroup1.Checked[0] := True;
    CheckGroup1.Checked[1] := True;
    CheckGroup1.Checked[2] := True;
    CheckGroup1.CheckEnabled[0] := false;
    CheckGroup1.CheckEnabled[1] := false;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  gApi: TGoogleOAuth2;
  Scopes: GoogleScopeSet;
begin
  // Onetime authentication
  // Save tokens to token.dat
  gApi := TGoogleOAuth2.Create(client_id, client_secret);
  gApi.DebugMemo := Memo1;

  Scopes := [];
  if CheckGroup1.Checked[2] then Include(Scopes, goMail);
  if CheckGroup1.Checked[3] then Include(Scopes, goCalendar);
  if CheckGroup1.Checked[4] then Include(Scopes, goContacts);
  if CheckGroup1.Checked[5] then Include(Scopes, goDrive);

  gApi.GetAccess(Scopes, True); // <- get from file

  if gApi.EMail <> '' then
    edSender.Text := format('%s <%s>', [gApi.FullName, gApi.EMail]);

end;

procedure AddToLog(Str: string);
begin
  Form1.Memo1.Lines.Add(Str);
end;


// -----------------------------------------------------
// Little hack for TSMTPSend to give the command XOAUTH2
// -----------------------------------------------------

type
  TmySMTPSend = class helper for TSMTPSend
  public
    function DoXOAuth2(const Value: string): boolean;
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

// -----------------------------------------------------
// -----------------------------------------------------

procedure TForm1.Button2Click(Sender: TObject);
var
  gApi: TGoogleOAuth2;
  smtp: TSMTPSend;
  msg_lines: TStringList;
begin
  if (edRecipient.Text = '') or (edRecipient.Text = 'recipient@valid_domain.com') then
  begin
    Memo1.Lines.Add('Please change the recipient');
    exit;
  end;

  gApi := TGoogleOAuth2.Create(client_id, client_secret);
  smtp := TSMTPSend.Create;
  msg_lines := TStringList.Create;
  try
    Button2.Enabled := False;

    // first get oauthToken
    gApi.DebugMemo := Memo1;
    gApi.GetAccess([goMail], True); // <- get from file
    if gApi.EMail = '' then
      exit;

    edSender.Text := format('%s <%s>', [gApi.FullName, gApi.EMail]);

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
    if not smtp.DoXOAuth2(gApi.GetXOAuth2Base64) then
    begin
      AddToLog('XOAUTH2 ERROR');
      exit;
    end;

    AddToLog('SMTP Mail');
    if not smtp.MailFrom(gApi.EMail, Length(gApi.EMail)) then
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
    gApi.Free;
    smtp.Free;
    msg_lines.Free;
    Button2.Enabled := True;
  end;

end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if not FileExists('tokens.dat') then
  begin
    AddToLog('tokens.dat didn''t exist');
    exit;
  end;

  Deletefile('tokens.dat');

  if not FileExists('tokens.dat') then
    AddToLog('tokens.dat deleted')
  else
    AddToLog('error while removing tokens.dat');

end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Memo1.Clear;
end;

end.
