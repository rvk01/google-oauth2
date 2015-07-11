unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Grids;

type

  { TMainform }

  TMainform = class(TForm)
    btGetAccess: TButton;
    btSendMail: TButton;
    btRemoveTokens: TButton;
    btClearLog: TButton;
    Button5: TButton;
    btGetAppointments: TButton;
    btClearDebug: TButton;
    Button8: TButton;
    btGetContacts: TButton;
    CheckGroup1: TCheckGroup;

    ckForceManualAuth: TCheckBox;
    ckUseBrowserTitle: TCheckBox;


    edBody: TMemo;
    Edit1: TEdit;
    edRecipient: TEdit;
    edSender: TEdit;
    edSubject: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    PageControl3: TPageControl;
    PageControl4: TPageControl;
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet10: TTabSheet;
    TabSheet11: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    procedure btGetAccessClick(Sender: TObject);
    procedure btSendMailClick(Sender: TObject);
    procedure btRemoveTokensClick(Sender: TObject);
    procedure btClearLogClick(Sender: TObject);
    procedure btGetAppointmentsClick(Sender: TObject);
    procedure btClearDebugClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
  private
    { private declarations }
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
  google_calendar,
  blcksock, smtpsend,
  Windows,
  comobj;

{$R *.lfm}

{ TMainform }

const
  client_id = '896304839415-nnl5e0smrtakhr9r2l3bno0tes2mrtgk.apps.googleusercontent.com';
  client_secret = 'dUahHDn3IMyhCIk3qD4tf8E_';

procedure TMainform.AddToLog(Str: string);
begin
  Memo1.Lines.Add(Str);
end;

procedure TMainform.CheckTokenFile;
begin

  if FileExists('tokens.dat') then // already tokens
  begin
    CheckGroup1.Enabled := False;
    CheckGroup1.Caption :=
      'Access (scope)             remove tokens.dat first to get new access';
    btGetAccess.Caption := 'Check access';
    ckForceManualAuth.Enabled := False;
    ckUseBrowserTitle.Enabled := False;
  end
  else
  begin
    CheckGroup1.Enabled := True;
    CheckGroup1.Caption := 'Access (scope)';
    btGetAccess.Caption := 'Get access';
    ckForceManualAuth.Enabled := True;
    ckUseBrowserTitle.Enabled := True;
  end;

end;

procedure TMainform.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
  Memo2.Clear;

  //Left := (Screen.Width - round(Screen.Width * 0.8)) div 2;
  //Top := (Screen.Height - round(Screen.Height * 0.8)) div 2;
  Width := round(Screen.Width * 0.6);
  Height := round(Screen.Height * 0.9) - 100;
  Top := 100;

  ckForceManualAuth.Checked := False;
  ckUseBrowserTitle.Checked := True;

  if CheckGroup1.Items.Count > 2 then
  begin
    CheckGroup1.Checked[0] := True;
    CheckGroup1.Checked[1] := True;
    CheckGroup1.Checked[2] := True;
    CheckGroup1.CheckEnabled[0] := False;
    CheckGroup1.CheckEnabled[1] := False;
  end;

  PageControl1.ActivePageIndex := 0;

  CheckTokenFile;

end;

procedure TMainform.StringGrid1DblClick(Sender: TObject);
var
  Browser: olevariant;
  GoUrl: variant;
begin

  GoUrl := '';
  with TStringGrid(Sender) do
    GoUrl := Cells[4, Row];
  if Pos('https://', GoUrl) = 0 then exit;

  Browser := CreateOleObject('InternetExplorer.Application');
  Browser.Visible := True;
  Browser.AddressBar := False;
  Browser.Menubar := False;
  Browser.ToolBar := False;
  Browser.StatusBar := False;
  Browser.Left := (Screen.Width - round(Screen.Width * 0.8)) div 2;
  Browser.Top := (Screen.Height - round(Screen.Height * 0.8)) div 2;
  Browser.Width := round(Screen.Width * 0.8);
  Browser.Height := round(Screen.Height * 0.8);
  Browser.Navigate(GoUrl);

end;

procedure TMainform.btGetAccessClick(Sender: TObject);
var
  gOAuth2: TGoogleOAuth2;
  Scopes: GoogleScopeSet;
begin
  // Onetime authentication
  // Save tokens to token.dat
  gOAuth2 := TGoogleOAuth2.Create(client_id, client_secret);
  try

    Scopes := [];
    if CheckGroup1.Checked[2] then
      Include(Scopes, goMail);
    if CheckGroup1.Checked[3] then
      Include(Scopes, goContacts);
    if CheckGroup1.Checked[4] then
      Include(Scopes, goCalendar);
    if CheckGroup1.Checked[5] then
      Include(Scopes, goDrive);

    gOAuth2.LogMemo := Memo1;
    gOAuth2.DebugMemo := Memo2;
    gOAuth2.ForceManualAuth := ckForceManualAuth.Checked;
    gOAuth2.UseBrowserTitle := ckUseBrowserTitle.Checked;
    gOAuth2.GetAccess(Scopes, True); // <- get from file

    if gOAuth2.EMail <> '' then
      edSender.Text := format('%s <%s>', [gOAuth2.FullName, gOAuth2.EMail]);

    CheckTokenFile;

  finally
    gOAuth2.Free;
  end;

end;

procedure TMainform.btRemoveTokensClick(Sender: TObject);
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
    gOAuth2.ForceManualAuth := ckForceManualAuth.Checked;
    gOAuth2.UseBrowserTitle := ckUseBrowserTitle.Checked;
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

// Bubblesort Integer

const
  // Define the Separator
  TheSeparator = #254;

procedure BubbleSort_int(Items: TStrings);
var
  done: boolean;
  ThePosition, ThePosition2, i, n: integer;
  TempString, TempString2, MyString, Mystring2, Dummy: string;
begin
  n := Items.Count;
  repeat
    done := True;
    for i := 0 to n - 2 do
    begin
      MyString := items[i];
      MyString2 := items[i + 1];
      ThePosition := Pos(TheSeparator, MyString);
      ThePosition2 := Pos(TheSeparator, MyString2);
      TempString := Copy(MyString, 1, ThePosition);
      TempString2 := Copy(MyString2, 1, ThePosition2);
      if AnsiCompareText(TempString, TempString2) < 0 then
      begin
        Dummy := Items[i];
        Items[i] := Items[i + 1];
        Items[i + 1] := Dummy;
        done := False;
      end;
    end;
  until done;
end;

procedure SortStringGrid(var GenStrGrid: TStringGrid; ThatCol: integer);
var
  CountItem, I, J, K, ThePosition: integer;
  MyList: TStringList;
  MyString, TempString: string;
begin
  // Give the number of rows in the StringGrid
  CountItem := GenStrGrid.RowCount;
  //Create the List
  MyList := TStringList.Create;
  MyList.Sorted := False;
  try
    begin
      for I := 1 to (CountItem - 1) do
        MyList.Add(GenStrGrid.Rows[I].Strings[ThatCol] + TheSeparator +
          GenStrGrid.Rows[I].Text);
      //Sort the List
      //Mylist.Sort; INSTEAD
      BubbleSort_int(Mylist);

      for K := 1 to Mylist.Count do
      begin
        //Take the String of the line (K – 1)
        MyString := MyList.Strings[(K - 1)];
        //Find the position of the Separator in the String
        ThePosition := Pos(TheSeparator, MyString);
        TempString := '';
        {Eliminate the Text of the column on which we have sorted the StringGrid}
        TempString := Copy(MyString, (ThePosition + 1), Length(MyString));
        MyList.Strings[(K - 1)] := '';
        MyList.Strings[(K - 1)] := TempString;
      end;

      // Refill the StringGrid
      for J := 1 to (CountItem - 1) do
        GenStrGrid.Rows[J].Text := MyList.Strings[(J - 1)];
    end;
  finally
    //Free the List
    MyList.Free;
  end;
end;


procedure TMainform.btGetAppointmentsClick(Sender: TObject);
var
  Response: TStringList;
  Q: integer;
  StartDt: string;
  EndDt: string;
  nwWidth: integer;

var
  ds: TGoogleCalendar;

begin
  Response := TStringList.Create;
  ds := TGoogleCalendar.Create(Self, client_id, client_secret);
  try
    btGetAppointments.Enabled := False;

    ds.gOAuth2.LogMemo := Memo1;
    ds.gOAuth2.DebugMemo := Memo2;
    ds.gOAuth2.ForceManualAuth := ckForceManualAuth.Checked;
    ds.gOAuth2.UseBrowserTitle := ckUseBrowserTitle.Checked;
    ds.gOAuth2.GetAccess([goCalendar], True);

    CheckTokenFile;

    if ds.gOAuth2.EMail = '' then
      exit;

    ds.Open;
    ds.Populate();

    StringGrid1.Options := StringGrid1.Options + [goRowSelect];
    StringGrid1.ColCount := 5;
    StringGrid1.RowCount := 2;
    StringGrid1.Cells[1, 0] := 'Start';
    StringGrid1.Cells[2, 0] := 'Eind';
    StringGrid1.Cells[3, 0] := 'Afspraak';
    StringGrid1.Cells[4, 0] := 'Link';

    AddToLog('Busy filling grid');
    SendMessage(StringGrid1.Handle, WM_SETREDRAW, 0, 0);
    try
      ds.First;
      while not ds.EOF do
      begin

        with StringGrid1 do
        begin
          Cells[1, StringGrid1.RowCount - 1] := ds.FieldByName('start').AsString;
          Cells[2, StringGrid1.RowCount - 1] := ds.FieldByName('end').AsString;
          Cells[3, StringGrid1.RowCount - 1] := ds.FieldByName('summary').AsString;
          Cells[4, StringGrid1.RowCount - 1] := ds.FieldByName('htmllink').AsString;
        end;

        for Q := 1 to 4 do
        begin
          nwWidth := StringGrid1.Canvas.TextWidth(
            StringGrid1.Cells[Q, StringGrid1.RowCount - 1]);
          if nwWidth > StringGrid1.ColWidths[Q] then
            StringGrid1.ColWidths[Q] := nwWidth + 20;
        end;
        Application.ProcessMessages;
        StringGrid1.RowCount := StringGrid1.RowCount + 1;

        ds.Next;
      end;

      AddToLog('Sorting');
      SortStringGrid(StringGrid1, 1);

      StringGrid1.ColWidths[0] := 10;
      StringGrid1.ColWidths[4] := 0; // <- also not -1
      // StringGrid1.Columns[4].Visible := false; // <- why does this give an error ?
      while (StringGrid1.RowCount > 2) and (StringGrid1.Cells[3, 1] = '') do
        StringGrid1.DeleteRow(1);

      AddToLog('Done filling grid');

    finally
      SendMessage(StringGrid1.Handle, WM_SETREDRAW, 1, 0);
      StringGrid1.Repaint;
      StringGrid1.SetFocus;
    end;

  finally
    Response.Free;
    ds.Free;
    btGetAppointments.Enabled := True;
  end;

end;

procedure TMainform.btClearDebugClick(Sender: TObject);
begin
  Memo2.Clear;
end;

end.
