unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Grids, DBGrids;

type

  { TMainform }

  TMainform = class(TForm)
    btGetAccess: TButton;
    btGetCalendar: TButton;
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
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;


    edBody: TMemo;
    edRecipient: TEdit;
    edSender: TEdit;
    edSubject: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
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
    TabSheet12: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    procedure btGetCalendarClick(Sender: TObject);
    procedure btGetAccessClick(Sender: TObject);
    procedure btSendMailClick(Sender: TObject);
    procedure btRemoveTokensClick(Sender: TObject);
    procedure btClearLogClick(Sender: TObject);
    procedure btGetAppointmentsClick(Sender: TObject);
    procedure btClearDebugClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  httpsend,
  Windows,
  fpjson, jsonparser;

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
    CheckGroup1.Caption := 'Access (scope)             remove tokens.dat first to get new access';
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

procedure TMainform.btGetAccessClick(Sender: TObject);
var
  gApi: TGoogleOAuth2;
  Scopes: GoogleScopeSet;
begin
  // Onetime authentication
  // Save tokens to token.dat
  gApi := TGoogleOAuth2.Create(client_id, client_secret);
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

    gApi.LogMemo := Memo1;
    gApi.DebugMemo := Memo2;
    gApi.ForceManualAuth := ckForceManualAuth.Checked;
    gApi.UseBrowserTitle := ckUseBrowserTitle.Checked;
    gApi.GetAccess(Scopes, True); // <- get from file

    if gApi.EMail <> '' then
      edSender.Text := format('%s <%s>', [gApi.FullName, gApi.EMail]);

    CheckTokenFile;

  finally
    gApi.Free;
  end;

end;

procedure TMainform.btGetCalendarClick(Sender: TObject);
var
  ds: TGoogleCalendar;
begin

  ds := TGoogleCalendar.Create(Self, client_id, client_secret);
  DataSource1.DataSet := ds;
  ds.GoogleOAuth2.LogMemo := Memo1;
  ds.GoogleOAuth2.GetAccess([goCalendar], True);
  ds.Open;
  ds.First;
  ds.Last;
  ds.First;

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
  gApi: TGoogleOAuth2;
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

  gApi := TGoogleOAuth2.Create(client_id, client_secret);
  smtp := TSMTPSend.Create;
  msg_lines := TStringList.Create;
  try
    btSendMail.Enabled := False;

    // first get oauthToken
    gApi.LogMemo := Memo1;
    gApi.DebugMemo := Memo2;
    gApi.ForceManualAuth := ckForceManualAuth.Checked;
    gApi.UseBrowserTitle := ckUseBrowserTitle.Checked;
    gApi.GetAccess([], True); // <- get from file
    // no need for scope because we should already have access
    // via the btGetAccess for all the scopes in Groupbox
    if gApi.EMail = '' then
      exit;

    CheckTokenFile;

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
      AddToLog('XOAUTH2 ERROR: ' + CRLF + smtp.ChallengeError());
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
  gApi: TGoogleOAuth2;
  Response: TStringList;
  URL: string;
  Params: string;
  P: TJSONParser;
  I, Q: integer;
  J, D, E: TJSONData;
  StartDt: string;
  EndDt: string;
  nwWidth: integer;
  LastErrorCode, LastErrorMessage: string;
begin
  gApi := TGoogleOAuth2.Create(client_id, client_secret);
  Response := TStringList.Create;
  try
    btGetAppointments.Enabled := False;

    // first get oauthToken
    gApi.LogMemo := Memo1;
    gApi.DebugMemo := Memo2;
    gApi.ForceManualAuth := ckForceManualAuth.Checked;
    gApi.UseBrowserTitle := ckUseBrowserTitle.Checked;
    gApi.GetAccess([goMail], True); // <- get from file
    if gApi.EMail = '' then
      exit;

    CheckTokenFile;

    AddToLog('Retrieving Calendar ' + gApi.EMail);
    URL := 'https://www.googleapis.com/calendar/v3/calendars/' + gApi.EMail + '/events';
    Params := 'access_token=' + gApi.Access_token;
    Params := Params + '&maxResults=2500';
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
            LastErrorCode := gApi.RetrieveJSONValue(D, 'code');
            LastErrorMessage := gApi.RetrieveJSONValue(D, 'message');
            AddToLog(format('Error %s: %s', [LastErrorCode, LastErrorMessage]));
            exit;
          end;

          AddToLog('Name: ' + gApi.RetrieveJSONValue(J, 'summary'));
          AddToLog('Updated: ' + gApi.RetrieveJSONValue(J, 'updated'));
          AddToLog('Timezone: ' + gApi.RetrieveJSONValue(J, 'timeZone'));
          AddToLog('Next page: ' + gApi.RetrieveJSONValue(J, 'nextPageToken'));
          AddToLog('Next sync: ' + gApi.RetrieveJSONValue(J, 'nextSyncToken'));

          AddToLog('Busy filling grid');

          StringGrid1.Options := StringGrid1.Options + [goRowSelect];
          StringGrid1.ColCount := 5;
          StringGrid1.RowCount := 2;
          StringGrid1.Cells[1, 0] := 'Start';
          StringGrid1.Cells[2, 0] := 'Eind';
          StringGrid1.Cells[3, 0] := 'Afspraak';
          StringGrid1.Cells[4, 0] := 'Link';

          SendMessage(StringGrid1.Handle, WM_SETREDRAW, 0, 0);
          try
            D := J.FindPath('items');
            for I := 0 to D.Count - 1 do
            begin
              E := D.Items[I].FindPath('start');
              StartDt := gApi.RetrieveJSONValue(E, 'dateTime');
              if StartDt = '' then
                StartDt := gApi.RetrieveJSONValue(E, 'date');
              StringGrid1.Cells[0, StringGrid1.RowCount - 1];

              E := D.Items[I].FindPath('end');
              EndDt := gApi.RetrieveJSONValue(E, 'dateTime');
              if EndDt = '' then
                EndDt := gApi.RetrieveJSONValue(E, 'date');

              StringGrid1.Cells[1, StringGrid1.RowCount - 1] := StartDt;
              StringGrid1.Cells[2, StringGrid1.RowCount - 1] := EndDt;
              StringGrid1.Cells[3, StringGrid1.RowCount - 1] :=
                gApi.RetrieveJSONValue(D.Items[I], 'summary');
              StringGrid1.Cells[4, StringGrid1.RowCount - 1] :=
                gApi.RetrieveJSONValue(D.Items[I], 'htmlLink');
              for Q := 1 to 4 do
              begin
                nwWidth := StringGrid1.Canvas.TextWidth(
                  StringGrid1.Cells[Q, StringGrid1.RowCount - 1]);
                if nwWidth > StringGrid1.ColWidths[Q] then
                  StringGrid1.ColWidths[Q] := nwWidth + 2;
              end;

              Application.ProcessMessages;
              StringGrid1.RowCount := StringGrid1.RowCount + 1;

            end;

            AddToLog('Sorting');
            SortStringGrid(StringGrid1, 1);

            StringGrid1.ColWidths[0] := 10;
            StringGrid1.ColWidths[4] := 0; // <- also not -1
            // StringGrid1.Columns[4].Visible := false; // <- why does this give an error ?
            while (StringGrid1.RowCount > 2) and (StringGrid1.Cells[1, 1] = '') do
              StringGrid1.DeleteRow(1);

            AddToLog('Done filling grid');

          finally
            SendMessage(StringGrid1.Handle, WM_SETREDRAW, 1, 0);
            StringGrid1.Repaint;
            StringGrid1.SetFocus;
          end;

        end;
      finally
        if assigned(J) then
          J.Free;
        P.Free;
      end;

    end;


  finally
    Response.Free;
    gApi.Free;
    btGetAppointments.Enabled := True;
  end;

end;

procedure TMainform.btClearDebugClick(Sender: TObject);
begin
  Memo2.Clear;
end;

end.
