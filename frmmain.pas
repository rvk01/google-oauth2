unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Grids, blcksock, fpjson, jsonConf, LMessages, EditBtn,
  DbCtrls, md5;

const
  WM_AFTER_SHOW = WM_USER + 300;

type

  { TMainform }

  TMainform = class(TForm)
    btGetAccess: TButton;
    btGetFileList: TButton;
    btSendMail: TButton;
    btRemoveTokens: TButton;
    btClearLog: TButton;
    btnSimpleUpload: TButton;
    btnUploadWithResume: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    btGetAppointments: TButton;
    btClearDebug: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    btGetContacts: TButton;
    listmthd: TCheckBox;
    ckHideFolders: TCheckBox;
    CheckGroup1: TCheckGroup;

    ckForceManualAuth: TCheckBox;
    ckUseBrowserTitle: TCheckBox;
    Edit4: TEdit;
    edLocation: TEdit;
    edStart: TDateEdit;
    edEnd: TDateEdit;


    edBody: TMemo;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    edTitle: TEdit;
    edDescription: TEdit;
    edRecipient: TEdit;
    edSender: TEdit;
    edSubject: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    ListView1: TListView;
    ListView2: TListView;
    PageControl6: TPageControl;
    Panel2: TPanel;
    Panel3: TPanel;
    ProgressBar2: TProgressBar;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    StringGrid4: TStringGrid;
    Summary: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    Memo2: TMemo;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    PageControl3: TPageControl;
    PageControl4: TPageControl;
    PageControl5: TPageControl;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    StringGrid3: TStringGrid;
    Summary1: TLabel;
    Summary2: TLabel;
    Summary3: TLabel;
    Summary4: TLabel;
    TabSheet1: TTabSheet;
    TabSheet10: TTabSheet;
    TabSheet11: TTabSheet;
    TabSheet12: TTabSheet;
    TabSheet13: TTabSheet;
    TabSheet14: TTabSheet;
    TabSheet15: TTabSheet;
    TabSheet16: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    TreeView1: TTreeView;
    procedure btGetAccessClick(Sender: TObject);
    procedure btGetFileListClick(Sender: TObject);
    procedure btSendMailClick(Sender: TObject);
    procedure btRemoveTokensClick(Sender: TObject);
    procedure btClearLogClick(Sender: TObject);
    procedure btGetAppointmentsClick(Sender: TObject);
    procedure btClearDebugClick(Sender: TObject);
    procedure btnSimpleUploadClick(Sender: TObject);
    procedure btnUploadWithResumeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ckHideFoldersClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView2DblClick(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure StringGrid3KeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure StringGrid4DblClick(Sender: TObject);
    procedure TabSheet12Show(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1SelectionChanged(Sender: TObject);
  private
    { private declarations }
  protected
    procedure AfterShow(var Msg: TLMessage); message WM_AFTER_SHOW;
  public
    { public declarations }
    procedure AddToLog(Str: string);
    procedure CheckTokenFile;
    function GetJSONParam(filename, param: string): string;
    procedure SetJSONParam(filename, param, Value: string);
    procedure DeleteJSONPath(filename, param: string);
    // function Download_Gdrive_File(id,auth, target: string): Boolean;
    procedure FillDriveGrid_old;
    procedure FillDriveView;
    procedure UploadWithResume(fileid: string = '');
    procedure FillDriveView2;
  end;

var
  Mainform: TMainform;


implementation

uses
  DB,
  google_oauth2,
  google_calendar,
  google_drive,

  smtpsend,
  httpsend,
  synautil,
  Windows,
  comobj;

{$R *.lfm}

{ TMainform }

const
  client_id = '896304839415-nnl5e0smrtakhr9r2l3bno0tes2mrtgk.apps.googleusercontent.com';
  client_secret = 'dUahHDn3IMyhCIk3qD4tf8E_';
var  JDrive: Tgoogledrive;

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

  ListView1.Clear;
  Treeview1.Items.Clear;

  Jdrive := TGoogleDrive.Create(Self, client_id, client_secret);
  Jdrive.Progress := ProgressBar1;
  Jdrive.LogMemo := Memo1;

  //Left := (Screen.Width - round(Screen.Width * 0.8)) div 2;
  //Top := (Screen.Height - round(Screen.Height * 0.8)) div 2;
  Width := round(Screen.Width * 0.6);
  Height := round(Screen.Height * 0.9) - 100;
  Top := 100;
  Self.WindowState := wsMaximized; // for now

  ckForceManualAuth.Checked := False;
  ckUseBrowserTitle.Checked := True;
  edStart.Date := Now;
  edEnd.Date := Now;

  if CheckGroup1.Items.Count > 2 then
  begin
    CheckGroup1.Checked[0] := True;
    CheckGroup1.Checked[1] := True;
    CheckGroup1.Checked[2] := True;
    CheckGroup1.CheckEnabled[0] := False;
    CheckGroup1.CheckEnabled[1] := False;
  end;

  // PageControl1.ActivePageIndex := 0;

  CheckTokenFile;

end;

procedure TMainform.FormDestroy(Sender: TObject);
begin

  Jdrive.Free;

end;

procedure TMainform.AfterShow(var Msg: TLMessage);
begin

  //if FileExists('Pendingupload.txt') then
  //begin
  //  PageControl1.ActivePage := TabSheet3;
  //  PageControl5.ActivePage := TabSheet12;
  //  btnUploadWithResume.Click;
  //end;

end;

procedure TMainform.FormShow(Sender: TObject);
begin
  PostMessage(Self.Handle, WM_AFTER_SHOW, 0, 0);
end;




procedure TMainform.StringGrid1DblClick(Sender: TObject);
var
  Filename: string;
  FileId: string;
  A: TGFileRevisions;
  Rev: integer;
begin

  if Jdrive.gOAuth2.EMail = '' then exit;

  StringGrid4.Options := StringGrid4.Options + [goRowSelect];
  Stringgrid4.colcount := 9;
  Stringgrid4.rowcount := 1;
  StringGrid4.Cells[1, 0] := 'Title';
  StringGrid4.Cells[2, 0] := 'Created';
  StringGrid4.Cells[3, 0] := 'Modified';
  StringGrid4.Cells[4, 0] := 'Filename';
  StringGrid4.Cells[5, 0] := 'Size';
  StringGrid4.Cells[6, 0] := 'FileId';
  StringGrid4.Cells[7, 0] := 'MimeType';
  StringGrid4.Cells[8, 0] := 'RevisionId';



  with TStringGrid(Sender) do
  begin
    FileId := cells[6, Row];
    Filename := cells[4, Row];
    if Filename = '' then Filename := cells[1, Row]; // title
  end;

  Filename := Extractfilepath(ParamStr(0)) + Filename;
  // check for valid filename
  try
    // JDrive.DownloadFile(FileId, Filename);
    A := JDrive.GetRevisions(FileId);
    stringgrid4.rowcount := Length(A) + 1;
    Memo1.Lines.add(IntToStr(length(A)) + ' revisions found');
    if Length(A) > 0 then
      for Rev := 0 to Length(A) - 1 do
      begin;
        StringGrid4.Cells[8, Rev + 1] := A[Rev].revisionid;
        StringGrid4.Cells[6, Rev + 1] := A[Rev].id;
        StringGrid4.Cells[4, Rev + 1] := A[Rev].originalFileName;
        StringGrid4.Cells[7, Rev + 1] := A[Rev].mimetype;
        StringGrid4.Cells[3, Rev + 1] := A[Rev].modifieddate;
        Memo1.Lines.Add(A[Rev].revisionid + ' - ' + A[Rev].mimetype + ' - ' + A[Rev].modifieddate);
      end
    else
      Memo1.Lines.Add('no revisions');
  except
    ShowMessage('Could not save ' + Filename);
  end;
end;

procedure TMainform.StringGrid3KeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
var fileid, revisionid: string;
begin

  if Jdrive.gOAuth2.EMail = '' then exit;

  if key = VK_DELETE then
    with TStringGrid(Sender) do
    begin
      FileId := cells[6, Row];
      Revisionid := cells[8, Row];

    end;
end;

procedure TMainform.StringGrid4DblClick(Sender: TObject);
var fileid, revisionid, filename: string;
begin
  with TStringGrid(Sender) do
  begin
    filename := cells[4, Row];
    FileId := cells[6, Row];
    revisionid := cells[8, Row];
  end;
  Jdrive.DownloadFile(fileid, filename, revisionid);
end;

{var
  Browser: olevariant;
  GoUrl: variant;
begin

  GoUrl := '';
  with TStringGrid(Sender) do
    GoUrl := Cells[4, Row];
  if Pos('https://', GoUrl) = 0 then
    exit;

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
  }

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
    if CheckGroup1.Checked[2] then Include(Scopes, goMail);
    if CheckGroup1.Checked[3] then Include(Scopes, goContacts);
    if CheckGroup1.Checked[4] then Include(Scopes, goCalendar);
    if CheckGroup1.Checked[5] then Include(Scopes, goDrive);

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


procedure TMainform.Button5Click(Sender: TObject);
var
  gOAuth2: TGoogleOAuth2;
  HTTP: THTTPSend;
  URL: string;
  json: TJSONObject;
  dt_start, dt_end: TJSONObject;
begin

  gOAuth2 := TGoogleOAuth2.Create(client_id, client_secret);
  try
    gOAuth2.LogMemo := Memo1;
    gOAuth2.DebugMemo := Memo2;
    gOAuth2.ForceManualAuth := ckForceManualAuth.Checked;
    gOAuth2.UseBrowserTitle := ckUseBrowserTitle.Checked;
    gOAuth2.GetAccess([goCalendar], True);

    CheckTokenFile;

    if gOAuth2.EMail = '' then exit;

    HTTP := THTTPSend.Create;
    try

      json := TJSONObject.Create;
      dt_start := TJSONObject.Create;
      dt_end := TJSONObject.Create;
      try
        json.Add('summary', edTitle.Text);
        json.Add('location', edLocation.Text);
        json.Add('description', edDescription.Text);
        dt_start.Add('dateTime', FormatDateTime('yyyy-mm-dd', edStart.Date) + 'T' + FormatDateTime('hh:nn:ss', Now));
        dt_start.Add('timeZone', 'Europe/Amsterdam');
        dt_end.Add('dateTime', FormatDateTime('yyyy-mm-dd', edStart.Date) + 'T' + FormatDateTime('hh:nn:ss', Now));
        dt_end.Add('timeZone', 'Europe/Amsterdam');
        json.Add('start', dt_start);
        json.Add('end', dt_end);
        WriteStrToStream(HTTP.Document, ansistring(json.AsJSON));
      finally
        json.Free;
        // dt_start.Free; nope, added to json
        // dt_end.Free; nope, added to json
      end;

      URL := 'https://www.googleapis.com/calendar/v3/calendars/' + gOAuth2.EMail + '/events';
      HTTP.Headers.Add('Authorization: Bearer ' + gOAuth2.Access_token);
      HTTP.MimeType := 'application/json; charset=UTF-8';
      if HTTP.HTTPMethod('POST', URL) then
      begin
        if HTTP.ResultCode = 200 then
          Memo1.Lines.Add('event inserted')
        else
          Memo1.Lines.Add('error inserting');
        Memo2.Lines.LoadFromStream(HTTP.Document);
      end
      else
      begin
        Memo1.Lines.Add('error');
        Memo1.Lines.Add(HTTP.Headers.Text);
      end;
    finally
      HTTP.Free;
    end;

  finally
    gOAuth2.Free;
  end;
end;

procedure TMainform.ckHideFoldersClick(Sender: TObject);
begin
  FillDriveGrid_old;
end;

procedure TMainform.FillDriveGrid_old;
var
  Q: integer;
  nwWidth: integer;
begin

  StringGrid3.RowCount := 2;
  AddToLog('Busy filling grid');
  SendMessage(StringGrid3.Handle, WM_SETREDRAW, 0, 0);
  try
    Jdrive.First;
    while not Jdrive.EOF do
    begin
      if not ckHideFolders.Checked or not Jdrive.FieldByName('IsFolder').AsBoolean then
      begin
        with StringGrid3 do
        begin
          Cells[1, StringGrid3.RowCount - 1] := Jdrive.FieldByName('title').AsString;
          Cells[2, StringGrid3.RowCount - 1] := Jdrive.FieldByName('created').AsString;
          Cells[3, StringGrid3.RowCount - 1] := Jdrive.FieldByName('modified').AsString;
          Cells[4, StringGrid3.RowCount - 1] := Jdrive.FieldByName('filename').AsString;
          Cells[5, StringGrid3.RowCount - 1] := Jdrive.FieldByName('filesize').AsString;
          Cells[6, StringGrid3.RowCount - 1] := Jdrive.FieldByName('fileId').AsString;
          Cells[7, StringGrid3.RowCount - 1] := Jdrive.FieldByName('mimeType').AsString;
          if Jdrive.FieldByName('mimeType').AsString = 'application/vnd.google-apps.folder' then
            Cells[7, StringGrid3.RowCount - 1] := '<dir>';
        end;

        StringGrid3.RowCount := StringGrid3.RowCount + 1;

      end;

      Jdrive.Next;
    end;

    if (StringGrid3.RowCount > 2) then
      StringGrid3.RowCount := StringGrid3.RowCount - 1;

    StringGrid3.AutoSizeColumns;
    StringGrid3.ColWidths[0] := 10;
    StringGrid3.ColWidths[6] := 1;

    AddToLog('Done filling grid');

  finally
    SendMessage(StringGrid3.Handle, WM_SETREDRAW, 1, 0);
    StringGrid3.Repaint;
    StringGrid3.SetFocus;
  end;

end;

procedure TMainform.btGetFileListClick(Sender: TObject);
var
  Response: TStringList;
  StartDt: string;
  EndDt: string;
begin

  Response := TStringList.Create;
  try
    btGetFileList.Enabled := False;

    JDrive.gOAuth2.LogMemo := Memo1;
    Jdrive.gOAuth2.DebugMemo := Memo2;
    Jdrive.gOAuth2.ForceManualAuth := ckForceManualAuth.Checked;
    Jdrive.gOAuth2.UseBrowserTitle := ckUseBrowserTitle.Checked;
    Jdrive.gOAuth2.GetAccess([goDrive], True);

    CheckTokenFile;

    if Jdrive.gOAuth2.EMail = '' then
      exit;

    Jdrive.Open;

    // JDrive.CreateFolder('bbbb');

    Jdrive.Populate();

    StringGrid3.Options := StringGrid3.Options + [goRowSelect];
    StringGrid3.ColCount := 8;

    StringGrid3.RowCount := 2;
    StringGrid3.Cells[1, 0] := 'Title';
    StringGrid3.Cells[2, 0] := 'Created';
    StringGrid3.Cells[3, 0] := 'Modified';
    StringGrid3.Cells[4, 0] := 'Filename';
    StringGrid3.Cells[5, 0] := 'Size';
    StringGrid3.Cells[6, 0] := 'FileId';
    StringGrid3.Cells[7, 0] := 'MimeType';
    StringGrid3.AutoFillColumns := False;

    FillDriveGrid_old;

  finally
    Response.Free;
    btGetFileList.Enabled := True;
  end;

end;

function DownloadHTTPStream(cURL: string; aStream: TStream): boolean;
var
  HTTP: THTTPSend;
begin
  Result := False;
  HTTP := THTTPSend.Create;
  try
    { HTTPGetResult := } HTTP.HTTPMethod('GET', cURL);
    if (HTTP.ResultCode >= 100) and (HTTP.ResultCode <= 299) then
    begin
      HTTP.Document.SaveToStream(aStream);
      aStream.Position := 0;
      Result := True;
    end;
  finally
    HTTP.Free;
  end;
end;

procedure LoadImageFromWeb(const Image: TImage; Url: string);
var
  mems: TMemoryStream;
  R: TRect;
begin
  if Url <> '' then
  begin
    mems := TMemoryStream.Create;
    try
      if DownloadHTTPStream(Url, mems) then
      begin
        Image.Picture.LoadFromStream(mems);
      end;
    finally
      mems.Free;
    end;
  end;
end;

procedure TMainform.TreeView1SelectionChanged(Sender: TObject);
begin
end;

procedure TMainform.TreeView1Click(Sender: TObject);
begin
  //FillDriveView;
end;


procedure TMainform.FillDriveView2;
var
  Q: integer;
  nwWidth: integer;
  TreeNode: TTreeNode;
  ListItem: TListItem;
  Img: TImage;
  IconLink: string;
  ImgLinkList: TStringList;
  MapId: String;
  z:integer;
begin

  //MapId := '';
  //if (Treeview1.Selected <> nil) and (Treeview1.Selected.Data <> nil) then
  //begin
  //  JDrive.GotoBookmark(Treeview1.Selected.Data);
  //  MapId := Jdrive.FieldByName('FileId').AsString;
  //  StatusBar1.SimpleText := MapId;
  //end;

  ListView1.Clear;
  Treeview1.Items.Clear;
  Listview1.Visible:=false;
  Treeview1.Visible:=false;
  Application.ProcessMessages; // update views


  TreeView1.Images := ImageList1;

  // ListView1.ViewStyle:=vsIcon;
  ListView1.MultiSelect := True;

  ListView1.LargeImages := nil;
  ListView1.SmallImages := nil;
  ImageList1.Clear;
  ListView1.LargeImages := ImageList1;
  ListView1.SmallImages := ImageList1;

  ImgLinkList := TStringList.Create;


  ProgressBar2.Max := length(JDrive.Files);

  AddToLog('Busy filling grid');
  try
    for z:=0 to length(JDrive.Files)-1 do
    begin
      application.processmessages;
      ProgressBar2.Position := z;
      if Jdrive.Files[z].isFolder then
      begin
        if TreeView1.Items.Count = 0 then
        begin
          Treeview1.Items.Add(nil, 'Google Drive');
        end;
        TreeNode := Treeview1.Items.AddChild(Treeview1.Items.GetFirstNode, Jdrive.Files[z].name);

        IconLink := Jdrive.Files[z].iconLink;
        if false and (IconLink <> '') then
        begin
          TreeNode.ImageIndex := ImgLinkList.IndexOf(IconLink);
          if TreeNode.ImageIndex = -1 then
          begin
            Img := TImage.Create(nil);
            try
              LoadImageFromWeb(Img, IconLink);
              TreeNode.ImageIndex := ImageList1.Add(Img.Picture.Bitmap, nil);
              ImgLinkList.Add(IconLink);
            finally
              Img.Free;
            end;
          end;
        end;

      end
      else
      begin
        ListItem := ListView1.Items.Add;
        (*
        Load icon 90x90 to a stringlist
        Convert them to 16x16
        and load from internet in a thread
        *)
        IconLink := Jdrive.Files[z].iconLink;
        if true and (IconLink <> '') then
        begin
          ListItem.ImageIndex := ImgLinkList.IndexOf(IconLink);
          if ListItem.ImageIndex = -1 then
          begin
            Img := TImage.Create(nil);
            try
              LoadImageFromWeb(Img, IconLink);
              ListItem.ImageIndex := ImageList1.Add(Img.Picture.Bitmap, nil);
              ImgLinkList.Add(IconLink);
            finally
              Img.Free;
            end;
          end;
        end;

        ListItem.Caption := Jdrive.Files[z].name;
        ListItem.SubItems.Add(Jdrive.Files[z].modifiedTime);
        ListItem.SubItems.Add(Jdrive.Files[z].size);
        ListItem.SubItems.Add(Jdrive.Files[z].mimeType);
        ListItem.SubItems.Add(Jdrive.Files[z].originalFilename);
        ListItem.SubItems.Add(Jdrive.Files[z].fileid);

      end;

//      Jdrive.Next;
    end;

    AddToLog('Done filling grid');
    TreeView1.FullExpand;

  finally
    ProgressBar2.Position := 0;
    ImgLinkList.Free;
  Listview1.Visible:=true;
  Treeview1.Visible:=true;

  end;
end;


procedure TMainform.FillDriveView;
var
  Q: integer;
  nwWidth: integer;
  TreeNode: TTreeNode;
  ListItem: TListItem;
  Img: TImage;
  IconLink: string;
  ImgLinkList: TStringList;
  MapId: String;
  BookMark: TBookmark;
begin

  //MapId := '';
  //if (Treeview1.Selected <> nil) and (Treeview1.Selected.Data <> nil) then
  //begin
  //  JDrive.GotoBookmark(Treeview1.Selected.Data);
  //  MapId := Jdrive.FieldByName('FileId').AsString;
  //  StatusBar1.SimpleText := MapId;
  //end;

  ListView1.Clear;
  Treeview1.Items.Clear;
  Application.ProcessMessages; // update views


  TreeView1.Images := ImageList1;

  // ListView1.ViewStyle:=vsIcon;
  ListView1.MultiSelect := True;

  ListView1.LargeImages := nil;
  ListView1.SmallImages := nil;
  ImageList1.Clear;
  ListView1.LargeImages := ImageList1;
  ListView1.SmallImages := ImageList1;

  ImgLinkList := TStringList.Create;

  ProgressBar2.Max := JDrive.RecordCount;

  AddToLog('Busy filling grid');
  try
    Jdrive.First;
    while not Jdrive.EOF do
    begin

      ProgressBar2.Position := JDrive.RecNo;

      if Jdrive.FieldByName('IsFolder').AsBoolean then
      begin
        if TreeView1.Items.Count = 0 then
        begin
          Treeview1.Items.Add(nil, 'Google Drive');
        end;
        BookMark := JDrive.Bookmark;
        TreeNode := Treeview1.Items.AddChildObject(
           Treeview1.Items.GetFirstNode, Jdrive.FieldByName('title').AsString, Pointer(Bookmark));

        IconLink := Jdrive.FieldByName('iconLink').AsString;
        if false and (IconLink <> '') then
        begin
          TreeNode.ImageIndex := ImgLinkList.IndexOf(IconLink);
          if TreeNode.ImageIndex = -1 then
          begin
            Img := TImage.Create(nil);
            try
              LoadImageFromWeb(Img, IconLink);
              TreeNode.ImageIndex := ImageList1.Add(Img.Picture.Bitmap, nil);
              ImgLinkList.Add(IconLink);
            finally
              Img.Free;
            end;
          end;
        end;

      end
      else
      begin
        ListItem := ListView1.Items.Add;

        (*
        Load icon 90x90 to a stringlist
        Convert them to 16x16
        and load from internet in a thread
        *)
        IconLink := Jdrive.FieldByName('iconLink').AsString;
        if true and (IconLink <> '') then
        begin
          ListItem.ImageIndex := ImgLinkList.IndexOf(IconLink);
          if ListItem.ImageIndex = -1 then
          begin
            Img := TImage.Create(nil);
            try
              LoadImageFromWeb(Img, IconLink);
              ListItem.ImageIndex := ImageList1.Add(Img.Picture.Bitmap, nil);
              ImgLinkList.Add(IconLink);
            finally
              Img.Free;
            end;
          end;
        end;

        ListItem.Caption := Jdrive.FieldByName('title').AsString;
        ListItem.SubItems.Add(Jdrive.FieldByName('modified').AsString);
        ListItem.SubItems.Add(Jdrive.FieldByName('filesize').AsString);
        ListItem.SubItems.Add(Jdrive.FieldByName('mimeType').AsString);
        ListItem.SubItems.Add(Jdrive.FieldByName('filename').AsString);
        ListItem.SubItems.Add(Jdrive.FieldByName('fileId').AsString);

      end;

      Jdrive.Next;
    end;

    AddToLog('Done filling grid');
    TreeView1.FullExpand;

  finally
    ProgressBar2.Position := 0;
    ImgLinkList.Free;
  end;
end;

procedure TMainform.Button1Click(Sender: TObject);
var
  Q: integer;
  nwWidth: integer;
  TreeNode: TTreeNode;
  ListItem: TListItem;
  Img: TImage;
  IconLink: string;
  ImgLinkList: TStringList;
begin

  JDrive.gOAuth2.LogMemo := Memo1;
  Jdrive.gOAuth2.DebugMemo := Memo2;
  Jdrive.gOAuth2.ForceManualAuth := ckForceManualAuth.Checked;
  Jdrive.gOAuth2.UseBrowserTitle := ckUseBrowserTitle.Checked;
  Jdrive.gOAuth2.GetAccess([goDrive], True);

  CheckTokenFile;

  if Jdrive.gOAuth2.EMail = '' then
    exit;

  Jdrive.Open;
  if not listmthd.checked then
    begin
  Jdrive.Populate();
  FillDriveView;
    end
  else
    begin
    JDrive.ListFiles(JDrive.Files,[],'root','name,mimeType,id,size,modifiedTime,iconLink');
    FillDriveView2;
    end;


end;

procedure TMainform.ListView1DblClick(Sender: TObject);
var
  ListItem: TListItem;
  FileId: string;
  A: TGFileRevisions;
  Rev: integer;
begin
  Jdrive.Progress := ProgressBar1;

  if Jdrive.gOAuth2.EMail = '' then exit;

  ListView2.Clear;
  ListView2.LargeImages := ImageList1;
  ListView2.SmallImages := ImageList1;

  FileId := '';
  if (ListView1.Selected.SubItems.Count > 4) then
    FileId := ListView1.Selected.SubItems.Strings[4];

  if FileId <> '' then
  begin
    A := JDrive.GetRevisions(FileId);
    Memo1.Lines.add(IntToStr(length(A)) + ' revisions found');
    if Length(A) > 0 then
      for Rev := Length(A) - 1 downto 0 do
      begin;
        ListItem := ListView2.Items.Add;
        ListItem.Caption := A[Rev].revisionid;
        ListItem.SubItems.Add(A[Rev].modifieddate);
        ListItem.SubItems.Add(A[Rev].originalFileName);
        ListItem.SubItems.Add(A[Rev].mimetype);
        ListItem.SubItems.Add(A[Rev].id);
      end
    else
    begin
      ListItem := ListView2.Items.Add;
      ListItem.Caption := 'no revisions';
    end;
  end
  else
  begin
    ListItem := ListView2.Items.Add;
    ListItem.Caption := 'error getting revisions';
  end;

end;

procedure TMainform.ListView2DblClick(Sender: TObject);
var
  FileId, RevisionId, Filename: string;
begin
  Jdrive.Progress := ProgressBar1;
  RevisionId := ListView2.Selected.Caption;
  FileId := '';
  if (ListView2.Selected.SubItems.Count > 3) then
  begin
    FileId := ListView2.Selected.SubItems.Strings[3];
    Filename := ListView2.Selected.SubItems.Strings[1];
    if Jdrive.DownloadFile(fileid, filename, revisionid) then
    begin
      StatusBar1.SimpleText := Filename + ' downloaded';
    end;
  end;
end;

procedure TMainform.Button2Click(Sender: TObject);
var files: TGfiles;
var i: integer;
begin
  StringGrid3.Options := StringGrid3.Options + [goRowSelect];
  StringGrid3.ColCount := 9;

  StringGrid3.RowCount := 2;
  StringGrid3.Cells[1, 0] := 'Title';
  StringGrid3.Cells[2, 0] := 'Created';
  StringGrid3.Cells[3, 0] := 'Modified';
  StringGrid3.Cells[4, 0] := 'Filename';
  StringGrid3.Cells[5, 0] := 'Size';
  StringGrid3.Cells[6, 0] := 'FileId';
  StringGrid3.Cells[7, 0] := 'MimeType';
  StringGrid3.Cells[8, 0] := 'RevisionId';

  StringGrid3.AutoFillColumns := False;

  JDrive.gOAuth2.LogMemo := Memo1;
  Jdrive.gOAuth2.DebugMemo := Memo2;
  Jdrive.gOAuth2.ForceManualAuth := ckForceManualAuth.Checked;
  Jdrive.gOAuth2.UseBrowserTitle := ckUseBrowserTitle.Checked;
  Jdrive.gOAuth2.GetAccess([goDrive], True);
  CheckTokenFile;


  if Jdrive.gOAuth2.EMail = '' then
    exit;
  JDrive.Open;
  for i := 0 to length(files) - 1 do
  begin

    if not ckHideFolders.Checked or not files[i].isFolder then
    begin
      with StringGrid3 do
      begin
        Mainform.Memo1.Lines.add('Processing ...' + IntToStr(i + 1));
        Cells[1, StringGrid3.RowCount - 1] := files[i].name;
        Cells[2, StringGrid3.RowCount - 1] := files[i].createdTime;
        Cells[3, StringGrid3.RowCount - 1] := files[i].modifiedTime;
        Cells[4, StringGrid3.RowCount - 1] := files[i].originalFilename;
        Cells[5, StringGrid3.RowCount - 1] := files[i].Size;
        Cells[6, StringGrid3.RowCount - 1] := files[i].fileid;
        Cells[7, StringGrid3.RowCount - 1] := files[i].mimeType;
        if files[i].mimeType = 'application/vnd.google-apps.folder' then
          Cells[7, StringGrid3.RowCount - 1] := '<dir>';
        if Length(files[i].revisions) > 0 then Cells[8, StringGrid3.RowCount - 1] := files[i].Revisions[0].revisionid;
      end;

      StringGrid3.RowCount := StringGrid3.RowCount + 1;

    end;

  end;

end;

procedure TMainform.Button3Click(Sender: TObject);
var x: integer;
var fileid, revisionid: string;
begin
  if stringgrid4.RowCount = 2 then
  begin
    ShowMessage('No revision found, you may delete the file');
    exit;
  end;

  x := Stringgrid4.Selection.Top;

  fileid := StringGrid4.Cells[6, x];
  revisionid := StringGrid4.Cells[8, x];

  if QuestionDlg('Question', 'You''re about to delete a revision of the current file, continue anyway ?',
    mtCustom, [1, 'Ok', 2, 'No thanks'], '') = 2 then exit;

  JDrive.DeleteGFile(fileid, revisionid);
end;


procedure TMainform.btClearDebugClick(Sender: TObject);
begin
  Memo2.Clear;
end;

function Gdrivepostfile(const URL, auth, FileName: string; const Data: TStream;
  const ResultData: TStrings): boolean;
var
  HTTP: THTTPSend;
  Bound, s: string;
begin
  Bound := IntToHex(Random(MaxInt), 8) + '_Synapse_boundary';
  HTTP := THTTPSend.Create;
  try
    s := '--' + Bound + CRLF;
    s := s + 'Content-Type: application/json; charset=UTF-8' + CRLF + CRLF;
    s := s + '{' + CRLF;
    s := s + '"name": "' + ExtractFileName(FileName) + '"' + CRLF;
    s := s + '}' + CRLF + CRLF;

    s := s + '--' + Bound + CRLF;
    s := s + 'Content-Type: application/octet-stream' + CRLF + CRLF;
    WriteStrToStream(HTTP.Document, ansistring(s));
    HTTP.Document.CopyFrom(Data, 0);

    s := CRLF + '--' + Bound + '--' + CRLF;
    WriteStrToStream(HTTP.Document, ansistring(s));

    HTTP.Headers.Add('Authorization: Bearer ' + auth);
    HTTP.MimeType := 'multipart/form-data; boundary=' + Bound;
    Result := HTTP.HTTPMethod('POST', URL);
    Mainform.Memo2.Lines.Add(HTTP.Headers.Text);

    if Result then
      ResultData.LoadFromStream(HTTP.Document);
  finally
    HTTP.Free;
  end;
end;

procedure TMainform.btnSimpleUploadClick(Sender: TObject);
var
  URL: string;
  Data: TFileStream;
  ResultData: TStringList;
begin
  // URL := 'https://www.googleapis.com/upload/drive/v3/files?uploadType=media';
  // URL := 'https://www.googleapis.com/upload/drive/v3/files?uploadType=resumable';
  URL := 'https://www.googleapis.com/upload/drive/v3/files?uploadType=multipart';

  JDrive.gOAuth2 := TGoogleOAuth2.Create(client_id, client_secret);
  ResultData := TStringList.Create;
  Data := TFileStream.Create('c:\temp\test.txt', fmOpenRead);
  try
    if not FileExists('tokens.dat') then
    begin
      // first get all access clicked on Groupbox
      btGetAccess.Click;
    end;

    JDrive.gOAuth2.LogMemo := Memo1;
    JDrive.gOAuth2.DebugMemo := Memo2;
    JDrive.gOAuth2.ForceManualAuth := ckForceManualAuth.Checked;
    JDrive.gOAuth2.UseBrowserTitle := ckUseBrowserTitle.Checked;
    JDrive.gOAuth2.GetAccess([], True); // <- get from file
    // no need for scope because we should already have access
    // via the btGetAccess for all the scopes in Groupbox
    if JDrive.gOAuth2.EMail = '' then
      exit;

    Gdrivepostfile(URL, JDrive.gOAuth2.Access_token, 'test.txt', Data, ResultData);

    Memo1.Lines.Add(ResultData.Text);

  finally
    Data.Free;
    ResultData.Free;
    JDrive.gOAuth2.Free;
  end;

end;


type
  TPendingUpload = packed record
    // id: integer;
    filename: string;
    url: string;
    md5: string;
    description: string;
    // date: tdatetime;
  end;

type
  PendingUploadArray = array of TPendingUpload;

procedure Retrieve_All_upload_files(filename: string; var pendinguploads: PendingUploadArray);
var
  a: TJSONConfig;
  b: TStringList;
  i: integer;
begin
  a := TJSONConfig.Create(nil);
  try
    a.Filename := filename;
    b := TStringList.Create;
    Setlength(pendinguploads, 0);
    a.EnumSubKeys('/', b);
    Setlength(pendinguploads, b.Count);
    for i := 0 to b.Count - 1 do
    begin
      with pendinguploads[i] do
      begin
        filename := a.Getvalue(b[i] + '/Filename', '');
        url := a.Getvalue(b[i] + '/URL', '');
        description := a.Getvalue(b[i] + '/Description', '');
        md5 := a.Getvalue(b[i] + '/Md5', '');
      end;
    end;
  finally
    a.Free;
    b.Free;
  end;
end;



procedure TMainform.UploadWithResume(fileid: string = '');
const
  BaseURL = 'https://www.googleapis.com/upload/drive/v3/files';
  Param = 'uploadType=resumable';
  Pendingfile = 'Pendingupload.json';

  function GetNewUploadFile: TPendingUpload;
  var
    UploadFilename: string;
    UploadURL: string;
    Data: TStream;
  begin
    Result.Url := '';
    UploadFilename := '';
    with TOpenDialog.Create(nil) do
      try
        Execute;
        UploadFilename := Filename;
      finally
        Free;
      end;

    if UploadFilename = '' then exit; // aborted
    Result.Description := Edit3.Text;

    // add to pending
    Result.filename := Uploadfilename;
    Result.description := Edit3.Text;
    Result.url := ''; // not yet
    Result.md5 := md5print(md5file(UploadFilename));;

    Data := TFileStream.Create(UploadFilename, fmOpenRead);
    try
      UploadURL := JDrive.GetUploadURI(BaseURL, JDrive.gOAuth2.Access_token,
        Result.filename, Result.Description, Data, Param, fileid);
      ShowMessage(UploadURL);
      if pos('upload_id', UploadURL) > 0 then
      begin
        Result.url := UploadURL;
      end
      else
      begin
        ShowMessage('Error getting upload_id');
      end;
    finally
      Data.Free;
    end;

  end;

var
  Res: string;
  Data: TFileStream;
  Answer: TModalResult;
  md5: string;
  Pending: PendingUploadArray;
  Current: TPendingUpload;
  qUrl: string;
  i, j: integer;
begin
  // https://developers.google.com/drive/v3/web/manage-uploads

  if not FileExists('tokens.dat') then
  begin
    // first get all access clicked on Groupbox
    btGetAccess.Click;
  end;
  try
    JDrive.gOAuth2.LogMemo := Memo1;
    JDrive.gOAuth2.DebugMemo := Memo2;
    JDrive.gOAuth2.ForceManualAuth := ckForceManualAuth.Checked;
    JDrive.gOAuth2.UseBrowserTitle := ckUseBrowserTitle.Checked;
    JDrive.gOAuth2.GetAccess([], True); // <- get from file
    // no need for scope because we should already have access
    // via the btGetAccess for all the scopes in Groupbox
    if JDrive.gOAuth2.EMail = '' then exit;

    SetLength(Pending, 0);
    if FileExists(pendingfile) then
      retrieve_all_upload_files(pendingfile, pending);

    Listbox1.Clear;
    for j := 0 to Length(Pending) - 1 do
      ListBox1.Items.Add(Pending[j].filename);

    Answer := 2; // don't foget in case hasuploads is false

    if Length(pending) > 0 then
    begin;
      Answer := QuestionDlg('Question', 'Previous upload(s) was/were in progress.' + #13 +
        'Do you want to continue, abort or remove pending-status?',
        mtCustom, [1, 'Continue all', 2, 'Upload another file and continue all', 3, 'Remove status'], '');
      //if Answer = 2 then exit;
      if Answer = 3 then
      begin
        DeleteFile(Pendingfile); // the one in fileutils doesn't need pchar()
        ShowMessage('Pending upload-status removed');
        exit;
      end;
    end;

    if Answer = 2 then
    begin // new upload

      Current := GetNewUploadFile;
      if Current.Url <> '' then
      begin
        Setlength(pending, Length(Pending) + 1);
        pending[Length(Pending) - 1] := Current;
        // and add it directory to the pendingfile
        SetJsonparam(Pendingfile, Current.filename + '/Filename', Current.filename);
        SetJsonparam(Pendingfile, Current.filename + '/Description', Current.description);
        SetJsonparam(Pendingfile, Current.filename + '/URL', Current.url);
        SetJsonparam(Pendingfile, Current.filename + '/Md5', Current.md5);
      end;

    end;

    // now the main loop
    for i := 0 to length(pending) - 1 do
    begin

      Listbox1.Clear;
      for j := 0 to Length(Pending) - 1 do
        ListBox1.Items.Add(Pending[j].filename);

      Current := Pending[i];
      // Memo1.Lines.Add('Result request upload_id = ' + UploadURL);
      md5 := md5print(md5file(Current.filename)); // always before tstream

      Data := TFileStream.Create(Current.Filename, fmOpenRead);
      try

        if Current.md5 <> md5 then
        begin
          Memo1.Lines.add(Current.filename + ' md5 mismatch');
          // need to reupload
          qURL := JDrive.GetUploadURI(BaseURL, JDrive.gOAuth2.Access_token,
            Current.Filename, Current.Description, Data, Param, fileid);
          if pos('upload_id', qURL) > 0 then
          begin
            Current.url := qURL;
            Current.md5 := md5;
            Pending[i] := Current;
            // and add it directory to the pendingfile
            SetJsonparam(Pendingfile, Current.filename + '/URL', Current.url);
            SetJsonparam(Pendingfile, Current.filename + '/Md5', Current.md5);
          end
          else
          begin
            ShowMessage('Error getting upload_id');
            Continue;
          end;
        end;

        // do the transfer in chunks if needed
        Res := JDrive.UploadResumableFile(Current.URL, Data);
        Memo1.Lines.Add(Res);

        // remove from pending
        if Copy(Res, 1, 3) = '200' then
          DeleteJSONPath(Pendingfile, Current.filename);

        Jdrive.Progress.Position := 0;

      finally
        Data.Free;
      end;

    end;

  finally
    Listbox1.Clear;
  end;

end;



procedure TMainform.btnUploadWithResumeClick(Sender: TObject);
begin
  Uploadwithresume;
end;

procedure TMainform.Button4Click(Sender: TObject);
var x: integer;
var fileid: string;
begin
  x := Stringgrid4.Selection.Top;
  fileid := stringgrid4.Cells[6, x];
  if fileid = '' then exit;
  uploadwithresume(fileid);

end;

procedure TMainform.TabSheet12Show(Sender: TObject);
const
  Pendingfile = 'Pendingupload.json';
var
  Pending: PendingUploadArray;
  j: integer;
begin
  SetLength(Pending, 0);
  if FileExists(pendingfile) then
    retrieve_all_upload_files(pendingfile, pending);

  Listbox1.Clear;
  for j := 0 to Length(Pending) - 1 do
    ListBox1.Items.Add(Pending[j].filename);

end;


function TMainform.GetJSONParam(filename, param: string): string;
var
  a: TJSONConfig;
begin
  a := TJSONConfig.Create(nil);
  try
    a.Filename := filename;
    Result := a.GetValue(param, '');
  finally
    a.Free;
  end;
end;

procedure TMainform.SetJSONParam(filename, param, Value: string);
var
  a: TJSONConfig;
begin
  a := TJSONConfig.Create(nil);
  try
    a.Formatted := True;
    a.Filename := filename;
    a.SetValue(param, Value);
  finally
    a.Free;
  end;
end;

procedure TMainform.DeleteJSONPath(filename, param: string);
var
  a: TJSONConfig;
begin
  a := TJSONConfig.Create(nil);
  try
    a.Formatted := True;
    a.Filename := filename;
    a.SetValue(param + '/dummy', 'dummy'); // this will make sure changes are detected
    // see http://bugs.freepascal.org/view.php?id=30907
    a.DeletePath(param);
  finally
    a.Free;
  end;
end;

end.
