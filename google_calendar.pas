unit google_calendar;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, Forms, google_oauth2, fpjson, jsonparser, memds;

type
  TGoogleCalendar = class(TMemDataSet)
  private
    { private declarations }
    FgOAuth2: TGoogleOAuth2;
    LastErrorCode: string;
    LastErrorMessage: string;
  protected
    { protected declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent; client_id, client_secret: string); overload;
    destructor Destroy; override;
    procedure Populate(aFilter: string = '');

    property gOAuth2: TGoogleOAuth2 read FgOAuth2 write FgOAuth2;
  published
  end;


implementation

uses httpsend;

constructor TGoogleCalendar.Create(AOwner: TComponent; client_id, client_secret: string);
begin
  inherited Create(AOwner);
  FieldDefs.Clear;
  //FieldDefs.Add('Boolean', ftBoolean, 0, False);
  //FieldDefs.Add('Integer', ftInteger, 0, False);
  //FieldDefs.Add('SmallInt', ftSmallInt, 0, False);
  //FieldDefs.Add('Float', ftFloat, 0, False);
  //FieldDefs.Add('String', ftString, 30, False);
  //FieldDefs.Add('Time', ftTime, 0, False);
  //FieldDefs.Add('Date', ftDate, 0, False);
  //FieldDefs.Add('DateTime', ftDateTime, 0, False);
  FieldDefs.Add('start', ftString, 25, False);
  FieldDefs.Add('end', ftString, 25, False);
  FieldDefs.Add('summary', ftString, 255, False);
  FieldDefs.Add('htmllink', ftString, 255, False);
  CreateTable;

  gOAuth2 := TGoogleOAuth2.Create(client_id, client_secret);

end;

destructor TGoogleCalendar.Destroy;
begin
  gOAuth2.Free;
  inherited Destroy;
end;

function RetrieveJSONValue(JSON: TJSONData; Value: string): string;
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


procedure TGoogleCalendar.Populate(aFilter: string = '');
var
  Response: TStringList;
  URL: string;
  Params: string;
  P: TJSONParser;
  I: integer;
  J, D, E: TJSONData;
  StartDt: string;
  EndDt: string;
begin

(*
  {
   "kind": "calendar#event",
   "etag": "\"2847129938594000\"",
   "id": "0hbjgoqstouc0olq6s0rs0rb4k",
   "status": "confirmed",
   "htmlLink": "https://www.google.com/calendar/event?eid=MGhiamdvcXN0b3VjMG9scTZzMHJzMHJiNGsgcmlrLnZhbi5rZWtlbUBt",
   "created": "2015-02-10T10:19:08.000Z",
   "updated": "2015-02-10T10:42:49.297Z",
   "summary": "Lloyd voor dorpel",
   "start": {
    "dateTime": "2012-05-18T15:45:00+02:00"
   },
   "end": {
    "dateTime": "2012-05-18T16:00:00+02:00"
   },
   "iCalUID": "0hbjgoqstouc0olq6s0rs0rb4k@google.com"
  },
*)

  Response := TStringList.Create;
  Self.DisableControls;
  try

    if gOAuth2.EMail = '' then
      exit;

    gOAuth2.LogLine('Retrieving Calendar ' + gOAuth2.EMail);
    URL := 'https://www.googleapis.com/calendar/v3/calendars/' +
      gOAuth2.EMail + '/events';
    Params := 'access_token=' + gOAuth2.Access_token;
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
            LastErrorCode := RetrieveJSONValue(D, 'code');
            LastErrorMessage := RetrieveJSONValue(D, 'message');
            gOAuth2.LogLine(format('Error %s: %s',
              [LastErrorCode, LastErrorMessage]));
            exit;
          end;

          gOAuth2.DebugLine('Name: ' + RetrieveJSONValue(J, 'summary'));
          gOAuth2.DebugLine('Updated: ' + RetrieveJSONValue(J, 'updated'));
          gOAuth2.DebugLine('Timezone: ' + RetrieveJSONValue(J, 'timeZone'));
          gOAuth2.DebugLine('Next page: ' + RetrieveJSONValue(J, 'nextPageToken'));
          gOAuth2.DebugLine('Next sync: ' + RetrieveJSONValue(J, 'nextSyncToken'));

          gOAuth2.LogLine('Busy filling dataset');

          D := J.FindPath('items');
          gOAuth2.DebugLine(format('%d items received', [D.Count]));
          for I := 0 to D.Count - 1 do
          begin
            E := D.Items[I].FindPath('start');
            StartDt := RetrieveJSONValue(E, 'dateTime');
            if StartDt = '' then
              StartDt := RetrieveJSONValue(E, 'date');

            E := D.Items[I].FindPath('end');
            EndDt := RetrieveJSONValue(E, 'dateTime');
            if EndDt = '' then
              EndDt := RetrieveJSONValue(E, 'date');

            Append;
            // 2015-02-10T10:42:49.297Z
            // 2012-05-18T15:45:00+02:00
            FieldByName('start').AsString := StartDt;
            FieldByName('end').AsString := EndDt;
            FieldByName('summary').AsString :=
              RetrieveJSONValue(D.Items[I], 'summary');
            FieldByName('htmllink').AsString :=
              RetrieveJSONValue(D.Items[I], 'htmlLink');
            Self.Post;
            Application.ProcessMessages;

          end;

          gOAuth2.LogLine(format('%d items stored', [Self.RecordCount]));

          gOAuth2.LogLine('Done filling dataset');

        end;
      finally
        if assigned(J) then
          J.Free;
        P.Free;
      end;

    end;


  finally
    Response.Free;
    Self.EnableControls;
  end;

end;



(*

      Open;
      D:=now;
      ACount:=1000;
      for I:=1 to ACount do
        begin
        Append;
        FieldByName('Boolean').AsBoolean:=False;
        FieldByName('Integer').AsInteger:=I;
        FieldByName('SmallInt').AsInteger:=I;
        FieldByName('Float').AsFloat:=I/10;
        FieldByName('String').AsString:='Test-Data '+IntToStr(I);
        FieldByName('Time').AsDateTime:=D;
        FieldByName('Date').AsDateTime:=D;
        Post;
        end;
      First;
      ACount:=0;
      While Not EOF do
        begin
        Inc(ACount);
        Writeln('Record ',ACount,' : ');
        Writeln('------------------------');
        For I:=0 to Fields.Count-1 do
          Writeln(Fields[I].FieldName,' : ',Fields[I].AsString);
        Writeln;
        Next;
        end;
      Writeln('Total data size : ',DataSize);
      If (ParamCount>0) then
        FileName:=ParamStr(1);
      Close;

*)



end.
