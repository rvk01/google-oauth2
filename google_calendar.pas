unit google_calendar;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, google_oauth2, fpjson, jsonparser;

type
  TGoogleCalendar = class(TDataset)
  private
    { private declarations }
    FGoogleOAuth2: TGoogleOAuth2;
    FRecCount: integer;
    FRecSize: integer;
    FCurrRecNo: integer;
    FIsOpen: boolean;
    JSONObject: TJSONParser;
    CompleteJSON: TJSONData;
    CurrentJSON: TJSONData;
    LastErrorCode: string;
    LastErrorMessage: string;
  protected
    { protected declarations }
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
      DoCheck: boolean): TGetResult; override;
    procedure InternalFirst; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: boolean; override;
  public
    { public declarations }
    constructor Create(AOwner: TComponent; client_id, client_secret: string); overload;
    destructor Destroy; override;
    function GetFieldData(Field: TField; Buffer: Pointer): boolean; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    function GetRecordCount: integer; override;
    property GoogleOAuth2: TGoogleOAuth2 read FGoogleOAuth2 write FGoogleOAuth2;
  end;


implementation

uses httpsend;

constructor TGoogleCalendar.Create(AOwner: TComponent; client_id, client_secret: string);
begin
  inherited Create(AOwner);
  FGoogleOAuth2 := TGoogleOAuth2.Create(client_id, client_secret);
  JSONObject := nil;
  CompleteJSON := nil;
  CurrentJSON := nil;
  FRecCount := 0;
  FCurrRecNo := -1;

  //FStream:=TMemoryStream.Create;
  //FRecCount:=0;
  //FRecSize:=0;
  //FRecInfoOffset:=0;
  //FCurrRecNo:=-1;
  //BookmarkSize := sizeof(Longint);
end;

destructor TGoogleCalendar.Destroy;
begin
  if assigned(CompleteJSON) then
    CompleteJSON.Free;
  if assigned(JSONObject) then
    JSONObject.Free;
  FGoogleOauth2.Free;
  //FreeMem(FFieldOffsets);
  //FreeMem(FFieldSizes);
  inherited Destroy;
end;



function TGoogleCalendar.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: boolean): TGetResult;
var
  Accepted: boolean;
  J, D: TJSONData;
begin
  Result := grOk;
  Accepted := False;
  if (FRecCount < 1) then
  begin
    Result := grEOF;
    exit;
  end;
  repeat
    case GetMode of
      gmCurrent:
        if (FCurrRecNo >= FRecCount) or (FCurrRecNo < 0) then
          Result := grError;
      gmNext:
        if (FCurrRecNo < FRecCount - 1) then
          Inc(FCurrRecNo)
        else
          Result := grEOF;
      gmPrior:
        if (FCurrRecNo > 0) then
          Dec(FCurrRecNo)
        else
          Result := grBOF;
    end;
    if Result = grOK then
    begin

      try
        D := CompleteJSON.FindPath('items');
        // for I := 0 to D.Count - 1 do ;
        CurrentJSON := D.Items[FCurrRecNo];
        Accepted := True;
      finally
      end;

      if (GetMode = gmCurrent) and not Accepted then
        Result := grError;

    end;
  until (Result <> grOK) or Accepted;

end;

procedure TGoogleCalendar.InternalFirst;
begin
  FCurrRecNo := -1;
end;

procedure TGoogleCalendar.InternalHandleException;
begin
  FCurrRecNo := FRecCount;
end;

procedure TGoogleCalendar.InternalInitFieldDefs;
var
  I: integer;
  Fieldnm: ansistring;
  iFldType: TFieldType;
  iFldSize: integer;
begin
  FieldDefs.Clear;

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

  for I := 0 to 4 do
  begin
    Fieldnm := 'field' + IntToStr(I);
    iFldType := ftString;
    iFldSize := 20;
    TFieldDef.Create(FieldDefs, Fieldnm, iFldType, iFldSize, False, I + 1);
  end;
end;

procedure TGoogleCalendar.InternalOpen;
var
  URL: string;
  Params: string;
  Response: TStringList;
  D: TJSONData;
begin
  InternalInitFieldDefs;
  // if DefaultFields then
  CreateFields;
  FCurrRecNo := -1;

  Response := TStringList.Create;
  try

    URL := 'https://www.googleapis.com/calendar/v3/calendars/' +
      GoogleOauth2.EMail + '/events';
    Params := 'access_token=' + GoogleOauth2.Access_token;
    Params := Params + '&maxResults=2500';
    if HttpGetText(URL + '?' + Params, Response) then
    begin

      JSONObject := TJSONParser.Create(Response.Text);
      try
        // Response.SavetoFile('c:\temp\test.json');
        CompleteJSON := JSONObject.Parse;
        if Assigned(CompleteJSON) then
        begin
          D := CompleteJSON.FindPath('error');
          if assigned(D) then
          begin
            LastErrorCode := GoogleOauth2.RetrieveJSONValue(D, 'code');
            LastErrorMessage := GoogleOauth2.RetrieveJSONValue(D, 'message');
            exit;
          end;
          D := CompleteJSON.FindPath('items');
          // for I := 0 to D.Count - 1 do ;
          FRecCount := D.Count;
          FIsOpen := True;
        end;

      finally
      end;

    end;

  finally
  end;

end;

procedure TGoogleCalendar.InternalClose;
begin
  FIsOpen := False;
  if DefaultFields then
    DestroyFields;
end;

function TGoogleCalendar.IsCursorOpen: boolean;
begin
  Result := FIsOpen;
end;

function TGoogleCalendar.GetFieldData(Field: TField; Buffer: Pointer): boolean;
var
  SrcBuffer: TRecordBuffer;
  I: integer;
  aStr: string;
begin

  (*
  I := Field.FieldNo - 1;
  case Field.DataType of
    //ftBoolean: wordbool(Buffer^) := StrToBool(FStrings.Values[Field.FieldName]);
    ftinteger: longint(Buffer^) := 25; // Todo:
    ftFloat: double(Buffer^) := pi;  // Todo:
    ftString:
    begin
      //aStr := FStrings.Values[Field.FieldName];
      move(aStr[1], Buffer^, length(aStr) + 1);
    end;
  end;
  *)

  //  aStr := CurrentJSON;
  aStr := GoogleOauth2.RetrieveJSONValue(CurrentJSON, 'summary');
  move(aStr[1], Buffer^, length(aStr) + 1);
  Result := True; // <-------

  //MessageBox(CurrentJSON);
  //if I >= 0 then
  //begin
  //  Result := not getfieldisnull(pointer(srcbuffer), I);
  //  if Result and assigned(Buffer) then
  //    Move(GetRecordBufferPointer(SrcBuffer, GetIntegerPointer(ffieldoffsets, I)^)^,
  //      Buffer^, GetIntegerPointer(FFieldSizes, I)^);
  //end
  //else // Calculated, Lookup
  //begin
  //  Inc(SrcBuffer, RecordSize + Field.Offset);
  //  Result := boolean(SrcBuffer[0]);
  //  if Result and assigned(Buffer) then
  //    Move(SrcBuffer[1], Buffer^, Field.DataSize);
  //end;

end;

procedure TGoogleCalendar.SetFieldData(Field: TField; Buffer: Pointer);
begin

end;

function TGoogleCalendar.GetRecordCount: integer;
begin
  CheckActive;
  Result := FRecCount;
end;


end.
