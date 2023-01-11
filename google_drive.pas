unit google_drive;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, Forms, google_oauth2, fpjson, jsonparser, memds,
  httpsend, blcksock, typinfo, ComCtrls, synautil, StdCtrls, md5;

type TGDExport = record
    Description : string;
    MimeType : string;
    FileExtension : string;
end;

type TGDExportArray = array of TGDExport;

const GoogleDocumentsExport : TGDExportArray =
    (
    (Description:'HTML';MimeType:'text/html';FileExtension:'.html'),
    (Description:'Plain Text';MimeType:'text/plain';FileExtension:'.txt'),
    (Description:'Rich text';MimeType:'application/rtf';FileExtension:'.rtf'),
    (Description:'Open Office';MimeType:'application/vnd.oasis.opendocument.text';FileExtension:'.odt'),
    (Description:'PDF';MimeType:'application/pdf';FileExtension:'.pdf'),
    (Description:'MS Word document';MimeType:'application/vnd.openxmlformats-officedocument.wordprocessingml.document';FileExtension:'.docx')
    ) ;

const GoogleSpreadsheetsExport : TGDExportArray =
    (
    (Description:'MS Excel';MimeType:'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet';FileExtension:'.xlsx'),
    (Description:'Open Office sheet';MimeType:'application/x-vnd.oasis.opendocument.spreadsheet';FileExtension:'.ods'),
    (Description:'PDF';MimeType:'application/pdf';FileExtension:'.pdf'),
    (Description:'CSV (first sheet only)';MimeType:'text/csv';FileExtension:'.csv')
    ) ;

const GoogleDrawingsExport : TGDExportArray =
    (
    (Description:'JPEG';MimeType:'image/jpeg';FileExtension:'.jpg'),
    (Description:'PNG';MimeType:'image/png';FileExtension:'.png'),
    (Description:'SVG';MimeType:'image/svg+xml';FileExtension:'.svg'),
    (Description:'PDF';MimeType:'application/pdf';FileExtension:'.pdf')
    ) ;


const GooglePresentationsExport : TGDExportArray =
    (
    (Description:'MS PowerPoint';MimeType:'application/vnd.openxmlformats-officedocument.presentationml.presentation';FileExtension:'.pptx'),
    (Description:'Plain text';MimeType:'text/plain';FileExtension:'.txt'),
    (Description:'PDF';MimeType:'application/pdf';FileExtension:'.pdf')
    ) ;

type apiver = (v2, v3);

const  UploadURL = 'https://www.googleapis.com/upload/drive/v3/files';
const  MetaDataURL = 'https://www.googleapis.com/drive/v3/files';

type TUploadSetting = (RenameFile, KeepForever);
type TUploadSettings = set of TUploadSetting;


type Tlistsetting = (listrevisions, listparents, showpreviousfolder);
type Tlistsettings = set of Tlistsetting;

type TGFileParent = packed record
    id: string;
end;

type TGFileParents = array of TGFileParent;


type TCustomPropertyAs = (asstring, asboolean, aslist, asinteger, aselse);
type TCustomProperty = packed record
     name : string;
     value : string;
end;

type TCustomProperties = array of TCustomProperty;

type TGFileRevision = packed record
    id: string;
    revisionid: string;
    size: string;
    modifiedTime: string;
    mimetype: string;
    originalFileName: string;
  end;

type TGFileRevisions = array of TGfileRevision;

type TGFile = packed record
    name: string;
    fileid: string;
    description: string;
    createdTime: string;
    modifiedTime: string;
    downloadUrl: string;
    originalFilename: string;
    md5Checksum: string;
    size: string;
    mimeType: string;
    iconLink: string;
    isFolder: boolean;
    headRevisionId: string;
    trashed: boolean;
    revisions: TGFilerevisions;
    parents: TGFileParents;
  end;

type TGFiles = array of TGfile;

type TGoogleDriveInformation= packed record
    rootFolderId:string;
    limit:int64;
    usage:int64;
    usageInDrive:int64;
    usageInDriveTrash:int64;
end;

type
  TGoogleDrive = class(TMemDataSet)
  private
    { private declarations }
  const MaxResults: integer = 500;
  var
    CancelCur: boolean;
    CurFolder:string;
    FgOAuth2: TGoogleOAuth2;
    LastErrorCode: string;
    LastErrorMessage: string;
    Bytes: integer;
    MaxBytes: integer;
    downHTTP: THTTPSend;
    FLogMemo: TMemo;
    FDebugMemo: TMemo;
    FProgress: TProgressBar;
    procedure DownStatus(Sender: TObject; Reason: THookSocketReason;
      const Value: string);
    function GetSizeFromHeader(Header: string): integer;
    procedure UpStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
    function ParseMetadata(A:TJSONData;settings:TlistSettings):TGFile;

    Function ExtractQueryProperties:string;
    Function ExtractBodyProperties:string;

    protected
    { protected declarations }
  public
    { public declarations }

  var Files: TGFiles;

  var CustomBodyProperties : TCustomProperties;
  var CustomQueryProperties : TCustomProperties;

    constructor Create(AOwner: TComponent; client_id, client_secret: string); overload;
    destructor Destroy; override;

    procedure Populate(aFilter: string = '');
    function DownloadFile(id, TargetFile: string; revisionid: string = ''; exportmimetype : string = ''): boolean;
    function DownloadResumableFile(JFile: TGFile; TargetFile: string; revisionid: string = ''; exportmimetype : string = ''): boolean;

    function GetUploadURI(const URL, auth, FileN, Description: string;const Data: TStream; parameters: string = ''; fileid: string = ''; settings : TuploadSettings = [] ): string;

    property gOAuth2: TGoogleOAuth2 read FgOAuth2 write FgOAuth2;
    property CurrentFolder:string read CurFolder write CurFolder;
    property Progress: TProgressBar read Fprogress write Fprogress;
    property GFiles: TGFiles read Files write Files;
    property LogMemo: TMemo read FLogMemo write FLogMemo;
    property DebugMemo: TMemo read FDebugMemo write FDebugMemo;
    property CancelCurrent: boolean read CancelCur write CancelCur;
    function UploadResumableFile(const URL: string; const Data: TStream): string;
    procedure CreateFolder(foldername: string; parentid: string = '');

    Procedure ClearAllCustomProperties;
    Procedure AddCustomProperty(var customproperty:TCustomproperties;cname,cvalue:string; PropAs : TCustomPropertyAs = aselse);

    Function SetFileProperties(id : string):string;
    Function SetGFileProperties(Gfile:TGfile):string;
    Function SetGFileRevisionProperties(Gfilerev:TGfileRevision):string;


    function GetRevisions(fileid: string): TGFileRevisions;
    procedure GetGFileRevisions(var A: TGFile);

    function DeleteGFile(fileid:string; revisionid: string=''): boolean;
    function DeleteGFileRevision(var A: TGFileRevision): boolean;
    function DeleteAllGFileRevisions(var A: TGFileRevisions): boolean;

    function GetGFileMetadata(id:string;settings:TListSettings;customfields:string='*'):TGFile;
    procedure ListFiles(var A: TGFiles;settings:Tlistsettings;parentid:string='root';customfields:string='*');
    procedure FillGFileMetadata(var A:TGFile;settings:Tlistsettings);

  //function GetRootFolderId:string;
    function AboutGdrive(version:apiver):TGoogleDriveInformation;

    published

  end;


implementation


Procedure TGoogleDrive.ClearAllCustomProperties;
begin
setlength(CustomBodyProperties,0);
setlength(CustomQueryProperties,0);
end;

Procedure TGoogleDrive.AddCustomProperty(var customproperty:TCustomproperties;cname,cvalue:string; PropAs : TCustomPropertyAs = aselse);
var i : integer;
begin

 i:=length(CustomProperty);
 Setlength(CustomProperty, i + 1);

with CustomProperty[i] do
     begin
     name:=cname;
     value:=cvalue;
     if (PropAs = asstring) then value := '"' + value + '"';
     end
end;

procedure TGoogleDrive.UpStatus(Sender: TObject; Reason: THookSocketReason;
  const Value: string);
begin
  if Reason = HR_WriteCount then
  begin
    Progress.StepBy(StrToIntDef(Value, 0));
    Application.ProcessMessages;
  end;
end;

function TGoogleDrive.UploadResumableFile(const URL: string;
  const Data: TStream): string;
const
  MaxChunk = 40 * 256 * 1024; // ALWAYS chunks of 256KB
var
  HTTP: THTTPSend;
  s: string;
  i: integer;
  From, Size: integer;
  Tries, PrevFrom: integer;
begin
  Result := '';
  HTTP := THTTPSend.Create;
  try
    // Always check if there already was aborted upload (is easiest)
    HTTP.Headers.Add('Content-Length: 0');
    HTTP.Headers.Add('Content-Range: bytes */*');

    if not HTTP.HTTPMethod('PUT', URL) then exit;
    Result := 'pre - ' + #13 + HTTP.Headers.Text + #13 + #13 + HTTP.ResultString;
    From := 0;
    if HTTP.ResultCode in [200, 201] then
    begin
      Result := '200 already uploaded completely';
      exit;
    end;
    if HTTP.ResultCode = 308 then // Resume Incomplete
    begin
      for i := 0 to HTTP.Headers.Count - 1 do
      begin
        if Pos('Range: bytes=0-', HTTP.Headers.Strings[i]) > 0 then
        begin
          s := StringReplace(HTTP.Headers.Strings[i], 'Range: bytes=0-', '', []);
          From := StrToIntDef(s, -1) + 1; // from 0 or max_range + 1
          break;
        end;
      end;
    end;
    if not HTTP.ResultCode in [200, 201, 308] then
      exit;

    Tries := 0;
    PrevFrom := From;
    Progress.Min := 0;
    Progress.Max := Data.Size - 1;
    HTTP.Sock.OnStatus := @UpStatus;
    repeat

      Progress.Position := From;

      HTTP.Document.Clear;
      HTTP.Headers.Clear;

      // We need to resune upload from position "from"
      Data.Position := From;
      Size := Data.Size - From;
      if Size > MaxChunk then
        Size := MaxChunk;
      HTTP.Document.CopyFrom(Data, Size);
      HTTP.Headers.Add(Format('Content-Range: bytes %d-%d/%d',
        [From, From + Size - 1, Data.Size]));
      HTTP.MimeType := '';
      LogMemo.Lines.Add(HTTP.Headers.Text);
      if not HTTP.HTTPMethod('PUT', URL) then exit;

      Result := HTTP.Headers.Text + #13 + #13 + HTTP.ResultString;
      // Mainform.Memo2.Lines.Add(Result);

      if HTTP.ResultCode in [200, 201] then
        Result := '200 Upload complete';
      if HTTP.ResultCode = 308 then // Resume Incomplete
      begin
        for i := 0 to HTTP.Headers.Count - 1 do
        begin
          if Pos('Range: bytes=0-', HTTP.Headers.Strings[i]) > 0 then
          begin
            s := StringReplace(HTTP.Headers.Strings[i], 'Range: bytes=0-', '', []);
            PrevFrom := From;
            From := StrToIntDef(s, -1) + 1; // from 0 or max_range + 1
            break;
          end;
        end;
      end;

      // no 308 with actual transfer is received, increase tries
      if PrevFrom = From then
        Inc(Tries);

    until (HTTP.ResultCode in [200, 201]) or (Tries > 1);

  finally
    HTTP.Free;
  end;

end;


Function TGoogleDrive.ExtractBodyProperties:string;
var i : integer;
begin
result:= '{' + CRLF + '}';
if length(CustomBodyProperties)>0 then
   begin
   result  := '{' + CRLF;
   for i:=0 to length(CustomBodyProperties)-1 do
       begin;
          result := result + '"' + CustomBodyProperties[i].name + '": ';
          result := result + CustomBodyProperties[i].value;
          if i<length(CustomBodyProperties)-1 then result := result + ',';
          result := result + CRLF;
   end;
   result :=  result + '}';
   end;
end;

Function TGoogleDrive.ExtractQueryProperties:string;
var i : integer;
begin
result:='';
if length(CustomQueryProperties)>0 then
begin
     result := '?';
     for i:=0 to length(CustomQueryProperties)-1 do
         begin
     result := result + CustomQueryProperties[i].name + '=' + CustomQueryProperties[i].value;
     if i<length(CustomQueryProperties)-1 then result := result + '&';
         end;
end;
end;



Function TGoogleDrive.SetFileProperties(id : string):string;
var
  HTTP: THTTPSend;
  s, p: string;
  URL : string;
  i: integer;
begin

  Result := '';

  if (length(CustomBodyProperties)=0) and (length(CustomQueryProperties)=0) then
  begin
  result:='No properties to set, can''t continue';
  exit;
  end;

  URL := MetadataURL + '/' + id;

  // Query Parameters
  URL := URL + ExtractQueryProperties;
  // Body parameters
  s:= ExtractBodyProperties;

  HTTP := THTTPSend.Create;
  try
    HTTP.MimeType := 'application/json; charset=UTF-8';
    WriteStrToStream(HTTP.Document, ansistring(s));
    HTTP.Headers.Add('Authorization: Bearer ' + gOAuth2.Access_token);

  LogMemo.Lines.Add(s + #13 + URL);
  if not HTTP.HTTPMethod('PATCH', URL) then
    begin
      LogMemo.Lines.Add('Error setting parameters');
      exit;
    end;
    Result := HTTP.ResultString;
    LogMemo.Lines.Add( Result);
  finally
    HTTP.Free;
  end;
end;

Function TGoogleDrive.SetGFileProperties(Gfile:TGfile):string;
begin
result:=SetFileProperties(Gfile.fileid);
end;

Function TGoogleDrive.SetGFileRevisionProperties(Gfilerev:TGfileRevision):string;
begin
result:=SetFileProperties(Gfilerev.id+'revisions/'+Gfilerev.revisionid);
end;



function TGoogleDrive.GetUploadURI(const URL, auth, FileN, Description: string;
  const Data: TStream; parameters: string = ''; fileid: string = ''; settings : TuploadSettings = [] ): string;
var
  HTTP: THTTPSend;
  Method, URLM: string;
  s, rev: string;
  i: integer;
begin
  Result := '';

  if fileid <> '' then
  begin
    Method := 'PATCH';
    URLM := URL + '/' + fileid;
    rev:='originalFilename';
  end
  else
  begin
   Method := 'POST';
   URLM := URL;
   rev:='name';
  end;

  ClearAllCustomProperties;
  AddCustomProperty(CustomBodyProperties,rev,ExtractFileName(FileN),asstring);

  if (Renamefile in settings) and (fileid <> '') then
  AddCustomProperty(CustomBodyProperties,'name',ExtractFileName(FileN),asstring);

  AddCustomProperty(CustomBodyProperties,'description',Description,asstring);

  s := ExtractBodyProperties;

  HTTP := THTTPSend.Create;
  try
    HTTP.MimeType := 'application/json; charset=UTF-8';

    WriteStrToStream(HTTP.Document, ansistring(s));

    HTTP.Headers.Add(Format('X-Upload-Content-Length: %d', [Data.Size]));
    HTTP.Headers.Add('Authorization: Bearer ' + auth);

    AddCustomProperty(CustomQueryProperties,'uploadType','resumable');
    if (KeepForever in settings) then
    AddCustomProperty(CustomQueryProperties,'keepRevisionForever','true');

    parameters := ExtractQueryProperties;

  LogMemo.Lines.Add(s + chr(13) + URLM + '[' + parameters + ']');

  if not HTTP.HTTPMethod(Method, URLM + parameters) then
    begin
      LogMemo.Lines.Add('Error retrieving URI');
      exit;
    end;
    Result := HTTP.ResultString; // for any errors

    for i := 0 to HTTP.Headers.Count - 1 do
    begin
      if Pos('Location: ', HTTP.Headers.Strings[i]) > 0 then
      begin
        Result := StringReplace(HTTP.Headers.Strings[i], 'Location: ', '', []);
        break;
      end;
    end;
  finally
    HTTP.Free;
  end;
end;


function TGoogleDrive.DownloadResumableFile(JFile: TGFile; TargetFile: string; revisionid: string = ''; exportmimetype : string = ''): boolean;
const
  MaxChunk = 1024 * 1024;// 40 * 256 * 1024;
var
  HTTPGetResult: boolean;
  URL, URLM: string;
  from,size: integer;
  Stream: TFileStream;
  resume: boolean;
begin

  CancelCurrent:= False;
  Result := False;
  resume:= false;

    if FileExists(TargetFile) then
    begin
     Stream:=TFileStream.Create(TargetFile, fmOpenReadWrite);
     from:= Stream.size;
     resume:= true;
    end
  else
    begin
     Stream:=TFileStream.Create(TargetFile, fmCreate);
     from := 0;
    end;

  size := strtoint64(JFile.size);

  if gOAuth2.EMail = '' then exit;

  DownHTTP := THTTPSend.Create;
  Progress.Min := 0;
  Progress.Max := size;
  Progress.Position:= from;
  Bytes := 0;

  MaxBytes := -1;

  if not resume then
  LogMemo.Lines.Add('Downloading file...')
  else
  LogMemo.Lines.Add('Resuming file...');

  try
   repeat
    DownHTTP.Sock.OnStatus := @DownStatus;
    Stream.Seek(0,soEnd);
    URL := MetadataURL + '/' + JFile.fileid;
    ClearAllCustomProperties;

    if revisionid <> '' then URL := URL + '/revisions/' + revisionid;

    if exportmimetype <> '' then
    begin
    URL :=  URL + '/export';
    AddCustomProperty(CustomQueryProperties, 'mimeType',exportmimetype);
    end
    else AddCustomProperty(CustomQueryProperties, 'alt','media');

    DownHTTP.Clear;
    DownHTTP.Headers.Add('Authorization: Bearer ' + gOAuth2.Access_token);
    DownHTTP.Headers.Add(format('Range: bytes=%d-%d',[from,from+maxchunk]));

    Result := DownHTTP.HTTPMethod('GET', URL + ExtractQueryproperties);

    if (DownHTTP.ResultCode >= 100) and (DownHTTP.ResultCode <= 299) then
    begin
    Stream.CopyFrom(DownHTTP.Document, DownHTTP.Document.Size);

    LogMemo.Lines.Add('Download OK [' + IntToStr(DownHTTP.ResultCode) + ' - Range ' + inttostr(from) + ' to ' + inttostr(from+DownHTTP.Document.Size) +']');
   // LogMemo.Lines.Add(DownHTTP.Headers.Text);
    inc(from,DownHTTP.Document.Size);
    end
    else
    begin
      CancelCurrent:=true;
      LogMemo.Lines.Add('Error downloading file [' + IntToStr(DownHTTP.ResultCode) + ']');
    end;


   Application.processmessages;
   until (from >= size) or (CancelCurrent);
   Result := True;

  finally
    DownHTTP.Free;
    Stream.Free;

   if (JFile.md5Checksum<> '') and (JFile.md5Checksum=md5print(md5file(TargetFile))) then
   LogMemo.Lines.Add('Download OK - checkSum OK') else
   LogMemo.Lines.Add('Download OK - checkSum is not correct !!!');

  end;
end;


function TGoogleDrive.DownloadFile(id, TargetFile: string; revisionid: string = ''; exportmimetype : string = ''): boolean;
var
  HTTPGetResult: boolean;
  URL, URLM: string;
begin
  Result := False;
  if gOAuth2.EMail = '' then exit;
  Bytes := 0;
  MaxBytes := -1;
  DownHTTP := THTTPSend.Create;
  try
    Progress.Min := 0;
    Progress.Max := 100;
//    DownHTTP.Sock.OnStatus := @DownStatus;
    LogMemo.Lines.Add('Downloading file...');

    URL := MetadataURL + '/' + id;

    ClearAllCustomProperties;

    if revisionid <> '' then URL := URL + '/revisions/' + revisionid;

    if exportmimetype <> '' then
    begin
    URL :=  URL + '/export';
    AddCustomProperty(CustomQueryProperties, 'mimeType',exportmimetype);
    end
    else AddCustomProperty(CustomQueryProperties, 'alt','media');

    DownHTTP.Headers.Add('Authorization: Bearer ' + gOAuth2.Access_token);

    Result := DownHTTP.HTTPMethod('GET', URL + ExtractQueryproperties);

    if (DownHTTP.ResultCode >= 100) and (DownHTTP.ResultCode <= 299) then
    begin
      DownHTTP.Document.SaveToFile(TargetFile);
      LogMemo.Lines.Add('Download OK [' + IntToStr(DownHTTP.ResultCode) + ']');
      Result := True;
    end
    else
    begin
      LogMemo.Lines.Add('Error downloading file [' + IntToStr(DownHTTP.ResultCode) + ']');
    end;

  finally
    DownHTTP.Free;
  end;
end;

procedure TGoogleDrive.DownStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
var
  V, currentHeader: string;
  i: integer;
  pct: integer;
begin
  if (MaxBytes = -1) then
  begin
    for i := 0 to DownHTTP.Headers.Count - 1 do
    begin
      currentHeader := DownHTTP.Headers[i];
      MaxBytes := GetSizeFromHeader(currentHeader);
      if MaxBytes <> -1 then break;
    end;
  end;

  V := GetEnumName(TypeInfo(THookSocketReason), integer(Reason)) + ' ' + Value;

  if Reason = THookSocketReason.HR_ReadCount then
  begin
    Bytes := Bytes + StrToInt(Value);
    pct := round(Bytes / maxbytes * 100);
    Progress.Position := progress.position + StrToInt(Value);//pct;
    Application.ProcessMessages;
  end;

end;

function TGoogleDrive.GetSizeFromHeader(Header: string): integer;
var
  item: TStringList;
begin
  Result := -1;
  if Pos('Content-Length:', Header) <> 0 then
  begin
    item := TStringList.Create();
    try
      item.Delimiter := ':';
      item.StrictDelimiter := True;
      item.DelimitedText := Header;
      if item.Count = 2 then
      begin
        Result := StrToInt(Trim(item[1]));
      end;
    finally
      item.Free;
    end;
  end;
end;

constructor TGoogleDrive.Create(AOwner: TComponent; client_id, client_secret: string);
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
  FieldDefs.Add('title', ftString, 255, False);
  FieldDefs.Add('fileId', ftString, 255, False);
  FieldDefs.Add('description', ftString, 255, False);
  FieldDefs.Add('created', ftString, 255, False);
  FieldDefs.Add('modified', ftString, 255, False);
  FieldDefs.Add('downloadurl', ftString, 255, False);
  FieldDefs.Add('filename', ftString, 255, False);
  FieldDefs.Add('md5', ftString, 255, False);
  FieldDefs.Add('filesize', ftString, 20, False);
  FieldDefs.Add('IsFolder', ftBoolean, 0, False);
  FieldDefs.Add('mimeType', ftString, 255, False);
  FieldDefs.Add('iconLink', ftString, 255, False);
  CreateTable;

  gOAuth2 := TGoogleOAuth2.Create(client_id, client_secret);

end;

destructor TGoogleDrive.Destroy;
begin
  gOAuth2.Free;
  inherited Destroy;
end;


function RetrieveJSONValueInt64(JSON: TJSONData; Value: string): int64;
var
  D: TJSONData;
begin
  Result := 0;
  if Assigned(JSON) then
  begin
    D := JSON.FindPath(Value);
    if assigned(D) then
      Result := D.AsInt64;
  end;
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

procedure TGoogleDrive.Populate(aFilter: string = '');
var
  Response: TStringList;
  URL: string;
  Params: string;
  P: TJSONParser;
  I: integer;
  J, D, E: TJSONData;
begin
  (*
  {
   "kind": "drive#fileList",
   "etag": etag,
   "selfLink": string,
   "nextPageToken": string,
   "nextLink": string,
   "items": [ files Resource ]
  }

  {
    "kind": "drive#file",
    "id": string,
    "etag": etag,
    "selfLink": string,
    "webContentLink": string,
    "webViewLink": string,
    "alternateLink": string,
    "embedLink": string,
    "openWithLinks": {
      (key): string
    },
    "defaultOpenWithLink": string,
    "iconLink": string,
    "thumbnailLink": string,
    "thumbnail": {
      "image": bytes,
      "mimeType": string
    },
    "title": string,
    "mimeType": string,
    "description": string,
    "labels": {
      "starred": boolean,
      "hidden": boolean,
      "trashed": boolean,
      "restricted": boolean,
      "viewed": boolean
    },
    "createdDate": datetime,
    "modifiedDate": datetime,
    "modifiedByMeDate": datetime,
    "lastViewedByMeDate": datetime,
    "markedViewedByMeDate": datetime,
    "sharedWithMeDate": datetime,
    "version": long,
    "sharingUser": {
      "kind": "drive#user",
      "displayName": string,
      "picture": {
        "url": string
      },
      "isAuthenticatedUser": boolean,
      "permissionId": string,
      "emailAddress": string
    },
    "parents": [
      parents Resource
    ],
    "downloadUrl": string,
    "downloadUrl": string,
    "exportLinks": {
      (key): string
    },
    "indexableText": {
      "text": string
    },
    "userPermission": permissions Resource,
    "permissions": [
      permissions Resource
    ],
    "originalFilename": string,
    "fileExtension": string,
    "fullFileExtension": string,
    "md5Checksum": string,
    "fileSize": long,
    "quotaBytesUsed": long,
    "ownerNames": [
      string
    ],
    "owners": [
      {
        "kind": "drive#user",
        "displayName": string,
        "picture": {
          "url": string
        },
        "isAuthenticatedUser": boolean,
        "permissionId": string,
        "emailAddress": string
      }
    ],
    "lastModifyingUserName": string,
    "lastModifyingUser": {
      "kind": "drive#user",
      "displayName": string,
      "picture": {
        "url": string
      },
      "isAuthenticatedUser": boolean,
      "permissionId": string,
      "emailAddress": string
    },
    "ownedByMe": boolean,
    "editable": boolean,
    "canComment": boolean,
    "canReadRevisions": boolean,
    "shareable": boolean,
    "copyable": boolean,
    "writersCanShare": boolean,
    "shared": boolean,
    "explicitlyTrashed": boolean,
    "appDataContents": boolean,
    "headRevisionId": string,
    "properties": [
      properties Resource
    ],
    "folderColorRgb": string,
    "imageMediaMetadata": {
      "width": integer,
      "height": integer,
      "rotation": integer,
      "location": {
        "latitude": double,
        "longitude": double,
        "altitude": double
      },
      "date": string,
      "cameraMake": string,
      "cameraModel": string,
      "exposureTime": float,
      "aperture": float,
      "flashUsed": boolean,
      "focalLength": float,
      "isoSpeed": integer,
      "meteringMode": string,
      "sensor": string,
      "exposureMode": string,
      "colorSpace": string,
      "whiteBalance": string,
      "exposureBias": float,
      "maxApertureValue": float,
      "subjectDistance": integer,
      "lens": string
    },
    "videoMediaMetadata": {
      "width": integer,
      "height": integer,
      "durationMillis": long
    },
    "spaces": [
      string
    ],
    "isAppAuthorized": boolean
  }




  *)
  Response := TStringList.Create;
  Self.DisableControls;
  try

    if gOAuth2.EMail = '' then
      exit;

    // https://developers.google.com/drive/v2/reference/files/list
    gOAuth2.LogLine('Retrieving filelist ' + gOAuth2.EMail);
    URL := 'https://www.googleapis.com/drive/v2/files';
    Params := 'access_token=' + gOAuth2.Access_token;
    Params := Params + '&maxResults=1000';
    Params := Params + '&orderBy=folder,modifiedDate%20desc,title';
    if HttpGetText(URL + '?' + Params, Response) then
    begin
      gOAuth2.DebugLine(Response.Text);
      Self.Clear(False); // remove all records

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

          gOAuth2.LogLine('Busy filling dataset');

          D := J.FindPath('items');
          gOAuth2.DebugLine(format('%d items received', [D.Count]));
          for I := 0 to D.Count - 1 do
          begin
            Append;
            // 2015-02-10T10:42:49.297Z
            // 2012-05-18T15:45:00+02:00
            FieldByName('title').AsString := RetrieveJSONValue(D.Items[I], 'title');
            FieldByName('fileId').AsString := RetrieveJSONValue(D.Items[I], 'id');
            FieldByName('description').AsString := RetrieveJSONValue(D.Items[I], 'description');
            FieldByName('created').AsString := RetrieveJSONValue(D.Items[I], 'createdDate');
            FieldByName('modified').AsString := RetrieveJSONValue(D.Items[I], 'modifiedDate');
            FieldByName('downloadurl').AsString := RetrieveJSONValue(D.Items[I], 'downloadUrl');
            FieldByName('filename').AsString := RetrieveJSONValue(D.Items[I], 'originalFilename');
            FieldByName('md5').AsString := RetrieveJSONValue(D.Items[I], 'md5Checksum');
            FieldByName('filesize').AsString := RetrieveJSONValue(D.Items[I], 'fileSize');
            FieldByName('mimeType').AsString := RetrieveJSONValue(D.Items[I], 'mimeType');
            FieldByName('iconLink').AsString := RetrieveJSONValue(D.Items[I], 'iconLink');
            FieldByName('IsFolder').AsBoolean := FieldByName('mimeType').AsString = 'application/vnd.google-apps.folder';
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



function TGoogleDrive.ParseMetadata(A:TJSONData;settings:TlistSettings):TGFile;
var F: TJSONData;
var K: integer;
begin

  with result do
  begin

    fileid := RetrieveJSONValue(A, 'id');
    name := RetrieveJSONValue(A, 'name');
    mimeType := RetrieveJSONValue(A, 'mimeType');
    description := RetrieveJSONValue(A, 'description');
    createdTime := RetrieveJSONValue(A, 'createdTime');
    modifiedTime := RetrieveJSONValue(A, 'modifiedTime');
    downloadUrl := RetrieveJSONValue(A, 'downloadUrl');
    originalFilename := RetrieveJSONValue(A, 'originalFilename');
    md5Checksum := RetrieveJSONValue(A, 'md5Checksum');
    size := RetrieveJSONValue(A, 'size');
    iconLink := RetrieveJSONValue(A, 'iconLink');
    isFolder := mimeType = 'application/vnd.google-apps.folder';
    trashed := lowercase(RetrieveJSONValue(A, 'trashed'))='true';
    headRevisionId := RetrieveJSONValue(A, 'headRevisionId');

    if (listrevisions in settings) and not isFolder then revisions := GetRevisions(fileid);

   // get parents
   if (listparents in settings) then
       begin;
       setlength(parents,0);
       F := A.FindPath('parents');
       if not assigned(F) then
       begin
       // root case
          setlength(parents,1);
          parents[0].id:='root';
       end
       else
       for K:=0 to F.Count-1 do
             begin
             setlength(parents,K+1);
               with parents[K] do id:= (F.Items[K]).AsString;
             end;
       end;
  end;
end;

function TGoogleDrive.GetGFileMetadata(id:string;settings:TListSettings;customfields:string='*'):TGFile;
var
  Response: TStringList;
  URL: string;
  Params: string;
  P: TJSONParser;
  I, K: integer;
  A, J, D, E, F: TJSONData;
  HTTP:THTTPSend;
begin
  Response := TStringList.Create;

  result:=default(TGFile);
  with result do begin
  setlength(parents,1);
  parents[0].id:='';
  end;

    if gOAuth2.EMail = '' then exit;

    gOAuth2.LogLine('Retrieving metadata ' + gOAuth2.EMail);
    gOAuth2.LogLine('Busy...');

    URL := 'https://www.googleapis.com/drive/v3/files/'+id;
    Params := 'access_token=' + gOAuth2.Access_token;
    HTTP:=THTTPSend.Create;
    if HTTP.HTTPMethod('GET',URL + '?' + Params+'&fields='+customfields) then//HttpGetText(URL + '?' + Params+'&fields='+customfields, Response) then
    begin

    if HTTP.ResultCode=401 then begin;
    gOAuth2.LogLine('Session expired, please connect again');

    end
    else
    begin
    Response.LoadFromStream(HTTP.Document);
    gOauth2.logline(URL + '?' + Params+'&fields='+customfields);
    P := TJSONParser.Create(Response.Text);
      try
        J := P.Parse;

        if Assigned(J) then
        begin

          A := J.FindPath('error');
          if assigned(A) then
          begin
            LastErrorCode := RetrieveJSONValue(A, 'code');
            LastErrorMessage := RetrieveJSONValue(A, 'message');
            gOAuth2.LogLine(format('Error %s: %s',
              [LastErrorCode, LastErrorMessage]));
            exit;
          end;

          A := J;
          Result:=ParseMetaData(A,settings);

        end;

     finally
        if assigned(J) then
        J.Free;
        P.Free;
      end;
    end;

    Response.Free;

    end;
HTTP.Free;
  end;


procedure TGoogleDrive.FillGFileMetadata(var A:TGFile;settings:Tlistsettings);
begin
if A.fileid='' then exit;
A:=GetGFileMetadata(A.fileid,settings);
end;

procedure TGoogleDrive.ListFiles(var A: TGFiles;settings:Tlistsettings;parentid:string='root';customfields:string='*');
var
  Response: TStringList;
  URL: string;
  Params, pageToken: string;
  P: TJSONParser;
  I, K: integer;
  J, D, E, F: TJSONData;
  HTTP:THTTPSend;
  folderid,foldername:string;
  prevfolder:TGFile;
  begin
  Response := TStringList.Create;

Setlength(A,0);

if showpreviousfolder in settings then begin
GOauth2.LogLine('Retrieving parent''s informations');
PrevFolder:=GetGFileMetadata(parentid,[listparents],'name,parents');
  folderid:=PrevFolder.parents[0].id;
  foldername:=PrevFolder.name;

if folderid<>'root' then
  begin;
  SetLength(A, 1);
  with A[0] do
       begin
  fileid:=folderid;
  name:=''+foldername+' : Double click here to go back';
  iconLink:='https://ssl.gstatic.com/docs/doclist/images/icon_11_collection_list_1.png';
  mimeType:='application/vnd.google-apps.folder';
       end;
   end;
end;

  currentFolder:=parentid;
  pageToken:='';
  K:=0;
  try

    if gOAuth2.EMail = '' then exit;

    gOAuth2.LogLine('Retrieving filelist ' + gOAuth2.EMail);
    gOAuth2.LogLine('Busy...');

    if (customfields<>'') and (customfields<> '*') then
    customfields := 'nextPageToken,files(' + customfields + ')';

    repeat;
    URL := MetadataURL;//'https://www.googleapis.com/drive/v3/files';

    ClearAllCustomproperties;
    AddCustomproperty(customQueryProperties,'access_token',gOAuth2.Access_token);
    AddCustomproperty(customQueryProperties,'pageSize',IntToStr(MaxResults));
    AddCustomproperty(customQueryProperties,'orderBy','folder,modifiedTime%20desc,name');
    AddCustomproperty(customQueryProperties,'fields',customfields);

    // list specific parent folder
    if parentid<>'' then AddCustomproperty(customQueryProperties,'q','"' +parentid + '"+in+parents'); //Params := Params + '&q="' + parentid + '"%20in%20parents';
    if pageToken<>'' then AddCustomproperty(customQueryProperties,'pageToken',pageToken);//Params := Params + '&pageToken='+ pageToken;

    goauth2.LogLine(URL + ExtractQueryproperties);
    HTTP:=THTTPSend.Create;
    if HTTP.HTTPMethod('GET',URL + ExtractQueryproperties) then//HttpGetText(URL + '?' + Params, Response) then
    begin
     goauth2.Logline(inttostr(HTTP.ResultCode));
     if HTTP.ResultCode=401 then
     begin;
     goauth2.Logline('Session expired');
     end
     else
     begin
      Response.LoadFromStream(HTTP.Document);
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


          gOAuth2.LogLine('Parsing...');
          pageToken:=RetrieveJSONValue(J,'nextPageToken');
          //gOAuth2.logline(pageToken);

          D := J.FindPath('files');
          K:=length(A);
          SetLength(A,K+D.Count);
          gOAuth2.DebugLine(format('%d items received', [D.Count]));
          for I := 0 to D.Count - 1 do
          begin
            A[K+I]:=ParseMetaData(D.Items[I],settings);
            Application.ProcessMessages;
          end;
          gOAuth2.LogLine('Done');

        end;
      finally
        if assigned(J) then
        J.Free;
        P.Free;
      end;
     end;
    end;

    HTTP.Free;

until pageToken='';

  finally
    Response.Free;
  end;
end;


function TGoogleDrive.DeleteAllGFileRevisions(var A: TGFileRevisions): boolean;
var i: integer;
begin
  Result := False;
  for i := 0 to length(A) - 1 do if not (A[i].revisionid = '') then DeleteGFile(A[i].id, A[i].revisionid);
  Result := True;
end;


function TGoogleDrive.DeleteGFileRevision(var A: TGFileRevision): boolean;
begin
  Result := False;
  if A.revisionid = '' then exit;
  DeleteGFile(A.id, A.revisionid);
end;



function TGoogleDrive.DeleteGFile(fileid:string; revisionid: string=''): boolean;
var
  HTTP: THTTPSend;
  Params: String;
begin
  Result := False;
  HTTP := THTTPSend.Create;
  try
    if gOAuth2.EMail = '' then
    begin
      logmemo.Lines.add('Not connected');
      exit;
    end;
  Params := '';
  if revisionid <> '' then
  Params := '/revisions/' + revisionid;
    HTTP.Headers.Add('Authorization: Bearer ' + gOAuth2.Access_token);
    if not HTTP.HTTPMethod('DELETE', 'https://www.googleapis.com/drive/v3/files/' + fileId + Params) then exit;
    if HTTP.ResultString = '' then Result := True;
    logmemo.Lines.add(HTTP.Headers.Text + #13 + HTTP.ResultString);
  finally
    HTTP.Free;
  end;

end;




function TGoogleDrive.AboutGdrive(version:apiver):TGoogleDriveInformation;
var
  HTTP: THTTPSend;
  response:tstringlist;
  P: TJSONParser;
  I: integer;
  J, D: TJSONData;
  vx:string;
begin
  HTTP := THTTPSend.Create;
  try
    if gOAuth2.EMail = '' then
    begin
      logmemo.Lines.add('Not connected');
      exit;
    end;

    case version of
       v2:vx:='v2';
       v3:vx:='v3';
    end;

    logmemo.lines.add('Retrieving Google Drive Informations...');
    if HTTP.HTTPMethod('GET','https://www.googleapis.com/drive/'+vx+'/about?access_token=' + gOAuth2.Access_token+'&fields=*') then
    begin
    response:=tstringlist.create;
    response.LoadFromStream(HTTP.Document);
    P := TJSONParser.Create(Response.Text);

    try
    J := P.Parse;
        Case version of

        v2:if Assigned(J) then
        begin
        with result do begin
        rootFolderId:=RetrieveJSONValue(J, 'rootFolderId');
        limit:=RetrieveJSONValueInt64(J, 'quotaBytesTotal');
        usage:=RetrieveJSONValueInt64(J, 'quotaBytesUsedAggregate');
        usageInDrive:=RetrieveJSONValueInt64(J, 'quotaBytesUsed');
        usageInDriveTrash:=RetrieveJSONValueInt64(J, 'quotaBytesUsedInTrash');
        end;
        end;

        v3:if Assigned(J) then
        begin
        D:=J.FindPath('storageQuota');
        if assigned(D) then
        with result do begin
        rootFolderId:='';
        limit:=RetrieveJSONValueInt64(D, 'limit');
        usage:=RetrieveJSONValueInt64(D, 'usage');
        usageInDrive:=RetrieveJSONValueInt64(D, 'usageInDrive');
        usageInDriveTrash:=RetrieveJSONValueInt64(D, 'usageInDriveTrash');
        end;
        end;

        end;

    finally
      P.Free;
      if assigned(J) then J.Free;
    end;

    end;
  finally
    HTTP.Free;
    Response.free;
  end;
   logmemo.lines.add('Done...');
end;


procedure TGoogleDrive.GetGFileRevisions(var A: TGFile);
begin
  setlength(A.revisions, 0);
  A.revisions := GetRevisions(A.fileid);
end;

function TGoogleDrive.GetRevisions(fileid: string): TGFileRevisions;
var
  Response: TStringList;
  URL: string;
  Params: string;
  P: TJSONParser;
  I: integer;
  J, D, E: TJSONData;
  F: TGFileRevisions;
begin
  (*
  {
  "kind": "drive#revision",
  "etag": etag,
  "id": string,
  "selfLink": string,
  "mimeType": string,
  "modifiedDate": datetime,
  "pinned": boolean,
  "published": boolean,
  "publishedLink": string,
  "publishAuto": boolean,
  "publishedOutsideDomain": boolean,
  "downloadUrl": string,
  "exportLinks": {
    (key): string
  },
  "lastModifyingUserName": string,
  "lastModifyingUser": {
    "kind": "drive#user",
    "displayName": string,
    "picture": {
      "url": string
    },
    "isAuthenticatedUser": boolean,
    "permissionId": string,
    "emailAddress": string
  },
  "originalFilename": string,
  "md5Checksum": string,
  "fileSize": long
}
  *)
  SetLength(F, 0);

  Response := TStringList.Create;
  try

    if gOAuth2.EMail = '' then
      exit;

    // https://developers.google.com/drive/v2/reference/files/list
    gOAuth2.LogLine('Retrieving revisions of the current file ' + fileid);
    URL := 'https://www.googleapis.com/drive/v2/files/' + fileid + '/revisions';
    Params := 'access_token=' + gOAuth2.Access_token;
    if HttpGetText(URL + '?' + Params, Response) then
    begin
      gOAuth2.DebugLine(Response.Text);
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

          //gOAuth2.LogLine('Busy retrieving file revisions');

          D := J.FindPath('items');
          gOAuth2.DebugLine(format('%d revisions currently available', [D.Count]));
          for I := 0 to D.Count - 1 do
          begin
            SetLength(F, length(F) + 1);

            with F[length(F) - 1] do
            begin
              id := fileid;
              revisionid := RetrieveJSONValue(D.Items[I], 'id');
              modifiedTime := RetrieveJSONValue(D.Items[I], 'modifiedTime');
              mimetype := RetrieveJSONValue(D.Items[I], 'mimeType');
              originalFileName := RetrieveJSONValue(D.Items[I], 'originalFilename');
              size := RetrieveJSONValue(D.Items[I], 'size');
              Application.ProcessMessages;
            end;
          end;

          gOAuth2.LogLine('Done');

        end;
        Result := F;
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



procedure TGoogleDrive.CreateFolder(foldername: string; parentid: string = '');
var
  HTTP: THTTPSend;
  s: string;
begin
  HTTP := THTTPSend.Create;
  try
    if foldername = '' then exit;
    if gOAuth2.EMail = '' then
    begin
      logmemo.Lines.add('Not connected');
      exit;
    end;

    ClearAllCustomProperties;
    AddCustomProperty(CustomBodyProperties,'name',foldername,asstring);

    if parentid <> '' then
    AddCustomProperty(CustomBodyProperties,'parents', '[{"id":"' + parentid + '"}]');

    AddCustomProperty(CustomBodyProperties,'mimeType','application/vnd.google-apps.folder',asstring);

    s := ExtractBodyproperties;

    WriteStrToStream(HTTP.Document, ansistring(s));
    logmemo.Lines.add(s);
    HTTP.Headers.Add('Authorization: Bearer ' + gOAuth2.Access_token);
    HTTP.MimeType := 'application/json; charset=UTF-8';
    if not HTTP.HTTPMethod('POST', 'https://www.googleapis.com/drive/v3/files') then exit;
    logmemo.Lines.add(HTTP.Headers.Text + #13 + HTTP.ResultString);
  finally
    HTTP.Free;
  end;
end;

end.
