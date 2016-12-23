unit google_drive;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, Forms, google_oauth2, fpjson, jsonparser, memds;

type
  TGoogleDrive = class(TMemDataSet)
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
  FieldDefs.Add('title', ftString, 25, False);
  FieldDefs.Add('description', ftString, 255, False);
  FieldDefs.Add('created', ftString, 255, False);
  FieldDefs.Add('modified', ftString, 255, False);
  FieldDefs.Add('downloadurl', ftString, 255, False);
  FieldDefs.Add('filename', ftString, 255, False);
  FieldDefs.Add('md5', ftString, 255, False);
  FieldDefs.Add('filesize', ftString, 20, False);
  CreateTable;

  gOAuth2 := TGoogleOAuth2.Create(client_id, client_secret);

end;

destructor TGoogleDrive.Destroy;
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

    if gOAuth2.EMail = '' then exit;

    // https://developers.google.com/drive/v2/reference/files/list
    gOAuth2.LogLine('Retrieving filelist ' + gOAuth2.EMail);
    URL := 'https://www.googleapis.com/drive/v2/files';
    Params := 'access_token=' + gOAuth2.Access_token;
    Params := Params + '&maxResults=1000';
    Params := Params + '&orderBy=folder,modifiedDate%20desc,title';
    if HttpGetText(URL + '?' + Params, Response) then
    begin
      // gOAuth2.DebugLine(Response.Text);

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
            FieldByName('description').AsString := RetrieveJSONValue(D.Items[I], 'description');
            FieldByName('created').AsString := RetrieveJSONValue(D.Items[I], 'createdDate');
            FieldByName('modified').AsString := RetrieveJSONValue(D.Items[I], 'modifiedDate');
            FieldByName('downloadurl').AsString := RetrieveJSONValue(D.Items[I], 'downloadUrl');
            FieldByName('filename').AsString := RetrieveJSONValue(D.Items[I], 'originalFilename');
            FieldByName('md5').AsString := RetrieveJSONValue(D.Items[I], 'md5Checksum');
            FieldByName('filesize').AsString := RetrieveJSONValue(D.Items[I], 'fileSize');
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

end.

