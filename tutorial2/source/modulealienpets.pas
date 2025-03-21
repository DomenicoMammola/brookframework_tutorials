unit modulealienpets;

{$mode ObjFPC}{$H+}
interface

uses
  BrookUtility,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookURLRouter;


type

  { TRouteSpecies }

  TRouteSpecies = class(TBrookURLRoute)
  protected
    procedure DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
  public
    procedure AfterConstruction; override;
  end;

  { TRoutePetAliens }

  TRoutePetAliens = class(TBrookURLRoute)
  protected
    procedure DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
  public
    procedure AfterConstruction; override;
  end;

  TRoutePetAlien = class(TBrookURLRoute)
  strict private
    procedure Post(ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    procedure Put(ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    procedure Delete(const aPetId: integer; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
  protected
    procedure DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
  public
    procedure AfterConstruction; override;
  end;


implementation
uses
  fpjson, SysUtils,
  standardheaders, standardresponses, alienpets;

var
  pets : TAlienPetsArchive;

procedure InitAlienPetsArchive;
var
  tmp : TAlienPet;
begin
  tmp := TAlienPet.Create;
  tmp.Id:= 1;
  tmp.Name:= 'Prootelon';
  tmp.Species:= 'Zog';
  pets.Add(tmp);

  tmp := TAlienPet.Create;
  tmp.Id:= 2;
  tmp.Name:= 'Badelon';
  tmp.Species:= 'Bloop';
  pets.Add(tmp);

  tmp := TAlienPet.Create;
  tmp.Id:= 3;
  tmp.Name:= 'Evilon';
  tmp.Species:= 'Gleep';
  pets.Add(tmp);
end;


{ TRouteSpecies }

procedure TRouteSpecies.DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if HandleOptions(ARoute, ARequest, AResponse) then
    exit;
  AddStandardHeaders(aResponse);
  AResponse.Send('["Zog", "Gleep", "Bloop"]', 'application/json; charset=utf-8', 200);
end;

procedure TRouteSpecies.AfterConstruction;
begin
  Methods:= [rmGET, rmOPTIONS];
  Pattern:= '/species';
end;

{ TRoutePetAliens }

procedure TRoutePetAliens.DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if HandleOptions(ARoute, ARequest, AResponse) then
    exit;
  AddStandardHeaders(AResponse);
  AResponse.Send('{"pets":' + pets.ToJson + '}', 'application/json', 200);
end;

procedure TRoutePetAliens.AfterConstruction;
begin
  Methods:= [rmGET, rmOPTIONS];
  Pattern:= '/alienpets';
end;

procedure TRoutePetAlien.Post(ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  str : String;
  jData, subData : TJSONData;
begin
  if ARequest.Payload.Text <> '' then
  begin
    str := ARequest.Payload.Text;
    jData := GetJSON(str);
    try
      //subData := jData.FindPath('referent');
      //if Assigned(subData) then
      //begin
      //  if not LeggiReferenteDaJson(referente, subData) then
      //    malformed := true;
      //end
      //else
      //  malformed := true;
    finally
      jData.Free;
    end;
  end
  else
    AResponse.Send('Invalid request', 'text/plain', 400);
end;

procedure TRoutePetAlien.Put(ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin

end;

procedure TRoutePetAlien.Delete(const aPetId: integer; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin

end;

procedure TRoutePetAlien.DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  tmpId : integer;
begin
  if HandleOptions(ARoute, ARequest, AResponse) then
    exit;
  AddStandardHeaders(AResponse);

  tmpId := 0;
  if (Length(ARoute.Segments) >= 1) then
    tmpId := StrToInt(ARoute.Segments[Length(ARoute.Segments)-1]);

  if ARequest.Method = 'POST' then
    Post(ARoute, ARequest, AResponse)
  else if ARequest.Method = 'PUT' then
    Put(ARoute, ARequest, AResponse)
  else if ARequest.Method = 'DELETE' then
    Delete(tmpId, ARoute, ARequest, AResponse);
end;

procedure TRoutePetAlien.AfterConstruction;
begin
  Methods:= [rmGET, rmPOST, rmPUT, rmDELETE, rmOPTIONS];
  Pattern := '/alienpet/([[0-9]*)';
end;


initialization
  pets := TAlienPetsArchive.Create;
  InitAlienPetsArchive;

finalization
  pets.Free;

end.
