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

implementation
uses
  standardheaders;

{ TRouteSpecies }

procedure TRouteSpecies.DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  AddStandardHeaders(aResponse);
  AResponse.Send('["Zog", "Gleep", "Bloop"]', 'application/json; charset=utf-8', 200);
end;

procedure TRouteSpecies.AfterConstruction;
begin
  Methods:= [rmGET];
  Pattern:= '/species';
end;

end.
