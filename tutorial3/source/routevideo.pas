unit routevideo;

{$mode ObjFPC}{$H+}

interface

uses
  BrookUtility,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookURLRouter;

type

  { TRouteVideo }

  TRouteVideo = class(TBrookURLRoute)
  protected
    procedure DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
  public
    procedure AfterConstruction; override;
  end;

implementation

uses
  sysutils,
  standardheaders, standardresponses;

{ TRouteVideo }

procedure TRouteVideo.DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if HandleOptions(ARoute, ARequest, AResponse) then
    exit;
  AddStandardHeaders(AResponse);
  AResponse.Headers.Add('Content-Type', 'video/mp4');
  AResponse.Render('sample.mp4');
end;

procedure TRouteVideo.AfterConstruction;
begin
  Methods:= [rmGET];
  Pattern:= '/video';
end;

end.

