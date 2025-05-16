unit httpserver;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  BrookHTTPServer, BrookHTTPRequest,
  BrookHTTPResponse, BrookURLRouter, BrookUtility;

type

  { THTTPServer }

  THTTPServer = class(TBrookHTTPServer)
  private
    FRouter : TBrookURLRouter;
  protected
    procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetupServer;
  end;


implementation

uses
  routevideo;

{ THTTPServer }

procedure THTTPServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  FRouter.Route(ASender, ARequest, AResponse);
end;

constructor THTTPServer.Create(AOwner: TComponent);
var
  rv : TRouteVideo;
begin
  inherited Create(AOwner);
  FRouter := TBrookURLRouter.Create(Self);
  rv := TRouteVideo.Create(FRouter.Routes);
  FRouter.Active := true;
end;

procedure THTTPServer.SetupServer;
var
  lst : TStringList;
begin
  Self.Port := 6443;

  lst := TStringList.Create;
  try
    lst.LoadFromFile('self_signed.pem');
    Self.Security.Certificate:= lst.Text;
    lst.LoadFromFile('self_signed_key.pem');
    Self.Security.PrivateKey:= lst.Text;
    Self.Security.Active:= true;
  finally
    lst.Free;
  end;
end;

end.

