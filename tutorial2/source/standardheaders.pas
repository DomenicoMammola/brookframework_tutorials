unit standardheaders;

{$mode ObjFPC}{$H+}

interface

uses
  BrookHTTPResponse;

procedure AddStandardHeaders(AResponse: TBrookHTTPResponse);

implementation

procedure AddStandardHeaders(AResponse: TBrookHTTPResponse);
begin
  //{$IFDEF DEVELOPMENT}
  AResponse.Headers.Add('Access-Control-Allow-Origin', '*');
  //{$ENDIF}
  AResponse.Headers.Add('Server', 'Alien puppies server');
  AResponse.Headers.Add('Keep-Alive', 'timeout=5, max=99');
  AResponse.Headers.Add('Connection', 'Keep-Alive');
  AResponse.Headers.Add('X-Frame-Options', 'SAMEORIGIN'); // https://stackoverflow.com/questions/27358966/how-can-i-set-x-frame-options-on-an-iframe
end;
end.

