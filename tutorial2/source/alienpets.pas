unit alienpets;

{$mode ObjFPC}{$H+}

interface

uses
  Contnrs, fpjson, syncobjs;

type

  { TAlienPet }
  TAlienPet = class
  strict private
    FId : integer;
    FName : string;
    FSpecies : string;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(const aSource : TAlienPet);
    procedure FromJson (aJsonData : TJSONData); overload;
    procedure FromJson (const aJsonString : string); overload;
    function ToJson: String;

    property Id : integer read FId write FId;
    property Name : string read FName write FName;
    property Species : string read FSpecies write FSpecies;
  end;

  { TAlienPetsArchive }

  TAlienPetsArchive = class
  strict private
    FList : TObjectList;
    FCriticalSection : TCriticalSection;
    function Get(const aIndex : integer): TAlienPet;
  public
    constructor Create;
    destructor Destroy; override;
    function ToJson: String;

    procedure Add(const aAlienPet : TAlienPet);
    function GetCopy(const aIndex : integer): TAlienPet;
    function GetCopyById (const aId : integer) : TAlienPet;
    procedure Delete (const aId : integer);
    function Count : integer;
  end;


implementation

uses
  SysUtils;

{ TAlienPet }

procedure TAlienPet.FromJson(aJsonData : TJSONData);
var
  tmp : TJSONData;
begin
  Clear;
  tmp := aJsonData.FindPath('id');
  if Assigned(tmp) then
    Self.FId := tmp.Value;
  tmp := aJsonData.FindPath('name');
  if Assigned(tmp) then
    Self.FName := tmp.Value;
  tmp := aJsonData.FindPath('species');
  if Assigned(tmp) then
    Self.FSpecies := tmp.Value;
end;

procedure TAlienPet.FromJson(const aJsonString: string);
var
  jData, subdata : TJSONData;
begin
  jData := GetJSON(aJsonString);
  try
    subData := jData.FindPath('alienpet');
    if Assigned(subData) then
      fromJson(subData)
    else
      Clear;
  finally
    jData.Free;
  end;
end;

function TAlienPet.ToJson: String;
begin
  Result := '{"id":' + IntToStr(Id) + ', "name":"' + FName + '", "species":"' + FSpecies + '"}';
end;

procedure TAlienPet.Clear;
begin
  FId := 0;
  FName := '';
  FSpecies := '';
end;

procedure TAlienPet.Assign(const aSource: TAlienPet);
begin
  FId := aSource.Id;
  FName := aSource.Name;
  FSpecies := aSource.Species;
end;

constructor TAlienPet.Create;
begin
  Clear;
end;

{ TAlienPetsArchive }

constructor TAlienPetsArchive.Create;
begin
  FList := TObjectList.Create(true);
  FCriticalSection := TCriticalSection.Create;
end;

destructor TAlienPetsArchive.Destroy;
begin
  FCriticalSection.Free;
  FList.Free;
  inherited Destroy;
end;

function TAlienPetsArchive.ToJson: String;
var
  i : integer;
  sep : String;
begin
  sep := '';
  Result := '[';
  FCriticalSection.Acquire;
  try
    for i := 0 to FList.Count - 1 do
    begin
      Result := Result + sep + Get(i).ToJson;
      sep := ',';
    end;
  finally
    FCriticalSection.Leave;
  end;
  Result := Result + ']';
end;

procedure TAlienPetsArchive.Add(const aAlienPet: TAlienPet);
begin
  FCriticalSection.Acquire;
  try
    FList.Add(aAlienPet);
  finally
    FCriticalSection.Leave;
  end;
end;

function TAlienPetsArchive.GetCopy(const aIndex: integer): TAlienPet;
begin
  Result := TAlienPet.Create;
  Result.Assign(Get(aIndex));
end;

function TAlienPetsArchive.Get(const aIndex: integer): TAlienPet;
begin
  Result := FList.Items[aIndex] as TAlienPet;
end;

function TAlienPetsArchive.GetCopyById(const aId: integer): TAlienPet;
var
  i : integer;
begin
  Result := nil;
  FCriticalSection.Acquire;
  try
    for i := 0 to FList.Count - 1 do
    begin
      if Get(i).Id = aId then
      begin
        Result := TAlienPet.Create;
        Result.Assign(Get(i));
        exit;
      end;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TAlienPetsArchive.Delete(const aId: integer);
var
  i : integer;
begin
  FCriticalSection.Acquire;
  try
    for i := 0 to FList.Count - 1 do
    begin
      if Get(i).Id = aId then
      begin
        FList.Delete(i);
        exit;
      end;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

function TAlienPetsArchive.Count: integer;
begin
  FCriticalSection.Acquire;
  try
    Result := FList.Count;
  finally
    FCriticalSection.Leave;
  end;
end;

end.
