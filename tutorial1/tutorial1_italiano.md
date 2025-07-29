# Tutorial 1 - Il framework Brook

[Brook](https://github.com/risoflora/brookframework) è un framework per FreePascal e per Delphi che consente di sviluppare server web e microservizi basati sul protocollo *http*.

La libreria internamente utilizza un'altra libreria, scritta in C, che si chiama [Sagui](https://risoflora.github.io/libsagui/).

Sagui a sua volta internamente si appoggia alla libreria GNU [Libmicrohttpd](https://www.gnu.org/software/libmicrohttpd/).

Dal sito GNU:
```
GNU libmicrohttpd is a small C library that makes it easy to run an HTTP server as part of another application. GNU Libmicrohttpd is free software and part of the GNU project.
```

## Il primo progetto in Lazarus

Iniziamo creando un piccolissimo server web.

Questo esercizio sarà utile per imparare a configurare un progetto e per muovere i primi passi con la libreria.

Apri Lazarus e crea un nuovo progetto di tipo *Applicazione console* con questi parametri:

![New console application options](printscreens/new_console_application.png)

Salva il progetto in una cartella come *TestProject.dpi*.

Apri un terminale in quella cartella ed effettua il clone del framework Brook con il comando:

```
git clone https://github.com/risoflora/brookframework.git
```

Ora aggiungi la sottocartella `brookframework/Source` ai paths del progetto in modo tale che il codice sorgente della libreria sia richiamabile.

Scarica la libreria Sagui dal repository: [https://github.com/risoflora/libsagui/releases](https://github.com/risoflora/libsagui/releases). Scegli la versione più recente (mentre scrivo questo tutorial l'ultima release disponibile è la *v3.5.2*).

Dovrai ovviamente scegliere il pacchetto adatto alla tua architettura. Ad esempio se sviluppi su un pc con linux il file giusto è `libsagui-3.5.2-linux_amd64.tar.gz`, per windows invece servirà il pacchetto `libsagui-3.5.2-windows_amd64.zip`.

Per windows sarà sufficiente copiare il contenuto del file zip nella cartella in cui è salvato il progetto.

In linux invece andranno eseguiti questi comandi:
```
sudo cp libsagui.so.3.5.2 /usr/lib
cd /usr/lib
sudo ln -s libsagui.so.3.5.2 libsagui.so.3
```

### Passiamo finalmente al codice...

I webservice costruiti tramite il framework Brook poggiano su 2 pilastri:
  * il primo pilastro è la classe `TBrookHTTPServer` che rappresenta il cuore del webservice: è il contenitore di una serie di parametri fondamentali, come ad esempio la porta su cui il servizio verrà esposto, ed ha un metodo `Open` per attivare il webservice;
  * la classe `TBrookURLRoute` da cui dovranno essere derivate diverse classi figlie: una per ogni route che vorremo implementare per organizzare le API del nostro webservice. Una istanza di ognuna di queste classi dovrà essere creata durante il setup del webservice e poi aggiunta all'istanza della classe `TBrookURLRouter` a cui verrà delegata l'orchestrazione del routing quando il webservice sarà attivo.


Per questo primo progetto vogliamo utilizzare un'unica route: `/ping`.

Aggiungi quindi una nuova unit `routeping` al progetto e definisci una nuova classe derivata da `TBrookURLRoute`:

``` pascal
TRoutePing = class(TBrookURLRoute)
protected
  procedure DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
public
    procedure AfterConstruction; override;
end;
```

Il metodo `AfterConstruction` definisce i parametri della route. Vogliamo che la nostra route risponda solo a chiamate http di tipo *GET* quando viene richiesto il percorso */ping*:

``` pascal
procedure TRoutePing.AfterConstruction;
begin
  Methods:= [rmGET];
  Pattern:= '/ping';
end;
```

Il router, quando il match con una delle route definite sarà positivo, eseguirà il corrispondente metodo `DoRequest` che nel nostro esempio sarà: 

``` pascal
procedure TRoutePing.DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  AResponse.Send('<html><head><title>Ping</title></head><body>Pong</body></html>', 'text/html; charset=utf-8', 200);
end; 
```

Aggiungi al progetto una nuova unit `httpserver` e definisci una nuova classe derivata da `TBrookHTTPServer`:

``` pascal
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
```

Questa classe ha il compito di mettere assieme tutto: i componenti del framework Brook e le route definite nel progetto, per cui inserisci questo codice nel costruttore:

``` pascal
constructor THTTPServer.Create(AOwner: TComponent);
var
  rp : TRoutePing;
begin
  inherited Create(AOwner);
  FRouter := TBrookURLRouter.Create(Self);
  rp := TRoutePing.Create(FRouter.Routes);
  FRouter.Active := true;
end;
```

Il costruttore crea una istanza del router del framework e carica al suo interno le route definite. L'ownership delle route viene passata al router stesso per cui non dovremo preoccuparci di richiamarne la Free.

Il metodo `DoRequest` della classe è quello richiamato dal framework per rispondere alle chiamate. Nel nostro esempio potrà funzionare da semplice passacarte:

``` pascal
procedure THTTPServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  FRouter.Route(ASender, ARequest, AResponse);
end; 
```

Il metodo SetupServer ci servirà per inizializzare i parametri di connessione:

``` pascal
procedure THTTPServer.SetupServer;
begin
  Self.Port := 8080;
end; 
```

Ora è sufficiente inserire nel codice dell'applicazione la creazione e l'attivazione del server appena creato:

``` pascal
procedure TBrookframeworkTest.DoRun;
var
  server: THTTPServer;
begin
  server := THTTPServer.Create(nil);
  try
    server.SetupServer;
    server.Open;
    if not server.Active then
    begin
      WriteLn('Unable to start server at http://localhost:', server.Port);
      Terminate(-1);
    end
    else
    begin
      WriteLn('Server running at http://localhost:', server.Port);
      ReadLn;
    end;
  finally
    server.Free;
  end;
  Terminate;
end; 
```

Esegui il progetto e prova a connetterti col browser all'indirizzo http://localhost:8080/ping. Quello che dovresti vedere è:

![Route ping in browser](printscreens/localhost.png)

Il server sta funzionando ed ha risposto correttamente alla richiesta del browser.

### Tutto dev'essere criptato!

Al giorno d'oggi le connessioni dovrebbero essere tutte criptate e quindi vediamo come trasformare il nostro server http in uno https.

Il primo passaggio da fare è procurarsi un certificato ssl self-signed.

Apri una shell nella cartella del progetto e digita il seguente comando (openssl dovrà essere installato sul tuo sistema):

```
openssl req -x509 -nodes -days 1000 -newkey rsa:2048 -keyout self_signed_key.pem -out self_signed.pem
```

Sarà necessario anche procurarsi la versione della libreria Sagui col supporto tls. Il repository come abbiamo visto prima è [https://github.com/risoflora/libsagui/releases](https://github.com/risoflora/libsagui/releases), questa volta però andrà scaricato l'archivio `libsagui_tls-3.5.2-linux_amd64.tar.gz` per linux o `libsagui_tls-3.5.2-windows_amd64.zip` per windows. Andranno poi ripetute le operazioni di deploy precedentemente descritte.

Se usi windows sarà necessario installare anche le dll della libreria `gnutls`. Mentre scrivo l'ultima disponibile per windows a 64bit si può scaricare da qui: [https://github.com/risoflora/libsagui/releases/download/v3.5.1/gnutls-3.8.6-mingw_amd64.zip](https://github.com/risoflora/libsagui/releases/download/v3.5.1/gnutls-3.8.6-mingw_amd64.zip). Nuovamente il contenuto dello zip andrà copiato nella cartella in cui è salvato il progetto.

Una volta generato il certificato e messo a posto la libreria, possiamo dedicarci alle modifiche al codice.

La procedura che fa il setup della classe `THTTPServer` dovrà essere così modificata:

``` pascal
procedure THTTPServer.SetupServer;
var
  lst : TStringList;
begin
  Self.Port := 443;

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
```

A livello di codice basta questo. Riesegui il programma e prova ad aprire col browser l'indirizzo https://localhost/ping.

Sarà necessario aggiungere un'eccezione di sicurezza nel browser, il certificato è self-signed e quindi nessuno può garantirne l'autenticità, fatto questo però quello che dovresti vedere è:

![Route ping in browser https](printscreens/localhost_https.png)

Nel wiki della libreria Brook è descritto come utilizzare un certificato vero su un server Windows connesso ad internet: https://github.com/risoflora/brookframework/wiki/How-to-configure-TLS-in-Brook-5-Framework-on-a-Microsoft-Windows-Server

## Cosa abbiamo visto
  * come fare il setup di un progetto che utilizza il framework Brook
  * come creare un semplice servizio che risponda ad una chiamata http
  * come implementare lo stesso servizio utilizzando https

  ## Dove trovare il codice sorgente
Il codice sorgente di questo tutorial è disponibile a questo indirizzo: [https://github.com/DomenicoMammola/brookframework_tutorials](https://github.com/DomenicoMammola/brookframework_tutorials)
  