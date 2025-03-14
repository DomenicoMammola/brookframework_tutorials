# Tutorial 1 - Brookframework

Brookframework è una libreria per FreePascal e per Delphi che consente di sviluppare server web e microservizi basati sul protocollo *http*.

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

Apri un terminale in quella cartella ed effettua il clone della libreria Brookframework con il comando:

```
git clone https://github.com/risoflora/brookframework.git
```

Ora aggiungi la sottocartella `brookframework/Source` ai paths del progetto in modo tale che il codice sorgente della libreria sia utilizzabile nel progetto.

Scarica la libreria Sagui dal repository: [https://github.com/risoflora/libsagui/releases](https://github.com/risoflora/libsagui/releases). Scegli la versione più recente (al momento in cui scrivo questo tutorial l'ultima release disponibile è la *v3.5.2*).

Dovrai ovviamente scegliere il pacchetto adatto alla tua architettura. Ad esempio se sviluppi in linux il file giusto è `libsagui-3.5.2-linux_amd64.tar.gz`, per windows invece servirà il pacchetto `libsagui-3.5.2-windows_amd64.zip`.

Per windows sarà sufficiente copiare il contenuto del file zip nella cartella in cui è salvato il progetto.

In linux invece andranno eseguiti questi comandi:
```
sudo cp libsagui.so.3.5.2 /usr/lib
cd /usr/lib
sudo ln -s libsagui.so.3.5.2 libsagui.so.3
```

### Passiamo finalmente al codice...

I webservice costruiti tramite il Brookframework poggiano su 2 pilastri:
  * il primo pilastro è la classe `TBrookHTTPServer` che rappresenta il cuore del webservice: è il contenitore di una serie di parametri fondamentali, come ad esempio la porta su cui il servizio verrà esposto, ed ha un metodo `Open` per attivare il webservice;
  * la classe `TBrookURLRoute` da cui dovranno essere derivate diverse classi figlie, una per ogni route che vorremo implementare per organizzare le API del nostro webservice. Una istanza di ognuna di queste classi dovrà essere creata durante il setup del webservice e poi aggiunta all'istanza della classe `TBrookURLRouter` a cui verrà delegata l'orchestrazione del routing quando il webservice sarà attivo.


Per questo primo progetto decidiamo di utilizzare un'unica route: `/ping`.

Aggiungiamo una nuova unit `routeping` al progetto e definiamo una nuova classe, derivata da `TBrookURLRoute`:

``` pascal
TRoutePing = class(TBrookURLRoute)
protected
  procedure DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
public
    procedure AfterConstruction; override;
end;
```

Il metodo `AfterConstruction` è così definito per la nostra route che risponderà solo a chiamate http di tipo *GET*:

``` pascal
procedure TRoutePing.AfterConstruction;
begin
  Methods:= [rmGET];
  Pattern:= '/ping';
end;
```

Il router, quando verrà richiesta la route definita, eseguirà il corrispondente metodo `DoRequest`: 

``` pascal
procedure TRoutePing.DoRequest(ASender: TObject; ARoute: TBrookURLRoute; ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  AResponse.Send('<html><head><title>Ping</title></head><body>Pong</body></html>', 'text/html; charset=utf-8', 200);
end; 
```

