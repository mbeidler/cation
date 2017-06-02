# cation

A full-stack Haskell web application template. 

cation uses [servant](http://haskell-servant.readthedocs.io/en/stable/) and 
[persistent](https://hackage.haskell.org/package/persistent) on the server, and 
[react-flux](https://hackage.haskell.org/package/react-flux) on the client. It also contains a sample endpoint to demonstrate documentation with [servant-swagger](https://hackage.haskell.org/package/servant-swagger), using existing React components, etc.

See the [slides](http://slides.com/mbeidler/pdxfunc-haskell#/) from my recent talk at pdxfunc.

## cation-server

`cation-server` by default runs on port **8081**, but you can choose a different one by exporting a `PORT` environment variable.

#### Database

`cation-server` uses PostgreSQL and builds on the work of `servant-persistent`.

On Debian-based systems, the following is all that is needed to be able to run the application. `persistent` creates the tables 
if they don't exist and performs any necessary migrations if they do.

```bash
sudo apt-get install postgres libpq-dev
sudo -u postgres createuser -se test
sudo -u postgres psql -c "alter role test with password 'test'"
sudo -u postgres psql -c "create database cation"
```

**Building**

```bash
cd cation-server
stack setup
stack build
```

**Running**

```bash
stack exec cation-server-exe
```

## cation-client

You can use the `http-server` **npm** package to serve the app.

We made a `go` script in **package.json** that runs a stack build, copies the **all.js** output and launches **index.html**. 
Note: if you're running on Windows, you'll need to be using *cygwin* / *mingw* / *msys*.

```bash
cd cation-client
stack setup
stack build
npm install
npm install -g browserify http-server
browserify imports.js -o bundle.js
npm run go
```

To run GHCJS interactive mode:

```bash
npm install -g socket.io
```

```bash
cd cation-client
stack ghci
```

Now open a browser to **localhost:6400** to connect to GHCJSi.

You can use interactive mode from emacs by using the `exec-path-from-shell` package:

```elisp
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "NODE_PATH")
```

After adding those to your **~/.emacs** file, you can use `C-c C-l`. Launch your browser as described above.
