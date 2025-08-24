# com.secresoft

This is the source code for https://secresoft.com

It's my fake "consultancy" home page, I'm not incorporated (yet -- and I don't really like the name anyway). While the screenshots in "Our Work" are from things
I've done, they were just lying around from old versions of my portfolio, and aren't from anything more recent. The filler text on the pages full of
self-important consultant language was created with minor direction by an early version of ChatGPT. (This was around the time that stable diffusion had their
first open source release, the logo was made with it.)

Recently I decided to finish adding a couple [Prometheus](https://github.com/deadtrickster/prometheus.cl) metrics to hook up to a Grafana dashboard, and clean
up some rough edges and publish this as an open source repo.

I don't think this is a terrible example of a Lisp webapp but it's not particularly great, either. In particular some dependencies are commented out because
while they were useful for other webapps they aren't useful here. But hopefully it can be a little use to someone else, even just the build and deploy section
that follows.

# License

This code is in the public domain, for full details see the Unlicense file.

# Building and Deploying

In theory, you can just run `sbcl --script build.lisp` and you'll get a binary. You can ship this binary to a server along with the `ref/`, `static/`, and `templates/`
folders, run it, and be good to go. You can change just the template files and they will automatically be recompiled by Djula and the changes reflected on the
live site.

I also used this site as my first test of using HTMX, just dropping it in and seeing just the body section of pages change on navigation. It's pretty cool.

In practice, there's more to it than just building a binary, though.

## Deploying

I just use `rsync` to copy the relevant files up to my server. As mentioned, if I just make a change to a template file, it's all good. Sometimes I've changed
Lisp code, though, so a new binary needs to be made. Once that's done, it's also copied up, and I restart the process with `sudo systemctl restart secresoft.service`.

Yes, alas, my hosting server uses systemd. I used to host on an EC2 gentoo box, but EC2 decide to spit on small time users like me, so I moved to OVH.
Everything's been great other than I have to use debian, but that's not so bad. The way I set things up so that if the box has to restart, my Lisp server comes
back up too, follows.

I made a new file `/lib/systemd/system/secresoft.service` with these contents:

```
[Unit]
Description=secresoft.com

[Service]
User=debian
ExecStart=/var/www/sites/launchers/secresoft.sh

[Install]
WantedBy=multi-user.target
```

That referenced shell script just contains:

```
#!/bin/bash
cd /var/www/sites/secresoft.com/
./secresoft.com
```

Next, I ran:

```
sudo systemctl daemon-reload
sudo systemctl enable secresoft.service
sudo systemctl start secresoft.service
sudo systemctl status secresoft.service
```

The enable command copies the systemd file to `/etc/systemd/system/multi-user.target.wants/`.
If you need to debug a stacktrace for some reason, you can see the output with `journalctl -u secresoft.service`. Or stop and run the binary directly.

As shown in the `environment.conf` file, this app also expects two log files to exist and be writeable in `/var/log/`. This will let you see HTTP logs.

I don't expose the server directly to the web, but reverse proxy it with Apache. (Why Apache? Because I've already long been using it for PHP and other sites,
so it's just there and works fine. Nginx or so on would work fine too.)

Configuring Apache is annoyingly distro-specific but here's most of my .conf for SSL (there's one for port 80 too but it's setup to just redirect to https).

```
<VirtualHost *:443>
  ServerAdmin admin@secresoft.com
  ServerName secresoft.com
  ServerAlias *.secresoft.com
  KeepAlive Off

  DocumentRoot /var/www/localhost/
  <Directory /var/www/localhost/>
    Require all granted
  </Directory>
  ErrorDocument 503 /secresoft-home.html
  ProxyPass /secresoft-home.html !

  ProxyRequests "Off"
  ProxyPreserveHost "On"
  AllowEncodedSlashes NoDecode

  ProxyPass / http://0.0.0.0:5312/
  ProxyPassReverse / http://0.0.0.0:5312/
  ProxyPassReverse / http://secresoft.com/

  RequestHeader set X-Forwarded-Proto https

  ErrorLog ${APACHE_LOG_DIR}/secresoft.com-error.log

  # Possible values include: debug, info, notice, warn, error, crit,
  # alert, emerg.
  LogLevel warn

  CustomLog ${APACHE_LOG_DIR}/secresoft.com-access.log combined


Include /etc/letsencrypt/options-ssl-apache.conf
SSLCertificateFile /etc/letsencrypt/live/secresoft.com/fullchain.pem
SSLCertificateKeyFile /etc/letsencrypt/live/secresoft.com/privkey.pem
</VirtualHost>
```

This gives me normal apache logs as well, and provides a fallback HTML page stored in /var/www/localhost/ (it's pretty blank) in case apache is up but the lisp
server isn't.

## Docker building

Another side effect of having to use debian for my hosting server is that a binary I build on my desktop (running gentoo) typically won't work anymore on the
debian server. This is because my gentoo's version of glibc is always quite recent, and debian's is old.

I don't want my server to know about Lisp and how to build it, all it sees is a binary. The solution I came up with was to build my binary locally in a docker
container running an old version of debian. I asked a newer version of an AI tool to vibe-code me up a shell script to handle this, once I set things up anyway.
Here it is, in case it's helpful:

```bash
#!/usr/bin/env bash
# Set to exit immediately on any command failure, treat unset vars as errors,
# and fail if either command fails in `cmd1 | cmd2`.
set -euo pipefail

if [[ $EUID -ne 0 ]]; then
    echo "Error: This script must be run as root" >&2
    exit 1
fi

usage() {
  cat <<EOF
Usage: $0 -b BIN_NAME [-s BUILD_SCRIPT]
  -b BIN_NAME      Name of the executable ASDF produces (required)
  -s BUILD_SCRIPT  Lisp build file to load (default: build.lisp)
EOF
  exit 1
}

BUILD_SCRIPT="build.lisp"
BIN_NAME=""

while getopts ":b:s:" opt; do
  case $opt in
    b) BIN_NAME="$OPTARG" ;;
    s) BUILD_SCRIPT="$OPTARG" ;;
    *) usage ;;
  esac
done

if [[ -z "$BIN_NAME" ]]; then
  echo "ERROR: -b BIN_NAME is required." >&2
  usage
fi

CHROOT_DIR="/fornost/devstorage/debian-chroot"
CHROOT_BUILD_SUBDIR="lisp-build"

SRC_DIR="$(realpath .)"
DEST_DIR="$CHROOT_DIR/$CHROOT_BUILD_SUBDIR"

cleanup() {
  echo "Cleaning up mounts and temp build files"
  # (mountpoint -q checks that it's actually mounted)
  for m in proc sys dev; do
    mountpoint -q "$CHROOT_DIR/$m" && umount "$CHROOT_DIR/$m"
  done
  rm -rf "$DEST_DIR"
}
trap cleanup EXIT

echo "Copying project to $DEST_DIR"
rm -rf "$DEST_DIR"
mkdir -p "$DEST_DIR"
#cp -ra "$SRC_DIR/"* "$DEST_DIR/"
rsync -a \
  --filter=':- .gitignore' \
  --exclude='.git' \
  "$SRC_DIR/" "$DEST_DIR/"

echo "Preparing to chroot"
mount -t proc proc  "$CHROOT_DIR/proc"
mount -t sysfs sysfs "$CHROOT_DIR/sys"
mount --bind /dev    "$CHROOT_DIR/dev"
cp /etc/hosts        "$CHROOT_DIR/etc/hosts"

echo "Entering chroot and building"
chroot "$CHROOT_DIR" /bin/bash <<-EOF
  set -euo pipefail
  echo -n "SBCL path: "
  which sbcl
  echo -n "SBCL version: "
  sbcl --version

  echo -n "Quicklisp dist: "
  sbcl --eval '(princ (first (ql-dist:available-versions (ql-dist:dist "quicklisp"))))' --quit | tail -n 1

  echo
  echo
  echo "== Building =="
  echo

  cd "/$CHROOT_BUILD_SUBDIR"
  sbcl \
    --no-sysinit --no-userinit \
    --load build.lisp \
    --quit
EOF

echo
echo "Copying $BIN_NAME back to $SRC_DIR"
cp "$DEST_DIR/$BIN_NAME" "$SRC_DIR/"
chown --reference="$SRC_DIR" "$SRC_DIR/$BIN_NAME"

echo "Done"
```

It copies the project over to the debian location, chroot's into it, runs the build script, then copies the binary back out.

I'll probably later port over some code from my broken jenkins server that fetches and uses the latest SBCL as well, just to avoid having to update
SBCL on the debian docker instance too often or finding interesting broken issues not present in my development version.

# Metrics

Besides the main application server, I also spin up a metrics server that exposes an endpoint meant for Prometheus to consume.

I added to the end of my `/etc/prometheus/prometheus.yml` file in the `scrape_configs` section:

```
  - job_name: 'secresoft.com'
    static_configs:
      - targets: ['localhost:9101']
```

Restarted the service, and setup a Grafana dashboard. It started life as being based on https://github.com/deadtrickster/prometheus.cl/blob/master/dashboards/HunchentootSBCL.json
Obviously change the app to be different, and the job= to be whatever the job name was in the yml. I got rid of the given queries around http requests and
instead have written some of my own. As this site is basically not visited by anyone (by design) it doesn't really matter, more for proof of concept. Some
metrics I have panels for though are:

* Total request count by route
* Average response time by route
* 90th percentile responses by path
* Slowest endpoints (p90)
* Total request rate

The SBCL dynamic memory and breakdown, threads & cpu, and file descriptors are mildly interesting to have too. Make sure to change the units on the memory ones.

There's some commentary in src/metrics.lisp. You may find it more valuable to look at https://gist.github.com/Jach/a79159518da5b1eebc1de788505d2dcd though. It's
a short demonstration from a REPL context of how to use prometheus metrics.

In this project I took the stance of having my requests counter and durations metrics be "total", i.e. for the lifetime of the server. There's an argument to be
made that it can also be useful sometimes to have them "reset" after each metric sample in the time series, so most of the time they'll be 0 unless you get some
requests over a sampling period.
