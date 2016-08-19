# TLS, HPKP, and HSTS

I've put some effort in to ensuring that the transport security for this site leverages the latest technologies available. To that end, I've enabled HSTS (to ensure that client browsers know to only request this site over HTTPS) and HPKP (to instruct client browsers to only accept a connection made with the correct public key).

![](/file/hpkp.png)

To get the fingerprint to use for pinning, I used OpenSSL:

~~~~ {.bash}
openssl x509 -noout -in cert.pem -pubkey | openssl asn1parse -noout -inform pem -out pub.key
openssl dgst -sha256 -binary pub.key | openssl enc -base64
~~~~

With that hash in hand, I was able to set the HPKP header in my Apache reverse proxy by adding this directive:

~~~~ {.apache}
Header set Public-Key-Pins "pin-sha256=\"bkA5AE46qiFRYaIPvCRXq0xu7XOyyXlByc+NCfjJzWs=\"; pin-sha256=\"RLt6ujrv+Dt/VTHoDkkeaFbVTX59IotW0U9AInf6TPQ=\"; max-age=2592000; includeSubDomains"
~~~~

Having a backup key is important in case something goes awry with the primary. In this case, I only have one purchased certificate for this site, so I just used my own CA-signed (jdt.io) certificate as a backup. That'll work for me since my browser trusts that root (even if no one else's does).

HSTS is set similarly:

~~~~ {.apache}
Header always set Strict-Transport-Security "max-age=63072000; includeSubdomains; preload"
~~~~

I've included the preload directive even though this site isn't actually preloaded. I may submit it at some point just to be fancy, but I don't want to generate unnecessary work for the browser folks for this vanity site.

In the interest of completeness, here are my TLS directives and cipher list:

~~~~ {.apache}
SSLProtocol all -SSLv2 -SSLv3
SSLHonorCipherOrder on
SSLCipherSuite "EECDH+ECDSA+AESGCM EECDH+aRSA+AESGCM EECDH+ECDSA+SHA384 EECDH+ECDSA+SHA256 EECDH+aRSA+SHA384 EECDH+aRSA+SHA256 EECDH EDH+aRSA !aNULL !eNULL !LOW !3DES !MD5 !EXP !PSK !SRP !DSS !RC4"
~~~~

I suspect that I can simplify that, but that list works well for now.

Next I plan to enable CSP, which should be trivial since I'm not using any JavaScript at all on this site yet. There may be some weirdness with in-line CSS, but I suspect it shouldn't be too bad.

I may work on writing a handler for `report-uri` submissions (from HPKP and CSP) before I dive in to that. I'll do that in Haskell with Scotty and Persistent - it seems like a straightforward task.
