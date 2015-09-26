# Introduction

Eventually, this repository will just contain an implementation of various social
authentication protocols in common lisp.  Right now, the only supported protocols
are OpenID Connect and Facebook Login.  The OpenID Connect implementation has only
been tested against Google.  More testing to follow.

This repository also contains a "demo" application that uses the openid-connect
library: a self-hostable rss reader. Eventually, this project will be split into its
own repository, leaving this one for the authentication part.  Also, eventually this
library will have a better name.

# Installation notes

This project implements the OpenId Connect API. In its current state, it can
authenticate a user against Google and display the informaiton Google sends
back. The only tricky requirement it has is cljwt, a library for parsing JSON
Web Tokens, which is not in quicklisp but can be gotten from
<https://github.com/fiddlerwoaroof/cljwt>. (This is my fork, I've made a couple
changes since the original library didn't support the signature algorithm Google
uses).

(c) 2015 Edward Langley, distributed under a 2-clause BSD License
