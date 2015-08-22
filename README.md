This project implements the OpenId Connect API. In its current state, it can
authenticate a user against Google and display the informaiton Google sends
back. The only tricky requirement it has is cljwt, a library for parsing JSON
Web Tokens, which is not in quicklisp but can be gotten from
<https://github.com/fiddlerwoaroof/cljwt>. (This is my fork, I've made a couple
changes since the original library didn't support the signature algorithm Google
uses).

(c) 2015 Edward Langley, distributed under a 2-clause BSD License
