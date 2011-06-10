Overview
========
Sytyc is a web-based problem database with a compiler on the server side.
Only Andrew Rock's [MaSH compiler](http://www.ict.griffith.edu.au/arock/MaSH/index.html)
is supported at the moment, but Sytyc will happily run any compiler that the
server can run, if configured correctly.

Github repository: https://github.com/SpaXe/Sytyc

Screenshot
----------
![Sytyc Problem Sample Screen](https://github.com/SpaXe/Sytyc/raw/master/images/screenshot.jpg)

Credits
-------
Author: Xavier Ho contact@xavierho.com

Supervisor: Andrew Rock a.rock@griffith.edu.au

And other Functional Programming classmates.

Installation & Usage
--------------------
@TODO

Sytyc is not thread-safe. You should not compile and run it with -thread flag.

Dependencies
------------
Haskell dependencies:

  * GHC version >= 7.0.3. As of Feb 2011, the official Haskell Platform 
  supports GHC 7.0.3. Sytyc was compiled against the GHC libraries.
  
  * pandoc-1.8
  
  * missingH-1.1.0.3

Server dependencies:

  * Apache, or other server programs that support CGI.

Other dependencies:

  * Java SDK SE 6
  
  * MaSH compiler http://www.ict.griffith.edu.au/arock/MaSH/index.html

Customisation
-------------
@TODO

License
-------
Sytyc by Xavier Ho is licensed under
Creative Commons Attribution-ShareAlike 3.0 Unported License.
http://creativecommons.org/licenses/by-sa/3.0/