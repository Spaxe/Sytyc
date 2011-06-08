Overview
========
Sytyc is a web-based problem database with a compiler on the server side.
A list of problems is presented to the user, from which they can pick a
problem to work on, and submit their source code. The code is (unsafely)
compiled server-side, and the result is then printed back onto the web
page. Kudos is given when the program computes the correct results.

Github repository: [https://github.com/SpaXe/Sytyc]

Credits
-------
Author: Xavier Ho [contact@xavierho.com]
Supervisor: Andrew Rock [a.rock@griffith.edu.au]
And other Functional Programming classmates.

Installation & Usage
--------------------
@TODO

Dependencies
------------
Haskell dependencies:
*   GHC version >= 7.0.3. As of Feb 2011, the official Haskell Platform 
supports GHC 7.0.3. Sytyc was compiled against the GHC libraries.
*   data-hash-0.1.0.0
*   pandoc-1.8

Server dependencies:
*   Apache, or other server programs that support CGI.

Other dependencies:
*   Java SDK SE 6
*   MaSH compiler [http://www.ict.griffith.edu.au/arock/MaSH/index.html]

Customisation
-------------
@TODO

License
-------
Sytyc by Xavier Ho is licensed under
Creative Commons Attribution-ShareAlike 3.0 Unported License.
[http://creativecommons.org/licenses/by-sa/3.0/]