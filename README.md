OPAM Bulk Build UI Prototype
============================

Copyright Thomas Leonard, 2015


** This is an early prototype, and very buggy. **


Overview
--------
This is a prototype to explore how to improve the view of the OPAM bulk build reports, which are generated regularly by rebuilding the entire OPAM repository on multiple platforms using a set of Docker containers.
The existing system is running at <http://opam.ocaml.org/builds/>.

In the new system, the main display shows a matrix of packages and build runs.
Each box shows the number of failures (if any), with green meaning all passed, red for all failed and orange for a mixture.
A blank box indicates that no build ran.
This view makes it easy to see packages that were recently broken, when a regression was committed to the OPAM repository that broke many packages, and when a failure is intermittent.
Clicking a package row shows a breakdown for the different build platforms over time, and clicking on one of these cells shows the build log and other details.
The build log is shown with differences from the previous build on that platform highlighted.
This is useful for spotting where the build diverged from the previous one.

Installation
------------

In the source directory:

    mkdir logs
    git clone --mirror https://github.com/talex5/opam-bulk-build-logs.git logs/.git
    git clone https://github.com/ocaml/opam-repository.git
    opam install mirage
    mirage configure --unix
    make
    ./mir-bulk-build-www

The visit <http://localhost:29211/> in your browser.


Implementation notes
--------------------
* The server is implemented as a [Mirage][] unikernel (it currently only targets Unix, but will target Xen in future).
* The previous solution, based around generating static HTML files from raw logs stored on a filesystem, required 150G of storage.
  Using [Irmin][], the same data requires less than 1G, but still allows random access.
* The web-pages are generated using [TyXML][] on the server side to generate valid HTML 5.
* The diffs are calculated using [patience_diff][].


CONDITIONS
----------

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA


[Mirage]: https://mirage.io/
[Irmin]: https://github.com/mirage/irmin/
[TyXML]: http://ocsigen.org/tyxml/
[patience_diff]: https://github.com/janestreet/patience_diff
