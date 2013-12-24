# Roo

Roo is an alternative schedule viewer for [Windesheim.](http://www.windesheiminternational.nl/) "Rooster" is a Dutch word for "schedule".
A working instance of it runs [here.](http://roo.joram.io)

## Installation and usage

Roo is built in Common Lisp on SBCL. It depends on:

- [Drakma](http://weitz.de/drakma/) (BSD)
- [ST-JSON](http://marijnhaverbeke.nl/st-json/) (zlib-style)
- [caramel](https://github.com/pocket7878/Caramel) (LLGPL)
- [local-time](http://common-lisp.net/project/local-time/) (BSD)
- [cl-ppcre](http://weitz.de/cl-ppcre/) (BSD)
- [hunchentoot](http://weitz.de/hunchentoot/) (BSD)
- [spinneret](https://github.com/ruricolist/spinneret) (MIT)
- [split-sequence](http://www.cliki.net/split-sequence) (Public Domain)
- [ht-routes](https://github.com/gihnius/ht-routes) (Unknown)

All of those except ht-routes can be automatically downloaded by Quicklisp. You'll have to clone the ht-routes repository and put it somewhere Quicklisp can find it.

After downloading ht-routes you can run

    (ql:quickload :roo)

to load Roo.

The Roo server can then be started using

    (roo-site:start)

## Organisation

Roo is divided into two packages, roo-site and roo-parser. 

- roo-parser downloads the schedules from [the official site](https://roosters.windesheim.nl/) and parses them into simple objects.
- roo-site serves the schedules created by roo-parser as HTML.

## Issues

- There is no caching, every time someone requests a schedule Roo goes out and fetches it from the official site.
- roo-site doesn't serve static files, so it has to run behind something like Nginx to look pretty.
- It is currently not possible to get the schedule for a specific week in a user-friendly way. It's possible to append a date in the format /YYYYMMDD to the end of the schedule for a certain class, but that's not a nice way to go about it.
- The code isn't as clean and well-organized as I'd like it to be.

## Notes

- site/static/logo.svg is a recolored version of [this image.](http://openclipart.org/detail/10686/coq-by-yves_guillou-10686)

## License

    Copyright (c) 2013 Joram Schrijver

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
