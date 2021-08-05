---
title: How I build this site
date: 2021-01-18T09:50:00-0500
---

I started working on this site nearly four years ago. The idea was that it
would be a blog about my bicycle trips. I eventually decided to just put
everything here, moving away from my old site,
[mjhoy.com](https://mjhoy.com).

Finally, just a few days ago, I implemented automated deployment, which I'm
mostly happy about, and thought I'd write about how this all works.

The code and the content for the site lives at
[github.com/mjhoy/mikey.bike][repo]. There is no database or runtime: I am
ultimately just rsync'ing HTML and images and CSS up to my web
server. These files are generated from a source of markdown files and
templates by a Haskell project using the excellent [hakyll][hakyll] library
for generating static sites.

Hakyll lets me build up what is essentially a compiler for my site. I can
define rules for taking input files and producing output files. In the simple
case, I simply copy files. This rule says copy everything under `images/`:

```haskell
match "images/**/*" $ do
  route idRoute
  compile copyFileCompiler
```

`route` is a function for producing a path given the input path; `compile` is
a function for producing a file given the input file. I'm essentially using
the identity function for these. (A more complicated version of this rule
might pipe all my images through imagemagick to size down for the web.)

Hakyll also gives you tools to do more complicated things with, say, a series
of journal entries, for you which you need an index and an RSS feed and a
"landing" page and all that. For example, [here is the code][rss] to produce
my RSS feed. My journal entries are written in Markdown, with some metadata
added to the top of the file (like the title and date).

When I push up a new commit, a Github action fires off to test the commit. It
builds the Haskell project, and then attempts to build the site. Any Haskell
compilation errors are caught by the first step, and any content issues are
caught by the second -- say I misspelled the "date" metadata field in a
journal entry. The [Github action for this][test-action] is pretty simple.
One important step is to set up caching of builds, because building all of
Hakyll's dependencies takes a long time -- around 40 minutes! With the cache,
if dependencies haven't changed, each run takes only about 2 minutes.

Finally, I also wrote an action to deploy the site when a PR is merged into
the main branch. It adds [one additional step][rsync-step] to deploy the
site; after building, it just rsyncs the output to my web server. For this to
work, I added several secret keys to a "deploy" environment I gave this
action access to.

Overall, I'm pretty happy with this. Although I still edit my journal entries
locally, I could also now use Github's editor to add a new markdown journal
entry, commit the change, and deploy the updated site to my server.

[repo]: https://github.com/mjhoy/mikey.bike
[hakyll]: https://jaspervdj.be/hakyll/
[rss]: https://github.com/mjhoy/mikey.bike/blob/main/src/Rules/Journal.hs#L40-L46
[test-action]: https://github.com/mjhoy/mikey.bike/blob/06ce7eef8b464822d85ed446396e591a7cff65b3/.github/workflows/test.yml
[rsync-step]: https://github.com/mjhoy/mikey.bike/blob/06ce7eef8b464822d85ed446396e591a7cff65b3/.github/workflows/deploy.yml#L38-L46
