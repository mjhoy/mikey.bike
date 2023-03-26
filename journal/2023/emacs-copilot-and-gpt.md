---
title: Copilot, ChatGPT, and Emacs
date: 2023-03-26T06:30:00-0400
---

The last couple weeks have felt a little like the early days of the
pandemic to me. AI news is moving terrifyingly fast, and there's an
ominous sense that the world might be very different in a year or
two. Even if these LLMs hit a ceiling soon, I think it's a safe bet
there will be a huge amount of fallout from what already is
possible. I've been mostly trying to avoid the fuss, because thinking
about it too much makes me anxious, but finally this week I gave in
and got `copilot` up and running in `emacs`. Here's what I did, and my
initial thoughts.

## Setting up `emacs` with `copilot`

There is no _official_ `emacs` plugin, but there is a very good
[unofficial port][copilot.el]. It relies on the wasm binaries
distributed with the `vim` plugin; I'm assuming that because of these, it
isn't available on Melpa, so I just cloned the repo locally. There's a
little bit of ceremony to getting it to work nicely with other
completion frameworks -- in my case, `company-mode` -- but once that's
set up, it all works nicely.

![Copilot completion in emacs](/images/j/2023/03/copilot-on-emacs.png)

[This is how I set up copilot in emacs][emacs-copilot], and also
[`company-box`][company-box-emacs] which helps fix auto-completion
conflicts.

## Setting up a ChatGPT buffer

I've also signed up for an OpenAI account and set up a ChatGPT plugin
-- [gptel, which is nice and minimal][gptel].
This gives me a little ChatGPT buffer and a few commands to send code
to it.

![ChatGPT in emacs](/images/j/2023/03/chatgpt-on-emacs.png)

[This is how I set up gptel in emacs][emacs-gptel]. (It's very simple.)

## Initial thoughts

Copilot works in _most_ kinds of buffers, and works just as well
composing emails as it does for writing code. I've automatically
turned it on in all `prog-mode`, `git-commit-mode`, and `org-mode`
buffers. It doesn't seem to work in my `M-x shell` buffers, I'm not
sure why. (Does GitHub recognize I'm using a shell and turn it off, so
as not to conflict with their forthcoming "Copilot for CLI" product?)

It's mostly very good at setting up boilerplate or structural pieces
of code. I've been using it a little to help me set up a new Rust
project -- see the screenshot above -- and that's where it's been most
impressive. At work, it's been a little less impressive. It doesn't
seem to understand very well the patterns we've built up; this makes
sense, as we've refined our patterns over time to be somewhat unique,
and our "boilerplate" code is much different from boilerplate you'd
find on Stack Overflow, for instance. I'm assuming that this will
improve as `copilot` takes can take in more context.

In general, it's kind of annoying, but admittedly useful. Like someone
at the crag who keeps shouting beta up at you, trying to work
something out: half the time, they're wrong, but at least it's giving
you ideas. (Side-note: I hate receiving beta, and can get way too
frustrated about it; it's something I'm working on personally. I may
have to work on my relationship with `copilot`, too.)

ChatGPT is convenient to have in a buffer, because it's easy to just
kill and yank text after asking it to write some code for you. You can
ask it, e.g., "Can you write me a Rails validator that validates phone
numbers?"  and gives you something reasonable to start with. It's
often much faster than googling and going to Stack Overflow for
something that you know you can do, but forgot the details. A little
like pairing, it's nice to have going, to throw out ideas when you're
stuck. I've used it to have a relatively long conversation about birds
and migration -- I was trying to come up with a good name for the little
migrator tool I want to write -- and it worked quite well.

Both of these tools (like the beta sprayer) can confidently give you
the wrong answer, and I think learning to be skeptical and review
exactly what's written is going to be an important skill in using them
effectively. At first, `copilot` wasn't saving me any time, because
I'd trust it too much and then have to go hunt down all the bugs after
I noticed it broke everything.

On a happy note, I think the whole model of `emacs` -- everything in a
buffer, everything as text -- will do relatively well in this new
world. I'm cautiously excited to see where this goes when `copilot`
understands more local context.

[copilot.el]: https://github.com/zerolfx/copilot.el
[emacs-copilot]: https://github.com/mjhoy/dotfiles/pull/148/files
[gptel]: https://github.com/karthink/gptel
[emacs-gptel]: https://github.com/mjhoy/dotfiles/pull/147/files
[company-box-emacs]: https://github.com/mjhoy/dotfiles/pull/149/files
[copilot-x]: https://github.blog/2023-03-22-github-copilot-x-the-ai-powered-developer-experience/
