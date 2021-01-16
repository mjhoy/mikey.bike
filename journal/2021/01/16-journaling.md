---
title: A year of journaling
dte: 2021-01-16T07:30:00-0500
---

Every morning, I try to write down what happened the day before. I do this
with a cup of coffee. I used to write in a physical journal, but I've found I
don't stick with this as well. So I have a global emacs keybinding to pop me
into a new journal entry: C-c C-j, which opens an org-mode file at
`~/journal/YYYY-MM-DD.org`. If the file exists, it adds a new entry to it.

Yes, I journal on the computer. Yes, it's lame. In any case, I guess it's
nice to know that in 2020, I wrote 177 entries, and that there are some 7k+
lines:

```sh
~/journal % ls | grep 2020 | wc -l
     177

~/journal % ls | grep 2020 | xargs cat | wc -l
    7589
```

I admit it'd be nice to point to a notebook and say I wrote _this many_
pages.

Looking back over them now, they seem a little boring. 2020 was a strange
year, shouldn't daily life have been interesting? Perhaps the truth is that
lockdown is just dull. No going out, meeting new people, seeing new things.

It is nice to find little moments I had forgotten about: zoom dating (fun),
zoom Spanish class (less fun: no one had quite figured out zoom etiquette
yet), my roommate and I getting in an argument about washing hands (that used
to seem so important), becoming obsessed with Hamilton in August.

My favorite thing about journaling is finishing the entry. I hit C-c C-c to
write the org file, the buffer closes, and I've saved one more day from being
lost in the eventual cloud of forgetting.
