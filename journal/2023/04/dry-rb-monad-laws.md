---
title: dry-rb and lawless monads
date: 2023-04-06T07:00:00-0400
---

At work, we make heavy use of the [`dry-monads`][dry-monads] gem,
which gives us result objects (either `Success` or `Failure`) and
methods to bind them together, and if you squint it looks a lot like
Haskell's do notation. If you squint _too_ hard, though, you might
forget you're in Ruby, where -- despite the "principle of least
surprise" -- side effects are hiding all over the place. Here's one
way that can bite you if you aren't careful.

# A somewhat contrived example

Let's say you have a couple of methods, each of which returns some
result.

```ruby
def mark_user_last_posted(user)
  if user.update(last_posted_at: Time.current)
    Success(user)
  else
    Failure(user.errors)
  end
end

def create_post(user, title)
  post = user.posts.build(title:)
  if post.save
    Success(post)
  else
    Failure(post.errors)
  end
end
```

And now, within a `Dry::Monads::Do`-wrapped method, you compose these
together, like so:

```ruby
include Dry::Monads::Do.for(:run)

def run(user, title)
  user = yield mark_user_last_posted(user)
  post = yield create_post_for_user(user, title)
  Success(post)
end
```

(Apologies for the silliness of this example.) You are doing two
things that might fail: mutating the `user` by updating their
`last_posted_at` attribute, and creating a new `post` object. If
either fail, a `Failure` that wraps errors is returned; otherwise, the
newly-created post is returned wrapped in a `Success`.

The `Dry::Monads::Do.for(:run)` decorates your `run` method with
machinery handles the results of the `yield` calls by either
unwrapping `Success` values, or returning a `Failure` if it gets
one. It's a nice, readable way to compose monadic methods, and it also
has an important feature.

# Transactional safety

You think about it for a second and realize this code has a problem:
it's possible for this method to succeed in marking the user as having
posted, but fail in creating the new post. Luckily, `Dry::Monads::Do`
supports wrapping everything in a transaction:

```ruby
def run(user, title)
  user.transaction do
    user = yield mark_user_last_posted(user)
    post = yield create_post_for_user(user, title)
    Success(post)
  end
end
```

Any `Failure` returned within a `yield` triggers a rollback; you now
have the transactional semantics you want. How does this work? You can
see the special do machinery [defined here][do-machinery]. Here's the
relevant bit:

```ruby
module Dry::Monads::Do::Mixin
  def bind(monads)
    monads = Do.coerce_to_monad(Array(monads))
    unwrapped = monads.map do |result|
      monad = result.to_monad
      monad.or { Do.halt(monad) }.value!
    end
    monads.size == 1 ? unwrapped[0] : unwrapped
  end
end
```

If you remove all the array-coercing shenanigans, you can simplify
this to the core logic:

```ruby
def bind(monad)
  monad.or { Do.halt(monad) }.value!
end
```

`Do.halt` _raises an exception_ -- this is key -- which is caught
higher up in the method wrapper that Do generates. That exception will
first blow through the `transaction` block, triggering a rollback.

# The Monad laws

Great! Everything's working. You `git add` your code and take once
last look before you commit. As you do, some deep memory surfaces --
_this code looks familiar_.

```ruby
def run(user, title)
  user.transaction do
    user = yield mark_user_last_posted(user)
    post = yield create_post_for_user(user, title)
    Success(post)
  end
end
```

You squint.

```ruby
a = yield run_a
b = yield run_b(a)
Success(b)
```

In Haskell, this looks like:

```haskell
a <- run_a
b <- run_b a
pure b
```

Of course, you think. _It's a call to bind, [I know this][i-know-this]_.

An indelible feature of monads in Haskell are that [monads have
laws][monad-laws]. The identity law tells you that given some monad
value `m`, you can rely on the following equality:

```haskell
val <- m
pure m
-- can be rewritten as just:
m
```

With some satisfaction (_"I knew learning Haskell would finally come
in handy!"_), you rewrite your method:

```ruby
def run(user, title)
  user.transaction do
    user = yield mark_user_last_posted(user)
    create_post_for_user(user, title)
  end
end
```

# The problem

This doesn't actually work, and you just broke transactional
safety. If `create_post_for_user` fails, the transaction _still
commits_ and the user is marked as having posted.

Why is this? The problem is that `yield` was doing work here in
producing side effects that caused the rollback. Pure `Failure`
objects don't cause exceptions, and the transaction happily commits.

Going back to our monad laws, `dry-rb` breaks the identity law by
producing a side effect in the `bind` method when using `Do`. In other
words, the identity equation does not hold:

```haskell
m >>= \val -> pure val ≡ m
```

The two sides are not equal because the expression on the right has
removed a bind call (`>>=`), and because `bind` has side effects, the
semantics are different.

In general, _any_ pure return of a `Failure` result without using
`yield` will skip the exception side effect. So, for instance, this
(somewhat silly) code is also broken:

```ruby
def run(user, title)
  user.transaction do
    user = yield mark_user_last_posted(user)
    return Failure("user is not verified") unless user.verified?
    post = yield create_post_for_user(user, title)
    Success(post)
  end
end
```

I'm not sure why you would write the logic this way, but the return
guard in the middle of the transaction won't trigger the rollback, for
the same reason: it's just a pure `Failure` value. Instead, you'd need
to write it using `yield`:

```ruby
def run(user, title)
  user.transaction do
    user = yield mark_user_last_posted(user)
    yield Failure("user is not verified") unless user.verified?
    post = yield create_post_for_user(user, title)
    Success(post)
  end
end
```

# The solution

Don't assume that Ruby has laws.

[do-machinery]: https://github.com/dry-rb/dry-monads/blob/5c02f73e698aac49a49b5a504412de0258a2d50d/lib/dry/monads/do/mixin.rb#L46-L53
[dry-monads]: https://github.com/dry-rb/dry-monads
[monad-laws]: https://wiki.haskell.org/Monad_laws
[i-know-this]: https://www.youtube.com/watch?v=dFUlAQZB9Ng
