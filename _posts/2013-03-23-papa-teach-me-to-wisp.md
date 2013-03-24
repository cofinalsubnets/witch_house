---
layout: post
category: help
q: papa teach me to wisp
---
very simple little boy
----------------------

if you've never written lisp (or programmed at all) before, there are lots of resources available online to help you learn. [here's a giant textbook to get you started.](http://htdp.org/)

wisp is the tiny lisp at the heart of `witch_house`. it's not exactly right to say it's a scripting language - aside from a few toplevel commands, wisp is the language `witch_house` is played _in_. what does that mean??

for starters, player and object attributes like name, description, and password are just bindings in objects' lisp environments, and they can be modified using primitives like `set!` and `define` (like in Scheme). by default, wisp expressions entered by a player are evaluated in that player's environment, but other environments can be entered using the `as` special form, or the `@<name> <expr>` toplevel syntax (more on that later).

so here are some progressively more complicated examples!

{% highlight scheme %}
    (set! *name* "Frances") ;; change yr name
    (set! *desc* "the prettiest persyn around!!") ;; change yr description
    (define (cry me) (tell-room me "BAWW" (cat (name me) " starts to cry. Zie obviously needs support!"))) ;; learn to free your emotions
{% endhighlight %}

now you have a `cry` command! when you type it, other people will see: 

"Frances starts to cry. Zie obviously needs support!"

imagine Frances has a friend named Timmy who always keeps hir emotions bottled up tight. Frances can teach Timmy how to let it all out with the following toplevel command:

{% highlight bash %}
    share cry Timmy
{% endhighlight %}

Timmy now has a copy of Frances' `cry` function in his shared bindings. zie can make hir own `cry` function like this:

{% highlight scheme %}
    (define cry (car *shared-bindings*))
{% endhighlight %}

and now that `cry` is bound to a name, Timmy can take it out of hir shared bindings stack:

{% highlight scheme %}
    (set! *shared-bindings* (cdr *shared-bindings*))
{% endhighlight %}

here's another fun example of what shared bindings can do:

{% highlight scheme %}
    (define *do-not-disturb* #t)
    (define (dnd me) (set! *do-not-disturb* (not *do-not-disturb*)))
    (define (tell-frances msg) (if (not *do-not-disturb*) (tell *handle* msg) "Frances doesn't want to be disturbed right now."))
{% endhighlight %}

if Frances shares `tell-frances` with other people, they'll be able to send hir private messages from anywhere, as long as zie doesn't have `*do-not-disturb*` set.

shared bindings aren't the only way you can use wisp to interact with other objects, though. you can also evaluate lisp directly _in other objects' environments._ to do that, you need a ref.

whats a ref
-----------

a ref is a primitive datatype in wisp. refs can be compared for equality, and refs can be generated with `make-ref` (which creates a new ref) and `make-self-ref` (which creates a new ref _for the evaluating environment_). refs have the special property that if they correspond to an environment, they permit evaluation of code in that environment using the `as` special form. so in practical terms, refs are "keys" that allow access to a player or object's environment. they can also
be used as a primitive to implement more sophisticated security and authentication schemes.

there are three ways to obtain a ref:

- the `make-ref` and `make-self-ref` functions;
- creating an object using the `make` command `cons`es a ref onto your `*refs*` binding;
- refs can be transferred between players using the `share` command.

once you have a ref, you can use it in an expression like:

{% highlight scheme %}
    (as frances (set! *name* "Spider Jerusalem")) ;; frances is a ref to Frances
{% endhighlight %}

or in a toplevel command like:

{% highlight scheme %}
    @Frances (set! *desc* "Heavily armed, heavily tattooed, heavily drugged, currently naked.")
{% endhighlight %}

(NB: this will only work if the ref is present in the caller's \*refs\* variable -- creating an object using `make` places a ref there automatically. think of it like a keychain.)

obviously you don't want to give away your own ref to just anyone. but suppose you want to give someone _temporary_ access to a ref of yours. they're understandably unwilling to give you their own ref, so how can you ensure they have access to yours only as long as you want them to? write a function and share it!

{% highlight scheme %}
    (define *revoked-refs* '())
    (define (revoked ref) (member ref *revoked-refs*))
    (define (revoke! ref) (set! *revoked-refs* (cons ref *revoked-refs*)))
    (define (make-revokeable-ref ref) (lambda () (if (not (revoked ref)) ref #f)))
    (define shareable-ref (make-revokeable-ref my-valuable-ref))
{% endhighlight %}

the recipient, after binding the `shareable-ref` in their own namespace, can use it like this:

{% highlight scheme %}
    (as (shareable-ref) (cause-mischief))
{% endhighlight %}

but only for as long as you permit it.

this demonstrates an important point about shared bindings: they're still evaluated in their original environment. but the evaluation is "opaque" in the sense that it doesn't give the holder of the binding any access to that environment beyond what the function itself provides. so you can use shared bindings to safely share valuable resources - just like Frances did with hir output handle earlier.

