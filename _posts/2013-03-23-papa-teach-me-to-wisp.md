---
layout: post
category: help
q: papa teach me to wisp
---
very simple little boy
----------------------

wisp is the tiny lisp at the heart of `witch_house`. it's not exactly right to say it's a scripting language - aside from a few toplevel commands, wisp is the language `witch_house` is played _in_. other articles go into greater depth on precisely what that means - this article is meant to serve as a brief introduction to the language for people who may be new to lisps, or to programming in general.

\`lisp' refers to a large family of programming languages with a decades-long history. lisps are often categorized as functional languages, although some influential dialects (eg, common lisp) can be written in a very object-oriented style, and some very object-oriented languages (eg, ruby) are heavily influenced by lisp.

if that means nothing to you, don't worry. the salient point here is that, as in all functional languages, wisp allows you to manipulate functions as data. and like many lisps, wisp allows you to manipulate more general _code_ as data using a facility called macros, which we'll talk about more later.

lisps also share a (mostly) similar syntax. there are basically three kinds of expressions:
- self-evaluating expressions like numbers, strings, and the boolean values `#t` and `#f` (true and false);
- symbols, which may or may not be bound to a value in a given environment, and which evaluate to that value if it exists (and cause an error if it doesn't);
- s-expressions, which are lists in which the first element (the \`car') is assumed to evaluate to an applicable value (a function, macro, or special form), to which the remaining elements (the \`cdr') are passed as arguments.

variations exist between dialects (for example, common lisp uses a separate namespace for functions), but the above is true enough when it comes to wisp. when you're writing wisp in `witch_house`, you'll generally be writing s-expressions, so here are some examples:

{% highlight scheme %}
    (+ 1 2 3) ;; 1 + 2 + 3 = 6
    (odd? 4)  ;; #f
    (cat "hello" " " "world" "!") ;; "hello world!"
    (const id) ;; (lambda (y) x) ;; <probably a really big number>
{% endhighlight %}

the values returned by these expressions are, respectively, a number, a boolean, a string, and a function. the visual representation of functions in wisp might be kind of confusing, so let's break it down:
- `lambda` is short for 'lambda expression,' a concept taken from a mathematical model of computation called the lambda calculus. is basically means 'function.'
- `(x)` is a _parameter list_. this function takes one argument, and within its lexical scope, the value of that argument is bound to the symbol `x`.
- `y` is the body of the function. this particular function contains only one expression, which ignores its argument and returns whatever value is bound to `y`. in this case, the value of `y` is inherited from another frame. speaking of frames...
- the `really big number` is a frame identifier. a frame is basically a lookup table that associates symbols with values. frames generally have parent frames, and if a lookup fails in one frame, then the evaluator will try again in that frame's parent, until it hits the special 'toplevel' frame, which has no parent.

what happens if we call this function?

{% highlight scheme %}
    ((const id) 0) ;; (lambda (n) n) ;; 0
{% endhighlight %}

the function ignored its argument (0) and returned the identity function - a function that takes one argument (n) and returns that argument unchanged. in this case the frame number is 0, which means `id` is a value bound at toplevel.

the ability to write functions that return functions lets you do all kinds of neat tricks that are difficult or impossible in other languages. for example:

{% highlight scheme %}
    (define make-infixer (lambda (left right) (lambda (str) (cat left str right))))
{% endhighlight %}

this expression uses the `define` special form, which creates a binding with the given name in the current namespace. this example defines a function that takes two strings as arguments, and returns a function of one string argument that returns the given string wrapped in the two arguments to the first function. here's a demonstration:

{% highlight scheme %}
    (define parenthesize (make-infixer "(" ")"))
    (parenthesize "<3 i lisp") ;; "(<3 i lisp)"
    (cat "the bbq seitan was " ((make-infixer "\"" "\"") "delicious")) ;; "the bbq seitan was \"delicious\""
{% endhighlight %}

the last example calls an anonymous function to generate a string (we did the same thing, to less sarcastic effect, in the `(const id)` example above). also notice the backslashes - double-quotes are wisp's string delimiters, so in order to include them in  a string, you need to \`escape' them using a backslash. this tells the interpreter that the double-quote is just an ordinary character, not the end of the string. you can also use escape sequences to include line breaks ("\n") and tabs
("\t") in a string. to include a literal backslash, just type "\\".

we used the `define` special form above. a special form is a procedure that elicits some unique, low-level response from the interpreter that you couldn't get using functions alone. other special forms include:
- `set!`, which changes the value of an already-bound variable. note that `set!` can operate on different frames than the current one - the value will be changed in the frame in which it's defined. the exception is the toplevel frame, where `set!` can't be used to change values.

- `unset!`, which removes a binding from the _current_ frame.

- `if`, which takes three arguments: first, an expression to be evaluated as a test; second, an expression to be evaluated if the test succeeds; and third, an expression to be evaluated if the test fails. this has to be a special form, rather than a primitive function, because the evaluation strategy for functions is such that _all arguments are evaluated in the calling context_ before being passed to the function. if `if` were a function, then, both the `then' and `else' clauses would be evaluated regardless of the outcome of the test!

- `begin`, which takes any number of expressions as arguments, evaluates them in order, and returns the value of the last one. we've actually used this one already - the bodies of functions are wrapped in an implicit `begin` form.

- `lambda`, which we have already met.

- `quote`, which evaluates to its argument. a common use of `quote` is to prevent a list of data (eg, `(1 2 3)`) from being evaluated as an s-expression. the wisp parser treates expressions preceded by a single quote as quoted - for example, evaluating `not-bound` will raise an error, since the symbol `not-bound` has no particular value. but evaluating `'not-bound` returns the literal symbol `not-bound`.

- `macro`, and the related forms `quasiquote` and `splice`. macros, which we mentioned earlier, are a means of \`reconfiguring' data into executable code, but their use is a little bit subtle and we won't go into detail here. it's worth mentioning, though, that like `quote`, `quasiquote` and `splice` have syntax that's specially recognized by the parser: the backquote (\` - on the same key as \~ on US keyboards) and comma (,) respectively. also worth a mention is that without any `splice`s, `quasiquote` functions exactly like `quote`; and that attempting to use `splice` outside of a `quasiquote`d expression is illegal and will raise an error.

one more word on `if`: in wisp, all values other than `#f` are \`true' in a boolean context. so for example, `(if "yes" "no" "maybe")` is perfectly valid, and evaluates to `"no"`.

let's cover a few more useful functions before closing this article:

- `list` takes any number of arguments, evaluates them, and returns a list containing them. this is really necessary - you can't simply type `(1 2 3)` into the interpreter and get a list back, since the interpreter will attempt to execute the (non-existent) function `1` and raise an error. in this case you could quote the list - `'(1 2 3)` - but what if the list contained expressions you wanted to be evaluated? `'(1 2 (+ 1 2))`, for example, evaluates to the list `(1 2 (+ 1 2))`.
- `car` returns the first element of a list.
- `cdr` returns the tail of a list - the list with the first element removed. if these function names seem obscure, that's because they are - they originally referred to CPU register locations on IBM 704 computers from the 1950s! other lisps (common lisp is particularly guilty of this) retain many other arcane names for list operations, such as `rplacd` and `nconc`.
- `cons` takes an item and a list, and returns a list with the new item as the `car` and the old list as the `cdr`. it shares the same etymological lineage as `car` and `cdr`.
- `member` tests whether an element is present in a list. if so, it returns that sublist of the original list of which the element is the `car`.

- `map` takes a function and a list, and returns a new list whose elements are obtained by sequentially applying the function to elements of the original list. for example:
{% highlight scheme %}
    (map even? '(1 2 3 4)) ;; (#f #t #f #t)
    (map (lambda (s) (cat s "!")) (list "hello" "there" "friend")) ;; ("hello!" "there!" "friend!")
{% endhighlight %}
- `filter` takes a function and a list, and returns a new list whose elements are those elements of the original list that, when passed to the function, returned a true value:
{% highlight scheme %}
    (filter odd? '(1 2 3 4)) ;; (1 3)
{% endhighlight %}
- `fold` takes a function of two arguments, and initial value, and a list, and returns the value obtained by sequentially applying the function to the current value and the next element of the list - the result then becomes the current value.
{% highlight scheme %}
    (fold + 0 '(1 2 3)) ;; 6
    (fold (lambda (a b) (cat a " " b)) "hello" '("there" "friend!")) ;; "hello there friend!"
{% endhighlight %}

`map`, `filter`, and `fold` are _heavily_ used in functional programming, btw. the ability to easily create new functions on the fly makes them astonishingly versatile. if you understand them, you understand a lot!

there are lots of resources available online to help you learn more about programming, and lisp programming in particular. [here's a giant textbook to get you started!](http://htdp.org/) fundamental topics not touched on _at all_ in this little article include iteration, recursion, and data structures. so get reading (and hacking), if yr so inclined!

