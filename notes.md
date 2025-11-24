## Part II

It looks like Part II covers mostly pattern matching and search. I will need to go back and review this and take a crack at implementing and perhaps correcting some "improvements" I made in implementing both GPS and Eliza. I simplified to solve the problem, but I didn't realize we were pushing towards more abstract versions of search and pattern matching. Maybe I can do better after learning more.

However, I am getting the feeling that pattern matching is a dead-end technique. It seems like chapters 7 and 8 have warnings about the shortcomings of pattern matching systems. But, it may still be good to understand this stuff. For one, there may be some problems where these simple ideas may prove beneficial. Secondly, by working through the problems, I will have a better understanding of why the techniques were abandoned.

Part III may be easier for me to get through as it looks like it's more programming focused and therefore may be more familiar to me.

## Part III

### Chapter 9

Yep, I was able to skim through it pretty fast.	The topics covered are summarized on page 269: caching, compiling, delaying, indexing.

* Caching: mostly using memoization
* Compiling: Converting runtime rule search into compile functions/lambdas
* Delaying: Lazy evaluation
* Indexing: Using data structures other than lists. BTW, my rule of thumb for Lisp is as follows: use lists for dealing with metaprogramming issues in Lisp, use vectors and hash tables for most other things.
* Also some stuff on profiling (not mentioned in the summary on 268), such as building your own profiling code. As SBCL comes with extensive profiling support, I mostly ignored that section.

### Chapter 10

Once again, there is a helpful summary of topics on page 316. All are things I know about already, except maybe the compiler macros section.

* Use declarations. Yep, I know this.
* Avoid generic functions. Yep, I know this.
* Avoid complex argument lists. Somewhat interesting, it makes sense that keyword arguments can be slow in Lisp implementations. I know they are definitely slower in groovy because they are just maps. I just did a quick experiment in SBCL and it does look like keyword arguments are about 2x slower than positional arguments. So, keyword arguments should probably be avoided for shorter functions called in a hot loop.
* Provide compiler macros. Not much on this. I guess he just means to provide macros to the compiler??
* Avoid unnecessary consing. Basically avoid garbage collection and use more effiecient data structures like vectors. However, his advice on re-using objects is probably wrong. It is for sure wrong when using something like ABCL.
* Use the right data structure. Mostly more advice about not using lists in favor of vectors and hash tables. Also some advice on how to implement a queue efficiently with lists. I already am using a variation on this for work in fast-structured-io, so it's fairly obvious. Also included is how to implement a trie data structure in lisp. TODO: implement one based on an algorithm test and then see how my implementation compares to Norvig's.

### Chapter 11

I skimmed this chapter because two things became clear:

* I have to go back and study the implementation of pattern matching in chapter 6. That is turning out to be crucial and re-used over and over again. Unification is basically a slight variation of pattern matching.
* This chapter will take some time to study and understand. At some point I will, but for now I just want to get a feel for what is in the book.


