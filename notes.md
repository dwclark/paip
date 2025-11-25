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

### Chapter 13

CLOS just isn't as useful as you might expect. It's been my experience. Norvig says on page 453, "So far the case for CLOS has not been compelling." You might think that this is to build tension for the forthcoming explanation of what _does_ make CLOS compelling. But, this page is the end of the CLOS tutorial, after that it's all summary and historical notes.

I guess CLOS is not that compelling.

However, the amount of history in this chapter is interesting. One gets the impression that OO programming never really achieved final form. Everyone came up with their own versions and the great OO wars commenced. It also seems like the 1980's really was the last great era of programming language design. And if you believe Pike's Systems Software Research is Irrelevant, that was the end of the line for all systems software.

### Chapter 14

I skimmed it again. My plan/hope is to make studying this text a long term project because I want to get more experience with AI, its history, and its algorithms.

But, this was easily the densest chapter so far. I have to confess that I really didn't get a lot from the skim, which isn't surprising. One thing I did finally conclude is that in AI (maybe old fashioned AI??) is that there is zero distinction between the compiler, knowledge representation, the code that is writen, how data is expressed, etc. This is most likely why traditional AI/Lisp programmer always reach for lists. It's the language of the base system reader and the representation of the base language. This to get the lack of distinctions, it's easiest to stick with lists in writing code, data, and knowledge representation. For example, if everything is a list, you can always make things more performant at selective points by compiling those lists into more efficient data structures and functions.

The point is that my approach to this text needs to change. My inclination is to always change what Norvig does to make it something more to my liking: more efficient, more similar to systems I have made in the past, more like things I have done in the past, etc. However, there is one large problem with that. The systems I have made in the past tend to be closed systems that one _uses_. However, the systems in this book, and what Lisp excels at, is making open systems that one _lives within_. It's a different way of thinking. Of course this is a metaphor and in some sense these Lisp programs are closed systems. In some sense the systems I have done are open. In fact, ironically, the systems I make tend to be more open with their emphasis on DSLs and extensibility than are most systems. But there's still a pretty wide gulf between these systems and the systems I have made/used in the past.

I'm also wondering if this is why LLMs tend to succeed and fail in ways that traditional AI systems do not. LLMs are closed systems and not just because they tend to be closed source and proprietary. You can't live within them, you can only put queries to them. They are successful in that by emphasizing efficiency and being closed they can encompass all of human knowlege. Well, they encompass all digital _representations_ of knowledge. But they really don't bring any insight, nobody really knows how or why they work, and nobody really knows when they are BS-ing and hallucinating...that is unless you already know the answer is wrong. But if you already know that, then why use the AI in the first place?
