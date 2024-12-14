At the request of a friend I've decided to make a short intro doc on Haskell and how I solved a few of the problems
during this Advent of Code using it.

Haskell, at it's core, is a pure and lazy functional programming language. Let's break down what that means.

# Functional

As a refresher, the three primary paradigms of programming languages are:

* Procedural (C, Python, Go, etc.) - Programs are constructed as a sequence of instructions and subroutines that
  manipulate program state
* Object Oriented (Java, C#, Kotlin) - Programs are constructed as a collection of objects that contain internal state
  and behavior that modifies that state
* Functional - Programs are constructed as a composition of functions, that take an input and produce an output.

The core of any functional programming language is, of course, functions. Programs in a functional language are built
largely of composing functions together into a pipeline to process data from A -> B. In functional programming
languages, functions are values, just as ints, bools, or strings are. This means they can be passed as arguments to
other functions, be returned from functions, and can be manipulated by other functions to create new functions. This is
the core building block.

# Pure

Haskell is a pure language, meaning that regular functions are not allowed to have side effects. What does that mean?
Functions in Haskell generally follow these rules:

* Functions are dependent only on their input. Given a set of input parameters, a function will always produce the same
  output. No exceptions
* Functions may not interact with any data external to itself or its inputs. That means no updating private variables in
  a class, no communicating with global variables, no talking to the outside world (user input, databases, etc.)

These rules are generally referred to as "referential transparency". As an example of something that breaks referential
transparency:

```python
class Counter:
   def __init__(self):
      self._count = 0

   def increment(self):
      self._count += 1

   def get(self):
      return self._count
```

The outcome of `get()` is dependent on how many times `increment()` has been called, meaning it's dependent on the prior
state of the program up to that point. This breaks referential transparency.

## Immutability

A core part of ensuring the rules of pure-ness outlined above is that all values in a haskell program are immutable.
This means that once they're assigned, you can't re-assign them, increment/decrement them, or otherwise modify them in
any way. At first this seems like a massive limitation, but once you get used to it, you'll never want to go back to
mutable balls of program state. Immutability makes your program orders of magnitude easier to reason about, and greatly
reduces the chance of tricky bugs.

# Lazy

One of the hardest concepts to grasp about Haskell is that all values are evaluated lazily. That means that values only
enter memory at exactly the moment that they're needed. This allows some crazy things, such as infinite lists.

```haskell
x = [1..]
```

This will define a list of integers from 1 to infinity, incrementing by 1. This is possible because the rest of the list
isn't calculated until it's requested.

Another quick important note about lists: the default list in Haskell is a linked list, not an array. You may know linked lists from 
intro Comp Sci courses. Linked lists are defined in C as follows:

```c
typedef struct {
    int current;
    LinkedList* next;
} LinkedList;
```

The same applies here. The basic list type (denoted as `[Type]`) in Haskell, is a linked list. Due to the recursive
nature of linked lists, they work quite nicely with functional programming, which relies heavily on recursive
definitions.

# Day 1 Explanation

On day 1 part 1, we get two lists of numbers in a file. We need to pair up the elements in each list by ascending order,
get their differences, and find the sum total difference between the elements.

Let's skip the parser logic for now, as that will require us to get into some more advanced topics like Applicatives and Parser Combinators that will probably be a bit too much for this intro. For now, we'll look at the core solution for this
problem.

```haskell
part1 :: IO ()
part1 = basicAnswer readLocationList calculate
  where
    calculate =
      sum
        . map abs
        . uncurry (zipWith (-))
        . bimap sort sort
        . unzip
```

This is a function definition in Haskell. Let's break this down piece by piece.

```haskell
part1 :: IO ()
```
Earlier we mentioned that Haskell is a pure language, meaning that functions aren't allowed to have side effects. That's
not entirely true. If you had a program full of functions that didn't have side effects, you would have a useless
program. No side effects means we couldn't communicate with the outside world, couldn't display results, couldn't accept
user input. So to mitigate this, Haskell has the `IO` type.

Functions in Haskell are separated into two categories: Functions that are pure, and functions that have side effects.
Functions that have side effects live within the `IO` type. These functions are allowed to do the operations that
regular programs do, read input, write output, access mutable program state, etc. Functions outside of this type are not
allowed to do this.

So `part1` is a side-effecting `IO` action that produces no result (`()` is equivalent to `void`). In this case, the
side effect will be printing to the console.

```haskell
basicAnswer readLocationList calculate
```

basicAnswer is defined as:

```haskell
basicAnswer :: (TextShow b) => IO a -> (a -> b) -> IO ()
basicAnswer reader compute = reader >>= printT . compute
```

There's a lot to break down here.

# Generics and Constraint

Haskell relies heavily on programming over generic types, meaning that we don't know the concrete type of input we're
getting when we receive it. Let's look at a trivial example.

The function `identity` will take a value as an input, and return that same value.

```haskell
identity x = x
```

The type of this function is:

```haskell
identity :: a -> a
```

This means that identity is a function that takes any value of type `a`, and produces a value of the same type `a`. So
idenntity can be used for any type.

```haskell
identity 1 == 1
identity "foo" == "foo"
identity True == True
```

However, sometimes we need to know something about the type of value we're given so we can do something useful with it.
This is called a Constraint. In this case, our constraint is:

```haskell
(TextShow b) =>
```
What this says is in this function, `b` is a type that satisfies the constraint `TextShow`. A type satisfies a
constraint `TextShow` if it can be converted to a textual representation of itself. For example:

```haskell
showt True == "True"
showt 1 == "1"
showt "foo" = "foo"
```

Constraints are defined by "type classes". If you've ever heard of "Traits" in Rust, they were ripped directly from
here. For sake of convenience they can also be thought of sort of like Interfaces from Go or Java, or abstract classes
from C++, but way more powerful and flexible.

So the class of `TextShow` could be something like:

```haskell
class TextShow a where
    showt :: a -> Text
```

We'll demonstrate how to instantiate these classes later.

Now that that's out of the way, let's go to the next part:

# Function signatures and currying

```
IO a -> (a -> b) -> IO ()
```

This can be read as a function which accepts first "An IO action that produces something of type a" (`IO a`), second
another function which transforms something of `a` to something of `b` (`(a -> b)`), and returns an IO action that
produces nothing.

One important thing to note that separates Haskell (and Ocaml, F#, etc.) functions from functions in other langauges is that
functions in Haskell are "curried". Theoretically this is due to these languages deriving from an earlier computation
model called [Lambda Calculus](https://www.youtube.com/watch?v=eis11j_iGMs) but it comes with some practical benefits.

In Haskell, functions only ever take one argument, and produce one argument (`a -> b`). What appears to be a function
that takes multiple arguments is actually a chain of nested functions that eventually result in a single value. To
demonstrate this, I'll show the same function written in Haskell and Python:

```haskell
add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z

total = add3 1 2 3
```

Would be more accurately written as something like this:

```python
def add3(x):
    def inner1(y):
        def inner2(z):
            returnn x + y + z
    return inner1

total = add3(1)(2)(3)
```

Since functions in Haskell are curried, you can partially apply their arguments to extract the intermediate functions.
For example:

```haskell
add2 = add3 1

add2 2 3 == 6

add1 = add2 1 2

add1 3 == 6
```

Now that we're done with the signature, let's look at the body.

```haskell
basicAnswer reader compute = reader >>= printT . compute
```

We'll need to explain the funky operators

# Function composition: the "." operator

Functions can be chained together using the operator `.`. This is equivalent to mathematical composition operator

(f ∘ g)(x) = f(g(x))

So in Haskell this would be written

```haskell
-- Note: All operators are simply functions that take two arguments. You can
-- define any custom operator you want.

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = g (f x)
```

Basically, two functions, creates a new function that first applies the right function, passes the output to the left
function, and returns the result.

# Monads, (>>=), and do notation

One of the most notorious terms when learning Haskell is the Monad. It can be a really difficult wrap your head around,
but if you've ever used `map()/flatmap()` in Java/Javascript, or `async/await`, or `Result/Option` in Rust, you've
already used them.

A monad can be thought of as one of two things:

* A computation that may produce a value that contains extra context
* A value combined with some extra context where operations that can be applied to the inner value.

In this case, that extra context is that the value `a` is produced by an `IO` action, an action that has side effects,
and Haskell is a language that doesn't like side effects. So instead, we model these operations as a Monad, one that
can be used to build up a descriptive chain of side-effectful actions. This way, we separate the complicated impure
world from the nice, clean pure world of regular functions.

The operator `(>>=)` has the signature 

`(>>=) :: (Monad m) => m a -> (a -> m b) -> m b`

So it accepts on the left hand a Monad or action that can produce a value of type a, and binds it to a function on the
right hand that takes an a and produces a monad or action that produces values of type b. The whole resulting action is
then produces type b. It's a little bit like a more
complicated version of the `.` operator. In this case the Monad is `IO`, so you can instead think of it as

`(>>=) :: IO a -> (a -> IO b) -> IO b`

Haskell has a built-in syntax for this that will look a little more familiar to people coming from procedural languages.
Instead, you may write it as:

```haskell
basicAnswer reader compute = do
    input <- reader
    let result = compute input
    printT result
```

Where `<-` is an operator that will pull the resulting value out of the `IO`, and it can be used in subsequent
statements. result is bound with a `let` statement, because it's the result of a non `IO` function.

So all in all,

```haskell
basicAnswer reader compute = reader >>= printT . compute
```

sequences to `IO` operations. First, it uses `reader` to read in user input, and then passes the result to the second
action, which first runs `compute` on the input, and then prints the output of `compute` to the terminal.

Now let's look at the actual answer itself.

# Part 1

```haskell
part1 :: IO ()
part1 = basicAnswer readLocationList calculate
  where
    calculate =
      sum
        . map abs
        . uncurry (zipWith (-))
        . bimap sort sort
        . unzip
```

first, we use basicAnswer to construct an IO action that returns nothing (`IO ()`). In this case, our input reader is
`readLocationList`, which is an `IO` action that will read the file for day1 and convert it to a list of tuples of
ints (`[(Int, Int)]`).

So if the input is:

```
3   4
4   3
2   5
1   3
3   9
3   3
```

`readLocationList` produces:

```haskell
[ (3, 4)
, (4, 3)
, (2, 5)
, (1, 3)
, (3, 9)
, (3, 3) 
]
```

Then, it uses the function `calculate` to calculate the result.

Finally, basicAnswer will pass that result to `printT` and print it.

`where` is just a way to define local variables within a function, same as any other language. In this case, `calculate`
is a local variable of `[(Int, Int)] -> Int`. All types in Haskell can technically be inferred by the compiler, which is
why we don't have them written out here. However it's best practice to annotate the types of your top level
declarations.

The real meat of the solution is in `calculate`, so let's break that down.

```haskell
    calculate =
      sum
        . map abs
        . uncurry (zipWith (-))
        . bimap sort sort
        . unzip
```

Here where using that `.` operator to compose a long chain of functions together to eventually create a pipeline that
can take our input and transform it to our desired output. The order of operations here should be read bottom-to-top, so
something like:

```
input -> unzip -> bimap sort sort -> uncurry (zipWith (-)) -> map abs -> sum -> output
```

Let's break down each one

## zip/unzip

zipping means to take two lists and combine them pairwise. The default version is to combine two lists `[a]` and `[b]`
into a list of tuples `[(a, b)]`. So:

```
zip [1,2,3] [4,5,6] == [(1,4), (2,5), (3,6)]
```

Unzipping is the exact inverse. Take a list of tuples, and create a tuple of two separate lists. So our input is
transformed into:

```haskell
([3,4,2,1,3,3], [4,3,5,3,9,3])
```

Next step:

## map and bimap

map is a function you'll see all over the place in functional programming. Essentially, it takes a function, and applies
it to each element of a list, producing a new list. So think of:

```python
def double_each(values):
    ret = []
    for i in values:
        ret.append(i * 2)
    return ret

double_each([1,2,3]) == [2,4,6]
```

Instead in Haskell we could write:

```haskell
doubleEach :: [Int] -> [Int]
doubleEach = map (* 2)

doubleEach [1,2,3] == [2,4,6]
```

Remember, all functions can be partially applied, and multiplication (`*`) is technically a function, so here we
partially apply it to create a new function that multiplies by two, and then we pass it to map to create another
function that applies that to each element of a list.

bimap is a more specific version of map. It's a function from the `Bifunctor` class. This class is meant to represent
types that may contain two elements, or where mapping over two distinct elements makes sense. In this case it's easy.
Here's the class instance of `Bifunctor` for `(a, b)`:

```haskell
-- The constraint definition
class Bifunctor (f a b) where
    bimap :: (a -> c) -> (b -> d) -> f a b -> f c d

-- The instance
instance Bifunctor (a, b) where
    bimap f g (x, y) = (f x, g y)
```

So bimap over a `(a, b)` will apply two functions, one to the first value and the other to the second, and produces a
new tuple.

In this case, we're simply using it to sort both lists:

```haskell
bimap sort sort ([3,4,2,1,3,3], [4,3,5,3,9,3]) == ([1,2,3,3,3,4], [3,3,3,4,5,9])
```

## uncurry

```haskell
uncurry (zipWith (-))
```

Remember how we said that the default combination for `zip` was making tuples? `zipWith` allows us to supply a custom
function for pairwise combination. In this case, subtraction.

Also remember that functions in Haskell are "curried", so `a -> b -> c` is actually more like `a -> (b -> c)`. Uncurry reverses this,
and transforms `a -> b -> c` into `(a, b) -> c`, a function that takes a tuple of two values and returns a value.

Since the result of the last operation was a tuple of two sorted lists, the full expression will take those two lists,
and apply them to a function that subtracts their elements pairwise. So

```haskell
(uncurry (zipWith (-))) ([1,2,3,3,3,4], [3,3,3,4,5,9]) == [-2, -1, 0, -1, -2, -5]
```

Finally, we take the absolute values of each of those numbers (`map abs`), and `sum` them up to get the total difference
between the two lists.

Hopefully now that all the groundwork is laid out, part 2 will be much shorter of an explanation:

# Day 1 part 2

Part two asks us to build a similarity score between the left and right list, by first multiplying each element with the
number of times it appeared in the right list, and summing them together

```haskell
import Data.IntMap.Strict qualified as IM
import Data.List (foldl', sort)

part2 :: IO ()
part2 = basicAnswer readLocationList calculate
  where
    calculate input = foldl' accum 0 left
      where
        (left, right) = unzip input
        frequency = foldl' (\freq i -> IM.insertWith (+) i 1 freq) IM.empty right
        accum total x = total + (x * IM.findWithDefault 0 x frequency)
```

In this one we don't use any fancy `.` combination logic, and instead use intermediate variables.

First, we unzip `[(Int, Int)]` into `([Int], [Int])`, and then assign each component to values `left` and `right`.

`frequency` is a Map of `Int -> Int` (think like a map `{1: 2}` in Python or Go). For this I'm using an `IntMap` for
performance, but there are other types of maps.

We build the map by `fold`ing over the right list. A fold is an operation that can be used to project a list of elements
down into a single element. Thing like rolling up a long sleeping bag into a roll so you can stuff it into your pack,
except the sleeping bag is a `[Int]`, rolling up is a `Int -> Int`, and your pack is an `Int`.

The first argument of `foldl'` is the accumulation function. The second argument is an initial seed value to start from.
The last is your list. So `foldl'` starts with your initial value, then combines it with the first element of the list
using the function, then combines that result with the second element, then that with the third, and so on until you
have a single value. To demonstrate, let's look at how `sum` would be written in Python vs Haskell.

```python
def sum(values):
    ret = 0
    for i in values:
        ret += i
    return ret
```

```haskell
sum values = foldl' (+) 0 values

-- or

sum = foldl' (+) 0
```

So to build the frequency map, we first start an empty map `IM.empty`. Then we traverse throught the list `right`
applying the function:

```haskell
(\freq i -> IM.insertWith (+) i 1 freq) 
```

Where `freq` is the current map and `i` is the current element. At each element, we `insertWith` 1 into the map at value
`i`. If the element was already in the map we instead add 1 (`(+)`) to the current value in the map. This will give us a
map of unique values to the amount of times they occurred in the list.

Then the final definition of the function is:

```haskell
foldl' accum 0 left
```

where accum is:

```haskell
accum total x = total + (x * IM.findWithDefault 0 x frequency)
```

So accum takes the total and adds it to the current value times how many times it occurred, or 0 if it didn't. foldl'
will take care of iterating through the list, and we start the total at 0. This will give us our answer.
