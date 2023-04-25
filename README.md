
# 🎄 Advent of Code 2021

Solving advent of code 2021 in haskell. Learning haskell has been a long term goal for me and I've never really stuck with it. I thought that doing a bunch of questions in haskell would be a good way of learning the basics.

The solutions can be ran with:
```
cabal run AoC2021
```

# WRITEUPS

## Day1

A lot of the work here was setting up the haskell project with cabal and
getting used to functional programming and general syntax. Both parts of this
question weren't too difficult (although I did completely rip the window code
for part b from stackoverflow).

## Day 2

Probably the biggest issue I ran into for this day was converting between the
native `String` type and `Data.Text` string and understanding the differences.
I found that the `OverloadedStrings` language extension made the code a bit
less verbose, but also made it harder to keep track of what the types were
doing.

## Day 3

Getting a bit into the hang of functional programming as I started making use
of more higher level functions. Also messed around with `Debug.Trace` to debug
part b.

## Day 4

Pretty straight forward. Although I feel like my solution wasn't exactly the
cleanest, it may be worth coming back later and trying to clean up this
solution.

## Day 5

I took a (couple month) break in the middle of doing this question. At first I
was trying to use a comparison method to determine line intersections. After I
settled for a very naive method of mapping each line to the points it covers
and search for points that have been covered more than once.

Once again, I feel like I could have made the code cleaner. However, my LSP
already helps quite a bit when dealing with uncurrying suggestions. For example
I learned about the `uncurry` function.

## Day 6

Not much to comment on, this day was pretty straight forward. It involved me
iterating through the most naive solution (simulating all fish in an array and
foldr to construct new array every time), to slightly better one (mapping
through array and adding baby fish seperately), to a final solution (buckets of
fish of each age).

## Day 7

Easy Peasy

## Day 8

Part B required a bit of a decision tree like structure, which I implemented in
a really ugly way in haskell (see the `decodeLine` function). Maybe next step
is to explore a more funtional / idiomatic method of solving this

# TODO
- [ ] cli that let's you print output of each day, also has flags for part a/b etc
- [ ] consider literate programming for questions?
