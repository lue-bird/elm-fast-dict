module FastSet exposing
    ( Set
    , empty, singleton, insert, remove
    , isEmpty, member, size, equals
    , getMin, getMax
    , popMin, popMax
    , toList, fromList
    , fromDictKeys
    , map, foldl, foldr, filter, partition
    , union, intersect, diff
    , toCoreSet, fromCoreSet
    , stoppableFoldl, stoppableFoldr, restructure, fromListFast
    )

{-| A set of unique values. The values can be any comparable type. This
includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists
of comparable types.

Insert, remove, and query operations all take _O(log n)_ time.


# Sets

@docs Set


# Build

@docs empty, singleton, insert, remove


# Query

@docs isEmpty, member, size, equals


# Min / Max

@docs getMin, getMax

@docs popMin, popMax


# Lists and dictionaries

@docs toList, fromList
@docs fromDictKeys


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff


# Interoperability

@docs toCoreSet, fromCoreSet


# Advanced functions

@docs stoppableFoldl, stoppableFoldr, restructure, fromListFast

-}

import FastDict
import Set


{-| Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
-}
type Set key
    = Set (FastDict.Dict key ())


{-| Create an empty set.
-}
empty : Set a
empty =
    Set FastDict.empty


{-| Create a set with one value.
-}
singleton : comparable -> Set comparable
singleton key =
    Set (FastDict.singleton key ())


{-| Insert a value into a set.
-}
insert : comparable -> Set comparable -> Set comparable
insert key (Set dict) =
    Set (FastDict.insert key () dict)


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : comparable -> Set comparable -> Set comparable
remove key (Set dict) =
    Set (FastDict.remove key dict)


{-| Determine if a set is empty.
-}
isEmpty : Set a -> Bool
isEmpty (Set dict) =
    FastDict.isEmpty dict


{-| Determine if a value is in a set.
-}
member : comparable -> Set comparable -> Bool
member key (Set dict) =
    FastDict.member key dict


{-| Determine the number of elements in a set.
-}
size : Set a -> Int
size (Set dict) =
    FastDict.size dict


{-| Determine if two sets are equal. This is needed because the structure could be different depending on insertion order.
-}
equals : Set comparable -> Set comparable -> Bool
equals (Set leftDict) (Set rightSet) =
    FastDict.equals leftDict rightSet


{-| Gets the smallest key in the set.

    [ 1, 2 ]
        |> fromList
        |> getMin
    --> Just 1


    empty
        |> getMin
    --> Nothing

-}
getMin : Set k -> Maybe k
getMin (Set dict) =
    FastDict.getMinKey dict


{-| Gets the biggest key in the set.

    [ 1, 2 ]
        |> fromList
        |> getMax
    --> Just 2


    empty
        |> getMax
    --> Nothing

-}
getMax : Set k -> Maybe k
getMax (Set dict) =
    FastDict.getMaxKey dict


{-| Removes the key-value pair with the smallest key from the dictionary, and returns it.

    [ 1, 2 ]
        |> fromList
        |> popMin
    --> Just ( 1, fromList [ 2 ] )


    empty
        |> popMin
    --> Nothing

-}
popMin : Set comparable -> Maybe ( comparable, Set comparable )
popMin (Set dict) =
    Maybe.map (\( ( key, () ), withoutKey ) -> ( key, Set withoutKey ))
        (FastDict.popMin dict)


{-| Removes the key with the biggest key from the set, and returns it.

    [ 1, 2 ]
        |> fromList
        |> popMax
    --> Just ( 2, fromList [ 1 ] )


    empty
        |> popMax
    --> Nothing

-}
popMax : Set comparable -> Maybe ( comparable, Set comparable )
popMax (Set dict) =
    Maybe.map (\( ( key, () ), withoutKey ) -> ( key, Set withoutKey ))
        (FastDict.popMax dict)


{-| Get the union of two sets. Keep all values.
-}
union : Set comparable -> Set comparable -> Set comparable
union (Set dict1) (Set dict2) =
    Set (FastDict.union dict1 dict2)


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : Set comparable -> Set comparable -> Set comparable
intersect (Set dict1) (Set dict2) =
    Set (FastDict.intersect dict1 dict2)


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : Set comparable -> Set comparable -> Set comparable
diff (Set dict1) (Set dict2) =
    Set (FastDict.diff dict1 dict2)


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : Set a -> List a
toList (Set dict) =
    FastDict.keys dict


{-| Convert a list into a set, removing any duplicates.
-}
fromList : List comparable -> Set comparable
fromList list =
    List.foldl insert empty list


{-| Convert a list into a set, removing any duplicates.
-}
fromListFast : List comparable -> Set comparable
fromListFast list =
    -- TODO introduce FastDict.fromListMapFast to save one traversal
    list
        |> List.map (\key -> ( key, () ))
        |> FastDict.fromListFast
        |> Set


{-| Get all of the keys in a [dictionary](FastDict#Dict) as a set.

    import FastDict

    [ ( 0, "Alice" ), ( 1, "Bob" ) ]
        |> FastDict.fromList
        |> fromDictKeys
    --> FastSet.fromList [ 0, 1 ]

-}
fromDictKeys : FastDict.Dict k v -> Set k
fromDictKeys dict =
    Set (FastDict.map (\_ _ -> ()) dict)


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> b -> b) -> b -> Set a -> b
foldl func initialState (Set dict) =
    FastDict.foldl (\key _ state -> func key state) initialState dict


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> b -> b) -> b -> Set a -> b
foldr func initialState (Set dict) =
    FastDict.foldr (\key _ state -> func key state) initialState dict


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (comparable -> comparableChanged) -> Set comparable -> Set comparableChanged
map func set =
    -- TODO fromListFast?
    fromList (foldl (\x xs -> func x :: xs) [] set)


{-| Only keep elements that pass the given test.

    import Set exposing (Set)

    numbers : Set Int
    numbers =
        Set.fromList [ -2, -1, 0, 1, 2 ]

    positives : Set Int
    positives =
        Set.filter (\x -> x > 0) numbers

    -- positives == Set.fromList [1,2]

-}
filter : (comparable -> Bool) -> Set comparable -> Set comparable
filter isGood (Set dict) =
    Set (FastDict.filter (\key () -> isGood key) dict)


{-| Create two new sets. The first contains all the elements that passed the
given test, and the second contains all the elements that did not.
-}
partition : (comparable -> Bool) -> Set comparable -> ( Set comparable, Set comparable )
partition isGood (Set dict) =
    let
        ( goodDict, badDict ) =
            FastDict.partition (\key () -> isGood key) dict
    in
    ( Set goodDict, Set badDict )



-- INTEROPERABILITY


{-| Convert the dictionary into an equivalent one from elm/core.
-}
toCoreSet : Set comparable -> Set.Set comparable
toCoreSet set =
    foldl Set.insert Set.empty set


{-| Convert the dictionary from an equivalent one from elm/core.
-}
fromCoreSet : Set.Set comparable -> Set comparable
fromCoreSet coreSet =
    Set.foldl insert empty coreSet



-- ADVANCED


{-| A foldl that can stop early instead of traversing the whole dictionary.
`Stop` and `Continue` used in the example below are from [`FastDict.Step`](FastDict#Step)

    stoppableFoldl
        (\k acc ->
            if k >= 10 then
                Stop acc
            else
                Continue (k + acc)
        )
        0
        (fromList (List.range 1 10000))
    --> 55

-}
stoppableFoldl : (k -> acc -> FastDict.Step acc) -> acc -> Set k -> acc
stoppableFoldl func acc (Set dict) =
    FastDict.stoppableFoldl (\key () soFar -> func key soFar) acc dict


{-| A foldr that can stop early instead of traversing the whole dictionary.
`Stop` and `Continue` used in the example below are from [`FastDict.Step`](FastDict#Step)

    stoppableFoldr
        (\k acc ->
            if k <= 9990 then
                Stop acc
            else
                Continue (k + acc)
        )
        0
        (fromList (List.range 1 10000))
    --> 89964

-}
stoppableFoldr : (k -> acc -> FastDict.Step acc) -> acc -> Set k -> acc
stoppableFoldr func acc (Set dict) =
    FastDict.stoppableFoldr (\key () soFar -> func key soFar) acc dict


{-| This allows you to take advantage of the tree structure of the dictionary to do some operations more efficiently.

Calling `left` will give the result of calling `restructure` on the left subtree (lower keys), `right` on the right one (higher keys).

If this is confusing you probably don't need this function!

    any dict =
        -- Notice how if `value` is `True` we don't call `left` nor `right`,
        -- and if `value` is `False` but `left ()` is `True` we don't call right.
        restructure False (\{ value, left, right } -> value || left () || right ())

-}
restructure :
    acc
    -> ({ key : key, left : () -> acc, right : () -> acc } -> acc)
    -> Set key
    -> acc
restructure leafAcc nodeFunc (Set dict) =
    FastDict.restructure leafAcc
        (\nodeRestructureInfo ->
            nodeFunc
                { key = nodeRestructureInfo.key
                , left = nodeRestructureInfo.left
                , right = nodeRestructureInfo.right
                }
        )
        dict
