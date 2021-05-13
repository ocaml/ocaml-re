//
// Implementation of the OCaml `Seq` module. Most documentation comments come
// from: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Seq.html
//

// The type t('a) is a delayed list, i.e. a list where some evaluation is
// needed to access the next element. This makes it possible to build infinite
// sequences, to build sequences as we traverse them, and to transform them in
// a lazy fashion rather than upfront.

// The type of delayed lists containing elements of type 'a. Note that the
// concrete list node node('a) is delayed under a closure, not a lazy block,
// which means it might be recomputed every time we access it.
type t('a) = unit => node('a)
and node('a) =
    | Nil
    | Cons('a, t('a)); /* A fully-evaluated list node, either empty or
                          containing an element and a delayed tail. */

// The empty sequence, containing no elements.
let empty: t('a) = () => Nil;

// The singleton sequence containing only the given element.
let return: 'a => t('a) = x => {
    () => Cons(x, empty)
};

// Converts a sequence into a list.
let rec to_list: t('a) => list('a) = seq => {
    switch (seq()) {
    | Nil => []
    | Cons(x, xs) => [x, ...to_list(xs)]
    }
};

// Converts a list into a sequence. This transformation is lazy, it only
// applies when the result is traversed.
let rec of_list: list('a) => t('a) = list => {
    switch (list) {
    | [] => empty
    | [x, ...xs] => () => Cons(x, of_list(xs))
    };
};

// map f seq returns a new sequence whose elements are the elements of seq,
// transformed by f. This transformation is lazy, it only applies when the
// result is traversed.
// If seq = [1;2;3], then map f seq = [f 1; f 2; f 3].
let rec map: ('a => 'b, t('a)) => t('b) = (f, seq) => {
    switch (seq()) {
    | Nil => empty
    | Cons(x, xs) => () => Cons(f(x), map(f, xs))
    }
};

// Remove from the sequence the elements that do not satisfy the given
// predicate. This transformation is lazy, it only applies when the result is
// traversed.
let rec filter: ('a => bool, t('a)) => t('a) = (pred, seq) => {
    switch (seq()) {
    | Nil => empty
    | Cons(x, xs) when pred(x) => () => Cons(x, filter(pred, xs))
    | Cons(_, xs) => filter(pred, xs)
    }
};

// Apply the function to every element; if f x = None then x is dropped;
// if f x = Some y then y is returned. This transformation is lazy, it only
// applies when the result is traversed.
let rec filter_map: ('a => option('b), t('a)) => t('b) = (f, seq) => {
    switch (seq()) {
    | Nil => empty
    | Cons(x, xs) => switch (f(x)) {
        | None => filter_map(f, xs)
        | Some(y) => () => Cons(y, filter_map(f, xs))
        }
    }
};

// Append all of the first sequence onto the end of the second sequence. This
// transformation is lazy, it only applies when the result is traversed.
let rec append: (t('a), t('a)) => t('a) = (seq1, seq2) => {
    switch (seq1()) {
    | Nil => seq2
    | Cons(x, xs) => () => Cons(x, append(xs, seq2))
    }
};

let (@~) = append;

// Map each element to a subsequence, then return each element of this
// sub-sequence in turn. This transformation is lazy, it only applies when the
// result is traversed.
let rec flat_map: ('a => t('b), t('a)) => t('b) = (f, seq) => {
    switch (seq()) {
    | Nil => empty
    | Cons(x, xs) => f(x) @~ flat_map(f, xs)
    }
}

// Traverse the sequence from left to right, combining each element with the
// accumulator using the given function. The traversal happens immediately and
// will not terminate on infinite sequences.
// Also see List.fold_left
let rec fold_left = (
    f: ('acc, 'ele) => 'acc,
    accumulator: 'acc,
    seq: t('ele)
): 'acc => {
    switch (seq()) {
    | Nil => accumulator
    | Cons(x, xs) => fold_left(f, f(accumulator, x), xs)
    }
};

// Iterate on the sequence, calling the (imperative) function on every element.
// The traversal happens immediately and will not terminate on infinite sequences.
let rec iter: ('a => unit, t('a)) => unit = (f, seq) => {
    switch (seq()) {
    | Nil => ()
    | Cons(x, xs) => f(x); iter(f, xs)
    }
};