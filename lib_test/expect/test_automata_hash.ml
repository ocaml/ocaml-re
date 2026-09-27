open! Import
module A = Re_private.Automata
module Cset = Re_private.Cset
module Category = Re_private.Category

let check_interning expected actual =
  let table = A.State.Table.create 1 in
  A.State.Table.add table expected ();
  assert (A.State.Table.mem table actual);
  A.State.Table.replace table actual ();
  assert (A.State.Table.length table = 1)
;;

let%expect_test "initial and derived states have compatible hashes" =
  let ids = A.Ids.create () in
  let a = A.cst ids (Cset.csingle 'a') in
  let b = A.cst ids (Cset.csingle 'b') in
  let suffixes =
    [ b
    ; A.alt ids [ a; b ]
    ; A.rep ids `Greedy `Longest b
    ; A.seq ids `First (A.mark ids A.Mark.start) b
    ]
  in
  List.iter
    Category.[ inexistant; letter; not_letter; newline; search_boundary ]
    ~f:(fun cat ->
      List.iter suffixes ~f:(fun suffix ->
        let expected = A.State.create cat suffix in
        let initial = A.State.create cat (A.seq ids `First a suffix) in
        let actual = A.delta (A.Working_area.create ()) cat (Cset.of_char 'a') initial in
        check_interning expected actual));
  [%expect {| |}]
;;

let%expect_test "nested derivatives with captures remain stable interning keys" =
  List.iter [ `First; `Longest; `Shortest ] ~f:(fun sem ->
    let ids = A.Ids.create () in
    let char c = A.cst ids (Cset.csingle c) in
    let seq x y = A.seq ids sem x y in
    let mark = A.Mark.start in
    let pmark = Re_private.Pmark.gen () in
    let captured =
      seq
        (A.mark ids mark)
        (seq
           (A.rep ids `Non_greedy sem (char 'c'))
           (seq (A.mark ids (A.Mark.next mark)) (A.pmark ids pmark)))
    in
    let repeated =
      A.rep
        ids
        `Greedy
        sem
        (A.alt
           ids
           [ seq captured (char 'd')
           ; seq (A.erase ids mark (A.Mark.next mark)) (char 'e')
           ])
    in
    let suffix = seq repeated (char 'z') in
    let cat = Category.from_char 'a' in
    let left_area = A.Working_area.create () in
    let right_area = A.Working_area.create () in
    let step area state c = A.delta area (Category.from_char c) (Cset.of_char c) state in
    let initial c = A.State.create cat (seq (char c) suffix) in
    let left = step left_area (initial 'a') 'a' in
    let right = step right_area (initial 'b') 'b' in
    check_interning left right;
    List.iter [ ""; "c"; "ccd"; "de"; "ccdedz"; "x" ] ~f:(fun input ->
      let left = ref left in
      let right = ref right in
      String.iter input ~f:(fun c ->
        left := step left_area !left c;
        right := step right_area !right c;
        check_interning !left !right);
      check_interning (A.advance left_area !left) (A.advance right_area !right)));
  [%expect {| |}]
;;
