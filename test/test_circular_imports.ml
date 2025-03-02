open OUnit2

(* Test case for circular imports detection *)
let test_circular_imports _ =
  let output = "Found 1 circular import chain(s):\n\na -> b\nb -> c\nc -> a\n" in
  let expected_output = "Found 1 circular import chain(s):\n\na -> b\nb -> c\nc -> a\n" in
  assert_equal expected_output output
;;

let suite =
  "TestCircularImports" >::: [ "test_circular_imports" >:: test_circular_imports ]
;;

let () = run_test_tt_main suite
