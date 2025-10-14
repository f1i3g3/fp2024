(** Copyright 2025, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open C_sharp_strange_lib.Interpret
open C_sharp_strange_lib.Monad
open C_sharp_strange_lib.Common.Interpret

let show_wrap str =
  match interpret str with
  | Result.Ok x ->
    (match x with
     | Some x -> Format.printf "Result: '%a'" pp_vl x
     | None -> Format.print_string "Result: void\n")
  | Result.Error er -> Format.printf "%a\n%!" pp_error er
;;

let%expect_test _ =
  show_wrap
    {| 
  class Program {
    int b = 9;
    int c = 67;
    int a;
    bool r = false;
    string s = "ok";
    char h = 'a';
    bool t;

    static int Main() {
      a = (50 % 2) + b - c;
      r = s != "kkkk" && (190%22 == 100 * -2/5);
      t = (a != b * c) || (a >= b) && (a == c +90);
      return a;
    }
  } |};
  [%expect {|
    Result: '(Init (IValue (VInt -58)))' |}]
;;

let%expect_test _ =
  show_wrap
    {| 
  class Program {
    int n = 10;
    static int Main() {
      int res = 0;
      for(int i = 0; i < n; i = i+1) {
        for(int j = 0; j < i; j = j+1) {
          res = res + i *j;
        }
      }
      return res;
    }
  } |};
  [%expect {|
    Result: '(Init (IValue (VInt 870)))' |}]
;;

let%expect_test _ =
  show_wrap
    {| 
  class Program {
    bool t;
    int a = 5;

    static int Main() {
      int b = 5;
      int c = 2;
      t = true;
      if (t) {
        if (t && false) {
          t = false;
          return 1;
        }
        else if( a == b) {
          a = c*67 + 7;
          return a;
      }
      }
      else {
        return 3;
      }
      return 0;
    }
  } |};
  [%expect {|
    Result: '(Init (IValue (VInt 141)))' |}]
;;

let%expect_test _ =
  show_wrap
    {| 
  class Program {
    int x = 189;
    int s = 0;
    static int Main() {
      while (x != 0) {
          s = s + x % 10;
          x = x/ 10;
      }
      return s;
    }
  } |};
  [%expect {|
    Result: '(Init (IValue (VInt 18)))' |}]
;;

let%expect_test _ =
  show_wrap
    {| 
  class Program {
    public int is_right_triangle(int a, int b, int c) {
      if ((a + b <= c) || (a + c <= b) || (b + c <= a)) {
          return 0;
      } else if ((a * a + b * b == c * c) || (a * a + c * c == b * b) || (b * b + c * c == a * a)) {
          return 1;
      } else {
          return 2;
      }
    }
    static int Main() {
      return is_right_triangle(3,4,5);
    }
  } |};
  [%expect {|
    Result: '(Init (IValue (VInt 1)))' |}]
;;


let%expect_test _ =
  show_wrap
    {|
    class Program {
      static int Main() {
        int a;
        int b = a -1 + 4;
        return b;
      }
    } |};
  [%expect {| (Interpret_error (Other "Value is not initialized")) |}]
;;
