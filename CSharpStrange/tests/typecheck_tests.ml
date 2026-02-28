(** Copyright 2025, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open C_sharp_strange_lib.Typecheck
open C_sharp_strange_lib.Monads
open C_sharp_strange_lib.Parser
open C_sharp_strange_lib.Ast
open C_sharp_strange_lib.Common

let show_wrap = function
  | Some (Program x) ->
    (match typecheck x with
     | _, Result.Ok _ -> Format.print_string "Ok!\n"
     | _, Result.Error e -> Format.printf "%a\n%!" pp_error e)
  | _ -> Format.print_string "Some error\n"
;;

let print_tc p str = show_wrap (parse_option p str)
let test_ast = print_tc parse_prog

let%expect_test _ =
  test_ast
    {|
    class Program {
      int Fac(int num) {
        if (num == 1) {
          return 1;
        }
        else 
        {
          return num * Fac(num - 1);
        }
      }
      static int Main() {
        return Fac(5);
      }
    } |};
  [%expect {|
    Ok! |}]
;;

(* TODO: funccall! *)

let%expect_test _ =
  test_ast
    {|
    class Program {
      int Fac(int num) {
        if (num == 1) {
          return "one";
        }
      }
    } |};
  [%expect
    {|
    (TCError (OtherError "Returned type does not match the function type")) |}]
;;

(* TODO: funccall! *)

let%expect_test _ =
  test_ast {| 
  class Program {
    int a = 5;
    int b = 9;
    int a = 9;
  } |};
  [%expect {|
    (TCError (OtherError "This variable is already declared")) |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class Program {
    int b = 9;
    int c = b * 67;
    int a = (50 % 2) + b - c;
    bool r = (a != b * c) || (a >= b) && (a == c +90);
    string s = "ok";
    char h = 'a';

    void M() {
      a = 5;
      r = s != "kkkk" && (190%22 == 100 * -2/5);
    }
  } |};
  [%expect {|
    Ok! |}]
;;

(* TODO: parser check! *)

let%expect_test _ =
  test_ast {| 
  class Program {
    string a = "5";
    int c = 9 + a;
  } |};
  [%expect {|
    (TCError TypeMismatch) |}]
;;

(* TODO: string! *)

let%expect_test _ =
  test_ast
    {| 
  class Program {
    static int Main() {
      int counter = 0;
      bool b = true;
      while(true) {
        if (count != 2) {
          count = count + 1;
          b = b && false;
        }
        else if (b == false){
          return -1;
        }
        else {
          return 0;
        }
      }
    }
  } |};
  [%expect {|
    Ok! |}]
;;

(* TODO: ????! *)

let%expect_test _ =
  test_ast
    {| 
  class Program {
    int n = 10;
    int count = 7% 2*67;
    static int Main() {
      for (int i = 0; i < n; i=i+1) {
        for (int j = 1;;) {
          for (;j != n; j = j + 2) {
            for (;;) {
              count = count + i + j;
            }
          }
        }
      }
      return count;
    }
  } |};
  [%expect {|
    Ok! |}]
;;

(* TODO: some stuff here! *)

let%expect_test _ =
  test_ast {| 
  class Program {
    public virtual void Main() {}
  }
  |};
  [%expect
    {|
    (TCError
       (OtherError
          "Main must be a static method, have no params and return only int and void")) |}]
;;

(* TODO: formatting???! *)

let%expect_test _ =
  test_ast
    {| 
  class Program {
    void Test() {}
    int a = 9;
    void Test() {}
  } |};
  [%expect {|
    (TCError (OtherError "This variable is already declared")) |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class Program {
    public void a(int n, int m){
      return n+m;
    };
  }|};
  [%expect {|
    (TCError TypeMismatch) |}]
;;
(* TODO: check formatting??!*)

(* TODO: occurs check: smth like
  {|
  class Program {
    public void foo() {
      bool a = new A();
    };
  }|}
*)
