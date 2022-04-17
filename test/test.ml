open OUnit2
open Ocaml_lang.Lexer

let tests =
  "lexer"
  >::: [
         ( "is_identifier" >:: fun _ ->
           assert_equal true (Lexer.is_identifier 'a');
           assert_equal true (Lexer.is_identifier '_');
           assert_equal false (Lexer.is_identifier '!');
           assert_equal false (Lexer.is_identifier ' ') );
         ( "is_whitespace" >:: fun _ ->
           assert_equal true (Lexer.is_whitespace ' ');
           assert_equal false (Lexer.is_whitespace 'a');
           assert_equal false (Lexer.is_whitespace '!') );
         ( "is_digit" >:: fun _ ->
           assert_equal true (Lexer.is_digit '0');
           assert_equal true (Lexer.is_digit '1');
           assert_equal false (Lexer.is_digit 'a');
           assert_equal false (Lexer.is_digit '!');
           assert_equal false (Lexer.is_digit ' ') );
         ( "string_of_chars" >:: fun _ ->
           assert_equal "test" (Lexer.string_of_chars [ 't'; 'e'; 's'; 't' ]);
           assert_equal "123a" (Lexer.string_of_chars [ '1'; '2'; '3'; 'a' ]) );
         ( "lex_while" >:: fun _ ->
           assert_equal ("123", [ 'a' ])
             (Lexer.lex_while Lexer.is_digit [ '1'; '2'; '3'; 'a' ]);
           assert_equal
             ("", [ 'a'; 'b' ])
             (Lexer.lex_while Lexer.is_digit [ 'a'; 'b' ]);
           assert_equal ("abc", [])
             (Lexer.lex_while Lexer.is_identifier [ 'a'; 'b'; 'c' ]);
           assert_equal ("abc", [ '1' ])
             (Lexer.lex_while Lexer.is_identifier [ 'a'; 'b'; 'c'; '1' ]) );
         ( "lex_token" >:: fun _ ->
           assert_equal (Lexer.EOF, []) (Lexer.lex_token []);
           assert_equal (Lexer.Plus, []) (Lexer.lex_token [ '+' ]);
           assert_equal (Lexer.Plus, [ 'a' ]) (Lexer.lex_token [ '+'; 'a' ]);
           assert_equal (Lexer.Identifier "a", []) (Lexer.lex_token [ 'a' ]);
           assert_equal
             (Lexer.Identifier "abc", [])
             (Lexer.lex_token [ 'a'; 'b'; 'c' ]);
           assert_equal (Lexer.Identifier "_", []) (Lexer.lex_token [ '_' ]);
           assert_equal
             (Lexer.Identifier "_a", [])
             (Lexer.lex_token [ '_'; 'a' ]);
           assert_equal
             (Lexer.Identifier "a_", [])
             (Lexer.lex_token [ 'a'; '_' ]);
           assert_equal
             (Lexer.Integer "1", [ '_' ])
             (Lexer.lex_token [ '1'; '_' ]);
           assert_equal
             (Lexer.Integer "123", [ 'a' ])
             (Lexer.lex_token [ '1'; '2'; '3'; 'a' ]);
           assert_equal
             (Lexer.Integer "1", [ '_' ])
             (Lexer.lex_token [ '1'; '_' ]);
           assert_equal (Lexer.Illegal '?', []) (Lexer.lex_token [ '?' ]) );
         ( "lex" >:: fun _ ->
           assert_equal [] (Lexer.lex "");
           assert_equal
             [ Lexer.Identifier "a1"; Lexer.Illegal '?' ]
             (Lexer.lex "a1?");
           assert_equal
             [ Lexer.Integer "1"; Lexer.Identifier "a" ]
             (Lexer.lex "1a") );
       ]

let _ = run_test_tt_main tests
