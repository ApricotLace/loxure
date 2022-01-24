(ns scanner-test
  (:require [scanner :as sut]
            [clojure.test :as t]))

(t/deftest scanner-test
  (t/testing "Basic scenarios"
    (t/is (= (sut/scan-tokens "\"!\" a")
             {:start 4
              :current 5
              :line 1
              :length 5
              :source "\"!\" a"
              :tokens
              [{:type :STRING :text "\"!\"" :literal "!" :line 1}
               {:type :IDENTIFIER :text "a" :literal nil :line 1}]}))

    (t/is (= (sut/scan-tokens "// this is a comment
                               identifier b and c
                               ((\"Lemon\" \"Apple\")){} // grouping stuff
                               !*+-/=<> <= == 5490 12.43 // operators
                               foo(arg1, arg2)
                               print \"Hello, World!\"
                               b and c")
             {:start 349
              :current 350
              :line 7
              :length 350
              :source
              "// this is a comment\n                               identifier b and c\n                               ((\"Lemon\" \"Apple\")){} // grouping stuff\n                               !*+-/=<> <= == 5490 12.43 // operators\n                               foo(arg1, arg2)\n                               print \"Hello, World!\"\n                               b and c"
              :tokens
              [{:type :IDENTIFIER :text "identifier" :literal nil :line 2}
               {:type :IDENTIFIER :text "b" :literal nil :line 2}
               {:type :AND :text "and" :literal nil :line 2}
               {:type :IDENTIFIER :text "c" :literal nil :line 2}
               {:type :LEFT_PAREN :text "(" :literal nil :line 3}
               {:type :LEFT_PAREN :text "(" :literal nil :line 3}
               {:type :STRING :text "\"Lemon\"" :literal "Lemon" :line 3}
               {:type :STRING :text "\"Apple\"" :literal "Apple" :line 3}
               {:type :RIGHT_PAREN :text ")" :literal nil :line 3}
               {:type :RIGHT_PAREN :text ")" :literal nil :line 3}
               {:type :LEFT_BRACE :text "{" :literal nil :line 3}
               {:type :RIGHT_BRACE :text "}" :literal nil :line 3}
               {:type :BANG :text "!" :literal nil :line 4}
               {:type :STAR :text "*" :literal nil :line 4}
               {:type :PLUS :text "+" :literal nil :line 4}
               {:type :MINUS :text "-" :literal nil :line 4}
               {:type :SLASH :text "/" :literal nil :line 4}
               {:type :EQUAL :text "=" :literal nil :line 4}
               {:type :LESS :text "<" :literal nil :line 4}
               {:type :GREATER :text ">" :literal nil :line 4}
               {:type :LESS_EQUAL :text "<=" :literal nil :line 4}
               {:type :EQUAL_EQUAL :text "==" :literal nil :line 4}
               {:type :NUMBER :text "5490" :literal 5490.0 :line 4}
               {:type :NUMBER :text "12.43" :literal 12.43 :line 4}
               {:type :IDENTIFIER :text "foo" :literal nil :line 5}
               {:type :LEFT_PAREN :text "(" :literal nil :line 5}
               {:type :IDENTIFIER :text "arg1" :literal nil :line 5}
               {:type :COMMA :text "," :literal nil :line 5}
               {:type :IDENTIFIER :text "arg2" :literal nil :line 5}
               {:type :RIGHT_PAREN :text ")" :literal nil :line 5}
               {:type :PRINT :text "print" :literal nil :line 6}
               {:type :STRING :text "\"Hello, World!\"" :literal "Hello, World!" :line 6}
               {:type :IDENTIFIER :text "b" :literal nil :line 7}
               {:type :AND :text "and" :literal nil :line 7}
               {:type :IDENTIFIER :text "c" :literal nil :line 7}]})))

  (t/testing "Error handling"
    (t/is (= (sut/scan-tokens "a and b and \"unmatched string ")
             {:start 12
              :current 13
              :line 1
              :length 30
              :source "a and b and \"unmatched string "
              :tokens
              [{:type :IDENTIFIER :text "a" :literal nil :line 1}
               {:type :AND :text "and" :literal nil :line 1}
               {:type :IDENTIFIER :text "b" :literal nil :line 1}
               {:type :AND :text "and" :literal nil :line 1}]
              :error {:where "" :message "Unmatched quote at position 13"}})))

  )
