open TokenTypes

(* PASTE YOUR LEXER FROM P4A HERE *)

let tokenize input = 
  let length = String.length input in

  let rec tok pos =
    if pos >= length then
      []

    else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos then
      let value = Str.matched_string input in 
      if (value = "true"||value = "false") then
          (if (value = "true") then Tok_Bool(true)::(tok (pos + 4))
          else Tok_Bool(false)::(tok (pos + 5)))
      else if (value = "not") then
        Tok_Not::(tok (pos + 3))
  
      else if (value = "if") then
        Tok_If::(tok (pos + 2))
  
      else if (value = "then") then
        Tok_Then::(tok (pos + 4))
  
      else if (value = "else") then
        Tok_Else::(tok (pos + 4))

      else if (value = "let") then
        Tok_Let::(tok (pos + 3))
  
      else if (value = "def") then
        Tok_Def::(tok (pos + 3))
  
      else if (value = "in") then
        Tok_In::(tok (pos + 2))
  
      else if (value = "rec") then
        Tok_Rec::(tok (pos + 3))
  
      else if (value = "fun") then
        Tok_Fun::(tok (pos + 3))
      
      else
        Tok_ID(value)::(tok (pos + String.length value))

    else if Str.string_match (Str.regexp "[0-9]+\\|(-[0-9]+)") input pos then
      let value = Str.matched_string input in
      if (String.contains value '-') then
        Tok_Int(int_of_string (String.sub value 1 ((String.length value)-2)))::(tok (pos + String.length value))
      else
        Tok_Int(int_of_string value)::(tok (pos + String.length value))

    else if Str.string_match (Str.regexp "\"[^\"]*\"") input pos then
      let value = Str.matched_string input in
      Tok_String(String.escaped (String.sub value 1 (String.length value - 2)))::(tok (pos + String.length value))

    else if Str.string_match (Str.regexp "(") input pos then
      Tok_LParen::(tok (pos + 1))

    else if Str.string_match (Str.regexp ")") input pos then
      Tok_RParen::(tok (pos + 1))

    else if Str.string_match (Str.regexp "=") input pos then
      Tok_Equal::(tok (pos + 1))

    else if Str.string_match (Str.regexp "<>") input pos then
      Tok_NotEqual::(tok (pos + 2))

    else if Str.string_match (Str.regexp "->") input pos then
      Tok_Arrow::(tok (pos + 2))

    else if Str.string_match (Str.regexp ">") input pos then
      Tok_Greater::(tok (pos + 1))

    else if Str.string_match (Str.regexp "<") input pos then
      Tok_Less::(tok (pos + 1))

    else if Str.string_match (Str.regexp ">=") input pos then
      Tok_GreaterEqual::(tok (pos + 2))

    else if Str.string_match (Str.regexp "<=") input pos then
      Tok_LessEqual::(tok (pos + 2))

    else if Str.string_match (Str.regexp "||") input pos then
      Tok_Or::(tok (pos + 2))

    else if Str.string_match (Str.regexp "&&") input pos then
      Tok_And::(tok (pos + 2))

    else if Str.string_match (Str.regexp "\\+") input pos then
      Tok_Add::(tok (pos + 1))

    else if Str.string_match (Str.regexp "\\-") input pos then
      Tok_Sub::(tok (pos + 1))

    else if Str.string_match (Str.regexp "\\*") input pos then
      Tok_Mult::(tok (pos + 1))

    else if Str.string_match (Str.regexp "\\/") input pos then
      Tok_Div::(tok (pos + 1))

    else if Str.string_match (Str.regexp "\\^") input pos then
      Tok_Concat::(tok (pos + 1))

    else if Str.string_match (Str.regexp ";;") input pos then
      Tok_DoubleSemi::(tok (pos + 2))

    else
      tok (pos + 1)

  in tok 0;;